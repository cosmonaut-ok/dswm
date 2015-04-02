;; Copyright (C) 2003-2008 Shawn Betts
;;
;;  This file is part of dswm.
;;
;; dswm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; dswm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; Frame functions
;;
;; Code:

(in-package #:dswm)

(export '(save-frame-excursion
	  hsplit
	  vsplit
	  remove-split
	  remove
	  only
	  curframe
	  fnext
	  fprev
	  sibling
	  fother
	  fselect
	  resize
	  fclear
	  move-focus
	  move-window
	  next-in-frame
	  prev-in-frame
	  other-in-frame
	  balance-frames
	  move-window-to-frame))

(defclass tile-group (group)
  ((frame-tree :accessor tile-group-frame-tree)
   (last-frame :initform nil :accessor tile-group-last-frame)
   (current-frame :accessor tile-group-current-frame)))

(defmethod initialize-instance :after ((group tile-group) &key &allow-other-keys)
  (let* ((heads (copy-heads (group-screen group))))
    (setf (tile-group-frame-tree group) heads
          (tile-group-current-frame group) (first heads))))

(defmethod group-startup ((group tile-group))
  (let ((window (first (group-windows group))))
    (if window
        (focus-frame group (window-frame window))
        (focus-frame group (tile-group-current-frame group)))))

(defmethod group-wake-up ((group tile-group))
  (focus-frame group (tile-group-current-frame group))
  ;; doesn't get called by focus-frame
  (show-frame-indicator group))

(defmethod group-delete-window ((group tile-group) window)
  (let ((f (window-frame window)))
    ;; maybe pick a new window for the old frame
    (when (eq (frame-window f) window)
      (frame-raise-window group f (first (frame-windows group f)) nil))))

(defmethod group-add-window ((group tile-group) window &key frame raise &allow-other-keys)
  ;; This is important to get the frame slot
  (change-class window 'tile-window)
  ;; Try to put the window in the appropriate frame for the group.
  (setf (window-frame window)
        (or frame
            (when *processing-existing-windows*
              (find-frame group (xlib:drawable-x (window-parent window))
                          (xlib:drawable-y (window-parent window))))
            (pick-preferred-frame window)))
  (when *processing-existing-windows*
    (setf (frame-window (window-frame window)) window))
  (when (and frame raise)
    (setf (tile-group-current-frame group) frame
          (frame-window frame) nil))
  (sync-frame-windows group (window-frame window))
  ;; maybe show the window in its new frame
  (when (null (frame-window (window-frame window)))
    ;; Experimental: Issue 0000012
    ;;    (really-raise-window window)))
    (frame-raise-window group (window-frame window) window raise)))
;; /Issue

(defmethod group-current-window ((group tile-group))
  (frame-window (tile-group-current-frame group)))

(defmethod group-current-head ((group tile-group))
  (frame-head group (tile-group-current-frame group)))

(defmethod group-move-request ((group tile-group) window x y relative-to)
  (when *honor-window-moves*
    (dformat 3 "Window requested new position ~D,~D relative to ~S~%" x y relative-to)
    (let* ((pos  (if (eq relative-to :parent)
                     (list
                      (+ (xlib:drawable-x (window-parent window)) x)
                      (+ (xlib:drawable-y (window-parent window)) y))
                     (list x y)))
           (frame (apply #'find-frame group pos)))
      (when frame
        (pull-window window frame)))))

(defmethod group-resize-request ((group tile-group) window width height)
  ;; it's important to grant the resize request first so that resize
  ;; increment hints use the proper base size to resize from.
  (set-window-geometry window :width width :height height)
  (maximize-window window))

(defmethod group-raise-request ((group tile-group) window stack-mode)
  (when (window-in-current-group-p window)
    (case stack-mode
      (:map
       (maybe-map-window window))
      (:above
       (maybe-raise-window window)))))

(defmethod group-lost-focus ((group tile-group))
  ;; If this window had the focus, try to avoid losing it.
  (let ((frame (tile-group-current-frame group)))
    (setf (frame-window frame)
          (first (remove-if 'window-hidden-p (frame-windows group frame))))
    (focus-frame group frame)))

(defmethod group-indicate-focus ((group tile-group))
  (show-frame-indicator group))

(defmethod group-focus-window ((group tile-group) win)
  (frame-raise-window group (window-frame win) win))

(defmethod group-button-press ((group tile-group) x y (where (eql :root)))
  (when *root-click-focuses-frame*
    (let* ((frame (find-frame group x y)))
      (when frame
        (focus-frame group frame)
        (update-all-mode-lines)))))

(defmethod group-button-press ((group tile-group) x y (where window))
  (declare (ignore x y))
  (when (eq *mouse-focus-policy* :click)
    (focus-all where)
    (update-all-mode-lines)))

(defmethod group-root-exposure ((group tile-group))
  (show-frame-outline group nil))

(defmethod group-add-head ((group tile-group) head)
  (let ((new-frame-num (find-free-frame-number group)))
    (setf (tile-group-frame-tree group)
          (insert-before (tile-group-frame-tree group)
                         (copy-frame head)
                         (head-number head)))
    ;; Try to put something in the new frame and give it an unused number
    (let ((frame (tile-group-frame-head group head)))
      (setf (frame-number frame) new-frame-num)
        (choose-new-frame-window frame group)
        (when (frame-window frame)
          (unhide-window (frame-window frame))))))

(defmethod group-remove-head ((group tile-group) head)
  (let ((windows (head-windows group head)))
    ;; Remove it from the frame tree.
    (setf (tile-group-frame-tree group) (delete (tile-group-frame-head group head) (tile-group-frame-tree group)))
    ;; Just set current frame to whatever.
    (let ((frame (first (group-frames group))))
      (setf (tile-group-current-frame group) frame
            (tile-group-last-frame group) nil)
      ;; Hide its windows.
      (dolist (window windows)
        (hide-window window)
        (setf (window-frame window) frame))))
  ;; Try to do something with the orphaned windows
  (populate-frames group))

(defmethod group-resize-head ((group tile-group) oh nh)
  (resize-tree (tile-group-frame-head group oh) (head-width nh) (head-height nh) (head-x nh) (head-y nh)))

(defmethod group-sync-all-heads ((group tile-group))
  (sync-all-frame-windows group))

(defmethod group-sync-head ((group tile-group) head)
  (dolist (f (head-frames group head))
    (sync-frame-windows group f)))

;;;;;

(defun tile-group-frame-head (group head)
  (elt (tile-group-frame-tree group) (position head (group-heads group))))

(defun (setf tile-group-frame-head) (frame group head)
  (setf (elt (tile-group-frame-tree group) (position head (group-heads group))) frame))

(defun populate-frames (group)
  "Try to fill empty frames in GROUP with hidden windows"
  (dolist (f (group-frames group))
    (unless (frame-window f)
      (choose-new-frame-window f group)
      (when (frame-window f)
        (maximize-window (frame-window f))
        (unhide-window (frame-window f))))))

(defun frame-by-number (group n)
  (unless (eq n nil)
    (find n (group-frames group)
          :key 'frame-number
          :test '=)))

(defun find-frame (group x y)
  "Return the frame of GROUP containing the pixel at X Y"
  (dolist (f (group-frames group))
    (let* ((fy (frame-y f))
           (fx (frame-x f))
           (fwx (+ fx (frame-width f)))
           (fhy (+ fy (frame-height f))))
      (when (and
             (>= y fy) (<= y fhy)
             (>= x fx) (<= x fwx)
             (return f))))))


(defun frame-set-x (frame v)
  (decf (frame-width frame)
        (- v (frame-x frame)))
  (setf (frame-x frame) v))

(defun frame-set-y (frame v)
  (decf (frame-height frame)
        (- v (frame-y frame)))
  (setf (frame-y frame) v))

(defun frame-set-r (frame v)
  (setf (frame-width frame)
        (- v (frame-x frame))))

(defun frame-set-b (frame v)
  (setf (frame-height frame)
        (- v (frame-y frame))))

(defun frame-r (frame)
  (+ (frame-x frame) (frame-width frame)))

(defun frame-b (frame)
  (+ (frame-y frame) (frame-height frame)))

(defun frame-display-y (group frame)
  "Return a Y for frame that doesn't overlap the mode-line."
  (let* ((head (frame-head group frame))
         (ml (head-mode-line head))
	 (head-y (frame-y head))
	 (rel-frame-y (- (frame-y frame) head-y)))
    (if (and ml (not (eq (mode-line-mode ml) :hidden)))
        (case (mode-line-position ml)
          (:top
           (+ head-y
	      (+ (mode-line-height ml) (round (* rel-frame-y (mode-line-factor ml))))))
          (:bottom
           (+ head-y
	      (round (* rel-frame-y (mode-line-factor ml))))))
        (frame-y frame))))

(defun frame-display-height (group frame)
  "Return a HEIGHT for frame that doesn't overlap the mode-line."
  (let* ((head (frame-head group frame))
         (ml (head-mode-line head)))
    (if (and ml (not (eq (mode-line-mode ml) :hidden)))
        (round (* (frame-height frame) (mode-line-factor ml)))
        (frame-height frame))))

(defun frame-intersect (f1 f2)
  "Return a new frame representing (only) the intersection of F1 and F2. WIDTH and HEIGHT will be <= 0 if there is no overlap"
  (let ((r (copy-frame f1)))
    (when (> (frame-x f2) (frame-x f1))
      (frame-set-x r (frame-x f2)))
    (when (< (+ (frame-x f2) (frame-width f2))
             (+ (frame-x f1) (frame-width f1)))
      (frame-set-r r (frame-r f2)))
    (when (> (frame-y f2) (frame-y f1))
      (frame-set-y r (frame-y f2)))
    (when (< (+ (frame-y f2) (frame-height f2))
             (+ (frame-y f1) (frame-height f1)))
      (frame-set-b r (frame-b f2)))
  (values r)))

(defun frames-overlap-p (f1 f2)
  "Returns T if frames F1 and F2 overlap at all"
  (check-type f1 frame)
  (check-type f2 frame)
  (and (and (frame-p f1) (frame-p f2))
       (let ((frame (frame-intersect f1 f2)))
         (values (and (plusp (frame-width frame))
                      (plusp (frame-height frame)))))))

(defun frame-raise-window (g f w &optional (focus t))
  "Raise the window w in frame f in group g. if FOCUS is
T (default) then also focus the frame."
  (let ((oldw (frame-window f)))
    ;; nothing to do when W is nil
    (setf (frame-window f) w)
    (unless (and w (eq oldw w))
      (if w
          (raise-window w)
          (mapc 'hide-window (frame-windows g f))))
    ;; If raising a window in the current frame we must focus it or
    ;; the group and screen will get out of sync.
    (when (or focus
              (eq (tile-group-current-frame g) f))
      (focus-frame g f))
    (when (and w (not (window-modal-p w)))
      (raise-modals-of w))))

(defun focus-frame (group f)
  (let ((w (frame-window f))
        (last (tile-group-current-frame group))
        (show-indicator nil))
    (setf (tile-group-current-frame group) f)
    ;; record the last frame to be used in the fother command.
    (unless (eq f last)
      (setf (tile-group-last-frame group) last)
      (run-hook-with-args *focus-frame-hook* f last)
      (setf show-indicator t))
    (if w
        (focus-window w)
        (no-focus group (frame-window last)))
    (if show-indicator
        (show-frame-indicator group)
        (show-frame-outline group))))

(defun frame-windows (group f)
  (remove-if-not (lambda (w) (eq (window-frame w) f))
                 (group-windows group)))

(defun frame-sort-windows (group f)
  (remove-if-not (lambda (w) (eq (window-frame w) f))
                 (sort-windows group)))

(defun copy-frame-tree (tree)
  "Return a copy of the frame tree."
  (cond ((null tree) tree)
        ((typep tree 'frame)
         (copy-structure tree))
        (t
         (mapcar #'copy-frame-tree tree))))

(defun group-frames (group)
  (tree-accum-fn (tile-group-frame-tree group) 'nconc 'list))

(defun head-frames (group head)
  (tree-accum-fn (tile-group-frame-head group head) 'nconc 'list))

(defun find-free-frame-number (group)
  (find-free-number (mapcar 'frame-number (group-frames group))))

(defun choose-new-frame-window (frame group)
  "Find out what window should go in a newly created frame."
  (let ((win (case *new-frame-action*
               (:last-window (other-hidden-window group))
               (t nil))))
    (setf (frame-window frame) win)
    (when win
      (setf (window-frame win) frame))))

(defun split-frame-h (group p ratio)
  "Return 2 new frames. The first one stealing P's number and window"
  (let* ((w (ratio-or-pixel (frame-width p) ratio))
         (h (frame-height p))
         (f1 (make-frame :number (frame-number p)
                         :x (frame-x p)
                         :y (frame-y p)
                         :width w
                         :height h
                         :window (frame-window p)))
         (f2 (make-frame :number (find-free-frame-number group)
                         :x (+ (frame-x p) w)
                         :y (frame-y p)
                         ;; gobble up the modulo
                         :width (- (frame-width p) w)
                         :height h
                         :window nil)))
    (run-hook-with-args *split-frame-hook* p f1 f2)
    (run-hook-with-args *new-frame-hook* f2)
    (values f1 f2)))

(defun split-frame-v (group p ratio)
  "Return 2 new frames. The first one stealing P's number and window"
  (let* ((w (frame-width p))
         (h (ratio-or-pixel (frame-height p) ratio))
         (f1 (make-frame :number (frame-number p)
                         :x (frame-x p)
                         :y (frame-y p)
                         :width w
                         :height h
                         :window (frame-window p)))
         (f2 (make-frame :number (find-free-frame-number group)
                         :x (frame-x p)
                         :y (+ (frame-y p) h)
                         :width w
                         ;; gobble up the modulo
                         :height (- (frame-height p) h)
                         :window nil)))
    (run-hook-with-args *split-frame-hook* p f1 f2)
    (run-hook-with-args *new-frame-hook* f2)
    (values f1 f2)))

(defun ratio-or-pixel (length ratio)
  "Return a ratio of length unless ratio is an integer.
If ratio is an integer return the number of pixel desired."
  (if (integerp ratio)
      ratio
      (truncate (* length ratio))))

(defun funcall-on-leaf (tree leaf fn)
  "Return a new tree with LEAF replaced with the result of calling FN on LEAF."
  (cond ((atom tree)
         (if (eq leaf tree)
             (funcall fn leaf)
             tree))
        (t (mapcar (lambda (sib)
                     (funcall-on-leaf sib leaf fn))
                   tree))))

(defun funcall-on-node (tree fn match)
  "Call fn on the node where match returns t."
  (if (funcall match tree)
      (funcall fn tree)
      (cond ((atom tree) tree)
            (t (mapcar (lambda (sib)
                         (funcall-on-node sib fn match))
                       tree)))))

(defun replace-frame-in-tree (tree f &rest frames)
  (funcall-on-leaf tree f (lambda (f)
                            (declare (ignore f))
                            frames)))

(defun sibling-internal (tree leaf fn)
  "helper for next-sibling and prev-sibling."
  (cond ((atom tree) nil)
        ((find leaf tree)
         (let* ((rest (cdr (member leaf (funcall fn tree))))
                (pick (car (if-null rest (funcall fn tree) rest))))
           (unless (eq pick leaf)
             pick)))
        (t (find-if (lambda (x)
                      (sibling-internal x leaf fn))
                    tree))))

(defun next-sibling (tree leaf)
  "Return the sibling of LEAF in TREE."
  (sibling-internal tree leaf 'identity))

(defun prev-sibling (tree leaf)
  (sibling-internal tree leaf 'reverse))

(defun closest-sibling (tree leaf)
  "Return the sibling to the right/below of leaf or left/above if
leaf is the most right/below of its siblings."
  (let* ((parent (tree-parent tree leaf))
         (lastp (= (position leaf parent) (1- (length parent)))))
    (if lastp
        (prev-sibling parent leaf)
        (next-sibling parent leaf))))

(defun migrate-frame-windows (group src dest)
  "Migrate all windows in SRC frame to DEST frame."
  (mapc (lambda (w)
          (when (eq (window-frame w) src)
            (setf (window-frame w) dest)))
        (group-windows group)))

(defun tree-accum-fn (tree acc fn)
  "Run an accumulator function on fn applied to each leaf"
  (cond ((null tree) nil)
        ((atom tree)
         (funcall fn tree))
        (t (apply acc (mapcar (lambda (x) (tree-accum-fn x acc fn)) tree)))))

(defun tree-iterate (tree fn)
  "Call FN on every leaf in TREE"
  (cond ((null tree) nil)
        ((atom tree)
         (funcall fn tree))
        (t (mapc (lambda (x) (tree-iterate x fn)) tree))))

(defun tree-x (tree)
  (tree-accum-fn tree 'min 'frame-x))

(defun tree-y (tree)
  (tree-accum-fn tree 'min 'frame-y))

(defun tree-width (tree)
  (cond ((atom tree) (frame-width tree))
        ((tree-row-split tree)
         ;; in row splits, all children have the same width, so use the
         ;; first one.
         (tree-width (first tree)))
        (t
         ;; for column splits we add the width of each child
         (reduce '+ tree :key 'tree-width))))

(defun tree-height (tree)
  (cond ((atom tree) (frame-height tree))
        ((tree-column-split tree)
         ;; in row splits, all children have the same width, so use the
         ;; first one.
         (tree-height (first tree)))
        (t
         ;; for column splits we add the width of each child
         (reduce '+ tree :key 'tree-height))))

(defun tree-parent (top node)
  "Return the list in TOP that contains NODE."
  (cond ((atom top) nil)
        ((find node top) top)
        (t (loop for i in top
                 thereis (tree-parent i node)))))

(defun tree-leaf (top)
  "Return a leaf of the tree. Use this when you need a leaf but
you don't care which one."
  (tree-accum-fn top
                 (lambda (&rest siblings)
                   (car siblings))
                 #'identity))

(defun tree-row-split (tree)
  "Return t if the children of tree are stacked vertically"
  (loop for i in (cdr tree)
        with head = (car tree)
        always (= (tree-x head) (tree-x i))))

(defun tree-column-split (tree)
  "Return t if the children of tree are side-by-side"
  (loop for i in (cdr tree)
        with head = (car tree)
        always (= (tree-y head) (tree-y i))))

(defun tree-split-type (tree)
  "return :row or :column"
  (cond ((tree-column-split tree) :column)
        ((tree-row-split tree) :row)
        (t (error "tree-split-type unknown"))))

(defun offset-tree (tree x y)
  "move the screen's frames around."
  (tree-iterate tree (lambda (frame)
                       (incf (frame-x frame) x)
                       (incf (frame-y frame) y))))

(defun offset-tree-dir (tree amount dir)
  (ecase dir
    (:left   (offset-tree tree (- amount) 0))
    (:right  (offset-tree tree amount 0))
    (:top    (offset-tree tree 0 (- amount)))
    (:bottom (offset-tree tree 0 amount))))

(defun expand-tree (tree amount dir)
  "expand the frames in tree by AMOUNT in DIR direction. DIR can be :top :bottom :left :right"
  (labels ((expand-frame (f amount dir)
             (ecase dir
               (:left   (decf (frame-x f) amount)
                        (incf (frame-width f) amount))
               (:right  (incf (frame-width f) amount))
               (:top    (decf (frame-y f) amount)
                        (incf (frame-height f) amount))
               (:bottom (incf (frame-height f) amount)))))
    (cond ((null tree) nil)
          ((atom tree)
           (expand-frame tree amount dir))
          ((or (and (find dir '(:left :right))
                    (tree-row-split tree))
               (and (find dir '(:top :bottom))
                    (tree-column-split tree)))
           (dolist (i tree)
             (expand-tree i amount dir)))
          (t
           (let* ((children (if (find dir '(:left :top))
                              (reverse tree)
                              tree))
                  (sz-fn (if (find dir '(:left :right))
                           'tree-width
                           'tree-height))
                  (total (funcall sz-fn tree))
                  (amt-list (loop for i in children
                                  for old-sz = (funcall sz-fn i)
                                  collect (floor (* amount old-sz) total)))
                  (remainder (- amount (apply '+ amt-list)))
                  (ofs 0))
             ;; spread the remainder out as evenly as possible
             (assert (< remainder (length amt-list)))
             (loop for i upfrom 0
                   while (> remainder 0)
                   do
                   (incf (nth i amt-list))
                   (decf remainder))
             ;; resize proportionally
             (loop for i in children
                   for amt in amt-list
                   do
                   (expand-tree i amt dir)
                   (offset-tree-dir i ofs dir)
                   (incf ofs amt)))))))

(defun join-subtrees (tree leaf)
  "expand the children of tree to occupy the space of
LEAF. Return tree with leaf removed."
  (let* ((others (remove leaf tree))
         (newtree (if (= (length others) 1)
                      (car others)
                      others))
         (split-type (tree-split-type tree))
         (dir (if (eq split-type :column) :right :bottom))
         (ofsdir (if (eq split-type :column) :left :top))
         (amt (if (eq split-type :column)
                  (tree-width leaf)
                  (tree-height leaf)))
         (after (cdr (member leaf tree))))
    ;; align all children after the leaf with the edge of the
    ;; frame before leaf.
    (offset-tree-dir after amt ofsdir)
    (expand-tree newtree amt dir)
    newtree))

(defun resize-tree (tree w h &optional (x (tree-x tree)) (y (tree-y tree)))
  "Scale TREE to width W and height H, ignoring aspect. If X and Y are
  provided, reposition the TREE as well."
  (let* ((tw (tree-width tree))
         (th (tree-height tree))
         (tx (tree-x tree))
         (ty (tree-y tree))
         (wf (/ w tw))
         (hf (/ h th)))
    (tree-iterate tree (lambda (f)
                         (setf (frame-height f) (round (* (frame-height f) hf))
                               (frame-y f) (+ (round (* (- (frame-y f) ty) hf)) y)
                               (frame-width f) (round (* (frame-width f) wf))
                               (frame-x f) (+ (round (* (- (frame-x f) tx) wf)) x))))
    (dformat 4 "resize-tree ~Dx~D -> ~Dx~D~%" tw th (tree-width tree) (tree-height tree))))

(defun remove-frame (tree leaf)
  "Return a new tree with LEAF and it's sibling merged into
one."
  (cond ((atom tree) tree)
        ((find leaf tree)
         (join-subtrees tree leaf))
        (t (mapcar (lambda (sib)
                     (remove-frame sib leaf))
                   tree))))

(defun sync-frame-windows (group frame)
  "synchronize windows attached to FRAME."
  (mapc (lambda (w)
          (when (eq (window-frame w) frame)
            (dformat 3 "maximizing ~S~%" w)
            (maximize-window w)))
        (group-windows group)))

(defun sync-all-frame-windows (group)
  "synchronize all frames in GROUP."
  (let ((tree (tile-group-frame-tree group)))
    (tree-iterate tree
                  (lambda (f)
                    (sync-frame-windows group f)))))

(defun sync-head-frame-windows (group head)
  "synchronize all frames in GROUP and HEAD."
  (dolist (f (head-frames group head))
    (sync-frame-windows group f)))

(defun offset-frames (group x y)
  "move the screen's frames around."
  (let ((tree (tile-group-frame-tree group)))
    (tree-iterate tree (lambda (frame)
                         (incf (frame-x frame) x)
                         (incf (frame-y frame) y)))))

(defun resize-frame (group frame amount dim)
  "Resize FRAME by AMOUNT in DIM dimension, DIM can be
either :width or :height"
  (check-type group group)
  (check-type frame frame)
  (check-type amount integer)
  ;; (check-type dim (member :width :height))
  (labels ((max-amount (parent node min dim-fn)
             (let ((right-sibling (cadr (member node parent)))
                   (left-sibling (cadr (member node (reverse parent)))))

               (dformat 10 "max ~@{~a~^ ~}~%" parent node min dim-fn right-sibling left-sibling)
               (if parent
                   (cond (right-sibling
                          (max 0 (- (funcall dim-fn right-sibling) min)))
                         (left-sibling
                          (max 0 (- (funcall dim-fn left-sibling) min)))
                         (t 0))
                   ;; no parent means the frame can't get any bigger.
                   0))))
    (let* ((tree (tile-group-frame-tree group))
           (parent (tree-parent tree frame))
           (gparent (tree-parent tree parent))
           (split-type (tree-split-type parent)))
      (dformat 10 "~s ~s parent: ~s ~s width: ~s h: ~s~%" dim amount split-type parent (tree-width parent) (tree-height parent))
      ;; normalize amount
      (let* ((max (ecase dim
                    (:width
                     (if (>= (frame-width frame) (frame-width (frame-head group frame)))
                         0
                         (if (eq split-type :column)
                             (max-amount parent frame *min-frame-width* 'tree-width)
                             (max-amount gparent parent *min-frame-width* 'tree-width))))
                    (:height
                     (if (>= (frame-height frame) (frame-height (frame-head group frame)))
                         0
                         (if (eq split-type :row)
                             (max-amount parent frame *min-frame-height* 'tree-height)
                             (max-amount gparent parent *min-frame-height* 'tree-height))))))
             (min (ecase dim
                    ;; Frames taking up the entire HEAD in one
                    ;; dimension can't be resized in that dimension.
                    (:width
                     (if (and (eq split-type :row)
                              (or (null gparent)
                                  (>= (frame-width frame) (frame-width (frame-head group frame)))))
                         0
                         (- *min-frame-width* (frame-width frame))))
                    (:height
                     (if (and (eq split-type :column)
                              (or (null gparent)
                                  (>= (frame-height frame) (frame-height (frame-head group frame)))))
                         0
                         (- *min-frame-height* (frame-height frame)))))))
        (setf amount (max (min amount max) min))
        (dformat 10 "bounds ~d ~d ~d~%" amount max min))
      ;; if FRAME is taking up the whole DIM or if AMOUNT = 0, do nothing
      (unless (zerop amount)
        (let* ((resize-parent (or (and (eq split-type :column)
                                       (eq dim :height))
                                  (and (eq split-type :row)
                                       (eq dim :width))))
               (to-resize (if resize-parent parent frame))
               (to-resize-parent (if resize-parent gparent parent))
               (lastp (= (position to-resize to-resize-parent) (1- (length to-resize-parent))))
               (to-shrink (if lastp
                              (prev-sibling to-resize-parent to-resize)
                              (next-sibling to-resize-parent to-resize))))
          (expand-tree to-resize amount (ecase dim
                                          (:width (if lastp :left :right))
                                          (:height (if lastp :top :bottom))))
          (expand-tree to-shrink (- amount) (ecase dim
                                              (:width (if lastp :right :left))
                                              (:height (if lastp :bottom :top))))
          (unless (and *resize-hides-windows* (eq *top-map* *resize-map*))
            (tree-iterate to-resize
                          (lambda (leaf)
                            (sync-frame-windows group leaf)))
            (tree-iterate to-shrink
                          (lambda (leaf)
                            (sync-frame-windows group leaf)))))))))

(defun balance-frames-internal (group tree)
  "Resize all the children of tree to be of equal width or height
depending on the tree's split direction."
  (let* ((split-type (tree-split-type tree))
         (fn (if (eq split-type :column)
                 'tree-width
                 'tree-height))
         (side (if (eq split-type :column)
                   :right
                   :bottom))
         (total (funcall fn tree))
         size rem)
    (multiple-value-setq (size rem) (truncate total (length tree)))
    (loop
     for i in tree
     for j = rem then (1- j)
     for totalofs = 0 then (+ totalofs ofs)
     for ofs = (+ (- size (funcall fn i)) (if (plusp j) 1 0))
     do
     (expand-tree i ofs side)
     (offset-tree-dir i totalofs side)
     (tree-iterate i (lambda (leaf)
                       (sync-frame-windows group leaf))))))

(defun split-frame (group how &optional (ratio 1/2))
  "Split the current frame into 2 frames. Return new frame number, if
it succeeded. NIL otherwise. RATIO is a fraction of the screen to
allocate to the new split window. If ratio is an integer then the
number of pixels will be used. This can be handy to setup the
desktop when starting."
  (check-type how (member :row :column))
  (let* ((frame (tile-group-current-frame group))
         (head (frame-head group frame)))
    ;; don't create frames smaller than the minimum size
    (when (or (and (eq how :row)
                   (>= (frame-height frame) (* *min-frame-height* 2)))
              (and (eq how :column)
                   (>= (frame-width frame) (* *min-frame-width* 2))))
      (multiple-value-bind (f1 f2) (funcall (if (eq how :column)
                                                'split-frame-h
                                                'split-frame-v)
                                            group frame ratio)
        (setf (tile-group-frame-head group head)
              (if (atom (tile-group-frame-head group head))
                  (list f1 f2)
                  (funcall-on-node (tile-group-frame-head group head)
                                   (lambda (tree)
                                     (if (eq (tree-split-type tree) how)
                                         (list-splice-replace frame tree f1 f2)
                                         (substitute (list f1 f2) frame tree)))
                                   (lambda (tree)
                                     (unless (atom tree)
                                       (find frame tree))))))
        (migrate-frame-windows group frame f1)
        (choose-new-frame-window f2 group)
        (if (eq (tile-group-current-frame group)
                frame)
            (setf (tile-group-current-frame group) f1))
        (setf (tile-group-last-frame group) f2)
        (sync-frame-windows group f1)
        (sync-frame-windows group f2)
        ;; we also need to show the new window in the other frame
        (when (frame-window f2)
          (unhide-window (frame-window f2)))
        (frame-number f2)))))

(defun draw-frame-outline (group f tl br)
  "Draw an outline around FRAME."
  (let* ((screen (group-screen group))
         (win (if (frame-window f) (window-xwin (frame-window f)) (screen-root screen)))
         (width (screen-frame-outline-width screen))
         (gc (screen-frame-outline-gc screen))
         (halfwidth (/ width 2)))
    (when (> width 0)
      (let ((x (frame-x f))
	    (y (frame-display-y group f))
	    (w (frame-width f))
	    (h (frame-display-height group f)))
	(when tl
	  (xlib:draw-line win gc
			  x (+ halfwidth y) w 0 t)
	  (xlib:draw-line win gc
			  (+ halfwidth x) y 0 h t))
	(when br
	  (xlib:draw-line win gc
			  (+ x (- w halfwidth)) y 0 h t)
	  (xlib:draw-line win gc
			  x (+ y (- h halfwidth)) w 0 t))))))

(defun draw-frame-outlines (group &optional head)
  "Draw an outline around all frames in GROUP."
  (clear-frame-outlines group)
  (dolist (h (if head (list head) (group-heads group)))
    (draw-frame-outline group h nil t)
    (tree-iterate (tile-group-frame-head group h) (lambda (f)
                                                    (draw-frame-outline group f t nil)))))

(defun clear-frame-outlines (group)
  "Clear the outlines drawn with DRAW-FRAME-OUTLINES."
  (xlib:clear-area (screen-root (group-screen group))))

(defun draw-frame-numbers (group)
  "Draw the number of each frame in its corner. Return the list of
windows used to draw the numbers in. The caller must destroy them."
  (let ((screen (group-screen group)))
    (mapcar (lambda (f)
              (let ((w (xlib:create-window
                        :parent (screen-root screen)
                        :x (frame-x f) :y (frame-display-y group f) :width 1 :height 1
                        :background (screen-fg-color screen)
                        :border (screen-border-color screen)
                        :border-width 1
                        :event-mask '())))
                (xlib:map-window w)
                (setf (xlib:window-priority w) :above)
                (echo-in-window w (screen-font screen)
                                (screen-fg-color screen)
                                (screen-bg-color screen)
                                (string (get-frame-number-translation f)))
                (xlib:display-finish-output *display*)
                (dformat 3 "mapped ~S~%" (frame-number f))
                w))
            (group-frames group))))

(defmacro save-frame-excursion (&body body)
  "Execute body and then restore the current frame."
  (let ((oframe (gensym "OFRAME"))
        (ogroup (gensym "OGROUP")))
    `(let ((,oframe (tile-group-current-frame (current-group)))
           (,ogroup (current-group)))
      (unwind-protect (progn ,@body)
        (focus-frame ,ogroup ,oframe)))))

;;; Frame commands

(defun split-frame-in-dir (group dir &optional (ratio 1/2))
  (let ((f (tile-group-current-frame group)))
    (if (split-frame group dir ratio)
        (progn
          (when (frame-window f)
            (update-decoration (frame-window f)))
          (show-frame-indicator group))
        (message "Cannot split smaller than minimum size."))))

(defun move-focus-and-or-window-to (frame-number &optional win-p)
  (declare (type
	    (member (mapcar 'frame-number (group-frames (current-group))) frame-number)))
  (let* ((group (current-group))
         (new-frame frame-number)
	 (window (current-window)))
    (when new-frame
      (if (and win-p window)
          (pull-window window new-frame)
	(focus-frame group new-frame)))))

(defcommand (hsplit tile-group) (&optional (ratio "1/2")) (:string)
  "Split the current frame into 2 side-by-side frames."
  (split-frame-in-dir (current-group) :column (read-from-string ratio)))

(defcommand (vsplit tile-group) (&optional (ratio "1/2")) (:string)
  "Split the current frame into 2 frames, one on top of the other."
  (split-frame-in-dir (current-group) :row (read-from-string ratio)))

(defcommand (remove-split tile-group) (&optional (group (current-group)) (frame (tile-group-current-frame group))) ()
  "Remove the specified frame in the specified group (defaults to current
group, current frame). Windows in the frame are migrated to the frame taking up its
space."
  (let* ((head (frame-head group frame))
         (current (tile-group-current-frame group))
         (tree (tile-group-frame-head group head))
         (s (closest-sibling (list tree) frame))
         ;; grab a leaf of the siblings. The siblings doesn't have to be
         ;; a frame.
         (l (tree-accum-fn s
                           (lambda (&rest siblings)
                             (car siblings))
                           #'identity)))
    ;; Only remove the current frame if it has a sibling
    (if (atom tree)
        (message "No more frames!")
        (when s
          (when (frame-is-head group frame)
            (setf (frame-number l) (frame-number frame)))
          ;; Move the windows from the removed frame to its sibling
          (migrate-frame-windows group frame l)
          ;; If the frame has no window, give it the current window of
          ;; the current frame.
          (unless (frame-window l)
            (setf (frame-window l)
                  (frame-window frame)))
          ;; Unsplit
          (setf (tile-group-frame-head group head) (remove-frame tree frame))
          ;; update the current frame and sync all windows
          (when (eq frame current)
            (setf (tile-group-current-frame group) l))
          (tree-iterate tree
                        (lambda (leaf)
                          (sync-frame-windows group leaf)))
          (frame-raise-window group l (frame-window l) nil)
          (when (frame-window l)
            (update-decoration (frame-window l)))
          (when (eq frame current)
            (show-frame-indicator group))))))

(defcommand-alias remove remove-split)

(defcommand (only tile-group) () ()
  "Delete all the frames but the current one and grow it to take up the entire head."
  (let* ((screen (current-screen))
         (group (screen-current-group screen))
         (win (group-current-window group))
         (head (current-head group))
         (frame (copy-frame head)))
    (if (atom (tile-group-frame-head group head))
        (message "There's only one frame.")
        (progn
          (mapc (lambda (w)
                  ;; windows in other frames disappear
                  (unless (eq (window-frame w) (tile-group-current-frame group))
                    (hide-window w))
                  (setf (window-frame w) frame))
                (head-windows group head))
          (setf (frame-window frame) win
                (tile-group-frame-head group head) frame
                (tile-group-current-frame group) frame)
          (focus-frame group frame)
          (if (frame-window frame)
              (update-decoration (frame-window frame))
              (show-frame-indicator group))
          (sync-frame-windows group (tile-group-current-frame group))))))

(defcommand (curframe tile-group) () ()
"Display a window indicating which frame is focused."
  (show-frame-indicator (current-group) t))

(defun focus-frame-next-sibling (group)
  (let* ((sib (next-sibling (tile-group-frame-tree group)
                            (tile-group-current-frame group))))
    (when sib
      (focus-frame group (tree-accum-fn sib
                                        (lambda (x y)
                                          (declare (ignore y))
                                          x)
                                        'identity))
      (show-frame-indicator group))))

(defun focus-last-frame (group)
  ;; make sure the last frame still exists in the frame tree
  (when (and (tile-group-last-frame group)
             (find (tile-group-last-frame group) (group-frames group)))
    (focus-frame group (tile-group-last-frame group))))

(defun focus-frame-after (group frames)
  "Given a list of frames focus the next one in the list after
the current frame."
  (let ((rest (cdr (member (tile-group-current-frame group) frames :test 'eq))))
    (focus-frame group
                 (if-null rest
                     (car frames)
                     (car rest)))))

(defun focus-next-frame (group)
  (focus-frame-after group (group-frames group)))

(defun focus-prev-frame (group)
  (focus-frame-after group (nreverse (group-frames group))))

(defcommand (fnext tile-group) () ()
"Cycle through the frame tree to the next frame."
  (focus-next-frame (current-group)))

(defcommand (fprev tile-group) () ()
"Cycle through the frame tree to the previous frame."
  (focus-prev-frame (current-group)))

(defcommand (sibling tile-group) () ()
"Jump to the frame's sibling. If a frame is split into two frames,
these two frames are siblings."
  (focus-frame-next-sibling (current-group)))

(defcommand (fother tile-group) () ()
"Jump to the last frame that had focus."
  (focus-last-frame (current-group)))

(defun choose-frame-by-number (group)
  "show a number in the corner of each frame and wait for the user to
select one. Returns the selected frame or nil if aborted."
  (let* ((wins (progn
                 (draw-frame-outlines group)
                 (draw-frame-numbers group)))
         (ch (read-one-char (group-screen group)))
         (num (read-from-string (string ch) nil nil)))
    (dformat 3 "read ~S ~S~%" ch num)
    (mapc #'xlib:destroy-window wins)
    (clear-frame-outlines group)
    (find ch (group-frames group)
          :test 'char=
          :key 'get-frame-number-translation)))


(defcommand (fselect tile-group) (frame-number) ((:frame t))
"Display a number in the corner of each frame and let the user to
select a frame by number. If @var{frame-number} is specified, just
jump to that frame."
  (let ((group (current-group)))
    (focus-frame group frame-number)))

(defcommand (resize tile-group) (width height) ((:number "Input new width: ")
                                                (:number "Input new height: "))
  "Resize the current frame by @var{width} and @var{height} pixels"
  (let* ((group (current-group))
         (f (tile-group-current-frame group)))
    (if (atom (tile-group-frame-tree group))
        (message "No more frames!")
        (progn
          (clear-frame-outlines group)
          (resize-frame group f width :width)
          (resize-frame group f height :height)
          (draw-frame-outlines group (current-head))))))

(defun clear-frame (frame group)
  "Clear the given frame."
  (frame-raise-window group frame nil (eq (tile-group-current-frame group) frame)))

(defcommand (fclear tile-group) () ()
"Clear the current frame."
  (clear-frame (tile-group-current-frame (current-group)) (current-group)))

(defun get-edge (frame edge)
  "Returns the specified edge of FRAME.  Valid values for EDGE are :TOP, :BOTTOM, :LEFT, and :RIGHT.
  An edge is a START, END, and OFFSET. For horizontal edges, START is the left coordinate, END is
  the right coordinate, and OFFSET is the Y coordinate.  Similarly, for vertical lines, START is
  top, END is bottom, and OFFSET is X coordinate."
  (let* ((x1 (frame-x frame))
         (y1 (frame-y frame))
         (x2 (+ x1 (frame-width frame)))
         (y2 (+ y1 (frame-height frame))))
    (ecase edge
      (:top
       (values x1 x2 y1))
      (:bottom
       (values x1 x2 y2))
      (:left
       (values y1 y2 x1))
      (:right
       (values y1 y2 x2)))))

(defun neighbour (direction frame frameset)
  "Returns the best neighbour of FRAME in FRAMESET on the DIRECTION edge.
   Valid directions are :UP, :DOWN, :LEFT, :RIGHT.
   eg: (NEIGHBOUR :UP F FS) finds the frame in FS that is the 'best'
   neighbour above F."
  (let ((src-edge (ecase direction
                    (:up :top)
                    (:down :bottom)
                    (:left :left)
                    (:right :right)))
        (opposite (ecase direction
                    (:up :bottom)
                    (:down :top)
                    (:left :right)
                    (:right :left)))
        (best-frame nil)
        (best-overlap 0))
    (multiple-value-bind (src-s src-e src-offset)
        (get-edge frame src-edge)
      (dolist (f frameset)
        (multiple-value-bind (s e offset)
            (get-edge f opposite)
          (let ((overlap (- (min src-e e)
                            (max src-s s))))
            ;; Two edges are neighbours if they have the same offset and their starts and ends
            ;; overlap.  We want to find the neighbour that overlaps the most.
            (when (and (= src-offset offset)
                       (> overlap best-overlap))
              (setf best-frame f)
              (setf best-overlap overlap))))))
    best-frame))

(defun move-focus-and-or-window (dir &optional win-p)
  (declare (type (member :up :down :left :right) dir))
  (let* ((group (current-group))
         (new-frame (neighbour dir (tile-group-current-frame group) (group-frames group)))
         (window (current-window)))
    (when new-frame
      (if (and win-p window)
          (pull-window window new-frame)
          (focus-frame group new-frame)))))

(defcommand (move-focus tile-group) (dir) ((:direction "To what direction? "))
"Focus the frame adjacent to the current one in the specified
direction. The following are valid directions:
@table @asis
@item up
@item down
@item left
@item right
@end table"
  (move-focus-and-or-window dir))

(defcommand (move-window tile-group) (dir) ((:direction "To what direction? "))
"Just like move-focus except that the current is pulled along."
  (move-focus-and-or-window dir t))

(defcommand (next-in-frame tile-group) () ()
"Go to the next window in the current frame."
  (let ((group (current-group)))
    (if (group-current-window group)
        (focus-forward group (frame-sort-windows group (tile-group-current-frame group)))
        (other-window-in-frame group))))

(defcommand (prev-in-frame tile-group) () ()
"Go to the previous window in the current frame."
  (let ((group (current-group)))
    (if (group-current-window group)
        (focus-forward group (reverse (frame-sort-windows group (tile-group-current-frame group))))
        (other-window-in-frame group))))

(defcommand (other-in-frame tile-group) () ()
"Go to the last accessed window in the current frame."
  (other-window-in-frame (current-group)))

(defcommand (balance-frames tile-group) () ()
  "Make frames the same height or width in the current frame's subtree."
  (let* ((group (current-group))
         (tree (tree-parent (tile-group-frame-head group (current-head))
                            (tile-group-current-frame group))))
    (if tree
        (balance-frames-internal (current-group) tree)
        (message "There's only one frame."))))

(defcommand (move-window-to-frame tile-group) (frame-number) ((:frame "Move to what frame? "))
  "Just like move-focus except that the current is pulled along."
  (move-focus-and-or-window-to frame-number t))
