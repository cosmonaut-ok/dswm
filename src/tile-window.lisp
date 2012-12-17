;; Copyright (C) 2003-2008 Shawn Betts
;; Copyright (C) 2010-2012 Alexander aka CosmonauT Vynnyk
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
;; Window functionality.
;;
;; Code:

(in-package :dswm)

(export '(remember-window
	  remember-windows-current-group
	  remember-windows-current-screen
	  remember-all-windows
	  forget-window
	  forget-windows-current-group
	  forget-windows-current-screen
	  forget-all-windows
	  redisplay
	  frame-windowlist))

(defclass tile-window (window)
  ((frame   :initarg :frame   :accessor window-frame)))

(defmethod update-decoration ((window tile-window))
  ;; give it a colored border but only if there are more than 1 frames.
  (let* ((group (window-group window))
         (screen (group-screen group)))
    (let ((c (if (and (> (length (group-frames group)) 1)
                      (eq (group-current-window group) window))
                 (screen-focus-color screen)
                 (screen-unfocus-color screen))))
      (setf (xlib:window-border (window-parent window)) c
            ;; windows that dont fill the entire screen have a transparent background.
            (xlib:window-background (window-parent window))
            (if (eq (window-type window) :normal)
                (if (eq *window-border-style* :thick)
                    c
                    (screen-win-bg-color screen))
                :none))
      ;; get the background updated
      (xlib:clear-area (window-parent window)))))

(defmethod window-sync ((window tile-window) hint)
  (case hint
    ((:normal-hints :type)
     (maximize-window window))))

(defmethod window-visible-p ((window tile-window))
  ;; In this case, visible means the window is the top window in the
  ;; frame. This is not entirely true when it doesn't take up the
  ;; entire frame and there's a window below it.
  (eq window
      (frame-window (window-frame window))))

(defmethod window-head ((window tile-window))
  (frame-head (window-group window) (window-frame window)))

(defmethod (setf window-fullscreen) :after (val (window tile-window))
  (if val
      (maximize-window window)
      (progn
        (setf (xlib:drawable-border-width (window-parent window)) (default-border-width-for-type window))
        (maximize-window window))))

;;;;

(defun really-raise-window (window)
  (frame-raise-window (window-group window) (window-frame window) window))

(defun raise-modals-of (window)
  (mapc 'really-raise-window (modals-of window)))

(defun raise-modals-of-gang (window)
  (mapc 'really-raise-window (only-modals (window-gang window))))

(defun raise-transients-of-gang (window)
  (mapc 'really-raise-window (only-transients (window-gang window))))

;;;

(defun geometry-hints (win)
  "Return hints for max width and height and increment hints. These
hints have been modified to always be defined and never be greater
than the root window's width and height."
  (let* ((f (window-frame win))
         (x (frame-x f))
         (y (frame-display-y (window-group win) f))
         (border (xlib:drawable-border-width (window-parent win)))
         (fwidth (- (frame-width f) (* 2 border)))
         (fheight (- (frame-display-height (window-group win) f)
                     (* 2 border)))
         (width fwidth)
         (height fheight)
         (hints (window-normal-hints win))
         (hints-min-width (and hints (xlib:wm-size-hints-min-width hints)))
         (hints-min-height (and hints (xlib:wm-size-hints-min-height hints)))
         (hints-max-width (and hints (xlib:wm-size-hints-max-width hints)))
         (hints-max-height (and hints (xlib:wm-size-hints-max-height hints)))
         (hints-width (and hints (xlib:wm-size-hints-base-width hints)))
         (hints-height (and hints (xlib:wm-size-hints-base-height hints)))
         (hints-inc-x (and hints (xlib:wm-size-hints-width-inc hints)))
         (hints-inc-y (and hints (xlib:wm-size-hints-height-inc hints)))
         (hints-min-aspect (and hints (xlib:wm-size-hints-min-aspect hints)))
         (hints-max-aspect (and hints (xlib:wm-size-hints-max-aspect hints)))
         (border (case *window-border-style*
                   (:none 0)
                   (t (default-border-width-for-type win))))
         center)
    ;;    (dformat 4 "hints: ~s~%" hints)
    ;; determine what the width and height should be
    (cond
      ;; handle specially fullscreen windows.
      ((window-fullscreen win)
       (let ((head (frame-head (window-group win) f)))
         (setf x (frame-x head)
               y (frame-y head)
               width (frame-width head)
               height (frame-height head)
               (xlib:window-priority (window-parent win)) :above))
       (return-from geometry-hints (values x y 0 0 width height 0 t)))
      ;; Adjust the defaults if the window is a transient_for window.
      ((find (window-type win) '(:transient :dialog))
       (setf center t
             width (min (max (or hints-width 0)
                             (or hints-min-width 0)
                             (window-width win))
                        width)
             height (min (max (or hints-height 0)
                              (or hints-min-height 0)
                              (window-height win))
                         height)))
      ;; aspect hints are handled similar to max size hints
      ((and hints-min-aspect hints-max-aspect)
       (let ((ratio (/ width height)))
         (cond ((< ratio hints-min-aspect)
                (setf height (ceiling width hints-min-aspect)))
               ((> ratio hints-max-aspect)
                (setf width  (ceiling (* height hints-max-aspect)))))
         (setf center t)))
      ;; Update our defaults if the window has the maxsize hints
      ((or hints-max-width hints-max-height)
       (when (and hints-max-width
                  (< hints-max-width width))
         (setf width hints-max-width))
       (when (and hints-max-height
                  (< hints-max-height height))
         (setf height hints-max-height))
       (setf center t))
      (t
       ;; if they have inc hints then start with the size and adjust
       ;; based on those increments until the window fits in the frame
       (when (and hints-inc-x (plusp hints-inc-x))
         (let ((w (or hints-width (window-width win))))
           (setf width (+ w (* hints-inc-x
                               (+ (floor (- fwidth w) hints-inc-x)))))))
       (when (and hints-inc-y (plusp hints-inc-y))
         (let ((h (or hints-height (window-height win))))
           (setf height (+ h (* hints-inc-y
                                (+ (floor (- fheight h -1) hints-inc-y)))))))))
    ;; adjust for gravity
    (multiple-value-bind (wx wy) (get-gravity-coords (gravity-for-window win)
                                                     width height
                                                     0 0
                                                     fwidth fheight)
      (when (or center
                (find *window-border-style* '(:tight :none)))
        (setf x (+ wx (frame-x f))
              y (+ wy (frame-display-y (window-group win) f))
              wx 0 wy 0))
      ;; Now return our findings
      (values x y wx wy width height border center))))

(defun maximize-window (win)
  "Maximize the window."
  (multiple-value-bind (x y wx wy width height border stick)
      (geometry-hints win)
    (dformat 4 "maximize window ~a x: ~d y: ~d width: ~d height: ~d border: ~d stick: ~s~%" win x y width height border stick)
    ;; This is the only place a window's geometry should change
    (set-window-geometry win :x wx :y wy :width width :height height :border-width 0)
    (xlib:with-state ((window-parent win))
      ;; FIXME: updating the border doesn't need to be run everytime
      ;; the window is maximized, but only when the border style or
      ;; window type changes. The overhead is probably minimal,
      ;; though.
      (setf (xlib:drawable-x (window-parent win)) x
            (xlib:drawable-y (window-parent win)) y
            (xlib:drawable-border-width (window-parent win)) border)
      ;; the parent window should stick to the size of the window
      ;; unless it isn't being maximized to fill the frame.
      (if (or stick
              (find *window-border-style* '(:tight :none)))
          (setf (xlib:drawable-width (window-parent win)) (window-width win)
                (xlib:drawable-height (window-parent win)) (window-height win))
          (let ((frame (window-frame win)))
            (setf (xlib:drawable-width (window-parent win)) (- (frame-width frame)
                                                               (* 2 (xlib:drawable-border-width (window-parent win))))
                  (xlib:drawable-height (window-parent win)) (- (frame-display-height (window-group win) frame)
                                                                (* 2 (xlib:drawable-border-width (window-parent win)))))))
      ;; update the "extents"
      (xlib:change-property (window-xwin win) :_NET_FRAME_EXTENTS
                            (list wx wy
                                  (- (xlib:drawable-width (window-parent win)) width wx)
                                  (- (xlib:drawable-height (window-parent win)) height wy))
                            :cardinal 32))))

;;;

(defun focus-next-window (group)
  (focus-forward group (sort-windows-by-number group)))

(defun focus-prev-window (group)
  (focus-forward group
                 (reverse
                  (sort-windows-by-number group))))

(defcommand (next tile-group) () ()
  "Go to the next window in the window list."
  (let ((group (current-group)))
    (if (group-current-window group)
        (focus-next-window group)
        (other-window group))))

(defcommand (prev tile-group) () ()
  "Go to the previous window in the window list."
  (let ((group (current-group)))
    (if (group-current-window group)
        (focus-prev-window group)
        (other-window group))))

(defun pull-window (win &optional (to-frame (tile-group-current-frame (window-group win))))
  (let ((f (window-frame win))
        (group (window-group win)))
    (unless (eq (frame-window to-frame) win)
      (xwin-hide win)
      (setf (window-frame win) to-frame)
      (maximize-window win)
      (when (eq (window-group win) (current-group))
        (xwin-unhide (window-xwin win) (window-parent win)))
      ;; We have to restore the focus after hiding.
      (when (eq win (screen-focus (window-screen win)))
        (screen-set-focus (window-screen win) win))
      (frame-raise-window group to-frame win)
      ;; if win was focused in its old frame then give the old
      ;; frame the frame's last focused window.
      (when (eq (frame-window f) win)
        ;; the current value is no longer valid.
        (setf (frame-window f) nil)
        (frame-raise-window group f (first (frame-windows group f)) nil)))))

;; In the future, this window will raise the window into the current
;; frame.
(defun focus-forward (group window-list &optional pull-p (predicate (constantly t)))
  "Set the focus to the next item in window-list from the focused
window. If PULL-P is T then pull the window into the current
frame."
  ;; The window with focus is the "current" window, so find it in the
  ;; list and give that window focus
  (let* ((w (group-current-window group))
         (wins (remove-if-not predicate (cdr (member w window-list))))
         (nw (if-null wins
                 ;; If the last window in the list is focused, then
                 ;; focus the first one.
                 (car (remove-if-not predicate window-list))
                 ;; Otherwise, focus the next one in the list.
                 (first wins))))
    ;; there's still the case when the window is the only one in the
    ;; list, so make sure its not the same as the current window.
    (if (and nw
             (not (eq w nw)))
        (if pull-p
            (pull-window nw)
            (frame-raise-window group (window-frame nw) nw))
        (message "No other window."))))

(defcommand (pull-window-by-number tile-group) (n &optional (group (current-group)))
                                               ((:window-number "Select window number to pull: "))
  "Pull window N from another frame into the current frame and focus it."
  (let ((win (find n (group-windows group) :key 'window-number :test '=)))
    (when win
      (pull-window win))))

(defcommand-alias pull pull-window-by-number)

(defun other-hidden-window (group)
  "Return the last window that was accessed and that is hidden."
  (let ((wins (remove-if (lambda (w) (eq (frame-window (window-frame w)) w)) (group-windows group))))
    (first wins)))

(defun pull-other-hidden-window (group)
  "pull the last accessed hidden window from any frame into the
current frame and raise it."
  (let ((win (other-hidden-window group)))
    (if win
        (pull-window win)
        (echo-string (group-screen group) "No other window."))))

(defun other-window-in-frame (group)
  (let* ((f (tile-group-current-frame group))
         (wins (frame-windows group f))
         (win (if (frame-window f)
                  (second wins)
                  (first wins))))
    (if win
        (frame-raise-window group (window-frame win) win)
        (echo-string (group-screen group) "No other window."))))

(defcommand (pull-hidden-next tile-group) () ()
"Pull the next hidden window into the current frame."
  (let ((group (current-group)))
    (focus-forward group (sort-windows-by-number group) t (lambda (w) (not (eq (frame-window (window-frame w)) w))))))

(defcommand (pull-hidden-previous tile-group) () ()
"Pull the next hidden window into the current frame."
  (let ((group (current-group)))
    (focus-forward group (nreverse (sort-windows-by-number group)) t (lambda (w) (not (eq (frame-window (window-frame w)) w))))))

(defcommand (pull-hidden-other tile-group) () ()
"Pull the last focused, hidden window into the current frame."
  (let ((group (current-group)))
    (pull-other-hidden-window group)))

(defun exchange-windows (win1 win2)
  "Exchange the windows in their respective frames."
  (let ((f1 (window-frame win1))
        (f2 (window-frame win2)))
    (unless (eq f1 f2)
      (pull-window win1 f2)
      (pull-window win2 f1)
      (focus-frame (window-group win1) f2))))

(defcommand (exchange-direction tile-group) (dir &optional (win (current-window)))
    ((:direction "To what direction? "))
  "Exchange the current window (by default) with the top window of the frame in specified direction.
@table @asis
@item up
@item down
@item left
@item right
@end table"
  (if win
      (let* ((frame-set (group-frames (window-group win)))
             (neighbour (neighbour dir (window-frame win) frame-set)))
        (if (and neighbour (frame-window neighbour))
            (exchange-windows win (frame-window neighbour))
            (message "No window in direction ~A!" dir)))
      (message "No window in current frame!")))


(defun echo-frame-windows (&optional (fmt *window-format*))
  "Display a list of all the windows in the current frame."
  (echo-windows fmt (current-group) (frame-windows (current-group)
                                                   (tile-group-current-frame (current-group)))))

(defcommand (fullscreen tile-group) () ()
  "Toggle the fullscreen mode of the current widnow. Use this for clients
with broken (non-NETWM) fullscreen implemenations, such as any program
using SDL."
  (update-fullscreen (current-window) 2))

(defcommand (gravity tile-group) (gravity) ((:gravity "Gravity to where? "))
  "Set a window's gravity within its frame. Gravity controls where the
window will appear in a frame if it is smaller that the
frame. Possible values are:

@table @var
@item center
@item top
@item right
@item bottom
@item left
@item top-right
@item top-left
@item bottom-right
@item bottom-left
@end table"
  (when (current-window)
    (setf (window-gravity (current-window)) gravity)
    (maximize-window (current-window))))

(defcommand (pull-marked tile-group) () ()
"Pull all marked windows into the current frame and clear the marks."
  (let ((group (current-group)))
    (dolist (i (marked-windows group))
      (pull-window i))
    (clear-window-marks group)))

;;; window placement commands

(defun make-rule-for-window (window &optional lock title)
  "Guess at a placement rule for WINDOW and add it to the current set."
  (if (typep window 'tile-window) ;; TODO: Make rules for float windows
      (let* ((group (window-group window))
	     (group-name (group-name group))
	     (frame-number (frame-number (window-frame window)))
	     (role (window-role window)))
	(push (list group-name frame-number t lock
		    :class (window-class window)
		    :instance (window-res window)
		    :title (and title (window-name window))
		    :role (and (not (equal role "")) role))
	      *window-placement-rules*))))

(defun make-rules-for-group (group &optional lock title)
  "Guess at a placement rule for all WINDOWS in group and add it to the current set."
  (if (> (length (group-windows (current-group))) 0)
    (dolist (i (group-windows group))
      (make-rule-for-window i lock title)) t))

(defun make-rules-for-screen (screen &optional lock title)
  "Guess at a placement rule for all WINDOWS in all groups in current screen and add it to the current set."
  (dolist (i (screen-groups screen))
    (make-rules-for-group i lock title)) t) ; dolist always gives nil

(defun make-rules-for-desktop (&optional lock title)
  (dolist (i *screen-list*)
    (make-rules-for-screen i lock title)))

(defun remove-rule-for-window (window)
  "Forget window of given group and screen"
  (let ((match (rule-matching-window window)))
    (when match
      (setf
       *window-placement-rules*
       (delete match *window-placement-rules*)))))

(defun remove-rules-for-group (group)
  "Forget all windows of given group"
  (dolist (i (group-windows group))
    (remove-rule-for-window i)))

(defun remove-rules-for-screen (screen &optional lock title)
  "Forget all windows of given screen"
  (dolist (i (screen-groups screen))
    (remove-rules-for-group i)))

(defmacro forget-remember-rules (body message message-false)
  "Local macro. Forget or remember windows placement rules"
  `(eval-with-message :body (progn ,body
				   (ensure-directories-exist
				    (data-dir-file "window-placement" "rules" "rules.d"))
				   (dump-structure
				    *window-placement-rules* t
				    (data-dir-file "window-placement" "rules" "rules.d")))
		      :message-if-done ,message
		      :message-if-false ,message-false))

(defcommand (remember-window tile-group) (lock title)
  ((:y-or-n "Lock to group? ")
   (:y-or-n "Use title? "))
  "Make a generic placement rule for the current window. Might be too specific/not specific enough!"
  (forget-remember-rules
   (make-rule-for-window (current-window) (first lock) (first title))
   "Rules remembered"
   "Can't remember rules. Check write permissions to dswm data
directory and files"))

(defcommand-alias remember remember-window) ;; TODO remove in next release

(defun remember-windows-current-group (lock title)
  "Make a generic placement rule for the current window. Might be too specific/not specific enough!"
  (forget-remember-rules
   (make-rules-for-group (current-group) (first lock) (first title))
   "Rules remembered"
   "Can't remember rules. Check write permissions to dswm data
directory and files"))

(defun remember-windows-current-screen (lock title)
  "Make a generic placement rule for the current window. Might be too specific/not specific enough!"
  (forget-remember-rules
   (make-rules-for-screen (current-screen) (first lock) (first title))
   "Rules remembered"
   "Can't remember rules. Check write permissions to dswm data
directory and files") )

(defcommand (remember-all-windows tile-group) (lock title)
  ((:y-or-n "Lock to group? ")
   (:y-or-n "Use title? "))
  "Make a generic placement rule for the current window. Might be too specific/not specific enough!"
  (forget-remember-rules
   (and
    (not (setf *window-placement-rules* nil))
    (make-rules-for-desktop (first lock) (first title)))
    "Rules remembered"
    "Can't remember rules. Check write permissions to dswm data directory and files"))

(defcommand (forget-window tile-group) () ()
  "Make a generic placement rule for the current window. Might be too
specific/not specific enough!"
  (forget-remember-rules
   (remove-rule-for-window (current-window))
   "Rules forgotten"
   "Can't forgot rules. Check write permissions to dswm data directory and files"))

(defcommand-alias forget forget-window) ;; TODO remove in next release

(defun forget-windows-current-group (group tile-group)
  "Remove a generic placement rule for the current window. Might be too specific/not specific enough!"
  (forget-remember-rules
   (remove-rules-for-group (current-group))
   "Rules forgotten"
   "Can't forgot rules. Check write permissions to dswm data directory and files"))

(defun forget-windows-current-screen (group tile-group)
  "Remove generic placement rules for the all windows in current screen"
  (forget-remember-rules
   (make-rules-for-screen (current-screen))
   "Rules forgotten"
   "Can't forgot rules. Check write permissions to dswm data directory and files"))

(defcommand (forget-all-windows tile-group) () ()
  "Remove placement rules for all windows"
  (forget-remember-rules
   (setf *window-placement-rules* nil)
   "Rules forgotten"
   "Can't forgot rules. Check write permissions to dswm data directory and files") )

(defcommand (redisplay tile-group) () ()
  "Refresh current window by a pair of resizes, also make it occupy entire frame."
  (let ((window (current-window)))
    (if-not-null
     window
     (with-slots (width height frame) window
       (set-window-geometry window
			    :width (- width (window-width-inc window))
			    :height (- height (window-height-inc window)))
       ;; make sure the first one goes through before sending the second
       (xlib:display-finish-output *display*)
       (set-window-geometry window
			    :width (+ width
				      (* (window-width-inc window)
					 (floor (- (frame-width frame) width)
						(window-width-inc window))))
			    :height (+ height
				       (* (window-height-inc window)
					  (floor (- (frame-height frame) height)
						 (window-height-inc window)))))
       (maximize-window window))
     (message "Frame is empty. No window to redisplay"))))

(defcommand frame-windowlist (&optional (fmt *window-format*)) (:rest)
  "Allow the user to select a window from the list of windows in the current
frame and focus the selected window.  The optional argument @var{fmt} can be
specified to override the default window formatting."
  (let* ((group (current-group))
	 (frame (tile-group-current-frame group)))
    (if-null (frame-windows group frame)
	(message "No Managed Windows")
	(let ((window (select-window-from-menu (frame-sort-windows group frame) fmt)))
	  (if window
	      (group-focus-window group window)
	      (throw 'error :abort))))))
