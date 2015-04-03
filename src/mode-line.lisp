;; Copyright (C) 2006-2008 Shawn Betts
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

(in-package :dswm)

(export '(add-mode-line-formatter
	  enable-mode-line
	  toggle-mode-line
	  bar
	  bar-zone-color
	  set-mode-line-bg-color
	  set-mode-line-border-color
	  set-mode-line-border-width
	  set-mode-line-fg-color
	  set-mode-line-format
	  set-info-line-format
		mode-line))

(defstruct mode-line
  screen
  head
  window
  format
  position
  contents
  cc
  height
  factor
  (mode :ds))

(defun mode-line-gc (ml)
  (ccontext-gc (mode-line-cc ml)))

(defun screen-mode-line-format ()
  "Show mode-line format"
  (if-not-null *mode-line-format*
	       *mode-line-format* ;; for back compatability
	       (format nil "~a~%~a"
		       *info-line-format*
		       *window-list-line-format*)))

(defvar *current-mode-line-formatters* nil
  "used in formatting modeline strings.")

(defvar *current-mode-line-formatter-args* nil
  "used in formatting modeline strings.")

;; ;;; Formatters

(defun add-mode-line-formatter (character fmt-fun)
  "Add a format function to a format character (or overwrite an existing one)."
  (setf *mode-line-formatters*
        (cons (list character fmt-fun)
              (remove character *mode-line-formatters* :key #'first))))

;; All mode-line formatters take the mode-line they are being invoked from
;; as the first argument. Additional arguments (everything between the first
;; ',' and the ';' are provided as strings [not yet implemented]).

(defun fmt-urgent-window-list (ml)
  "Using *window-format*, return a 1 line list of the urgent windows, space seperated."
   (format nil "|~{~a~^ ~}|"
          (mapcar (lambda (w)
                    (let ((str (format-expand *window-formatters* *window-format* w)))
                      (if (eq w (current-window))
                          (fmt-highlight str)
                          str)))
                  (screen-urgent-windows (mode-line-screen ml)))))

(defun fmt-blink-urgent-window-list (ml)
  "Using *window-format*, return a 1 blink line list of the urgent windows, space seperated."
   (format nil "|~{~a~^ ~}^r|"
	   (mapcar (lambda (w)
		     (let ((str (format-expand *window-formatters* *window-format* w)))
		       (fmt-blink str)))
		    (screen-urgent-windows (mode-line-screen ml))
		    )))

;; (defun fmt-head-window-list-with-urgent (ml)
;;   "Using *window-format*, return a list windows from current group and all urgent windows."
;;   ;; TODO: make. Is it needed?
;;   )

;; (defun fmt-head-window-list-with-blink-urgent (ml)
;;   "Using *window-format*, return a list windows from current group and all blinking urgent windows."
;;   ;; TODO: make. Is it needed?
;; )

(defun fmt-window-list (ml)
   "Using *window-format*, return a 1 line list of the windows, space seperated."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (w) (format-expand *window-formatters* *window-format* w))
                  (sort-windows-by-number (mode-line-current-group ml)))))

(defun fmt-group-list (ml)
  "Given a group list all the groups in the group's screen.
Redefining standard fmt-group-list for hiding scratchpad group"
  (labels
      ((remove-empty-elements (list)
			      (cond ((eq 0 (length list))
				     '())
				    ((equal "" (car list))
				     (remove-empty-elements (cdr list)))
				    (t
				     (cons (car list)
					   (remove-empty-elements (cdr list)))))))
    (format nil "~{~a~^ ~}"
	    (if (eq 0 (group-number (current-group)))
		;; TODO Make it better
		(reverse
		 (cons "]]" (reverse
			     (cons (concat (fmt-highlight "<SCRATCHPAD>") "[[")
				   (remove-empty-elements
				    (mapcar (lambda (w)
					      (let* ((str (format-expand *group-formatters* *group-format* w)))
						(cond ((eq w (second (screen-groups (current-screen))))
						       (fmt-highlight str))
						      ((not (eq 0 (group-number w)))
						       str)
						      (t
						       (format nil "")))))
					    (sort-groups (group-screen (mode-line-current-group
									ml)))))))))
                ;;; Inversion
                ;;;    \/
                ;; (remove-empty-elements
                ;;  (mapcar (lambda (w)
                ;;            (let* ((str (format-expand *group-formatters* *group-format* w)))
                ;;              (cond ((eq w (second (screen-groups (current-screen))))
                ;;                     str)
                ;;                    ((not (eq 0 (group-number w)))
                ;;                     (fmt-highlight str))
                ;;                    (t
                ;;                     (format nil "")))))
                ;;          (sort-groups (group-screen (mode-line-current-group
                ;;                                      ml)))))
	      (remove-empty-elements
	       (mapcar (lambda (w)
			 (let* ((str (format-expand *group-formatters* *group-format* w)))
			   (cond ((eq w (current-group))
				  (fmt-highlight str))
				 ((not (eq 0 (group-number w)))
				  str)
				 (t
				  (format nil "")))))
		       (sort-groups (group-screen (mode-line-current-group
						   ml)))))))))

(defun fmt-head (ml)
  (format nil "~d" (head-number (mode-line-head ml))))

(defun fmt-group (ml)
  (format nil "~a" (group-name (mode-line-current-group ml))))

(defun fmt-highlight (s)
  (format nil "^R~A^r" s))

(defun toggle-mode-line-blink ()
  (if-null *mode-line-blinker*
      (setf *mode-line-blinker* t)
    (setf *mode-line-blinker* nil)))

(defun fmt-solo-blink (s)
  (if-null *mode-line-blinker*
      (format nil "^R~A^r" s)
    (format nil "^r~A^r" s)))

(defun fmt-blink (s)
  (if-null *mode-line-blinker*
      (format nil "^R~A^r" s)
    (format nil "^r~A^R" s)))

(defun fmt-head-window-list (ml)
  "Using *window-format*, return a 1 line list of the windows, space seperated."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (w)
                    (let ((str (format-expand *window-formatters* *window-format* w)))
                      (if (eq w (current-window))
                          (fmt-highlight str)
                          str)))
                  (sort1 (head-windows (mode-line-current-group ml) (mode-line-head ml))
                         #'< :key #'window-number))))

(defun fmt-hidden (s)
  (format nil (concat "^[" *hidden-window-color* "~A^]") s))

(defun fmt-head-window-list-hidden-windows (ml)
  "Using *window-format*, return a 1 line list of the windows, space
separated. The currently focused window is highlighted with
fmt-highlight. Any non-visible windows are colored the
*hidden-window-color*."
  (let* ((all (head-windows (mode-line-current-group ml) (mode-line-head ml)))
         (non-top (set-difference all (top-windows))))
    (format nil "~{~a~^ ~}"
            (mapcar (lambda (w)
                      (let ((str (format-expand *window-formatters*
                                                *window-format* w)))
                        (cond ((eq w (current-window)) (fmt-highlight str))
                              ((find w non-top) (fmt-hidden str))
                              (t str))))
                    (sort1 all #'< :key #'window-number)))))

(defun fmt-modeline-time (ml)
  (declare (ignore ml))
    (time-format *time-modeline-string*))

(defvar *bar-med-color* "^B")
(defvar *bar-hi-color* "^B^3*")
(defvar *bar-crit-color* "^B^1*")

(defun bar-zone-color (amount &optional (med 20) (hi 50) (crit 90) reverse)
  "Return a color command based on the magnitude of the argument. If
the limits for the levels aren't specified, they default to sensible
values for a percentage. With reverse, lower numbers are more
critical."
  (labels ((past (n) (funcall (if reverse #'<= #'>=) amount n)))
    (cond ((past crit) *bar-crit-color*)
          ((past hi) *bar-hi-color*)
          ((past med) *bar-med-color*)
          (t ""))))

(defun repeat (n char)
 (make-string n :initial-element char))

(defun bar (percent width full empty)
  "Return a progress bar string of WIDTH characters composed of characters FULL
  and EMPTY at PERCENT complete."
  (let ((chars (truncate (* (/ width 100) percent))))
    (format nil "^[~A~A^]~A" (bar-zone-color percent)
            (repeat chars full)
            (repeat (- width chars) empty))))

(defvar *alt-prev-index* 0)
(defvar *alt-prev-time* 0)

;; TODO: Figure out a way to objectify fmt-alternate and fmt-scroll so that
;; multiple instances can coexist.

(defun alternate (strings period)
  "Show each of STRINGS, alternating at most once every PERIOD seconds."
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
    (when (>= (- now *alt-prev-time*) period)
      (setf *alt-prev-time* now)
      (if (< *alt-prev-index* (1- (length strings)))
        (incf *alt-prev-index*)
        (setf *alt-prev-index* 0))))
  (elt strings *alt-prev-index*))

(defvar *scroll-prev-index* 0)
(defvar *scroll-prev-time* 0)
(defvar *scroll-prev-dir* :forward)

(defun scroll (string width delay)
  "Scroll STRING within the space of WIDTH characters, with a step of DELAY"
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
    (when (>= (- now *scroll-prev-time*) delay)
      (setf *scroll-prev-time* now)
      (case *scroll-prev-dir*
        (:forward
         (if (< *scroll-prev-index* (- (length string) width))
             (incf *scroll-prev-index*)
             (setf *scroll-prev-dir* :backward)))
        (:backward
         (if (> *scroll-prev-index* 0)
             (decf *scroll-prev-index*)
             (setf *scroll-prev-dir* :forward))))))
  (subseq string *scroll-prev-index* (+ *scroll-prev-index* width)))



(defun make-mode-line-window (parent screen)
  "Create a window suitable for a modeline."
  (xlib:create-window
   :parent parent
   :x 0 :y 0 :width 1 :height 1
   :background (alloc-color screen *mode-line-background-color*)
   :border (alloc-color screen *mode-line-border-color*)
   :border-width *mode-line-border-width*
   ;; You can click the modeline
   :event-mask (xlib:make-event-mask :button-press :exposure)
   ;; these windows are not controlled by the window manager
   :override-redirect :on))

(defun resize-mode-line (ml)
  (when (eq (mode-line-mode ml) :ds)
    ;; This is a DSWM mode-line
    (setf (xlib:drawable-height (mode-line-window ml))
          (+ (* (1+ (count #\Newline (mode-line-contents ml) :test #'equal))
                (font-height (xlib:gcontext-font (mode-line-gc ml))))
             (* *mode-line-pad-y* 2))))
  (setf (xlib:drawable-width (mode-line-window ml)) (- (frame-width (mode-line-head ml))
                                                       (* 2 (xlib:drawable-border-width (mode-line-window ml))))
        (xlib:drawable-height (mode-line-window ml)) (min (xlib:drawable-height (mode-line-window ml))
                                                          (truncate (head-height (mode-line-head ml)) 4))
        (mode-line-height ml) (+ (xlib:drawable-height (mode-line-window ml))
                                 (* 2 (xlib:drawable-border-width (mode-line-window ml))))
        (mode-line-factor ml) (- 1 (/ (mode-line-height ml)
                                      (head-height (mode-line-head ml))))
        (xlib:drawable-x (mode-line-window ml)) (head-x (mode-line-head ml))
        (xlib:drawable-y (mode-line-window ml)) (if (eq (mode-line-position ml) :top)
                                                    (head-y (mode-line-head ml))
                                                    (- (+ (head-y (mode-line-head ml))
                                                          (head-height (mode-line-head ml)))
                                                       (mode-line-height ml)))))

(defgeneric mode-line-format-elt (elt))

(defmethod mode-line-format-elt ((elt string))
  (apply 'format-expand *current-mode-line-formatters* elt
         *current-mode-line-formatter-args*))

(defmethod mode-line-format-elt ((elt symbol))
  (if (boundp elt)
      (let ((val (symbol-value elt)))
        ;; ignore T and nil, like emacs.
        (unless (or (eq val t)
                    (eq val nil))
          (mode-line-format-elt val)))
      (symbol-name elt)))

(defmethod mode-line-format-elt ((elt null))
  "")

(defmethod mode-line-format-elt ((elt list))
  (etypecase (first elt)
    ((or string list)
     (apply 'concatenate 'string
            (mapcar 'mode-line-format-elt elt)))
    (symbol
     (mode-line-format-elt
      (case (first elt)
        ;; FIXME: silently failing is probably not the best idea.
        (:eval (ignore-errors (eval (second elt))))
        (t (and (boundp (first elt))
                (symbol-value (first elt))
                (second elt))))))))

(defun mode-line-format-string (ml)
  (mode-line-format-elt (mode-line-format ml)))

(defun make-mode-line-gc (window screen)
  (xlib:create-gcontext :drawable window
                        :font (screen-font screen)
                        :foreground (alloc-color screen *mode-line-foreground-color*)
                        :background (alloc-color screen *mode-line-background-color*)))


(defun update-mode-line-color-context (ml)
  (let* ((cc (mode-line-cc ml))
         (screen (mode-line-screen ml))
         (bright (if (stringp *mode-line-foreground-color*)
                     (lookup-color screen *mode-line-foreground-color*)
		   *mode-line-foreground-color*)))
    ;; (adjust-color bright 0.25)
    (setf (ccontext-default-bright cc) (alloc-color screen bright))))

(defun make-head-mode-line (screen head format)
  (let* ((w (make-mode-line-window (screen-root screen) screen))
         (gc (make-mode-line-gc w screen)))
    (make-mode-line :window w
                    :screen screen
                    :head head
                    :format format
                    :position *mode-line-position*
                    :cc (make-ccontext :gc gc
                                       :win w
                                       :default-fg (xlib:gcontext-foreground gc)
                                       :default-bg (xlib:gcontext-background gc)))))

(defun mode-line-current-group (ml)
  (screen-current-group (mode-line-screen ml)))

(defun redraw-mode-line (ml &optional force)
  (when (eq (mode-line-mode ml) :ds)
    (let* ((*current-mode-line-formatters* *mode-line-formatters*)
           (*current-mode-line-formatter-args* (list ml))
           (string (mode-line-format-string ml)))
      (when (or force (not (string= (mode-line-contents ml) string)))
        (setf (mode-line-contents ml) string)
        (resize-mode-line ml)
        (render-strings (mode-line-screen ml) (mode-line-cc ml)
                        *mode-line-pad-x*     *mode-line-pad-y*
                        (split-string string (string #\Newline)) '())))))

(defun find-mode-line-window (xwin)
  (dolist (s *screen-list*)
    (dolist (h (screen-heads s))
      (let ((mode-line (head-mode-line h)))
        (when (and mode-line (eq (mode-line-window mode-line) xwin))
          (return-from find-mode-line-window mode-line))))))

(defun sync-mode-line (ml)
  (dolist (group (screen-groups (mode-line-screen ml)))
    (group-sync-head group (mode-line-head ml))))

(defun set-mode-line-window (ml xwin)
  "Use an external window as mode-line."
  (run-hook-with-args *mode-line-destroy-hook* ml)
  (xlib:destroy-window (mode-line-window ml))
  (setf (mode-line-window ml) xwin
        (mode-line-mode ml) :visible
        (xlib:window-priority (mode-line-window ml)) :above)
  (resize-mode-line ml)
  (sync-mode-line ml))

(defun destroy-mode-line-window (ml)
  (run-hook-with-args *mode-line-destroy-hook* ml)
  (xlib:destroy-window (mode-line-window ml))
  (setf (head-mode-line (mode-line-head ml)) nil)
  (sync-mode-line ml))

(defun move-mode-line-to-head (ml head)
  (if (head-mode-line head)
      (when (mode-line-head ml)
        ;; head already has a mode-line. Try swapping them.
        (let ((old-head (mode-line-head ml)))
          (setf (mode-line-head ml) head
                (head-mode-line old-head) (head-mode-line head)
                (mode-line-head (head-mode-line head)) old-head
                (head-mode-line head) ml)))
      (progn
        (when (mode-line-head ml)
          (setf (head-mode-line (mode-line-head ml)) nil))
        (setf (head-mode-line head) ml
              (mode-line-head ml) head))))

(defun update-mode-line-position (ml x y)
  (let ((head
         ;; Find the appropriate head
         (find-if (lambda (h) (and (= x (head-x h))
                                   (>= y (head-y h))
                                   (< y (+ (head-y h) (head-height h)))))
                  (screen-heads (mode-line-screen ml)))))
    (when (or (not head)
              (not (eq (head-mode-line head) ml)))
      ;; No luck. Just try to find a head without a mode-line already.
      (setf head (find-if-not #'head-mode-line (screen-heads (mode-line-screen ml)))))
    (if head
        (progn
          (unless (eq ml (head-mode-line head))
            (move-mode-line-to-head ml head))
          (when (mode-line-head ml)
            (setf (mode-line-position ml) (if (< y (/ (head-height (mode-line-head ml)) 2)) :top :bottom))))
        nil)))

(defun place-mode-line-window (screen xwin)
  (let ((ml (make-mode-line :window xwin :screen screen :mode :visible :position *mode-line-position*)))
    (xlib:reparent-window xwin (screen-root screen) 0 0)
    (when (update-mode-line-position ml (xlib:drawable-x xwin) (xlib:drawable-y xwin))
      (resize-mode-line ml)
      (xlib:map-window xwin)
      (sync-mode-line ml))))

(defun update-mode-lines (screen)
  "Update all mode lines on SCREEN"
  (dolist (h (screen-heads screen))
    (let ((mode-line (head-mode-line h)))
      (when mode-line
        (redraw-mode-line mode-line)))))

(defun update-all-mode-lines ()
  "Update all mode lines."
  (mapc 'update-mode-lines *screen-list*))

(defun turn-on-mode-line-timer ()
  (when (timer-p *mode-line-timer*)
    (cancel-timer *mode-line-timer*))
  (setf *mode-line-timer* (run-with-timer *mode-line-timeout*
                                          *mode-line-timeout*
                                          'update-all-mode-lines)))

(defun all-heads ()
  "Return all heads on all screens."
  (loop for s in *screen-list*
        nconc (copy-list (screen-heads s))))

(defun maybe-cancel-mode-line-timer ()
  (unless (find-if 'head-mode-line (all-heads))
    (when (timer-p *mode-line-timer*)
      (cancel-timer *mode-line-timer*)
      (setf *mode-line-timer* nil))))

(defun toggle-mode-line (screen head &optional (format (screen-mode-line-format)))
  "Toggle the state of the mode line for the specified screen"
  (check-type format (or symbol list string))
  (let ((ml (head-mode-line head)))
    (if ml
        (case (mode-line-mode ml)
          (:visible
           ;; Hide it.
           (setf (mode-line-mode ml) :hidden)
           (xlib:unmap-window (mode-line-window ml)))
          (:hidden
           ;; Show it.
           (setf (mode-line-mode ml) :visible)
           (xlib:map-window (mode-line-window ml)))
          (:ds
           ;; Delete it
	   (run-hook-with-args *mode-line-destroy-hook* ml)
           (xlib:destroy-window (mode-line-window ml))
           (xlib:free-gcontext (mode-line-gc ml))
           (setf (head-mode-line head) nil)
           (maybe-cancel-mode-line-timer)))
        (progn
          (setf (head-mode-line head) (make-head-mode-line screen head format))
          (update-mode-line-color-context (head-mode-line head))
          (resize-mode-line (head-mode-line head))
          (xlib:map-window (mode-line-window (head-mode-line head)))
          (redraw-mode-line (head-mode-line head))
          (dformat 3 "modeline: ~s~%" (head-mode-line head))
          ;; setup the timer
          (turn-on-mode-line-timer)
	  (run-hook-with-args *mode-line-new-hook* (head-mode-line head))))
    (dolist (group (screen-groups screen))
      (group-sync-head group head))))

(defun enable-mode-line (screen head state &optional format)
  "Set the state of SCREEN's HEAD's mode-line. If STATE is T and FORMAT is
  specified, then the mode-line's format is updated."
  (check-type screen screen)
  (check-type head head)
  (check-type format (or symbol list string))
  (if state
      (if (head-mode-line head)
          (when format
            (setf (mode-line-format (head-mode-line head)) format))
	(toggle-mode-line screen head (or format
					  *mode-line-format*
					  (format nil "~a~%~a" *info-line-format* *window-list-line-format*))))
    (when (head-mode-line head)
      (toggle-mode-line screen head))))

(defun maybe-refresh-mode-line (screen head)
  "Refresh mode line, if it needed"
  (let ((ml (head-mode-line head)))
    (if ml
	(case (mode-line-mode ml)
	  (:visible
	   (setf (mode-line-mode ml) :hidden)
	   (xlib:unmap-window (mode-line-window ml))
	   (xlib:map-window (mode-line-window ml)))
	   (:ds
	    (xlib:destroy-window (mode-line-window ml))
	    (xlib:free-gcontext (mode-line-gc ml))
	    (setf (head-mode-line head) nil)
	    (maybe-cancel-mode-line-timer)
	    (setf (head-mode-line head) (make-head-mode-line screen head (screen-mode-line-format)))
	    (xlib:map-window (mode-line-window (head-mode-line head))))))
    (redraw-mode-line (head-mode-line head))))

(defun set-mode-line-any-color (val color)
  "Set any mode-line color for the specified screen"
  (and
   (setf (symbol-value (intern (concat "*MODE-LINE-" (princ-to-string val) "-COLOR*"))) color)
   (maybe-refresh-mode-line (current-screen) (current-head))))

(defun set-mode-line-fg-color (color)
  "Set mode-line foreground color"
  (set-mode-line-any-color 'foreground color))

(defun set-mode-line-bg-color (color)
  "Set mode-line background color"
  (set-mode-line-any-color 'background color))

(defun set-mode-line-border-color (color)
  "Set mode-line background color"
  (set-mode-line-any-color 'border color))

(defun set-mode-line-border-width (width)
  "Set mode-line border width"
  (and
   (setf *mode-line-border-width* width)
   (maybe-refresh-mode-line (current-screen) (current-head))))

(defun set-mode-line-format (format)
  (and
   (setf *mode-line-format* format)
   (maybe-refresh-mode-line (current-screen) (current-head))))

(defun set-info-line-format (format)
  (and
   (setf *info-line-format* format)
   (maybe-refresh-mode-line (current-screen) (current-head))))

(defcommand mode-line () ()
  "A command to toggle the mode line visibility."
  (toggle-mode-line (current-screen) (current-head)))
