;; Copyright (C) 2013 Alexander aka CosmonauT Vynnyk
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
;; COMMENTS
;; HERE
;;
;; Code:

(in-package :dswm)

;; (export '(
;; 	  ;; TODO:
;; 	  ))

(define-dswm-type :rfr (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt (rfr-elements) :require-match t)))


(defun rfr-elements ()
  (list "frame" "group" "screen" "window" "current-frame" "current-group" "current-screen" "current-window" "desktop" "help"))

;; (defcommand remember (what) ((:rfr "What do you want to remember (type `help` for help)? "))
;;   (cond ((equal what "frame")
;; 	 )
;; 	((equal what "group")
;; 	 )
;; 	((equal what "screen")
;; 	 )
;; 	((equal what "window")
;; 	 )
;; 	((equal what "current-frame")
;; 	 )
;; 	((equal what "current-group")
;; 	 )
;; 	((equal what "current-screen")
;; 	 )
;; 	((equal what "current-window")
;; 	 )
;; 	((equal what "desktop")
;; 	 )
;; 	((equal what "help")
;; 	 )))




;; ;;remember
;; (defun remember-group (&optional (group (current-group)))

;;   )

;; (defun remember-screen (&optional (screen (current-screen)))
;;   )

;; (defun remember-frame (&optional (frame (tile-group-current-frame (current-group))) ;; TODO: only for tile groups
;;   )

;; (defun remember-window-place (&optional (window (current-window)))
;;   )

;; (defun remember-group-windows-places (&optional (group (current-group)))
;;   )

;; (defun remember-screen-windows-places (&optional (screen (current-screen)))
;;   )

;; (defun remember-all ()
;;   "Make rules of all existing windows, bind it to groups and frames,
;; where they located now and dump all groups frames and window placement
;; rules to frame-froup-placement.rules and window-placement.rules in
;; data dir"
;;   (eval-with-message
;;    :body
;;    (progn
;;      (remember-all-windows '(t) '(nil))
;;      (dump-desktop :to-fs t))
;;    :message-if-done "Snapshot created"
;;    :message-if-false "Can't create snapshot"))

;; (defun remember-all-window-places ()
;;   )

;; ;; forget
;; (defun forget-group (&optional (group (current-group)))
;;   )

;; (defun forget-screen (&optional (screen (current-screen)))
;;   )

;; (defun forget-frame (&optional (frame (tile-group-current-frame (current-group))) ;; TODO: only for tile groups
;;   )

;; (defun forget-window-place (&optional (window (current-window)))
;;   )

;; (defun forget-group-windows-places (&optional (group (current-group)))
;;   )

;; (defun forget-screen-windows-places (&optional (screen (current-screen)))
;;   )

;; (defun forget-all ()
;;   )

;; (defun forget-all-window-places ()
;;   )


;; ;;recall
;; (defun recall-group (&optional (group (current-group)))
;;   )

;; (defun recall-screen (&optional (screen (current-screen)))
;;   )

;; (defun recall-frame (&optional (frame (tile-group-current-frame (current-group))) ;; TODO: only for tile groups
;;   )

;; (defun recall-window-place (&optional (window (current-window)))
;;   )

;; (defun recall-group-windows-places (&optional (group (current-group)))
;;   )

;; (defun recall-screen-windows-places (&optional (screen (current-screen)))
;;   )

;; (defun recall-all ()
;;   "Recall frame and group and windows placement rules of all groups and frames"
;;   (eval-with-message
;;    :body
;;    (progn
;;      (when (file-exists-p *desktop-dump-file*)
;;        (restore-from-file *desktop-dump-file*))
;;      (when (file-exists-p *window-placement-dump-file*)
;;        (progn
;; 	 (setf *window-placement-rules*
;; 	       (read-dump-from-file *window-placement-dump-file*))
;; 	 (place-existing-windows)))
;;      ;; TODO: Add function for restore all programs, running in last session
;;      t) ;; unfortunately someone function gives "NIL"; TODO: fix it!
;;    :message-if-done "Snapshot recalled"
;;    :message-if-false "Can't recall snapshot"))
;; )

;; (defun recall-all-window-placement ()
;;   )





;; ;;;;
;; ;;;;
;; ;;; window placement commands ;; TODO: make it more logical and useful
;; ;;;;
;; ;;;;

;; (defun make-rules-for-group (group &optional lock title)
;;   "Guess at a placement rule for all WINDOWS in group and add it to the current set."
;;   (if (> (length (group-windows (current-group))) 0)
;;     (dolist (i (group-windows group))
;;       (make-rule-for-window i lock title)) t))

;; (defun make-rules-for-screen (screen &optional lock title)
;;   "Guess at a placement rule for all WINDOWS in all groups in current screen and add it to the current set."
;;   (dolist (i (screen-groups screen))
;;     (make-rules-for-group i lock title)) t) ; dolist always gives nil

;; (defun make-rules-for-desktop (&optional lock title)
;;   (dolist (i *screen-list*)
;;     (make-rules-for-screen i lock title)))

;; (defun remove-rule-for-window (window)
;;   "Forget window of given group and screen"
;;   (let ((match (rule-matching-window window)))
;;     (when match
;;       (setf
;;        *window-placement-rules*
;;        (delete match *window-placement-rules*)))))

;; (defun remove-rules-for-group (group)
;;   "Forget all windows of given group"
;;   (dolist (i (group-windows group))
;;     (remove-rule-for-window i)))

;; (defun remove-rules-for-screen (screen &optional lock title)
;;   "Forget all windows of given screen"
;;   (dolist (i (screen-groups screen))
;;     (remove-rules-for-group i)))

;; (defmacro forget-remember-rules (body message message-false)
;;   "Local macro. Forget or remember windows placement rules"
;;   `(eval-with-message :body (progn ,body
;; 				   (ensure-directories-exist
;; 				    (data-dir-file "window-placement" "rules" "rules.d"))
;; 				   (dump-structure
;; 				    *window-placement-rules* t
;; 				    (data-dir-file "window-placement" "rules" "rules.d")))
;; 		      :message-if-done ,message
;; 		      :message-if-false ,message-false))

;; (defcommand (remember-window tile-group) (lock title)
;;   ((:y-or-n "Lock to group? ")
;;    (:y-or-n "Use title? "))
;;   "Make a generic placement rule for the current window. Might be too specific/not specific enough!"
;;   (forget-remember-rules
;;    (make-rule-for-window (current-window) (first lock) (first title))
;;    "Rules remembered"
;;    "Can't remember rules. Check write permissions to dswm data
;; directory and files"))

;; (defcommand-alias remember remember-window) ;; TODO remove in next release

;; (defun remember-windows-current-group (lock title)
;;   "Make a generic placement rule for the current window. Might be too specific/not specific enough!"
;;   (forget-remember-rules
;;    (make-rules-for-group (current-group) (first lock) (first title))
;;    "Rules remembered"
;;    "Can't remember rules. Check write permissions to dswm data
;; directory and files"))

;; (defun remember-windows-current-screen (lock title)
;;   "Make a generic placement rule for the current window. Might be too specific/not specific enough!"
;;   (forget-remember-rules
;;    (make-rules-for-screen (current-screen) (first lock) (first title))
;;    "Rules remembered"
;;    "Can't remember rules. Check write permissions to dswm data
;; directory and files") )

;; (defcommand (remember-all-windows tile-group) (lock title)
;;   ((:y-or-n "Lock to group? ")
;;    (:y-or-n "Use title? "))
;;   "Make a generic placement rule for the current window. Might be too specific/not specific enough!"
;;   (forget-remember-rules
;;    (and
;;     (not (setf *window-placement-rules* nil))
;;     (make-rules-for-desktop (first lock) (first title)))
;;     "Rules remembered"
;;     "Can't remember rules. Check write permissions to dswm data directory and files"))

;; (defcommand (forget-window tile-group) () ()
;;   "Make a generic placement rule for the current window. Might be too
;; specific/not specific enough!"
;;   (forget-remember-rules
;;    (remove-rule-for-window (current-window))
;;    "Rules forgotten"
;;    "Can't forgot rules. Check write permissions to dswm data directory and files"))

;; (defcommand-alias forget forget-window) ;; TODO remove in next release

;; (defun forget-windows-current-group (group tile-group)
;;   "Remove a generic placement rule for the current window. Might be too specific/not specific enough!"
;;   (forget-remember-rules
;;    (remove-rules-for-group (current-group))
;;    "Rules forgotten"
;;    "Can't forgot rules. Check write permissions to dswm data directory and files"))

;; (defun forget-windows-current-screen (group tile-group)
;;   "Remove generic placement rules for the all windows in current screen"
;;   (forget-remember-rules
;;    (make-rules-for-screen (current-screen))
;;    "Rules forgotten"
;;    "Can't forgot rules. Check write permissions to dswm data directory and files"))

;; (defcommand (forget-all-windows tile-group) () ()
;;   "Remove placement rules for all windows"
;;   (forget-remember-rules
;;    (setf *window-placement-rules* nil)
;;    "Rules forgotten"
;;    "Can't forgot rules. Check write permissions to dswm data directory and files") )

;;;;
;;;;
;; /END
;;;;
;;;;