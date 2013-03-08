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

(export '(
	  remove-group-from-rules
	  remove-screen-from-rules
	  update-group-in-rules
	  update-screen-in-rules
	  ))

(define-dswm-type :rfr (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt (rfr-elements) :require-match t)))


(defun rfr-elements ()
  (list "frame" "group" "screen" "window" "current-frame" "current-group" "current-screen" "current-window" "desktop" "help"))

(defun sdump-member-of-list-p (sdump dump-list)
  "Check, if screen is member of dump part"
  (cond ((null dump-list) nil)
	((eq (sdump-number sdump) (sdump-number (car dump-list)))
	 (car dump-list))
	(t (sdump-member-of-list-p sdump (cdr dump-list)))))

(defun replace-sdump-member-of-list (sdump dump-list)
  "Replace, sdump to new sdump with same ID in sdumps list"
  (cond ((null dump-list) nil)
	((eq (sdump-number sdump) (sdump-number (car dump-list)))
	 (cons sdump (cdr dump-list)))
	(t
	 (cons (car dump-list) (replace-sdump-member-of-list sdump (cdr dump-list))))))

(defun remove-sdump-member-of-list (sdump dump-list)
  "Remove, sdump from sdumps list"
  (cond ((null dump-list) nil)
	((eq (sdump-number sdump) (sdump-number (car dump-list)))
	 (cdr dump-list))
	(t
	 (cons (car dump-list) (remove-sdump-member-of-list sdump (cdr dump-list))))))

(defun gdump-member-of-list-p (gdump dump-list) ;; TODO
  "Check, if gdump is member of dump part"
  (cond ((null dump-list)
  	 nil)
  	((eq (type-of (car dump-list)) 'gdump)
  	 (if (or
  	      (eq (gdump-number gdump) (gdump-number (car dump-list)))
  	      (eq (gdump-name gdump) (gdump-number (car dump-list))))
  	     (car dump-list)
  	     (gdump-member-of-list-p gdump (cdr dump-list))))
  	((eq (type-of (car dump-list)) 'fgdump)
  	 (if (or
  	      (eq (gdump-number gdump) (fgdump-number (car dump-list)))
  	      (eq (gdump-name gdump) (fgdump-number (car dump-list))))
  	     (car dump-list)
  	     (gdump-member-of-list-p gdump (cdr dump-list))))))

(defun replace-gdump-member-of-list (gdump dump-list) ;; TODO
  "Replace, group dump with new group dump in dump list"
  (cond ((null dump-list)
  	 nil)
  	((eq (type-of (car dump-list)) 'gdump)
  	 (if (or
  	      (eq (gdump-number gdump) (gdump-number (car dump-list)))
  	      (eq (gdump-name gdump) (gdump-number (car dump-list))))
	     (cons (dump-group gdump) (cdr dump-list))
  	     (cons (car dump-list) (replace-gdump-member-of-list gdump (cdr dump-list)))))
  	((eq (type-of (car dump-list)) 'fgdump)
  	 (if (or
  	      (eq (group-number gdump) (fgdump-number (car dump-list)))
  	      (eq (group-name gdump) (fgdump-number (car dump-list))))
	     (cons (dump-group gdump) (cdr dump-list))
  	     (cons (car dump-list) (replace-gdump-member-of-list gdump (cdr dump-list)))))))

(defun remove-gdump-member-of-list (gdump dump-list) ;; TODO
  "Remove, group dump with new group dump in dump list"
  (cond ((null dump-list)
  	 nil)
  	((eq (type-of (car dump-list)) 'gdump)
  	 (if (or
  	      (eq (gdump-number gdump) (gdump-number (car dump-list)))
  	      (eq (gdump-number gdump) (gdump-number (car dump-list))))
	     (cdr dump-list)
  	     (cons (car dump-list) (remove-gdump-member-of-list gdump (cdr dump-list)))))
  	((eq (type-of (car dump-list)) 'fgdump)
  	 (if (or
  	      (eq (gdump-number gdump) (fgdump-number (car dump-list)))
  	      (eq (gdump-name gdump) (fgdump-number (car dump-list))))
	     (cdr dump-list)
  	     (cons (car dump-list) (remove-gdump-member-of-list gdump (cdr dump-list)))))))

(defun update-group-in-rules (&optional (group (current-group)) (screen (current-screen)))
  "Insert or replace group into desktop rules tree"
  (let* ((dumped-screen (dump-screen screen))
	 (dumped-group (dump-group group))
	 (sdump-member (sdump-member-of-list-p
			dumped-screen
			(ddump-screens *desktop-rules*)))
	 (gdump-member (gdump-member-of-list-p
			dumped-group
			(sdump-groups sdump-member))))
    (cond ((and
	    (not (null sdump-member))
	    (not (null gdump-member)))
	   (replace-gdump-member-of-list dumped-group (sdump-groups sdump-member)))
	  ((and
	    (not (null sdump-member))
	    (null gdump-member))
	   (push dumped-group (sdump-groups sdump-member)))
	  (t
	   (push dumped-screen (ddump-screens *desktop-rules*)) ;; TODO: DSWM::*DESKTOP-RULES* ;note: deleting unreachable code
	   (update-group-in-rules group screen)))))

(defun remove-group-from-rules (&optional (group (current-group)) (screen (current-screen)))
  "Remove group from desktop rules tree"
  (let* ((dumped-screen (dump-screen screen))
	 (dumped-group (dump-group group))
	 (sdump-member (sdump-member-of-list-p
			dumped-screen
			(ddump-screens *desktop-rules*)))
	 (gdump-member (gdump-member-of-list-p
			dumped-group
			(sdump-groups sdump-member))))
    (cond ((and
	    (not (null sdump-member))
	    (not (null gdump-member)))
	   (remove-gdump-member-of-list dumped-group (sdump-groups sdump-member)))
	  ((and
	    (not (null sdump-member))
	    (null gdump-member))
	   (error "No group, named ~a" (group-name group)))
	  (t (error "No screen with ID ~a" (screen-id screen)))))) ;; TODO: "No screen with ID ~a"; note: deleting unreachable code

(defun update-screen-in-rules (&optional (screen (current-screen)))
  "Insert or replace screen into desktop rules tree"
  (let* ((dumped-screen (dump-screen screen))
	 (sdump-member (sdump-member-of-list-p
			dumped-screen
			(ddump-screens *desktop-rules*))))
    (if (not (null sdump-member))
	(replace-sdump-member-of-list dumped-screen (ddump-screens *desktop-rules*))
	(push dumped-screen (ddump-screens *desktop-rules*)))))

(defun remove-screen-from-rules (&optional (screen (current-screen)))
  "Remove screen from desktop rules tree"
  (let* ((dumped-screen (dump-screen screen))
	 (sdump-member (sdump-member-of-list-p
			dumped-screen
			(ddump-screens *desktop-rules*))))
    (if (not (null sdump-member))
	(remove-sdump-member-of-list dumped-screen (ddump-screens *desktop-rules*))
	(warn "No screen with ID ~a. Nothing to remove" (screen-id screen)))))


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

