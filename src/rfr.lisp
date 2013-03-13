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

(defvar *rfr-elements* '("frame" "group" "screen" "window" "current-frame" "current-group" "current-screen" "current-window" "desktop" "all" "help"))

(define-dswm-type :rfr (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt *rfr-elements* :require-match t)))

(defun sdump-member-of-list-p (sdump dump-list)
  "Check, if screen is member of dump part"
  (cond ((null dump-list) nil)
	((eq (sdump-number sdump) (sdump-number (car dump-list)))
	 (car dump-list))
	(t (sdump-member-of-list-p sdump (cdr dump-list)))))

(defun update-sdump-member-of-list (sdump dump-list)
  "Replace, sdump to new sdump with same ID in sdumps list"
  (cond ((null dump-list) nil)
	((eq (sdump-number sdump) (sdump-number (car dump-list)))
	 (cons sdump (cdr dump-list)))
	(t
	 (cons (car dump-list) (update-sdump-member-of-list sdump (cdr dump-list))))))

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

(defun update-gdump-member-of-list (gdump dump-list) ;; TODO
  "Replace, group dump with new group dump in dump list"
  (cond ((null dump-list)
  	 nil)
  	((eq (type-of (car dump-list)) 'gdump)
  	 (if (or
  	      (eq (gdump-number gdump) (gdump-number (car dump-list)))
  	      (eq (gdump-name gdump) (gdump-number (car dump-list))))
	     (cons (dump-group gdump) (cdr dump-list))
  	     (cons (car dump-list) (update-gdump-member-of-list gdump (cdr dump-list)))))
  	((eq (type-of (car dump-list)) 'fgdump)
  	 (if (or
  	      (eq (group-number gdump) (fgdump-number (car dump-list)))
  	      (eq (group-name gdump) (fgdump-number (car dump-list))))
	     (cons (dump-group gdump) (cdr dump-list))
  	     (cons (car dump-list) (update-gdump-member-of-list gdump (cdr dump-list)))))))

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
	   (update-gdump-member-of-list dumped-group (sdump-groups sdump-member)))
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
	(update-sdump-member-of-list dumped-screen (ddump-screens *desktop-rules*))
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

;;;;
;; remember
;;;;

(defun remember-group (&key (group (current-group)) permantnt-p)
  (progn
    (update-gdump-member-of-list (gdump group) *desktop-rules*)
    (if-not-null permantnt-p
		 (dump-to-file *desktop-rules* *desktop-dump-file*))))

(defun remember-screen (&key (screen (current-screen)) permantnt-p)
  (progn
    (update-gdump-member-of-list (gdump group) *desktop-rules*)
    (if-not-null permantnt-p
		 (dump-to-file *desktop-rules* *desktop-dump-file*))))

(defun remember-desktop ()
  (let ((desktop-dump (desktop-dump)))
    (progn
    (setf *desktop-rules* desktop-dump)
    (dump-to-file (dump-desktop) *desktop-dump-file*))))

(defun remember-window-placement (&key (window (current-window)) (lock-p t) title-p) ;; is it function really needed?
  (make-rule-for-window window lock-p title-p))

(defun remember-group-windows-placement (&key (group (current-group)) (lock-p t) title-p)
  "Guess at a placement rule for all WINDOWS in group and add it to the current set."
  (if (> (length (group-windows group)) 0)
      (dolist (i (group-windows group))
	(make-rule-for-window i lock title)) t)) ; dolist always gives nil

(defun remember-screen-windows-placement (&key (screen (current-screen)) (lock-p t) title-p)
  "Guess at a placement rule for all WINDOWS in all groups in current screen and add it to the current set."
  (dolist (i (screen-groups screen))
    (remember-group-windows-placement i lock-p title-p)) t) ; dolist always gives nil

(defun remember-all-window-placement (&key (lock-p t) title-p)
  (dolist (i *screen-list*)
    (remember-screen-windows-placement i lock-p title-p)))

(defun remember-all ()
  "Make rules of all existing windows, bind it to groups and frames,
where they located now and dump all groups frames and window placement
rules to frame-froup-placement.rules and window-placement.rules in
data dir"
  (eval-with-message
   :body
   (progn
     ;; (remember-all-windows '(t) '(nil)) ;; TODO: add part for window-placement rules
     (remember-desktop))
   :message-if-done "Rules remembered"
   :message-if-false "Can`t remember rules"))

;;;;
;; forget
;;;;

(defun forget-group (&key (group (current-group)) permantnt-p)
  (progn
    (remove-gdump-member-of-list (gdump group) *desktop-rules*)
    (if-not-null permantnt-p
		 (dump-to-file *desktop-rules* *desktop-dump-file*))))

(defun forget-screen (&key (screen (current-screen)) permantnt-p)
  (progn
    (remove-gdump-member-of-list (gdump group) *desktop-rules*)
    (if-not-null permantnt-p
		 (dump-to-file *desktop-rules* *desktop-dump-file*))))

(defun forget-window-placement (&optional (window (current-window)))
;; (defun remove-rule-for-window (window)
;;   "Forget window of given group and screen"
;;   (let ((match (rule-matching-window window)))
;;     (when match
;;       (setf
;;        *window-placement-rules*
;;        (delete match *window-placement-rules*)))))

  (message "Locked"))

(defun forget-group-windows-placement (&optional (group (current-group)))
;; (defun remove-rules-for-group (group)
;;   "Forget all windows of given group"
;;   (dolist (i (group-windows group))
;;     (remove-rule-for-window i)))

  (message "Locked"))

(defun forget-screen-windows-placement (&optional (screen (current-screen)))
;; (defun remove-rules-for-screen (screen &optional lock title)
;;   "Forget all windows of given screen"
;;   (dolist (i (screen-groups screen))
;;     (remove-rules-for-group i)))


  (message "Locked"))

(defun forget-all-window-placement ()


  (message "Locked"))

(defun forget-all ()
  "Remove all placement rules and rule files"
  (progn
    (setf *window-placement-rules* nil)
    (setf *desktop-rules* nil)
    (when (file-exists-p *window-placement-dump-file*)
      (delete-file *window-placement-dump-file*))
    (when (file-exists-p *desktop-dump-file*)
      (delete-file *desktop-dump-file*))))

;;;;
;; recall
;;;;

(defun recall-group (&optional (group (current-group)))
  (message "Locked"))

(defun recall-screen (&optional (screen (current-screen)))
  (message "Locked"))

(defun recall-frame (&optional (frame (tile-group-current-frame (current-group)))) ;; TODO: only for tile groups
  (message "Locked"))

(defun recall-window-placement (&optional (window (current-window)))
  (message "Locked"))

(defun recall-group-windows-placement (&optional (group (current-group)))
  (message "Locked"))

(defun recall-screen-windows-placement (&optional (screen (current-screen)))
  (message "Locked"))

(defun recall-all-window-placement ()
  (message "Locked"))

(defun recall-all ()
  "Recall frame and group and windows placement rules of all groups and frames"
  (eval-with-message
   :body
   (progn
     (when (file-exists-p *desktop-dump-file*)
       (restore-from-file *desktop-dump-file*))
     (when (file-exists-p *window-placement-dump-file*)
       (progn
	 (setf *window-placement-rules*
	       (read-dump-from-file *window-placement-dump-file*))
	 (sync-window-placement)))
     ;; TODO: Add function for restore all programs, running in last session
     t) ;; unfortunately sync-window-placement gives nil, because dolist
   :message-if-done "All rules recalled"
   :message-if-false "Can't recall rules"))
)

;; ;;;;
;; ;;;;
;; ;;; window placement commands ;; TODO: make it more logical and useful
;; ;;;;
;; ;;;;

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

;;;;
;; Commands
;;;;

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

(defcommand remember (what) ((:rfr "What element do you want to remember? "))
  (cond ((equal what "frame")
	 )
	((equal what "group")
	 )
	((equal what "screen")
	 )
	((equal what "window")
	 )
	((equal what "current-frame")
	 )
	((equal what "current-group")
	 )
	((equal what "current-screen")
	 )
	((equal what "current-window")
	 )
	((equal what "desktop")
	 )
	))

(defcommand forget (what) ((:rfr "What element do you want to forget? "))
  (cond ;; ((equal what "frame")
	;;  )
	;; ((equal what "group")
	;;  )
	;; ((equal what "screen")
	;;  )
	;; ((equal what "window")
	;;  )
	;; ((equal what "current-frame")
	;;  )
	;; ((equal what "current-group")
	;;  )
	;; ((equal what "current-screen")
	;;  )
	;; ((equal what "current-window")
	;;  )
	;; ((equal what "desktop")
	;;  )
	((equal what "all")
	 (forget-all))
	;; ((equal what "help")
	;;  )
	))

(defcommand recall (what) ((:rfr "What element do you want to recall? "))
  (cond 
   ;; ((equal what "group")
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
   ((equal what "all")
    (recall-all))))

;;;;
;;;;
;; /END
;;;;
;;;;

