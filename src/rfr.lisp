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
      (completing-read (current-screen) prompt *rfr-elements* :require-match t)))

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
	     (cons gdump (cdr dump-list))
  	     (cons (car dump-list) (update-gdump-member-of-list gdump (cdr dump-list)))))
  	((eq (type-of (car dump-list)) 'fgdump)
  	 (if (or
  	      (eq (group-number gdump) (fgdump-number (car dump-list)))
  	      (eq (group-name gdump) (fgdump-number (car dump-list))))
	     (cons gdump (cdr dump-list))
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

(defun remember-group (&key (group (current-group)) permanent-p)
  ;; (labels ((upd-list (group list)
  ;; 		     (let* ((i (car list))
  ;; 			    (gdumps (sdump-groups i)))
  ;; 		       (cond ((null list) nil)
  ;; 			     ((gdump-member-of-list-p (dump-group group) gdumps)
  ;; 			      (setf (sdump-groups i)
  ;; 				    (update-gdump-member-of-list (dump-group group) gdumps))
  ;; 			      (cons 
  ;; 			     (t
  ;; 			      (setf (sdump-groups i)
  ;; 				    (cons (dump-group group) gdumps))))
  ;; 		       (cons i (upd-list group (cdr list))))))

  ;; 	  (if-null (ddump-screens *desktop-rules*)
  ;; 		   (setf (ddump-screens *desktop-rules*)
  ;; 			 (list (make-sdump :number (screen-id (group-screen group))
  ;; 					   :current (group-number group)
  ;; 					   :groups (list (dump-group group))))))
	  
  ;; 	  (let ((screens (ddump-screens *desktop-rules*)))
  ;; 	    (message "~a" (upd-list group screens))
  ;; 	    (setf (ddump-screens *desktop-rules*)
  ;; 		  (upd-list group screens))))

  (if-not-null permanent-p
  	       (dump-to-file *desktop-rules* *desktop-dump-file*))
  (message "Locked ~a" (group-name group)))

(defun remember-screen (&key (screen (current-screen)) permanent-p)
  (progn
    (if-null (ddump-screens *desktop-rules*)
	     (setf (ddump-screens *desktop-rules*)
		   (list (dump-screen screen)))
	     (setf (ddump-screens *desktop-rules*)
		   (update-sdump-member-of-list (dump-screen screen)
						(ddump-screens *desktop-rules*))))
    (if-not-null permanent-p
		 (dump-to-file *desktop-rules* *desktop-dump-file*))))

(defun remember-desktop (&key permanent-p)
  (progn
    (setf *desktop-rules* (dump-desktop))
    (if-not-null permanent-p
		 (dump-to-file (dump-desktop) *desktop-dump-file*))))

(defun remember-window-placement (&key (window (current-window)) (lock-p t) title-p permanent-p)
  (progn
    (make-rule-for-window window lock-p title-p)
    (if-not-null permanent-p
		 (dump-to-file *window-placement-rules* *window-placement-dump-file*))))

(defun remember-group-windows-placement (&key (group (current-group)) (lock-p t) (title-p nil) (permanent-p nil))
  "Guess at a placement rule for all WINDOWS in group and add it to the current set."
  (if (> (length (group-windows group)) 0)
      (dolist (i (group-windows group))
	(remember-window-placement :window i
				   :lock-p lock-p
				   :title-p title-p
				   :permanent-p permanent-p)) t)) ; dolist always gives nil

(defun remember-screen-windows-placement (&key (screen (current-screen)) (lock-p t) title-p permanent-p)
  "Guess at a placement rule for all WINDOWS in all groups in current screen and add it to the current set."
  (dolist (i (screen-groups screen))
    (remember-group-windows-placement :group i
				      :lock-p lock-p
				      :title-p title-p
				      :permanent-p permanent-p)) t) ; dolist always gives nil

(defun remember-all-window-placement (&key (lock-p t) title-p permanent-p)
  (dolist (i *screen-list*)
    (remember-screen-windows-placement :screen i
				       :lock-p lock-p
				       :title-p title-p
				       :permanent-p permanent-p)) t)

(defun remember-all (&key (lock-p t) title-p permanent-p)
  "Make rules of all existing windows, bind it to groups and frames,
where they located now and dump all groups frames and window placement
rules to frame-froup-placement.rules and window-placement.rules in
data dir"
  (and
   (remember-desktop :permanent-p permanent-p)
   (remember-all-window-placement :lock-p lock-p
				  :title-p title-p
				  :permanent-p permanent-p)))

;;;;
;; forget
;;;;

(defun forget-group (&key (group (current-group)) permanent-p)
  (progn
    (dolist (i (ddump-screens *desktop-rules*))
      (let ((grouplist i))
	(setf i (remove-gdump-member-of-list (dump-group group) grouplist))))
    (if-not-null permanent-p
		 (dump-to-file *desktop-rules* *desktop-dump-file*))))

(defun forget-screen (&key (screen (current-screen)) permanent-p)
  (progn
    (setf (ddump-screens *desktop-rules*)
	  (remove-sdump-member-of-list (dump-screen screen)
				       (ddump-screens *desktop-rules*)))
    (if-not-null permanent-p
		 (dump-to-file *desktop-rules* *desktop-dump-file*))))

(defun forget-desktop (&key permanent-p)
  (progn
    (setf *desktop-rules* (make-ddump :screens nil :current nil))
    (if-not-null permanent-p
		 (delete-file *desktop-dump-file*))))

(defun forget-window-placement (&key (window (current-window)) permanent-p)
  "Forget window of given group and screen"
  (let ((match (rule-matching-window window)))
    (progn
      (when match
	(setf *window-placement-rules*
	      (delete match *window-placement-rules*)))
      (if-not-null permanent-p
		   (dump-to-file *window-placement-rules*
				 *window-placement-dump-file*)))))

(defun forget-group-windows-placement (&key (group (current-group)) permanent-p)
  "Forget all windows of given group"
  (dolist (i (group-windows group))
    (forget-window-placement :window i :permanent-p permanent-p)) t)

(defun forget-screen-windows-placement (&key (screen (current-screen)) permanent-p)
  "Forget all windows of given screen"
  (dolist (i (screen-groups screen))
    (forget-group-windows-placement :group i :permanent-p permanent-p)) t)

(defun forget-all-window-placement (&key permanent-p)
  "Remove all window-placement rules"
  (progn
    (setf *window-placement-rules* nil)
    (if-not-null permanent-p
		 (delete-file *window-placement-dump-file*))))

(defun forget-all (&key permanent-p)
  "Remove all placement rules and rule files"
  (progn
    (forget-desktop :permanent-p permanent-p)
    (forget-all-window-placement :permanent-p permanent-p)))

;;;;
;; recall
;;;;

(defun recall-group (&key (group (current-group)) permanent-p)
  (progn 
    (if-not-null permanent-p
		 (setf *desktop-rules* 
		       (read-dump-from-file *desktop-dump-file*)))
    (let ((current-ddump (dump-desktop)))
      (dolist (i (ddump-screens current-ddump))
	(dolist (j (sdump-groups i))
	  (setf j (update-gdump-member-of-list (dump-group group) j))))
      (setf *desktop-rules* current-ddump)
      (restore-desktop *desktop-rules*))))

(defun recall-screen (&key (screen (current-screen)) permanent-p)
  (progn 
    (if-not-null permanent-p
		 (setf *desktop-rules* 
		       (read-dump-from-file *desktop-dump-file*)))
    (let ((current-ddump (dump-desktop)))
      (dolist (i (ddump-screens current-ddump))
	(setf i (update-sdump-member-of-list (dump-screen screen) i)))
      (setf *desktop-rules* current-ddump)
      (restore-desktop *desktop-rules*))))

(defun recall-desktop (&key permanent-p)
  (progn
    (if-not-null permanent-p
		 (setf *desktop-rules* 
		       (read-dump-from-file *desktop-dump-file*)))
    (restore-desktop *desktop-rules*) t))

(defun recall-window-placement (&key (window (current-window)) (permanent-p nil))
  (progn
    (if-not-null permanent-p
		 (setf *window-placement-rules*
		       (read-dump-from-file *window-placement-dump-file*)))
    (sync-window-placement window)))

(defun recall-group-windows-placement (&key (group (current-group)) (permanent-p nil))
  (dolist (i (group-windows group))
    (recall-window-placement :window i :permanent-p permanent-p)))

(defun recall-screen-windows-placement (&key (screen (current-screen)) (permanent-p nil))
  (dolist (i (screen-groups screen))
    (recall-group-windows-placement :group i :permanent-p permanent-p)))

(defun recall-all-window-placement (&key permanent-p)
  (progn
    (if-not-null permanent-p
		 (setf *window-placement-rules*
		       (read-dump-from-file *window-placement-dump-file*)))
    (sync-windows-placement)))

(defun recall-all ()
  "Recall frame and group and windows placement rules of all groups and frames"
  (progn
    (when (file-exists-p *desktop-dump-file*)
      (restore-from-file *desktop-dump-file*))
    (when (file-exists-p *window-placement-dump-file*)
      (progn
	(setf *window-placement-rules*
	      (read-dump-from-file *window-placement-dump-file*))
	(sync-windows-placement)))
    ;; TODO: Add function for restore all programs, running in last session
    t) ;; unfortunately sync-window-placement gives nil, because dolist
  )


(defcommand remember (what) ((:rfr "What element do you want to remember? "))
  "Remember rules for some desktop element, like frames placement on group,
 group placement on screen, or window-placement"
  (cond ((equal what "group")
	 (remember-group :permanent-p *rfr-permanent-p*))
	((equal what "screen")
	 (remember-screen :permanent-p *rfr-permanent-p*))
	((equal what "window")
	 (remember-window-placement :permanent-p *rfr-permanent-p*))
	((equal what "group-windows")
	 (remember-group-windows-placement :permanent-p *rfr-permanent-p*))
	((equal what "screen-windows")
	 (remember-screen-windows-placement :permanent-p *rfr-permanent-p*))
	((equal what "all-windows")
	 (remember-all-window-placement :permanent-p *rfr-permanent-p*))
	((equal what "desktop")
	 (remember-desktop :permanent-p *rfr-permanent-p*))
	((equal what "all")
	 (remember-all :permanent-p *rfr-permanent-p*))))

(defcommand forget (what) ((:rfr "What element do you want to forget? "))
  "Forget remembered rules for some desktop element, like frames 
placement on group, group placement on screen, or window-placement"
  (cond ((equal what "group")
	 (forget-group :permanent-p *rfr-permanent-p*))
	((equal what "screen")
	 (forget-screen :permanent-p *rfr-permanent-p*))
	((equal what "window")
	 (forget-window-placement :permanent-p *rfr-permanent-p*))
	((equal what "group-windows")
	 (forget-group-windows-placement :permanent-p *rfr-permanent-p*))
	((equal what "screen-windows")
	 (forget-screen-windows-placement :permanent-p *rfr-permanent-p*))
	((equal what "all-windows")
	 (forget-all-window-placement :permanent-p *rfr-permanent-p*))
	((equal what "desktop")
	 (forget-desktop :permanent-p *rfr-permanent-p*))
	((equal what "all")
	 (forget-all :permanent-p *rfr-permanent-p*))))

(defcommand recall (what) ((:rfr "What element do you want to recall? "))
  "Recall remembered rules for some desktop element, like frames 
placement on group, group placement on screen, or window-placement"
  (cond ((equal what "group")
	 (recall-group :permanent-p *rfr-permanent-p*))
	((equal what "screen")
	 (recall-screen :permanent-p *rfr-permanent-p*))
	((equal what "window")
	 (recall-window-placement :permanent-p *rfr-permanent-p*))
	((equal what "group-windows")
	 (recall-group-windows-placement :permanent-p *rfr-permanent-p*))
	((equal what "screen-windows")
	 (recall-screen-windows-placement :permanent-p *rfr-permanent-p*))
	((equal what "all-windows")
	 (recall-all-window-placement :permanent-p *rfr-permanent-p*))
	((equal what "desktop")
	 (recall-desktop))
	((equal what "all")
	 (recall-all))))

;;;;
;;;;
;; /END
;;;;
;;;;

