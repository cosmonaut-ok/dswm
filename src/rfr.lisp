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
	  remember
	  remember-group
	  remember-screen
	  remember-desktop
	  remember-all
	  remember-window-placement
	  remember-group-windows-placement
	  remember-screen-windows-placement
	  remember-all-window-placement
	  forget
	  forget-group
	  forget-screen
	  forget-desktop
	  forget-all
	  forget-window-placement
	  forget-group-windows-placement
	  forget-screen-windows-placement
	  forget-all-window-placement
	  recall
	  recall-group
	  recall-screen
	  recall-desktop
	  recall-all
	  recall-window-placement
	  recall-group-windows-placement
	  recall-screen-windows-placement
	  recall-all-window-placement
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

(defun gdump-member-of-list-p (gdump dump-list)
  "Check, if gdump is member of dump part"
  (cond ((null dump-list)
  	 nil)
  	((eq (type-of (car dump-list)) 'gdump)
  	 (if (or
  	      (eq (group-dump-number gdump) (gdump-number (car dump-list)))
  	      (equal (group-dump-name gdump) (gdump-number (car dump-list))))
  	     (car dump-list)
	   (gdump-member-of-list-p gdump (cdr dump-list))))
  	((eq (type-of (car dump-list)) 'fgdump)
  	 (if (or
  	      (eq (group-dump-number gdump) (fgdump-number (car dump-list)))
  	      (equal (group-dump-name gdump) (fgdump-number (car dump-list))))
  	     (car dump-list)
	   (gdump-member-of-list-p gdump (cdr dump-list))))))

(defun group-dump-number (dump)
  (cond ((eq (type-of dump) 'gdump)
	 (gdump-number dump))
	((eq (type-of dump) 'fgdump)
	 (fgdump-number dump))))

(defun group-dump-name (dump)
  (cond ((eq (type-of dump) 'gdump)
	 (gdump-name dump))
	((eq (type-of dump) 'fgdump)
	 (fgdump-name dump))))

(defun update-gdump-member-of-list (gdump dump-list)
  "Replace, group dump with new group dump in dump list"
  (cond ((null dump-list)
  	 nil)
  	((eq (type-of (car dump-list)) 'gdump)
  	 (if (or
  	      (eq (group-dump-number gdump) (gdump-number (car dump-list)))
  	      (eq (group-dump-name gdump) (gdump-number (car dump-list))))
	     (cons gdump (cdr dump-list))
  	     (cons (car dump-list) (update-gdump-member-of-list gdump (cdr dump-list)))))
  	((eq (type-of (car dump-list)) 'fgdump)
  	 (if (or
  	      (eq (group-dump-number gdump) (fgdump-number (car dump-list)))
  	      (eq (group-dump-name gdump) (fgdump-number (car dump-list))))
	     (cons gdump (cdr dump-list))
  	     (cons (car dump-list) (update-gdump-member-of-list gdump (cdr dump-list)))))))

(defun remove-gdump-member-of-list (gdump dump-list) ;; TODO
  "Remove, group dump with new group dump in dump list"
  (cond ((null dump-list)
  	 nil)
  	((eq (type-of (car dump-list)) 'gdump)
  	 (if (or
  	      (eq (group-dump-number gdump) (gdump-number (car dump-list)))
  	      (eq (group-dump-number gdump) (gdump-number (car dump-list))))
	     (cdr dump-list)
  	     (cons (car dump-list) (remove-gdump-member-of-list gdump (cdr dump-list)))))
  	((eq (type-of (car dump-list)) 'fgdump)
  	 (if (or
  	      (eq (group-dump-number gdump) (fgdump-number (car dump-list)))
  	      (eq (group-dump-name gdump) (fgdump-number (car dump-list))))
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
  (let ((screen-number (screen-id (group-screen group)))
  	(desktop-rules *desktop-rules*))
    (labels ((add-gdump-to-sdump-list (gdump list)
  	       (if (null (car list))
  		   nil
  		   (let ((cur-sdump (car list)))
  		     (cond ((eq screen-number (sdump-number cur-sdump))
  			    (setf (sdump-groups cur-sdump) (update-group-in-rules group (group-screen group)))
			    (cons cur-sdump (add-gdump-to-sdump-list gdump (cdr list))))
  			   (t
  			    (cons (car list) (add-gdump-to-sdump-list gdump (cdr list)))))))))
      (progn
	(if (not (sdump-member-of-list-p (dump-screen (group-screen group)) (ddump-screens *desktop-rules*)))
	    (progn
	      (push (dump-screen (group-screen group)) (ddump-screens *desktop-rules*))
	      (setf (sdump-groups (car (ddump-screens *desktop-rules*))) nil)))

	(setf (ddump-screens *desktop-rules*) (add-gdump-to-sdump-list (dump-group group) (ddump-screens desktop-rules)))
	(if-not-null permanent-p
		     (dump-to-file *desktop-rules* *desktop-dump-file*))
	))))

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
  (if (not (rule-matching-window window))
      (progn
	(make-rule-for-window window lock-p title-p)
	(if-not-null permanent-p
		     (dump-to-file *window-placement-rules* *window-placement-dump-file*)))
      (and
       (forget-window-placement :window window)
       (remember-window-placement :window window :lock-p lock-p :title-p title-p :permanent-p permanent-p))))


(defun remember-group-windows-placement (&key (group (current-group)) (lock-p t) (title-p nil) (permanent-p nil))
  "Guess at a placement rule for all WINDOWS in group and add it to the current set."
  (if (> (length (group-windows group)) 0)
      (progn
	(forget-group-windows-placement :group group)
	(dolist (i (group-windows group))
	  (remember-window-placement :window i
				     :lock-p lock-p
				     :title-p title-p
				     :permanent-p permanent-p)) t))) ; dolist always gives nil

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
  (let ((screen-number (screen-id (group-screen group)))
	(desktop-rules *desktop-rules*))
    (labels ((remove-gdump-from-sdump-list (gdump list)
	       (if (null (car list))
		   nil
		   (let ((cur-sdump (car list)))
		     (cond ((eq screen-number (sdump-number cur-sdump))
			    (setf (sdump-groups cur-sdump) (remove-group-from-rules group (group-screen group)))
			    (cons cur-sdump (remove-gdump-from-sdump-list gdump (cdr list))))
			   (t
			    (cons (car list) (remove-gdump-from-sdump-list gdump (cdr list)))))))))
      (progn
      	(setf (ddump-screens *desktop-rules*) (remove-gdump-from-sdump-list (dump-group group) (ddump-screens desktop-rules)))
      	(if-not-null permanent-p
      		     (dump-to-file *desktop-rules* *desktop-dump-file*))))))

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
  (dolist (i *window-placement-rules*)
    (if (equal (group-name group) (car i))
     	(setf *window-placement-rules*
	(delete i *window-placement-rules*))))
  (if-not-null permanent-p
	       (dump-to-file *window-placement-rules*
			     *window-placement-dump-file*)))

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
  (labels ((get-gdump (group sdump-list)
	     (if-not-null
	      sdump-list
	      (let ((member-p
		     (gdump-member-of-list-p
		      (dump-group group)
		      (sdump-groups (car sdump-list)))))
		(cond ((and
			(eq (screen-id (group-screen group))
			    (sdump-number (car sdump-list)))
			(not (null member-p)))
		       member-p)
		      (t (get-gdump group (cdr sdump-list))))))))
    (progn
      (if-not-null permanent-p
		   (setf *desktop-rules*
			 (read-dump-from-file *desktop-dump-file*)))
      (let ((dump
	     (get-gdump group (ddump-screens *desktop-rules*))))
	(if-not-null dump
                     (restore-group group dump nil 'window-id t))))))

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
		 (if (file-exists-p *desktop-dump-file*)
		     (setf *desktop-rules*
			   (read-dump-from-file *desktop-dump-file*))))
    (restore-desktop *desktop-rules*) t))

(defun recall-window-placement (&key (window (current-window)) (permanent-p nil))
  (progn
    (if-not-null permanent-p
		 (if (file-exists-p *window-placement-dump-file*)
		     (setf *window-placement-rules*
			   (read-dump-from-file *window-placement-dump-file*))))
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
		 (if (file-exists-p *window-placement-dump-file*)
		     (setf *window-placement-rules*
			   (read-dump-from-file *window-placement-dump-file*))))
    (sync-windows-placement)))

(defun recall-all ()
  "Recall frame and group and windows placement rules of all groups and frames"
  (progn
    (if (file-exists-p *desktop-dump-file*)
	(recall-desktop :permanent-p t)
      (recall-desktop))
    (if (file-exists-p *window-placement-dump-file*)
	(recall-all-window-placement :permanent-p t)
      (recall-all-window-placement))
    ;; TODO: Add function for restore all programs, running in last session
    t) ;; unfortunately sync-window-placement gives nil, because dolist
  )

(defcommand remember (what) ((:rfr "What element do you want to remember? "))
  "Remember rules for some desktop element, like frames placement on group,
 group placement on screen, or window-placement"
  (cond ((equal what "group")
	 (remember-group :permanent-p *rfr-permanent-p*)
	(message "Group '~a' remembered" (group-name (current-group))))
	((equal what "screen")
	 (remember-screen :permanent-p *rfr-permanent-p*)
	 (message "Screen '~a' remembered" (screen-number (current-screen))))
	((equal what "window")
	 (remember-window-placement :permanent-p *rfr-permanent-p*)
	 (message "Window '~a' remembered" (window-name (current-window))))
	((equal what "group-windows")
	 (remember-group-windows-placement :permanent-p *rfr-permanent-p*)
	 (message "All windows in group '~a' remembered" (group-name (current-group))))
	((equal what "screen-windows")
	 (remember-screen-windows-placement :permanent-p *rfr-permanent-p*)
	 (message "All windows on screen '~a' remembered" (screen-number (current-screen))))
	((equal what "all-windows")
	 (remember-all-window-placement :permanent-p *rfr-permanent-p*)
	 (message "All windows  remembered"))
	((equal what "desktop")
	 (remember-desktop :permanent-p *rfr-permanent-p*)
	 (message "Desktop remembered"))
	((equal what "all")
	 (remember-all :permanent-p *rfr-permanent-p*)
	 (message "All objects remembered"))))

(defcommand forget (what) ((:rfr "What element do you want to forget? "))
  "Forget remembered rules for some desktop element, like frames
placement on group, group placement on screen, or window-placement"
  (cond ((equal what "group")
	 (forget-group :permanent-p *rfr-permanent-p*)
	 (message "Group '~a' forgot" (group-name (current-group))))
	((equal what "screen")
	 (forget-screen :permanent-p *rfr-permanent-p*)
	 (message "Screen '~a' forgot" (screen-number (current-screen))))
	((equal what "window")
	 (forget-window-placement :permanent-p *rfr-permanent-p*)
	 (message "Window '~a' forgot" (window-name (current-window))))
	((equal what "group-windows")
	 (forget-group-windows-placement :permanent-p *rfr-permanent-p*)
	 (message "All windows in group '~a' forgot" (group-name (current-group))))
	((equal what "screen-windows")
	 (forget-screen-windows-placement :permanent-p *rfr-permanent-p*)
	 (message "All windows on screen '~a' forgot" (screen-number (current-screen))))
	((equal what "all-windows")
	 (forget-all-window-placement :permanent-p *rfr-permanent-p*)
	 (message "All windows  forgot"))
	((equal what "desktop")
	 (forget-desktop :permanent-p *rfr-permanent-p*)
	 (message "Desktop forgot"))
	((equal what "all")
	 (forget-all :permanent-p *rfr-permanent-p*)
	 (message "All objects forgot"))))

(defcommand recall (what) ((:rfr "What element do you want to recall? "))
  "Recall remembered rules for some desktop element, like frames
placement on group, group placement on screen, or window-placement"
  (cond ((equal what "group")
	 (recall-group :permanent-p *rfr-permanent-p*)
	 (message "Group '~a' recalled" (group-name (current-group))))
	((equal what "screen")
	 (recall-screen :permanent-p *rfr-permanent-p*)
	 (message "Screen '~a' recalled" (screen-number (current-screen))))
	((equal what "window")
	 (recall-window-placement :permanent-p *rfr-permanent-p*)
	 (message "Window '~a' recalled" (window-name (current-window))))
	((equal what "group-windows")
	 (recall-group-windows-placement :permanent-p *rfr-permanent-p*)
	 (message "All windows in group '~a' recalled" (group-name (current-group))))
	((equal what "screen-windows")
	 (recall-screen-windows-placement :permanent-p *rfr-permanent-p*)
	 (message "All windows on screen '~a' recalled" (screen-number (current-screen))))
	((equal what "all-windows")
	 (recall-all-window-placement :permanent-p *rfr-permanent-p*)
	 (message "All windows  recalled"))
	((equal what "desktop")
	 (recall-desktop)
	 (message "Desktop recalled"))
	((equal what "all")
	 (recall-all)
	 (message "All objects recalled"))))

;;;;
;;;;
;; /END
;;;;
;;;;

