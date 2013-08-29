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
;; All group related code resides here
;;
;; Code:

(in-package :dswm)

(export '(scratchpad))

(defun find-scratchpad-group (&optional (screen (current-screen)) grouplist)
  (let ((groups (if-not-null grouplist grouplist (screen-groups screen))))
    (labels ((find-zero-group (list)
	       (cond ((null list) nil)
		     ((eq (group-number (car list)) 0)
		      (car list))
		     (t (find-zero-group (cdr list))))))
      (find-zero-group groups))))

(defun scratchpad-init ()
  "Initializing scratchpad support"
  (let ((scratchpad-group (find-scratchpad-group)))
    (if (null scratchpad-group)
	(setf scratchpad-group (add-group (current-screen) *scratchpad-group-name* :background t)
	      (group-number scratchpad-group) 0))
    scratchpad-group))

(defcommand scratchpad () ()
  "Show and hide scratchpad group"
  (let* ((scratchpad-group (or (find-scratchpad-group)
			      (scratchpad-init)))
	 (groups (screen-groups (current-screen))))
    (if (and
	 (> (length groups) 1)
	 (not (null scratchpad-group)))
	(if (eq (current-group) scratchpad-group)
	    (switch-to-group (nth 1 groups))
	    (switch-to-group scratchpad-group))
	(message "Cannot initialise scratchpad group: ~a" scratchpad-group))))

(defcommand gmove-scratchpad () ()
  "Move the current window to the specified group."
  (if-not-null (current-window)
	       (if (eq (current-group) (find-scratchpad-group))
		   (move-window-to-group (current-window) (cadr (screen-groups (current-screen))))
		 (move-window-to-group (current-window) (find-scratchpad-group)))
	       (message "There is no window to move")))

(defcommand gmove-marked-scratchpad () ()
  "move the marked windows to the specified group."
  (if (eq (current-group) (find-scratchpad-group))
      (dolist (i (marked-windows (current-group)))
	(setf (window-marked i) nil)
	(move-window-to-group i (cadr (screen-groups (current-screen)))))
    (dolist (i (marked-windows (current-group)))
      (setf (window-marked i) nil)
      (move-window-to-group i (find-scratchpad-group)))))
