;;; -*- Mode: LISP; Syntax: Common-lisp; Package: dswm.module.todo -*-

;; Copyright 2012 Alexander aka 'CosmonauT' Vynnyk
;;
;; Author: Alexander aka CosmonauT Vynnyk <cosmonaut.ok@gmail.com>
;; Version: id: web,v 0.1 20 Mar 2012 cosmonaut.ok@gmail.com
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;;==================================================================
;;; Filename: todo.lisp
;;; TODO for dswm
;;;==================================================================
;;
;;
;;; Code:
(defpackage :dswm.module.todo
  (:use :common-lisp :dswm :cl-ppcre))

(in-package :dswm.module.todo)

;; Install formatters.
;; (dolist (a '((#\T fmt-todo-stats)))
;;   (pushnew a *mode-line-formatters* :test 'equal))

(defvar *day-names*
  '("Mon" "Tue" "Wed"
    "Thu" "Fri" "Sat"
    "Sun"))

(defvar *statuses*
  '("new" "in progress" "done" "delayed")

(defvar *todos-list* nil
  "Defines list of all todos, saved by user")

(defvar *todos-file* (data-dir-file "todos" :type "list")
  "Defines default todo file")


(defstruct todo
  "Todo structure"
  name description time status)

(define-dswm-type :todo (input prompt)
  (or (argument-pop input)
      (completing-read (current-screen)
                       prompt
                       (get-todo-names-list))))

(define-dswm-type :todo-time (input prompt)
	   (let* ((current-time
		   (multiple-value-bind (s min h d m y d-o-w tz) (get-decoded-time) (format nil "~a ~a ~a ~a" h d m y)))
		 (time
		  (or (argument-pop-rest input)
			  (read-one-line (current-screen) prompt :initial-input current-time))))
	     (or
	      (ignore-errors
		(eval
		 (cons 'encode-universal-time
		       (with-input-from-string (in time) (loop for x = (read in nil nil) while x collect x)))))
	      (error "Incorrect date format"))))

(defun get-todo-names-list (&optional (list *todos-list*))
  (cond
    ((null (car list)) nil)
    (t (cons (todo-name (car list))
             (get-todo-names-list (cdr list))))))

(defun get-descriptions-list (&optional (list *todos-list*))
  (cond
    ((null (car list)) nil)
    (t (cons (todo-name (car list))
             (get-todo-names-list (cdr list))))))

(defun find-todo (&key name description time (list *todos-list*))
  (cond ((null list) nil)
        ((or
          (and (not (null name))
               (equal name (todo-name (car list))))
          (and (not (null description))
               (equal description (todo-description (car list))))
          (and (not (null time))
               (equal time (todo-time (car list)))))
         (car list))
        (t
         (find-todo
          :name name
          :description description
          :time time
          :list (cdr list)))))

(defmacro find-todo-by-name (name)
  `(find-todo :name ,name))

(defmacro find-todo-by-description (description)
  `(find-todo :description ,description))

(defmacro find-todo-by-time (time)
  `(find-todo :time ,time))

(defun dump-structure (structure to-fs file &optional backup-p)
  "Dump some code, values etc to file or just to output (from reoisitory. Not needed in dswm v.0.0.5"
  (if to-fs
      (progn
	(when (and backup-p (file-exists-p file))
	  (copy-file
	   file
	   (merge-pathnames
	    (make-pathname :type (concat (pathname-type file) "~")) file)
	   :overwrite t))
	(with-open-file (fp file :direction :output :if-exists :supersede)
			(with-standard-io-syntax
			 (let ((*package* (find-package :dswm))
			       (*print-pretty* t))
			   (prin1 structure fp))))
	structure)
    structure))

(defun dump-todos (&optional (tododump *todos-list*))
  (dswm::dump-structure tododump t *todos-file*))

;; (defun fmt-todo-list (ml)
;;   "Returns a string representing the current network activity."
;;   (declare (ignore ml))
;;   (let ((todos-number ___)
;; 	(todos-expired-number ___))
;;     ))

(defcommand todo-add (name description time)
  ((:string "Enter todo name: ")
   (:rest "Explain, please, what do you want to do: ")
   (:todo-time "To what date you have to do this task?(Format: sec min hour day mon year) "))
  (if
      (null (find-todo-by-name name))
      (progn
	(dswm::add-to-list *todos-list*
			   (make-todo
			    :name name
			    :description description
			    :time time))
	(dump-todos))
    (message "todo '~a' already exists" name)))

(defcommand todo-remove (name) ((:todo "What todo
  do you want to remove? "))
  (dswm::remove-from-list
   *todos-list*
   (find-todo-by-name name))
  (dump-todos))

(defcommand todo-list () ()
  (let ((list "Name~20tdescription~50ttime(hh:mm dd/mm/yyyy)~50tExpired?~%
---------------------------------------------------------------------------------~%"))
    (dolist (i *todos-list*)
      (let ((todo-decoded-time
	     (multiple-value-bind
		 (second minute hour date month year day-of-week dst-p tz)
		 (decode-universal-time (todo-time i))
	       (format nil "~2,'0d:~2,'0d ~2,'0d/~2,'0d/~d ~a"
		       hour
		       minute
		       date
		       month
		       year
		       (nth day-of-week *day-names*))))
	    (todo-expired-p (if (> (get-universal-time) (todo-time i)) "Expired" nil)))
	(setf list (concatenate 'string list
				(todo-name i) "~20t"
				(todo-description i) "~50t"
				todo-decoded-time "~50t"
				todo-expired-p "~%"))))
    (message list)))

(defcommand todo-reload () ()
  (if (probe-file *todos-file*)
      (progn
        (setf *todos-list*
              (dswm::read-dump-from-file *todos-file*))
        (message "Loaded"))
        (message "Nothing to load")))

;; Initialization
(todo-reload)

;;; todo.lisp ends here

