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
  (:use :cl :dswm :cl-ppcre))

;; (in-package :dswm.module.todo)
(in-package :dswm)

;; Install formatters.
;; (dolist (a '((#\T fmt-todo-stats)))
;;   (pushnew a *mode-line-formatters* :test 'equal))

(defvar *todo-priorities*
  '("Low" "Normal" "High" "Urgent"))

(defvar *todos-list* nil
  "Defines list of all todos, saved by user")

(defvar *todos-file* (data-dir-file "todos" "list" "save.d")
  "Defines default todo file")

(defstruct todo
  "Todo structure"
  name description priority)

(define-dswm-type :todo (input prompt)
  (or (argument-pop input)
      (completing-read (current-screen)
                       prompt (get-todo-names-list))))

(define-dswm-type :todo-priority (input prompt)
  (let ((match (member 
		(or (argument-pop input)
		    (completing-read (current-screen)
				     prompt *todo-priorities* :initial-input "Normal"))
		*todo-priorities* :test 'equal)))
    (or (car match)
        (throw 'error "Use standard priorities, or edit it (read the module manual, how to do this)"))))

(defun get-todo-names-list (&optional (list *todos-list*))
  "Gets list of TODO names"
  (cond
   ((null (car list)) nil)
   (t (cons (todo-name (car list))
	    (get-todo-names-list (cdr list))))))

(defun get-descriptions-list (&optional (list *todos-list*))
  (cond
    ((null (car list)) nil)
    (t (cons (todo-name (car list))
             (get-todo-names-list (cdr list))))))

(defun find-todo (&key name description priority (list *todos-list*))
  (cond ((null list) nil)
        ((or
          (and (not (null name))
               (equal name (todo-name (car list))))
          (and (not (null description))
               (equal description (todo-description (car list))))
          (and (not (null priority))
               (equal priority (todo-priority (car list)))))
         (car list))
        (t
         (find-todo
          :name name
          :description description
          :priority priority
          :list (cdr list)))))

(defmacro find-todo-by-name (name)
  `(find-todo :name ,name))

(defmacro find-todo-by-description (description)
  `(find-todo :description ,description))

(defmacro find-todo-by-priority (priority)
  `(find-todo :priority ,priority))

(defun dump-todos (&optional (tododump *todos-list*))
  (dump-structure tododump t *todos-file*))

(defun fmt-todos (ml)
  "Returns a string representing the current network activity."
  (declare (ignore ml))
  (format nil "TODOs number: ~a" (length *todos-list*)))

(defcommand todo-add (name description priority)
  ((:string "Enter todo name: ")
   (:rest "Explain, please, what do you want to do: ")
   (:todo-priority "How important this task for you (Low,Normal,High,Urgent)? "))
  "Add TODO to todo-list"
  (if
      (null (find-todo-by-name name))
      (progn
       (dswm::add-to-list *todos-list*
			  (make-todo
			   :name name
			   :description description
			   :priority priority))
       (message "todo ~a added" name)
       (dump-todos))
    (message "todo '~a' already exists" name)))

(defcommand todo-remove (name) ((:todo "What todo do you want to remove? "))
  "Remove TODO from todo-list"
  (if
      (and (remove-from-list *todos-list* (find-todo-by-name name))
	   (dump-todos))
      (message "todo ~a removed" name)
    (message "failed to remove ~a" name)))

(defcommand todo-list () ()
  "Show list of all active todo`s"
  (let ((list "Name~20tdescription~50tpriority~%
----------------------------------------------------------~%"))
    (dolist (i *todos-list*)
      (setf list (concatenate 'string list
			      (todo-name i) "~20t"
			      (todo-description i) "~50t"
			      (todo-priority i) "~10t~%")))
    (message-no-timeout list)))

(defcommand todo-reload () ()
  "Reread todo-list from file"
  (if (probe-file *todos-file*)
      (progn
        (setf *todos-list*
              (read-dump-from-file *todos-file*))
        (message "Loaded"))
    (message "Nothing to load")))

(add-mode-line-formatter #\T #'fmt-todos)
;; Initialization
(todo-reload)

;;; todo.lisp ends here

