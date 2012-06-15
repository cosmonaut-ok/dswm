;; Copyright (C) 2008 Julian Stecklina, Shawn Betts, Ivy Foster
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
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Commentary:
;;
;; Use `add-modules-dir' to add the location dswm searches for modules.

;; Directories with external modules:
;;
;; * $PREFIX/share/modules
;; * ~/.dswm.d/modules
;; * user-defined-dir

;; Code:

(in-package #:dswm)

(export '(load-module
          modules-list
	  *user-modules-dir*
	  add-modules-dir
          find-module))

(defvar *list-modules-dir* (list (string-as-directory "/usr/local/share/dswm/modules"))
  "The location of the contrib modules on your system.")

;; Add users home directory
(add-to-list *list-modules-dir* (merge-pathnames #p"modules/" (data-dir)))

(define-dswm-type :module (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt (modules-list) :require-match t)))

(defun modules-list ()
  "Return a list of the available modules."
  (let ((mod-list '()))
    (dolist (i *list-modules-dir*)
      (dolist (element
	       (mapcar 'pathname-name
		       (directory (make-pathname :defaults
						 (pathname (concat (princ-to-string i) "/system/"))
						 :name :wild
						 :type "asd"))))
	(add-to-list mod-list element)))
    mod-list))

(defun find-module (name)
  "Find module from list avaliable modules FIXME: test"
  (labels
      ((module-p (dir-list name)
	 (cond
	   ((null dir-list) nil)
	   ((file-exists-p (make-pathname :defaults (car dir-list) :name name :type "lisp"))
	    (make-pathname :defaults (pathname (concat (princ-to-string (car dir-list)) "/system/")) :name name :type "asd"))
	   (t (module-p (cdr dir-list) name)))))
    (module-p (reverse *list-modules-dir*) name)))


(defcommand add-modules-dir (dir) ((:string "Input directory name: "))
  ;; TODO: define dswm type pathname with autocomplete and automatic
  ;; directories creation
    "Add the location of the contrib modules"
    (add-to-list *list-modules-dir* (string-as-directory dir)))

(defcommand load-module (name) ((:module "Input module name: "))
  "Loads the contributed module with the given NAME."
  ;; FIXME: This should use ASDF in the future. And maybe there should
  ;; be an extra dswm-contrib repository.
  (when name
    (let ((module (find-module name)))
      (when module
          (load module)))))

(defcommand list-modules () ()
  (let ((list nil))
	  (dolist (w (modules-list))
	    (setf list (concat w "~%" list)))
	  (if (not (null list))
	      (message (format nil "~a" list))
	    (message "No modules found"))))


(defcommand module-description (name) ((:module "Input module name: "))
	    ;; class-based
	    (let ((module (find-system name)))
	      (when (class-p module 'system)
		(system-description module))))

(defcommand module-description (name) ((:module "Input module name: "))
	    ;; class-based
	    (let ((module (find-system name)))
	      (when (class-p module 'system)
		(system-description module))))

(defcommand module-description (name) ((:module "Input module name: "))
	    ;; class-based
	    (let ((module (find-system name)))
	      (when (class-p module 'system)
		(system-description module))))

(defcommand module-description (name) ((:module "Input module name: "))
	    ;; class-based
	    (let ((module (find-system name)))
	      (when (class-p module 'system)
		(system-description module))))



;; End of file
