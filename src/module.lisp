;; Copyright (C) 2008 Julian Stecklina, Shawn Betts, Ivy Foster
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
;; Use `add-modules-dir' to add the location dswm searches for modules.

;; Directories with external modules:
;;
;; * $PREFIX/share/modules
;; * ~/.dswm.d/modules
;; * user-defined-dir

;; Code:

(in-package #:dswm)

(export '(add-modules-dir
	  load-module
          modules
	  modules-list
	  module-info
	  add-modules-dir
          find-module
	  load-file))

;; (defun add-modules-dir (dir)
;;   "Add the location of the contrib modules"
;;   (push (string-as-directory dir) *modules-load-paths-list*))

(defun add-modules-dir (dir)
  "Add the location of the contrib modules"
  (cond ((eq (type-of dir) 'pathname)
	 (push dir *modules-load-paths-list*))
	((stringp dir)
	 (push (string-as-directory dir) *modules-load-paths-list*))
	(t
	 (err "Incorrect type ~a. Cannot add to *modules-load-paths-list*" dir))))
 
;; TODO: remake!!!!!
;; Add default modules directories
(add-modules-dir "/usr/local/share/dswm/modules/")
(add-modules-dir (concat (princ-to-string (data-dir)) "modules.d")) ;; FIXME: it set statical value, when compilling!

(defun modules-list ()
  "Return a list of the available modules."
  (let ((mod-list nil))
    (dolist (module-path *modules-load-paths-list*)
      (dolist (subpath (list-directory module-path))
	(let ((local-name (car (last (pathname-directory subpath)))))
	  (when (and (not (member local-name mod-list :test 'equal))
		     (file-exists-p (make-pathname :defaults subpath :name local-name :type "asd")))
	    (push local-name mod-list)))))
    (sort mod-list #'string-lessp)))

(defun find-system-file (name)
  "Find module from list avaliable modules"
  (labels ((system-p (dir-list name)
		     (let ((system
			    (make-pathname :directory
                                           (cons :absolute
                                                 (string-to-path-list
                                                  (princ-to-string (car dir-list)) name))
                                           :name name :type "asd")))
		       (cond ((null dir-list) nil)
			     ((file-exists-p system) system)
			     (t (system-p (cdr dir-list) name))))))
	  (system-p *modules-load-paths-list* name)))

(defcommand load-module (name &optional force-p) ((:module "Input module name: ")) ;; TODO: rewrite. Not passing test 'module 1'
  "Loads the contributed module with the given NAME."                              ;; Maybe, because incorrect 'find-system-file' function
  (let ((dir (dirname (find-system-file name))))
    (if (or (not (member name *loaded-modules-list* :test 'equal)) force-p)
	(progn
	  (and (push dir asdf:*central-registry*) (require name))
	  (push name *loaded-modules-list*)
	  (format nil "Module ~a loaded successfully" name))
      (message "Module already loaded"))))

(defcommand reload-module (name) ((:module "Input module name: "))
  "Reloads module with given NAME"
  (when (not (member name *loaded-modules-list* :test 'equal))
      (message "Module not loaded yet. Loading"))
  (load-module name t)
  (asdf:operate 'asdf:load-op name)
  (if (not (member name *loaded-modules-list*))
      (message "Module ~a loaded successfully" name)
    (message "Module ~a reloaded successfully" name)))
  
(defcommand modules () ()
  "Shows modules list"
  (let ((list nil))
    (dolist (w (modules-list))
      (setf list (concat w "~%" list)))
    (if-not-null list
		 (message (format nil "~a" list))
		 (message "No modules found"))))

(defcommand loaded-modules () ()
  "Shows modules list"
  (let ((list nil))
    (dolist (w *loaded-modules-list*)
      (setf list (concat w "~%" list)))
    (if-not-null list
		 (message (format nil "~a" list))
		 (message "No modules found"))))

(defcommand module-info (name) ((:module "Input module name: "))
  "Shows full information about module"
  (load (find-system-file name))
  (let* ((module (asdf:find-system name))
	 (module-description
	  (handler-case (asdf:system-description module)
	    (unbound-slot () (format nil "No description"))))
	 (module-author
	  (handler-case (asdf:system-author module)
	   (unbound-slot () (format nil "Unknown"))))
	 (module-maintainer
	  (handler-case (asdf:system-maintainer module)
	    (unbound-slot () (format nil "Unknown"))))
	 (module-version
	  (handler-case (slot-value module 'asdf:version)
	    (unbound-slot () (format nil "Unknown"))))
	 (module-licence
	  (handler-case (asdf:system-licence module)
	    (unbound-slot () (format nil "No one defined (`as is'?)")))))
    (message (format nil "Name:~20t~a~%Description:~20t~a~%Author:~20t~a~%Maintainer:~20t~a~%Licence:~20t~a~%Version:~20t~a~%"
		     name module-description module-author module-maintainer module-licence module-version))))

(defcommand load-file (name &optional (data-file-p nil)) ((:file "Input path to file: "))
  "Loads file from location you input (optionally you can set data-file-p
and file would de loaded from this location)"
  (if-not-null data-file-p
      (if (file-exists-p (data-dir-file name))
	  (load (data-dir-file name))
	(message "No such file, named ~a" (princ-to-string (data-dir-file name))))
    (if (file-exists-p name)
	(load name)
      (message "No such file, named ~a" name))))

;; End of file
