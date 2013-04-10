;; fdump.lisp -- Layout save and restore routines.
;; Copyright (C) 2007-2008 Jonathan Liles, Shawn Betts
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

;; Code:

(in-package #:dswm)

(export '(ddump
          ddump-current
          ddump-screens
          dump-desktop-to-file
          dump-group-to-file
          dump-screen-to-file
          fdump
          fdump-current
          fdump-height
          fdump-number
          fdump-width
          fdump-windows
          fdump-x
          fdump-y
          gdump
          gdump-current
          gdump-name
          gdump-number
          gdump-tree
          place-existing-windows
          restore
          sdump
          sdump-current
          sdump-groups
          sdump-number))

(defun dump-group (group &optional (window-dump-fn 'window-id))
  "Make group dump"
  (labels ((dump (f)
	     (make-fdump
	      :windows (mapcar window-dump-fn (frame-windows group f))
	      :current (and (frame-window f)
			    (funcall window-dump-fn (frame-window f)))
	      :number (frame-number f)
	      :x (frame-x f)
	      :y (frame-y f)
	      :width (frame-width f)
	      :height (frame-height f)))
           (copy (tree)
	     (cond ((null tree) tree)
		   ((typep tree 'frame)
		    (dump tree))
		   (t
		    (mapcar #'copy tree)))))
    (cond ((eq (type-of group) 'tile-group)
	   (make-gdump
	    ;; we only use the name and number for screen and desktop restores
	    :number (group-number group)
	    :name (group-name group)
	    :tree (copy (tile-group-frame-tree group))
	    :current (frame-number (tile-group-current-frame group))))
	  ((eq (type-of group) 'float-group)
	   (make-fgdump
	    :number (group-number group)
	    :name (group-name group))))))

(defun dump-screen (screen)
  (make-sdump :number (screen-id screen)
              :current (group-number (screen-current-group screen))
              :groups (mapcar 'dump-group (sort-groups screen))))

(defun dump-desktop ()
  (make-ddump :screens (mapcar 'dump-screen *screen-list*)
              :current (screen-id (current-screen))))

(defun dump-to-file (foo file &key backup-p)
  "Dump foo structure to file"
  (if (ensure-directories-exist file)
      (progn
	(when (and backup-p (file-exists-p file))
	  (let ((directory (pathname-directory file))
		(name (pathname-name file))
		(suffix "~"))
	    (copy-file file
		       (make-pathname
			:directory directory
			:name (concat name suffix))
		       :overwrite t)))
	(with-open-file (fp file :direction :output :if-exists :supersede)
		      (with-standard-io-syntax
		       (let ((*package* (find-package :dswm))
			     (*print-pretty* t))
			 (prin1 foo fp)))))
    (error "Cannot dump file ~a. Cannot create directory" file)))

(defun dump-group-to-file (file)
  "Dumps the frames of the current group of the current screen to the named file."
  (dump-to-file (dump-group (current-group)) file))

(defun dump-screen-to-file (file)
  "Dumps the frames of all groups of the current screen to the named file"
  (dump-to-file (dump-screen (current-screen)) file))

(defun dump-desktop-to-file (file)
  "Dumps the frames of all groups of all screens to the named file"
  (dump-to-file (dump-desktop) file))


;;;

(defun read-dump-from-file (file)
  (with-open-file (fp file :direction :input)
    (with-standard-io-syntax
      (let ((*package* (find-package :dswm)))
        (read fp)))))

(defun restore-group (group gdump &optional auto-populate (window-dump-fn 'window-id))
  "Restore group from group dump"
  (let ((windows (group-windows group)))
    (labels ((give-frame-a-window (f)
               (unless (frame-window f)
                 (setf (frame-window f) (find f windows :key 'window-frame))))
             (restore (fd)
               (let ((f (make-frame
                         :number (fdump-number fd)
                         :x (fdump-x fd)
                         :y (fdump-y fd)
                         :width (fdump-width fd)
                         :height (fdump-height fd))))
                 ;; import matching windows
                 (if auto-populate
                     (choose-new-frame-window f group)
                     (progn
                       (dolist (w windows)
                         (when (equal (fdump-current fd) (funcall window-dump-fn w))
                           (setf (frame-window f) w))
                         (when (find (funcall window-dump-fn w) (fdump-windows fd) :test 'equal)
                           (setf (window-frame w) f)))))
                 (when (fdump-current fd)
                   (give-frame-a-window f))
                 f))
             (copy (tree)
               (cond ((null tree) tree)
                     ((typep tree 'fdump)
                      (restore tree))
                     (t
                      (mapcar #'copy tree)))))
      (cond
	((and (eq (type-of group) 'tile-group)
	      (eq (type-of gdump) 'gdump))
	;; clear references to old frames
	 (dolist (w windows)
	   (setf (window-frame w) nil))
	 (setf (tile-group-frame-tree group) (copy (gdump-tree gdump))
	       (tile-group-current-frame group) (find (gdump-current gdump) (group-frames group) :key 'frame-number))
	 ;; give any windows still not in a frame a frame
	 (dolist (w windows)
	   (unless (window-frame w)
	     (setf (window-frame w) (tile-group-current-frame group))))
	 ;; FIXME: if the current window was blank in the dump, this does not honour that.
	 (give-frame-a-window (tile-group-current-frame group))
	 ;; raise the curtains
	 (dolist (w windows)
	   (if (eq (frame-window (window-frame w)) w)
	       (unhide-window w)
	       (hide-window w)))
	 (sync-all-frame-windows group)
	 (focus-frame group (tile-group-current-frame group)))
	;; If group is float and dump is float
	((and (eq (type-of group) 'float-group)
	      (eq (type-of gdump) 'fgdump))
	 (setf (group-name group) (fgdump-name gdump)))
	;; If group is float and dump is tile or group is tile and dump is float
	(t
	 (convert-group group) ;; TODO: TEST! TEST! TEST!!!
	 (restore-group group gdump))))))

(defun restore-screen (screen sdump)
  "Restore all frames in all groups of given screen. Create groups if
 they don't already exist."
  (dolist (gdump (sdump-groups sdump))
    (cond ((eq (type-of gdump) 'gdump)
	   (format t "~a~%" (gdump-name gdump))
	   (restore-group
	    (or
	     (find-group screen (gdump-name gdump))
	     ;; FIXME: if the group doesn't exist then
	     ;; windows won't be migrated from existing
	     ;; groups
	     (add-group screen (gdump-name gdump)))
	    gdump))
	  ((eq (type-of gdump) 'fgdump)
	   (format t "~a~%" (fgdump-name gdump))
	   (format t "~a"
		   (restore-group
		    (or
		     (find-group screen (fgdump-name gdump))
		     (add-group screen (fgdump-name gdump) :type 'float-group))
		    gdump))))))

(defun restore-desktop (ddump)
  "Restore all frames, all groups, and all screens."
  (dolist (sdump (ddump-screens ddump))
    (let ((screen (find (sdump-number sdump) *screen-list*
                        :key 'screen-id :test '=)))
      (when screen
        (restore-screen screen sdump)))))

(defun restore-from-file (file)
  "Restores screen, groups, or frames from named file, depending on file's contents."
  (let ((dump (read-dump-from-file file)))
    (typecase dump
      ((or gdump fgdump)
       (restore-group (current-group) dump)
       (message "Group restored."))
      (sdump
       (restore-screen (current-screen) dump)
       (message "Screen restored."))
      (ddump
       (restore-desktop dump)
       (message "Desktop restored."))
      (t
       (message "Don't know how to restore ~a" dump)))))

;; Maybe it needed?
;; (defcommand-alias restore restore-from-file)
