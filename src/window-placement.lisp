;; Copyright (C) 2003-2008 Shawn Betts
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
;; portability wrappers. Any code that must run different code for
;; different lisps should be wrapped up in a function and put here.
;;
;; Code:


(in-package :dswm)

(defun xwin-to-window (xwin)
  "Build a window for XWIN"
  (make-instance 'window
   :xwin xwin
   :width (xlib:drawable-width xwin) :height (xlib:drawable-height xwin)
   :x (xlib:drawable-x xwin) :y (xlib:drawable-y xwin)
   :title (xwin-name xwin)
   :class (xwin-class xwin)
   :res (xwin-res-name xwin)
   :role (xwin-role xwin)
   :type (xwin-type xwin)
   :wm-hints (xlib:wm-hints xwin)
   :normal-hints (get-normalized-normal-hints xwin)
   :state +iconic-state+
   :plist (make-hash-table)
   :unmap-ignores 0))

(defvar *rule-scanners-cache* (make-hash-table :test 'equal)
  "A cache for the ppcre scanners")

(defun get-or-create-rule-scanner (regex)
  (or (gethash regex *rule-scanners-cache*)
      (setf (gethash regex *rule-scanners-cache*)
	    (ppcre:create-scanner regex))))

(defun string-match (string pat)
  (ppcre:scan (get-or-create-rule-scanner pat) string))

(defun window-matches-properties-p (window &key class instance type role title)
  "Returns T if window matches all the given properties"
  (and
   (if class (string-match (window-class window) class) t)
   (if instance (string-match (window-res window) instance) t)
   (if type (string-match (window-type window) type) t)
   (if role (string-match (window-role window) role) t)
   (if title (string-match (window-title window) title) t) t))


(defun window-matches-rule-p (w rule)
  "Returns T if window matches rule"
  (destructuring-bind (group-name frame raise lock
                       &key create restore class instance type role title) rule
    (declare (ignore frame raise create restore))
    (if (or lock
            (equal group-name (group-name (or (when (slot-boundp w 'group)
                                                (window-group w))
                                              (current-group)))))
        (window-matches-properties-p w :class class
                                       :instance instance
                                       :type type
                                       :role role
                                       :title title))))

(defun rule-matching-window (window)
  (dolist (rule *window-placement-rules*)
    (when (window-matches-rule-p window rule) (return rule))))

(defun get-window-placement (screen window)
  "Returns the ideal group and frame that WINDOW should belong to and whether
  the window should be raised."
  (let ((match (rule-matching-window window)))
    (if match
        (destructuring-bind (group-name frame raise lock
                             &key create restore class instance type role title) match
          (declare (ignore lock class instance type role title))
          (let ((group (find-group screen group-name)))
            (cond (group
                   (when (and restore (stringp restore))
                     (let ((restore-file (data-dir-file restore)))
                       (if (file-exists-p restore-file)
                           (restore-group group
                                          (read-dump-from-file restore-file))
                           (message "^B^1*Can't restore group \"^b~a^B\" with \"^b~a^B\"."
                                    group-name restore-file))))
                   (values group (frame-by-number group frame) raise))
                  (create
                   (let ((new-group (add-group (current-screen) group-name))
                         (restore-file (if (stringp create)
                                           (data-dir-file create)
                                           (data-dir-file group-name))))
                     (if (and new-group
                              (file-exists-p restore-file))
                         (restore-group new-group
                                        (read-dump-from-file restore-file))
                         (when (stringp create)
                           (message "^B^1*Can't restore group \"^b~a^B\" with \"^b~a^B\"."
                                    group-name restore-file)))
                     (values new-group (frame-by-number new-group frame) raise)))
                    (t (message "^B^1*Error placing window, group \"^b~a^B\" does not exist." group-name)
                       (values)))))
        (values))))

(defun sync-window-placement ()
  "Re-arrange existing windows according to placement rules"
  (dolist (screen *screen-list*)
    (dolist (window (screen-windows screen))
      (multiple-value-bind (to-group frame raise)
          (with-current-screen screen
            (get-window-placement screen window))
        (declare (ignore raise))
        (when to-group
          (unless (eq (window-group window) to-group)
            (move-window-to-group window to-group))
          (unless (eq (window-frame window) frame)
            (pull-window window frame)))))))

(defun assign-window (window group &optional (where :tail))
  "Assign the window to the specified group and perform the necessary
housekeeping."
  (setf (window-group window) group
        (window-number window) (find-free-window-number group))
  (if (eq where :head)
      (push window (group-windows group))
      (setf (group-windows group) (append (group-windows group) (list window))))
  (setf (xwin-state (window-xwin window)) +iconic-state+)
  (netwm-set-group window))

(defun place-window (screen window &optional ignore-rules)
  "Pick a group WINDOW and return the group-specific placement hints, if any."
  (let* ((netwm-group (netwm-group window screen))
         (placement (multiple-value-list (get-window-placement screen window)))
         (placement-group (first placement))
         (group (or (when *processing-existing-windows*
                      netwm-group)
		    ;; TODO: realize through closure!
		    ;;     \/\/
		    (when (not ignore-rules)
		      placement-group)
                    netwm-group
                    (screen-current-group screen))))
    (assign-window window group (if *processing-existing-windows* :head :tail))
    ;; if we're using the placement group, then return the extra data.
    (when (eq group placement-group)
      (list :frame (second placement)
            :raise (third placement)))))

(defun pick-preferred-frame (window)
  (let* ((group (window-group window))
         (frames (group-frames group))
         (default (tile-group-current-frame group))
         (preferred-frame (or *new-window-preferred-frame* default)))
    (when (or (functionp *new-window-preferred-frame*)
              (and (symbolp *new-window-preferred-frame*)
                   (fboundp *new-window-preferred-frame*)))
      (setq preferred-frame
            (handler-case
                (funcall *new-window-preferred-frame* window)
              (t (c)
                (message "^1*^BError while calling ^b^3**new-window-preferred-frame*^1*^B: ^n~a" c)
                default))))
    (cond
      ;; If we already have a frame use it.
      ((frame-p preferred-frame)
       preferred-frame)
      ;; If `preferred-frame' is a list of keyword use it to determine the
      ;; frame.  The sanity check doesn't cover not recognized keywords.  We
      ;; simply fall back to the default then.
      ((and (listp preferred-frame)
            (every #'keywordp preferred-frame))
              (or
        (loop for i in preferred-frame
           thereis (case i
                     (:last
                      ;; last-frame can be stale
                      (and (> (length frames) 1)
                           (tile-group-last-frame group)))
                     (:unfocused
                      (find-if (lambda (f)
                                 (not (eq f (tile-group-current-frame group))))
                               frames))
                     (:empty
                      (find-if (lambda (f)
                                 (null (frame-window f)))
                               frames))
                     (:choice
                      ;; Transient windows sometimes specify a location
                      ;; relative to the TRANSIENT_FOR window. Just ignore
                      ;; these hints.
                      (unless (find (window-type window) '(:transient :dialog))
                        (let ((hints (window-normal-hints window)))
                          (when (and hints (xlib:wm-size-hints-user-specified-position-p hints))
                            (find-frame group (window-x window) (window-y window))))))))
        default))
      (t (message "^1*^BInvalid ^b^3**new-window-preferred-frame*^1*^B: ^n~a"
                  preferred-frame)
         default))))
