;; Copyright (C) 2008 Shawn Betts
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
;; Help and introspection commands
;;
;; Code:

(in-package #:dswm)

(export '(version
	  describe-key
	  describe-variable
	  describe-function
	  describe-command
	  where-is
	  frame-info
	  window-info
	  window-full-info))

;; Version
(defparameter *version*
  #.(concat "20150324" " Compiled On "
	    (format-expand *time-format-string-alist*
			   *time-format-string-default*)))

(defparameter *start-message-format* (format nil "
^7*Welcome to The ^BD^beep ^BS^bpace ^BW^bindow ^BM^banager!
This is first-start message for quick ^BDSWM^b usage learning
-------------------------------------------------------------
-------------------------------------------------------------
^BPlease, be aware! DSWM moved to freedesktop recommendations
for files and directories location. You must move your files
to correct locations (see locations below)^b
-------------------------------------------------------------
-------------------------------------------------------------
* Press ^5*~a ?^7* for more detail help.
* Your config-file is:
    ^5*~a.dswm^7* or
    ^5*~a^7*
* Your own modules directory is:
    ^5*~amodules/^7*
* Use prefix key ^5*~a^7* with keybindings

Basic keybindings:
+--------------+--------------------------------------------+
| Keybinding   | Role                                       |
+--------------+--------------------------------------------+
|     !        | Run program by name                        |
|     ;        | Run commandline for internal DSWM commands |
|     r        | Resize frame                               |
|     s        | Split frame horizontally                   |
|     S        | Splt frame vertically                      |
|     R        | Remove split                               |
|     m <N>    | Move window to frame with number <N>       |
|     <N>      | Select window with number <N>              |
|     F<N>     | Select group with number <N>               |
|     g c      | Create group                               |
|     g k      | Remove group                               |
+-----------------------------------------------------------+

* Run in internal command ^Bdescribe-key or describe-command^b etc
  for describing keybinding or command
* ^BYou can always show this message^b, entering ~a h^b, or using
  internal command ^Bhelp-short^b
Notes:
^BVersion: ~a
License: GNU GPL v.2^b"
					     (print-key *escape-key*)
					     (princ-to-string
					      (user-homedir-pathname))
					     (princ-to-string
					      (conf-dir-file "init" "lisp"))
					     (princ-to-string
					      (data-dir))
					     (print-key *escape-key*)
					     (print-key *escape-key*) *version*))

(defun version () ()
"Print version information and compilation date."
  (format nil *version*))

(defun columnize (list columns &key col-aligns (pad 1) (char #\Space) (align :left))
  "Make conlumns for messages from list"
  ;; only somewhat nasty
  (let* ((rows (ceiling (length list) columns))
         (data (loop for i from 0 below (length list) by rows
                     collect (subseq list i (min (+ i rows) (length list)))))
         (max (mapcar (lambda (col)
                        (reduce 'max col :key 'length :initial-value 0))
                      data))
         (padstr (make-string pad :initial-element char)))
    (apply 'mapcar 'concat
           ;; normalize width
           (loop
            for i in data
            for j in max
            for c from 0
            collect (loop
                     for k from 0 below rows
                     for s = (or (nth k i) "")
                     for len = (make-string (- j (length s))
                                            :initial-element char)
                     collect (ecase (or (nth c col-aligns) align)
                               (:left (format nil "~a~a~a" (if (= c 0) "" padstr) s len))
                               (:right (format nil "~a~a~a" (if (= c 0) "" padstr) len s))))))))

(defun display-bindings-for-keymaps (key-seq &rest keymaps)
  (let* ((screen (current-screen))
         (data (mapcan (lambda (map)
                         (mapcar (lambda (b) (format nil "^5*~5a^n ~a" (print-key (binding-key b)) (binding-command b))) (kmap-bindings map)))
                       keymaps))
         (cols (ceiling (1+ (length data))
                        (truncate (- (head-height (current-head)) (* 2 (screen-msg-border-width screen)))
                                  (font-height (screen-font screen))))))
    (message-no-timeout "Prefix: ~a~%~{~a~^~%~}"
                        (print-key-seq key-seq)
                        (columnize data cols))))

(defun first-start (&optional show)
  (let ((not-firststart-p (conf-dir-file ".started" "p"))
	(*message-window-gravity* :center))
    (when (or (not (file-exists-p not-firststart-p))
	       show)
      (progn (message-no-timeout *start-message-format*)
	     (ensure-directories-exist not-firststart-p)
	     (with-open-file (stream not-firststart-p :if-does-not-exist :create)
			     t)))))

(defcommand help-short () ()
  "Display short help"
  (first-start t))

(defcommand commands () ()
"List all available commands."
  (let* ((screen (current-screen))
         (data (all-commands))
	 (*message-window-gravity* :center)
         (cols (ceiling (length data)
                        (truncate (- (head-height (current-head)) (* 2 (screen-msg-border-width screen)))
                                  (font-height (screen-font screen))))))
    (message-no-timeout "~{~a~^~%~}"
                        (columnize data cols))))

;;; describe commands

(defcommand describe-key (keys) ((:key-seq "Type key to describe: "))
"Either interactively type the key sequence or supply it as text. This
command prints the command bound to the specified key sequence."
  (let ((cmd (loop for map in (top-maps)
                   for cmd = (lookup-key-sequence map keys)
                   when cmd return cmd))
	(*message-window-gravity* :center))
    (if cmd
        (message "Key ~{~a~^ ~}~%is bound to\"~a\" command." (mapcar 'print-key keys)  cmd)
        (message "~{~a~^ ~} is not bound." (mapcar 'print-key keys)))))

(defcommand describe-variable (var) ((:variable "Input variable name to describe: "))
"Print the online help associated with the specified variable."
  (let ((*message-window-gravity* :center))
  (message-no-timeout "~a"
                      (with-output-to-string (s)
                        (describe var s)))))

(defcommand describe-function (fn) ((:function "Input function name to describe: "))
"Print the online help associated with the specified function."
  (let ((*message-window-gravity* :center))
  (message-no-timeout "~a"
                      (with-output-to-string (s)
                        (describe fn s)))))

(defcommand describe-command (com) ((:command "Input command to describe: "))
  "Print the online help associated with the specified command."
  (let* ((deref (dereference-command-symbol com))
         (struct (get-command-structure com nil))
	 (*message-window-gravity* :center))
    (cond ((null struct)
           (message "Error: Command \"~a\" not found." com))
          ((eq deref struct)
           (message-no-timeout "Command \"~a\":~%~a" (command-name struct)
                               (documentation (command-name struct) 'function)))
          (t
           (message-no-timeout "\"~a\" is an alias for the command \"~a\":~%~a" (command-alias-from deref) (command-name struct)
                               (documentation (command-name struct) 'function))))))

(defcommand where-is (cmd) ((:command "Input command to detect to wat key it bind: "))
"Print the key sequences bound to the specified command."
(let ((bindings (loop for map in (top-maps) append (search-kmap cmd map)))
      (*message-window-gravity* :center))
  (if bindings
      (message-no-timeout "\"~a\" is on ~{~a~^, ~}"
                      cmd
                      ;; (mapcar 'print-key-seq bindings)
		      "ddd"
		      )
      (message-no-timeout "Command \"~a\" is not currently bound"
                      cmd))))

;;; Window-properties commands
(defcommand window-info (&optional (fmt *window-info-format*)) (:rest)
  "Display information about the current window."
  (if (current-window)
      (message "~a" (format-expand *window-formatters* fmt (current-window)))
      (message "No Current Window")))

(defcommand window-full-info () ()
  "List all the properties of the current window and their values,
like xprop."
  (message-no-timeout
   "~{~30a: ~a~^~%~}"
   (let ((win (if (current-window)
                  (window-xwin (current-window))
                  (screen-root (current-screen)))))
     (loop for i in (xlib:list-properties win)
        collect i
        collect (multiple-value-bind (values type)
                    (xlib:get-property win i)
                  (case type
                    (:wm_state (format nil "~{~a~^, ~}"
                                       (loop for v in values
                                          collect (case v (0 "Iconic") (1 "Normal") (2 "Withdrawn") (t "Unknown")))))
                    (:window i)
                    ;; _NET_WM_ICON is huuuuuge
                    (:cardinal (if (> (length values) 20)
                                   (format nil "~{~d~^, ~}..." (subseq values 0 15))
                                   (format nil "~{~d~^, ~}" values)))
                    (:atom (format nil "~{~a~^, ~}"
                                   (mapcar (lambda (v) (xlib:atom-name *display* v)) values)))
                    (:string (format nil "~{~s~^, ~}"
                                     (mapcar (lambda (x) (coerce (mapcar 'xlib:card8->char x) 'string))
                                             (split-seq values '(0)))))
                    (:utf8_string (format nil "~{~s~^, ~}"
                                          (mapcar 'utf8-to-string
                                                  (split-seq values '(0)))))
                    (t values)))))))

(defcommand-alias info window-info)

(defcommand modifiers () ()
  "List the modifiers dswm recognizes and what MOD-X it thinks they're on."
 (let ((*message-window-gravity* :center))
  (message "~@{~5@a: ~{~(~a~)~^ ~}~%~}"
           "Meta" (modifiers-meta *modifiers*)
           "Alt" (modifiers-alt *modifiers*)
           "Super" (modifiers-super *modifiers*)
           "Hyper" (modifiers-hyper *modifiers*)
           "AltGr" (modifiers-altgr *modifiers*))))

;;; Error reporting commands
(defcommand copy-unhandled-error () ()
  "When an unhandled error occurs, DSWM restarts and attempts to
continue. Unhandled errors should be reported to the mailing list so
they can be fixed. Use this command to copy the unhandled error and
backtrace to the X11 selection so you can paste in your email when
submitting the bug report."
  (if *last-unhandled-error*
      (progn
        (set-x-selection (format nil "~a~%~a" (first *last-unhandled-error*) (second *last-unhandled-error*)))
        (message "Copied to clipboard."))
      (message "There was no unhandled error!")))
