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
;; Window Manager commands that users can use to manipulate dswm.
;;
;; Code:

(in-package :dswm)

(export '(defprogram-shortcut
          pathname-is-executable-p
	  programs-in-path
	  restarts-menu
	  run-or-raise
	  run-or-pull
	  run-in-terminal
	  run-shell-command
	  run-shell-commands
          window-send-string
	  ratwarp
	  ratrelwarp
	  ratclick
	  editor
	  browser
	  terminal))

(defun restarts-menu (err)
  "Display a menu with the active restarts and let the user pick
one. Error is the error being recovered from. If the user aborts the
menu, the error is re-signalled."
  (let ((restart (select-from-menu (current-screen)
                                   (mapcar (lambda (r)
                                             (list (format nil "[~a] ~a"
                                                           (restart-name r)
                                                           (substitute #\Space
                                                                       #\Newline
                                                                       (write-to-string r :escape nil)))
                                                   r))
                                           ;; a crusty way to get only
                                           ;; the restarts from
                                           ;; dswm's top-level
                                           ;; restart inward.
                                           (reverse (member 'top-level
                                                            (reverse (compute-restarts))
                                                            :key 'restart-name)))
                                   (format nil "Error: ~a"
                                           (substitute #\Space
                                                       #\Newline
                                                       (write-to-string err :escape nil))))))
    (when restart
      (invoke-restart (second restart)))))

(defun banish-pointer (&optional (where *banish-pointer-to*))
  "Move the pointer to the lower right corner of the head, or
 WHEREever (one of :screen :head :frame or :window)"
  ;; FIXME: do it for (current-window) etc. For correct behavior
  ;; in :sloopy mouse policy
  (let* ((screen (current-screen))
         (group (current-group))
         (head (current-head))
         (frame (tile-group-current-frame group))
         (window (frame-window frame))
         (x (1- (+ (frame-x frame) (frame-width frame))))
         (y (1- (+ (frame-display-y group frame) (frame-display-height group frame)))))
    (ecase where
      (:screen
       (setf x (1- (+ (screen-x screen) (screen-width screen)))
             y (1- (+ (screen-y screen) (screen-height screen)))))
      (:head
       (setf x (1- (+ (head-x head) (head-width head)))
             y (1- (+ (head-y head) (head-height head)))))
      (:frame)
      (:window
       (when window
         (let ((win (window-parent window)))
           (setf x (1- (+ (xlib:drawable-x win) (xlib:drawable-width win)))
                 y (1- (+ (xlib:drawable-y win) (xlib:drawable-height win))))))))
    (warp-pointer (group-screen group) x y)))

(defcommand banish (&optional where) (:rest)
  "Warp the mouse the lower right corner of the current head."
  (if where
      (banish-pointer (intern1 where :keyword))
      (banish-pointer)))

(defun ratwarp (x y)
  "Warp the mouse to the specified location."
  (warp-pointer (current-screen) x y))

(defun ratrelwarp (dx dy)
  "Warp the mouse by the specified amount from its current position."
  (warp-pointer-relative dx dy))

(defun ratclick (&optional (button 1))
  "Simulate a pointer button event at the current pointer
location. Note: this function is unlikely to work unless
your X server and CLX implementation support XTEST."
  (when (current-window)
    (send-fake-click (current-window) button)))

(defun programs-in-path (&optional full-path (path (split-string (getenv "PATH") ":")))
  "Return a list of programs in the path. if @var{full-path} is
@var{t} then return the full path, otherwise just return the
filename. @var{path} is by default the @env{PATH} evironment variable
but can be specified. It should be a string containing each directory
seperated by a colon."
  (sort
   (loop
      for p in path
      for dir = (probe-path p)
      when dir
      nconc (loop
               for file in
	       ;; SBCL doesn't match files with types if type
	       ;; is not wild and CLISP won't match files
	       ;; without a type when type is wild. So cover all the bases
	       ;; TODO: check other compilers
	       #+clisp (directory-no-deref (merge-pathnames (make-pathname :name :wild) dir))
	       #-clisp (directory-no-deref (merge-pathnames (make-pathname :name :wild :type :wild) dir))
               for namestring = (file-namestring file)
               when (pathname-is-executable-p file)
               collect (if full-path
                           (namestring file)
                           namestring)))
   #'string<))

(defstruct path-cache
  programs modification-dates paths)

(defvar *path-cache* nil
  "A cache containing the programs in the path, used for completion.")

(defun rehash (&optional (paths (mapcar 'parse-namestring (split-string (getenv "PATH") ":"))))
  "Update the cache of programs in the path stored in @var{*programs-list*} when needed."
  (let ((dates (mapcar (lambda (p)
                         (when (probe-path p)
                           (portable-file-write-date p)))
                       paths)))
    (finish-output)
    (unless (and *path-cache*
                 (equal (path-cache-paths *path-cache*) paths)
                 (equal (path-cache-modification-dates *path-cache*) dates))
      (setf *path-cache* (make-path-cache :programs (programs-in-path nil paths)
                                          :modification-dates dates
                                          :paths paths)))))

(defun complete-program (base)
  "return the list of programs in @var{*path-cache*} whose names begin
with base. Automagically update the cache."
  (rehash)
  (remove-if-not #'(lambda (p)
		     (when (<= (length base) (length p))
                       (string= base p
                                :end1 (length base)
                                :end2 (length base)))) (path-cache-programs *path-cache*)))

;; (defun complete-filename (base)
;;   "Return the list of files in ")

(defun shell-program ()
  (let ((shell (or *shell-program* (getenv "SHELL") (princ-to-string (which "sh")))))
    (if-null shell
	     (error "No shell found and it was not defined in *shell-program* variable")
	     (princ-to-string shell))))

(defcommand run-shell-command (cmd &optional collect-output-p)
  ((:shell "Input command to run program: "))
  "Run the specified shell command. If @var{collect-output-p} is @code{T}
then run the command synchonously and collect the output. Be
careful. If the shell command doesn't return, it will hang DSWM. In
such a case, kill the shell command to resume DSWM."
  (if collect-output-p
      (run-prog-collect-output (shell-program) "-c" cmd)
    (run-prog (shell-program) :args (list "-c" cmd) :wait nil)))

(defcommand-alias exec run-shell-command)

(defun run-shell-commands (&rest commands)
  "Run many shell commands, called by one function"
  (if (not (null commands))
	 (progn
	   (run-shell-command (car commands))
	   (eval (cons 'run-shell-commands (cdr commands))))))

(defcommand eval-line (cmd) ((:rest "Input S-expression to eval it: "))
  "Evaluate the s-expression and display the result(s)."
  (handler-case
      (message "^20~{~a~^~%~}"
               (mapcar 'prin1-to-string
                       (multiple-value-list (eval (read-from-string cmd)))))
    (error (c)
      (err "^B^1*~A" c))))

(defcommand-alias eval eval-line)

(defcommand-alias run run-shell-command)

(defcommand run-in-terminal (cmd)
  ((:shell "Input shell command to run it in terminal: "))
  "Run command in terminal"
  (terminal cmd))
  ;; (run-shell-command (concat *terminal* " -e " cmd)))

(defcommand echo (string) ((:rest "Echo: "))
  "Display @var{string} in the message bar."
  ;; The purpose of echo is always to pop up a message window.
  (let ((*executing-dswm-command* nil))
    (message "~a" string)))

(defun send-meta-key (screen key)
  "Send the key to the current window on the specified screen."
  (when (screen-current-window screen)
    (send-fake-key (screen-current-window screen) key)))

(defcommand meta (key) ((:key "Input key to send: "))
"Send a fake key to the current window. @var{key} is a typical DSWM key, like @kbd{C-M-o}."
  (send-meta-key (current-screen) key))

(defcommand loadrc () ()
  "Reload the @file{~/.dswm} file."
  (handler-case
   (progn
     (with-restarts-menu (load-rc-file nil t)))
   (error (c)
	  (message "^1*^BError loading rc file: ^n~A" c))
   (:no-error (&rest args)
	      (declare (ignore args))
	      (message "rc file loaded successfully."))))

(defcommand keyboard-quit () ()
    ""
  ;; This way you can exit from command mode
  (let ((in-command-mode (eq *top-map* *root-map*)))
    (when (pop-top-map)
      (if in-command-mode
        (run-hook *command-mode-end-hook*)
        (message "Exited.")))))

(defcommand-alias abort keyboard-quit)

(defcommand quit () ()
"Quit DSWM."
  (throw :top-level :quit))

(defcommand restart-soft () ()
  "Soft Restart DSWM. The lisp process isn't restarted. Instead,
control jumps to the very beginning of the dswm program. This
differs from RESTART, which restarts the unix process.

Since the process isn't restarted, existing customizations remain
after the restart."
  (throw :top-level :restart))

(defcommand restart-hard () ()
  "Restart dswm. This is handy if a new dswm executable has been
made and you wish to replace the existing process with it.

Any run-time customizations will be lost after the restart."
  (throw :top-level :hup-process))

(defun find-matching-windows (props all-groups all-screens)
  "Returns list of windows matching @var{props} (see run-or-raise
documentation for details). @var{all-groups} will find windows on all
groups. Same for @{all-screens}. Result is sorted by group and window
number, with group being more significant (think radix sort)."
  (let* ((screens (if all-screens
                      *screen-list*
                      (list (current-screen))))
         (winlist (if all-groups
                      (mapcan (lambda (s) (screen-windows s)) screens)
                      (group-windows (current-group))))
         (matches (remove-if-not (lambda (w)
                                   (apply 'window-matches-properties-p w props))
                                 winlist)))
    (stable-sort (sort matches #'< :key #'window-number)
                 #'< :key (lambda (w) (group-number (window-group w))))))

(defun run-or-raise (cmd props &optional (all-groups *run-or-raise-all-groups*) (all-screens *run-or-raise-all-screens*))
  "Run the shell command, @var{cmd}, unless an existing window
matches @var{props}. @var{props} is a property list with the following keys:

@table @code
@item :class
Match the window's class.
@item :instance
Match the window's instance or resource-name.
@item :role
Match the window's @code{WM_WINDOW_ROLE}.
@item :title
Match the window's title.
@end table

By default, the global @var{*run-or-raise-all-groups*} decides whether
to search all groups or the current one for a running
instance. @var{all-groups} overrides this default. Similarily for
@var{*run-or-raise-all-screens*} and @var{all-screens}."
  (labels
      ;; Raise the window win and select its frame.  For now, it
      ;; does not select the screen.
      ((goto-win (win)
         (let* ((group (window-group win))
                (frame (window-frame win))
                (old-frame (tile-group-current-frame group)))
           ;; (frame-raise-window group frame win) ;; TODO: stumpwm #68. Test
           (focus-all win)
           (unless (eq frame old-frame)
             (show-frame-indicator group)))))
    (let* ((matches (find-matching-windows props all-groups all-screens))
           ;; other-matches is list of matches "after" the current
           ;; win, if current win matches. getting 2nd element means
           ;; skipping over the current win, to cycle through matches
           (other-matches (member (current-window) matches))
           (win (if (> (length other-matches) 1)
                    (second other-matches)
                    (first matches))))
      (if win
          (goto-win win)
          (run-shell-command cmd)))))

(defun run-or-pull (cmd props &optional (all-groups *run-or-raise-all-groups*)
                    (all-screens *run-or-raise-all-screens*))
  "Similar to run-or-raise, but move the matching window to the
current frame instead of switching to the window."
  (let* ((matches (find-matching-windows props all-groups all-screens))
         ;; other-matches is for cycling through matches
         (other-matches (member (current-window) matches))
         (win (if (> (length other-matches) 1)
                  (second other-matches)
                  (first matches))))
    (if win
        (progn
          (move-window-to-group win (current-group))
          (pull-window win))
        (run-shell-command cmd))))

(defcommand reload () ()
  "Reload DSWM using @code{asdf}."
  #+(and asdf (not ecl))
  (progn
    (message "Reloading DSWM...")
    (with-restarts-menu
     (asdf:operate 'asdf:load-op :dswm))
    (message "Reloading DSWM...^B^2*Done^n."))
  #-asdf (message "^B^1*Sorry, DSWM can only be reloaded with asdf (for now.)")
  #+ecl (message "^B^1*Sorry, StumpWM cannot be reloaded when built as standalone program."))

(defcommand editor () ()
  "Start default DSWM editor unless it is already running, in which case focus it."
  (let* ((ed (or *editor*
		 (getenv "EDITOR")
		 (princ-to-string (which "emacs"))))
	 (ed-coerce (coerce (princ-to-string ed) 'list))
	 (ed-class
	  (coerce (cons (char-upcase (car ed-coerce)) (cdr ed-coerce)) 'string)))
    (if-null ed
	     (error "No editor found, and it was not defined in *editor* variable")
	     (run-or-raise (princ-to-string ed) (list :class ed-class)))))


(defcommand browser (&optional url) ()
  "Start default browser, defined in *browser* variable unless it is already running, in which case focus it."
  (let* ((br (or
	      (which *browser*)
	      (which (getenv "BROWSER"))
	      (which "conkeror")
	      (which "firefox")))
	 (br-coerce (coerce (princ-to-string (basename br)) 'list))
	 (br-class
	  (coerce (cons (char-upcase (car br-coerce)) (cdr br-coerce)) 'string)))
    (if-null br
	     (error "No browser found, and it was not defined in *browser* variable")
	     (if-not-null url
			  (run-shell-command (concat (princ-to-string br) url))
			  (run-or-raise (princ-to-string br) (list :class br-class))))))

(defcommand terminal (&optional command) ()
  "Run default terminal"
  (let ((term (or (which *terminal*) (which (getenv "TERM")) (which "xterm"))))
    (if-null term
	     (error "No terminal emulator found, and it was not defined in variable *terminal*")
	     (if (null command)
		 (run-shell-commands (princ-to-string term))
	       (run-shell-command (concat (princ-to-string term) " -e " command))))))

(defcommand lastcmd () ()
  "Repeat last inserted command"
  (labels
      ((find-last-command (list)
			  (let ((command (cl-ppcre:regex-replace "\ $" (car list) "")))
			    (cond
			     ((null list) nil)
			     ((or (equal command "lastcmd")
				  (equal command "colon")) ;; TODO: incorrect. We can call 'colon' then call 'PREF l' or 'eval' and '(lastcmd)' here
			      (find-last-command (cdr list)))
			     ((member command (all-commands) :test 'equal)
			      command)
			     (t
			      (find-last-command (cdr list)))))))
    (let ((command (find-last-command (gethash :command *input-history*))))
      (if command
	  (run-commands (princ-to-string command))
	(message "You not input any command yet")))))

(defmacro defprogram-shortcut (name &key (command (string-downcase (string name)))
                                         (props `'(:class ,(string-capitalize command)))
                                         (map *top-map*)
                                         (key (kbd (concat "H-" (subseq command 0 1))))
                                         (pullp nil)
                                         (pull-name (intern1 (concat (string name) "-PULL")))
                                         (pull-key (kbd (concat "H-M-" (subseq command 0 1)))))
  "Define a command and key binding to run or raise a program. If
@var{pullp} is set, also define a command and key binding to run or
pull the program."
  `(progn
     (defcommand ,name () ()
       (run-or-raise ,command ,props))
     (define-key ,map ,key ,(string-downcase (string name)))
     (when ,pullp
       (defcommand (,pull-name tile-group) () ()
          (run-or-pull ,command ,props))
       (define-key ,map ,pull-key ,(string-downcase (string pull-name))))))
