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
;; Many useful extensions for using with
;; filesystem, system environments etc
;; Some code given from cl-fad
;;
;; Code:

(in-package :dswm)

#+:sbcl (require :sb-executable)
#+:sbcl (require :sb-posix)

(export '(add-hook
	  add-to-list
	  backtrace-string
	  backup-file
	  basename
	  cat
	  clisp-subdirectories-wildcard
	  command-mode-end-message
	  command-mode-start-message
	  component-present-p
	  conc1
	  copy-file
	  copy-stream
	  define-frame-preference
	  delete-directory-and-files
	  deny-request-p
	  directory-exists-p
	  directory-pathname-p
	  directory-wildcard
	  dirname
	  eval-with-message 
	  file-exists-p
	  find-free-number
	  font-height
	  format-expand
	  getenv
	  get-frame-number-translation
	  get-uid
	  if-not-null
	  if-null
	  insert-before
	  intern1
          link-p
          link
	  list-directory
	  list-splice-replace
	  mapcar-hash
	  move-to-head
	  my-rad-fn
	  pathname-as-directory
	  pathname-as-file
	  pathname-is-executable-p
	  probe-path
	  remove-from-list
	  remove-hook
	  remove-plist
	  run-hook
	  run-hook-with-args
	  sort1
	  split-seq
	  split-string
	  string-as-directory
	  walk-directory
	  with-focus
	  with-restarts-menu))
 
;;;; Pathname operations
(defun pathname-is-executable-p (pathname)
  "Return T if the pathname describes an executable file."
  (declare (ignorable pathname))
  #+sbcl
  (let ((filename (coerce (sb-ext:native-namestring pathname) 'string)))
    (and (or (pathname-name pathname)
             (pathname-type pathname))
         (sb-unix:unix-access filename sb-unix:x_ok)))
  ;; FIXME: this is not exactly perfect
  #+clisp
  (logand (posix:convert-mode (posix:file-stat-mode (posix:file-stat pathname)))
          (posix:convert-mode '(:xusr :xgrp :xoth)))
  #-(or sbcl clisp) (error "Not implemented"))

(defun probe-path (path)
  "Return the truename of a supplied path, or nil if it does not exist."
  (handler-case
      (truename
       (let ((pathname (pathname path)))
         ;; If there is neither a type nor a name, we have a directory
         ;; pathname already. Otherwise make a valid one.
         (if (and (not (pathname-name pathname))
                  (not (pathname-type pathname)))
             pathname
             (make-pathname
              :directory (append (or (pathname-directory pathname)
                                     (list :relative))
                                 (list (file-namestring pathname)))
              :name nil :type nil :defaults pathname))))
    (file-error () nil)))

(defun basename (pathname)
  "Returns basename of given path"
  (make-pathname :name
		 (pathname-name
		  (pathname pathname))
		 :type
		 (pathname-type
		  (pathname pathname))))

(defun dirname (pathname)
  "Returns dirname of path"
  (make-pathname :directory
		 (pathname-directory
		  (pathname pathname))))

(defun backup-file (pathname &key (overwrite nil))
  "Backup file to file with same name, but `~` at the tail"
  (let ((new-pathname
	 (merge-pathnames
	  (make-pathname :type (concat (pathname-type pathname) "~")) pathname)))
    (when (and
	   (file-exists-p pathname)
	   (or (eq overwrite t) (not (file-exists-p pathname))))
      (copy-file (make-pathname :defaults pathname)
		 new-pathname
		 :overwrite overwrite)
      new-pathname)))

(defun cat (file)
  "Like UNIX command 'cat'" ;; FIXME: It's working not completely correct
  (let ((a nil))
    (with-open-file (in file)
      (loop for line = (read-line in nil nil)
	 while line
	 do
	   (setf a (append a (list line)))))
    (car (format nil "~a~%" a))))

;;;; Code from cl-fad

(defun component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC \(a pathname designator) does not designate
a directory, PATHSPEC otherwise.  It is irrelevant whether file or
directory designated by PATHSPEC does actually exist."
  (and 
    (not (component-present-p (pathname-name pathspec)))
    (not (component-present-p (pathname-type pathspec)))
    pathspec))

(defun pathname-as-directory (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory
form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((not (directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

(defun directory-wildcard (dirname)
  "Returns a wild pathname designator that designates all files within
the directory named by the non-wild pathname designator DIRNAME."
  (when (wild-pathname-p dirname)
    (error "Can only make wildcard directories from non-wildcard directories."))
  (make-pathname :name  :wild
                 :type #-:clisp :wild
		       #+:clisp nil
                 :defaults (pathname-as-directory dirname)))

#+:clisp
(defun clisp-subdirectories-wildcard (wildcard)
  "Creates a wild pathname specifically for CLISP such that
sub-directories are returned by DIRECTORY."
  (make-pathname :directory (append (pathname-directory wildcard)
                                    (list :wild))
                 :name nil
                 :type nil
                 :defaults wildcard))

(defun list-directory (dirname)
  "Returns a fresh list of pathnames corresponding to the truenames of
all files within the directory named by the non-wild pathname
designator DIRNAME.  The pathnames of sub-directories are returned in
directory form - see PATHNAME-AS-DIRECTORY."
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))
    #+:sbcl (directory wildcard)
    #+:clisp (nconc (directory wildcard :if-does-not-exist :keep)
                    (directory (clisp-subdirectories-wildcard wildcard))))
  #-(or :sbcl :clisp)
  (error "Not implemented"))

(defun pathname-as-file (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to file form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((directory-pathname-p pathspec)
           (let* ((directory (pathname-directory pathname))
                  (name-and-type (pathname (first (last directory)))))
             (make-pathname :directory (butlast directory)
                            :name (pathname-name name-and-type)
                            :type (pathname-type name-and-type)
                            :defaults pathname)))
          (t pathname))))

(defun file-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and returns its truename if this is the case, NIL otherwise.
The truename is returned in `canonical' form, i.e. the truename of a
directory is returned as if by PATHNAME-AS-DIRECTORY."
  #+:sbcl (probe-file pathspec)
  #+:clisp (or (ignore-errors
                 (let ((directory-form (pathname-as-directory pathspec)))
                   (when (ext:probe-directory directory-form)
                     directory-form)))
               (ignore-errors
                 (probe-file (pathname-as-file pathspec)))))

(defun directory-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and if it is a directory.  Returns its truename if this is the
case, NIL otherwise.  The truename is returned in directory form as if
by PATHNAME-AS-DIRECTORY."
  (let ((result (file-exists-p pathspec)))
    (and result
         (directory-pathname-p result)
         result)))

(defun walk-directory (dirname fn &key directories
                                       (if-does-not-exist :error)
                                       (test (constantly t)))
  "Recursively applies the function FN to all files within the
directory named by the non-wild pathname designator DIRNAME and all of
its sub-directories.  FN will only be applied to files for which the
function TEST returns a true value.  If DIRECTORIES is not NIL, FN and
TEST are applied to directories as well.  If DIRECTORIES is :DEPTH-FIRST,
FN will be applied to the directory's contents first.  If
DIRECTORIES is :BREADTH-FIRST and TEST returns NIL, the
directory's content will be skipped. IF-DOES-NOT-EXIST must be
one of :ERROR or :IGNORE where :ERROR means that an error will be
signaled if the directory DIRNAME does not exist."
  (labels ((walk (name)
             (cond
               ((directory-pathname-p name)
                ;; the code is written in a slightly awkward way for
                ;; backward compatibility
                (cond ((not directories)
                       (dolist (file (list-directory name))
                         (walk file)))
                      ((eql directories :breadth-first)
                       (when (funcall test name)
                         (funcall fn name)
                         (dolist (file (list-directory name))
                           (walk file))))
                      ;; :DEPTH-FIRST is implicit
                      (t (dolist (file (list-directory name))
                           (walk file))
                         (when (funcall test name)
                           (funcall fn name)))))
               ((funcall test name)
                (funcall fn name)))))
    (let ((pathname-as-directory (pathname-as-directory dirname)))
      (case if-does-not-exist
        ((:error)
         (cond ((not (file-exists-p pathname-as-directory))
                (error "File ~S does not exist."
                       pathname-as-directory))
               (t (walk pathname-as-directory))))
        ((:ignore)
         (when (file-exists-p pathname-as-directory)
           (walk pathname-as-directory)))
        (otherwise
         (error "IF-DOES-NOT-EXIST must be one of :ERROR or :IGNORE."))))
    (values)))

(defvar *stream-buffer-size* 8192)

(defun copy-stream (from to &optional (checkp t))
  "Copies into TO \(a stream) from FROM \(also a stream) until the end
of FROM is reached, in blocks of *stream-buffer-size*.  The streams
should have the same element type.  If CHECKP is true, the streams are
checked for compatibility of their types."
  (when checkp
    (unless (subtypep (stream-element-type to) (stream-element-type from))
      (error "Incompatible streams ~A and ~A." from to)))
  (let ((buf (make-array *stream-buffer-size*
                         :element-type (stream-element-type from))))
    (loop
       (let ((pos #-:clisp (read-sequence buf from)
                  #+:clisp (ext:read-byte-sequence buf from :no-hang nil)))
         (when (zerop pos) (return))
         (write-sequence buf to :end pos))))
  (values))

(defun copy-file (from to &key overwrite)
  "Copies the file designated by the non-wild pathname designator FROM
to the file designated by the non-wild pathname designator TO.  If
OVERWRITE is true overwrites the file designtated by TO if it exists."
  (let ((element-type '(unsigned-byte 8)))
    (with-open-file (in from :element-type element-type)
      (with-open-file (out to :element-type element-type
                              :direction :output
                              :if-exists (if overwrite
					     :supersede
					     :error))
        (copy-stream in out))))
  (values))

(defun delete-directory-and-files (dirname &key (if-does-not-exist :error))
  "Recursively deletes all files and directories within the directory
designated by the non-wild pathname designator DIRNAME including
DIRNAME itself.  IF-DOES-NOT-EXIST must be one of :ERROR or :IGNORE
where :ERROR means that an error will be signaled if the directory
DIRNAME does not exist."
  (walk-directory dirname
		  (lambda (file)
		    (cond ((directory-pathname-p file)
			   #+:sbcl (sb-posix:rmdir file)
			   #+:clisp (ext:delete-directory file))
			  (t (delete-file file))))
		  :directories t
		  :if-does-not-exist if-does-not-exist)
  (values))

;;;; /Code from cl-fad

(defun get-uid ()
  "get uid"
  #+sbcl (sb-unix:unix-getuid)
  #+clisp (posix:uid)
  #-(or sbcl clisp) (error "Not implemented"))

(defmacro move-to-head (list elt)
   "Move the specified element in in LIST to the head of the list."
 `(progn
    (setf ,list (remove ,elt ,list))
    (push ,elt ,list)))

(define-condition dswm-error (error)
  () (:documentation "Any dswm specific error should inherit this."))

(defun intern1 (thing &optional (package *package*) (rt *readtable*))
  "A DWIM intern."
  (intern
   (ecase (readtable-case rt)
     (:upcase (string-upcase thing))
     (:downcase (string-downcase thing))
     ;; Prooobably this is what they want? It could make sense to
     ;; upcase them as well.
     (:preserve thing)
     (:invert (string-downcase thing)))
   package))

(defun command-mode-start-message ()
  (message "Press C-g to exit command-mode."))

(defun command-mode-end-message ()
  (message "Exited command-mode."))

(defmacro with-focus (xwin &body body)
  "Set the focus to xwin, do body, then restore focus"
  `(progn
     (grab-keyboard ,xwin)
     (unwind-protect
          (progn ,@body)
       (ungrab-keyboard))))

(defmacro define-frame-preference (target-group &rest frame-rules)
  "Create a rule that matches windows and automatically places them in
a specified group and frame. Each frame rule is a lambda list:
@example
\(frame-number raise lock &key create restore dump-name class instance type role title)
@end example

@table @var
@item frame-number
The frame number to send matching windows to

@item raise
When non-nil, raise and focus the window in its frame

@item lock
When this is nil, this rule will only match when the current group
matches @var{target-group}. When non-nil, this rule matches regardless
of the group and the window is sent to @var{target-group}. If
@var{lock} and @var{raise} are both non-nil, then dswm will jump to
the specified group and focus the matched window.

@item create
When non-NIL the group is created and eventually restored when the value of
create is a group dump filename in *DATA-DIR*. Defaults to NIL.

@item restore
When non-NIL the group is restored even if it already exists. This arg should
be set to the dump filename to use for forced restore. Defaults to NIL

@item class
The window's class must match @var{class}.

@item instance
The window's instance/resource name must match @var{instance}.

@item type
The window's type must match @var{type}.

@item role
The window's role must match @var{role}.

@item title
The window's title must match @var{title}.
@end table"
  (let ((x (gensym "X")))
    `(dolist (,x ',frame-rules)
       ;; verify the correct structure
       (destructuring-bind (frame-number raise lock
                                         &rest keys
                                         &key create restore class instance type role title) ,x
         (declare (ignore create restore class instance type role title))
         (push (list* ,target-group frame-number raise lock keys)
               *window-placement-rules*)))))

(defun backtrace-string ()
  "Similar to print-backtrace, but return the backtrace as a string."
  (with-output-to-string (*standard-output*)
    (print-backtrace)))

(defun deny-request-p (window deny-list)
  (or (eq deny-list t)
      (and
       (listp deny-list)
       (find-if (lambda (props)
                  (apply 'window-matches-properties-p window props))
                deny-list)
       t)))

(defun list-splice-replace (item list &rest replacements)
  "splice REPLACEMENTS into LIST where ITEM is, removing
ITEM. Return the new list."
  (let ((p (position item list)))
    (if p
        (nconc (subseq list 0 p) replacements (subseq list (1+ p)))
        list)))

(defun font-height (font)
  (+ (xlib:font-descent font)
     (xlib:font-ascent font)))

(defun format-expand (fmt-alist fmt &rest args)
  (let* ((chars (coerce fmt 'list))
         (output "")
         (cur chars))
    ;; FIXME: this is horribly inneficient
    (loop
     (cond ((null cur)
            (return-from format-expand output))
           ;; if % is the last char in the string then it's a literal.
           ((and (char= (car cur) #\%)
                 (cdr cur))
            (setf cur (cdr cur))
            (let* ((tmp (loop while (and cur (char<= #\0 (car cur) #\9))
                              collect (pop cur)))
                   (len (and tmp (parse-integer (coerce tmp 'string))))
                   ;; So that eg "%25^t" will trim from the left
                   (from-left-p (when (char= #\^ (car cur)) (pop cur))))
              (if (null cur)
                  (format t "%~a~@[~a~]" len from-left-p)
                  (let* ((fmt (cadr (assoc (car cur) fmt-alist :test 'char=)))
                         (str (cond (fmt
                                     ;; it can return any type, not jut as string.
                                     (format nil "~a" (apply fmt args)))
                                    ((char= (car cur) #\%)
                                     (string #\%))
                                    (t
                                     (concat (string #\%) (string (car cur)))))))
                    ;; crop string if needed
                    (setf output (concat output
					 (cond ((null len) str)
					       ((not from-left-p) ; Default behavior
						(subseq str 0 (min len (length str))))
					       ;; New behavior -- trim from the left
					       (t (subseq str (max 0 (- (length str) len)))))))
                    (setf cur (cdr cur))))))
           (t
            (setf output (concatenate 'string output (string (car cur)))
                  cur (cdr cur)))))))

(defun insert-before (list item nth)
  "Insert ITEM before the NTH element of LIST."
  (declare (type (integer 0 *) nth))
  (let* ((nth (min nth (length list)))
         (pre (subseq list 0 nth))
         (post (subseq list nth)))
    (nconc pre (list item) post)))

(defun conc1 (list arg)
  "Append arg to the end of list"
  (nconc list (list arg)))

(defmacro add-to-list (list arg)
  "Adds element to list, if this element is not a list member"
  `(if (not (member ,arg ,list))
       (pushnew ,arg ,list)))

(defmacro remove-from-list (list arg)
  "Removes element from list"
  `(labels
    ((rm-from-list (list arg)
	(cond
	 ((null list) nil)
	 ((equal arg (car list))
	  (rm-from-list (cdr list) arg))
	 (t (cons (car list) (rm-from-list (cdr list) arg))))))
    (setf ,list (rm-from-list ,list ,arg))))

(defmacro if-not-null (value body &optional else-body)
  "Replaces <<(if (not (null X)))>> construction. It`s really better"
  `(if (not (null ,value))
       ,body
     ,(if (not (null else-body))
	  else-body)))

(defmacro if-null (value body &optional else-body)
  "Replaces <<(if (null X))>> construction. It`s really better"
  `(if (null ,value)
       ,body
     ,(if (not (null else-body))
	  else-body)))

(defun sort1 (list sort-fn &rest keys &key &allow-other-keys)
  "Return a sorted copy of list."
  (let ((copy (copy-list list)))
    (apply 'sort copy sort-fn keys)))

(defun mapcar-hash (fn hash)
  "Just like maphash except it accumulates the result in a list."
  (let ((accum nil))
    (labels ((mapfn (key val)
               (push (funcall fn key val) accum)))
      (maphash #'mapfn hash))
    accum))

(defun find-free-number (l &optional (min 0) dir)
  "Return a number that is not in the list l. If dir is :negative then
look for a free number in the negative direction. anything else means
positive direction."
  (let* ((dirfn (if (eq dir :negative) '> '<))
         ;; sort it and crop numbers below/above min depending on dir
         (nums (sort (remove-if (lambda (n)
                                  (funcall dirfn n min))
                                l) dirfn))
         (max (car (last nums)))
         (inc (if (eq dir :negative) -1 1))
         (new-num (loop for n = min then (+ n inc)
                        for i in nums
                        when (/= n i)
                        do (return n))))
    (dformat 3 "Free number: ~S~%" nums)
    (if new-num
        new-num
        ;; there was no space between the numbers, so use the max+inc
        (if max
            (+ inc max)
            min))))

(defun remove-plist (plist &rest keys)
  "Remove the keys from the plist.
Useful for re-using the &REST arg after removing some options."
  (do (copy rest)
      ((null (setq rest (nth-value 2 (get-properties plist keys))))
       (nreconc copy plist))
    (do () ((eq plist rest))
      (push (pop plist) copy)
      (push (pop plist) copy))
    (setq plist (cddr plist))))

(defun split-seq (seq separators &key test default-value)
  "split a sequence into sub sequences given the list of seperators."
  (let ((seps separators))
    (labels ((sep (c)
               (find c seps :test test)))
      (or (loop for i = (position-if (complement #'sep) seq)
                then (position-if (complement #'sep) seq :start j)
                as j = (position-if #'sep seq :start (or i 0))
                while i
                collect (subseq seq i j)
                while j)
          ;; the empty seq causes the above to return NIL, so help
          ;; it out a little.
          default-value))))

(defun split-string (string &optional (separators "
"))
  "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
***If SEPARATORS is absent, it defaults to \"[ \f\t\n\r\v]+\".

If there is match for SEPARATORS at the beginning of STRING, we do not
include a null substring for that.  Likewise, if there is a match
at the end of STRING, we don't include a null substring for that.

Modifies the match data; use `save-match-data' if necessary."
  (split-seq string separators :test #'char= :default-value '("")))

(defmacro with-restarts-menu (&body body)
  "Execute BODY. If an error occurs allow the user to pick a
restart from a menu of possible restarts. If a restart is not
chosen, resignal the error."
  (let ((c (gensym)))
    `(handler-bind
         ((warning #'muffle-warning)
          ((or serious-condition error)
           (lambda (,c)
             (restarts-menu ,c)
             (signal ,c))))
       ,@body)))

;;; Hook functionality

(defun run-hook-with-args (hook &rest args)
  "Call each function in HOOK and pass args to it."
  (handler-case
      (with-simple-restart (abort-hooks "Abort running the remaining hooks.")
        (with-restarts-menu
            (dolist (fn hook)
              (with-simple-restart (continue-hooks "Continue running the remaining hooks.")
                (apply fn args)))))
    (t (c) (message "^B^1*Error on hook ^b~S^B!~% ^n~A" hook c) (values nil c))))

(defun run-hook (hook)
  "Call each function in HOOK."
  (run-hook-with-args hook))

(defmacro add-hook (hook fn)
  "Add @var{function} to the hook @var{hook-variable}. For example, to
display a message whenever you switch frames:

@example
\(defun my-rad-fn (to-frame from-frame)
  (dswm:message \"Mustard!\"))

\(dsmwm:add-hook dswm:*focus-frame-hook* 'my-rad-fn)
@end example"
  `(setf ,hook (adjoin ,fn ,hook)))

(defmacro remove-hook (hook fn)
"Remove the specified function from the hook."
  `(setf ,hook (remove ,fn ,hook)))

(defun get-frame-number-translation (frame)
  "Given a frame return its number translation using *frame-number-map* as a char."
  (let ((num (frame-number frame)))
    (or (and (< num (length *frame-number-map*))
             (char *frame-number-map* num))
        ;; translate the frame number to a char. FIXME: it loops after 9
        (char (prin1-to-string num) 0))))

(defmethod print-object ((object frame) stream)
  (format stream "#S(frame ~d ~a ~d ~d ~d ~d)"
          (frame-number object) (frame-window object) (frame-x object) (frame-y object) (frame-width object) (frame-height object)))

(defun string-as-directory (dir)
  (unless (string= "/" (subseq dir (1- (length dir))))
    (setf dir (concat dir "/")))
  (pathname dir))

(defmacro eval-with-message (&key body
				  body-alternative
				  message-if-done
				  message-if-false)
  "Eval someting s-expression with messages, when it's done and false"
  `(if ,body
       ,(if-not-null
	 message-if-done
	 `(message ,message-if-done)
	 t)
     ,(if (member 'interactive *mode*)
	  (cond ((not (null body-alternative))
		 `body-alternative)
		((not (null message-if-false))
		 `(error ,message-if-false))
		(t nil))
	(if-not-null
	 message-if-false
	 `(error ,message-if-false)
	 nil))))

(defun link (from to &key (type :symbolic))
  (cond ((eq type :symbolic)
         #+sbcl
         (ignore-errors (sb-posix:symlink from to))
         #-sbcl
         (error "Not implemented"))
        ((eq type :hard)
         (error "Hard link not implemented yet"))
        (t
         (error "Invalid type ~a" type))))

(defun link-p (target)
  #+sbcl
  (sb-posix:s-islnk
   (sb-posix:stat-mode
    (sb-posix:lstat target)))
  #-sbcl
  (error "Not implemented"))
