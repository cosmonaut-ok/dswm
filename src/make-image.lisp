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
;; Code:

;;; SBCL
#+sbcl
(progn
  (require 'asdf)
  (require 'dswm))
#+sbcl
(progn
  (load "dswm.asd")
  (sb-ext:save-lisp-and-die "dswm" :toplevel (lambda ()
                                                  ;; asdf requires sbcl_home to be set, so set it to the value when the image was built
                                                  (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
                                                  (dswm:dswm)
                                                  0)
                            :executable t))

;;; CLISP

;; Is there a better way to use asdf.lisp than including it with dswm?
#+clisp
(progn
  (load "asdf.lisp")
  (require 'asdf)
  (load "dswm.asd"))
;  (load "@PPCRE_PATH@/cl-ppcre.asd"))
#+clisp
(progn
  (asdf:oos 'asdf:load-op 'dswm))
#+clisp
(progn
  (ext:saveinitmem "dswm" :init-function (lambda ()
                                              (dswm:dswm)
                                              (ext:quit))
                   :executable t :keep-global-handlers t :norc t :documentation "The DSWM Executable"))

#-(or sbcl clisp) (error "This lisp implementation is not supported.")
