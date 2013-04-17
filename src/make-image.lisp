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
#-(or sbcl cmucl clisp openmcl ecl ccl)
(error "This lisp implementation is not supported.")

(require 'asdf)
#+(or :clisp :ecl :cmucl) (require "clx") ;; because clisp and ecl uses it's own CLX module
#-ecl (asdf:oos 'asdf:load-op 'dswm)
#+ecl (asdf:oos 'asdf:load-fasl-op 'dswm)

#+sbcl
(sb-ext:save-lisp-and-die "dswm" :toplevel (lambda ()
                                                ;; asdf requires sbcl_home to be set, so set it to the value when the image was built
                                                (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
                                                (dswm:dswm)
                                                0)
                          :executable t)

#+cmucl
(save-lisp "dswm" :init-function (lambda ()
				   (dswm:dswm)
				   0)
	   :executable t)

#+clisp
(ext:saveinitmem "dswm" :init-function (lambda ()
					 (dswm:dswm)
					 (ext:quit))
                 :executable t :keep-global-handlers t :norc nil :documentation "The DSWM Executable")

#+ccl
(ccl:save-application "dswm" :prepend-kernel t :toplevel-function #'dswm:dswm)

#+ecl
(asdf:make-build 'dswm :type :program :monolithic t
                 :move-here "."
                 :name-suffix ""
                 :epilogue-code '(dswm:dswm))
