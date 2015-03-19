;; Copyright (C) 2007-2008 Shawn Betts
;; Copyright (C) 2010-2014 Alexander aka 'CosmonauT' Vynnyk <cosmonaut.ok@zoho.com>
;;
;; This file is part of dswm.
;;
;; dswm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; dswm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING. If not, see
;; <http://www.gnu.org/licenses/>.
;; Commentary:
;;
;; Generate the texinfo manual from docstrings in the source. Note,
;; this only works in sbcl, clisp and lispworks
;;
;; Code:
(in-package #:dswm)

#+sbcl (require :sb-introspect)

;; handy for figuring out which symbol is borking the documentation
(defun dprint (sym)
  (declare (ignorable sym))
  ;;(format t "~&Doing ~a..." sym))
  )

(defun generate-function-doc (s line)
  (ppcre:register-groups-bind (name) ("^@@@ (.*)" line)
															(dprint name)
															(let ((fn (if (find #\( name :test 'char=)
																						;; handle (setf <symbol>) functions
																						(with-standard-io-syntax
																							(let ((*package* (find-package :dswm)))
																								(fdefinition (read-from-string name))))
																						(symbol-function (find-symbol (string-upcase name) :dswm))))
																		(*print-pretty* nil))
																(format s "@defun {~a} ~{~a~^ ~}~%~a~&@end defun~%~%"
																				name
																				#+sbcl (sb-introspect:function-lambda-list fn)
																				#+clisp (ext:arglist fn)
																				#+ccl (ccl:arglist fn)
																				#+lispworks (lw:function-lambda-list fn)
																				#- (or sbcl clisp ccl lispworks) '("(Check the code for args list)")
																				(documentation fn 'function))
																t)))

(defun generate-macro-doc (s line)
  (ppcre:register-groups-bind (name) ("^%%% (.*)" line)
															(dprint name)
															(let* ((symbol (find-symbol (string-upcase name) :dswm))
																		 (*print-pretty* nil))
																(format s "@defmac {~a} ~{~a~^ ~}~%~a~&@end defmac~%~%"
																				name
																				#+sbcl (sb-introspect:function-lambda-list (macro-function symbol))
																				#+clisp (ext:arglist symbol)
																				#+ccl (ccl:arglist symbol)
																				#+lispworks (lw:function-lambda-list symbol)
																				#- (or sbcl clisp ccl lispworks) '("(Check the code for args list)")
;;; FIXME: when clisp compiles
;;; a macro it discards the
;;; documentation string! So
;;; unless when generating the
;;; manual for clisp, it is
;;; loaded and not compiled
;;; this will return NIL.
																				#+clisp (or (documentation symbol 'function)
																										"Due to a bug in clisp, macro function documentation is not generated. Try building the manual using sbcl.")
																				#-clisp (documentation symbol 'function))
																t)))

(defun generate-variable-doc (s line)
  (ppcre:register-groups-bind (name) ("^### (.*)" line)
															(dprint name)
															(let ((sym (find-symbol (string-upcase name) :dswm)))
																(format s "@defvar ~a~%~a~&@end defvar~%~%"
																				name (documentation sym 'variable))
																t)))

(defun generate-hook-doc (s line)
  (ppcre:register-groups-bind (name) ("^\\$\\$\\$ (.*)" line)
															(dprint name)
															(let ((sym (find-symbol (string-upcase name) :dswm)))
																(format s "@defvr {Hook} ~a~%~a~&@end defvr~%~%"
																				name (documentation sym 'variable))
																t)))

(defun generate-command-doc (s line)
  (ppcre:register-groups-bind (name) ("^!!! (.*)" line)
															(dprint name)
															(let ((cmd (symbol-function (find-symbol (string-upcase name) :dswm))))
																(format s "@deffn {Command} ~a ~{~a~^ ~}~%~a~&@end deffn~%~%"
																				name
																				#+sbcl (sb-introspect:function-lambda-list cmd)
																				#+clisp (ext:arglist cmd)
																				#+ccl (ccl:arglist cmd)
																				#+lispworks (lw:function-lambda-list cmd)
																				#- (or sbcl clisp ccl lispworks) '("(Check the code for args list)")
																				(documentation cmd 'function))
																t)))

(defun generate-manual (&key (in #p"dswm.texi.in") (out #p"dswm.texi"))
  (let ((*print-case* :downcase))
    (with-open-file (os out :direction :output :if-exists :supersede)
			(with-open-file (is in :direction :input)
				(loop for line = (read-line is nil is)
					 until (eq line is) do
						 (or (generate-function-doc os line)
								 (generate-macro-doc os line)
								 (generate-hook-doc os line)
								 (generate-variable-doc os line)
								 (generate-command-doc os line)
								 (write-line line os)))))))
