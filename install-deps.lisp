;;; -*- Mode: LISP; Syntax: Common-lisp; Package: dswm -*-

;; Copyright 2010 Alexander aka 'CosmonauT' Vynnyk
;;
;; Author: Alexander aka 'CosmonauT' Vynnyk <cosmonaut.ok@gmail.com>
;; Version: id: install-deps,v 0.0 27 жов 2010 cosmonaut.ok@gmail.com
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;;==================================================================
;;; Filename: install-deps.lisp
;;; File for installing dependences: cl-clx and cl-ppcre
;;;==================================================================
;;
;;
;;

;;; Code:

(require 'asdf)

(defun test-dep (arg)
  (asdf:find-system arg nil))


;; SBCL
#+sbcl
(defun install-dep-sbcl (arg)
(if (not (test-dep arg))
    (progn
      (require 'asdf-install)
      (asdf-install:install dep)))
#+sbcl
(progn
  (install-dep-sbcl :clx)
  (install-dep-sbcl :cl-ppcre))

#+clisp
(defun download-dep-clisp (dep)
  (cond ((equal dep "clx")
         (ext:shell "wget http://cclan.cvs.sourceforge.net/*checkout*/cclan/asdf/asdf.lisp -O ~/.cl/asdf/asdf.lisp")
         )))

#+clisp
(defun install-dep-clisp (arg)
  (if (not (test-dep arg))
      (progn
        "ololo")))
#+clisp
(progn
  (install-dep-sbcl :clx)
  (install-dep-sbcl :cl-ppcre))

;;; install-deps.lisp ends here

  $ mkdir -p ~/.cl/asdf && cd ~/.cl
  $ wget http://cclan.cvs.sourceforge.net/*checkout*/cclan/asdf/asdf.lisp -O ~/.cl/asdf/asdf.lisp
  $ echo "(load #p\"/home/USER/.cl/asdf/asdf\")" >> ~/.clisprc.lisp
  $ mkdir -p ~/.cl/systems
  $ echo "(push #p\"/home/USER/.cl/systems\" asdf:*central-registry*)" >> ~/.clisprc.lisp
  $ wget http://common-lisp.net/project/asdf-install/asdf-install_latest.tar.gz
  $ tar xf asdf-install_latest.tar.gz
  $ ln -s ~/.cl/asdf-install/asdf-install/asdf-install.asd ~/.cl/systems/
  $ clisp
    * (asdf:operate 'asdf:compile-op 'asdf-install)
    * (asdf:operate 'asdf:load-op 'asdf-install)
    * (asdf-install:install :cl-ppcre)