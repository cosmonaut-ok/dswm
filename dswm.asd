;;; -*- Mode: Lisp -*-

(defpackage :dswm-system
  (:use :cl :asdf))

(in-package :dswm-system)

;; This is a hack for debian because it calls cmucl's clx
;; cmucl-clx. *very* annoying. I don't actually know if debian still
;; does this.
#+cmu (progn
          (ignore-errors (require :cmucl-clx))
          (ignore-errors (require :clx)))

(defsystem :dswm
  :name "DSWM"
  :author "Alexander Vynnyk <cosmonaut.ok@gmail.com>"
  :version "0.0.4-git"
  :maintainer "Alexander Vynnyk <cosmonaut.ok@gmail.com>"
  :license "GNU General Public License"
  :description "A tiling, keyboard driven window manager"
  :serial t
  :depends-on (:cl-ppcre #-(or cmu clisp) :clx #+sbcl :sb-posix)
  :components ((:file "package")
               (:file "defaults")
	       (:file "library")
               (:file "workarounds")
               (:file "wrappers")
               (:file "keysyms")
	       ;;(:file "keysyms-uni")
               (:file "kmap")
               (:file "input")
               (:file "core")
               (:file "command")
               (:file "menu")
               (:file "screen")
               (:file "head")
               (:file "group")
               (:file "window")
               (:file "floating-group")
               (:file "tile-group")
               (:file "tile-window")
               (:file "scratchpad")
               (:file "window-placement")
               (:file "message-window")
               (:file "selection")
	       ;; (:file "user")
               (:file "iresize")
	       (:file "bindings")
               (:file "user")
               (:file "events")
               (:file "fdump")
               (:file "time")
               (:file "mode-line")
               (:file "color")
               (:file "module")
               (:file "help")
               (:file "dswm")
))

