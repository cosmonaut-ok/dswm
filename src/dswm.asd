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
  :version "0.1"
  :maintainer "Alexander Vynnyk <cosmonaut.ok@gmail.com>"
  :license "GNU General Public License"
  :description "A tiling, keyboard driven window manager"
  :serial t
  :depends-on (:cl-ppcre #-(or cmu clisp ecl) :clx #+sbcl :sb-posix :xembed :alexandria) ;; :clx-truetype)
  :components ((:file "package")
							 
               (:file "definitions")
							 (:file "library")
							 ;; (:file "workarounds") ;; removed workarounds. Checking for sbcl bugs...
               (:file "wrappers")
							 ;; (:file "font-rendering")
               (:file "keysyms")
	       ;;(:file "keysyms-uni")
               (:file "kmap")
               (:file "input")
               (:file "helper")
               (:file "command")
               (:file "menu")
               (:file "screen")
               (:file "head")
               (:file "group")
               (:file "window")
               (:file "floating-group")
							 (:file "floating-window")
               (:file "tile-group")
               (:file "tile-window")
               (:file "scratchpad")
               (:file "window-placement")
               (:file "message-window")
               (:file "selection")
               (:file "iresize")
							 (:file "bindings")
               (:file "user")
               (:file "events")
               (:file "fdump")
							 (:file "rfr")
               (:file "time")
							 (:file "color")
							 (:file "systray")
               (:file "mode-line")
	       ;; (:file "experimental/mode-line-experimental")
               (:file "module")
               (:file "help")
							 
	       ;; (:file "experimental/ttf")
               (:file "dswm")
))
