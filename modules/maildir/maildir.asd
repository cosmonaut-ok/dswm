(defpackage :maildir-system
  (:use :cl :asdf))

(in-package :maildir-system)

(defsystem :maildir
  :name "Maildir"
  :author "Morgan Veyret"
  :version "0.1"
  :maintainer "Alexander aka CosmonauT Vynnyk"
  :license "GNU General Public License v2 or later"
  :description "Maildir monitoring for dswm's modeline"
  :serial t
  :components ((:file "maildir")))
