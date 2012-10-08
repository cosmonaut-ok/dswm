(defpackage :todo-system
  (:use :cl :asdf))

(in-package :todo-system)

(defsystem :todo
  :name "TODO"
  :author "Alexander aka CosmonauT Vynnyk <cosmonaut.ok@gmail.com>"
  :version "0.1"
  :maintainer "Alexander aka CosmonauT Vynnyk"
  :license "GNU General Public License v2 or later"
  :description "TODO module for DSWM"
  :serial t
  :components ((:file "todo")))
