(defpackage :s-bindings-system
  (:use :cl :asdf))

(in-package :s-bindings-system)

(defsystem :s-bindings
  :name "s-bindings"
  :author "Alexander aka 'CosmonauT' Vynnyk"
  :version "0.1"
  :maintainer "Alexander aka 'CosmonauT' Vynnyk"
  :license "GNU General Public License v2 or later"
  :description "External bindings, using 'Super' key for DSWM"
  :serial t
  :components ((:file "s-bindings")))
