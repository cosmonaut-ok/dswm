(defpackage :mem-system
  (:use :cl :asdf))

(in-package :mem-system)

(defsystem :mem
  :name "Mem"
  :author "Vitaly Mayatskikh"
  :version "0.1"
  :maintainer "Alexander aka CosmonauT Vynnyk"
  :license "GNU General Public License v2 or later"
  :description "MEM formatters for the mode-line"
  :serial t
  :components ((:file "mem")))
