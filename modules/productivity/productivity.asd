(defpackage :productivity-system
  (:use :cl :asdf))

(in-package :productivity-system)

(defsystem :productivity
  :name "Productivity"
  :author "Ivy Foster"
  :version "0.1"
  :maintainer "Alexander aka CosmonauT Vynnyk"
  :license "GNU General Public License v2 or later"
  :description ""
  :serial t
  :components ((:file "productivity")))
