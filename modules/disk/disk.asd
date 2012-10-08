(defpackage :disk-system
  (:use :cl :asdf))

(in-package :disk-system)

(defsystem :disk
  :name "Disk"
  :author "Morgan Veyret"
  :version "0.1"
  :maintainer "Alexander aka CosmonauT Vynnyk"
  :license "GNU General Public License v2 or later"
  :description "Disk usage monitoring for dswm's modeline"
  :serial t
  :components ((:file "disk")))
