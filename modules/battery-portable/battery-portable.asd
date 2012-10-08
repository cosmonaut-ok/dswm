(defpackage :battery-portable-system
  (:use :cl :asdf))

(in-package :battery-portable-system)

(defsystem :battery-portable
  :name "Battery portable"
  :author "Julian Stecklina"
  :version "0.1"
  :maintainer "Alexander aka 'CosmonauT' Vynnyk"
  :license "GNU General Public License v2 or later"
  :description "Portable battery information for DSWM's mode-line."
  :serial t
  :components ((:file "battery-portable")))
