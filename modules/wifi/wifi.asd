(defpackage :wifi-system
  (:use :cl :asdf))

(in-package :wifi-system)

(defsystem :wifi
  :name "WIFI"
  :author "John Li"
  :version "0.1"
  :maintainer "Alexander aka CosmonauT Vynnyk"
  :license "GNU General Public License v2 or later"
  :description "Wifi formatter for the mode-line"
  :serial t
  :components ((:file "wifi")))
