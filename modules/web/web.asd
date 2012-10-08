(defpackage :web-system
  (:use :cl :asdf))

(in-package :web-system)

(defsystem :web
  :name "WEB"
  :author "Alexander aka CosmonauT Vynnyk <cosmonaut.ok@gmail.com>"
  :version "0.1"
  :maintainer "Alexander aka CosmonauT Vynnyk"
  :license "GNU General Public License v2 or later"
  :description "Web extension for DSWM"
  :serial t
  :components ((:file "web")
	       (:file "hyperbookmarks")))
