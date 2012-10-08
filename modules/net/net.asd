(defpackage :net-system
  (:use :cl :asdf))

(in-package :net-system)

(defsystem :net
  :name "Net"
  :author "Vitaly Mayatskikh"
  :version "0.1"
  :maintainer "Alexander aka CosmonauT Vynnyk"
  :license "GNU General Public License v2 or later"
  :description "Network activity formatter for the mode-line"
  :serial t
  :components ((:file "net")))
