(defpackage :notifications-system
  (:use :cl :asdf))

(in-package :notifications-system)

(defsystem :notifications
  :name "Notifications"
  :author "Tassilo Horn <tassilo@member.fsf.org>"
  :version "0.1"
  :maintainer "Alexander aka CosmonauT Vynnyk"
  :license "GNU General Public License v3 or later"
  :description "notifications.lisp -- Poor man's systray for DSWM"
  :serial t
  :components ((:file "notifications")))
