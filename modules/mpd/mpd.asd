(defpackage :mpd-system
  (:use :cl :asdf))

(in-package :mpd-system)

(defsystem :mpd
  :name "MPD"
  :author "Morgan Veyret"
  :version "0.1"
  :maintainer "Alexander aka CosmonauT Vynnyk"
  :license "GNU General Public License v2 or later"
  :description "MPD client description formatters for dswm"
  :serial t
  :components ((:file "mpd")))
