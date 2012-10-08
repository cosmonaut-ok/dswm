(defpackage :wmii-like-dswmrc-system
  (:use :cl :asdf))

(in-package :wmii-like-dswmrc-system)

(defsystem :wmii-like-dswmrc
  :name "wmii-like-dswmrc"
  :author "Julian Stecklina"
  :version "0.1"
  :maintainer "Alexander aka CosmonauT Vynnyk"
  :license "GNU General Public License v2 or later"
  :description "A mode line showing all groups in its first and all windows in the current group in the second line"
  :serial t
  :components ((:file "wmii-like-dswmrc")))
