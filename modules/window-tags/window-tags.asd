(defpackage :window-tags-system
  (:use :cl :asdf))

(in-package :window-tags-system)

(defsystem :window-tags
  :name "window-tags"
  :author "Michael Raskin"
  :version "0.1"
  :maintainer "Alexander aka CosmonauT Vynnyk"
  :license "GNU General Public License v2 or later"
  :description "Window tags are special window properties (stored in X11 window properties) that can be used for window manipulations. They can survive temporary WM change and allow more flexible classification of windows than selecting window groups for them."
  :serial t
  :components ((:file "window-tags")))
