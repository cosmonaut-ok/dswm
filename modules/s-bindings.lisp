;;; -*- Mode: LISP; Syntax: Common-lisp; Package: dswm -*-

;; Copyright 2011 Alexander aka 'CosmonauT' Vynnyk
;;
;; Author: Alexander aka 'CosmonauT' Vynnyk <cosmonaut.ok@gmail.com>
;; Version: id: s-bindings,v 0.1 22 Feb 2011 cosmonaut.ok@gmail.com
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;;==================================================================
;;; Filename: s-bindings.lisp
;;; Super- extrabindings for dswm (experimental)
;;;==================================================================
;;
;;
;;

;;; Code:
(in-package :dswm)

(defkeys-top
  ;; experimental addon s-based keybindings
  ("s-x"  "colon")                   ; experimental feature
  ("s-!"  "exec")
  ("s-b"  "browser")
  ("s-c"   "terminal")
  ("s-e"   "emacs")
  ("s-F1"  "gselect 1")                 ;+-
  ("s-F2"  "gselect 2")                 ;+-
  ("s-F3"  "gselect 3")                 ;+-
  ("s-F4"  "gselect 4")                 ;+-
  ("s-F5"  "gselect 5")                 ;+-
  ("s-F6"  "gselect 6")                 ;+-
  ("s-F7"  "gselect 7")                 ;+-
  ("s-F8"  "gselect 8")                 ;+-
  ("s-F9"  "gselect 9")                 ;+-
  ("s-F10" "gselect 10")                ;+-
  ("s-1"  "select-window-by-number 1")  ;+-
  ("s-2"  "select-window-by-number 2")  ;+-
  ("s-3"  "select-window-by-number 3")  ;+-
  ("s-4"  "select-window-by-number 4")  ;+-
  ("s-5"  "select-window-by-number 5")  ;+-
  ("s-6"  "select-window-by-number 6")  ;+-
  ("s-7"  "select-window-by-number 7")  ;+-
  ("s-8"  "select-window-by-number 8")  ;+-
  ("s-9"  "select-window-by-number 9")  ;+-
  ("s-0"  "select-window-by-number 0")  ;+-

  ("C-s-0"     "pull 0")
  ("C-s-1"     "pull 1")
  ("C-s-2"     "pull 2")
  ("C-s-3"     "pull 3")
  ("C-s-4"     "pull 4")
  ("C-s-5"     "pull 5")
  ("C-s-6"     "pull 6")
  ("C-s-7"     "pull 7")
  ("C-s-8"     "pull 8")
  ("C-s-9"     "pull 9")

  ("s-f" "fselect")
  ("s-m" "move-window-to-frame")
  ("s-\"" "windowlist")
  ("s-#" "mark")
 ;; Experimental emacs-like frame resizing
  ("C-s-Up" "resize 0 -10")
  ("C-s-Down" "resize 0 10")
  ("C-s-Left" "resize -10 0")
  ("C-s-Right" "resize 10 0")

  ("s-j" "pull-hidden-other")
  ("s-s" "vsplit")
  ("s-S" "hsplit")
  ("s-Q" "only")
  ("s-R" "remove")
  ("s-g" *groups-map*)

  ("s-Up"      "move-focus up")
  ("s-Down"    "move-focus down")
  ("s-Left"    "move-focus left")
  ("s-Right"   "move-focus right")
  ("s-M-Up"    "move-window up")
  ("s-M-Down"  "move-window down")
  ("s-M-Left"  "move-window left")
  ("s-M-Right" "move-window right")
 )


;;; s-bindings.lisp ends here

