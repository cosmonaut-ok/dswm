;;; -*- Mode: LISP; Syntax: Common-lisp; Package: dswm -*-

;; Copyright 2010 Alexander aka 'CosmonauT' Vynnyk
;;
;; Author: Alexander aka 'CosmonauT' Vynnyk <cosmonaut.ok@gmail.com>
;; Version: id: emacs-keymap,v 0.1 12 Dec 2010 cosmonaut.ok@gmail.com
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
;;; Filename: emacs-keymap.lisp
;;; Module for alternate emacs-like keybindings
;;;==================================================================
;;
;;
;;

;;; Code:
(in-package :dswm)

(defvar *stump-key* (kbd "C-j")
  "The escape key for alternative emacs-like keybindings"
  )

(defvar *stump-fake-key* (kbd "j")
  "The binding that sends the fake escape key for alternate emacs
  keybindings to the current window.")

(defvar *stump-map* nil
  "This is the keymap by default bound to @kbd{C-t}. It is known as
  the @dfn{prefix map}.")

(fill-keymap *top-map*
             *stump-key* '*stump-map*)

(fill-keymap *stump-map*
  ;; Common operations
  (kbd "C-z")   "terminal"
  (kbd "e")   "emacs"
  (kbd "B")   "banish"
  ;;  (kbd "b")   "browser"
  (kbd "x")   "exec"
  (kbd "M-x") "colon"
  (kbd ":")   "eval"
  (kbd "'")   "select"
  (kbd "C-g") "abort"
  *emacs-fake-key* "send-escape"

  ;; Debugging operations
  (kbd "v")   "version"
  (kbd "i")   "info"

  ;; Operations with groups
  (kbd "g")   '*groups-map*
  (kbd "G")   "vgroups"
  (kbd "F1")  "gselect 1"
  (kbd "F2")  "gselect 2"
  (kbd "F3")  "gselect 3"
  (kbd "F4")  "gselect 4"
  (kbd "F5")  "gselect 5"
  (kbd "F6")  "gselect 6"
  (kbd "F7")  "gselect 7"
  (kbd "F8")  "gselect 8"
  (kbd "F9")  "gselect 9"
  (kbd "F10") "gselect 10"

  ;; Operations with frames
  (kbd "0")   "remove"
  (kbd "1")   "only"
  (kbd "2")   "vsplit"
  (kbd "3")   "hsplit"
  (kbd "+")       "balance-frames"
  (kbd "o")       "fnext"
  (kbd "TAB")     "fnext"
  (kbd "M-TAB")   "fother"
  (kbd "f")       "fselect"
  (kbd "F")       "curframe"

  ;; Operations with windows and focus
  (kbd "Up")      "move-focus up"
  (kbd "Down")    "move-focus down"
  (kbd "Left")    "move-focus left"
  (kbd "Right")   "move-focus right"
  (kbd "M-Up")    "move-window up"
  (kbd "M-Down")  "move-window down"
  (kbd "M-Left")  "move-window left"
  (kbd "M-Right") "move-window right"
  (kbd "m")       "move-window-to-frame"
  (kbd "C-m")     "move-window-to-frame"
  (kbd "C-u")     "next-urgent"
  (kbd "b")       "windowlist"
  (kbd "M-0")     "select-window-by-number 0"
  (kbd "M-1")     "select-window-by-number 1"
  (kbd "M-2")     "select-window-by-number 2"
  (kbd "M-3")     "select-window-by-number 3"
  (kbd "M-4")     "select-window-by-number 4"
  (kbd "M-5")     "select-window-by-number 5"
  (kbd "M-6")     "select-window-by-number 6"
  (kbd "M-7")     "select-window-by-number 7"
  (kbd "M-8")     "select-window-by-number 8"
  (kbd "M-9")     "select-window-by-number 9"
  (kbd "C-0")     "pull 0"
  (kbd "C-1")     "pull 1"
  (kbd "C-2")     "pull 2"
  (kbd "C-3")     "pull 3"
  (kbd "C-4")     "pull 4"
  (kbd "C-5")     "pull 5"
  (kbd "C-6")     "pull 6"
  (kbd "C-7")     "pull 7"
  (kbd "C-8")     "pull 8"
  (kbd "C-9")     "pull 9"
  (kbd "C-N")     "number"
  (kbd "#")       "mark"
  (kbd "F11")     "fullscreen"
  (kbd "A")       "title"

  ;;  (kbd "w")   "windows"
  ;;  (kbd "C-w") "windows"
  (kbd "k")   "delete"
  (kbd "K")   "kill"

  (kbd "n")       "pull-hidden-next"
  (kbd "C-n")     "pull-hidden-next"
  (kbd "M-n")     "next"
  (kbd "C-M-n")   "next-in-frame"
  (kbd "SPC")     "pull-hidden-next"
  (kbd "C-SPC")   "pull-hidden-next"
  (kbd "p")       "pull-hidden-previous"
  (kbd "C-p")     "pull-hidden-previous"
  (kbd "M-p")     "prev"
  (kbd "C-M-p")   "prev-in-frame"
  (kbd "W")       "place-existing-windows"
  *escape-key*    "pull-hidden-other"
  (kbd "M-t")     "other-in-frame"
  (kbd "r")       "iresize"
  (kbd "-")       "fclear"

  (kbd "l")       "redisplay"
  (kbd "C-l")     "redisplay"

  (kbd "h")   '*help-map*)

(defkeys-top
    ("C-s-Up" "resize 0 -10")
    ("C-s-Down" "resize 0 10")
    ("C-s-Left" "resize 10 0")
    ("C-s-Right" "resize -10 0"))

;;; emacs-keymap.lisp ends here

