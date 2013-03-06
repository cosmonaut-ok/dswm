;; Copyright (C) 2013 Alexander aka CosmonauT Vynnyk
;;
;;  This file is part of dswm.
;;
;; dswm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; dswm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; COMMENTS
;; HERE
;;
;; Code:

(in-package :dswm)



(define-dswm-type :rfr (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt (rfr-elements) :require-match t)))


(defun rfr-elements ()
  (list "frame" "group" "screen" "window" "current-frame" "current-group" "current-screen" "current-window" "desktop" "help"))

(defcommand remember (what) ((:rfr "What do you want to remember (type `help` for help)? "))
  (cond ((equal what "frame")
	 )
	((equal what "group")
	 )
	((equal what "screen")
	 )
	((equal what "window")
	 )
	((equal what "current-frame")
	 )
	((equal what "current-group")
	 )
	((equal what "current-screen")
	 )
	((equal what "current-window")
	 )
	((equal what "desktop")
	 )
	((equal what "help")
	 )))