;; Copyright (C) 2006-2008 Matthew Kennedy
;; Copyright (C) 2010-2012 Alexander aka CosmonauT Vynnyk
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
;; Translate between dswm key names and keysym names.
;;
;; Code:

(in-package #:dswm)

(defvar *dswm-name->keysym-name-translations* (make-hash-table :test #'equal)
  "Hashtable mapping from dswm key names to keysym names.")

(defun define-keysym-name (dswm-name keysym-name)
  "Define a mapping from a DSWM-NAME to KEYSYM-NAME.
This function is used to translate Emacs-like names to keysym
names."
  (setf (gethash dswm-name *dswm-name->keysym-name-translations*)
        keysym-name))

(defun dswm-name->keysym-name (dswm-name)
  (multiple-value-bind (value present-p)
      (gethash dswm-name *dswm-name->keysym-name-translations*)
    (declare (ignore present-p))
    value))

(defun keysym-name->dswm-name (keysym-name)
  (maphash (lambda (k v)
             (when (equal v keysym-name)
               (return-from keysym-name->dswm-name k)))
           *dswm-name->keysym-name-translations*))

(defun dswm-name->keysym (dswm-name)
  "Return the keysym corresponding to DSWM-NAME.
If no mapping for DSWM-NAME exists, then fallback by calling
KEYSYM-NAME->KEYSYM."
  (let ((keysym-name (dswm-name->keysym-name dswm-name)))
    (keysym-name->keysym (or keysym-name dswm-name))))

(defun keysym->dswm-name (keysym)
  "Return the dswm key name corresponding to KEYSYM.
If no mapping for the dswm key name exists, then fall back by
calling KEYSYM->KEYSYM-NAME."
  (let ((keysym-name (keysym->keysym-name keysym)))
    (or (keysym-name->dswm-name keysym-name)
        keysym-name)))

(define-keysym-name "RET" "Return")
(define-keysym-name "ESC" "Escape")
(define-keysym-name "TAB" "Tab")
(define-keysym-name "DEL" "BackSpace")
(define-keysym-name "SPC" "space")
(define-keysym-name "!" "exclam")
(define-keysym-name "\"" "quotedbl")
(define-keysym-name "$" "dollar")
(define-keysym-name "%" "percent")
(define-keysym-name "&" "ampersand")
(define-keysym-name "'" "quoteright")   ;deprecated
(define-keysym-name "'" "apostrophe")
(define-keysym-name "`" "quoteleft")    ;deprecated
(define-keysym-name "`" "grave")
(define-keysym-name "&" "ampersand")
(define-keysym-name "(" "parenleft")
(define-keysym-name ")" "parenright")
(define-keysym-name "*" "asterisk")
(define-keysym-name "+" "plus")
(define-keysym-name "," "comma")
(define-keysym-name "-" "minus")
(define-keysym-name "." "period")
(define-keysym-name "/" "slash")
(define-keysym-name ":" "colon")
(define-keysym-name ";" "semicolon")
(define-keysym-name "<" "less")
(define-keysym-name "=" "equal")
(define-keysym-name ">" "greater")
(define-keysym-name "?" "question")
(define-keysym-name "@" "at")
(define-keysym-name "[" "bracketleft")
(define-keysym-name "\\" "backslash")
(define-keysym-name "]" "bracketright")
(define-keysym-name "^" "asciicircum")
(define-keysym-name "_" "underscore")
(define-keysym-name "#" "numbersign")
(define-keysym-name "{" "braceleft")
(define-keysym-name "|" "bar")
(define-keysym-name "}" "braceright")
(define-keysym-name "~" "asciitilde")
