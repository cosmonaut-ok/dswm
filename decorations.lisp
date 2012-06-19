;;; -*- Mode: LISP; Syntax: Common-lisp; Package: dswm -*-

;; Copyright 2010 Alexander aka 'CosmonauT' Vynnyk
;;
;; Author: Alexander aka 'CosmonauT' Vynnyk <cosmonaut.ok@gmail.com>
;; Version: id: decorations,v 0.1 02 Dec 2010 cosmonaut.ok@gmail.com
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
;;; Filename: decorations.lisp
;;; Theming for DSWM
;;;==================================================================
;;
;;
;;

;;; Code:
(in-package #:dswm)

;; ;;        (run-shell-command (conat "xsetroot -solid " bg-color) t)
;; ;;        (run-shell-command (format nil "xsetroot -cursor_name left_ptr -fg \"~a\"" fg-color bg-color))
;; ;;        (run-with-timer 5 5 'banish-pointer)

;; (defvar *normal-border-width* 3)

;; (setf *message-window-gravity* :bottom-right)

;; (setf *format-time-string-default* "%a %b %e %k: %M")


;; (setf *mode-line-pad-x* 2)
;; (setf *mode-line-pad-y* 2)

;; (setf stumpwm:*screen-mode-line-format*
;;  (list "%w | "
;;   '(:eval (run-shell-command "date" t))))

;; ;; keep the X cursor out of the way.
;; ;(run-with-timer 5 5 'banish-pointer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun color-theme-install (main-items)
  (let ((theme-name (car main-items))
        (theme-main-params (cadr main-items))
        (theme-other-params (cddr main-items)))

    (progn
      ;; Set current theme name
      (setf *current-color-theme* theme-name)
      ;; Set main theme parameters
      (if (not (null theme-main-params))
          (dolist (i *main-theme-parameters-list*)
            (let ((a (assoc (car i) theme-main-params)))
              (if (not (null a))
                  (eval `(,(cdr i) ',(cdr a)))))))

      )))

(defmacro call-modeline (list)
  "For emacs compatible"
  `(apply 'set-mode-line ',(cons (caar list) (cadar list)))))

(defun set-mode-line (mode-line-switch &key foreground background position timeout border-width)
  (progn
    (if (null (car mode-line-switch))
        (enable-mode-line (current-screen) (current-head) nil)
        (enable-mode-line (current-screen) (current-head) t))
    (dolist (i '(foreground background position timeout border-width)
             (if (not (null i))

    ))