;;; dswm-mode.el --- special lisp mode for evaluating code into running dswm

;; Copyright (C) 2007  Shawn Betts

;; Maintainer: Shawn Betts
;; Keywords: comm, lisp, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; load this file, set dswm-shell-program to point to dsish and
;; run M-x dswm-mode in your dswm lisp files. Now, you can
;; easily eval code into a running dswm using the regular bindings.

;;; Code:

(defvar dswm-shell-program "dsish"
  "program name, including path if needed, for the dsish program.")

(define-minor-mode dswm-mode
    "add some bindings to eval code into a running dswm using dsish."
  :global nil
  :lighter " DSWM"
  :keymap (let ((m (make-sparse-keymap)))
            (define-key m (kbd "C-M-x") 'dswm-eval-defun)
            (define-key m (kbd "C-x C-e") 'dswm-eval-last-sexp)
            m))

(defun dswm-eval-region (start end)
  (interactive "r")
  (let ((s (buffer-substring-no-properties start end)))
    (message "%s"
             (with-temp-buffer
               (call-process dswm-shell-program nil (current-buffer) nil
                             "eval"
                             s)
               (buffer-string)))))

(defun dswm-eval-defun ()
  (interactive)
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f")
    (let ((end (point)))
      (beginning-of-defun)
      (dswm-eval-region (point) end))))

(defun dswm-eval-last-sexp ()
  (interactive)
  (dswm-eval-region (save-excursion (backward-sexp) (point)) (point)))

(provide 'dswm-mode)
;;; dswm-mode.el ends here
