;;; -*- Mode: LISP; Syntax: Common-lisp; Package: dswm.web -*-

;; Copyright 2011 Alexander aka 'CosmonauT' Vynnyk
;;
;; Author: Alexander aka CosmonauT Vynnyk <cosmonaut.ok@gmail.com>
;; Version: id: web,v 0.1 05 Feb 2011 cosmonaut.ok@gmail.com
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
;;; Filename: hyperbookmark.lisp
;;; Hyperbookmarks for dswm
;;;==================================================================
;;

;; Hyperbookmarks

(defvar *hyperbookmarks-list* nil
  "Defines list of all hyperbookmarks, saved by user")

(defvar *hyperbookmarks-browsers-list* '("firefox" "chromium" "opera"
  "dillo" "conkeror" "swiftfox" "iceape" "iceweacel" "seamonkey"
  "galeon" "arora" "epiphany" "konqueror" "rekonq" "midori" "omniweb"
  "owb" "rockmelt" "Safari" "shiira" "uzbl")
  "Defines list of avaliable webbrowsers")

(defvar *hyperbookmarks-file* (data-dir-file "hyperbookmarks" "list" "save.d")
  "Defines default hyperbookmark file")

(defstruct hyperbookmark
  "Hyperbookmark structure"
  name browser url)

(define-dswm-type :hyperbookmark (input prompt)
  (or (argument-pop input)
      (completing-read (current-screen)
                       prompt
                       (get-hyperbookmark-names-list))))

(define-dswm-type :hb-browser (input prompt)
  (or (argument-pop input)
      (completing-read (current-screen)
                       prompt
                       (get-browsers-list))))

(define-dswm-type :hb-url (input url)
  (or (argument-pop input)
      (read-one-line (current-screen) url :initial-input (get-x-selection))))
                     ;; (if (not
                     ;;      (cl-ppcre:scan-to-strings "^http\:\/\/|^https\:\/\/" url)
                     ;;      (concat "http://" prompt)
                     ;;      prompt)))))

(define-dswm-type :string-with-clipboard (input prompt)
  (or (argument-pop input)
      (read-one-line (current-screen) prompt :initial-input (get-x-selection))))

(defun get-hyperbookmark-names-list (&optional (list *hyperbookmarks-list*))
  (cond
    ((null (car list)) nil)
    (t (cons (hyperbookmark-name (car list))
             (get-hyperbookmark-names-list (cdr list))))))

(defun get-browsers-list ()
  (let ((path (cl-ppcre:split ":" (dswm::getenv "PATH")))
        (browsers-list))
    (dolist (i *hyperbookmarks-browsers-list*)
      (dolist (j path)
        (if (probe-file (dswm::concat j "/" i))
            (setf
             browsers-list
             (dswm::add-to-list
              browsers-list
              (dswm::concat j "/" i))))))
    browsers-list))

(defun find-hyperbookmark (&key name browser url (list *hyperbookmarks-list*))
  (cond ((null list) nil)
        ((or
          (and (not (null name))
               (equal name (hyperbookmark-name (car list))))
          (and (not (null browser))
               (equal browser (hyperbookmark-browser (car list))))
          (and (not (null url))
               (equal url (hyperbookmark-url (car list)))))
         (car list))
        (t
         (find-hyperbookmark
          :name name
          :browser browser
          :url url
          :list (cdr list)))))

(defmacro find-hyperbookmark-by-name (name)
  `(find-hyperbookmark :name ,name))

(defmacro find-hyperbookmark-by-browser (browser)
  `(find-hyperbookmark :browser ,browser))

(defmacro find-hyperbookmark-by-url (url)
  `(find-hyperbookmark :url ,url))

(defun dump-hyperbookmarks (&optional (hbdump *hyperbookmarks-list*))
  (dswm::dump-to-file hbdump *hyperbookmarks-file*))

(defcommand hb-add (name browser url open-p)
  ((:string "Enter hyperbookmark name: ")
   (:hb-browser "With what browser open? ")
   (:hb-url "Enter URL to go to ")
   (:y-or-n "Open right now? "))
  (if
   (null (find-hyperbookmark-by-name name))
   (progn
     (dswm::add-to-list *hyperbookmarks-list*
                        (make-hyperbookmark
                         :name name
                         :browser browser
                         :url url))
     (dump-hyperbookmarks)
     (if (not (null open-p))
         (hb-open name)))
   (message "hyperbookmark '~a' already exists" name)))

(defcommand hb-remove (name) ((:hyperbookmark "Enter hyperbookmark
  name to remove: "))
  (dswm::remove-from-list
   *hyperbookmarks-list*
   (find-hyperbookmark-by-name name))
  (dump-hyperbookmarks))

(defcommand hb-open (name)
  ((:hyperbookmark "Enter hyperbookmark name: "))
  (let ((bookmark (find-hyperbookmark-by-name name)))
    (run-shell-command
     (dswm::concat (hyperbookmark-browser bookmark)
             " "
             (hyperbookmark-url bookmark)))))

(defcommand hb-list () ()
            (let ((list "Name~20tbrowser~50turl~%
-------------------------------------------------------------------------~%"))
              (dolist (i *hyperbookmarks-list*)
                (setf list (concatenate 'string list
                                        (hyperbookmark-name i) "~20t"
                                        (hyperbookmark-browser i) "~50t"
                                        (hyperbookmark-url i) "~%")))
               (message list)))

(defcommand hb-reload () ()
  (if (probe-file *hyperbookmarks-file*)
      (progn
        (setf *hyperbookmarks-list*
              (dswm::read-dump-from-file *hyperbookmarks-file*))
        (message "Loaded"))
        (message "No hyperbookmarks to load")))


;; Initialization
(hb-reload)

(define-keys *root-map*
  ((kbd "u") "hb-open")
  ((kbd "M-s") "web-search")
  ((kbd "M-w") "wiki-search")
  ((kbd "M-d") "dict-search")
  ((kbd "M-j") "web-jump")
  ((kbd "M-g") "g-translate"))

;;; hyperbookmarks.lisp ends here
