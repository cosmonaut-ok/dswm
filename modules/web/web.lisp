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
;;; Filename: web.lisp
;;; Web extension for dswm
;;;==================================================================
;;
;; TODO: replace this codemonkey code (aka бидлокод) to nice and beautiful ;)
;;
;;; Code:
(defpackage :dswm.modules.web
  (:use :common-lisp :dswm :cl-ppcre))

(in-package :dswm.modules.web)

;;
;; Websearch
;;
(defvar *default-geolocation* "ua"
  "Your geolocation for search engines, wiki languages etc
avaliable locations:
us -
ua -
ru -
inn - for international
"
  )

(defvar *default-wiki-engine* "wikipedia"
  "Default wiki engine" )

(defvar *default-search-engine* "google"
  "Default search engine")

(defvar *default-dict-engine* "reference"
  "Default dict engine")

(defvar *google-translate-languages* '("en" "uk")
  "FIXME: Temporary solution")

(defvar *engines-list*
  ;; Engine types: search, dict, wiki
  ;;  Search format: (location prefix suffix)
  ;;  Translate format (location preffix from-separator to-separator suffix)
  ;;  Wiki format (location prefix suffix)
  '((google search
     (ua "http://www.google.com.ua/search?hl=uk&q=" "")
     (ru "http://www.google.ru/search?hl=ru&q=" "")
     (nl "http://www.google.nl/search?hl=nl&q=" "")
     (inn "http://www.google.com/search?hl=en&q=" ""))
    (google translate
     (inn "http://translate.google.com/#" "|" "|" ""))
    (slang dict
     (inn "http://www.urbandictionary.com/define.php?term=" ""))
    (reference dict
     (inn "http://dictionary.reference.com/browse/" ""))
    (wikipedia wiki
     (inn "http://en.wikipedia.org/wiki/" "")
     (ua "http://uk.wikipedia.org/wiki/" "")
     (nl "http://nl.wikipedia.org/w/index.php?search=" "")
     (ru "http://ru.wikipedia.org/wiki/" ""))
    (citizendium wiki
     (inn "http://en.citizendium.org/wiki/Special:Search?search=" "&fulltext=Search"))
    (startrek wiki
      (inn "http://memory-alpha.org/index.php?title=Special%3ASearch&redirs=1&search=" "&fulltext=Search&ns0=1&ns1=1&ns2=1&ns3=1&ns4=1&ns5=1&ns6=1&ns7=1&ns8=1&ns9=1&ns10=1&ns11=1&ns12=1&ns13=1&ns14=1&ns15=1&ns102=1&ns103=1&ns110=1&ns111=1&redirs=1&title=Special%3ASearch&advanced=1&fulltext=Advanced+search"))
    (physics wiki
     (inn "http://www.physics.thetangentbundle.net/wiki/Special:Search?ns0=1&ns4=1&search=" "&searchx=Search"))
    (mathematics wiki
     (inn "http://www.mathematics.thetangentbundle.net/wiki/Special:Search?ns0=1&ns4=1&search=" "&searchx=Search"))
    (progpedia wiki
     (ru "http://progopedia.ru/search/?cx=partner-pub-6767755207614565%3Ah93dnl-b2so&cof=FORID%3A10&ie=UTF-8&q="
         "&sa=%D0%9F%D0%BE%D0%B8%D1%81%D0%BA&siteurl=progopedia.ru%2F#974")
     (inn "http://progopedia.com/search/?cx=partner-pub-6767755207614565%3Ap1su7p-a811&cof=FORID%3A10&ie=UTF-8&q="
          "&sa=Search&siteurl=progopedia.com%2F#1002"))
    (bing search
     (inn "http://www.bing.com/search?q=" ""))
    (lisp search
     (inn "http://l1sp.org/search?q=" ""))
    (ask-com search
     (inn "http://www.ask.com/web?q=test" "")
     (ru "http://ru.ask.com/web?q=" "&qsrc=0&o=0&l=dir"))
    (yandex search
     (inn "http://yandex.com/yandsearch?text=" "")
     (ua "http://yandex.ua/yandsearch?text=" "")
     (ru "http://yandex.ru/yandsearch?text=" ""))
    (yahoo search
     (inn "http://search.yahoo.com/search?p=" ""))
    (conkeror wiki
     (inn "http://conkeror.org/?action=fullsearch&context=60&value=" "&fullsearch=Text"))
    (stumpwm wiki
     (inn "http://stumpwm.antidesktop.net/wiki?search=" ""))
    (dswm wiki
     (inn "http://dss-de.sourceforge.net/doku.php/?do=search&id=dswm " ""))
    (duckduckgo search
     (inn "http://duckduckgo.com/?q=" ""))
    (emacs wiki
     (inn "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&q=" "&sa=Search&siteurl=www.emacswiki.org" ""))))

(define-dswm-type :search (input prompt)
  (or (argument-pop input)
      (completing-read (current-screen)
                       prompt
                       (get-engines-list 'search *engines-list*))))

(define-dswm-type :wiki (input prompt)
  (or (argument-pop input)
      (completing-read (current-screen)
                       prompt
                       (get-engines-list 'wiki *engines-list*))))

(define-dswm-type :dict (input prompt)
  (or (argument-pop input)
      (completing-read (current-screen)
                       prompt
                       (get-engines-list 'dict *engines-list*))))

(defun find-by-car (name body)
  (cond
    ((null body) nil)
    ((eq name (caar body))
     (cons (car body) (find-by-car name (cdr body))))
    (t (find-by-car name (cdr body)))))

(defun find-by-cdr (name body)
  (cond
    ((null body) nil)
    ((eq name (cadar body))
     (cons (car body) (find-by-cdr name (cdr body))))
    (t (find-by-cdr name (cdr body)))))

(defun get-engines-list (name body)
  (cond
    ((null body) nil)
    ((eq name (cadar body))
     (cons (string-downcase (write-to-string (caar body))) (get-engines-list name (cdr body))))
    (t (get-engines-list name (cdr body)))))

(defun find-engine-parameters (name type location)
  (if (null
       (find-by-car location
                    (cddar (find-by-cdr type (find-by-car
                                               name *engines-list*)))))
      (if (null
           (find-by-car 'inn (cddar (find-by-cdr type (find-by-car name *engines-list*)))))
          (message "NIL")
;;        FIXME:   "No such ~a engine with name ~a. Check value of
;;                variable *default-~a-engine* and your location variable
;;                (*default-geolocation*)" type name type)
        (cdar
           (find-by-car 'inn
                        (cddar (find-by-cdr type
                                            (find-by-car name *engines-list*))))))
      (cdar
       (find-by-car location
                    (cddar (find-by-cdr type (find-by-car
                                              name *engines-list*)))))))

(defun get-current-location ()
  (if (not (null *default-geolocation*))
      *default-geolocation*
    "inn"))

(defun run-engine-with-key (engine type)
  (run-shell-command
   (concatenate 'string
    *browser* " \""
    (if (not (null (car engine)))
        (car engine))
    type
    (if (not (null (cadr engine)))
        (cadr engine))
    "\""
    )))

 (defcommand web-search-with-engine (engine search)
  ((:search "Search with what? ") (:string-with-clipboard "Search for what? "))
  (run-engine-with-key
   (find-engine-parameters (intern (string-upcase engine)) 'search
                           (get-current-location)) search))

(defcommand web-search () ()
  "Web search with current default search engine"
  (run-commands
   (dswm::concat "web-search-with-engine " *default-search-engine*)))

 (defcommand wiki-search-with-engine (engine search)
  ((:wiki "Search with what? ") (:string-with-clipboard "Search for what? "))
  (run-engine-with-key
   (find-engine-parameters (intern (string-upcase engine)) 'wiki
                           (get-current-location)) search))

(defcommand wiki-search () ()
  "Web search with current default search engine"
  (run-commands
   (dswm::concat "wiki-search-with-engine " *default-wiki-engine*)))

(defcommand dict-search-with-engine (engine search)
  ((:dict "Search with what? ") (:string-with-clipboard "Search for what? "))
  (run-engine-with-key
   (find-engine-parameters (intern (string-upcase engine)) 'dict
                           (get-current-location)) search))

(defcommand dict-search () ()
  "Web search with current default search engine"
  (run-commands
   (dswm::concat "dict-search-with-engine " *default-dict-engine*)))

(defcommand g-translate (what) ((:string-with-clipboard "What do you want to translate? "))
  "It`s just temporary, whether we don`t make good translate engine"
  (run-shell-command
   (dswm::concat *browser* " \"http://translate.google.com/#"
		 (car *google-translate-languages*) "|"
		 (cadr *google-translate-languages*) "|" what "\"")))

(defcommand web-jump (url) ((:string "Enter URL: "))
  "Open URL in default web-browser"
  (run-shell-command (concatenate 'string *browser* " " url)))

;;; web.lisp ends here

