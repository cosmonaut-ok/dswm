;; Copyright (C) 2003-2008 Shawn Betts
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
;; Head functionality
;;
;; Code:

(in-package #:dswm)

(export '(current-head))

(defstruct (head (:include frame))
  ;; point back to the screen this head belongs to
  screen
  ;; a bar along the top or bottom that displays anything you want.
  mode-line)

(defun head-by-number (screen n)
  (find n (screen-heads screen) :key 'head-number))

(defun parse-xinerama-head (line)
  (ppcre:register-groups-bind (('parse-integer number width height x y))
                              ("^ +head #([0-9]+): ([0-9]+)x([0-9]+) @ ([0-9]+),([0-9]+)" line :sharedp t)
                              (handler-case
                                  (make-head :number number
                                             :x x :y y
                                             :width width
                                             :height height)
                                (parse-error ()
                                  nil))))

(defun make-screen-heads (screen root)
  "or use xdpyinfo to query the xinerama extension, if it's enabled."
  (or (and (xlib:query-extension *display* "XINERAMA")
           (with-current-screen screen
             ;; Ignore 'clone' heads.
             (loop
                for i = 0 then (1+ i)
                for h in
                (delete-duplicates
                 (loop for i in (split-string (run-shell-command "xdpyinfo -ext XINERAMA" t)) ;; TODO: need to remove 'run-shell-command'
                    for head = (parse-xinerama-head i)
                    when head
                    collect head)
                 :test #'frames-overlap-p)
                do (setf (head-number h) i)
                collect h)))
      (list (make-head :number 0
                       :x 0 :y 0
                       :width (xlib:drawable-width root)
                       :height (xlib:drawable-height root)
                       :window nil))))

(defun copy-heads (screen)
  "Return a copy of screen's heads."
  (mapcar 'copy-frame (screen-heads screen)))


;; Determining a frame's head based on position probably won't
;; work with overlapping heads. Would it be better to walk
;; up the frame tree?
(defun frame-head (group frame)
  (let ((center-x (+ (frame-x frame) (ash (frame-width frame) -1)))
       (center-y (+ (frame-y frame) (ash (frame-height frame) -1))))
    (dolist (head (screen-heads (group-screen group)))
      (when (and
            (>= center-x (frame-x head))
            (>= center-y (frame-y head))
            (<= center-x
                (+ (frame-x head) (frame-width head)))
            (<= center-y
                (+ (frame-y head) (frame-height head))))
       (return head)))))

(defun group-heads (group)
  (screen-heads (group-screen group)))

(defun resize-head (number x y width height)
  "Resize head number `number' to given dimension."
  (let* ((screen (current-screen))
         (oh (find number (screen-heads screen) :key 'head-number))
         (nh (make-head :number number
                        :x x :y y
                        :width width
                        :height height
                        :window nil)))
    (scale-head screen oh nh)
    (dolist (group (screen-groups screen)) (group-sync-head group oh))
    (update-mode-lines screen)))

(defun current-head (&optional (group (current-group)))
  (group-current-head group))

(defun head-windows (group head)
  "Returns a list of windows on HEAD of GROUP"
  (remove-if-not
   (lambda (w)
     (eq head (window-head w)))
   (group-windows group)))

(defun frame-is-head (group frame)
  (< (frame-number frame) (length (group-heads group))))

(defun add-head (screen head)
  (dformat 1 "Adding head #~D~%" (head-number head))
  (setf (screen-heads screen) (sort (push head (screen-heads screen)) #'< :key 'head-number))
  (dolist (group (screen-groups screen))
    (group-add-head group head)))

(defun remove-head (screen head)
  (dformat 1 "Removing head #~D~%" (head-number head))
  (when (head-mode-line head)
    (toggle-mode-line screen head))
  (dolist (group (screen-groups screen))
    (group-remove-head group head))
  ;; Remove it from SCREEN's head list.
  (setf (screen-heads screen) (delete head (screen-heads screen))))

(defun scale-head (screen oh nh)
  "Scales head OH to match the dimensions of NH."
  (dolist (group (screen-groups screen))
    (group-resize-head group oh nh))
  (setf (head-x oh) (head-x nh)
        (head-y oh) (head-y nh)
        (head-width oh) (head-width nh)
        (head-height oh) (head-height nh)))

(defun scale-screen (screen heads)
  "Scale all frames of all groups of SCREEN to match the dimensions
  of HEADS."
  (when (< (length heads) (length (screen-heads screen)))
    ;; Some heads were removed (or cloned), try to guess which.
    (dolist (oh (screen-heads screen))
			(when (= (head-number oh) (head-number nh))
				;; Same frame number, probably the same head
				(setf (head-number nh) (head-number oh)))))
	;; Actually remove the missing heads.
	(dolist (head (set-difference (screen-heads screen) heads :key 'head-number))
		(remove-head screen head))
  (loop
   for nh in heads
   as oh = (find (head-number nh) (screen-heads screen) :key 'head-number)
   do (if oh
          (scale-head screen oh nh)
          (add-head screen nh))))
