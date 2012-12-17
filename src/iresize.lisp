;; Copyright (C) 2003-2008 Shawn Betts
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
;;; A resize minor mode. Something a bit better should probably be
;;; written. But it's an interesting way of doing it.
;;
;; Code:

(in-package #:dswm)

(export '(*resize-increment*
          iresize
          abort-iresize
          exit-iresize))

(defvar *resize-backup* nil)

(defvar *resize-increment* 10
  "Number of pixels to increment by when interactively resizing frames.")

(defun set-resize-increment (val)
  (setf *resize-increment* val)
  (update-resize-map))

(defun update-resize-map (fx fy fh fw dh dw)
  (let ((m (or *resize-map* (setf *resize-map* (make-sparse-keymap)))))
    (let ((i *resize-increment*))
      (labels ((dks (m keys c)
                 (let ((cmd (format nil c i)))
                   (dolist (k keys)
                     (define-key m (kbd k) cmd)))))
        (if (or (= fx 0)
                (< (+ fx fw) dw))
            (progn
              (dks m '("Right" "f" "l" "C-f") "resize ~D 0")
              (dks m '("Left"  "b" "h" "C-b") "resize -~D 0"))
            (progn
              (dks m '("Right" "f" "l" "C-f") "resize -~D 0")
              (dks m '("Left"  "b" "h" "C-b") "resize ~D 0")))
        (if (or (= fy 0)
                (< (+ fy fh) dh))
            (progn
              (dks m '("Up" "p" "k" "C-p") "resize 0 -~D")
              (dks m '("Down" "n" "j" "C-n") "resize 0 ~D"))
            (progn
              (dks m '("Up" "p" "k" "C-p") "resize 0 ~D")
              (dks m '("Down" "n" "j" "C-n") "resize 0 -~D")))))
    
    (define-key m (kbd "RET") "exit-iresize")
    (define-key m (kbd "C-g") "abort-iresize")
    (define-key m (kbd "ESC") "abort-iresize")))

(defcommand (iresize tile-group) () ()
  "Start the interactive resize mode. A new keymap specific to
resizing the current frame is loaded. Hit @key{C-g}, @key{RET}, or
@key{ESC} to exit."
  (let* ((group (current-group))
         (frame (tile-group-current-frame group))
         (head-frame (frame-head group (tile-group-current-frame group)))
         (dx (frame-x head-frame))
         (dh (frame-height head-frame))
         (dw (frame-width head-frame))
         (fx (- (frame-x frame) dx))
         (fy (frame-y frame))
         (fh (frame-height frame))
         (fw (frame-width frame)))
    (if (atom (tile-group-frame-head group (frame-head group frame)))
        (message "There's only 1 frame!")
        (progn
          (when *resize-hides-windows*
            (dolist (f (head-frames group (current-head)))
              (clear-frame f group)))
          (message "Resize Frame")
          (update-resize-map fx fy fh fw dh fw)
          (push-top-map *resize-map*)
          (draw-frame-outlines group (current-head)))
        ;;   (setf *resize-backup* (copy-frame-tree (current-group)))
        )))

(defun resize-unhide ()
  (clear-frame-outlines (current-group))
  (when *resize-hides-windows*
    (let ((group (current-group))
          (head (current-head)))
      (dolist (f (head-frames group head))
        (sync-frame-windows group f))
      (dolist (w (reverse (head-windows group head)))
        (setf (frame-window (window-frame w)) w)
        (raise-window w))
      (when (current-window)
        (focus-window (current-window))))))

(defcommand (abort-iresize tile-group) () ()
  "Exit from the interactive resize mode."
  (resize-unhide)
  (message "Abort resize")
  ;; TODO: actually revert the frames
  (pop-top-map))

(defcommand (exit-iresize tile-group) () ()
  "Exit from the interactive resize mode."
  (resize-unhide)
  (message "Resize Complete")
  (pop-top-map))
