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
;; This file contains primitive data structures and functions used
;; throughout dswm.
;;
;; Code:

(in-package :dswm)

(export '(gnew-float
	  gnewbg-float
	  gnew-float-with-window
	  gnew-float-with-marked
	  grun-new-float))

(defclass float-group (group)
  ((current-window :accessor float-group-current-window)))

(defmethod group-startup ((group float-group))
  )

(defmethod group-add-window ((group float-group) window &key &allow-other-keys)
  (change-class window 'float-window)
  (float-window-align window)
  (focus-window window))

(defun &float-focus-next (group)
  (if (group-windows group)
      (focus-window (first (group-windows group)))
      (no-focus group nil)))

(defmethod group-delete-window ((group float-group) window)
  (declare (ignore window))
  (&float-focus-next group))

(defmethod group-wake-up ((group float-group))
  (&float-focus-next group))

(defmethod group-suspend ((group float-group))
  )


(defmethod group-resize-request ((group float-group) window width height)
  (float-window-move-resize window :width width :height height))

(defmethod group-move-request ((group float-group) window x y relative-to)
  (declare (ignore relative-to))
  (float-window-move-resize window :x x :y y))

(defmethod group-raise-request ((group float-group) window type)
  (declare (ignore type))
  (focus-window window))

(defmethod group-lost-focus ((group float-group))
  (&float-focus-next group))

(defmethod group-indicate-focus ((group float-group))
  )

(defmethod group-focus-window ((group float-group) window)
  (focus-window window))

(defmethod group-root-exposure ((group float-group))
  )

(defmethod group-add-head ((group float-group) head)
  (declare (ignore head)))

(defmethod group-remove-head ((group float-group) head)
  (declare (ignore head)))

(defmethod group-resize-head ((group float-group) oh nh)
  (declare (ignore oh nh)))

(defmethod group-sync-all-heads ((group float-group))
  )

(defmethod group-sync-head ((group float-group) head)
  (declare (ignore head))
  )

(defcommand gnew-float (name) ((:string "Input group name: "))
"Create a floating window group with the specified name and switch to it."
  (add-group (current-screen) name :type 'float-group))

(defcommand gnewbg-float (name) ((:string "Input group name: "))
"Create a floating window group with the specified name, but do not switch to it."
  (add-group (current-screen) name :background t :type 'float-group))

(defcommand gnew-float-with-window (name) ((:string "Input group name: "))
  "Move current window to new float group"
  (let ((group (gnewbg-float name)))
    (gmove group)
    (gselect group)))

(defcommand gnew-float-with-marked (name) ((:string "Input group name: "))
  "Move marked windows to new float group"
  (let ((group (gnewbg-float name)))
    (gmove-marked group)
    (gselect group)))

(defcommand grun-new-float (command) ((:shell "Input command to run program: "))
  "Run shell command in new float group with same name with command"
  (check-type command string)
  (gnew-float command)
  (run-shell-command command))
