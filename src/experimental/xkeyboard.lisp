;;;; Save keyboard layout per window for stumpwm. 

(asdf:compute-source-registry)
(asdf:load-system :xkeyboard)

(in-package :stumpwm)

(defun get-current-layout (display)
  (xlib:device-state-locked-group (xlib:get-state display)))

(defun window-focus-changed (window previous-window)
  (let ((current-layout (get-current-layout *display*)))
    (when previous-window
      (setf (getf (xlib:window-plist (window-xwin previous-window)) :keyboard-layout) current-layout)
      (when window
        (let ((window-layout (getf (xlib:window-plist (window-xwin window)) :keyboard-layout current-layout)))
          (when (not (equal current-layout window-layout))
            (xlib:lock-group *display* :group window-layout)))))))

(defun group-focus-changed (group previous-group)
  (let ((previous-window (stumpwm::group-current-window previous-group))
        (window (stumpwm::group-current-window group)))
    (window-focus-changed window previous-window)))

(defcommand enable-per-window-layout () ()
  "Enable layout switching"
  (xlib::initialize-extensions *display*) ;; we need it because
  (xlib:enable-xkeyboard *display*) ;; stumpwm opens display before extension definition  
  (add-hook *focus-group-hook* 'group-focus-changed)
  (add-hook *focus-window-hook* 'window-focus-changed))

(defcommand disable-per-window-layout () ()
  "Disable layout switching"
  (remove-hook *focus-window-hook* 'window-focus-changed)
  (remove-hook *focus-group-hook* 'group-focus-changed))

