;; -*-lisp-*-
;;
;;;; This i a default dswm configfile
;;;; You may copy it to ~/.dswm or ~/.dswm.d/init.lisp
;;;; and edit, using your favourite text editor
;;;;
;;;; NOTE: short manual for any of this variables
;;;;       you can get, using internal dswm command
;;;;       'describe-variable' You have to enter
;;;;       'Control+j' (or your cutom prefix) then
;;;;       enter ';', in input prompt enter 'describe-variable',
;;;;       then input name of variable (for example *terminal*)
;;;;       and you will get short documentation for requested variable
;;;;
;;;; NOTE: You can get short manual for functions (like '(set-font ...)' )
;;;;       using 'describe-function'

;;;; Change the prefix key to something else
;; Maybe would be a good thing to use (kbd "Insert")
;; for prefix key
;; (set-prefix-key (kbd "C-z"))

;;;; Terminal emulator program (default xterm)
;; (setf *terminal* "urxvt")

;;;; Web Browser (default conkeror)
;; (setf *browser* "firefox")

;;;; Emacs implementation (default emacs, can be used xemacs, climacs etc)
;; (setf *emacs* "xemacs")

;;;; Messages window gravity
;; ; :top-left :top-right :bottom-left :bottom-right :center
;; (setf *message-window-gravity* :top-right)

;;;; Input window gravity
;; ; :top-left :top-right :bottom-left :bottom-right :center
;; (setf *input-window-gravity* :bottom-left)

;;;; From which source window name will be formed
;;;; Can be used :title, :class, :resource-name
;; (setf *window-name-source* :title)

;;;; Window format
;;;; See short manual, using internal command 'describe-variable'
;; (setf *window-format* "|%m%s%n %c %10t...")

;; in seconds, how long a message will appear for. This must be an integer.
;; (setf *timeout-wait* 5)

;; In what frame system try to open next window (default :focused)
;; (setf *new-window-preferred-frame* '(:empty :focused))

;;;; Mouse focus policy (default :sloopy)
;; ; :ignore, :sloppy, and :click
;; (setf *mouse-focus-policy* :sloppy)

;;;; Name of default group
;; (setq *default-group-name* "Main")

;;;; Style of window borders
;; ; :thick :thin :tight :none
;; (setf *window-border-style* :thick)

;;;; Mode line
;; Mode line position (default :top)
;; :top or :bottom
;; (setf *mode-line-position* :top)

;; How thick the mode line border
;; (setf *mode-line-border-width* 0)

;; How much padding should be between the mode line text and the sides?
;; (setf *mode-line-pad-x* 6)

;; Define mode line background color
;; (setf *mode-line-background-color* "black")

;; Define mode line foreground color
;; (setf *mode-line-foreground-color* "lightgreen")

;; Define mode line border color
;; (setf *mode-line-border-color* "lightyellow")

;;;; Debug level: 0-5
(setf *debug-level* 0)

;;;;
;;;; Custom keybindings:
;;;;
;;;; Read some doc with gv
;; (define-key *root-map* (kbd "d") "exec gv")
;;;; Lock screen
;; (define-key *root-map* (kbd "C-l") "exec xlock")

;;;; Message window font
;; May to be present in all popular linux systems
(set-font "-*-fixed-medium-r-*-*-14-*-*-*-*-*-iso10646-1")

;;;; Loading some external modules by default
(load-module "s-bindings")

;;;; Load module amixer and define volume control functions
(if (load-module "amixer")
    (progn
      (dswm::defvolcontrol amixer-PCM-5- "PCM" "5-")
      (dswm::defvolcontrol amixer-PCM-5+ "PCM" "5+")
      (dswm::defvolcontrol amixer-Front-5- "Front" "5-")
      (dswm::defvolcontrol amixer-Front-5+ "Front" "5+")
      (dswm::defvolcontrol amixer-Master-5- "Master" "5-")
      (dswm::defvolcontrol amixer-Master-5+ "Master" "5+")
      (dswm::defvolcontrol amixer-Headphone-5- "Headphone" "5-")
      (dswm::defvolcontrol amixer-Headphone-5+ "Headphone" "5+")
      (defkeys-top
	("XF86AudioLowerVolume" "amixer-Master-5-")
	("XF86AudioRaiseVolume" "amixer-Master-5+")
	("XF86AudioMute" "amixer-Master-toggle"))))

;; Define some useful keybindings
(defkeys-top
  ("XF86Calculator" "exec emacs -Q -f calc")
  ("XF86HomePage" (concat "exec " *browser* " http://dss-project.org"))
  ("XF86Search" (concat "exec " *browser* " http://google.com"))
  ("XF86Mail" "exec thunderbird")
  ;; Good group navigation
  ;;  ("XF86Forward" "gnext") ;; Goto next group
  ;;  ("XF86Back" "gprev")    ;; Goto preview group
  
  ;; Control music playing. Need installed mpd and mpc
  ;; ("XF86AudioPlay"  "exec mpc toggle")
  ;; ("XF86Launch5"  "exec mpc next")
  ;; ("XF86Launch1"  "exec mpc prev")
)

;;;; set up X cursor color.
;; (run-shell-command (format nil "xsetroot -cursor_name left_ptr -fg \"~a\"" "red" BACKGROUND-COLOR))

;;;; Eye candy
;;;; Set background color
;; (set-bg-color "black")

;;;; Set foreground color
;; (set-fg-color "lightgreen")

;;;; Set border color
;; (set-border-color "yellow")

;;;; Set color of focused window
;; (set-focus-color   "DarkRed")

;;;; Set color of unfocused window
;; (set-unfocus-color "black")

;;;; Set message border width
;; (set-msg-border-width 0)