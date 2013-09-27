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
;; (setf *terminal* "xterm")

;;;; Web Browser (default conkeror)
;; (setf *browser* "firefox")

;;;; Default shell program
;; (setf *shell-program* "/bin/sh")

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
;;;; Because clisp doesn`t support UTF8 by default
#+clisp
(set-font "-*-fixed-medium-r-*-*-14-*-*-*-*-*-*")
#-clisp
(set-font "-*-fixed-medium-r-*-*-14-*-*-*-*-*-iso10646-1")

;;;; let`s add custom directory with modules
;; (add-modules-dir "/my/favorite/directory/dswm-modules")

;; ;;;; Define some useful keybindings
;; (define-keys *top-map*
;; 	((kbd "XF86Calculator") "exec emacs -Q -f calc")
;; 	((kbd "XF86HomePage") (concat "exec " *browser* " http://dss-project.org"))
;; 	((kbd "XF86Search") (concat "exec " *browser* " http://google.com"))
;; 	((kbd "XF86Mail") "exec thunderbird")

;; 		;; Good group navigation
;; 		;; ((kbd "XF86Forward") "gnext") ;; Goto next group
;; 		;; ((kbd "XF86Back") "gprev")    ;; Goto preview group

;; 		;; Control music playing. Need installed mpd and mpc
;; 		;; ((kbd "XF86AudioPlay")  "exec mpc toggle")
;; 		;; ((kbd "XF86Launch5")  "exec mpc next")
;; 		;; ((kbd "XF86Launch1")  "exec mpc prev")
;; )

;; ;;;; Wake up, Neo. Matrix has you
;; (defvar *matrix-bg-color* "black")
;; (defvar *matrix-fg-color* "green")
;; (defvar *matrix-cursor-color* "DarkRed")
;; ;; also, we can define colors, using HEX codes
;; (defvar *matrix-border-color* (make-color-hex "#008000"))
;; (defvar *matrix-focus-color* "lightgreen")
;; (defvar *matrix-unfocus-color* "DarkGreen")

;; ;;;; Do not load this code, when executing 'loadrc' command:
;; (startup-only
;;  ;; let`s run some shell commands
;;  (run-shell-commands

;; 	;; set up X cursor color.
;; 	(format nil "xsetroot -cursor_name left_ptr -fg \"~a\"" "red" *matrix-cursor-color*)

;; 	;; simple set up multiple keymaps for different languages
;; 	"setxkbmap -layout 'us,ua,ru' -option 'grp:alt_shift_toggle,grp_led:caps,ctrl:nocaps"

;; 	;; disable replace Caps Lock to Control key
;; 	"setxkbmap -option 'ctrl:nocaps'"

;; 	;; let`s start conkeror automatically
;; 	"conkeror"
;; 	)

;; 	;; ...and load some DSWM modules...
;;  (load-module "s-bindings")

;;  ;; input some your own commands here, if you wish
;;  )

;; ;;;; Eye candy
;; ;;;; Set background color
;; (set-bg-color *matrix-bg-color*)

;; ;;;; Set foreground color
;; (set-fg-color *matrix-fg-color*)

;; ;;;; Set border color
;; (set-border-color *matrix-border-color*)

;; ;;;; Set color of focused window
;; (set-focus-color *matrix-focus-color*)

;; ;;;; Set color of unfocused window
;; (set-unfocus-color *matrix-unfocus-color*)

;; ;;;; It's just a small bug ;)
;; (in-package :dswm)
;; ;;;; mode-line coloring
;; (set-mode-line-fg-color *matrix-fg-color*)
;; (set-mode-line-bg-color *matrix-bg-color*)
;; (set-mode-line-border-color *matrix-border-color*)

;; ;;;; Set message border width
;; (set-msg-border-width 0)
