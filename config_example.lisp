
(in-package :stumpwm)

(defun echo-focus-window-hook (new old)
  (message "~a" new))

(defun echo-urgent-window-hook (target)
  (message "~a" target)
  )

(defun raise-urgent-window-hook (target)
  (when (should-be-raised target)
    (let*
      ((group (find-group (current-screen) "Default")))
      (gselect group)
      (move-window-to-group target group)
      (gselect group)
      (pull-window target)
      (gselect group)
      (group-focus-window group target)
      (gselect group)
      )))

(defun remember-focus-window-hook (new old)
  (setf *globally-previous* *globally-current*)
  (setf *globally-current* new))

(defun click-conversation-focus-window-hook (new old)
  (when (or
	  (window-matches-properties-p new :class "Carrier" :role "conversation")
	  (window-matches-properties-p new :class "Pidgin" :role "conversation")
	  )
    (ratwarp 50 760)
    (ratclick 1) ; FIXME: it doesn't work
    (run-shell-command "echo -e 'ButtonPress 1\\nButtonRelease 1' | xmacroplay $DISPLAY")
    ))
(add-hook *focus-window-hook* 'click-conversation-focus-window-hook)

(defun renumbering-start-hook ()
  (message "Starting renumbering hook")

  (place-existing-windows)

  (gselect (find-group (current-screen) "Net"))
  (net-window-sort)

  (switch-to-group (find-group (current-screen) "Base"))
  (repack-window-numbers)
  )

(defun modeline-start-hook ()
  (message "Setting up modeline")
  (let ((group1 (find-if (lambda (x) (= (group-number x) 1)) (screen-groups (current-screen)))))
    (gselect group1))
  (let ((window (global-matching-window :title "XWatchSystem")))
    (if window
      (progn
	(move-window-to-group window (current-group))
	(group-focus-window (current-group) window)
	(really-raise-window window)
	(my-mode-line :window window)))))

(defun window-set-start-hook ()
  (message "Restoring window set")
  (pull+push "IN-CURRENT-GROUP")
  (number-by-tags))

(defun echo-switch-group-hook (g1 g2)
  (format *error-output* "~s ~s" g1 g2))


defkey.lisp

(in-package :stumpwm)

(set-prefix-key (kbd "Menu"))

(define-key *root-map* (kbd "M") "meta Menu")
(define-key *root-map* (kbd "N") "eval (run-commands \"exec pkill -9 xneur\" \"exec xneur &\")")
(define-key *root-map* (kbd "M-N") "exec pkill -9 xneur")
(define-key *root-map* (kbd "~") "command-mode")
(define-key *root-map* (kbd "ESC") "abort")
(define-key *root-map* (kbd "C-r") "loadrc")
(define-key *root-map* (kbd "O") "other")
(define-key *root-map* (kbd "L") "force-redisplay")

(define-key *root-map* (kbd "M-c") "exec urxvt")
(define-key *root-map* (kbd "C-h") "exec :hour-sleep")
(define-key *root-map* (kbd "C-y") "exec :away")
(define-key *root-map* (kbd "P") "exec :asleep")

(define-key *root-map* (kbd "C-v") "exec ugvim")
(define-key *root-map* (kbd "C-M-v") "exec gvim")

(define-key *root-map* (kbd "u") "exec uzbl")
(define-key *root-map* (kbd "U") "exec uzbl $(xclip -o)")

(define-key *root-map* (kbd "M-w") "exec webkit-program-GtkLauncher")

(define-key *root-map* (kbd "C-f") "exec urxvt -e zsh -c 'xtitle web-streams; screen -D -RR -S 
view-web-streams -U view-web-streams'")
(define-key *root-map* (kbd "C-M") "exec urxvt -e zsh -c 'xtitle emails; screen -D -RR -S view-emails -U 
view-emails'")
(define-key *root-map* (kbd "C-F") "exec uzbl $(find-related-uri)") 

(define-key *root-map* (kbd "C-M-f") "exec firefox")

(define-key *root-map* (kbd "I") "show-im-status")

(define-key *root-map* (kbd "e") "")

(define-key *root-map* (kbd "B") "exec brightness")

;(define-key *root-map* (kbd "Menu") "globally-previous")

(define-key *root-map* (kbd "F12") "gselect .system")
(define-key *root-map* (kbd "F12") "gselect .tag-store")
(define-key *root-map* (kbd "DEL") "gselect Default")
(define-key *root-map* (kbd "M-F11") "pull+push+renumber t")
(define-key *root-map* (kbd "F1")   "ftg-set-tags sh")
(define-key *root-map* (kbd "F2")   "eval (progn (ftg-set-tags \"im\") (number-by-tags))")
(define-key *root-map* (kbd "F3")   "ftg-set-tags tb")
(define-key *root-map* (kbd "F4")   "ftg-set-tags heavy-browser")
(define-key *root-map* (kbd "F5")   "ftg-set-tags light-browser")
(define-key *root-map* (kbd "F6")   "ftg-set-tags view mplayer xine")
(define-key *root-map* (kbd "F7")   "ftg-set-tags vim gvim limp editor")
(define-key *root-map* (kbd "F8")   "ftg-set-tags ssh")
(define-key *root-map* (kbd "F9")   "ftg-set-tags root")
(define-key *root-map* (kbd "M-F1") "ftg-set-tags games")
(define-key *root-map* (kbd "M-F2") "ftg-set-tags monitor")
(define-key *root-map* (kbd "M-F3") "ftg-set-tags p2p")
(define-key *root-map* (kbd "M-F4") "lazarus-layout")
(define-key *root-map* (kbd "M-F5") "ftg-set-tags qemu")
(define-key *root-map* (kbd "M-F6") "gimp-layout")
(define-key *root-map* (kbd "M-F7") "dia-layout")

(define-key *root-map* (kbd "XF86_Switch_VT_1")   "pull-tag sh")
(define-key *root-map* (kbd "XF86_Switch_VT_2")   "pull-tag im")
(define-key *root-map* (kbd "XF86_Switch_VT_3")   "pull-tag tb")
(define-key *root-map* (kbd "XF86_Switch_VT_4")   "pull-tag ff")
(define-key *root-map* (kbd "XF86_Switch_VT_5")   "pull-tag light-browser")
(define-key *root-map* (kbd "XF86_Switch_VT_6")   "pull-tag view mplayer xine")
(define-key *root-map* (kbd "XF86_Switch_VT_7")   "pull-tag vim gvim editor")
(define-key *root-map* (kbd "XF86_Switch_VT_8")   "pull-tag ssh")
(define-key *root-map* (kbd "XF86_Switch_VT_9")   "pull-tag root")
(define-key *root-map* (kbd "M-XF86_Switch_VT_1") "pull-tag games")
(define-key *root-map* (kbd "M-XF86_Switch_VT_2") "pull-tag monitor")
(define-key *root-map* (kbd "M-XF86_Switch_VT_3") "pull-tag p2p")
;(define-key *root-map* (kbd "S-M-F4") "lazarus-layout")
(define-key *root-map* (kbd "M-XF86_Switch_VT_5") "pull-tag qemu")
;(define-key *root-map* (kbd "S-M-F6") "gimp-layout")
;(define-key *root-map* (kbd "S-M-F7") "dia-layout")

(define-key *root-map* (kbd "M-1") "select-window-by-number 11")
(define-key *root-map* (kbd "M-2") "select-window-by-number 12")
(define-key *root-map* (kbd "M-3") "select-window-by-number 13")
(define-key *root-map* (kbd "M-4") "select-window-by-number 14")
(define-key *root-map* (kbd "M-5") "select-window-by-number 15")
(define-key *root-map* (kbd "M-6") "select-window-by-number 16")
(define-key *root-map* (kbd "M-7") "select-window-by-number 17")
(define-key *root-map* (kbd "M-8") "select-window-by-number 18")
(define-key *root-map* (kbd "M-9") "select-window-by-number 19")
(define-key *root-map* (kbd "M-0") "select-window-by-number 10")

(define-key *root-map* (kbd "N") "repack-window-numbers")
(define-key *root-map* (kbd "N") "number-by-tags")

(define-key *root-map* (kbd "T") "ftg-set-tags")
(define-key *root-map* (kbd "C-T") "tag-window")
(define-key *root-map* (kbd "C-M-t") "window-tags")
(define-key *root-map* (kbd "C-M-T") "pull-tag")
(define-key *root-map* (kbd "s-t") "ftg-mark-windows")

(define-key *root-map* (kbd "x") "push-window")

(define-key *root-map* (kbd "d") "dead-windows-cleanup")

(define-key *root-map* (kbd "D") "default-tags")
(define-key *root-map* (kbd "V") "tag-visible")

(define-key *root-map* (kbd "/") "raise-short-tag")
(define-key *root-map* (kbd "M-/") "raise-tag")
(define-key *root-map* (kbd ".") "all-tags")
(define-key *root-map* (kbd "C-.") "scrollable-window-tag-list")

(define-key *root-map* (kbd "C-s") "ftg-set-tag-re")
(define-key *root-map* (kbd "C-S") "ftg-add-tag-re")

(define-key *top-map* (kbd "H-Right") "move-focus right")
(define-key *top-map* (kbd "H-Left") "move-focus left")
(define-key *top-map* (kbd "H-Up") "move-focus up")
(define-key *top-map* (kbd "H-Down") "move-focus down")

(define-key *root-map* (kbd "s-Left") "move-windows-dir Left")
(define-key *root-map* (kbd "s-Right") "move-windows-dir Right")
(define-key *root-map* (kbd "s-Up") "move-windows-dir Up")
(define-key *root-map* (kbd "s-Down") "move-windows-dir Down")

(define-key *root-map* (kbd "H-F1") "frame-push-pull-tags sh")
(define-key *root-map* (kbd "H-F2") "frame-push-pull-tags im")
(define-key *root-map* (kbd "H-F3") "frame-push-pull-tags tb")
(define-key *root-map* (kbd "H-F4") "frame-push-pull-tags heavy-browser")
(define-key *root-map* (kbd "H-F5") "frame-push-pull-tags light-browser")
(define-key *root-map* (kbd "H-F6") "frame-push-pull-tags view mplayer xine")
(define-key *root-map* (kbd "H-F7") "frame-push-pull-tag vim gvim limp editor")
(define-key *root-map* (kbd "H-F8") "frame-push-pull-tag ssh")
(define-key *root-map* (kbd "H-F9") "frame-push-pull-tag root")
(define-key *root-map* (kbd "H-M-F1") "frame-push-pull-tag games")
(define-key *root-map* (kbd "H-M-F2") "frame-push-pull-tag monitor")
(define-key *root-map* (kbd "H-M-F3") "frame-push-pull-tag p2p")
(define-key *root-map* (kbd "H-M-F5") "frame-push-pull-tag qemu")
(define-key *root-map* (kbd "H-M-F6") "frame-push-pull-tag gimp")
(define-key *root-map* (kbd "H-M-F7") "frame-push-pull-tag dia")

(define-key *root-map* (kbd "SPC") "ftg-next-window")
(define-key *root-map* (kbd "M-g") "set-frame-group")
(define-key *root-map* (kbd "C-Q") "only")
(define-key *root-map* (kbd "Q") "ftg-only")
(define-key *root-map* (kbd "M-f") "focus-frame-by-tag-re")

(define-key *root-map* (kbd "M-b") "ratcenter")


defoverride.lisp

(defun focus-all (win)
  "Focus the window, frame, group and screen belonging to WIN. Raise
  the window in it's frame."
  (when win
    (unmap-message-window (window-screen win))
    (switch-to-screen (window-screen win))
    (move-window-to-group win (current-group))
    (group-focus-window (window-group win) win)))


defparam.lisp

(in-package :stumpwm)

(defparameter *fg-color* "lightgreen")
(defparameter *bg-color* "black")
(defparameter *border-color* "darkgray")

(defparameter *default-groups*
  '("Base" "Net" "Administrating" "Split" ".system" ))



deftags.lisp

(in-package :stumpwm)
(defun union-mild (a b) (union a b :test 'equalp))

(defun ends-with (x y) 
  (and
    (>= (length x) (length y))
    (equalp y (subseq x (- (length x) (length y)) (length x)))))

(defun starts-with (x y) 
  (and
    (>= (length x) (length y))
    (equalp y (subseq x 0 (length y)))))

(defun is-room (x s)
  (or
   (equal (window-role x) s)
   (equal (window-title x) s)
   (and
    (equal (window-class x) "Vacuum")
    (equal
     (cl-ppcre:regex-replace-all "[@].*" s "")
     (cl-ppcre:regex-replace-all " .*" (window-title x) "")
     )
    )
   ))

(defun deftags (x)
  (unless 
    (find "no-auto-tags" (window-tags x) :test 'equalp)
    (reduce 
      #'union-mild 
      (list
	(mapcar (lambda(x) (cl-ppcre:regex-replace-all " " x "-"))
		(list 
		  (window-class x)
		  (concatenate 'string "i/" (window-res x))
		  (concatenate 'string "c/" (window-class x))
		  (concatenate 'string "r/" (window-role x))
		  (concatenate 'string "w/" 
			       (write-to-string 
				 (xlib:window-id 
				   (window-xwin x))))
		  ))
	(if (and 
	      (or 
		(equal (window-class x) "Carrier")
		(equal (window-class x) "Pidgin")
		) 
	      (equal (window-role x) "buddy_list")) 
	  (list 
	    ;"1" 
	    "im" "conversation" "base") nil)
	(if (or
	      (equal (window-title x) "Main 'screen' instance")
	      )
	  (list 
	    ;"0" 
	    "base" "sh") nil)
	(if (and 
	      (or 
		(equal (window-class x) "Carrier")
		(equal (window-class x) "Pidgin")
		) 
	      (equal (window-role x) "conversation")) 
	  (list 
	    ;"2" 
	    "im" "base") nil)
	(if (and
	      (equal (window-class x) "Gajim.py")
	      (equal (window-role x) "roster")
	      )
	  (list 
	    ;"2" 
	    "0"
	    "im" "base") nil)
	(if (and
	      (equal (window-class x) "psi")
	      )
	  (list 
	    "im" "base") nil)
	(if (and
	      (equal (window-class x) "Vacuum")
	      )
	  (list 
	    "im" "base") nil)
	(if (and
	      (equal (window-class x) "psi")
	      (equal (window-res x) "main")
	      )
	  (list 
	    ;"2" 
	    "0"
	    ) nil)
	(if (and
	      (equal (window-class x) "Vacuum")
	      (starts-with (window-title x) "Vacuum-IM - ")
	      )
	  (list 
	    ;"2" 
	    "0"
	    ) nil)
	(if (and
	      (equal (window-class x) "Gajim.py")
	      )
	  (list "im" "base" "gajim") nil)
	(if (or
	      (and
		(equal (window-class x) "Gajim.py")
		(not (equal (window-role x) "roster"))
		)
	      (and
		(equal (window-class x) "psi")
		(equal (window-res x) "groupchat")
		)
	      (and
		(equal (window-class x) "Vacuum")
		(ends-with (window-title x) " - Conference")
		)
	      )
	  (cond
	    ((is-room x "webkit%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "15"))
	    ((is-room x "antiutopia-of-the-day@conference.dev.mccme.ru") 
	     (list "15"))
	    ((is-room x "nixos@conference.jabber.ru")
	     (list "14"))
	    ((is-room x "ck%irc.oftc.net@irc.401a0bf1.ignorelist.com")
	     (list "13"))
	    ((is-room x "#dev%dev.mccme.ru@irc.401a0bf1.ignorelist.com") 
	     (list "12"))
	    ((is-room x "dev@conference.dev.mccme.ru") 
	     (list "11"))
	    ((is-room x "real_silence%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "10"))
	    ((is-room x "tailor%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "9"))
	    ((is-room x "glendix%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "8"))
	    ((is-room x "irp%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "7"))
	    ((is-room x "scheme%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "6"))
	    ((is-room x "stumpwm%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "5"))
	    ((is-room x "uzbl%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "4"))
	    ((is-room x "monotone%irc.oftc.net@irc.401a0bf1.ignorelist.com")
	     (list "3"))
	    ((is-room x "btrfs%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "2"))
	    ((is-room x "nixos%irc.freenode.net@irc.401a0bf1.ignorelist.com")
	     (list "1"))
	    (t nil)
	    ) nil)
	(if (and 
	      (or
		(equal (window-class x) "Thunderbird-bin")
		(equal (window-class x) "Mail")
		(equal (window-class x) "Shredder")
		(equal (window-class x) "Lanikai")
		)
	      )
	  (list "mail" "tb" "base") nil)
	(if (and
	      (or
		(equal (window-class x) "Thunderbird-bin")
		(equal (window-class x) "Mail")
		(equal (window-class x) "Shredder")
		)
	      (equal (window-type x) :Normal)
	      (> (length (window-title x)) 8)
	      (not (equal (subseq (window-title x) 0 8) "Compose:"))
	      (not (equal (subseq (window-title x) 0 6) "Write:"))
	      )
	  (list 
	    ;"3"
	    ) nil)
	(if (and   
	      (equal (window-res x) "Navigator")
	      )
	  (list 
	    ;"4" 
	    "browser" "ff" "www" "base") nil)
	(if (or
	      (equal (window-res x) "Browser")
	      (equal (window-class x) "Minefield")
	      (equal (window-class x) "Firefox")
	      (equal (window-class x) "Iceweasel")
	      (equal (window-class x) "Shiretoko")
	      (equal (window-class x) "Namoroka")
	      (equal (window-class x) "Tumucumaque")
	      (equal (window-class x) "Aurora")
	      )
	  (list "browser" "ff" "www" "base" "heavy-browser") nil)
	(if (or
	      (equalp (window-class x) "chrome")
	      ) 
	  (list "chrome" "heavy-browser"))
	(if (or
	      (equal (window-class x) "webkit-program-GtkLauncher")
	      (equal (window-class x) "Webkit-program-GtkLauncher")
	      )
	  (list 
	    ;"5" 
	    "browser" "webkit" "base" "wk" "light-browser") nil)
	(if (or
	      (equal (window-class x) ".midori-wrapped")
	      )
	  (list 
	    ;"5" 
	    "midori" "browser" "webkit" "base" "wk") nil)
	(if (or
	      (equal (window-class x) "Carrier")
	      (equal (window-class x) "Pidgin")
	      (equal (window-class x) "Thunderbird-bin")
	      (equal (window-class x) "Mail")
	      (equal (window-class x) "Shredder")
	      (equal (window-res x) "Navigator")
	      (equal (window-class x) "Gajim.py")
	      )
	  (list "web" "base"))
	(if (or
	      (equal (window-res x) "xterm")
	      (equal (window-res x) "urxvt")
	      (equal (window-res x) "rxvt")
	      )
	  (list "shell" "term"))
	(if (or
	      (equal (window-title x) "su shell")
	      )
	  (list 
	    ;"9" 
	    "root" "admin" "base"))
	(if (or
	      (equal (window-class x) "xmoto")
	      (equalp (window-class x) "warmux")
	      (equalp (window-class x) "tbe")
	      (equalp (window-class x) "glob2")
	      (equalp (window-class x) "widelands")
	      (equalp (window-class x) "liquidwar6")
	      (equal (window-class x) "Sand")
	      )
	  (list "games"))
	(if (or
	      (equal (window-class x) "display")
	      (equal (window-class x) ".wrapped-evince")
	      (equal (window-class x) ".evince-wrapped")
	      (equal (window-class x) "Xpdf")
	      (equal (window-class x) "MuPDF")
	      (equal (window-class x) "XSane")
	      (equal (window-res x) "gv")
	      (equal (window-class x) "Djview")
	      (equal (window-class x) "GQview")
	      (equal (window-class x) "Geeqie")
	      )
	  (list "viewers" "view" "base"))
	(if
	  (or
	    (equalp (window-title x) "qemu")
	    (equalp (window-class x) "qemu")
	    (starts-with (window-class x) "qemu-")
	    )
	  (list "qemu"))
	(if (or
	      (equal (window-res x) "VCLSalFrame")
	      (equal (window-res x) "VCLSalFrame.DocumentWindow")
	      )
	  (list "ooo" "openoffice" "oo.o" "view" "base"))
	(if (or
	      (equalp (window-res x) "gimp")
	      (equalp (window-class x) ".wrapped-inkscape")
	      (equalp (window-class x) ".inkscape-wrapped")
	      (equalp (window-res x) "xfig")
	      (equalp (window-res x) "drgeo")
	      (equalp (window-res x) "kig")
	      )
	  (list "graphics" "editor"))
	(if (or
	      (equalp (window-res x) "xfig")
	      (equalp (window-res x) "drgeo")
	      (equalp (window-res x) "kig")
	      )
	  (list "geom" "geometry"))
	(if (and
	      (or
		(equal (window-res x) "xterm")
		(equal (window-res x) "urxvt")
		(equal (window-res x) "rxvt")
		)
	      (> (length (window-title x)) 12)
	      (equal (subseq (window-title x) 0 12) "ssh session:")
	      )
	  (list "ssh" "base"))
	(if (and
	      (or
		(equal (window-res x) "xterm")
		(equal (window-res x) "urxvt")
		(equal (window-res x) "rxvt")
		)
	      (or
		(equal (window-title x) "web-streams")
		(equal (window-title x) "emails")
		)
	      )
	  (list "web-streams" "viewers" "view"))
	(if (or
	      (equal (window-title x) "Gateway6 monitoring")
	      (equal (window-title x) "Local IRC ghost")
	      )
	  (list "monitor"))
	(if (or
	      (equal (window-title x) "zsh")
	      (equal (window-title x) "sh")
	      (equal (window-title x) "su shell")
	      (equal (window-title x) "bash")
	      )
	  (list "open-shell"))
	(if (or
	      (equalp (window-res x) "vncviewer")
	      (equalp (window-class x) "Vncviewer")
	      (equalp (window-class x) "Gvncviewer")
	      )
	  (list "vnc" "ssh"))
	(if (or
	      (equal (window-class x) "Linuxdcpp")
	      (ends-with (window-title x) "(BitTornado)")
	      )
	  (list "p2p"))
	(if (or
	      (equal (window-class x) "Linuxdcpp")
	      )
	  (list "dc" "150"))
	(if (or
	      (equal (window-class x) "bittornado")
	      (ends-with (window-title x) "(BitTornado)")
	      )
	  (list "bt" "160"))
	(if (or
	      (equal (window-title x) "input-history (~/.local/share/uzbl) - VIM")
	      (equal (window-title x) "input-history + (~/.local/share/uzbl) - VIM")
	      (ends-with (window-title x) ".local/share/uzbl/forms) - VIM")
	      (equal (window-class x) ".uzbl-wrapped")
	      (equal (window-class x) ".uzbl-core-wrapped")
	      (equal (window-class x) ".wrapped-uzbl")
	      )
	  (list "uzbl" "light-browser"))
	(if (or
	      (equal (window-title x) "input-history (~/.local/share/uzbl) - VIM")
	      (ends-with (window-title x) ".local/share/uzbl/forms) - VIM")
	      (equal (window-class x) ".uzbl-wrapped")
	      (equal (window-class x) ".uzbl-core-wrapped")
	      (equal (window-class x) "uzbl")
	      (equal (window-class x) ".wrapped-uzbl")
	      (equal (window-class x) "Links")
	      (equal (window-class x) ".midori-wrapped")
	      (equal (window-class x) ".wrapped-midori")
	      )
	  (list "light-browser" "browser"))
	(if (and
	      (equal (window-class x) "Lazarus")
	      (starts-with (window-title x) "Lazarus IDE"))
	  (list "lazarus-ide-window"))
	(if (and
	      (equal (window-class x) "Lazarus")
	      (starts-with (window-title x) "Messages"))
	  (list "lazarus-message-window"))
	(if (and
	      (equal (window-class x) "Lazarus")
	      (starts-with (window-title x) "Object Inspector"))
	  (list "lazarus-inspector-window"))
	(if (and
	      (equal (window-class x) "Dia")
	      (equal (window-role x) "toolbox_window"))
	  (list "dia-toolbar"))
	(if (and
	      (equal (window-class x) "Gimp")
	      (equal (window-role x) "gimp-toolbox"))
	  (list "gimp-toolbar"))
	(if (or
	      (equalp (window-title x) "Limp")
	      )
	  (list "Limp")
	  )
	(if (or
	      (starts-with (window-title x) "SQuirreL SQL")
	      )
	  (list "SquirrelSQL" "editor" "sql")
	  )
	(if
	  (equalp (window-class x) "org-hypergraphdb-viewer-hgvdesktop")
	  (list "editor" "HGDB" "HGDBViewer" "GraphDB")
	  )
	(if
	  (equalp (window-class x) "freemind-main-FreeMindStarter")
	  (list "editor" "freemind" "mindmap")
	  )
	(if
	  (equalp (window-class x) "tufts-vue-VUE")
	  (list "editor" "vue" "mindmap")
	  )
	(if
	  (equal (window-title x) "XWatchSystem")
	  (list "xwatchsystem" "999"))
	))))


defun.lisp

(in-package :stumpwm)
(defun lock-rule-by-class (class)
  (list 0 T T :class class))
(defun lock-rule-by-title (title)
  (list 0 T T :title title))

(defcommand restart-xwatchsystem () ()
  "Kill old xwatchsystem instances"
  (run-shell-command "ps auxwww | egrep ' -title XWatchSystem ' | sed -e 's/\\s\\+/ /g' | cut -f 2 -d' ' | 
xargs kill " T)
  (run-shell-command "xwatchsystem null")
  )

(defcommand set-as-modeline (&key (window nil) (size nil)) ()
  "Set (possibly current) window as a modeline"
  (dformat 8 "Setting modeline..~%")
  (let* 
    ((win (or window (current-window)))
     (scr (window-screen win))
     (h (car (screen-heads scr)))
     (xwin (window-xwin win))
     )
    (dformat 8 "Withdrawing window ~s (~s) for modeline~%" win xwin)
    (withdraw-window win)
    (dformat 8 "Withdrawn window ~s (~s) for modeline~%" win xwin)
    (dformat 8 "Forgetting old modeline~%")
    (setf (head-mode-line h) nil)
    (dformat 8 "Adjusting modeline position - height ~s~%" size)
    (when size
      (setf (xlib:drawable-height xwin) size)
      )
    (dformat 8 "Remapping the modeline window~%")
    (place-mode-line-window scr xwin)
    ;(dformat 8 "Setting modeline window~%")
    ;(let 
    ;  ((ml (head-mode-line h)))
    ;  (set-mode-line-window ml xwin)
    ;  )
    (when (equal *mode-line-position* :bottom)
      (let* ((ml (head-mode-line h))
	     (xw (mode-line-window ml))
	     )
	(setf (mode-line-position ml) :bottom)
	(setf (xlib:drawable-y xw) (- (head-height h) (xlib:drawable-height xw)))
	(sync-mode-line ml)
	)
      )
    ))

(defcommand my-mode-line (&key (window nil)) ()
	    "Set (possibly current) window as modeline window with my special settings"
  (setf *mode-line-position* :bottom)
  (set-as-modeline :window window :size *min-frame-height*)
  )

(defun should-be-raised (window)
  (and
    (if 
      (equal (window-class window) "Carrier") 
      (and
	(equal (subseq (window-title window) 0 3) "(*)")
	)
      t)
    (if 
      (equal (window-class window) "Pidgin") 
      (and
	(equal (subseq (window-title window) 0 3) "(*)")
	)
      t)
    (if 
      (equal (window-class window) "psi") 
      (and
	(equal (subseq (window-title window) 0 2) "* ")
	)
      t)
    ))

(defun renumber-window (w n)
  (when w
    (select-window-by-number (window-number w))
    (renumber n)))

(defun local-matching-window (&rest args) ;(&key class instance type role title)
  (find-if 
    (lambda (w) 
      (apply 'window-matches-properties-p (cons w args)))
    (group-windows (screen-current-group (current-screen)))))

(defun global-matching-window (&rest args) ;(&key class instance type role title)
  (find-if 
    (lambda (w) 
      (apply 'window-matches-properties-p (cons w args)))
    (screen-windows (current-screen))))

(defcommand net-window-sort () ()
	    "Place networking-related windows in my preferred order"
  (renumber-window 
    (local-matching-window :class "Carrier" :role "buddy_list")
    1)
  (renumber-window 
    (local-matching-window :class "Pidgin" :role "buddy_list")
    1)
  (renumber-window 
    (local-matching-window :class "Carrier" :role "conversation")
    2)
  (renumber-window 
    (local-matching-window :class "Pidgin" :role "conversation")
    2)
  (renumber-window
    (local-matching-window :class "Thunderbird-bin")
    3)
  (renumber-window
    (local-matching-window :instance "Navigator")
    4))

(defcommand globally-previous () ()
	    "Switch to the previous window (possibly from another group) that had focus"
	    (let* 
	      ((window *globally-previous*)
	       (group (window-group window))
	       (frame (window-frame window)))
	      (gselect group)
	      (focus-frame group frame)
	      (focus-window window)))

(defcommand hibernate-pc () ()
	    "Execute suspend-to-disk"
  (fclear)
  (run-shell-command "susp"))

(defcommand restart-thunderbird () ()
	    "Restart Thunderbird"
	    (run-shell-command "pkill thunderbird" T)
	    (run-shell-command "thunderbird"))

(defcommand cleanup-window () ()
	    "Kill current window that actually got destroyed long ago"
	    (destroy-window (current-window)))

(defun numbered-tag (n)
  (if (= n 0) "@"
    (concatenate 'string (numbered-tag (truncate (/ (- n 1) 36)))
		 (let ((x (mod (- n 1) 36)))
		   (subseq "1234567890qwertyuiopasdfghjklzxcvbnm" x (+ x 1))))))

(defcommand short-tags () ()
	    "Create short tags for quick pulls"
	    (let*
	      ((wins (screen-windows (current-screen)))
	       (counter 0)
	       )
	      (mapcar 
		(lambda(y) (clear-tags-if (lambda(x) (equal (subseq x 0 1) "@")) y))
		wins)
	      (mapcar
		(lambda(x) 
		  (setf counter (+ counter 1))
		  (tag-window (numbered-tag counter) x))
		wins)
	      ))

(defcommand raise-short-tag (argtag) ((:rest "Short tag to pull: "))
	    "Make window current by short tag"
	    (or
	      (raise-tag (concatenate 'string "@" argtag))
	      (raise-tag argtag)))

(defcommand default-tags () ()
	    "Add default tags to all windows"
	    (mapcar
	      (lambda(x)
		(setf (window-tags x) (union (window-tags x) (deftags x) :test 'equalp))
		)
	      (screen-windows (current-screen))
	      )
	    (short-tags)
	    )

(defun window-alive (win)
  (let
    ((marker (random 255)))
    (xlib:change-property (window-xwin win)
			  :STUMPWM_CHECK_IF_ALIVE
			  (list marker)
			  :UINT 8)
    (equal (list marker) (xlib:get-property (window-xwin win)
				 :STUMPWM_CHECK_IF_ALIVE))))

(defcommand dead-windows-cleanup () ()
	    "Kill the windows that mysteriously disappeared"
	    (mapcar
	      (lambda(x) 
		(if (not (window-alive x)) 
		  (progn 
		    (move-window-to-group x (current-group))
		    (fclear)
		    (really-raise-window x)
		    (destroy-window x))))
	      (screen-windows (current-screen))))

(defcommand reload-defuns () ()
	    "Only load definitions of functions from rc"
	    (load "/var/repos/stumpwm/contrib/window-tags.lisp")
	    (load-rcpart "deftags")
	    (load-rcpart "defun")
	    )

(defcommand reload-defhooks () ()
	    "Only load definitions of hooks from rc"
	    (load-rcpart "defhook"))

(defcommand reload-defkeys () ()
	    "Only load key bindings from rc"
	    (load-rcpart "defkey"))

(defcommand reload-setvars () ()
	    "Only load variable values from rc"
    (load-rcpart "defpass")
    (load-rcpart "defparam")
    (load-rcpart "setvar")
    )

(defcommand pull+push+renumber (argtags) ((:rest "Tags to select: "))
  "Select windows by tags and renumber them"
  (gselect (find-group (current-screen) "Default"))
  (only)
  (fclear)
  (let ((visible-window (car (reverse (select-by-tags argtags)))))
       (if visible-window (move-window-to-group visible-window (current-group)))
       (pull+push argtags)
       (number-by-tags)
       (if visible-window 
           (setf (group-windows (current-group))
                 (cons visible-window (remove visible-window (group-windows (current-group)))))))
  (if (and (not (current-window)) (group-windows (current-group))) 
      (pull-hidden-next)))

(defcommand scrollable-window-tag-list () ()
	    "Show windows and their tags in a terminal"
	    (run-shell-command "urxvt -e sh -c 'echo all-tags | TERM=rxvt ~/script/external/stumpish | 
less'"))

(defun resize-local-frame-to (group frame x y)
  (if x (progn
         (resize-frame group frame -999999 :width) 
         (resize-frame group frame (- x *min-frame-width*) :width)
         ))
  (if y (progn 
         (resize-frame group frame -999999 :height) 
         (resize-frame group frame (- y *min-frame-height*) :height)
         )))

(defcommand lazarus-layout () ()
  "Load my Lazarus layout"
  (pull+push+renumber "lazarus")
  (let* (
         (group (current-group)) 
         (frame (tile-group-current-frame group))
         (header-number (frame-number frame))
         (inspector-number (split-frame group :row))
         (dummy (fselect (frame-by-number group inspector-number)))
         (form-number (split-frame group :column))
         (dummy (fselect (frame-by-number group form-number)))
         (messages-number (split-frame group :row))
         (header (frame-by-number group header-number))
         (inspector (frame-by-number group inspector-number))
         (form (frame-by-number group form-number))
         (messages (frame-by-number group messages-number))
         )
        (resize-local-frame-to group header nil 100)
        (resize-local-frame-to group inspector 210 nil)
        (resize-local-frame-to group messages nil 90)
        (mapcar (lambda (w) (pull-window w form)) (select-by-tags "lazarus"))
        (mapcar (lambda (w) (pull-window w header)) (select-by-tags "lazarus-ide-window"))
        (mapcar (lambda (w) (pull-window w inspector)) (select-by-tags "lazarus-inspector-window"))
        (mapcar (lambda (w) (pull-window w messages)) (select-by-tags "lazarus-message-window"))
        (focus-frame group form)
        ))

(defcommand dia-layout () ()
  "Load my Dia layout"
  (ftg-set-tags "dia")
  (ftg-only)
  (let* (
         (group (current-group))
         (fn (find-free-frame-number group))
         (dummy (hsplit))
         (f1 (tile-group-current-frame group))
         (f2 (frame-by-number group fn))
	 (ftg (frame-tagged-group f1))
         )
        (tag-frame "dia-main" f1)
        (tag-frame "dia-toolbar" f2)
        (resize -999999 0)
        (resize (- 160 *min-frame-width*) 0)
	(act-on-matching-windows 
	  (w :group)
	  (in-frame-tg-p w ftg)
	  (pull-window w f2))
        (mapcar (lambda(w) (pull-window w f1)) (select-by-tags "dia-toolbar"))
        (focus-frame group f2)))

(defcommand gimp-layout () ()
  "Load my Gimp layout"
  (pull+push+renumber "gimp")
  (let* (
         (group (current-group))
         (f2 (frame-by-number group (split-frame group :column)))
         (f1 (tile-group-current-frame group))
         )
        (resize-local-frame-to group f1 230 nil)
        (mapcar (lambda(w) (pull-window w f2)) (group-windows (current-group)))
        (mapcar (lambda(w) (pull-window w f1)) (select-by-tags "gimp-toolbar"))
        (focus-frame group f2)))

(defcommand xrandr (state) ((:rest "Desired XRandr state: "))
  "Switch xrandr state"
  (if (equal state "on")
    (run-shell-command "xrandr --output VGA1 --right-of LVDS1 --preferred"))
  (if (equal state "off")
    (run-shell-command "xrandr --output VGA1 --right-of LVDS1 --off")))

(defcommand irc-password () ()
  (loop for x in `( "i" "d" "e" "n" "t" "i" "f" "y" 
                        "space" 
                        ,@(map 'list 'string *irc-pass*)
                        "Return" "C-Return" "C-w")
        do (meta (kbd x))))

(defcommand kill-freenode-from-self () ()
  (mapcar 'delete-window 
          (remove-if (lambda (x) (not (equalp (window-title x) "MichaelRaskin - Gajim")))
                     (group-windows (current-group))))
  (mapcar 'delete-window 
          (remove-if (lambda (x) (not (starts-with (window-title x) "nickserv!")))
                     (group-windows (current-group))))
  )

(defcommand force-redisplay () ()
  "Like redisplay, only resizing to 1x1"
  (let ((window (current-window)))
       (set-window-geometry 
        window 
        :width (truncate (/ (window-width window) 2))
        :height (truncate (/ (window-height window) 2)))
       (xlib:display-finish-output *display*)
       (sleep 0.1)
       (redisplay)))

(defcommand show-im-status () ()
  (let*
   ((im-windows (select-by-tags "im"))
    (im-titles (mapcar 'window-title im-windows))
    )
   (restore-psi-windows)
   (message "IM windows:~%~{~%~a~}" im-titles)
   ))

(defcommand unread-folders-thunderbird () ()
  (meta (kbd "M-v"))
  (sleep 0.1)
  (meta (kbd "f"))
  (sleep 0.1)
  (meta (kbd "u")))

(defcommand all-folders-thunderbird () ()
  (meta (kbd "M-v"))
  (sleep 0.1)
  (meta (kbd "f"))
  (sleep 0.1)
  (meta (kbd "a")))

(defcommand create-windows-only-here () ()
  (setf *new-window-preferred-frame* 
        (constantly
         (tile-group-current-frame (current-group)))))
(defcommand create-windows-focused () ()
  (setf *new-window-preferred-frame* '(:focused)))

(defcommand restore-psi-windows () ()
  (loop for x in (screen-withdrawn-windows (current-screen))
        when (cl-ppcre:scan "[*] " (window-title x))
        do (restore-window x)))

(defcommand kill-all-here () ()
	    "Kill all windows in current group"
  (loop for w in (group-windows (current-group))
        do (delete-window w)))

(defun merge-frame (from to)
  (when to
    (act-on-matching-windows (w from) t (pull-window w to))))

(defcommand 
  (move-windows-dir tile-group) (dir) ((:direction "Direction: "))
  "Move all windows from this frame to frame num"
  (merge-frame (tile-group-current-frame (current-group))
	       (neighbour dir (tile-group-current-frame (current-group))
			  (group-frames (current-group)))))
(defcommand 
  (move-windows-num tile-group) (num) ((:number "Number: "))
  (merge-frame (tile-group-current-frame (current-group))
	       (frame-by-number (current-group) num)))

(defcommand 
  (move-windows-tag tile-group) (tag) ((:rest "Tag: "))
  "Move all windows to a frame tagged tag"
  (merge-frame (tile-group-current-frame (current-group))
	       (frame-by-number (current-group) (first-frame-by-tag tag))))

(defcommand
  frame-push-pull-tags (argtags) ((:rest "Tags: "))
  "Replace contents of current frame with windows selected by tags argtags"
  (let*
    ((tag (if (stringp argtags) 
	    (remove "" (cl-ppcre:split " " (string-upcase argtags))
		    :test 'equalp)
	    (mapcar 'string-upcase argtags))))
    (act-on-matching-windows 
      (w :frame) (not (tagged-any-p w tag))
      (push-w w))
    (act-on-matching-windows 
      (w :screen) (tagged-any-p w tag)
      (pull-w w) (pull-window w (tile-group-current-frame (current-group))))))

(defcommand 
  load-rcp (name) ((:rest "Part: "))
  "Load-rcpart wrapper"
  (load-rcpart name))

(defcommand
  reference-frame () ()
  "Create a reference frame that can house an URxvt of 80 symbols (820 px)"
  (let*
    ((group (current-group))
     (old-frame (tile-group-current-frame group))
     (ref-number (split-frame group :column 
			      (- (frame-width old-frame) 750)))
     (ref (frame-by-number group ref-number)))
    (focus-frame group ref)
    (set-frame-group "ref")
    (focus-frame group (frame-by-number group (frame-number old-frame)))))

(defcommand
  ratcenter () ()
  "Center the mouse pointer in current frame"
  (let*
    ((f (tile-group-current-frame (current-group)))
     (cx (+ (frame-x f) (ash (frame-width f) -1)))
     (cy (+ (frame-y f) (ash (frame-height f) -1))))
    (ratwarp cx cy)))


defvar.lisp

(in-package :stumpwm)

(defvar *globally-previous* '())
(defvar *globally-current* '())


