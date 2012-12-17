;; Copyright (C) 2003-2008 Shawn Betts
;; Copyright (C) 2010-2011 Alexander aka CosmonauT Vynnyk
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

(export '(*all-modifiers*
	  *browser*
	  *command-mode-end-hook*
	  *command-mode-start-hook*
	  *debug-expose-events*
	  *debug-level*
	  *debug-stream*
	  *deny-map-request*
	  *deny-raise-request*
	  *destroy-window-hook*
	  *display*
	  *editor-bindings*
	  *emacs*
	  *event-processing-hook*
	  *executing-dswm-command*
	  *focus-frame-hook*
	  *focus-group-hook*
	  *focus-window-hook*
	  *frame-indicator-text*
	  *frame-indicator-timer*
	  *frame-number-map*
	  *group-format*
	  *group-formatters*
	  *group-number-map*
	  *hidden-window-color*
	  *honor-window-moves*
	  *initializing*
	  *input-window-gravity*
	  *internal-loop-hook*
	  *key-press-hook*
	  *last-command*
	  *list-hidden-groups*
	  *map-window-hook*
	  *max-last-message-size*
	  *maxsize-border-width*
	  *maxsize-gravity*
	  *menu-maximum-height*
	  *menu-scrolling-step*
	  *menu-window-gravity*
	  *message-hook*
	  *message-window-gravity*
	  *message-window-padding*
	  *message-window-timer*
	  *min-frame-height*
	  *min-frame-width*
	  *mode-line-click-hook*
	  *mode-line-pad-x*
	  *mode-line-pad-y*
	  *mode-line-position*
	  *mode-line-timeout*
	  *modifiers*
	  *mouse-focus-policy*
	  *new-frame-action*
	  *new-frame-hook*
	  *new-window-hook*
	  *new-window-preferred-frame*
	  *normal-border-width*
	  *normal-gravity*
	  *place-window-hook*
	  *processing-existing-windows*
	  *record-last-msg-override*
	  *resize-hides-windows*
	  *resize-map*
	  *root-click-focuses-frame*
	  *root-click-hook*
	  *run-or-raise-all-groups*
	  *run-or-raise-all-screens*
	  *scratchpad-group-name*
	  *screen-list*
	  *mode-line-format*
	  *mode-line-formatters*
	  *info-line-format*
	  *shell-program*
	  *show-tip-of-the-day-p*
	  *split-frame-hook*
	  *start-hook*
	  *suppress-abort-messages*
	  *suppress-frame-indicator*
	  *terminal*
	  *text-color*
	  *timeout-frame-indicator-wait*
	  *timeout-wait*
	  *top-level-error-action*
	  *top-level-error-hook*
	  *transient-border-width*
	  *transient-gravity*
	  *unmap-window-hook*
	  *urgent-window-hook*
	  *window-border-style*
	  *window-events*
	  *window-format*
	  *window-formatters*
	  *window-name-source*
	  *window-parent-events*
	  *x-selection*
	  *xwin-to-window*
	  data-dir
	  data-dir-file
	  module-data-dir-file
	  with-data-file
	  make-color-hex
	  concat
	  ))



;;;; Init functions
;; Here, because needed for +default-foreground-color+... etc
(defun concat (&rest strings)
  "Concatenates strings, like the Unix command 'cat'.
A short for (concatenate 'string foo bar)."
  (apply 'concatenate 'string strings))

(defun make-color-hex (hex)
  "Converts a hexadecimal representation of a color to a decimal from [0,1)."
  (labels ((convert (x)
		    (/ (read-from-string (concat "#x" x)) 256.0)))
    (assert (and (eql (elt hex 0) #\#) (= (length hex) 7)))
    (let ((red (subseq hex 1 3))
          (green (subseq hex 3 5))
          (blue (subseq hex 5 7)))
      (xlib:make-color :red (convert red)
                       :green (convert green)
                       :blue (convert blue)))))
;; /Here, because needed for +default-foreground-color+... etc

;;; Message Timer
(defvar *suppress-abort-messages* nil
  "Suppress abort message when non-nil.")

(defvar *timeout-wait* 5
  "Specifies, in seconds, how long a message will appear for. This must
be an integer.")

(defvar *timeout-frame-indicator-wait* 1
  "The amount of time a frame indicator timeout takes.")

(defvar *frame-indicator-timer* nil
  "Keep track of the timer that hides the frame indicator.")

(defvar *frame-indicator-text* " Current Frame "
  "What appears in the frame indicator window?")

(defvar *suppress-frame-indicator* nil
  "Set this to T if you never want to see the frame indicator.")

(defvar *message-window-timer* nil
  "Keep track of the timer that hides the message window.")

(defvar *grab-pointer-count* 0
  "The number of times the pointer has been grabbed")

;;; Hooks

(defvar *command-mode-start-hook* '(command-mode-start-message)
  "A hook called whenever command mode is started")

(defvar *command-mode-end-hook* '(command-mode-end-message)
  "A hook called whenever command mode is ended")

(defvar *urgent-window-hook* '()
  "A hook called whenever a window sets the property indicating that
  it demands the user's attention")

(defvar *map-window-hook* '()
  "A hook called whenever a window is mapped.")

(defvar *unmap-window-hook* '()
  "A hook called whenever a window is withdrawn.")

(defvar *new-window-hook* '()
  "A hook called whenever a window is added to the window list. This
includes a genuinely new window as well as bringing a withdrawn window
back into the window list.")

(defvar *destroy-window-hook* '()
  "A hook called whenever a window is destroyed or withdrawn.")

(defvar *focus-window-hook* '()
  "A hook called when a window is given focus. It is called with 2
arguments: the current window and the last window (could be nil).")

(defvar *place-window-hook* '()
  "A hook called whenever a window is placed by rule. Arguments are
window group and frame")

(defvar *start-hook* '()
  "A hook called when dswm starts.")

(defvar *internal-loop-hook* '()
  "A hook called inside dswm's inner loop.")

(defvar *focus-frame-hook* '()
  "A hook called when a frame is given focus. The hook functions are
called with 2 arguments: the current frame and the last frame.")

(defvar *new-frame-hook* '()
  "A hook called when a new frame is created. the hook is called with
the frame as an argument.")

(defvar *split-frame-hook* '()
  "A hook called when a frame is split. the hook is called with
the old frame (window is removed), and two new frames as arguments.")

(defvar *message-hook* '()
  "A hook called whenever dswm displays a message. The hook
function is passed any number of arguments. Each argument is a
line of text.")

(defvar *top-level-error-hook* '()
  "Called when a top level error occurs. Note that this hook is
run before the error is dealt with according to
*top-level-error-action*.")

(defvar *focus-group-hook* '()
  "A hook called whenever dswm switches groups. It is called with 2 arguments: the current group and the last group.")

(defvar *key-press-hook* '()
  "A hook called whenever a key under *top-map* is pressed.
It is called with 3 argument: the key, the (possibly incomplete) key
sequence it is a part of, and command value bound to the key.")

(defvar *root-click-hook* '()
  "A hook called whenever there is a mouse click on the root
window. Called with 4 arguments, the screen containing the root
window, the button clicked, and the x and y of the pointer.")

(defvar *mode-line-click-hook* '()
  "Called whenever the mode-line is clicked. It is called with 4 arguments,
the mode-line, the button clicked, and the x and y of the pointer.")

(defvar *event-processing-hook* '()
  "A hook, called, when any event occurs")

(defvar *mode-line-position* :top
  "Specifies where the mode line is displayed. Valid values are :top and :bottom.")

(defvar *mode-line-border-width* 4
  "Specifies how thick the mode line's border will be. Integer value.")

(defvar *mode-line-pad-x* 5
  "Specifies the number of padding pixels between the text and the side of the mode line. Integer value.")

(defvar *mode-line-pad-y* 1
  "The number of padding pixels between the modeline text and the top/bottom of the modeline? Integer value.")

(defvar *mode-line-background-color* (make-color-hex "#335577")
  "The mode line background color.")

(defvar *mode-line-foreground-color* (make-color-hex "#DDEEEE")
  "The mode line foreground color.")

(defvar *mode-line-border-color* (make-color-hex "#443333")
  "The mode line border color.")

(defvar *hidden-window-color* "^6*"
  "Color command for hidden windows when using the
fmt-head-window-list-hidden-windows formatter. To disable coloring
hidden windows, set this to an empty string.")

(defvar *info-line-format* "%d[%g]")

(defvar *window-list-line-format* "%U%v")

(defvar *mode-line-format* nil
  "This variable describes what will be displayed on the modeline for each screen.
Turn it on with the function TOGGLE-MODE-LINE or the mode-line command.

It is a list where each element may be a string, a symbol, or a list.

For a symbol its value is used.

For a list of the form (:eval FORM) FORM is evaluated and the
result is used as a mode line element.

If it is a string the string is printed with the following formatting
options:

@table @asis
@item %a
List all windows on the current head of the current group using, including
urgent windows

@item %A
List all windows on the current head of the current group using, including
blinking urgent windows

@item %h
List the number of the head the mode-line belongs to

@item %w
List all windows in the current group windows using @var{*window-format*}

@item %W
List all windows on the current head of the current group using
@var{*window-format*}

@item %g
List the groups using @var{*group-format*}
@end table")

(defvar *mode-line-timeout* 1
  "The modeline updates after each command, when a new window appears or
an existing one disappears, and on a timer. This variable controls how
many seconds elapse between each update. If this variable is changed
while the modeline is visible, you must toggle the modeline to update
timer.")

(defvar *mode-line-timer* nil
  "The timer that updates the modeline
FIXME: do it around builtin timers")

(defvar *mode-line-blinker* nil
  "Variable for blink urgent windows, or widgets")

(defvar *mode-line-formatters* '((#\w fmt-window-list)
                                        (#\g fmt-group-list)
                                        (#\h fmt-head)
                                        (#\n fmt-group)
                                        (#\W fmt-head-window-list)
                                        (#\u fmt-urgent-window-list)
					(#\U fmt-blink-urgent-window-list)
                                        (#\v fmt-head-window-list-hidden-windows)
                                        (#\d fmt-modeline-time))
  "An alist containing format character format function pairs for
formatting screen mode-lines. functions are passed the screen's
current group.")

;; Data types and globals used by dswm

(defvar *display* nil
  "The display for the X server")

(defvar *shell-program* "/bin/sh"
  "The shell program used by @code{run-shell-command}.")

(defvar *terminal* "xterm"
  "Default terminal emulator")

(defvar *browser* "conkeror"
  "Default web browser")

(defvar *emacs* "emacs"
  "Default emacs implementation")

(defvar *maxsize-border-width* 1
  "The width in pixels given to the borders of windows with maxsize or ratio hints.")

(defvar *transient-border-width* 1
  "The width in pixels given to the borders of transient or pop-up windows.")

(defvar *normal-border-width* 2
  "The width in pixels given to the borders of regular windows.")

(defvar *text-color* (make-color-hex "#AAEEEE")
  "The color of message text.")

(defvar *menu-maximum-height* nil
  "Defines the maxium number of lines to display in the menu before enabling
   scrolling. If NIL scrolling is disabled.")

(defvar *menu-scrolling-step* 1
  "Number of lines to scroll when hitting the menu list limit.")

(defparameter +netwm-supported+
  '(:_NET_SUPPORTING_WM_CHECK
    :_NET_NUMBER_OF_DESKTOPS
    :_NET_DESKTOP_GEOMETRY
    :_NET_DESKTOP_VIEWPORT
    :_NET_CURRENT_DESKTOP
    :_NET_WM_WINDOW_TYPE
    :_NET_WM_STATE
    :_NET_WM_STATE_MODAL
    :_NET_WM_ALLOWED_ACTIONS
    :_NET_WM_STATE_FULLSCREEN
    :_NET_WM_STATE_HIDDEN
    :_NET_WM_STATE_DEMANDS_ATTENTION
    :_NET_WM_FULL_WINDOW_PLACEMENT
    :_NET_CLOSE_WINDOW
    :_NET_CLIENT_LIST
    :_NET_CLIENT_LIST_STACKING
    :_NET_ACTIVE_WINDOW
    :_NET_WM_DESKTOP
    :_KDE_NET_SYSTEM_TRAY_WINDOW_FOR)
  "Supported NETWM properties.
Window types are in +WINDOW-TYPES+.")

(defparameter +netwm-allowed-actions+
  '(:_NET_WM_ACTION_CHANGE_DESKTOP
    :_NET_WM_ACTION_FULLSCREEN
    :_NET_WM_ACTION_CLOSE)
  "Allowed NETWM actions for managed windows")

(defparameter +netwm-window-types+
  '(
    ;; (:_NET_WM_WINDOW_TYPE_DESKTOP . :desktop)
    (:_NET_WM_WINDOW_TYPE_DOCK . :dock)
    ;; (:_NET_WM_WINDOW_TYPE_TOOLBAR . :toolbar)
    ;; (:_NET_WM_WINDOW_TYPE_MENU . :menu)
    ;; (:_NET_WM_WINDOW_TYPE_UTILITY . :utility)
    ;; (:_NET_WM_WINDOW_TYPE_SPLASH . :splash)
    (:_NET_WM_WINDOW_TYPE_DIALOG . :dialog)
    (:_NET_WM_WINDOW_TYPE_NORMAL . :normal))
  "Alist mapping NETWM window types to keywords.
Include only those we are ready to support.")

;; Window states
(defconstant +withdrawn-state+ 0)
(defconstant +normal-state+ 1)
(defconstant +iconic-state+ 3)

(defvar *window-events* '(:structure-notify
                          :property-change
                          :colormap-change
                          :focus-change
                          :enter-window)
  "The events to listen for on managed windows.")

(defvar *window-parent-events* '(:substructure-notify
                                 :substructure-redirect)

  "The events to listen for on managed windows' parents.")

;; Message window variables
(defvar *message-window-padding* 5
  "The number of pixels that pad the text in the message window.")

(defvar *message-window-gravity* :top
  "This variable controls where the message window appears. The follow
are valid values.
@table @asis
@item :top-left
@item :top-right
@item :bottom-left
@item :bottom-right
@item :center
@end table")

(defvar *menu-window-gravity* :bottom-left
  "This variable controls where the message window appears. The follow
are valid values.
@table @asis
@item :top-left
@item :top-right
@item :bottom-left
@item :bottom-right
@item :center
@end table")

;; line editor
(defvar *editor-bindings* nil
  "A list of key-bindings for line editing.")

(defvar *input-window-gravity* :bottom-left
  "This variable controls where the input window appears. The follow
are valid values.
@table @asis
@item :top-left
@item :top-right
@item :bottom-left
@item :bottom-right
@item :center
@end table")

;; default values. use the set-* functions to these attributes
(defparameter +default-foreground-color+ (make-color-hex "#AAEEEE"))
(defparameter +default-background-color+ (make-color-hex "#113355"))
(defparameter +default-window-background-color+ (make-color-hex "#113355"))
(defparameter +default-border-color+ (make-color-hex "#443333"))
(defparameter +default-font-name+ "9x15bold")
(defparameter +default-focus-color+ (make-color-hex "#557799"))
(defparameter +default-unfocus-color+ (make-color-hex "#443333"))
(defparameter +default-frame-outline-width+ 2)
(defparameter +default-float-window-title-height+ 10)

;; Don't set these variables directly, use set-<var name> instead
(defvar *normal-gravity* :center)
(defvar *maxsize-gravity* :center)
(defvar *transient-gravity* :center)

(defvar *top-level-error-action* :abort
  "If an error is encountered at the top level, in
DSWM-INTERNAL-LOOP, then this variable decides what action
shall be taken. By default it will print a message to the screen
and to *standard-output*.

Valid values are :message, :break, :abort. :break will break to the
debugger. This can be problematic because if the user hit's a
mapped key the ENTIRE keyboard will be frozen and you will have
to login remotely to regain control. :abort quits dsmwm.")

(defvar *window-name-source* :title
  "This variable controls what is used for the window's name. The default is @code{:title}.

@table @code
@item :title
Use the window's title given to it by its owner.

@item :class
Use the window's resource class.

@item :resource-name
Use the window's resource name.
@end table")

(defvar *show-tip-of-the-day-p* t
  "Set, if needed to show tip of the day")

(defvar *scratchpad-group-name* "scratchpad"
  "Name of scratchpad group")

(defvar *scratchpad-group* nil
  "Working variable for scratchpad group")

(defvar *mode* '()
  "EXPERIMENTAL: Set enabled modes list
Available modes: session-transparent, interactive
session-transparent: set dswm behavior wich transparent throuth working
                     sessions. All configurations have to be reverted
                     after new logon
interactive:         set behavior, which will propose alternative, actions
                     instead errors, when you do something wrong")

(defstruct frame
  (number nil :type integer)
  x
  y
  width
  height
  (busy-p nil)
  window)

(defstruct ccontext
  win
  px
  gc
  default-fg
  default-bright
  default-bg)


(defvar *window-number-map* "0123456789"
  "Set this to a string to remap the window numbers to something more convenient.")

(defvar *group-number-map* "123456789"
  "Set this to a string to remap the group numbers to something more convenient.")

(defvar *frame-number-map* "123456789"
  "Set this to a string to remap the frame numbers to more convenient keys.
For instance,

\"hutenosa\"

would map frame 0 to 7 to be selectable by hitting the appropriate
homerow key on a dvorak keyboard. Currently, only single char keys are
supported. By default, the frame labels are the 36 (lower-case)
alphanumeric characters, starting with numbers 0-9.")

(defstruct modifiers
  (meta nil)
  (alt nil)
  (hyper nil)
  (super nil)
  (altgr nil)
  (numlock nil))

(defvar *all-modifiers* nil
  "A list of all keycodes that are considered modifiers")

(defvar *modifiers* nil
  "A mapping from modifier type to x11 modifier.")

(defvar *screen-list* '()
  "The list of screens managed by dswm.")

(defvar *initializing* nil
  "True when starting dswm. Use this variable in your rc file to
run code that should only be executed once, when dswm starts up and
loads the rc file.")

(defvar *processing-existing-windows* nil
  "True when processing pre-existing windows at startup.")

(defvar *executing-dswm-command* nil
  "True when executing external commands.")

(defvar *interactivep* nil
  "True when a defcommand is executed from colon or a keybinding")
 
;;; The restarts menu macro

;; Misc. utility functions

(defvar *debug-level* 0
  "Set this variable to a number > 0 to turn on debugging. The greater the number the more debugging output.")

(defvar *debug-expose-events* nil
  "Set this variable for a visual indication of expose events on internal DSWM windows.")

(defvar *debug-stream* *error-output*
  "This is the stream debugging output is sent to. It defaults to
*error-output*. It may be more convenient for you to pipe debugging
output directly to a file.")

(defun dformat (level fmt &rest args)
  (when (>= *debug-level* level)
    (multiple-value-bind (sec m h) (decode-universal-time (get-universal-time))
      (format *debug-stream* "~2,'0d:~2,'0d:~2,'0d " h m sec))
    ;; strip out non base-char chars quick-n-dirty like
    (write-string (map 'string (lambda (ch)
                                 (if (typep ch 'standard-char)
                                     ch #\?))
                       (apply 'format nil fmt args))
                  *debug-stream*)
    (force-output *debug-stream*)))

(defvar *redirect-stream* nil
  "This variable Keeps track of the stream all output is sent to when
`redirect-all-output' is called so if it changes we can close it
before reopening.")

(defun redirect-all-output (file)
  "Elect to redirect all output to the specified file. For instance,
if you want everything to go to ~/dswm.d/debug-output.txt you would
do:

@example
 (redirect-all-output (data-dir-file \"debug-output\" \"txt\"))
@end example
"
  (when (typep *redirect-stream* 'file-stream)
    (close *redirect-stream*))
  (setf *redirect-stream* (open file :direction :output :if-exists :append :if-does-not-exist :create)
        *error-output*    *redirect-stream*
        *standard-output* *redirect-stream*
        *trace-output*    *redirect-stream*
        *debug-stream*    *redirect-stream*))

;;; 
;;; formatting routines

(defvar *window-formatters* '((#\n window-map-number)
                              (#\s fmt-window-status)
                              (#\t window-name)
                              (#\c window-class)
                              (#\i window-res)
                              (#\r window-role)
                              (#\m fmt-window-marked)
                              (#\h window-height)
                              (#\w window-width)
                              (#\g gravity-for-window))
  "an alist containing format character format function pairs for formatting window lists.")

(defvar *window-format* "%m%n%s%15t..."
  "This variable decides how the window list is formatted. It is a string
with the following formatting options:

@table @asis
@item %n
Substitutes the windows number translated via *window-number-map*, if there
are more windows than *window-number-map* then will use the window-number.
@item %s
Substitute the window's status. * means current window, + means last
window, and - means any other window.
@item %t
Substitute the window's name.
@item %c
Substitute the window's class.
@item %i
Substitute the window's resource ID.
@item %m
Draw a # if the window is marked.
@end table

Note, a prefix number can be used to crop the argument to a specified
size. For instance, @samp{%20t} crops the window's title to 20
characters.")

(defvar *window-info-format* (format nil "Size~15t:~t%wx%h~%Window number~15t:~t%n~%Title~15t:~t%t)")
  "The format used in the info command. @xref{*window-format*} for formatting details.")

(defvar *group-formatters* '((#\n group-map-number)
                             (#\s fmt-group-status)
                             (#\t group-name))
  "An alist of characters and formatter functions. The character can be
used as a format character in @var{*group-format*}. When the character
is encountered in the string, the corresponding function is called
with a group as an argument. The functions return value is inserted
into the string. If the return value isn't a string it is converted to
one using @code{prin1-to-string}.")

(defvar *group-format* "%n%s%t"
  "The format string that decides what information will show up in the
group listing. The following format options are available:

@table @asis
@item %n
The group's number.

@item %s
The group's status. Similar to a window's status.

@item %t
The group's name.
@end table")

(defvar *list-hidden-groups* nil
  "Controls whether hidden groups are displayed by 'groups' and 'vgroups' commands")

(defvar *x-selection* nil
  "This holds dswm's current selection. It is generally set
when killing text in the input bar.")

(defvar *last-command* nil
  "Set to the last interactive command run.")

(defvar *commands-history* nil
  "History of executed commands")

(defvar *input-commands-history* nil
  "History of any input through input line
or keybindings commands")

(defvar *programs-history* nil
  "History of any input through input line
or keybindings commands")

(defvar *max-last-message-size* 20
  "how many previous messages to keep.")

(defvar *record-last-msg-override* nil
  "assign this to T and messages won't be recorded. It is
recommended this is assigned using LET.")

(defvar *suppress-echo-timeout* nil
  "Assign this T and messages will not time out. It is recommended this is assigned using LET.")

(defvar *ignore-echo-timeout* nil
  "Assign this T and the message time out won't be touched. It is recommended this is assigned using LET.")

(defvar *run-or-raise-all-groups* t
  "When this is @code{T} the @code{run-or-raise} function searches all groups for a
running instance. Set it to NIL to search only the current group.")

(defvar *run-or-raise-all-screens* nil
  "When this is @code{T} the @code{run-or-raise} function searches all screens for a
running instance. Set it to @code{NIL} to search only the current screen. If
@var{*run-or-raise-all-groups*} is @code{NIL} this variable has no effect.")

(defvar *deny-map-request* nil
  "A list of window properties that dswm should deny matching windows'
requests to become mapped for the first time.")

(defvar *deny-raise-request* nil
  "Exactly the same as @var{*deny-map-request*} but for raise requests.

Note that no denial message is displayed if the window is already visible.")

(defvar *suppress-deny-messages* nil
  "For complete focus on the task at hand, set this to @code{T} and no
raise/map denial messages will be seen.")

(defvar *honor-window-moves* t
  "Allow windows to move between frames.")

(defvar *resize-hides-windows* nil
  "Set to T to hide windows during interactive resize")

(defvar *startup-only-code* nil
  "Set code, which run only on startup")

(defvar *min-frame-width* 50
  "The minimum width a frame can be. A frame will not shrink below this
width. Splitting will not affect frames if the new frame widths are
less than this value.")

(defvar *min-frame-height* 50
  "The minimum height a frame can be. A frame will not shrink below this
height. Splitting will not affect frames if the new frame heights are
less than this value.")

(defvar *new-frame-action* :last-window
  "When a new frame is created, this variable controls what is put in the
new frame. Valid values are

@table @code
@item :empty
The frame is left empty

@item :last-window
The last focused window that is not currently visible is placed in the
frame. This is the default.
@end table")

(defvar *new-window-preferred-frame* '(:focused)
  "This variable controls what frame a new window appears in. It is a
list of preferences. The first preference that is satisfied is
used. Valid list elements are as follows:

@table @code
@item :focused
Choose the focused frame.

@item :last
Choose the last focused frame.

@item :empty
Choose any empty frame.

@item :unfocused
Choose any unfocused frame.
@end table

Alternatively, it can be set to a function that takes one argument, the new
window, and returns the preferred frame or a list of the above preferences.")

(defvar *default-package* (find-package '#:dswm-user)
  "This is the package eval reads and executes in. You might want to set
this to @code{:dswm} if you find yourself using a lot of internal
dswm symbols. Setting this variable anywhere but in your rc file
will have no effect.")

(defvar *window-placement-rules* '()
  "List of rules governing window placement. Use define-frame-preference to
add rules")

(defvar *mouse-focus-policy* :click
  "The mouse focus policy decides how the mouse affects input
focus. Possible values are :ignore, :sloppy, and :click. :ignore means
dswm ignores the mouse. :sloppy means input focus follows the
mouse; the window that the mouse is in gets the focus. :click means
input focus is transfered to the window you click on.")

(defvar *root-click-focuses-frame* t
  "Set to NIL if you don't want clicking the root window to focus the frame
  containing the pointer when *mouse-focus-policy* is :click.")

(defvar *banish-pointer-to* :head
  "Where to put the pointer when no argument is given to (banish-pointer) or the banish
  command. May be one of :screen :head :frame or :window")

(defvar *xwin-to-window* (make-hash-table)
  "Hash table for looking up windows quickly.")

(defvar *resize-map* nil
  "The keymap used for resizing a window")

(defvar *default-group-name* "Main"
  "The name of the default group.")

(defvar *last-unhandled-error* nil
  "If an unrecoverable error occurs, this variable will contain the
  condition and the backtrace.")

(defvar *show-command-backtrace* nil
  "When this is T a backtrace is displayed with errors that occurred
within an interactive call to a command.")

(defvar *window-border-style* :thick
  "This controls the appearance of the border around windows. valid
values are:
@table @var
@item :thick
All space within the frame not used by the window is dedicated to the
border.

@item :thin
Only the border width as controlled by *maxsize-border-width*
*normal-border-width* and *transient-border-width* is used as the
border. The rest is filled with the unfocus color.

@item :tight
The same as :thin but the border surrounds the window and the wasted
space within the frame is not obscured, revealing the background.

@item :none
Like :tight but no border is ever visible.
@end table

After changing this variable you may need to call
sync-all-frame-windows to see the change.")

(defvar *data-dir* nil
  "Set default data directory")

(defun data-dir (&optional subdir)
  (let ((directory
	 (if (not (null *data-dir*))
	     (make-pathname :directory (concat *data-dir* "/" subdir))
	   (make-pathname
	    :directory (append
			(pathname-directory (user-homedir-pathname))
			(list (concat ".dswm.d"	"/" subdir)))))))
    directory))
;;
;; OLD code. Remove it after changes
;;   \/
;; (when (ensure-directories-exist directory)
;;   directory)))

(defun data-dir-file (name &optional type subdir)
  "Return a pathname inside dswm's data dir with the specified name and type"
  (if (not (null type))
      (make-pathname :name name :type type :defaults (data-dir subdir))
    (make-pathname :name name :defaults (data-dir subdir))))

(defun module-data-dir-file (module name &optional type subdir)
  "Return a pathname inside dswm's module data dir with the specified name and type"
  (let ((dir-path (concat (princ-to-string (data-dir)) "/modules.d/" module subdir))) 
    (if (not (null type))
	(make-pathname :name name :type type :defaults dir-path)
      (make-pathname :name name :defaults dir-path))))

(defmacro with-data-file ((s file &rest keys &key (if-exists :supersede) &allow-other-keys) type subdir &body body)
  "Open a file in DSWM's data directory. keyword arguments are sent
directly to OPEN. Note that IF-EXISTS defaults to :supersede, instead
of :error."
  (declare (ignorable if-exists))
  `(with-open-file (,s ,(data-dir-file file type subdir)
		       ,@keys) ,@body))

;; Names of dump files
(defvar *desktop-dump-file* (data-dir-file "desktop" "rules" "rules.d")
  "Default filename for dump group placement rules")

(defvar *window-placement-dump-file* (data-dir-file "window-placement" "rules" "rules.d")
  "Default filename for dump window placement rules")

(defvar *default-window-name* "Unnamed"
  "The name given to a window that does not supply its own name.")

(defvar *keys* nil
  "Defines list of keynames and related keysyms to it")

(defvar *loaded-modules-list* nil
  "Defines list of already loaded modules")
