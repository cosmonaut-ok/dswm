;;============================================================
;;
;; dokuwiki.el
;;
;; Letzte Änderung: Time-stamp: <2007-11-05 20:34:47 jv>
;; Version:         0.1
;;
;; Einlesen und Bearbeiten von DokuWiki-Einträgen per EMACS.
;;
;; ------------------------------------------------------------
;;
;; Vorgehen:
;;
;; Parameter der Wiki setzen (dokuwiki-username, dokuwiki-password,
;; dokuwiki-base-url). Dann mit dokuwiki-get-page die Seite anfordern.
;; Bestehende Seiten werden per Completion zur Verfügung gestellt, um
;; eine neue Seite zu erstellen, einfach Namen (evtl. mit Namespace)
;; angeben.  Dann befindet man sich in einem Buffer, der entweder leer
;; ist oder den Rohtext der Wikiseite enthält. Dank einer angepassten
;; Version des `simple-wiki-mode' (der dann `simple-dokuwiki-mode`
;; heisst) stehen in diesem Buffer diverse Befehle zur einfachen
;; Formatierung und Eingabe der Tags zur Verfügung. Wichtige
;; Syntaxelemente wie Hervorhebung und Unterstreichen werden
;; angezeigt, Überschriften nach ihrer Priorität verschieden groß
;; dargestellt. Nach dem Editieren der Seite schließlich C-c C-c
;; drücken, damit wird die Seite zurückgeschrieben. Fertig.
;;
;; Die Funktion `dokuwiki-new-blog' kreiiert einen eindeutigen Dateinamen
;; (Tag-Stunde-Monat-Jahr, also eindeutig pro Stunde) im Namespace "blog:".
;; Die neu erstellte Seite wird mit einer rudimentären Schablone gefüllt.
;;
;; ------------------------------------------------------------
;;
;; History:
;;
;; - 18.3.2007 (hoffentlich) longlines nun richtig gehandhabt
;; - 12.3.2007 aufgeräumt
;; - 10.3.2007 Upload funktioniert nun auch; einige Namen von Funktionen und Variablen vereinheitlicht
;; -  9.3.2007 Medien-Tags einfügen, mit media-index
;; -  8.3.2007 Beim Speichern wird nun ein summary erfragt.
;;
;;
;; ------------------------------------------------------------
;; Bekannte Einschränkungen:
;;
;; - Das Syntax Highlighting ist sehr rudimentär.
;;
;; - Die Auswahl der Seiten über Completion ist etwas umständlich,
;;   wenn es darum geht, in ein Namespace "hineinzuwechseln". Hier
;;   schauen, wie find-file es mit den Verzeichnissen macht.
;;
;; - Fehler werden nur grob abgefangen.
;;
;; - Zur Zeit gibt es keine Möglichkeit, mehrere Wikis zu definieren und
;;   dann die Zielwiki auszusuchen.
;; 
;; - Eventuell sollte die Abhängigkeit vom `simple-wiki-mode'
;;   aufgelöst werden, so schwer ist es ja auch nicht, einen eigenen
;;   Major-Mode zu definieren.
;;
;; - Einige Vorgehensweisen sind recht unelegant, z.B. `dokuwiki-newpage-entered-p'
;;
;; Vorschläge für weitere Ergänzungen:
;;
;; - Die Trennung von pagename und namespace wird nur ein einziges Mal
;;   wirklich verwendet, nämlich in `dokuwiki-new-blog'. Besser, dass
;;   pagename wie eine Pfadangabe behandelt wird. Dann zwei drei
;;   convenience-functions zur Verfügung stellen, die etwa testen, ob
;;   ein Doppelpunkt am Anfang oder am Ende steht, die einen Namen
;;   "ranhängen" und dabei evtl. einen Doppelpunkt zwischenfügen, etc.
;;
;; - Follow-link-at-point (siehe dazu oddmuse.el)
;;

(require 'cl)
(require 'simple-wiki)
(require 'skeleton)

;; ============================================================
;; Globale Variablen:
;;

(defvar dokuwiki-username "")
(defvar dokuwiki-password "")
(defvar dokuwiki-base-url ""
  "URL to the dokuwiki, i.e. www.myweb.com")

(defvar dokuwiki-curl-output-buffer-name "*CURL Output*")

(defvar dokuwiki-index-re ""
  "Regular Expression which matches the equivalent to a directory
  when retrieving the index via do=index.")

(setq dokuwiki-index-re "idx=\\([^ ]+\\)\"")

(defvar dokuwiki-page-re ""
  "Regular expression that matches a page name when retrieving
  the index via do=index.")

(setq dokuwiki-page-re "id=\\([^ \"&]+\\)")

(defvar dokuwiki-mediafile-re ""
  "Regular expression that matches a media link within a wiki page.
Used to filter information from mediamanager.php")

(setq dokuwiki-mediafile-re "{{:\\([^}\n]+\\)}}")

(defvar dokuwiki-newpage-entered-p nil
	"t, when function `dokuwiki-enter-pagename' will ask for new, not yet known page.
It's set from  `dokuwiki-enter-pagename' function.")

(defvar dokuwiki-pagename "newpage"
  "Page name at DokuWiki-Buffer. (buffer-local)")

(defvar dokuwiki-cookie-jar "~/.curl-cookies")


;; ============================================================
;; Templates

(define-skeleton dokuwiki-new-blog-entry
  "Begin a new blog entry."
  "Blog heading: "
	\n "====== " str " ======" \n \n _ \n \n 
	"~~DISCUSSION:off~~" \n
	)

(define-skeleton dokuwiki-new-page
  "Generic Template for new DokuWiki-Page."
  "Page Heading: "
  \n "====== " str " ======" \n \n _ \n \n
  "~~DISCUSSION~~" \n
	)

(define-skeleton dokuwiki-new-code-block
  "Begin a new code block."
  "Language: "
  \n "<code " str ">" \n \n _ \n "</code>" \n
	)

;; ============================================================
;; Eigenen Major-Mode definieren für Fontlock etc.
;; 

(simple-wiki-define-major-mode
 'dokuwiki
 "DokuWiki"
 "Simple mode to edit doku wiki pages."
 ;;
 ;; TODO:
 ;;
 ;; - Kommentare (/* ... */)
 ;; - Aufzählungen
 ;;
 :camelcase 'none
 ;;
 :smilies 'none
 ;;
 :headlines '(("^=\\{6\\}\\([^\n=]+\\)=\\{6\\}\\([^=]\\|$\\)" . 1)
							("^=\\{5\\}\\([^\n=]+\\)=\\{5\\}\\([^=]\\|$\\)" . 1)
							("^=\\{4\\}\\([^\n=]+\\)=\\{4\\}\\([^=]\\|$\\)" . 1)
							("^=\\{3\\}\\([^\n=]+\\)=\\{3\\}\\([^=]\\|$\\)" . 1)
							("^=\\{2\\}\\([^\n=]+\\)=\\{2\\}\\([^=]\\|$\\)" . 1)
							("^=\\([^\n=]+\\)=[^=]" . 1))

 ;;
 :free-link 'none ;; '("\\[\\[\\([^|\n]+?\\)\\]\\]" . 1)
 ;;
 :strong-strings    '("**" . "**")
 :strong-em-strings '("**//" . "//**")
 ;;
 :em-strings     '("//" . "//")
 :em-patterns    '(("\\(\\W\\|^\\)//\\([^/]\\|[^/]/\\)*//" . 0)    ; emphasized
									 ("\\(\\W\\|^\\)\\*\\*\\([^*]\\|[^*]\\*\\)*\\*\\*" . 0)  ; strong
									 ("\\(\\W\\|^\\)\\*\\*//\\([^\]\\|[^\]\\\)*//\\*\\*" . 0)) ; strong emph

 ;;
 :keywords '(
						 ("\\(\\W\\|^\\)''\\([^_\n]+?\\)''" 2 'simple-wiki-teletype-face append)
						 ("\\(\\W\\|^\\)FIXME:" . font-lock-warning-face) ;; TODO: Bessere face definieren
						 ;; __unterstrichen__
						 ("\\(\\W\\|^\\)__\\([^_\n]+?\\)__" 2 'simple-wiki-underline-face)
						 ;; [[link|mit beschreibung]]
						 ("\\[\\[\\([^\n]+?\\)\\(\\]\\]\\|\|\\([^\n]+?\\)\\]\\]\\)"
							(1 'font-lock-function-name-face)
							(3 'font-lock-doc-face))
						 ;;
						 ;; tags wie <i> etc. (unnötig, übernommen aus dem alten mode)
						 (simple-wiki-match-tag-i 0 'simple-wiki-italic-face append)
						 (simple-wiki-match-tag-b 0 'simple-wiki-bold-face append)
						 (simple-wiki-match-tag-u 0 'simple-wiki-underline-face append)
						 (simple-wiki-match-tag-tt 0 'simple-wiki-teletype-face append)
						 (simple-wiki-match-tag-em 0 'simple-wiki-emph-face append)
						 (simple-wiki-match-tag-strong 0 'simple-wiki-strong-face append)
						 ("\\(</?\\)\\([A-Za-z]+\\)\\(\\([ 	]+[a-zA-Z]+\\)=\\(\".*\"\\)\\)*\\(/?>\\)?"
							(1 'default t t)
							(2 'font-lock-function-name-face t t)
							(4 'font-lock-variable-name-face t t)
							(5 'font-lock-string-face t t)
							(6 'default t t))
						 ;; hervorhebung des bereiches zwischen tags
						 ;; 	    (simple-wiki-match-tag-nowiki 0 'simple-wiki-nowiki-face t)
						 ;; 	    (simple-wiki-match-tag-pre 0 'simple-wiki-code-face t)
						 ;; 	    (simple-wiki-match-tag-code 0 'simple-wiki-code-face t)
						 ;; 	    (simple-wiki-match-code-block 0 'simple-wiki-code-face t)
						 )

 :indent 'none
 ;;
 :outline 'none
 ;;
 :horiz  '("----" . 0)
 ;;
 )

(add-hook 'simple-dokuwiki-mode-hook 'simple-dokuwiki-init-mode)

(defun simple-dokuwiki-init-mode ()
  (interactive)
  (make-local-variable 'dokuwiki-pagename)
  (set (make-local-variable 'dokuwiki-major-change) t)
  (set (make-local-variable 'global-mode-string) '(:eval 
																									 (progn 		      
																										 (rename-buffer dokuwiki-pagename)
																										 (if dokuwiki-major-change "*Major Change*" "*Minor Change*")
																										 )))
  ;;
  ;;  (auto-fill-mode 1)
  (longlines-mode 1)
  ;;
  ;;Keymap, sollte in die Definition des Major Mode, aber das lässt
  ;; das Makro simple-wiki-define-major-mode nicht zu:
  ;; 
  (define-key simple-dokuwiki-mode-map [(control c)(control c)]  'dokuwiki-save-page)
  (define-key simple-dokuwiki-mode-map [(control c)(control h)]  'simple-dokuwiki-insert-or-region-headline)  
  (define-key simple-dokuwiki-mode-map [(control c)(control i)]  'simple-dokuwiki-insert-internal-link)
  (define-key simple-dokuwiki-mode-map [(control c)(control m)]  'simple-dokuwiki-insert-or-region-media-string)
  (define-key simple-dokuwiki-mode-map [(control c)(m)]          'simple-dokuwiki-toggle-major-change)
  (define-key simple-dokuwiki-mode-map [(control c)(control u)]  'dokuwiki-upload-file)
  )

;; Diverses:

(defun simple-dokuwiki-set-pagename (string)
  (interactive "sNew name for this page: ")
  (when (memq 'dokuwiki-pagename (buffer-local-variables))
    (setq dokuwiki-pagename string)))

;; (dokuwiki-get-page "testseite")
;; (call-interactively 'dokuwiki-get-page)

(defun simple-dokuwiki-toggle-major-change ()
  "Toggles whether current edit is a major change."
  (interactive)
	(set 'dokuwiki-major-change (not dokuwiki-major-change))
	(redraw-modeline))

;; Headline:
;;

(defun dokuwiki-create-headline-string (level)
  "Erstellt einen String mit 7-LEVEL Gleichheitszeichen."
  (when (and (numberp level) (not (= level 0)) (< level 7))
    (make-string (- 7 level) ?=)))

(defun simple-dokuwiki-insert-headline (level)
  "Insert a headline at point and put the cursor between the opening and closing marks."
  (interactive "nHeader level from 1 (highest level) to 6: ")
  (simple-dokuwiki-insert-headline-string level)
  (save-excursion (simple-dokuwiki-insert-headline-string level t)))

(defun simple-dokuwiki-insert-or-region-headline (level)
  "Insert headline.
If in `transient-mark-mode' and the region is active markup the region
as headline."
  (interactive "nHeader level from 1 (highest level) to 6: ")
  (if (simple-wiki-active-mark)
      (let ((beg (min (point) (mark))) (end (max (point) (mark))))
        (simple-dokuwiki-headline-region beg end level))
    (simple-dokuwiki-insert-headline level)))

(defun simple-dokuwiki-headline-region (beg end level)
  "Fügt um die aktuell markierte Region herum Headlines ein."
  (interactive "r\nnHeader level from 1 (highest level) to 6: ")
  (save-excursion
    (goto-char beg)
    (simple-dokuwiki-insert-headline-string level)
    (goto-char (+ end 1 (length (dokuwiki-create-headline-string level))))
    (simple-dokuwiki-insert-headline-string level t)))

(defun simple-dokuwiki-insert-headline-string (level &optional closing)
  "Insert an opening or closing headline-marker"
  (let ((headline (dokuwiki-create-headline-string level)))
    (when (and headline (not (string= headline "")))
      (if closing (insert "  "))
      (insert headline)
      (if (not closing) (insert " "))
      )))

;; Links einfügen:

(defun simple-dokuwiki-extract-region-as-string ()
  (let ((s  (buffer-substring-no-properties (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    s))

(defun simple-dokuwiki-insert-internal-link (pagename &optional description)
  "Insert interactively chosen link ([[page|description]]) at point. 
If mark is active, use region as description."
  (interactive (list 
								(if current-prefix-arg
										(read-from-minibuffer "Web site (full path): ")
									(dokuwiki-enter-pagename))
								;;
								(if (simple-wiki-active-mark)
										(simple-dokuwiki-extract-region-as-string)
									(read-from-minibuffer "Description: "))))
  (insert (if description
							(format "[[%s|%s]]" pagename description)
						(format "[[%s]]" pagename))))

;; (call-interactively 'simple-dokuwiki-insert-internal-link)

;; Medien verlinken:


(defun simple-dokuwiki-media-string-region (begin end &optional medium-or-namespace)
  "Insert medialink at point, where the text in the current region will serve as a description."
  (let* ((description (buffer-substring-no-properties begin end))
				 (the-markup-string (dokuwiki-get-media-string medium-or-namespace description)))
    (delete-region begin end)
    (insert the-markup-string)))

(defun simple-dokuwiki-insert-media-string (&optional medium-or-namespace)
  "Insert medialink at point."
  (insert (dokuwiki-get-media-string medium-or-namespace)))

(defun simple-dokuwiki-insert-or-region-media-string ()
  "Insert media-link, if in region, use current mark as description."
  (interactive)
  (if (simple-wiki-active-mark)
      (let ((begin (min (point) (mark))) (end (max (point) (mark))))
        (simple-dokuwiki-media-string-region begin end))
    (simple-dokuwiki-insert-media-string)))


;; ============================================================
;; Hilfsfunktionen:
;; 

(defun dokuwiki-base-url ()
  "Return the URL to the dokuwiki."
  (format "http://%s/doku.php" dokuwiki-base-url))

(defun dokuwiki-mediamanager-url ()
  "Return the URL to the mediamanager.php of the dokuwiki."
  (format "http://%s/lib/exe/mediamanager.php" dokuwiki-base-url))

(defun dokuwiki-convert-hex-to-utf (string)
	"Convert string with hex encoded utf-8 text into string"
	(decode-coding-string
	 (string-as-unibyte
		(replace-regexp-in-string
		 "%[A-F0-9][A-F0-9]" (lambda (x)
													 (char-to-string
														(make-char 'eight-bit
																			 (string-to-number (substring x 1 3) 16))))
		 string))
	 'utf-8))

(defun dokuwiki-get-page-id (pagename &optional namespace)
  "Construct page ID to request the page via HTTP_GET."
  ;; jaja, das ist etwas mit kanonen auf spatzen geschossen....
	(dokuwiki-convert-hex-to-utf (replace-regexp-in-string
											 "::" ":" 
											 (format "%s:%s" (or namespace "") pagename))))

(defun read-file-into-string (file)
  "Return FILE as a string."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun x=y (a b)
  "Format A and B as to be form parameters for CURL:
\"--form-string A=B\"."
  (format "--form-string \"%s=%s\"" a b))

(defun cookieoption ()
  "Return CURL-Option for using a file as a cookie jar." ;; mein Englisch. Seufz.
  (format "--cookie-jar %s" dokuwiki-cookie-jar))

(defun dokuwiki-propose-blog-entry ()
  "Return (relatively) unique name for a new blog entry."
  (format-time-string "%d-%H-%m-%y"))

(global-set-key [(control v)(b)] 'dokuwiki-new-blog)

(defun dokuwiki-new-blog (pagename)
  "Suggest and create a new dokuwiki-page in the namesection \"blog\"."
  (interactive (list (read-from-minibuffer "Page name for blog entry: " 
																					 (dokuwiki-propose-blog-entry))))
  (setq dokuwiki-newpage-entered-p t)
  (dokuwiki-get-page pagename "blog") ;; TODO: Error checking
  ;; TODO: Wie kriege ich das mit dem Skeleton hin, da hier doch ein 
  ;; Argument übergeben wird?
  (simple-dokuwiki-insert-headline 1)
  (save-excursion 
    (goto-char (point-max))
    (insert "\n\n~~DISCUSSION~~\n")))

(defun dokuwiki-visit-as-dokuwiki-buffer (buffer pagename namespace)
  "Visit BUFFER, changing it's major mode to `simple-dokuwiki-mode' and 
initialising some buffer-local variables, i.e. `dokuwiki-pagename'."
  (switch-to-buffer buffer)
  (text-mode) ;; seltsamerweise nötig, damit fill-mode richtig klappt.
  (simple-dokuwiki-mode)
  (setq dokuwiki-pagename (dokuwiki-get-page-id pagename namespace)))


;; ============================================================
;; Hauptfunktionalität:
;; 

;; HTTP-Requests über CURL:

(defun dokuwiki-call-curl (url &rest options)
	"Calls CURL with URL and optional OPTIONS. Returns EXIT-STATUS or
the OUTPUT-BUFFER if successful."
  (let* (exit-status
				 (coding-system-for-read  'utf-8)
				 (coding-system-for-write 'utf-8)
				 (cookie-jar    (list (cookieoption)))	 ;; TODO: Unklar, ob gebraucht.
				 (curl-options  (nconc options cookie-jar (list (shell-quote-argument url))))
				 (curl-program  "curl")
				 (output-buffer (get-buffer-create dokuwiki-curl-output-buffer-name))
				 (stderr-file   (make-temp-file "curl_stderr")))   
		;;    (message (mapconcat 'print curl-options " "))
    (with-current-buffer output-buffer (erase-buffer))
    (setq exit-status 
					(apply 'call-process-shell-command
								 curl-program
								 nil
								 (list output-buffer stderr-file)
								 nil
								 curl-options))
    (cond
     ((eq exit-status 0) output-buffer)
     (t (progn
					(with-output-to-temp-buffer "*CURL Error*"
						(princ (read-file-into-string stderr-file))
						(delete-file stderr-file))
					exit-status)))))

;; (dokuwiki-call-curl "http://localhost")

;; LOGIN/LOGOUT:

;; DEPRECATED; seems to work w/o login/logout.
;; (@see dokuwiki-save-region)

(defun dokuwiki-login (&optional username password)
  (interactive)
  (let* (success 
				 (url          (format "%s?do=login" (dokuwiki-base-url)))
				 ;;
				 (u            (x=y "u"   (or username dokuwiki-username)))
				 (p            (x=y "p"   (or password dokuwiki-password)))
				 (r            (x=y "r"  1))
				 ;;
				 (result 
					(with-temp-message (format "Logging as user %s..." username)
						(dokuwiki-call-curl url
																u
																p
																r))))
    ;;
    (if (not (setq success (bufferp result)))
				(message "Cann't login.")
      (kill-buffer result))
    ;;
    success))

;; DEPRECATED
(defun dokuwiki-logout ()
  (interactive)
  (let* ((url          (format "%s?do=logout" (dokuwiki-base-url)))
				 ;;
				 (result 
					(with-temp-message "Logout..."
						(dokuwiki-call-curl url))))
    (when (bufferp result) (kill-buffer result))))


;; LADEN:

(global-set-key [(control v)(control d)] 'dokuwiki-get-page)

(defun dokuwiki-get-page (pagename &optional namespace)
  "Get page NAMESPACE:PAGENAME via curl.

When called interactively, asks for pagename via `dokuwiki-enter-pagename'.

Returns BUFFER or EXIT-CODE if not 0.
"
  (interactive (list (dokuwiki-enter-pagename)))
  ;;
  (if dokuwiki-newpage-entered-p
      (dokuwiki-visit-as-dokuwiki-buffer (get-buffer-create "NEW") pagename namespace)
		(let* ((dokuwiki-do-command  "export_raw")
					 (url          (format 
													"%s?id=%s&do=%s"
													(dokuwiki-base-url) 
													(dokuwiki-get-page-id pagename namespace)
													dokuwiki-do-command))
					 (result  (dokuwiki-call-curl url)))
			;;
			(cond
			 ;; Bei Erfolg zum Buffer wechseln:
			 ;;
			 ((bufferp result) 
				(dokuwiki-visit-as-dokuwiki-buffer result pagename namespace)
				)
			 ;; Fehler
			 ;;
			 (t (message "Error %d." result))))))

;; (dokuwiki-get-page "testpage") 
;; (call-interactively 'dokuwiki-get-page)


;; INDEX-SEITE HOLEN:

(defun dokuwiki-get-index (&optional namespace)
  (let* ((url (format "%s?do=index%s"
											(dokuwiki-base-url)
											(if namespace
													(concat "&idx=" namespace)
												"")))
				 (result (dokuwiki-call-curl url)))
    ;;
    (if (not (bufferp result))
				(message "Error %s." result)
      result)))

;; (dokuwiki-get-index "philo")

;; SPEICHERN:

(defun dokuwiki-write-region-to-temp-file (begin end)
  "Write REGION to a temporary file. Returns filename. Used by
`dokuwiki-save-region'."
  (let ((tempfile (make-temp-file "wikipage"))
				(longlines-mode-set-p longlines-mode)
				(coding-system-for-read  'utf-8)
				(coding-system-for-write 'utf-8))
    (save-excursion
      (when longlines-mode-set-p
				(longlines-mode 0))
      (write-region begin end tempfile)
      (when longlines-mode-set-p
				(longlines-mode 1)))
    tempfile))

(defun dokuwiki-save-region (point mark pagename &optional namespace)
  "Save region to dokuwiki-page PAGENAME (optionally specify NAMESPACE)."
  (interactive "r")
  (let*  ((url      (format "%s?do=save"  (dokuwiki-base-url)))
					;;
					(summary-string  (if dokuwiki-major-change 
															 (read-from-minibuffer "Description of this change: ")
														 "(minor change)"))
					(tempfile        (dokuwiki-write-region-to-temp-file point mark))
					(pagestring      (dokuwiki-get-page-id pagename namespace))
					;;
					(o-user        (x=y "u"   dokuwiki-username))
					(o-pwd         (x=y "p"   dokuwiki-password))
					(o-r           (x=y "r"   "1"))
					;;
					(o-id       (x=y "id"       pagestring))
					;;	  (o-rev      (x=y "rev"      ""))
					(o-summary  (x=y "summary"  summary-string))
					(o-minor    (x=y "minor"   (not dokuwiki-major-change)))
					;;
					(o-wikitext (format "-F \"wikitext=<%s\"" tempfile))
					;;
					(result   
					 (with-temp-message (format "Saving to page %s..." pagestring)
						 (dokuwiki-call-curl url
																 o-id
																 o-summary 
																 o-minor
																 o-user 
																 o-pwd 
																 o-r
																 o-wikitext))))
    (delete-file tempfile)
    (cond
     ((bufferp result) 
      (progn
				(message "Page %s was saved." (dokuwiki-get-page-id pagename namespace))
				(kill-buffer result)
				))
     (t (message "Error %d." result)))))

(defun dokuwiki-save-current-buffer (pagename &optional namespace)
  "Save current buffer (regardless of its mode) as dokuwiki-page
PAGENAME. Do not use this function. Use `dokuwiki-save-page'."
  (save-excursion
    (dokuwiki-save-region (point-min) (point-max) pagename namespace)
    (set-buffer-modified-p nil)))

(defun dokuwiki-save-page ()
  "Save current buffer to the DokuWiki."
  (interactive)
  (let ((buffer (current-buffer)))
    (if (not (eq major-mode 'simple-dokuwiki-mode))
				(message "Must be in DokuWiki-Mode.")
      (dokuwiki-save-current-buffer dokuwiki-pagename))))

;; (dokuwiki-get-page "testseite")

;; UPLOAD:

(defun dokuwiki-make-wiki-filename (filename)
	"Convert FILENAME (no spaces, no extra chars etc.)."
	(let* ((file (file-name-nondirectory filename))
				 (substitution-list '((" " "_")  ;; erstes Element ist eine RegExp!
															("'" "_")
															("ä" "ae")
															("ö" "oe")
															("ü" "ue")
															("Ä" "Ae")
															("Ö" "Oe")
															("Ü" "Ue")))
				 (substitution-function (lambda (item)
																	(setq file
																				(replace-regexp-in-string 
																				 (car item)
																				 (cadr item)
																				 file)))))
		(mapc substitution-function substitution-list)
		file))

;; (dokuwiki-make-wiki-filename "the file ä 'äaä")


(defun dokuwiki-upload-file (file &optional namespace)
	"Upload FILE to NAMESPACE. If NAMESPACE is NIL, asks for NAMESPACE."
	;; (interactive "fFile to upload: ")
	(interactive (list (let ((completion-ignored-extensions nil))
											 (read-file-name "File to upload: " nil nil t))))
	;;
	(if (not namespace)
			(setq namespace (dokuwiki-enter-namespace)))
	;;(setq namespace "playground"))
	;;
	(when (not (equal "" file))
		(let* ((id-filename (dokuwiki-make-wiki-filename file))
					 (url         (format "%s?nocache" (dokuwiki-mediamanager-url)))
																				;
					 (o-upload    (format "-F upload=@'%s'" (expand-file-name file)))
					 ;;
					 (o-id        (x=y "id" id-filename))
					 (o-ow        (x=y "ow" "1")) ;; do overwrite if necessary
					 (o-ns        (x=y "ns" (format "'%s'" namespace)))
					 (o-user      (x=y "u"   dokuwiki-username))
					 (o-pwd       (x=y "p"   dokuwiki-password))
					 (r           (x=y "r"  1))
					 ;;
					 (result
						(with-temp-message (format "Save file '%s' into namespace '%s'" file namespace)
							(dokuwiki-call-curl url
																	o-user
																	o-pwd
																	o-upload
																	o-ow
																	o-id
																	r
																	o-ns))))
			;; Rückmeldungen parsen und anzeigen (TODO: auslagern)
			(when (bufferp result)
				(switch-to-buffer result)
				(goto-char (point-min))
				(cond
				 ((re-search-forward "class=\"error\">\\([^<]+\\)<" nil t)
					(let ((error-description (match-string-no-properties 1)))
						(when error-description
							(kill-buffer result)
							(message "Error during upload. DokuWiki returns '%s'" error-description))))
				 ((re-search-forward "class=\"success\">\\([^<]+\\)<" nil t)
					(let ((success-description (match-string-no-properties 1)))
						(when success-description
							(kill-buffer result)
							(message "File was uploaded: DokuWiki returns '%s'" success-description)))
					;;
					)))  
			;;
			)))

;; (call-interactively 'dokuwiki-upload-file)
;; (dokuwiki-get-media-and-namespaces)

;; INDEX-INFORMATIONEN HOLEN:

(defun dokuwiki-collect-index-information (&optional namespace filter)
  "Gibt eine Liste der Seiten aus dem NAMESPACE zurück. Filtert diese Liste
ggf. auf alle Elemente, die der Regex FILTER entsprechen."
  (save-window-excursion
    (with-temp-message "DokuWiki-Mode: Collecting index on remote host..."
      (let (indexlist
						(buffer (dokuwiki-get-index namespace)))
				;; Angaben aus dem Buffer holen
				(when (bufferp buffer)
					(switch-to-buffer buffer)
					(goto-char (point-min))
					(while (re-search-forward dokuwiki-index-re nil t)
						(add-to-list 'indexlist (concat (dokuwiki-convert-hex-to-utf (match-string-no-properties 1)) ":")))
					(goto-char (point-min))
					(while (re-search-forward dokuwiki-page-re nil t)
						(add-to-list 'indexlist (dokuwiki-convert-hex-to-utf (match-string-no-properties 1))))
					;; ggf. filtern
					(when filter
						(let ((filter-function (lambda (a)
																		 (and (string-match filter a) a))))
							(setq indexlist
										(delq nil (mapcar filter-function indexlist)))))
					;; aufräumen
					(kill-buffer buffer))
				;;
				indexlist))))

;; (dokuwiki-collect-index-information "playground:")


(defun dokuwiki-enter-pagename (&optional pagename)
  "ACHTUNG: Rekursive Funktion. Immer ohne PAGENAME-Argument
aufrufen!

Fordert auf zur Eingabe einer DokuWiki-Seite. Man kann aus den
vorhandenen Seiten auswählen (via completion), wobei die
Einträge, die mit einem : enden (z.B. \"wiki:\"), für ein
Namespace stehen. Wird ein solcher Namespace ausgewählt, wird die
Suche innerhalb dieses Namenraums fortgesetzt. Auswahl einer
vorhandenen Seite oder die Eingabe eines neuen Seitennamens
beendet diese Funktion.

Setzt die Variable `dokuwiki-newpage-entered-p' auf NIL, wenn
eine existierende Seite ausgewählt wurde, ansonsten auf t.
"
  (cond 
   ;; Erster Aufruf: pagename ist nil
   ((not pagename)
    (let* ((pagelist (dokuwiki-collect-index-information))
					 (newpage  (completing-read "Page name: " pagelist)))
      (setq dokuwiki-newpage-entered-p (not (member* newpage pagelist :test 'equal)))
      (dokuwiki-enter-pagename newpage)))
	 ;; Rekursiver Aufruf: pagename endet mit :
   ((string-match ":$" pagename) 
    (let* ((pagelist (dokuwiki-collect-index-information pagename pagename))
					 (newpage (completing-read "Page name: " pagelist nil nil pagename)))
      (setq dokuwiki-newpage-entered-p (not (member* newpage pagelist :test 'equal)))
      (dokuwiki-enter-pagename newpage)))
   ;; Seitennamen (ohne :, oder mit : in der Mitte oder am Anfang)
   ;; werden direkt zurückgegeben:
   (t pagename)))


;; (dokuwiki-get-page (dokuwiki-enter-pagename))

(defun dokuwiki-enter-namespace (&optional namespace)
  "Fordert zur Eingabe eines NAMESPACE voraus. Kennt keine
rekursiven Namensräume, es wird also nur zur Eingabe eines einzigen möglichen
Namensraums aufgefordert."
	(let* ((nslist        (dokuwiki-collect-index-information namespace ":$")))
		(completing-read "Namespace: " nslist)))

;; (dokuwiki-enter-namespace)

(defun dokuwiki-get-media (&optional namespace)
  "Gibt eine Liste aller verfügbaren Medien in NAMESPACE zurück."
  (let* ((url      (dokuwiki-mediamanager-url))
				 ;;
				 (u            (x=y "u"   dokuwiki-username))
				 (p            (x=y "p"   dokuwiki-password))
				 ;;
				 (ns       (x=y "ns"       namespace))
				 ;;
				 (result   
					(dokuwiki-call-curl url
															ns
															u p)))
    result))

(defun dokuwiki-collect-media-information (&optional namespace filter)
  "Gibt eine Liste aller Medien aus dem NAMESPACE zurück. Filtert diese Liste
ggf. auf alle Elemente, die der Regex FILTER entsprechen."
  (save-window-excursion
    (with-temp-message "DokuWiki-Mode: Collecting media index on remote host..."
      (let (indexlist
						(buffer (dokuwiki-get-media namespace)))
				;; Angaben aus dem Buffer holen
				(when (bufferp buffer)
					(switch-to-buffer buffer)
					(goto-char (point-min))
					(while (re-search-forward dokuwiki-mediafile-re nil t)
						(add-to-list 'indexlist (match-string-no-properties 1))))
				;; ggf. filtern
				(if filter
						(let ((filter-function (lambda (a)
																		 (and (string-match filter a) a))))
							(delq nil (mapcar filter-function indexlist))))
				;; und nun buffer löschen
				(kill-buffer buffer) ;; TODO: wird das in collect-index auch gemacht?
				indexlist))))

;; (dokuwiki-get-media "playground:")
;; (dokuwiki-collect-media-information "playground:")

(defun dokuwiki-get-media-and-namespaces (&optional namespace-or-media)
  "Gibt für den NAMESPACE alle gespeicherten Medien und mögliche Unternamensräume zurück."
  (cond
   ;; Bedingung: Ist NIL oder NAMESPACE
   ((or (not namespace-or-media) (string-match ":$" namespace-or-media))
    (let* ((ns-list           (dokuwiki-collect-index-information namespace-or-media ":$"))
					 (media-list        (dokuwiki-collect-media-information namespace-or-media))
					 (ns-and-media-list (append ns-list media-list)))
      (dokuwiki-get-media-and-namespaces (completing-read "Namespace or media: " 
																													ns-and-media-list))))
   ;;
   (t namespace-or-media)))

;; (dokuwiki-get-media-and-namespaces)

(defun dokuwiki-get-media-string (&optional medium-or-namespace description)
  "Return a dokuwiki-link to media (\"{{:namespace:medium|description}}\"."
  ;;
  (setq medium-or-namespace (or medium-or-namespace (dokuwiki-get-media-and-namespaces))
				description         (or description         (read-from-minibuffer "Description: ")))
  ;;
  (when (and medium-or-namespace (not (string= "" medium-or-namespace)))
    ;;
    (format "{{%s}}" (concat medium-or-namespace
														 (and (not (string= "" description)) 
																	(concat "|" description))))))

;; (dokuwiki-get-media-string)

(provide 'dokuwiki)

