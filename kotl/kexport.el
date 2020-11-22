;;; kexport.el --- Convert koutlines to other textual formats, including HTML
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    26-Feb-98
;;
;; Copyright (C) 1998-2017  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hypb)
(require 'hpath)
(require 'hibtypes)
(require 'klink)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar kexport:input-filename nil
  "This is automatically set to the full pathname of the file presently being exported.")

(defvar kexport:output-filename nil
  "This is automatically set to the full pathname of the file presently being exported.")

(defcustom kexport:html-body-attributes
  "BGCOLOR=\"#FFFFFF\"" ;; white background
  "*String of HTML attributes attached to the <BODY> tag of an HTML exported koutline file."
  :type 'string
  :group 'hyperbole-koutliner)

(defcustom kexport:html-description
  "Created by Hyperbole's outliner.\nSee \"(hyperbole)Koutliner\" for more information."
  "*String to insert as the HTML-exported document's description, or nil for none."
  :type '(choice (const nil)
		 (string))
  :group 'hyperbole-koutliner)

(defcustom kexport:html-keywords nil
  "*String of comma separated keywords to include with an HTML-exported document, or nil for none."
  :type '(choice (const nil)
		 (string))
  :group 'hyperbole-koutliner)

(defcustom kexport:label-html-font-attributes
  "COLOR=\"#C100C1\" SIZE=\"-1\""
  "*String of HTML font attributes attached to kcell labels when exported."
  :type 'string
  :group 'hyperbole-koutliner)


(defvar kexport:kcell-reference-regexp
  "[0-9a-zA-Z][.0-9a-zA-Z]*=\\([.0-9a-zA-Z]+\\)")

(defvar kexport:kcell-partial-reference-regexp
   "\\([0-9a-zA-Z][.0-9a-zA-Z]*\\)")

(defvar kexport:html-replacement-alist
  (list
   ;; make <> into literal markup
   '("<" . "&lt;")
   '(">" . "&gt;")
   ;;
   ;; italicize keybindings
   '("{[^}]+}" . "<i>\0</i>")
   ;;
   ;; make URLs into hyperlinks
   (cons hpath:url-regexp  'kexport:html-url)
   ;; tightened version of hpath:url-regexp2
   (cons
    (concat
     "<?\\(URL:\\|[^/@]\\|\\`\\|\"\\)\\(\\(\\)\\(\\)\\("
     hpath:url-hostnames-regexp
     "\\.[^/:@ \t\n\r\"`']+\\)\\(:[0-9]+\\)?\\([/~]\\([^\]\[@ \t\n\r\"`'(){}<>]+[^\]\[@ \t\n\r\"`'(){}<>.,?#!*]\\)*\\)?\\)>?")
    'kexport:html-url)
   ;;
   ;; make mail addresses into hyperbuttons
   (cons mail-address-regexp "<a href=\"mailto:\\1\"><i>\\1</i></a>\\2")
   ;;
   ;; make klinks into hyperlinks
   (cons (concat "&lt;\\s-*@\\s-*" kexport:kcell-reference-regexp
		 "[^&>]*&gt;")
	 "<a href=\"#k\\1\">\0</a>")
   (cons (format "&lt;\\s-*@\\s-*\\(%s\\)[^=&>]*&gt;"
		 kexport:kcell-partial-reference-regexp)
	 "<a href=\"#k\\1\">\0</a>")
   (cons (format "&lt;\\s-*\\([^ \t\n\r,<>]+\\)\\s-*,\\s-*%s[^=&>]*&gt;"
		 kexport:kcell-reference-regexp)
	 'kexport:html-file-klink)
   (cons (format "&lt;\\s-*\\([^ \t\n\r,<>]+\\)\\s-*,\\s-*%s[^=&>]*&gt;"
		 kexport:kcell-partial-reference-regexp)
	 'kexport:html-file-klink)
   )
  "*List of (regexp . replacement-pattern) elements applied in order to the
contents of each kcell from a koutline exported to HTML format.  Replacement
pattern may be:
  a string with references to regexp's grouping numbers, e.g. \\1, or
  a function of one argument (it is passed the string being replaced in)
  which returns the modified string.  The function may use expressions such
  as (match-beginning 1) since the regexp has just been matched against
  the target string when it is called.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun kexport:html (export-from output-to &optional soft-newlines-flag)
  "Export a koutline buffer or file in EXPORT-FROM to html format in OUTPUT-TO.
By default, this retains newlines within cells as they are.  With optional prefix arg, SOFT-NEWLINES-FLAG,
hard newlines are not used.  Also converts Urls and Klinks into Html hyperlinks.
STILL TODO:
  Make delimited pathnames into file links (but not if within klinks).
  Copy attributes stored in cell 0 and attributes from each cell."
  (interactive "fKoutline buffer/file to export: \nFHTML buffer/file to save to: \nP")
  (let* ((export-buf-name
	  (cond ((or (bufferp export-from)
		     (get-buffer export-from))
		 (buffer-name (get-buffer export-from)))
		((get-file-buffer export-from)
		 (buffer-name (get-file-buffer export-from)))
		((stringp export-from)
		 (buffer-name (find-file-noselect export-from)))
		(t (error
		    "(kexport:html): `%s' is an invalid `export-from' argument" export-from))))
	 (font-lock-auto-fontify) ;; Prevent syntax highlighting
	 (font-lock-mode-disable-list '(html-mode))
	 (font-lock-mode-enable-list)
	 (html-mode-hook)
	 (hm--html-mode-hook)
	 (psgml-mode-hook)
	 (output-to-buf-name
	  (cond ((or (bufferp output-to)
		     (get-buffer output-to))
		 (buffer-name (get-buffer output-to)))
		((get-file-buffer output-to)
		 (buffer-name (get-file-buffer output-to)))
		((stringp output-to)
		 (buffer-name (find-file-noselect output-to)))
		(t (error
		    "(kexport:html): `%s' is an invalid `output-to' argument" output-to))))
	 (standard-output (get-buffer output-to-buf-name))
	 title)

    (set-buffer standard-output)
    (setq buffer-read-only nil
	  kexport:output-filename buffer-file-name)
    (erase-buffer)
    (set-buffer export-buf-name)
    (setq kexport:input-filename buffer-file-name)

    ;; Use the first line of the first cell as the default HTML document title.
    (setq title (save-excursion
		  (kotl-mode:beginning-of-buffer)
		  (kcell-view:contents)))
    (if (string-match "\n" title)
	(setq title (substring title 0 (match-beginning 0))))

    ;; If called interactively, prompt user for the title to use.
    (if (called-interactively-p 'interactive)
	(setq title (read-string (format "Title for %s: " output-to-buf-name)
				 title)))

    (princ "<html><head>\n\n")
    (princ "<a id=\"top\"></a><a id=\"k0\"></a>\n")
    (princ (format "<title>%s</title>\n" title))
    (if kexport:html-description
	(princ (format "<meta id=\"description\" content=\"%s\">\n"
		       kexport:html-description)))
    (if kexport:html-keywords
	(princ (format "<meta id=\"keywords\" content=\"%s\">\n"
		       kexport:html-keywords)))
    (princ "</head>\n\n")
    (princ (format "<body %s>\n\n" kexport:html-body-attributes))
    (princ (format "<center><h1>%s</h1></center>\n\n" title))
    (let* ((separator
	    (hypb:replace-match-string
	     ">" (hypb:replace-match-string
		  "<" (kview:label-separator kview) "&lt;")
	     "&gt;"))
	   i level label contents)
      (kview:map-tree
       (lambda (kview)
	 (setq level (kcell-view:level)
	       i level)
	 (while (> i 1)
	   (princ "<ul>")
	   (setq i (1- i)))
	 (princ "<table><tr>\n")
	 (setq label (kcell-view:label))
	 (princ (format "<a id=\"k%s\"></a>" label))
	 (princ (format "<a id=\"k%s\"></a>\n" (kcell-view:idstamp)))
	 (princ "<td width=2% valign=top><pre>\n")
	 (princ (format
		 "<font %s>%s%s</font></pre></td>\n"
		 kexport:label-html-font-attributes
		 label separator))
	 (princ "<td>")
	 (setq contents (kcell-view:contents))
	 (if (string-match "\\`\\([-_$%#@~^&*=+|/A-Za-z0-9 ]+\\):.*\\S-"
			   contents)
	     (princ (format "<a id=\"%s\"></a>"
			    (substring contents 0 (match-end 1)))))
	 (setq contents (kexport:html-markup contents))
	 (if soft-newlines-flag
	     (princ contents)
	   (princ "<pre>") (princ contents) (princ "</pre>"))
	 (princ "</td>\n")
	 (princ "</tr></table>")
	 (setq i level)
	 (while (> i 1)
	   (princ "</ul>")
	   (setq i (1- i)))
	 (terpri) (terpri))
       kview t t))
    (princ "</body></html>\n")
    (set-buffer standard-output)
    (save-buffer)))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun kexport:html-file-klink (string)
  "Convert STRING containing a klink with a file reference to Html format.
Works exclusively within a call to `hypb:replace-match-string'."
  (let ((filename (substring string (match-beginning 1)
			     (match-end 1))))
    (if (equal filename (file-name-nondirectory
			 kexport:input-filename))
	"<a href=\"#k\\2\">\0</a>"
      (format "<a href=\"file://%s#k\\2\">\0</a>"
	      (expand-file-name filename
				(if kexport:input-filename
				    (file-name-directory
				     kexport:input-filename)))))))

(defun kexport:html-markup (string)
  "Perform replacements on STRING specified by `kexport:html-replacement-alist'."
  (mapc
   (lambda (elt)
     (setq string (hypb:replace-match-string (car elt) string (cdr elt))))
   kexport:html-replacement-alist)
  string)

(defun kexport:html-url (string)
  "Convert STRING containing a Url to Html format.
Works exclusively within a call to `hypb:replace-match-string'."
  (let* ((url (substring string (match-beginning hpath:url-grpn)
			 (match-end hpath:url-grpn)))
	 (last-str-char (length string))
	 (last-url-char (length url)))
    (while (memq (aref url (1- last-url-char))
		 '(?. ?, ?? ?# ?! ?* ?\( ?\)))
      (setq last-url-char (1- last-url-char)))
    (while (memq (aref string (1- last-str-char))
		 '(?. ?, ?? ?# ?! ?* ?\( ?\)))
      (setq last-str-char (1- last-str-char)))
    (format "<a href=\"%s\">%s</a>%s"
	    (substring url 0 last-url-char)
	    (substring string (match-beginning 0) last-str-char)
	    (substring string last-str-char))))

(provide 'kexport)


;;; kexport.el ends here
