;;; kexport.el --- Convert koutlines to other textual formats, including HTML  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    26-Feb-98
;; Last-Mod:     18-Jul-22 at 21:50:10 by Mats Lidell
;;
;; Copyright (C) 1998-2022  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;; Use "https://validator.w3.org/" to validate the HTML that this generates.
;;
;; Within JavaScript-enabled web browsers, koutline parent cells exported to
;; HTML may be expanded and collapsed interactively.  This feature utilizes
;; a small 20-line JavaScript snippet that is included in each exported
;; koutline.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hypb)
(require 'hpath)
(require 'hibtypes) ;; loads hsys-www where www-url actype is defined
(require 'klink)
(require 'kview)
(require 'kotl-mode)
(require 'kimport)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar kexport:input-filename nil
  "This is automatically set to the full pathname of the exported file.")

(defvar kexport:output-filename nil
  "This is automatically set to the full pathname of the exported file.")

(defcustom kexport:html-body-attributes
  "BGCOLOR=\"#FFFFFF\"" ;; white background
  "*String of HTML attributes attached to the <BODY> tag.
Part of an HTML exported koutline file."
  :type 'string
  :group 'hyperbole-koutliner)

(defcustom kexport:html-description
  "Created by Hyperbole's outliner.\nSee \"(hyperbole)Koutliner\" for more information."
  "*String to insert as the HTML-exported document's description, or nil for none."
  :type '(choice (const nil)
		 (string))
  :group 'hyperbole-koutliner)

(defcustom kexport:html-keywords nil
  "*String of comma separated keywords to include with an HTML-exported document.
If nil, use no keywords."
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
   '("{[^}]+}" . "<i>\\&</i>")
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
	 "<a href=\"#k\\1\">\\&</a>")
   (cons (format "&lt;\\s-*@\\s-*\\(%s\\)[^=&>]*&gt;"
		 kexport:kcell-partial-reference-regexp)
	 "<a href=\"#k\\1\">\\&</a>")
   (cons (format "&lt;\\s-*\\([^ \t\n\r,<>]+\\)\\s-*,\\s-*%s[^=&>]*&gt;"
		 kexport:kcell-reference-regexp)
	 'kexport:html-file-klink)
   (cons (format "&lt;\\s-*\\([^ \t\n\r,<>]+\\)\\s-*,\\s-*%s[^=&>]*&gt;"
		 kexport:kcell-partial-reference-regexp)
	 'kexport:html-file-klink))
  "*List of (regexp . replacement-pattern) elements applied in order.
The elements are applied to the contents of each kcell from a
koutline exported to HTML format.  Replacement pattern may be:
  a string with references to regexp's grouping numbers, e.g. \\1, or
  a function of one argument (it is passed the string being replaced in)
  which returns the modified string.  The function may use expressions such
  as (match-beginning 1) since the regexp has just been matched against
  the target string when it is called.")

(defconst kexport:font-awesome-css-url
  "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.9.0/css/all.min.css"
  "Url that provides font-awesome expand/collapse glyphicons.
Font Awesome Free is free and GPL friendly.")

(defconst kexport:font-awesome-css-include
  "<style>

button {
 display: inline;
}

span.nobreak {
  white-space: nowrap;
}

div {
 display: inline;
}

li {
 list-style-type: none;
}

.collapsible {
  all: unset;
  background-color: inherit;
  cursor: pointer;
  display: block;
  font-size: 0;
  outline: inherit;
}

.collapsible:hover {
  background-color: #FAFAD2;
  font-size: 0;
}

.content {
  display: block;
  font-size: 0;
}
</style>\n"
  "CSS that styles collapsible HTML-exported Koutline parent cells.")

(defconst kexport:font-awesome-css-include-with-menus
  "<style>

button {
 display: block;
}

div {
 display: block;
}

li {
 list-style-type: none;
}

.collapsible {
  all: unset;
  background-color: inherit;
  cursor: pointer;
  display: block;
  outline: inherit;
}

.collapsible:hover {
  background-color: #FAFAD2;
}

.content {
  display: block;
}

/* Start Drop-down menu CSS */

/* HTML Nav Styles */
nav menuitem {
   position:relative;
   display:block;
   opacity:0;

   cursor:pointer;
}

nav menuitem > menu {
   position: absolute;
   pointer-events:none;
}
nav > menu { display:flex; }

nav > menu > menuitem { pointer-events: all; opacity:1; }
menu menuitem a { white-space:nowrap; display:block; }

menuitem:hover > menu {
   pointer-events:initial;
}
menuitem:hover > menu > menuitem,
menu:hover > menuitem{
   opacity:1;
}
nav > menu > menuitem menuitem menu {
   transform:translateX(100%);
   top:0; right:0;
}
/* User Styles Below Not Required */

nav {
   margin-top: 40px;
   margin-left: 40px;
}

nav a {
   background:#75F;
   color:#FFF;
   min-width:190px;
   transition: background 0.5s, color 0.5s, transform 0.5s;
   margin:0px 6px 6px 0px;
   padding:20px 40px;
   box-sizing:border-box;
   border-radius:3px;
   box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.5);
   position:relative;
}

nav a:hover:before {
   content: '';
   top:0;left:0;
   position:absolute;
   background:rgba(0, 0, 0, 0.2);
   width:100%;
   height:100%;
}

nav > menu > menuitem > a + menu:after{
   content: '';
   position:absolute;
   border:10px solid transparent;
   border-top: 10px solid white;
   left:12px;
   top: -40px;
}
nav menuitem > menu > menuitem > a + menu:after{
   content: '';
   position:absolute;
   border:10px solid transparent;
   border-left: 10px solid white;
   top: 20px;
   left:-180px;
   transition: opacity 0.6, transform 0s;
}

nav > menu > menuitem > menu > menuitem{
   transition: transform 0.6s, opacity 0.6s;
   transform:translateY(150%);
   opacity:0;
}
nav > menu > menuitem:hover > menu > menuitem,
nav > menu > menuitem.hover > menu > menuitem{
   transform:translateY(0%);
   opacity: 1;
}

menuitem > menu > menuitem > menu > menuitem{
   transition: transform 0.6s, opacity 0.6s;
   transform:translateX(195px) translateY(0%);
   opacity: 0;
}
menuitem > menu > menuitem:hover > menu > menuitem,
menuitem > menu > menuitem.hover > menu > menuitem{
   transform:translateX(0) translateY(0%);
   opacity: 1;
}
/* End Drop-down menu CSS */
</style>\n"
  "CSS that styles collapsible HTML-exported Koutline parent cells and menus.")

(defconst kexport:font-awesome-collapsible-javascript
  "<script>
var coll = document.getElementsByClassName('collapsible');
var i;

function childElt(elt, tag)
{
    return elt.getElementsByTagName(tag)[0];
}

for (i = 0; i < coll.length; i++) {
  coll[i].addEventListener('click', function() {
    var icon = childElt(this, 'span');
    var content = this.nextElementSibling;
    if (content.style.display === 'none') {
      content.style.display = 'block';
      icon.classList.add('fas', 'fa-chevron-down');
      icon.classList.remove('fa-chevron-right');
    } else {
      content.style.display = 'none';
      icon.classList.add('fas', 'fa-chevron-right');
      icon.classList.remove('fa-chevron-down');
    }
  });
}
</script>\n"
  "JavaScript which expands/collapses HTML-exported Koutline parent cells.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun kexport:koutline (&optional soft-newlines-flag)
  "Export current buffer's koutline to the same named file with a \".html\" suffix.
Return the pathname of the html file created.

By default, this retains newlines within cells as they are.  With
optional prefix arg, SOFT-NEWLINES-FLAG, hard newlines are not
used.  Also converts Urls and Klinks into Html hyperlinks."
  (interactive "P")
  (let ((export-from buffer-file-name)
  	(output-to (concat (file-name-sans-extension buffer-file-name) ".html")))
    (kexport:html export-from output-to soft-newlines-flag)
    output-to))

;;;###autoload
(defun kexport:display (&optional soft-newlines-flag)
  "Export the current buffer's koutline and display it in a web browser.
The buffer is exported to the same named file with a \".html\" suffix.
Return the pathname of the html file created.

By default, this retains newlines within cells as they are.  With
optional prefix arg, SOFT-NEWLINES-FLAG, hard newlines are not
used.  Also converts Urls and Klinks into Html hyperlinks."
  (interactive "P")
  (let ((html-file (kexport:koutline soft-newlines-flag)))
    (hact 'www-url (concat "file://" html-file))
    html-file))

;;;###autoload
(defun kexport:html (export-from output-to &optional soft-newlines-flag)
  "Export a koutline buffer or file in EXPORT-FROM to html format in OUTPUT-TO.
By default, this retains newlines within cells as they are.  With
optional prefix arg, SOFT-NEWLINES-FLAG, hard newlines are not
used.  Also converts Urls and Klinks into Html hyperlinks.
!! STILL TODO:
  Make delimited pathnames into file links (but not if within klinks).
  Copy attributes stored in cell 0 and attributes from each cell."
  (interactive (list (read-file-name
		      "Koutline buffer/file to export: " nil buffer-file-name t)
		     (read-file-name "HTML buffer/file to save to: ")
		     current-prefix-arg))
  (let* ((export-buf-name
	  (cond ((get-file-buffer export-from)
		 (buffer-name (get-file-buffer export-from)))
		((and (or (bufferp export-from)
			  (get-buffer export-from))
		      (kotl-mode:is-p))
		 (buffer-name (get-buffer export-from)))
		((and (stringp export-from)
		      (string-match "\\.kotl$" export-from)
		      (file-readable-p export-from))
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
	 ;; Get any title attribute from cell 0, invisible root of the outline
	 (title (kcell:get-attr (kcell-view:cell-from-ref 0) 'title)))

    (with-current-buffer standard-output
      (setq buffer-read-only nil
	    kexport:output-filename buffer-file-name)
      (erase-buffer))
    (with-current-buffer export-buf-name
      (save-excursion
	(kotl-mode:beginning-of-buffer)
	(setq kexport:input-filename buffer-file-name)

	;; If called interactively, prompt user for the title to use.
	(if (called-interactively-p 'interactive)
	    (setq title (read-string (format "Title for %s: " output-to-buf-name)
				     title))
	  ;; Otherwise, use any previously retrieved title attribute or if
	  ;; none, then the name of the current file sans the .kotl suffix.
	  (unless title
	    (setq title (file-name-sans-extension (file-name-nondirectory
						   buffer-file-name))))
	  (when (string-match "\n" title)
	    (setq title (substring title 0 (match-beginning 0)))))

	(princ "<html><head>\n\n")
	(princ "<a id=\"top\"></a><a id=\"k0\"></a>\n")
	(princ (format "<title>%s</title>\n" title))
	(if kexport:html-description
	    (princ (format "<meta id=\"description\" content=\"%s\">\n"
			   kexport:html-description)))
	(if kexport:html-keywords
	    (princ (format "<meta id=\"keywords\" content=\"%s\">\n"
			   kexport:html-keywords)))
	(princ "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">")
	;; CSS
	(princ (format "<link rel=\"stylesheet\" href=\"%s\">\n" kexport:font-awesome-css-url))
	(princ kexport:font-awesome-css-include)
	;; HTML
	(princ "</head>\n\n")
	(princ (format "<body %s>\n\n" kexport:html-body-attributes))
	(princ (format "<h1>%s</h1>\n\n" title))
	;; (princ (format "<label for=\"show-menu\" class=\"show-menu\"><h1>%s</h1></label>\n\n" title))
	;; (princ "<input type=\"checkbox\" id=\"show-menu\" role=\"button\">")
	;; (princ "<nav>
	;; 	<menu>
	;; 		<menuitem id=\"title-menu\">
	;; 			<a>Dropdown</a>
	;; 			<menu>")
	;; (let (text)
	;;   (kview:map-siblings (lambda (kv)
	;; 			(setq text (kcell-view:contents))
	;; 			(princ (format "<menuitem><a href=\"#k%s\">%s</a></menuitem>\n"
	;; 				       (kcell-view:label)
	;; 				       (substring text 0 (string-match "\n" text)))))
	;; 		      kview t))
	;; (princ "         		</menu>
	;; 		</menuitem>
	;; 	</menu>
   	;;     </nav>\n")
	(let* ((separator
		(replace-regexp-in-string
		 ">" "&gt;"
		 (replace-regexp-in-string
		      "<" "&lt;" (kview:label-separator kview))))
	       i is-parent is-last-sibling no-sibling-stack level label contents)
	  (kview:map-tree
	   (lambda (_kview)
	     (setq level (kcell-view:level)
		   i level
		   is-parent (kcell-view:child-p)
		   is-last-sibling (not (kcell-view:sibling-p)))
	     (when is-parent
	       (push is-last-sibling no-sibling-stack)
	       (princ "<button type=\"button\" class=\"collapsible\">\n"))
	     (while (> i 1)
	       (princ "<ul>")
	       (setq i (1- i)))
	     (princ "<li list-style-type=none>\n<table><tr valign=text-bottom>\n")
	     ;; (princ "<td width=1% valign=top>")
	     (princ "<td width=1%>")
	     (princ (format "<span class=\"fas fa-chevron-down fa-fw\"%s></span>"
			    (if is-parent
				""
			      ;; Fill same space for alignment but don't
			      ;; show collapsible chevron when not a parent
			      " style=\"visibility:hidden\"")))
	     (princ "</td>\n")
	     ;; (princ "<td width=2% valign=top>\n")
	     (princ "<td width=2%>\n")
	     (setq label (kcell-view:label))
	     (princ (format "<a id=\"k%s\"></a>" label))
	     (princ (format "<a id=\"k%s\"></a>\n" (kcell-view:idstamp)))
	     (princ (format
		     "<pre><font %s>%s%s</font></pre>\n"
		     kexport:label-html-font-attributes
		     label separator))
	     (princ "</td>\n<td>\n")
	     (setq contents (kcell-view:contents))
	     (when (string-match "\\`\\([-_$%#@~^&*=+|/A-Za-z0-9 ]+\\):.*\\S-"
				 contents)
	       (princ (format "<a id=\"%s\"></a>"
			      (substring contents 0 (match-end 1)))))
	     (setq contents (kexport:html-markup contents))
	     (if soft-newlines-flag
		 (princ contents)
	       (princ "<pre>") (princ contents) (princ "</pre>"))
	     (princ "</td>\n")
	     (princ "</tr></table></li>")
	     (setq i level)
	     (while (> i 1)
	       (princ "</ul>")
	       (setq i (1- i)))
	     (cond (is-parent
		    (princ "\n</button>\n<div class=\"content\">\n"))
		   ((and (/= level 1) is-last-sibling)
		    (princ "\n</div>")
		    (while (pop no-sibling-stack)
		      (princ "</div>"))))
	     (when (not is-parent)
	       (terpri) (terpri)))
	   kview t)
	  ;; Remove any extra newline at the end of any <pre> text
	  (save-excursion
	    (goto-char (point-min))
	    (when (re-search-forward "\r?\n\\'" nil t)
	      (replace-match "" nil nil))))
	;; JavaScript
	(princ kexport:font-awesome-collapsible-javascript)
	(princ "</body></html>\n")))
    (with-current-buffer standard-output
      (save-buffer))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun kexport:html-file-klink (string)
  "Convert STRING containing a klink with a file reference to Html format.
Works exclusively within a call to `replace-regexp-in-string'."
  (let ((filename (substring string (match-beginning 1)
			     (match-end 1))))
    (if (equal filename (file-name-nondirectory
			 kexport:input-filename))
	"<a href=\"#k\\2\">\\&</a>"
      (format "<a href=\"file://%s#k\\2\">\\&</a>"
	      (expand-file-name filename
				(when kexport:input-filename
				  (file-name-directory
				   kexport:input-filename)))))))

(defun kexport:html-markup (string)
  "Perform replacements on STRING specified by `kexport:html-replacement-alist'."
  (mapc
   (lambda (elt)
     (setq string (replace-regexp-in-string (car elt) (cdr elt) string)))
   kexport:html-replacement-alist)
  string)

(defun kexport:html-url (string)
  "Convert STRING containing a Url to Html format.
Works exclusively within a call to `replace-regexp-in-string'."
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
