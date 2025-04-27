;;; hibtypes.el --- GNU Hyperbole default implicit button types  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    19-Sep-91 at 20:45:31
;; Last-Mod:     27-Apr-25 at 17:30:02 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1991-2024 Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   Implicit button types (ibtypes) in this file are defined in increasing
;;   order of priority within this file (last one is highest priority).
;;
;;   To return a list of the implicit button types in priority order (highest
;;   to lowest), evaluate:
;;
;;      (symset:get 'ibtypes 'symbols)
;;
;;   If you need to reset the priorities of all ibtypes, evaluate:
;;
;;      (symset:clear 'ibtypes)
;;
;;   and then reload this file.
;;
;;   To get a list of all loaded action types, evaluate:
;;
;;      (symset:get 'actypes 'symbols)

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'cl-lib) ;; for cl-count
(require 'find-func) ;; used by grep-msg ibtype
(eval-when-compile (require 'hversion))
(require 'hactypes)
(require 'hypb)
(require 'org-macs) ;; for org-uuid-regexp
(require 'subr-x) ;; for string-trim
(require 'thingatpt)
(require 'smerge-mode) ;; for smerge-keep-{all, upper, lower}

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(defvar cscope-output-line-regexp)
(defvar id-cflow-repeated-indicator)
(defvar markdown-regex-link-reference)
(defvar markdown-regex-reference-definition)
(defvar org-uuid-regexp)

(declare-function actype:eval "hact")
(declare-function actype:identity "hact")
(declare-function ert-test-boundp "ert")
(declare-function hact "hact")
(declare-function hpath:display-buffer "hpath")
(declare-function htype:def-symbol "hact")
(declare-function hui:help-ebut-highlight "hui")
(declare-function hyperb:stack-frame "hversion")
(declare-function hyrolo-get-file-list "hyrolo")
(declare-function hywiki-get-singular-wikiword "hywiki")
(declare-function hywiki-referent-exists-p "hywiki")
(declare-function markdown-footnote-goto-text "ext:markdown")
(declare-function markdown-footnote-marker-positions "ext:markdown")
(declare-function markdown-footnote-return "ext:markdown")
(declare-function markdown-footnote-text-positions "ext:markdown")
(declare-function markdown-link-p "ext:markdown")
(declare-function markdown-link-url "ext:markdown")
(declare-function markdown-reference-goto-definition "ext:markdown")
(declare-function markdown-reference-goto-link "ext:markdown")
(declare-function markdown-wiki-link-p "ext:markdown")
(declare-function org-roam-id-find "ext:org-roam")
(declare-function set:member "set")
(declare-function symset:add "hact")
(declare-function symtable:add "hact")

;;; ************************************************************************
;;; Public implicit button types
;;; ************************************************************************

(run-hooks 'hibtypes-begin-load-hook)

;; Don't use require below here for any libraries with ibtypes in
;; them.  Use load instead to ensure are reloaded when resetting
;; ibtype priorities.

;;; ========================================================================
;;; Supports jumping to Emacs Regression Test (ert) 'should' source lines
;;; ========================================================================

(load "hsys-ert" nil t)

;;; ========================================================================
;;; Displays Org and Org Roam files and sections by name link
;;; ========================================================================

(load "hynote" nil t)

;;; ========================================================================
;;; Creates and displays personal wiki pages and sections with auto-wikiword links
;;; ========================================================================

(load "hywiki" nil t)

;;; ========================================================================
;;; Jumps to source line from Python traceback lines
;;; ========================================================================

(defib python-tb-previous-line ()
  "Move to prior line with potential Python line ref.
In Python, tracebacks may be on a line just below the source
reference line so since not on a Hyperbole button, move back a
line and check for a source reference line again."
  (save-excursion
    (unless (/= (forward-line -1) 0)
      (ibut:label-set "temp") ;; Real value set in action call below
      (hib-python-traceback))))

;;; ========================================================================
;;; Action Button Types that run Hyperbole tests;
;;; ert-deftest ibtype executes current ert test when on first line of def.
;;; ========================================================================

(load "hypb-ert" nil t)

;;; ========================================================================
;;; Handles social media hashtag and username references, e.g. twitter#myhashtag
;;; ========================================================================

(load "hib-social" nil t)

;;; ========================================================================
;;; Displays Org Roam and Org IDs.
;;; ========================================================================

(defib org-id ()
  "Display Org roam or Org node referenced by uuid at point, if any.
If on the :ID: definition line, display a message about how to copy the uuid.
If the referenced location is found, return non-nil.  Match to uuids
only to prevent false matches."
  (when (featurep 'org-id)
    (let* ((id (thing-at-point 'symbol t)) ;; Could be a uuid or some other form of id
           (bounds (when id (bounds-of-thing-at-point 'symbol)))
	   (start (when bounds (car bounds)))
	   (end   (when bounds (cdr bounds)))
	   m)
      ;; Ignore ID definitions or when not on a possible ID
      (when (and id (if (fboundp 'org-uuidgen-p)
			(org-uuidgen-p id)
		      (string-match org-uuid-regexp (downcase id))))
	(when (and start end)
	  (ibut:label-set id start end))
	(if (and (not assist-flag)
		 (save-excursion (beginning-of-line)
				 (re-search-forward ":\\(CUSTOM_\\)?ID:[ \t]+"
						    (line-end-position) t)))
	    (hact 'message "On ID definition; use {C-u M-RET} to copy a link to an ID.")
	  (when (let ((inhibit-message t) ;; Inhibit org-id-find status msgs
		      (obuf (current-buffer))
		      (omode major-mode))
		  (prog1 (setq m (or (and (featurep 'org-roam)
					  (org-roam-id-find id 'marker))
				     (org-id-find id 'marker)))
		    ;; org-find-id sets current buffer mode to Org
		    ;; mode even if ID is not found; switch it back
		    ;; when necessary.
		    (when (and (eq obuf (current-buffer))
			       (not (eq omode major-mode)))
		      (funcall omode))))
	    (hact 'link-to-org-id-marker m)))))))

(defun org-id:help (_hbut)
  "Copy link to kill ring of an Org roam or Org node referenced by id at point.
If on the :ID: definition line, do nothing and return nil.
If the referenced location is found, return non-nil."
  (when (featurep 'org-id)
    (let ((id (thing-at-point 'symbol t)) ;; Could be a uuid or some other form of id
	  m
	  mpos)
      ;; Ignore ID definitions or when not on a possible ID
      (when (and id
		 (let ((inhibit-message t)) ;; Inhibit org-id-find status msgs
		   (setq m (or (and (featurep 'org-roam) (org-roam-id-find id 'marker))
			       (org-id-find id 'marker)))))
	(save-excursion
	  (setq mpos (marker-position m))
	  (set-buffer (marker-buffer m))
	  (save-restriction
	    (widen)
	    (goto-char mpos)
	    (kill-new (message (org-id-store-link)))))))))

;;; ========================================================================
;;; Composes mail, in another window, to the e-mail address at point.
;;; ========================================================================

(defun mail-address-at-p ()
  "Return e-mail address, a string, that point is within or nil."
  (let ((case-fold-search t))
    (save-excursion
      (skip-chars-backward "^ \t\n\r\f\"\'(){}[];:<>|")
      (and (or (looking-at hypb:mail-address-regexp)
               (looking-at (concat "mailto:" hypb:mail-address-regexp)))
           (save-match-data
             (string-match hypb:mail-address-tld-regexp (match-string-no-properties 1)))
           (match-string-no-properties 1)))))

(defib mail-address ()
  "If on an e-mail address, compose mail to that address in another window.

Applies to any major mode in `hypb:mail-address-mode-list', the HyRolo match
buffer, any buffer attached to a file in `hyrolo-file-list', or any buffer with
\"mail\" or \"rolo\" (case-insensitive) within its name.

If `hypb:mail-address-mode-list' is set to nil, this button type is active
in all buffers."
  (when (let ((case-fold-search t))
          (or
           (and (or (null hypb:mail-address-mode-list)
		    (apply #'derived-mode-p hypb:mail-address-mode-list))
                (not (string-match "-Elements\\'" (buffer-name)))
                ;; Don't want this to trigger within an OOBR-FTR buffer.
                (not (string-match "\\`\\(OOBR.*-FTR\\|oobr.*-ftr\\)"
                                   (buffer-name)))
                (not (string-equal "*Implementors*" (buffer-name))))
           (and
            (string-match "mail\\|rolo" (buffer-name))
            ;; Don't want this to trigger in a mail/news summary buffer.
            (not (or (hmail:lister-p) (hnews:lister-p))))
           (when (boundp 'hyrolo-display-buffer)
             (equal (buffer-name) hyrolo-display-buffer))
           (and (hypb:buffer-file-name)
                (boundp 'hyrolo-file-list)
                (set:member (current-buffer)
                            (mapcar #'get-file-buffer (hyrolo-get-file-list))))))
    (let ((address (mail-address-at-p)))
      (when address
        (ibut:label-set address (match-beginning 1) (match-end 1))
        (hact 'compose-mail-other-window address)))))

;;; ========================================================================
;;; Displays files and directories when a valid pathname is activated.
;;; ========================================================================

(defib pathname ()
  "Make a valid pathname at point display the path entry.

If instead is a PATH-style variable name, .e.g. MANPATH, will prompt
with completion for one of the paths and will then display that.  If
it is the colon or semicolon-separated string of paths value from a
PATH-style variable, the path at point is used; empty paths, e.g. ::
represent the current directory, '.'.

Also works for delimited and non-delimited remote pathnames,
recursive \\='ls' listings, Texinfo @file{} entries, and hash-style
link references to HTML, XML, SGML, Markdown or Emacs outline
headings, shell script comments, and MSWindows paths (see
\"${hyperb:dir}/DEMO#POSIX and MSWindows Paths\" for details).
Emacs Lisp library files (filenames without any directory
component that end in .el, .elc or .eln) are looked up using the
`load-path' directory list.

The pathname may contain references to Emacs Lisp variables or
shell environment variables using the syntax, \"${variable-name}\".

See `hpath:at-p' function documentation for possible delimiters.
See `hpath:suffixes' variable documentation for suffixes that are
added to or removed from pathname when searching for a valid
match.  See `hpath:find' function documentation for special file
display options."
  ;;
  ;; Ignore paths in Buffer menu, dired and helm modes.
  (unless (or (derived-mode-p 'helm-major-mode)
              (delq nil (mapcar (lambda (substring)
                                  (string-match substring (format-mode-line mode-name)))
                                '("Buffer Menu" "IBuffer" "Dired"))))
    (let* ((orig-path (hpath:delimited-possible-path))
	   ;; Normalized and expanded path
	   (path (hpath:at-p))
	   elisp-suffix
           full-path)
      ;; If an Info path without parens, don't handle it here, use the
      ;; `Info-node' ibtype
      (unless (and path (string-match-p ".+\\.info\\([.#]\\|\\'\\)" path))
	(if path
	    (cond ((and (not (string-empty-p path))
 			(= (aref path 0) ?-)
			(or (setq elisp-suffix (string-match "\\`[^\\\\/~]+\\.el[cn]?\\(\\.gz\\)?\\'" path))
			    (string-match "\\`[^.\\/\t\n\r\f]+\\'" path))
			(string-match hpath:prefix-regexp path))
                   (setq path (substring path (match-end 0))
			 full-path (locate-library path elisp-suffix))
                   (cond (full-path
			  (setq path (concat "-" path))
			  (apply #'ibut:label-set orig-path (hpath:start-end orig-path))
			  (hact 'hpath:find path))
			 (elisp-suffix
			  (hact 'error "(pathname): \"%s\" not found in `load-path'" path))
			 ;; Don't match as a pathname ibut; could be a Lisp
			 ;; symbol or something else starting with a '-'.
			 (t nil)))
		  (t (when (string-match "\\`file://" path)
                       (setq path (substring path (match-end 0))))
		     (if (or (> (cl-count ?: orig-path) 2)
			     (> (cl-count ?\; orig-path) 2))
			 ;; PATH-like set of values; select just the one point is on
			 (apply #'ibut:label-set path (hpath:start-end path))
		       ;; Otherwise, use the unchanged orig-path
                       (apply #'ibut:label-set orig-path (hpath:start-end orig-path)))
                     (hact 'link-to-file path)))
          ;;
          ;; Match PATH-related Environment and Lisp variable names and
	  ;; Emacs Lisp and Info files without any directory component.
          (when (setq path orig-path)
            (cond ((and (string-match hpath:path-variable-regexp path)
			(setq path (match-string-no-properties 1 path))
			(hpath:is-path-variable-p path))
		   (setq path (if (or assist-flag (hyperb:stack-frame '(hkey-help)))
				  path
				(hpath:choose-from-path-variable path "Display")))
		   (unless (or (null path) (string-blank-p path)
			       ;; Could be a shell command from a semicolon
			       ;; separated list; ignore if so.
			       (and (string-match "\\`\\s-*\\([^; 	]+\\)" path)
				    (executable-find (match-string-no-properties 1 path))))
                     (apply #'ibut:label-set path (hpath:start-end path))
		     (hact 'link-to-file path)))
		  ((setq elisp-suffix (string-match "\\`[^\\\\/~]+\\.el[cn]?\\(\\.gz\\)?\\'" path))
                   (cond ((string-match hpath:prefix-regexp path)
			  (apply #'ibut:label-set path (hpath:start-end path))
			  (hact 'hpath:find path))
			 ((setq full-path
				(let ((load-suffixes '(".el")))
				  (locate-library path elisp-suffix)))
			  (apply #'ibut:label-set orig-path (hpath:start-end orig-path))
			  (hact 'link-to-file full-path))
			 (elisp-suffix
			  (hact 'error "(pathname): \"%s\" not found in `load-path'" path))
			 ;; Don't match as a pathname ibut; could be a Lisp
			 ;; symbol or something else starting with a '-'.
			 (t nil)))
                  ;; Match only if "(filename)" references a valid Info file
                  ;; and point is within the filename, not on any delimiters
                  ;; so that delimited thing matches trigger later.
                  ((and (not (looking-at "[\"()]"))
			(string-match "\\`(\\([^ \t\n\r\f]+\\))\\'" path)
			(save-match-data (require 'info))
			(Info-find-file (match-string-no-properties 1 path) t))
                   (apply #'ibut:label-set orig-path (hpath:start-end orig-path))
                   (hact 'link-to-Info-node (format "%sTop" path)))
                  ((string-match hpath:info-suffix path)
                   (apply #'ibut:label-set orig-path (hpath:start-end orig-path))
                   (hact 'link-to-Info-node (format "(%s)Top" path)))
                  ;; Otherwise, fall through and allow other implicit
                  ;; button types to handle this context.
                  )))))))

;;; ========================================================================
;;; Follows URLs by invoking a web browser.
;;; ========================================================================

(load "hsys-www" nil t)

;;; ========================================================================
;;; Uses web browser to display links to Hyperbole HTML manual sections;
;;; Links are of the form "hyperbole.html#Smart Keys"
;;; ========================================================================

(defib hyp-manual ()
  "When on a Hyperbole manual file path, display it.
For example, display \"hyperbole.html#Smart Keys\" in a web
browser using the local html version of the Hyperbole manual.
When on \"hyperbole.texi#Smart Keys\", jump to the \"Smart Keys\"
node in the local Texinfo manual.  Without a node name, go to the
top node.

Info file links like \"hyperbole.info#Smart Keys\" are handled by
the `Info-node' implicit button type and displayed in the Emacs
Info browser."
  (let* ((path-start-end (hargs:delimited "\"" "\"" nil nil t))
	 (path (nth 0 path-start-end))
	 (start (nth 1 path-start-end))
	 (end (nth 2 path-start-end))
	 node)
    (when (stringp path)
      (setq path (string-trim path))
      (when (string-match "\\`hyperbole.\\(html\\|texi\\)\\(#.*\\)?\\'" path)
	(save-match-data
	  (setq node (match-string 2 path))
	  (when node
	    (setq node (string-trim (substring node 1))))
	  (ibut:label-set path start end))
	(if (equal "html" (match-string 1 path))
	    (progn
	      ;; Any spaces in #section must be replaced with dashes to match html ids
	      (when node
		(setq node (replace-regexp-in-string "\\s-+" "-" node)))
	      (hact 'www-url (concat "file://"
				     (expand-file-name "hyperbole.html" (hpath:expand "${hyperb:dir}/man/"))
				     (when node "#") node)))
	  ;; texi file
	  (hact 'link-to-file (concat
			       (expand-file-name
				"hyperbole.texi"
				(hpath:expand "${hyperb:dir}/man/"))
			       (when node "#") node)))))))

;;; ========================================================================
;;; Handles internal references within an annotated bibliography, delimiters=[]
;;; ========================================================================

(defib annot-bib ()
  "Display annotated bibliography entries referenced internally.
References must be delimited by square brackets, must begin with a word
constituent character, not contain @ or # characters, must not be
in buffers whose names begin with a space or asterisk character, must
not be in a programming mode, Markdown or Org buffer and must have an
attached file."
  (and (not (bolp))
       (hypb:buffer-file-name)
       (let ((chr (aref (buffer-name) 0)))
         (not (or (eq chr ?\ ) (eq chr ?*))))
       (not (apply #'derived-mode-p
                   '(c++-mode c++-ts-mode c-mode c-ts-mode java-mode java-ts-mode
                              markdown-mode objc-mode org-mode prog-mode)))
       (unless (ibut:label-p t "[[" "]]" t) ;; Org link
	 (let ((ref (hattr:get 'hbut:current 'lbl-key))
	       (lbl-start (hattr:get 'hbut:current 'lbl-start))
	       lbl-start-end)
           (and ref
		lbl-start
		(eq ?w (char-syntax (aref ref 0)))
		(not (string-match "[#@]" ref))
		(save-excursion
		  (goto-char lbl-start)
		  (setq lbl-start-end (ibut:label-p t "[" "]" t)))
		(apply #'ibut:label-set lbl-start-end)
		(hact 'annot-bib ref))))))

;;; ========================================================================
;;; Follows Org links that are in non-Org mode buffers
;;; ========================================================================

;; Org links in Org mode are handled at the highest priority; see the last
;; section at the end of this file.

(defib org-link-outside-org-mode ()
  "Follow an Org link in a non-Org mode buffer.
This should be a very low priority so other Hyperbole types
handle any links they recognize first."
  (when (and (not (funcall hsys-org-mode-function))
	     ;; Prevent infinite recursion, e.g. if called via
	     ;; `org-metareturn-hook' from `org-meta-return' invocation.
	     (not (hyperb:stack-frame '(ibtypes::debugger-source org-meta-return))))
    (require 'hsys-org)
    (declare-function hsys-org-link-at-p      "hsys-org" ())
    (declare-function hsys-org-set-ibut-label "hsys-org" (start-end))
    (let ((start-end (hsys-org-link-at-p)))
      (when start-end
        (hsys-org-set-ibut-label start-end)
        (hact #'org-open-at-point-global)))))

;;; ========================================================================
;;; Displays in-file Markdown link referents.
;;; ========================================================================

(defun markdown-follow-link-p ()
  "Jump between reference links and definitions or footnote markers and text.
Return t if jump and nil otherwise."
  (cond
   ;; Footnote definition
   ((markdown-footnote-text-positions)
    (markdown-footnote-return)
    t)
   ;; Footnote marker
   ((markdown-footnote-marker-positions)
    (markdown-footnote-goto-text)
    t)
   ;; Reference link
   ((thing-at-point-looking-at markdown-regex-link-reference)
    (markdown-reference-goto-definition)
    t)
   ;; Reference definition
   ((thing-at-point-looking-at markdown-regex-reference-definition)
    (markdown-reference-goto-link (match-string-no-properties 2))
    t)))

(defun markdown-follow-inline-link-p (opoint)
  "If on an inline link, jump to its referent if it is absolute and return non-nil.
Absolute means not relative within the file.  Otherwise, if an
internal link, move back to OPOINT and return nil."
  ;; Caller already checked not on a URL (handled elsewhere).
  (let ((path (markdown-link-url)))
    (goto-char opoint)
    (when (markdown-link-p)
      (ibut:label-set (match-string-no-properties 0) (match-beginning 0) (match-end 0))
      (if path
	  (hact 'link-to-file path)
        (hpath:display-buffer (current-buffer))
        (hact 'markdown-follow-link-at-point)))))

(defib markdown-internal-link ()
  "Display any in-file Markdown link referent at point.
Url links are handled elsewhere."
  (when (and (derived-mode-p 'markdown-mode)
             (not (hpath:www-at-p)))
    (let ((opoint (point))
          npoint)
      (cond ((markdown-link-p)
             (condition-case ()
                 ;; Follows a reference link or footnote to its referent.
                 (if (markdown-follow-link-p)
                     (when (/= opoint (point))
                       (ibut:label-set (match-string-no-properties 0) (match-beginning 0) (match-end 0))
                       (setq npoint (point))
                       (goto-char opoint)
                       (hact 'link-to-file (hypb:buffer-file-name) npoint))
                   ;; Follows an absolute file link.
                   (markdown-follow-inline-link-p opoint))
               ;; May be on the name of an infile link, so move to the
               ;; link itself and then display it as a pathname.
               (error (markdown-follow-inline-link-p opoint))))
            ((markdown-wiki-link-p)
             (ibut:label-set (match-string-no-properties 0) (match-beginning 0) (match-end 0))
             (hpath:display-buffer (current-buffer))
             (hact 'markdown-follow-wiki-link-at-point))))))

;;; ========================================================================
;;; Summarizes an Internet rfc for random access browsing by section.
;;; ========================================================================

(defib rfc-toc ()
  "Summarize the contents of an Internet rfc from anywhere within an rfc buffer.
Each line in the summary may be selected to jump to a section."
  (let ((case-fold-search t)
        (toc)
        (opoint (point))
	sections-start)
    (if (and (string-match "\\`rfc[-_]?[0-9]" (buffer-name))
	     (not (string-match "toc" (buffer-name)))
             (goto-char (point-min))
             (progn (setq toc (search-forward "Table of Contents" nil t))
                    (re-search-forward "^[ \t]*1.0?[ \t]+[^ \t\n\r]" nil t
                                       (and toc 2))))
        (progn (beginning-of-line)
	       (setq sections-start (point))
               (ibut:label-set (buffer-name))
	       (goto-char opoint)
               (hact 'rfc-toc (buffer-name) nil sections-start))
      (goto-char opoint)
      nil)))

;;; ========================================================================
;;; Expands or collapses C call trees and jumps to code definitions.
;;; ========================================================================

(defib id-cflow ()
  "Expand or collapse C call trees and jump to code definitions.
Require cross-reference tables built by the external `cxref' program of Cflow."
  (when (and (derived-mode-p 'id-cflow-mode)
             (not (eolp)))
    (let ((pnt (point)))
      (save-excursion
        (cond
         ;; If on a repeated function mark, display its previously
         ;; expanded tree.
         ((progn (skip-chars-backward " ")
                 (looking-at id-cflow-repeated-indicator))
          (let ((end (point))
                start entry)
            (beginning-of-line)
            (skip-chars-forward "| ")
            (setq start (point)
                  entry (buffer-substring-no-properties start end))
            (ibut:label-set entry start end)
            (condition-case ()
                (hact 'link-to-regexp-match
                      (concat "^[| ]*[&%%]*" (regexp-quote entry) "$")
                      1 (current-buffer) t)
              (error
               (goto-char end)
               (error "(id-cflow): No prior expansion found")))))
         ;; If to the left of an entry, expand or contract its tree.
         ((progn (beginning-of-line)
                 (or (= pnt (point))
                     (and (looking-at "[| ]+")
                          (<= pnt (match-end 0)))))
          (hact 'id-cflow-expand-or-contract current-prefix-arg))
         ;; Within an entry's filename, display the file.
         ((search-forward "\(" pnt t)
          (let* ((start (point))
                 (end (1- (search-forward "\)" nil t)))
                 (file (buffer-substring-no-properties start end)))
            (ibut:label-set file start end)
            (hact 'link-to-file file)))
         ;; Within an entry's function name, jump to its definition.
         (t
          (hact 'smart-c)))))))

;;; ========================================================================
;;; Jumps to the source line associated with a ctags file entry.
;;; ========================================================================

(defib ctags ()
  "Jump to the source line associated with a ctags file entry in any buffer."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "^\\(\\S-+\\) \\(\\S-+\\.[a-zA-Z]+\\) \\([1-9][0-9]*\\)$")
      ;;             identifier       pathname              line-number
      ;; ctags vgrind output format entry
      (let ((identifier (match-string-no-properties 1))
            (file (expand-file-name (match-string-no-properties 2)))
            (line-num (string-to-number (match-string-no-properties 3))))
        (ibut:label-set identifier (match-beginning 1) (match-end 1))
        (hact 'link-to-file-line file line-num)))
     ((looking-at "^\\(\\S-+\\) +\\([1-9][0-9]*\\) \\(\\S-+\\.[a-zA-Z]+\\) ")
      ;; ctags cxref output format entry
      ;;             identifier    line-number           pathname
      (let ((identifier (match-string-no-properties 1))
            (line-num (string-to-number (match-string-no-properties 2)))
            (file (expand-file-name (match-string-no-properties 3))))
        (ibut:label-set identifier (match-beginning 1) (match-end 1))
        (hact 'link-to-file-line file line-num))))))

;;; ========================================================================
;;; Jumps to the source line associated with an etags file entry in a TAGS buffer.
;;; ========================================================================

(defib etags ()
  "Jump to the source line associated with an etags file entry in a TAGS buffer.
If on a tag entry line, jump to the source line for the tag.  If on a
pathname line or line preceding it, jump to the associated file."
  (when (let (case-fold-search) (string-match "^TAGS" (buffer-name)))
    (save-excursion
      (beginning-of-line)
      (cond
       ((save-excursion
          (and (or (and (eq (following-char) ?\^L)
                        (zerop (forward-line 1)))
                   (and (zerop (forward-line -1))
                        (eq (following-char) ?\^L)
                        (zerop (forward-line 1))))
               (looking-at "\\([^,\n\r]+\\),[0-9]+$")))
        (let ((file (match-string-no-properties 1)))
          (ibut:label-set file (match-beginning 1) (match-end 1))
          (hact 'link-to-file file)))
       ((looking-at
         "\\([^\^?\n\r]+\\)[ ]*\^?\\([^\^A\n\r]+\^A\\)?\\([1-9][0-9]+\\),")
        (let* ((tag-grouping (if (match-beginning 2) 2 1))
               (tag (buffer-substring-no-properties (match-beginning tag-grouping)
                                                    (1- (match-end tag-grouping))))
               (line (string-to-number (match-string-no-properties 3)))
               file)
          (ibut:label-set tag (match-beginning tag-grouping)
                          (1- (match-end tag-grouping)))
          (save-excursion
            (if (re-search-backward "\^L\r?\n\\([^,\n\r]+\\),[0-9]+$" nil t)
                (setq file (expand-file-name (match-string-no-properties 1)))
              (setq file "No associated file name")))
          (hact 'link-to-file-line file line)))))))

;;; ========================================================================
;;; Jumps to C/C++ source line associated with Cscope C analyzer output line.
;;; ========================================================================

(defib cscope ()
  "Jump to C/C++ source line associated with Cscope C analyzer output line.
The cscope.el Lisp library available from the Emacs package manager
must be loaded and the open source cscope program available from
http://cscope.sf.net must be installed for this button type to do
anything."
  (and (boundp 'cscope:bname-prefix)  ;; (featurep 'cscope)
       (stringp cscope:bname-prefix)
       (string-match (regexp-quote cscope:bname-prefix)
                     (buffer-name))
       (= (match-beginning 0) 0)
       (save-excursion
         (beginning-of-line)
         (looking-at cscope-output-line-regexp))
       (let (start end)
         (skip-chars-backward "^\n\r")
         (setq start (point))
         (skip-chars-forward "^\n\r")
         (setq end (point))
         (ibut:label-set (buffer-substring start end)
                         start end)
         (hact 'cscope-interpret-output-line))))

;;; ========================================================================
;;; Makes README table of contents entries jump to associated sections.
;;; ========================================================================

(defib text-toc ()
  "Jump to the text file section referenced by a table of contents entry at point.
Buffer must be in a text mode or must contain DEMO, README or
TUTORIAL and there must be a `Table of Contents' or `Contents'
label on a line by itself (it optionally may begin with an
asterisk), preceding the table of contents.  Each toc entry must
begin with optional whitespace followed by one or more asterisks.
Each section header linked to by the toc must start with optional
whitespace and then one or more asterisks at the beginning of the
line."
  (let (section)
    (when (and (or (derived-mode-p 'text-mode)
		   (string-match "DEMO\\|README\\|TUTORIAL" (buffer-name)))
               (save-excursion (re-search-backward
                                "^\\*?*[ \t]*\\(Table of \\)?Contents[ \t]*$"
                                nil t))
               (save-excursion
                 (beginning-of-line)
                 ;; Entry line within a TOC
                 (when (and (or
			     ;; Next line is typically in RFCs,
			     ;; e.g. "1.1.  Scope ..... 1"
			     (looking-at "^[ \t]*\\([0-9.]+\\([ \t]+[^ \t\n\r]+\\)+\\) \\.+")
			     (looking-at "[ \t]+\\([-+*o]+[ \t]+.*\\)$"))
			    (setq section (string-trim (match-string-no-properties 1))))
		   (ibut:label-set section (match-beginning 1) (match-end 1))
                   t)))
      (hact 'text-toc section))))

;;; ========================================================================
;;; Makes directory summaries into file list menus.
;;; ========================================================================

(defib dir-summary ()
  "Detect filename buttons in files named \"MANIFEST\" or \"DIR\".
Display selected files.  Each file name must be at the beginning of the line
or may be preceded by some semicolons and must be followed by one or more
spaces and then another non-space, non-parenthesis, non-brace character."
  (when (hypb:buffer-file-name)
    (let ((file (file-name-nondirectory (hypb:buffer-file-name)))
          entry start end)
      (when (and (or (string-equal file "DIR")
                     (string-match "\\`MANIFEST\\(\\..+\\)?\\'" file))
		 (save-excursion
		   (beginning-of-line)
		   (when (looking-at "\\(;+[ \t]*\\)?\\([^(){}* \t\n\r]+\\)")
		     (setq entry (match-string-no-properties 2)
			   start (match-beginning 2)
			   end (match-end 2))
		     (file-exists-p entry))))
        (ibut:label-set entry start end)
        (hact 'link-to-file entry)))))

;;; ========================================================================
;;; Handles Gnu debbugs issue ids, e.g. bug#45678 or just 45678.
;;; ========================================================================

(load "hib-debbugs" nil t)

;;; ========================================================================
;;; Executes or documents command bindings of brace delimited key sequences.
;;; ========================================================================

(load "hib-kbd" nil t)

;;; ========================================================================
;;; Makes Internet RFC references retrieve the RFC.
;;; ========================================================================

(defib rfc ()
  "Retrieve and display an Internet Request for Comments (RFC) at point.
The following formats are recognized: RFC822, rfc-822, and RFC 822.  The
`hpath:rfc' variable specifies the location from which to retrieve RFCs.
Requires the Emacs builtin Tramp library for ftp file retrievals."
  (let ((case-fold-search t)
        (rfc-num nil))
    (and (not (memq major-mode '(dired-mode monkey-mode)))
         (boundp 'hpath:rfc)
         (stringp hpath:rfc)
         (or (looking-at " *\\(rfc[- ]?\\([0-9]+\\)\\)")
             (save-excursion
               (skip-chars-backward "0-9")
               (skip-chars-backward "- ")
               (skip-chars-backward "rRfFcC")
               (looking-at " *\\(rfc[- ]?\\([0-9]+\\)\\)")))
         (progn (setq rfc-num (match-string-no-properties 2))
                (ibut:label-set (match-string-no-properties 1)
				(match-beginning 1)
				(match-end 1))
                t)
         ;; Ensure remote file access is available for retrieving a remote
         ;; RFC, if need be.
         (if (string-match "^/.+:" hpath:rfc)
             ;; This is a remote path.
             (hpath:remote-available-p)
           ;; local path
           t)
         (hact 'link-to-rfc rfc-num))))

;;; ========================================================================
;;; Shows man page associated with a man apropos entry.
;;; ========================================================================

(defib man-apropos ()
  "Make man apropos entries display associated man pages when selected."
  (save-excursion
    (beginning-of-line)
    (let ((nm "[^ \t\n\r!@,:;(){}][^ \t\n\r,(){}]*[^ \t\n\r@.,:;(){}]")
          topic)
      (and (looking-at
            (concat
             "^\\(\\*[ \t]+[!@]\\)?\\(" nm "[ \t]*,[ \t]*\\)*\\(" nm "\\)[ \t]*"
             "\\(([-0-9a-zA-z]+)\\)\\(::\\)?[ \t]+-[ \t]+[^ \t\n\r]"))
           (setq topic (concat (match-string-no-properties 3)
                               (match-string-no-properties 4)))
           (ibut:label-set topic (match-beginning 3) (match-end 4))
	   ;; Use 'man' instead of 'actypes::man-show' in next line so
	   ;; can follow cross-references within the same window when
	   ;; Hyperbole is set to display other referents in another window.
           (hact 'man topic)))))

;;; ========================================================================
;;; Follows links to Hyperbole Koutliner cells.
;;; ========================================================================

(load "klink" nil t)

;;; ========================================================================
;;; Links to Hyperbole button types
;;; ========================================================================

(defun hlink (link-actype label-prefix start-delim end-delim)
  "Call LINK-ACTYPE and use LABEL-PREFIX if point is within an implicit button.
LINK-ACTYPE is the action type and button is prefixed with
LABEL-PREFIX.  The button must be delimited by START-DELIM and
END-DELIM."
  ;; Used by e/g/ilink implicit buttons."
  (let* ((label-start-end (hbut:label-p t start-delim end-delim t t))
         (label-and-file (nth 0 label-start-end))
         (start-pos (nth 1 label-start-end))
         (end-pos (nth 2 label-start-end))
         but-key lbl-key key-file partial-lbl)
    (when label-and-file
      (setq label-and-file (hlink:parse-label-and-file label-and-file)
            partial-lbl (nth 0 label-and-file)
            but-key (hbut:label-to-key partial-lbl)
            key-file (nth 1 label-and-file)
            lbl-key (when but-key (concat label-prefix but-key)))
      (ibut:label-set (hbut:key-to-label lbl-key) start-pos end-pos)
      (hact link-actype but-key key-file))))

(defun hlink:parse-label-and-file (label-and-file)
  "Parse colon-separated string LABEL-AND-FILE into a list of label and file path."
  ;; Can't use split-string here because file path may contain colons;
  ;; we want to split only on the first colon.
  (let ((i 0)
        (len (length label-and-file))
        label
        file)
    (while (< i len)
      (when (= ?: (aref label-and-file i))
        (when (zerop i)
          (error "(hlink:parse-label-and-file): Missing label: '%s'" label-and-file))
        (setq label (hpath:trim (substring label-and-file 0 i))
              file (hpath:trim (substring label-and-file (1+ i))))
        (when (string-empty-p label) (setq label nil))
        (when (string-empty-p file) (setq file nil))
        (setq i len))
      (setq i (1+ i)))
    (unless (or label (string-empty-p label-and-file))
      (setq label label-and-file))
    (delq nil (list label file))))

(defconst elink:start "<elink:"
  "String matching the start of a link to a Hyperbole explicit button.")
(defconst elink:end   ">"
  "String matching the end of a link to a Hyperbole explicit button.")

(defib elink ()
  "At point, activate a link to an explicit button.
This executes the linked to explicit button's action in the
context of the current buffer.

Recognizes the format '<elink:' button_label [':' button_file_path] '>',
where : button_file_path is given only when the link is to another file,
e.g. <elink: project-list: ~/projs>."
  (progn
    (ibut:label-set "temp") ;; Real value set in action call below
    (hlink 'link-to-ebut "" elink:start elink:end)))

(defconst glink:start "<glink:"
  "String matching the start of a link to a Hyperbole global button.")
(defconst glink:end   ">"
  "String matching the end of a link to a Hyperbole global button.")

(defib glink ()
  "At point, activates a link to a global button.
This executes the linked to global button's action in the context
of the current buffer.

Recognizes the format '<glink:' button_label '>',
e.g. <glink: open todos>."
  (progn
    (ibut:label-set "temp") ;; Real value set in action call below
    (hlink 'link-to-gbut "" glink:start glink:end)))

(defconst ilink:start "<ilink:"
  "String matching the start of a link to a Hyperbole implicit button.")
(defconst ilink:end   ">"
  "String matching the end of a link to a Hyperbole implicit button.")

(defib ilink ()
  "At point, activate a link to a labeled implicit button.
This executes the linked to implicit button's action in the context of the
current buffer.

Recognizes the format '<ilink:' button_label [':' button_file_path] '>',
where button_file_path is given only when the link is to another file,
e.g. <ilink: my series of keys: ${hyperb:dir}/HYPB>."
  (progn
    (ibut:label-set "temp") ;; Real value set in action call below
    (hlink 'link-to-ibut "" ilink:start ilink:end)))

;;; ========================================================================
;;; Displays files at specific lines and optional column number
;;; locations.
;;; ========================================================================

(defib pathname-line-and-column ()
  "Display path at position given by a pathname:line-num[:column-num] pattern.
Also works for remote pathnames.
May also contain hash-style link references with the following format:
\"<path>[#<link-anchor>]:[L]<line-num>[:[C]<column-num>]}\".

See `hpath:at-p' function documentation for possible delimiters.
See `hpath:suffixes' variable documentation for suffixes that are added to or
removed from pathname when searching for a valid match.
See `hpath:find' function documentation for special file display options."
  (let* ((path-start-and-end (hpath:delimited-possible-path nil t))
	 (path-line-and-col (nth 0 path-start-and-end))
	 (start (nth 1 path-start-and-end)))
    (when (and (stringp path-line-and-col)
               (string-match hpath:section-line-and-column-regexp path-line-and-col))
      (let* ((line-num (string-to-number (match-string-no-properties 3 path-line-and-col)))
             (col-num (when (match-end 4)
			(string-to-number (match-string-no-properties 5 path-line-and-col))))
	     (label (match-string-no-properties 1 path-line-and-col))
	     ;; Next variable must come last as it can overwrite the match-data
	     (file (hpath:expand label)))
        (when (setq file (hpath:is-p file))
          (ibut:label-set label start (+ start (length label)))
          (if col-num
              (hact 'link-to-file-line-and-column file line-num col-num)
            (hact 'link-to-file-line file line-num)))))))

;;; ========================================================================
;;; Jumps to source line associated with ipython, ripgrep, grep or
;;; compilation errors or HyRolo stuck position error messages.
;;; ========================================================================

(defun hib-link-to-file-line (file line-num)
  "Expand FILE and jump to its LINE-NUM in Hyperbole specified window.
The variable `hpath:display-where' determines where to display the file.
LINE-NUM may be an integer or string."
  ;; RSW 12-05-2021 - Added hpath:expand in next line to
  ;; resolve any variables in the path before checking if absolute.
  (let ((source-loc (unless (file-name-absolute-p (hpath:expand file))
                      (hbut:to-key-src t)))
	ext)
    (if (stringp source-loc)
        (setq file (expand-file-name file (file-name-directory source-loc)))
      (setq file (or (hpath:prepend-shell-directory file)
		     ;; find-library-name will strip file
		     ;; suffixes, so use it only when the file
		     ;; either doesn't have a suffix or has a
		     ;; library suffix.
		     (and (or (null (setq ext (file-name-extension file)))
			      (member (concat "." ext) (get-load-suffixes)))
			  (ignore-errors (find-library-name file)))
		     (expand-file-name file))))
    (when (file-exists-p file)
      (actypes::link-to-file-line file line-num))))

(defib ipython-stack-frame ()
  "Jump to the line associated with an ipython stack frame line numbered msg.
ipython outputs each pathname once followed by all matching lines
in that pathname.  Messages are recognized in any buffer (other
than a helm completion buffer)."
  ;; Locate and parse ipython stack trace messages found in any buffer other than a
  ;; helm completion buffer.
  ;;
  ;; Sample ipython stack trace command output:
  ;;
  ;; ~/Dropbox/py/inview/inview_pr.py in ap(name_filter, value_filter, print_func)
  ;; 1389     apc(name_filter, value_filter, print_func, defined_only=True)
  ;; 1390     print('\n**** Modules/Packages ****')
  ;; -> 1391     apm(name_filter, value_filter, print_func, defined_only=True)
  ;; 1392
  ;; 1393 def apa(name_filter=None, value_filter=None, print_func=pd1, defined_only=False):
  (unless (derived-mode-p 'helm-major-mode)
    (save-excursion
      (beginning-of-line)
      (let ((line-num-regexp "\\( *\\|-+> \\)?\\([1-9][0-9]*\\) ")
            line-num
            file)
        (when (looking-at line-num-regexp)
          ;; ipython stack trace matches and context lines (-A<num> option)
          (setq line-num (match-string-no-properties 2)
                file nil)
          (while (and (= (forward-line -1) 0)
                      (looking-at line-num-regexp)))
          (unless (or (looking-at line-num-regexp)
                      (not (re-search-forward " in " nil (line-end-position)))
                      (and (setq file (buffer-substring-no-properties (line-beginning-position) (match-beginning 0)))
                           (string-empty-p (string-trim file))))
	    (ibut:label-set (concat file ":" line-num))
	    (hact 'hib-link-to-file-line file line-num)))))))

(defib ripgrep-msg ()
  "Jump to the line associated with a ripgrep (rg) line numbered msg.
Ripgrep outputs each pathname once followed by all matching lines
in that pathname.  Messages are recognized in any buffer (other
than a helm completion buffer)."
  ;; Locate and parse ripgrep messages found in any buffer other than a
  ;; helm completion buffer.
  ;;
  ;; Sample ripgrep command output:
  ;;
  ;; bash-3.2$ rg -nA2 hkey-throw *.el
  ;; hmouse-drv.el
  ;; 405:(defun hkey-throw (release-window)
  ;; 406-  "Throw either a displayable item at point or the current buffer to RELEASE-WINDOW.
  ;; 407-The selected window does not change."
  ;; --
  ;; 428:    (hkey-throw to-window)))
  ;; 429-
  ;; 430-(defun hmouse-click-to-drag ()
  ;;
  ;; Use `rg -n --no-heading' for pathname on each line.
  (unless (derived-mode-p 'helm-major-mode)
    (save-excursion
      (beginning-of-line)
      (when (looking-at "\\([1-9][0-9]*\\)[-:]")
        ;; Ripgrep matches and context lines (-A<num> option)
        (let ((line-num (match-string-no-properties 1))
              file)
          (while (and (= (forward-line -1) 0)
                      (looking-at "[1-9][0-9]*[-:]\\|--$")))
          (unless (or (looking-at "[1-9][0-9]*[-:]\\|--$")
                      (and (setq file (string-trim (buffer-substring-no-properties (line-beginning-position) (match-beginning 0))))
                           (or (string-empty-p file)
		               (not (file-exists-p file)))))
	    (ibut:label-set (concat file ":" line-num))
	    (hact 'hib-link-to-file-line file line-num)))))))

(defib hyrolo-stuck-msg ()
  "Jump to the position where a HyRolo search has become stuck from the error.
Such errors are recognized in any buffer (other than a helm completion
buffer)."
  (unless (derived-mode-p 'helm-major-mode)
    (save-excursion
      (beginning-of-line)
      ;; HyRolo stuck error
      (when (looking-at ".*(hyrolo-grep-file): Stuck looping in buffer \\\\?\"\\([^\\\t\n\r\f\"'`]+\\)\\\\?\" at position \\([0-9]+\\)")
        (let* ((buffer-name (match-string-no-properties 1))
               (pos  (or (match-string-no-properties 2) "1"))
               (but-label (concat buffer-name ":P" pos)))
	  (when (buffer-live-p (get-buffer buffer-name))
            (setq pos (string-to-number pos))
            (ibut:label-set but-label)
            (hact 'link-to-buffer-tmp buffer-name pos)))))))

(defib grep-msg ()
  "Jump to the line associated with line numbered grep or compilation error msgs.
Messages are recognized in any buffer (other than a helm completion
buffer) except for grep -A<num> context lines which are matched only
in grep and shell buffers."
  ;; Locate and parse grep messages found in any buffer other than a
  ;; helm completion buffer.
  (unless (derived-mode-p 'helm-major-mode)
    (save-excursion
      (beginning-of-line)
      (when (or
             ;; HyRolo stuck error
             (looking-at ".*(hyrolo-grep-file): Stuck looping in \\(buffer\\) \\\\?\"\\([^\\\t\n\r\f\"'`]+\\)\\\\?\" at position \\([0-9]+\\)")
	     ;; Emacs native compiler file lines
	     (looking-at "Compiling \\(\\S-+\\)\\.\\.\\.$")
	     (looking-at "Loading \\(\\S-+\\) (\\S-+)\\.\\.\\.$")
 	     (looking-at "[a-zA-Z0-9]+ ([-a-zA-Z0-9]+): \\([^:\"'`]+\\):\\([0-9]+\\):")
             ;; Grep matches (allowing for Emacs Lisp vars with : in
	     ;; name within the pathname), Ruby, UNIX C compiler and Introl 68HC11 C compiler errors
             (looking-at "\\([^ \t\n\r\"'`]*[^ \t\n\r:\"'`0-9]\\): ?\\([1-9][0-9]*\\)[ :]")
	     ;; Ruby tracebacks
             (looking-at "[ \t]+[1-9][0-9]*: from \\([^ \t\n\r\"'`]*[^ \t\n\r:\"'`]\\):\\([1-9][0-9]*\\):in")
             ;; Grep matches, UNIX C compiler and Introl 68HC11 C
             ;; compiler errors, allowing for file names with
             ;; spaces followed by a null character rather than a :
             (looking-at "\\([^\t\n\r\"'`]+\\)\0 ?\\([1-9][0-9]*\\)[ :]")
             ;; HP C compiler errors
             (looking-at "[a-zA-Z0-9]+: \"\\([^\t\n\r\",]+\\)\", line \\([0-9]+\\):")
             ;; BSO/Tasking 68HC08 C compiler errors
             (looking-at
              "[a-zA-Z 0-9]+: \\([^ \t\n\r\",]+\\) line \\([0-9]+\\)[ \t]*:")
             ;; UNIX Shell errors
             (looking-at "\\([^:\"'`]+\\): line \\([0-9]+\\): ")
             ;; UNIX Lint errors
             (looking-at "[^:\"'`]+: \\([^ \t\n\r:]+\\): line \\([0-9]+\\):")
             ;; SparcWorks C compiler errors (ends with :)
             ;; IBM AIX xlc C compiler errors (ends with .)
             (looking-at "\"\\([^\"'`]+\\)\", line \\([0-9]+\\)[:.]")
             ;; Introl as11 assembler errors
             (looking-at " \\*+ \\([^ \t\n\r\"'`]+\\) - \\([0-9]+\\) ")
             ;; perl5: ... at file.c line 10
             (looking-at ".+ at \\([^ \t\n\r\"'`]+\\) line +\\([0-9]+\\)")
             ;; Weblint
             (looking-at "\\([^ \t\n\r:()\"'`]+\\)(\\([0-9]+\\)): ")
             ;; Microsoft JVC
             ;; file.java(6,1) : error J0020: Expected 'class' or 'interface'
             (looking-at "^\\(\\([a-zA-Z]:\\)?[^:\( \t\n\r-]+\\)[:\(][ \t]*\\([0-9]+\\),")
             ;; Grep match context lines (-A<num> option)
             (and (string-match "grep\\|shell" (buffer-name))
                  (looking-at "\\([^ \t\n\r:\"'`]+\\)-\\([1-9][0-9]*\\)-")))
        (let* ((file (match-string-no-properties 1))
               (line-num  (or (match-string-no-properties 2) "1")))
	  (ibut:label-set (concat file ":" line-num))
	  (hact 'hib-link-to-file-line file line-num))))))

;;; ========================================================================
;;; Jumps to source line associated with debugger stack frame or breakpoint
;;; lines.  Supports pdb, gdb, dbx, and xdb.
;;; ========================================================================

(defun hib-python-traceback ()
"Test for and jump to line referenced in Python pdb, traceback, or pytype error."
  (when (or (looking-at "\\(^\\|.+ \\)File \"\\([^\"\t\f\n\r]+\\S-\\)\", line \\([0-9]+\\)")
            (looking-at ">?\\(\\s-+\\)\\([^\"()\t\f\n\r]+\\S-\\)(\\([0-9]+\\))\\S-"))
    (let* ((file (match-string-no-properties 2))
           (line-num (match-string-no-properties 3))
           (but-label (concat file ":" line-num)))
      (setq line-num (string-to-number line-num))
      (ibut:label-set but-label (match-beginning 2) (match-end 2))
      (hact 'link-to-file-line file line-num))))

(defib debugger-source ()
  "Jump to source line associated with stack frame or breakpoint lines.
This works with JavaScript and Python tracebacks, gdb, dbx, and
xdb.  Such lines are recognized in any buffer."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Python pdb or traceback, pytype error
     ((progn (ibut:label-set "temp") ;; Real value set in action call below
	     (hib-python-traceback)))

     ;; JavaScript traceback
     ((or (looking-at "[a-zA-Z0-9-:.()? ]+? +at \\([^() \t]+\\) (\\([^:, \t()]+\\):\\([0-9]+\\):\\([0-9]+\\))$")
          (looking-at "[a-zA-Z0-9-:.()? ]+? +at\\( \\)\\([^:, \t()]+\\):\\([0-9]+\\):\\([0-9]+\\)$")
          (looking-at "[a-zA-Z0-9-:.()? ]+?\\( \\)\\([^:, \t()]+\\):\\([0-9]+\\)\\(\\)$"))
      (let* ((file (match-string-no-properties 2))
             (line-num (match-string-no-properties 3))
             (col-num (match-string-no-properties 4))
             but-label)

        ;; For Meteor app errors, remove the "app/" prefix which
        ;; is part of the build subdirectory and not part of the
        ;; source tree.
        (when (and (not (string-equal col-num "")) (string-match "^app/" file))
          (setq file (substring file (match-end 0))))

        (setq but-label (concat file ":" line-num)
              line-num (string-to-number line-num))
        (ibut:label-set but-label)
        (hact 'link-to-file-line file line-num)))

     ;; GDB or WDB
     ((looking-at
       ".+ \\(at\\|file\\) \\([^ :,]+\\)\\(:\\|, line \\)\\([0-9]+\\)\\.?$")
      (let* ((file (match-string-no-properties 2))
             (line-num (match-string-no-properties 4))
             (but-label (concat file ":" line-num))
             (gdb-last-file (or (and (boundp 'gud-last-frame)
                                     (stringp (car gud-last-frame))
                                     (car gud-last-frame))
                                (and (boundp 'gdb-last-frame)
                                     (stringp (car gdb-last-frame))
                                     (car gdb-last-frame)))))
        (setq line-num (string-to-number line-num))
        ;; The `file' typically has no directory component and so may
        ;; not be resolvable.  `gdb-last-file' is the last file
        ;; displayed by gdb.  Use its directory if available as a best
        ;; guess.
        (when gdb-last-file
          (setq file (expand-file-name file (file-name-directory gdb-last-file))))
        (ibut:label-set but-label)
        (hact 'link-to-file-line file line-num)))

     ;; XEmacs assertion failure
     ((looking-at ".+ (file=[^\"\n\r]+\"\\([^\"\n\r]+\\)\", line=\\([0-9]+\\),")
      (let* ((file (match-string-no-properties 1))
             (line-num (match-string-no-properties 2))
             (but-label (concat file ":" line-num)))
        (setq line-num (string-to-number line-num))
        (ibut:label-set but-label)
        (hact 'link-to-file-line file line-num)))

     ;; New DBX
     ((looking-at ".+ line \\([0-9]+\\) in \"\\([^\"]+\\)\"$")
      (let* ((file (match-string-no-properties 2))
             (line-num (match-string-no-properties 1))
             (but-label (concat file ":" line-num)))
        (setq line-num (string-to-number line-num))
        (ibut:label-set but-label)
        (hact 'link-to-file-line file line-num)))

     ;; Old DBX and HP-UX xdb
     ((or (looking-at ".+ \\[\"\\([^\"]+\\)\":\\([0-9]+\\),") ;; Old DBX
          (looking-at ".+ \\[\\([^: ]+\\): \\([0-9]+\\)\\]")) ;; HP-UX xdb
      (let* ((file (match-string-no-properties 1))
             (line-num (match-string-no-properties 2))
             (but-label (concat file ":" line-num)))
        (setq line-num (string-to-number line-num))
        (ibut:label-set but-label)
        (hact 'link-to-file-line file line-num))))))

;;; ========================================================================
;;; Jumps to source of Emacs Lisp byte-compiler error messages.
;;; ========================================================================

(defib elisp-compiler-msg ()
  "Jump to definition of an Emacs Lisp symbol in an error or test message.
The message may come from the Emacs byte compiler, the Emacs Lisp native
compiler or the Emacs regression test system (ERT).
This works when activated anywhere within file line references."
  (when (or (member (buffer-name) '("*Compile-Log-Show*" "*Compile-Log*"
                                    "*compilation*" "*Async-native-compile-log*" "*ert*"))
            (save-excursion
              (and (re-search-backward "^[^ \t\n\r]" nil t)
                   (looking-at "While compiling\\|In \\([^ \n]+\\):$"))))
    (let ((case-fold-search t)
	  src buffer-p label start-end lbl-start-end)
      (or
       ;; Emacs Regression Test (ERT) output lines
       (when (or (save-excursion
		   (forward-line 0)
		   (or (looking-at "\\s-+\\(passed\\|failed\\|skipped\\)\\s-+\\(?:[0-9]+/[0-9]+\\s-+\\)\\(\\S-+\\)")
		       (looking-at "\\(Test\\)\\s-+\\(\\S-+\\)\\s-+\\(backtrace\\|condition\\):")))
		 ;; Handle symbols and pathnames in a backtrace from an ERT test exception
		 (save-match-data
		   (and (save-excursion
			  (re-search-backward "^$\\|^Test \\(\\S-+\\)\\s-+\\(backtrace\\|condition\\):" nil t)
			  (looking-at "Test "))
			(or
			 ;; Handle double-quoted pathnames
			 (and (setq lbl-start-end (hpath:delimited-possible-path nil t)
				    label (nth 0 lbl-start-end))
			      (ibut:label-set label (nth 1 lbl-start-end) (nth 2 lbl-start-end)))
			 ;; Handle symbols
			 (and (setq label (thing-at-point 'symbol)
				    start-end (bounds-of-thing-at-point 'symbol))
			      (ibut:label-set label (car start-end) (cdr start-end)))))))
         (unless label
	   (setq label (match-string-no-properties 2))
	   (ibut:label-set label (match-beginning 2) (match-end 2)))
	 (if (hpath:is-p label)
	     (hact 'link-to-file label)
           ;; Remove prefix generated by actype and ibtype definitions.
           (setq label (replace-regexp-in-string "[^:]+::" "" label nil t))
           (hact 'smart-tags-display label nil)))
       ;; GNU Emacs Byte Compiler
       (and (save-excursion
              (re-search-backward
               "^While compiling [^\t\n]+ in \\(file\\|buffer\\) \\([^ \n]+\\):$"
               nil t))
            (setq buffer-p (equal (match-string-no-properties 1) "buffer")
                  src (match-string-no-properties 2))
            (save-excursion
              (end-of-line)
              (re-search-backward "^While compiling \\([^ \n]+\\)\\(:$\\| \\)"
                                  nil t))
            (progn
              (setq label (match-string-no-properties 1))
              (ibut:label-set label (match-beginning 1) (match-end 1))
              ;; Remove prefix generated by actype and ibtype definitions.
              (setq label (replace-regexp-in-string "[^:]+::" "" label nil t))
              (hact 'link-to-regexp-match
                    (concat "^\(def[a-z \t]+" (regexp-quote label)
                            "[ \t\n\r\(]")
                    1 src buffer-p)))
       ;; GNU Emacs Native Compiler
       (and (save-excursion
              (re-search-backward "^Compiling \\([^ \n]+\\)\\.\\.\\.$" nil t))
            (setq buffer-p nil
                  src (match-string-no-properties 1))
            (save-excursion
              (end-of-line)
              (re-search-backward "^In \\([^ \n]+\\):$" nil t))
            (progn
              (setq label (match-string-no-properties 1))
              (ibut:label-set label (match-beginning 1) (match-end 1))
              ;; Remove prefix generated by actype and ibtype definitions.
              (setq label (replace-regexp-in-string "[^:]+::" "" label nil t))
              (hact 'link-to-regexp-match
                    (concat "^\(def[a-z \t]+" (regexp-quote label)
                            "[ \t\n\r\(]")
                    1 src buffer-p)))
       ;; InfoDock and XEmacs
       (and (save-excursion
              (re-search-backward
               "^Compiling \\(file\\|buffer\\) \\([^ \n]+\\) at "
               nil t))
            (setq buffer-p (equal (match-string-no-properties 1) "buffer")
                  src (match-string-no-properties 2))
            (save-excursion
              (end-of-line)
              (re-search-backward "^While compiling \\([^ \n]+\\)\\(:$\\| \\)"
                                  nil t))
            (progn
              (setq label (match-string-no-properties 1))
              (ibut:label-set label (match-beginning 1) (match-end 1))
              ;; Remove prefix generated by actype and ibtype definitions.
              (setq label (replace-regexp-in-string "[^:]+::" "" label nil t))
              (hact 'link-to-regexp-match
                    (concat "^\(def[a-z \t]+" (regexp-quote label)
                            "[ \t\n\r\(]")
                    1 src buffer-p)))))))

;;; ========================================================================
;;; Jumps to source associated with a line of output from `patch'.
;;; ========================================================================

(defib patch-msg ()
  "Jump to source code associated with output from the `patch' program.
Patch applies diffs to source code."
  (when (save-excursion
          (beginning-of-line)
          (looking-at "Patching \\|Hunk "))
    (let ((opoint (point))
          (file) line)
      (beginning-of-line)
      (cond ((looking-at "Hunk .+ at \\([0-9]+\\)")
             (setq line (match-string-no-properties 1))
             (ibut:label-set line (match-beginning 1) (match-end 1))
             (if (re-search-backward "^Patching file \\(\\S +\\)" nil t)
                 (setq file (match-string-no-properties 1))))
            ((looking-at "Patching file \\(\\S +\\)")
             (setq file (match-string-no-properties 1)
                   line "1")
             (ibut:label-set file (match-beginning 1) (match-end 1))))
      (goto-char opoint)
      (when file
        (setq line (string-to-number line))
        (hact 'link-to-file-line file line)))))

;;; ========================================================================
;;; Displays Texinfo or Info node associated with Texinfo @xref, @pxref or @ref at point.
;;; ========================================================================

(defib texinfo-ref ()
  "Display Texinfo, Info node or help associated with Texinfo constructs at point.
Supported Texinfo constructs are node, menu item, @xref, @pxref,
@ref, @code, @findex, @var or @vindex.

If point is within the braces of a cross-reference, the associated
Info node is shown.  If point is to the left of the braces but after
the @ symbol and the reference is to a node within the current
Texinfo file, then the Texinfo node is shown.

For @code, @findex, @var and @vindex references, the associated
documentation string is displayed."
  (when (memq major-mode '(texinfo-mode para-mode))
    (let ((opoint (point))
          (bol (save-excursion (beginning-of-line) (point))))
      (cond ((save-excursion
               (beginning-of-line)
               ;; If a menu item, display the node for the item.
               (looking-at "*\\s-+\\([^:\t\n\r]+\\)::"))
             (hact 'link-to-texinfo-node
                   nil
                   (ibut:label-set (match-string-no-properties 1) (match-beginning 1) (match-end 1))))
            ;; Show doc for any Emacs Lisp identifier references,
            ;; marked with @code{} or @var{}.
            ((save-excursion
               (and (search-backward "@" bol t)
                    (or (looking-at "@\\(code\\|var\\){\\([^\} \t\n\r]+\\)}")
                        (looking-at "@\\(findex\\|vindex\\)[ ]+\\([^\} \t\n\r]+\\)"))
                    (>= (match-end 2) opoint)))
             (let ((type-str (match-string-no-properties 1))
                   (symbol (intern-soft (ibut:label-set (match-string-no-properties 2) (match-beginning 2) (match-end 2)))))
               (when (and symbol (pcase type-str
                                   ((or "code" "findex") (fboundp symbol))
                                   ((or "var" "vindex") (boundp symbol))))
                 (hact 'link-to-elisp-doc `',symbol))))
            ;; If at an @node and point is within a node name reference
            ;; other than the current node, display it.
            ((save-excursion
               (and (save-excursion (beginning-of-line) (looking-at "@node\\s-+[^,\n\r]+,"))
                    (search-backward "," bol t)
                    (looking-at ",\\s-*\\([^,\n\r]*[^, \t\n\r]\\)[,\n\r]")))
             (hact 'link-to-texinfo-node
                   nil
                   (ibut:label-set (match-string-no-properties 1) (match-beginning 1) (match-end 1))))
            ((save-excursion
               (and (search-backward "@" bol t)
                    (looking-at
                     (concat
                      "@p?x?ref\\({\\)\\s-*\\([^,}]*[^,} \t\n\r]\\)\\s-*"
                      "\\(,[^,}]*\\)?\\(,[^,}]*\\)?"
                      "\\(,\\s-*\\([^,}]*[^,} \t\n\r]\\)\\)?[^}]*}"))
                    (> (match-end 0) opoint)))
             (let* ((show-texinfo-node
                     (and
                      ;; Reference to node within this file.
                      (not (match-beginning 6))
                      ;; To the left of the reference opening brace.
                      (<= opoint (match-beginning 1))))
                    (node
                     (save-match-data
                       (if (match-beginning 6)
                           ;; Explicit filename included in reference.
                           (format "(%s)%s"
                                   (match-string-no-properties 6)
                                   (match-string-no-properties 2))
                         ;; Derive file name from the source file name.
                         (let ((nodename (match-string-no-properties 2))
                               (file (file-name-nondirectory (hypb:buffer-file-name))))
                           (if show-texinfo-node
                               nodename
                             (format "(%s)%s"
                                     (if (string-match "\\.[^.]+$" file)
                                         (substring file 0
                                                    (match-beginning 0))
                                       "unspecified file")
                                     nodename)))))))
               (ibut:label-set (match-string-no-properties 0) (match-beginning 0) (match-end 0))
               (if show-texinfo-node
                   (hact 'link-to-texinfo-node nil node)
                 (hact 'link-to-Info-node node))))))))

;;; ========================================================================
;;; Activate any GNUS push-button at point.
;;; ========================================================================

(defib gnus-push-button ()
  "Activate GNUS-specific article push-buttons, e.g. for hiding signatures.
GNUS is a news and mail reader."
  (and (fboundp 'get-text-property)
       (fboundp 'gnus-article-press-button)
       (get-text-property (point) 'gnus-callback)
       (let* ((but (button-at (point)))
	      (but-start (when but (button-start but)))
	      (but-end (when but (button-end but))))
	 (when but
	   (ibut:label-set (buffer-substring-no-properties but-start but-end)
			   but-start but-end)
	   (hact 'gnus-article-press-button)))))

;;; ========================================================================
;;; Displays Info nodes when double quoted "(file)node" button is activated.
;;; ========================================================================

(defib Info-node ()
  "Make a \"(filename)nodename\" button display the associated Info node.
Also make a \"(filename)itemname\" button display the associated Info
index item.  Examples are \"(hyperbole)Implicit Buttons\" and
``(hyperbole)C-c /''.  Pathname formats like:
\"hyperbole.info#Implicit Buttons\" are also accepted.  Activates only
if point is within the first line of the Info reference."
  (let* ((node-ref-and-pos (or ;; HTML
			       (hbut:label-p t "&quot;" "&quot;" t t)
			       ;; Embedded double quotes
			       (hbut:label-p t "\\\"" "\\\"" t t)
			       ;; Double quotes
			       (hbut:label-p t "\"" "\"" t t)
                               ;; Typical GNU Info references; note
                               ;; these are special quote marks, not the
                               ;; standard ASCII characters.
                               (hbut:label-p t "‘" "’" t t)
                               (hbut:label-p t "‘" "’" t t)
                               ;; Regular dual single quotes (Texinfo smart quotes)
                               (hbut:label-p t "``" "''" t t)
                               ;; Regular open and close quotes
                               (hbut:label-p t "`" "'" t t)))
         (ref (car node-ref-and-pos))
         (node-ref (and (stringp ref)
			(setq ref (hpath:to-Info-ref ref))
                        (or (string-match-p "\\`([^\): \t\n\r\f]+)\\'" ref)
                            (string-match-p "\\`([^\): \t\n\r\f]+)[^ :;\"'`]" ref))
			;; Below handle decoding of Info node names in
			;; Hyperbole Help buffer lbl-key: lines,
			;; eliminating excess underscores.
			(save-excursion
			  (beginning-of-line)
			  (when (looking-at "\\s-+lbl-key:\\s-+\"")
			    (setq ref (ibut:key-to-label ref)))
			  t)
                        (hpath:is-p ref nil t))))
    (and node-ref
         (ibut:label-set node-ref-and-pos)
         (hact 'link-to-Info-node node-ref))))

;;; ========================================================================
;;; Makes Hyperbole mail addresses output Hyperbole environment info.
;;; ========================================================================

(defib hyp-address ()
  "Within a mail or news composer, make a Hyperbole support/discussion e-mail.
Hyperbole environment and version information is inserted.  See
also the documentation for `actypes::hyp-config'.

For example, an Action Mouse Key click on <hyperbole-users@gnu.org> in
a mail composer window would activate this implicit button type."
  (when (memq major-mode (list 'mail-mode hmail:composer hnews:composer))
    (let ((addr (thing-at-point 'email t)))
      (cond ((null addr) nil)
            ((member addr '("hyperbole" "hyperbole-users@gnu.org" "bug-hyperbole@gnu.org"))
	     (ibut:label-set addr)
             (hact 'hyp-config))
            ((string-match "\\(hyperbole\\|hyperbole-users@gnu\\.org\\|bug-hyperbole@gnu\\.org\\)\\(-\\(join\\|leave\\|owner\\)\\)" addr)
	     (ibut:label-set addr)
             (hact 'hyp-request))))))

;;; ========================================================================
;;; Makes source entries in Hyperbole reports selectable.
;;; ========================================================================

(defib hyp-source ()
  "Turn source location entries in Hyperbole reports into buttons.
The buttons jump to the associated location.

For example, {C-h h d d C-h h e h o} summarizes the properties of
the explicit buttons in the DEMO file and each button in that
report buffer behaves the same as the corresponding button in the
original DEMO file."
  (save-excursion
    (beginning-of-line)
    (when (looking-at hbut:source-prefix)
      (let ((src (hbut:source)))
        (when src
          (unless (stringp src)
            (setq src (prin1-to-string src)))
          (ibut:label-set src (point) (line-end-position))
          (hact 'hyp-source src))))))

;;; ========================================================================
;;; Executes an angle bracket delimited Hyperbole action, Elisp
;;; function call or display of an Elisp variable and its value.
;;; ========================================================================

;; Allow for parameterized action-types surrounded by angle brackets.
;; For example, <man-show "grep"> should display grep's man page
;; (since man-show is an action type).

(defconst action:start "<"
  "Regexp matching the start of a Hyperbole Emacs Lisp expression to evaluate.")

(defconst action:end ">"
  "Regexp matching the end of a Hyperbole Emacs Lisp expression to evaluate.")

;; Silence the byte-compiler that thinks these actype references
;; should be regular functions.
(declare-function display-boolean  "ext:ignore")
(declare-function display-variable "ext:ignore")
(declare-function display-value    "ext:ignore")

(defib action ()
  "The Action Button type.
At point, activate any of: an Elisp variable, a Hyperbole
action-type, an Elisp function call or an Ert test name
surrounded by <> rather than ().

If an Elisp variable, display a message showing its value.

There may not be any <> characters within the expression.  The
first identifier in the expression must be an Elisp variable,
action type, function symbol to call or test to execute, i.e.
'<'actype-or-elisp-symbol arg1 ... argN '>'.  For example,
<mail nil \"user@somewhere.org\">."
  (let ((hbut:max-len 0)
	(lbl-key (hattr:get 'hbut:current 'lbl-key))
	(name (hattr:get 'hbut:current 'name))
	(start-pos (hattr:get 'hbut:current 'lbl-start))
	(end-pos  (hattr:get 'hbut:current 'lbl-end))
        actype actype-sym action args lbl var-flag)

    ;; Continue only if start-delim is either:
    ;;     at the beginning of the buffer
    ;;     or preceded by a space character or a grouping character
    ;;   and that character after start-delim is:
    ;;     not a whitespace character
    ;;   and end-delim is either:
    ;;     at the end of the buffer
    ;;     or is followed by a space, punctuation or grouping character.
    (when (and lbl-key (or (null (char-before start-pos))
                           (memq (if (char-before start-pos)
				     (char-syntax (char-before start-pos))
				   0)
				 '(?\  ?\> ?\( ?\))))
	       (not (memq (if (char-after (1+ start-pos))
			      (char-syntax (char-after (1+ start-pos)))
			    0)
			  '(?\  ?\>)))
	       (or (null (char-after end-pos))
                   (memq (if (char-after end-pos)
			     (char-syntax (char-after end-pos))
			   0)
			 '(?\  ?\> ?. ?\( ?\)))
                   ;; Some of these characters may have symbol-constituent syntax
                   ;; rather than punctuation, so check them individually.
                   (memq (char-after end-pos) '(?. ?, ?\; ?: ?! ?\' ?\"))))
      (setq lbl (ibut:key-to-label lbl-key))
      ;; Handle $ preceding var name in cases where same name is
      ;; bound as a function symbol
      (when (string-match "\\`\\$" lbl)
        (setq var-flag t
	      lbl (substring lbl 1)))
      (setq actype (if (string-match-p " " lbl) (car (split-string lbl)) lbl)
            actype-sym (intern-soft (concat "actypes::" actype))
	    ;; Must ignore that (boundp nil) would be t here.
            actype (or (and actype-sym
			    (or (fboundp actype-sym) (boundp actype-sym)
				(special-form-p actype-sym))
			    actype-sym)
		       (and (setq actype-sym (intern-soft actype))
			    (or (fboundp actype-sym) (boundp actype-sym)
				(special-form-p actype-sym)
				(ert-test-boundp actype-sym))
			    actype-sym)))
      (when actype
	;; For <hynote> buttons, need to double quote each argument so
	;; 'read' does not change the idstamp 02 to 2.
	(when (and (memq actype '(hy hynote))
		   (string-match-p " " lbl))
	  (setq lbl (replace-regexp-in-string "\"\\(.*\\)\\'" "\\1\""
					      (combine-and-quote-strings
					       (split-string lbl) "\" \""))))
        (setq action (read (concat "(" lbl ")"))
	      args (cdr action))
	;; Ensure action uses an fboundp symbol if executing a
	;; Hyperbole actype.
	(when (and (car action) (symbolp (car action)))
	  (setcar action
		  (or (intern-soft (concat "actypes::" (symbol-name (car action))))
		      (car action))))
	(unless assist-flag
          (cond ((and (symbolp actype) (fboundp actype)
		      (string-match "-p\\'" (symbol-name actype)))
		 ;; Is a function with a boolean result
		 (setq actype #'display-boolean
		       args `(',action)))
		((and (null args) (symbolp actype) (boundp actype)
		      (or var-flag (not (fboundp actype))))
		 ;; Is a variable, display its value as the action
		 (setq args `(',actype)
		       actype #'display-variable))
		((and (null args) (symbolp actype) (ert-test-boundp actype))
		 ;; Is an ert-deftest, display the value from executing it
		 (setq actype #'display-value
		       args `('(hypb-ert-run-test ,lbl))))
		(t
		 ;; All other expressions, display the action result in the minibuffer
		 (setq actype #'display-value
		       args `(',action)))))

	;; Create implicit button object and store in symbol hbut:current.
	(ibut:label-set lbl)
	(ibut:create :name name :lbl-key lbl-key :lbl-start start-pos
		     :lbl-end end-pos :categ 'ibtypes::action :actype actype
		     :args args)

        ;; Necessary so can return a null value, which actype:act cannot.
        (let ((hrule:action
	       (if (eq hrule:action #'actype:identity)
                   #'actype:identity
                 #'actype:eval)))
          (if (eq hrule:action #'actype:identity)
	      `(hact ,actype ,@args)
            `(hact ,actype ,@(mapcar #'eval args))))))))

(defun action:help (hbut)
  "Display documentation for action button at point.
If a boolean function or variable, display its value."
  (interactive
   (list
    (when (hbut:at-p)
      'hbut:current)))
  (when (hbut:is-p hbut)
    (let* ((label (hbut:key-to-label (hattr:get hbut 'lbl-key)))
	   (actype (hattr:get hbut 'actype))
	   (args (hattr:get hbut 'args))
	   (type-help-func))
      (setq actype (or (htype:def-symbol actype) actype))
      (if hbut
	  (progn (setq type-help-func (intern-soft (concat (symbol-name actype) ":help")))
		 (if (functionp type-help-func)
		     (funcall type-help-func hbut)
		   (let ((total (hbut:report hbut)))
		     (when total (hui:help-ebut-highlight))))
		 (when (memq actype '(display-boolean display-variable))
		   (apply #'actype:eval actype args)))
	(error "(action:help): No action button labeled: %s" label)))))

;;; ========================================================================
;;; Activates HyWikiWords with existing HyWiki pages.
;;; Non-existing HyWikiWords are handled by the (load "hywiki") at a low
;;; priority earlier in this file which defines the `hywiki-word' ibtype.
;;; ========================================================================

(defib hywiki-existing-word ()
  "On a HyWikiWord with an existing referent, display the referent."
  (cl-destructuring-bind (wikiword start end)
      (hywiki-referent-exists-p :range)
    (when wikiword
      (unless (or (ibtypes::pathname-line-and-column)
		  (ibtypes::pathname))
	(if (and start end)
	    (ibut:label-set wikiword start end)
	  (ibut:label-set wikiword))
	(hact 'hywiki-find-referent wikiword)))))

;;; ========================================================================
;;; Inserts completion into minibuffer or other window.
;;; ========================================================================

(defib completion ()
  "Insert completion at point into minibuffer or other window."
  (let ((completion (hargs:completion t)))
    (and completion
         (ibut:label-set completion)
         (hact 'completion))))

;;; ========================================================================
;;; Follows Org mode links and radio targets and cycles Org heading views
;;; ========================================================================

;; See `smart-org' in "hui-mouse.el"; this is higher priority than all ibtypes.

;; If you want to to disable ALL Hyperbole support within Org major
;; and minor modes, set the custom option `hsys-org-enable-smart-keys' to nil.

;;; ========================================================================
;;; Resolve merge conflicts in smerge-mode
;;; ========================================================================

(defib smerge ()
  "Act on `smerge-mode' buffer conflicts.
On a merge conflict marker, keep either the upper, both or the lower
version of the conflict."
  (when (bound-and-true-p smerge-mode)
    (let ((op (save-excursion
                (beginning-of-line)
                (cond ((looking-at smerge-end-re) #'smerge-keep-lower)
                      ((looking-at smerge-begin-re) #'smerge-keep-upper)
                      ((looking-at smerge-lower-re) #'smerge-keep-all)))))
      (when op
        (save-excursion
          (ibut:label-set (match-string-no-properties 0) (match-beginning 0) (match-end 0))
          (hact op))))))

(run-hooks 'hibtypes-end-load-hook)
(provide 'hibtypes)

;;; hibtypes.el ends here
