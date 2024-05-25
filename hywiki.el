;;; hywiki.el --- Hyperbole's auto-wikiword note-taking system   -*- lexical-binding: t -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    21-Apr-24 at 22:41:13
;; Last-Mod:     25-May-24 at 16:54:09 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  This is Hyperbole's markup-free personal Wiki system for
;;  note-taking and automatic WikiWord hyperlinking.  A `HyWiki word'
;;  starts with a capitalized letter and contains only upper and
;;  lowercase letters.  `HyWiki pages' are Org or other text mode
;;  files with HyWiki word names (the page name) plus a file-type
;;  suffix which are stored within `hywiki-directory'.
;;
;;  To create a new HyWiki page or to jump to one, simply create an
;;  Org link in any buffer with the prefix "hy:" followed by a
;;  capitalized alpha characters-only WikiWord, e.g. [[hy:Emacs]], and
;;  then press the Action Key on the link to jump to the associated
;;  page; new pages are automatically created.

;;  If you set `hywiki-org-link-type-required' to `nil', then
;;  you don't need the prefix, e.g. [[Emacs]] and existing HyWiki page
;;  names will override Org's standard handling of such links.  To
;;  prevent Org mode's binding of {M-RET} from splitting lines and
;;  creating new headlines when on a HyWiki word whose page has not
;;  yet been created, set `hsys-org-enable-smart-keys' to 't' so that
;;  Hyperbole's Action Key does the right thing in this context.
;;
;;  HyWiki pages are created in `hywiki-directory'.  Within such
;;  pages, WikiWords (the names of HyWiki pages) work without the need
;;  for any delimiters.  Simply type them out, e.g. Emacs and if a
;;  page exists for the word, it is automatically highlighted when:
;;    - a HyWiki page file is read in
;;    - a whitespace character, ')', '}', or Org-mode punctuation/symbol
;;      character is inserted following a HyWiki word
;;    - the Action Key is pressed to activate a HyWiki word button.
;;
;;  HyWiki links can also link to a section headline within a page by
;;  simply following the page name a '#' character and then the
;;  section headline name.  For example, if your Emacs page has a
;;  'Major Modes section, then either [[hy:Emacs#Major Modes]] or
;;  Emacs#Major-Modes will work as a link to that section.  Note that
;;  without the square bracket delimiters, you must convert spaces in
;;  section names to '-' characters.
;;
;;  Although HyWiki creates new pages in Org mode, you can manually
;;  insert pages in Markdown or other text modes within
;;  `hywiki-directory' and then link to them.  You can also change the
;;  default `hywiki-file-suffix' to something else, like ".md" to have
;;  HyWiki use Markdown mode for its pages.  This usage has not yet
;;  been tested though, so use at your own risk.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hasht)
(require 'hpath)
(require 'hui-em-but)
(require 'ol)

(eval-and-compile
  '(when (require 'company nil t)
     (add-to-list 'company-backends 'hywiki-company-hasht-backend)))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom hywiki-word-highlight-flag t
  "Non-nil means automatically highlight non-Org link HyWiki word hyperbuttons."
  :type 'boolean
  :initialize #'custom-initialize-default
  :group 'hyperbole-buttons)

(defvar hywiki-file-suffix ".org"
  "File suffix (including period) to use when creating HyWiki pages.")

(defvar hywiki-directory '"~/hywiki/"
  "Directory in which to find HyWiki page files.")

(defvar hywiki-non-character-commands
  '(;; Org mode
    org-cycle                         ;; TAB
    org-return                        ;; RET, \r
    org-return-and-maybe-indent       ;; C-j, \n
    ;; Markdown mode
    markdown-cycle                    ;; TAB
    markdown-enter-key                ;; RET, \r
    electric-newline-and-maybe-indent ;; C-j, \n
    ;; Global
    newline                           ;; RET, \r
    newline-and-indent                ;; RET, \r
    quoted-insert                     ;; C-q
    )
  "Commands that insert characters but whose input events do not
  arrive as characters or that quote another character for input.")

;; Define the keymap for hywiki-mode.
(defvar hywiki-mode-map nil
  "Keymap for `hywiki-mode'.")

(defconst hywiki-org-link-type "hy"
  "HyWiki string prefix type for Org links.  Excludes trailing colon.")

(defvar hywiki-org-link-type-required t
  "When non-nil, HyWiki Org links must start with `hywiki-org-link-type'.
Otherwise, this prefix is not needed and HyWiki word Org links
override standard Org link lookups.  See \"(org)Internal Links\".")

(defconst hywiki-word-regexp
  "\\<\\([[:upper:]][[:alpha:]]+\\)\\>"
  "Regexp that matches a HyWiki word only.")

(defconst hywiki-word-section-regexp
  "\\(#[^][# \t\n\r\f]+\\)"
  "Regexp that matches a HyWiki word #section extension.
After the first # character, this may contain any non-square-bracket,
non-# and non-whitespace characters.")

(defconst hywiki-word-optional-section-regexp
  (concat hywiki-word-regexp hywiki-word-section-regexp "?")
  "Regexp that matches a HyWiki word with an optional #section.
Section may not contain spaces or square brackets.  Use '-' to
substitute for spaces in the section/headline name.  Grouping 1 is
the HyWiki word and grouping 2 is the #section with the # included.")

(defconst hywiki-word-org-link-regexp
  (concat hywiki-word-regexp "\\(#[^][\n\r\f]+\\)?")
  "Regexp that matches a HyWiki word with an optional #section in an Org link.
Section may not contain spaces or square brackets.  Use '-' to
substitute for spaces in the section/headline name.  Grouping 1 is
the HyWiki word and grouping 2 is the #section with the # included.")

(defface hywiki--word-face
  '((((min-colors 88) (background dark)) (:foreground "orange"))
    (((background dark)) (:background "orange" :foreground "black"))
    (((min-colors 88)) (:foreground "orange"))
    (t (:background "orange")))
  "Face for HyWiki word highlighting."
  :group 'hyperbole-buttons)

(defcustom hywiki-word-face 'hywiki--word-face
  "Hyperbole face for HyWiki word highlighting."
  :type 'face
  :initialize #'custom-initialize-default
  :group 'hyperbole-buttons)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hywiki--buttonize-characters nil
  "String of single character keys bound to `hywiki-buttonize-character-commands'.
Each such key self-inserts before highlighting any prior HyWiki word.")

(defvar hywiki--pages-hasht nil)

;;; ************************************************************************
;;; hywiki minor mode
;;; ************************************************************************

(defun hywiki-buttonize-character-commands ()
  "Turn any HyWikiWord before point into a highlighted Hyperbole button.
Triggered by `post-self-insert-hook' for self-inserting characters."
  (when (and (characterp last-command-event)
	     (seq-find (lambda (c) (= c last-command-event)) hywiki--buttonize-characters))
    (hywiki-highlight-page-name)))

(defun hywiki-buttonize-non-character-commands ()
  "Turn any HyWikiWord before point into a highlighted Hyperbole button.
Triggered by `pre-command-hook' for non-character-commands, e.g. return."
  (when (memq this-command hywiki-non-character-commands)
    (hywiki-highlight-page-name)))

(defun hywiki-get-buttonize-characters ()
  "Return a string of Org self-insert keys that have punctuation/symbol syntax."
  (let (key
	cmd
	key-cmds
	result)
    ;; Org and other text mode self-insert-command bindings are just
    ;; remaps inherited from global-map.  Create key-cmds list of
    ;; parsable (key . cmd) combinations where key may be a
    ;; (start-key . end-key) range of keys.
    (map-keymap (lambda (key cmd) (setq key-cmds (cons (cons key cmd) key-cmds))) (current-global-map))
    (dolist (key-cmd key-cmds (concat (nreverse result)))
      (setq key (car key-cmd)
	    cmd (cdr key-cmd))
      (when (eq cmd 'self-insert-command)
	(cond ((and (characterp key)
		    (= (char-syntax key) ?.))
	       ;; char with punctuation/symbol syntax
	       (setq result (cons key result)))
	      ((and (consp key)
		    (characterp (car key))
		    (characterp (cdr key))
		    (<= (cdr key) 256))
	       ;; ASCII char range, some of which has punctuation/symbol syntax
	       (with-syntax-table org-mode-syntax-table
		 (dolist (k (number-sequence (car key) (cdr key)))
		   (when (memq (char-syntax k) '(?. ?_))
		     (setq result (cons k result)))))))))))

(define-minor-mode hywiki-mode
  "Toggle HyWiki minor mode with \\[hywiki-mode].

The hywiki-mode minor mode auto-highlights and creates implicit
buttons from wiki words.  Any such button jumps to the associated
HyWiki page or associated section when HyWikiWord#section is used.

When hywiki-mode is enabled, the `hywiki-mode' variable is
non-nil.

See the Info documentation at \"(hyperbole)HyWiki\".

\\{hywiki-mode-map}"

  :lighter " HyWiki"
  :keymap hywiki-mode-map
  (if hywiki-mode
      (progn (unless hywiki-mode-map
               (setq hywiki-mode-map (make-sparse-keymap)))
	     ;; Self-insert punct/sym keys that trigger wiki-word
	     ;; highlighting via `hywiki-buttonize-character-commands' in `hywiki-mode'.
	     (unless hywiki--buttonize-characters
	       (setq hywiki--buttonize-characters
		     (concat " \t\r\n\)\]\>\}'" (hywiki-get-buttonize-characters))))
	     (add-hook 'post-self-insert-hook 'hywiki-buttonize-character-commands)
	     (add-hook 'pre-command-hook 'hywiki-buttonize-non-character-commands 95))
    (remove-hook 'post-self-insert-hook 'hywiki-buttonize-character-commands)
    (remove-hook 'pre-command-hook 'hywiki-buttonize-character-commands)))

;;; ************************************************************************
;;; Public Implicit Button and Action Types
;;; ************************************************************************

(defib hywiki ()
  "When on a HyWiki word, display its page and optional section."
  (let* ((page-name (hywiki-at-wikiword)))
    (when page-name
      (ibut:label-set page-name (match-beginning 0) (match-end 0))
      (hywiki-highlight-page-name t)
      (hact 'hywiki-find-page page-name))))

(defun hywiki-find-page (&optional page-name prompt-flag)
  "Display HyWiki PAGE-NAME.  By default, create any non-existent page.
With optional PROMPT-FLAG t, prompt to create if non-existent.  If
PROMPT-FLAG is 'exists, return nil unless the page already exists."
  (interactive (list (completing-read "Find HyWiki page: " (hywiki-get-page-list))))

  (let ((in-page-flag (null page-name))
	(in-hywiki-directory-flag (string-prefix-p (expand-file-name hywiki-directory)
						   (or buffer-file-name ""))))
    ;; If called from `find-file-hook' without a page-name and outside
    ;; hywiki-directory, do nothing (just finding a regular file).
    (when (or (stringp page-name) in-hywiki-directory-flag)
      (when in-page-flag
	;; Current buffer must be the desired page (called from 'find-file-hook')
	(unless in-hywiki-directory-flag
	  (error "(hywiki-find-page): No `page-name'; buffer file must be in `hywiki-directory', not %s"
		 default-directory))
	(when (null buffer-file-name)
	  (error "(hywiki-find-page): No `page-name' given in a buffer without an attached file"))
	(setq page-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))))

      (let* ((section (when (string-match "#" page-name)
			(substring page-name (match-beginning 0))))
	     (page-name (if (string-match "#" page-name)
			    (substring page-name 0 (match-beginning 0))
			  page-name))
	     (page-file (or (hywiki-get-page page-name)
			    (if prompt-flag
				(unless (eq prompt-flag 'exists)
				  (when (y-or-n-p (concat "Create new `" page-name "' page? "))
				    (hywiki-add-page page-name)))
			      (hywiki-add-page page-name)))))
	(when page-file
	  (unless in-page-flag (hpath:find (concat page-file section)))
	  (unless hywiki-mode (hywiki-mode 1))
	  (hywiki-highlight-page-names)
	  (run-hooks 'hywiki-find-page-hook)
	  page-file)))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hywiki-add-to-page (page-name text start-flag)
  "Add to PAGE-NAME TEXT at page start with START-FLAG non-nil, else end.
Create page if it does not exist.  If PAGE-NAME is invalid, return
nil, else return the file name of the page."
  (let* ((page-file (hywiki-add-page page-name))
	 (page-buf  (when page-file (find-file-noselect page-file))))
    (when page-buf
      (save-excursion
	(with-current-buffer page-buf
	  (barf-if-buffer-read-only)
	  (save-restriction
	    (widen)
	    (goto-char (if start-flag (point-min) (point-max)))
	    (unless (bolp) (insert (newline)))
	    (insert text)
	    (unless (bolp) (insert (newline)))
	    (goto-char (if start-flag (point-min) (point-max)))
	    page-file))))))

(defun hywiki-maybe-at-wikiword-beginning ()
  "Return non-nil if previous character is one preceding a HyWiki word.
Does not test whether or not a page exists for the HyWiki word.
Use `hywiki-get-page' to determine whether a HyWiki page exists."
  ;; Ignore wikiwords preceded by any non-whitespace character, except
  ;; any of these: ({"'`'
  (when (or (bolp)
	    (memq (char-before) '(?\( ?\{ ?\" ?\' ?\` ?\  ?\t ?\n ?\r ?\f)))
    t))

(defun hywiki-at-wikiword ()
  "Return HyWiki word and optional #section at point or nil if not on one.
Does not test whether or not a page exists for the HyWiki word.
Use `hywiki-get-page' to determine whether a HyWiki page exists."
  (when hywiki-mode
    (let ((wikiword (ibut:label-p t "[[" "]]")))
      (if wikiword
	  ;; Handle an Org link [[HyWikiWord]] [[hy:HyWikiWord]] or [[HyWikiWord#section]].
	  (progn
	    (setq wikiword
		  (org-link-expand-abbrev (org-link-unescape (string-trim wikiword))))
	    ;; Ignore prefixed, typed hy:HyWikiWord since Org mode will display those.
	    (when (hywiki-is-wikiword wikiword)
	      wikiword))
	;; Handle a HyWiki word with optional #section; if it is an Org
	;; link, it may optionally have a hy: link-type prefix.
	(save-excursion
          (let ((case-fold-search nil))
	    (skip-chars-backward "-*#[:alnum:]")
	    ;; Ignore wikiwords preceded by any non-whitespace
	    ;; character, except any of these: (["'`'
	    (and (hywiki-maybe-at-wikiword-beginning)
		 (looking-at hywiki-word-optional-section-regexp)
		 (string-trim (match-string-no-properties 0)))))))))

;; Globally set these values to avoid using 'let' with stack allocations
;; within `hywiki-highlight-page-name' frequently.
(defvar hywiki--any-page-regexp nil)
(defvar hywiki--but nil)
(defvar hywiki--but-end nil)
(defvar hywiki--but-start nil)
(defvar hywiki--current-page nil)
(defvar hywiki--end nil)
(defvar hywiki--page-name nil)
(defvar hywiki--save-case-fold-search nil)
(defvar hywiki--save-org-link-type-required nil)
(defvar hywiki--start nil)

(defun hywiki-highlight-page-names ()
  "Highlight all non-Org link HyWiki page names in a HyWiki buffer.
Use `hywiki-word-face' to highlight.  Does not highlight references to
the current page unless they have sections attached."
  (interactive)
  ;; Avoid doing any lets for efficiency.
  ;; Highlight HyWiki words in buffers where `hywiki-mode' is enabled
  ;; or with attached files below `hywiki-directory'.
  (when (and hywiki-word-highlight-flag
	     (or hywiki-mode
		 (string-prefix-p (expand-file-name hywiki-directory)
				  (or buffer-file-name ""))))
    (save-excursion
      (save-restriction
	(setq hywiki--any-page-regexp (regexp-opt (hywiki-get-page-list) 'words)
	      hywiki--save-case-fold-search case-fold-search
	      case-fold-search nil
	      hywiki--save-org-link-type-required hywiki-org-link-type-required
	      hywiki-org-link-type-required t
	      hywiki--current-page (hywiki-get-buffer-page-name))
	(widen)
	(goto-char (point-min))
	(while (re-search-forward hywiki--any-page-regexp nil t)
	  (setq hywiki--start (match-beginning 0)
		hywiki--end   (match-end 0))
	  (save-excursion
	    (goto-char hywiki--start)
	    (when (hywiki-maybe-at-wikiword-beginning)
	      ;; Include any #section.
	      (skip-syntax-forward "^-\)$\>._\"\'")
	      (skip-chars-forward "-#[:alnum:]")
	      (setq hywiki--end (point))
	      ;; Don't highlight current-page matches unless they
	      ;; include a #section.
	      (unless (string-equal hywiki--current-page
				    (buffer-substring-no-properties hywiki--start hywiki--end))
		(hproperty:but-add hywiki--start hywiki--end hywiki-word-face)))))))
    (setq case-fold-search hywiki--save-case-fold-search
	  hywiki-org-link-type-required hywiki--save-org-link-type-required)))

(defun hywiki-highlight-page-name (&optional on-page-name)
  "Highlight any non-Org link HyWiki page name one character before point.
With optional ON-PAGE-NAME non-nil, assume point is within the page or
section name.

Use `hywiki-word-face' to highlight.  Does not highlight references to
the current page unless they have sections attached."
  (interactive)
  (when (and hywiki-word-highlight-flag
	     (or on-page-name
		 (and (eq (char-before) last-command-event) ; Sanity check
		      (not (eq ?# last-command-event))
		      (memq (char-syntax last-command-event) '(?\  ?\) ?\$ ?\> ?. ?\" ?\'))))
             (not executing-kbd-macro)
             (not noninteractive))
    (save-excursion
      ;; (when (= (char-syntax (char-before)) ?\))
	;; Clear any HyWikiWord highlighting that may just be a part
	;; of a larger balanced delimiter text with multiple words.
	;; If there is just a single HyWikiWord, it will be
	;; re-highlighted later in this function.
	;; (ignore-errors
	;;   (let* ((sexp-end (point))
	;; 	 (sexp-start (scan-sexps sexp-end -1)))
	;;     (when sexp-start
	;;       (hproperty:but-clear-all-in-list
	;;        (hproperty:but-get-all-in-region sexp-start sexp-end 'face hywiki-word-face))))))

      (unless on-page-name
	;; after page name
	(skip-syntax-backward "-"))
      ;; May be a closing delimiter that we have to skip past
      (skip-chars-backward (regexp-quote hywiki--buttonize-characters))
      ;; Skip pass HyWikiWord or section
      (skip-syntax-backward "^-$()._\"\'")
      (skip-chars-backward "#[:alpha:]")

      (setq hywiki--save-case-fold-search case-fold-search
	    case-fold-search nil
	    hywiki--save-org-link-type-required hywiki-org-link-type-required
	    hywiki-org-link-type-required t)
      (if (and (hywiki-maybe-at-wikiword-beginning)
	       (looking-at hywiki-word-optional-section-regexp)
	       (progn
		 (setq hywiki--page-name (match-string-no-properties 1)
		       hywiki--start (match-beginning 0)
		       hywiki--end   (match-end 0))
		 (and (hywiki-get-page hywiki--page-name)
		      ;; Ignore wikiwords preceded by any non-whitespace character
		      ;; (or (bolp) (memq (preceding-char) '(?\  ?\t)))
		      )))
	  (progn
	    (setq hywiki--current-page (hywiki-get-buffer-page-name))
	    ;; Don't highlight current-page matches unless
	    ;; they include a #section.
	    (unless (string-equal hywiki--current-page
				  (buffer-substring-no-properties hywiki--start hywiki--end))
	      (if (setq hywiki--but (hproperty:but-get (point) 'face hywiki-word-face))
		  (progn
		    (setq hywiki--but-start (hproperty:but-start hywiki--but)
			  hywiki--but-end   (hproperty:but-end hywiki--but))
		    (unless (and (= hywiki--start hywiki--but-start) (= hywiki--end hywiki--but-end))
		      (hproperty:but-delete hywiki--but)
		      (hproperty:but-add hywiki--start hywiki--end hywiki-word-face)))
		(hproperty:but-add hywiki--start hywiki--end hywiki-word-face))))
	;; Remove any potential earlier highlighting since the
	;; previous word may have changed.
	(skip-syntax-backward "^-$()._\"\'")
	(hproperty:but-clear (point) 'face hywiki-word-face)))))

(defun hywiki-is-wikiword (word)
  "Return non-nil if WORD is a HyWiki word and optional #section.
The page for the word may not yet exist.  Use `hywiki-get-page'
to determine whether a HyWiki word page exists."
  (and (stringp word)
       (let (case-fold-search)
	 (or (eq (string-match (concat "\\`" hywiki-word-org-link-regexp "\\'") word)
		 0)
	     (eq (string-match (concat "\\`" hywiki-word-optional-section-regexp "\\'") word)
		 0)))))

(defun hywiki-get-buffer-page-name ()
  "Extract the page name from the buffer file name or else buffer name."
  (file-name-sans-extension (file-name-nondirectory
			     (or buffer-file-name (buffer-name)))))

(defun hywiki-get-page (page-name)
  "Return the absolute path of HyWiki PAGE-NAME or nil if it does not exist."
  (if (and (stringp page-name) (not (string-empty-p page-name))
	   (eq (string-match hywiki-word-org-link-regexp page-name) 0))
      (progn
	(when (match-string-no-properties 2 page-name)
	  ;; Remove any #section suffix in PAGE-NAME.
	  (setq page-name (match-string-no-properties 1 page-name)))

	(or (hash-get page-name (hywiki-get-page-hasht))
	    ;; If page exists but not yet in lookup hash table, add it.
	    (when (file-readable-p (hywiki-get-page-file page-name))
	      (hywiki-add-page page-name))))
    (user-error "(hywiki-get-page): Invalid page name: '%s'; must be capitalized, all alpha" page-name)))

(defun hywiki-get-page-file (page-name)
  "Return possibly non-existent file name for PAGE NAME.
No validation of PAGE-NAME is done."
  (concat (expand-file-name page-name hywiki-directory) hywiki-file-suffix))

(defun hywiki-get-page-files ()
  "Return the list of existing HyWiki page file names.
These may have any alphanumeric file suffix, if files were added manually."
  (when (and (stringp hywiki-directory) (file-readable-p hywiki-directory))
    (directory-files-recursively hywiki-directory (concat "^" hywiki-word-regexp "\\.[A-Za-z0-9]+$"))))

(defun hywiki-get-page-hasht ()
  "Return hash table of existing HyWiki pages."
  (or hywiki--pages-hasht (hywiki-make-pages-hasht)))

(defun hywiki-get-page-list ()
  (hash-map #'cdr (hywiki-get-page-hasht)))

(defun hywiki-add-page (page-name)
  "Add the HyWiki page for PAGE-NAME and return its file.
If file exists already, just return it.  If PAGE-NAME is invalid,
return nil.

Use `hywiki-get-page' to determine whether a HyWiki page exists."
  (if (and (stringp page-name) (not (string-empty-p page-name))
	   (eq (string-match hywiki-word-org-link-regexp page-name) 0))
      (progn
	(when (match-string-no-properties 2 page-name)
	  ;; Remove any #section suffix in PAGE-NAME.
	  (setq page-name (match-string-no-properties 1 page-name)))

	(let ((page-file (hywiki-get-page-file page-name))
	      (pages-hasht (hywiki-get-page-hasht)))
	  (unless (file-readable-p page-file)
	    ;; Create any parent dirs necessary to create empty file
	    (make-empty-file page-file t))
	  (unless (hash-get page-name pages-hasht)
	    (hash-add page-file page-name pages-hasht))
	  page-file))
    (user-error "(hywiki-add-page): Invalid page name: '%s'; must be capitalized, all alpha" page-name)))

(defun hywiki-make-pages-hasht ()
  (let* ((page-files (hywiki-get-page-files))
	 (page-elts (mapcar (lambda (file)
			      (cons file (file-name-sans-extension (file-name-nondirectory file))))
			    page-files)))
    (setq hywiki--pages-hasht (hash-make page-elts))))

(eval-and-compile
'(when (featurep 'company)
(defun hywiki-company-hasht-backend (command &optional _arg &rest ignored)
 "A `company-mode` backend that completes from the keys of a hash table."
 (interactive (list 'interactive))
 (when (hywiki-at-wikiword)
   (pcase command
     ('interactive (company-begin-backend 'company-hash-table-backend))
     ('prefix (company-grab-word))
     ('candidates
      (let ((prefix (company-grab-word)))
	(when prefix 
          (cl-loop for key being the hash-keys in (hywiki-get-page-list)
                   when (string-prefix-p prefix key)
                   collect key))))
     ('sorted t))))))

(defun hywiki-org-link-complete (&optional _arg)
  "Complete HyWiki page names for `org-insert-link'."
  (concat
   (when hywiki-org-link-type-required
     (concat hywiki-org-link-type ":"))
   (let ((completion-ignore-case t))
     (completing-read "HyWiki page: " (hywiki-get-page-list) nil t))))

(defun hywiki-org-link-store ()
  "Store a link to a HyWiki word at point, if any."
  (when (hywiki-at-wikiword)
    (let* ((page-name (hywiki-at-wikiword))
           (link (concat
		  (when hywiki-org-link-type-required
		    (concat hywiki-org-link-type ":"))
		  page-name))
           (description (format "HyWiki page for '%s'" page-name)))
      (org-link-store-props
       :type hywiki-org-link-type
       :link link
       :description description))))

(org-link-set-parameters hywiki-org-link-type
                         :complete #'hywiki-org-link-complete
			 :follow #'hywiki-find-page
			 :store #'hywiki-org-link-store)

(add-hook 'find-file-hook #'hywiki-find-page t)

(provide 'hywiki)
