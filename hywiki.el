;;; hywiki.el --- Hyperbole's auto-wikiword note-taking system   -*- lexical-binding: t -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    21-Apr-24 at 22:41:13
;; Last-Mod:     27-May-24 at 00:02:45 by Bob Weiner
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

(require 'cl-lib)     ;; For `cl-find'
(require 'hargs)
(require 'hbut)       ;; For `hbut:syntax-table'
(require 'hasht)
(require 'hpath)
(require 'hui-em-but)
(require 'outline)    ;; For `outline-mode-syntax-table'

(eval-and-compile
  '(when (require 'company nil t)
     (add-to-list 'company-backends 'hywiki-company-hasht-backend)))

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function org-link-store-props "ol" (&rest plist))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom hywiki-word-highlight-flag t
  "Non-nil means automatically highlight non-Org link HyWiki word hyperbuttons."
  :type 'boolean
  :initialize #'custom-initialize-default
  :group 'hyperbole-wiki)

(defcustom hywiki-excluded-major-modes nil
  "List of major modes to exclude from HyWiki word highlighting and recognition."
  :type '(list symbol)
  :group 'hyperbole-wiki)

(defvar hywiki-file-suffix ".org"
  "File suffix (including period) to use when creating HyWiki pages.")

(defvar hywiki-directory '"~/hywiki/"
  "Directory in which to find HyWiki page files.")

(defvar hywiki-highlight-all-in-prog-modes '(lisp-interaction-mode)
  "List of programming major modes to highlight HyWikiWords outside of comments.")

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
  "Keymap for `hywiki-mode'.
Presently, there are no key bindings; this is for future use.")

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

(defconst hywiki-word-with-optional-section-regexp
  (concat hywiki-word-regexp hywiki-word-section-regexp "?")
  "Regexp that matches a HyWiki word with an optional #section.
Section may not contain spaces or square brackets.  Use '-' to
substitute for spaces in the section/headline name.  Grouping 1 is
the HyWiki word and grouping 2 is the #section with the # included.")

(defconst hywiki-word-with-optional-section-exact-regexp
  (concat "\\`" hywiki-word-regexp "\\(#[^][\n\r\f]+\\)?\\'")
  "Exact match regexp for a HyWiki word with an optional #section.
Section may not contain spaces or square brackets.  Use '-' to
substitute for spaces in the section/headline name.  Grouping 1 is
the HyWiki word and grouping 2 is the #section with the # included.")

(defface hywiki--word-face
  '((((min-colors 88) (background dark)) (:foreground "orange"))
    (((background dark)) (:background "orange" :foreground "black"))
    (((min-colors 88)) (:foreground "orange"))
    (t (:background "orange")))
  "Face for HyWiki word highlighting."
  :group 'hyperbole-wiki)

(defcustom hywiki-word-face 'hywiki--word-face
  "Hyperbole face for HyWiki word highlighting."
  :type 'face
  :initialize #'custom-initialize-default
  :group 'hyperbole-wiki)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hywiki--buttonize-characters nil
  "String of single character keys bound to `hywiki-buttonize-character-commands'.
Each such key self-inserts before highlighting any prior HyWiki word.")

(defvar hywiki--directory-mod-time 0
  "Last mod time for `hywiki-directory' or 0 if the value has not been read.")

;; Redefine the `org-mode-syntax-table' for use in `hywiki-get-buttonize-characters'
;; so do not have to load all of Org mode there.
(defvar hywiki--org-mode-syntax-table
  (let ((st (make-syntax-table outline-mode-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "_" st)
    (modify-syntax-entry ?~ "_" st)
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    st)
  "Standard syntax table for Org mode buffers with HyWiki support.")

(defvar hywiki--pages-hasht nil)

;; Globally set these values to avoid using 'let' with stack allocations
;; within `hywiki-highlight-page-name' frequently.
(defvar hywiki--any-page-regexp nil)
(defvar hywiki--buts nil)
(defvar hywiki--but-end nil)
(defvar hywiki--but-start nil)
(defvar hywiki--current-page nil)
(defvar hywiki--end nil)
(defvar hywiki--page-name nil)
(defvar hywiki--save-case-fold-search nil)
(defvar hywiki--save-org-link-type-required nil)
(defvar hywiki--start nil)

;;; ************************************************************************
;;; hywiki minor mode
;;; ************************************************************************

(defun hywiki-buttonize-character-commands ()
  "Turn any HyWikiWord before point into a highlighted Hyperbole button.
Triggered by `post-self-insert-hook' for self-inserting characters."
  (when (and hywiki-word-highlight-flag
	     (characterp last-command-event)
	     (cl-find last-command-event hywiki--buttonize-characters))
    (hywiki-highlight-page-name)))

(defun hywiki-buttonize-non-character-commands ()
  "Turn any HyWikiWord before point into a highlighted Hyperbole button.
Triggered by `pre-command-hook' for non-character-commands, e.g. return."
  (when (and hywiki-word-highlight-flag
	     (memq this-command hywiki-non-character-commands))
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
    (dolist (key-cmd key-cmds (concat (seq-difference (nreverse result)
						      "-_*#" #'=)))
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
	       (with-syntax-table hywiki--org-mode-syntax-table
		 (dolist (k (number-sequence (car key) (cdr key)))
		   (when (memq (char-syntax k) '(?. ?_))
		     (setq result (cons k result)))))))))))

(define-minor-mode hywiki-mode
  "Toggle HyWiki global minor mode with \\[hywiki-mode].

The hywiki-mode minor mode auto-highlights and creates implicit
buttons from wiki words.  Any such button jumps to the associated
HyWiki page or associated section when HyWikiWord#section is used.

When hywiki-mode is enabled, the `hywiki-mode' variable is
non-nil.

See the Info documentation at \"(hyperbole)HyWiki\".

\\{hywiki-mode-map}"
  :global t
  :lighter " HyWiki"
  :keymap hywiki-mode-map
  :group 'hyperbole-wiki
  (if hywiki-mode
      (progn (unless hywiki-mode-map
               (setq hywiki-mode-map (make-sparse-keymap)))
	     ;; Self-insert punct/sym keys that trigger wiki-word
	     ;; highlighting via `hywiki-buttonize-character-commands'
	     ;; in `hywiki-mode'.
	     (unless hywiki--buttonize-characters
	       (setq hywiki--buttonize-characters
		     (concat " \t\r\n()<>[]{}'" (hywiki-get-buttonize-characters))))
	     (add-hook 'post-self-insert-hook 'hywiki-buttonize-character-commands)
	     (add-hook 'pre-command-hook 'hywiki-buttonize-non-character-commands 95))
    (remove-hook 'post-self-insert-hook 'hywiki-buttonize-character-commands)
    (remove-hook 'pre-command-hook 'hywiki-buttonize-character-commands))
  (hywiki-highlight-page-names-in-frame (selected-frame)))

;;; ************************************************************************
;;; Public Implicit Button and Action Types
;;; ************************************************************************

(defib hywiki ()
  "When on a HyWiki word, display its page and optional section."
  (let ((page-name (hywiki-at-wikiword)))
    (when page-name
      (ibut:label-set page-name (match-beginning 0) (match-end 0))
      (hywiki-highlight-page-name t)
      (hact 'hywiki-find-page page-name))))

(defun hywiki-find-page (&optional page-name prompt-flag)
  "Display HyWiki PAGE-NAME or a regular file with PAGE-NAME nil.
Return the absolute path to any page successfully found; nil if failed
or if displaying a regular file.

By default, create any non-existent page.  With optional
PROMPT-FLAG t, prompt to create if non-existent.  If PROMPT-FLAG
is 'exists, return nil unless the page already exists.  After
successfully finding a page and reading it into a buffer, run
`hywiki-find-page-hook'."
  (interactive (list (completing-read "Find HyWiki page: " (hywiki-get-page-list))))
  (let ((in-page-flag (null page-name))
	(in-hywiki-directory-flag (hywiki-in-page-p)))
    ;; If called from `find-file-hook' without a page-name and outside
    ;; hywiki-directory, do nothing (just finding a regular file).
    (if (or (stringp page-name) in-hywiki-directory-flag)
	(progn
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
	      (hywiki-highlight-page-names)
	      (run-hooks 'hywiki-find-page-hook)
	      page-file)))
      (hywiki-highlight-page-names))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hywiki-active-in-current-buffer-p ()
  "Return non-nil if HyWiki word links are active in the current buffer."
  (and hywiki-word-highlight-flag
       (not (apply #'derived-mode-p hywiki-excluded-major-modes))
       (or hywiki-mode (hywiki-in-page-p))))

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
  ;; any of these: ({<"'`'
  (when (or (bolp) (cl-find (char-before) "\(\{\<\"'`\t\n\r\f "))
    t))

(defun hywiki-at-wikiword ()
  "Return HyWiki word and optional #section at point or nil if not on one.
Does not test whether or not a page exists for the HyWiki word.
Use `hywiki-get-page' to determine whether a HyWiki page exists."
  (when (hywiki-active-in-current-buffer-p)
    (let ((wikiword (ibut:label-p t "[[" "]]")))
      (if wikiword
	  ;; Handle an Org link [[HyWikiWord]] [[hy:HyWikiWord]] or [[HyWikiWord#section]].
	  (progn
	    ;; Don't use next line so don't have to load all of Org
	    ;; mode just to check for HyWikiWords; however, disables
	    ;; support for Org mode aliases.
	    ;; (setq wikiword (org-link-expand-abbrev (org-link-unescape (string-trim wikiword))))
	    (setq wikiword (string-trim wikiword))
	    ;; Ignore prefixed, typed hy:HyWikiWord since Org mode will display those.
	    (when (hywiki-is-wikiword wikiword)
	      wikiword))
	;; Handle a HyWiki word with optional #section; if it is an Org
	;; link, it may optionally have a hy: link-type prefix.
	(save-excursion
          (let ((case-fold-search nil))
	    (skip-chars-backward "-_*#[:alnum:]")
	    ;; Ignore wikiwords preceded by any non-whitespace
	    ;; character, except any of these: (["'`'
	    (and (hywiki-maybe-at-wikiword-beginning)
		 (looking-at hywiki-word-with-optional-section-regexp)
		 (string-trim (match-string-no-properties 0)))))))))

;;;###autoload
(defun hywiki-dehighlight-page-names (&optional region-start region-end)
  "Deighlight any highlighted HyWiki page names in a HyWiki buffer/region.
With optional REGION-START and REGION-END positions (active region
interactively), limit dehighlighting to the region."
  (interactive (when (use-region-p) (list (region-beginning) (region-end))))
  (unless (hywiki-active-in-current-buffer-p)
    (hproperty:but-clear-all-in-list
     (hproperty:but-get-all-in-region (or region-start (point-min))
				      (or region-end (point-max))
				      'face hywiki-word-face))))

(defun hywiki-directory-get-mod-time ()
  "Return the last mod time for `hywiki-directory' or 0."
  (if (file-readable-p hywiki-directory)
      (time-convert (file-attribute-modification-time
		     (file-attributes hywiki-directory))
		  'integer)
    0))

(defun hywiki-directory-modified-p ()
  "Return non-nil if `hywiki-directory' has been modified since last read."

  (or (zerop hywiki--directory-mod-time)
      (/= hywiki--directory-mod-time (hywiki-directory-get-mod-time))))

(defun hywiki-highlight-on-yank (_prop-value start end)
  "Used in `yank-handled-properties' called with START and END pos of the text."
  (hywiki-highlight-page-names start end))

;;;###autoload
(defun hywiki-highlight-page-name (&optional on-page-name)
  "Highlight any non-Org link HyWiki page#section one character before point.
With optional ON-PAGE-NAME non-nil, assume point is within the page or
section name.

If in a programming mode, must be within a comment.
Use `hywiki-word-face' to highlight.  Does not highlight references to
the current page unless they have sections attached."
  (interactive)
  (when (and hywiki-word-highlight-flag
	     (if (and (derived-mode-p 'prog-mode)
		      (not (apply #'derived-mode-p hywiki-highlight-all-in-prog-modes)))
		 ;; Non-nil if match is inside a comment
		 (nth 4 (syntax-ppss))
	       t)
	     (or on-page-name
		 (and (eq (char-before) last-command-event) ; Sanity check
		      (cl-find (char-syntax last-command-event)
			       " _()<>$.\"'")))
             (not executing-kbd-macro)
             (not noninteractive))
    (with-syntax-table hbut:syntax-table
      (save-excursion
	(unless on-page-name
	  ;; after page name
	  (skip-syntax-backward "-"))

	(save-excursion
	  (save-restriction
	    ;; Limit sexp checks to a single line for speed since links and
	    ;; targets should be on a single line.
	    (narrow-to-region (line-beginning-position) (line-end-position))
	    (cond ((memq (char-before) '(?\[ ?\<))
		   ;; Clear any HyWikiWord highlighting within square or
		   ;; angle brackets, as this may be a link or target.
		   (ignore-errors
		     (goto-char (1- (point)))
		     (let* ((sexp-start (point))
			    (sexp-end (scan-sexps sexp-start 1)))
		       (when sexp-end
			 (hproperty:but-clear-all-in-list
			  (hproperty:but-get-all-in-region sexp-start sexp-end 'face hywiki-word-face))))))
		  ((memq (char-before) '(?\( ?\{))
		   ;; Highlight any HyWikiWords within parens or braces.
		   (ignore-errors
		     (goto-char (1- (point)))
		     (let* ((sexp-start (point))
			    (sexp-end (scan-sexps sexp-start 1)))
		       (when sexp-end
			 (hywiki-highlight-page-names sexp-start sexp-end)))))
		  ((memq (char-before) '(?\] ?\>))
		   ;; Clear any HyWikiWord highlighting within square or
		   ;; angle brackets, as this may be a link or target.
		   (ignore-errors
		     (let* ((sexp-end (point))
			    (sexp-start (scan-sexps sexp-end -1)))
		       (when sexp-start
			 (hproperty:but-clear-all-in-list
			  (hproperty:but-get-all-in-region sexp-start sexp-end 'face hywiki-word-face))))))
		  ((memq (char-before) '(?\) ?\}))
		   ;; Highlight any HyWikiWords within parens or braces.
		   (ignore-errors
		     (let* ((sexp-end (point))
			    (sexp-start (scan-sexps sexp-end -1)))
		       (when sexp-start
			 (hywiki-highlight-page-names sexp-start sexp-end))))))))

	;; May be a closing delimiter that we have to skip past
	(skip-chars-backward (regexp-quote hywiki--buttonize-characters))
	;; Skip past HyWikiWord or section
	(skip-syntax-backward "^-$()<>._\"\'")
	(skip-chars-backward "-_*#[:alpha:]")

	(setq hywiki--save-case-fold-search case-fold-search
	      case-fold-search nil
	      hywiki--save-org-link-type-required hywiki-org-link-type-required
	      hywiki-org-link-type-required t)
	(if (and (hywiki-maybe-at-wikiword-beginning)
		 (looking-at hywiki-word-with-optional-section-regexp)
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
	      ;; Don't highlight current-page matches unless they
	      ;; include a #section.
	      (unless (string-equal hywiki--current-page
				    (buffer-substring-no-properties hywiki--start hywiki--end))
		(if (setq hywiki--buts (hproperty:but-get-all-in-region
					hywiki--start hywiki--end
					'face hywiki-word-face))
		    (if (> (length hywiki--buts) 1)
			(progn (hproperty:but-clear-all-in-list hywiki--buts)
			       (hproperty:but-add hywiki--start hywiki--end hywiki-word-face))
		      ;; There is only one existing button
		      (setq hywiki--buts (car hywiki--buts)
			    hywiki--but-start (hproperty:but-start hywiki--buts)
			    hywiki--but-end   (hproperty:but-end hywiki--buts))
		      (unless (and (= hywiki--start hywiki--but-start)
				   (= hywiki--end hywiki--but-end))
			(hproperty:but-delete hywiki--buts)
			(hproperty:but-add hywiki--start hywiki--end hywiki-word-face)))
		  (hproperty:but-add hywiki--start hywiki--end hywiki-word-face))))
	  ;; Remove any potential earlier highlighting since the
	  ;; previous word may have changed.
	  (skip-syntax-backward "^-$()<>._\"\'")
	  (hproperty:but-clear-all-in-list
	   (hproperty:but-get-all-in-region (point) (1+ (point))
					    'face hywiki-word-face)))))))

;;;###autoload
(defun hywiki-highlight-page-names (&optional region-start region-end)
  "Highlight each non-Org link HyWiki page#section in a buffer/region.
With optional REGION-START and REGION-END positions (active region
interactively), limit highlighting to the region.

Use `hywiki-word-face' to highlight.  Do not highlight references to
the current page unless they have sections attached.

Dehighlight buffers other than HyWiki pages when `hywiki-mode' is
disabled.  Highlight/dehighlight HyWiki page buffers when `hywiki-word-highlight-flag'
is changed."
  (interactive (when (use-region-p) (list (region-beginning) (region-end))))
  ;; Avoid doing any lets for efficiency.
  ;; Highlight HyWiki words in buffers where `hywiki-mode' is enabled
  ;; or with attached files below `hywiki-directory'.
  (if (hywiki-active-in-current-buffer-p)
      (unwind-protect
	  (save-excursion
	    (save-restriction
	      (when (or (null hywiki--any-page-regexp)
			(hywiki-directory-modified-p))
		;; Compute this expensive regexp only if `hywiki-directory' mod time has changed.
		(setq hywiki--any-page-regexp (regexp-opt (hywiki-get-page-list) 'words)
		      hywiki--directory-mod-time (hywiki-directory-get-mod-time)))
	      (setq hywiki--save-case-fold-search case-fold-search
		    case-fold-search nil
		    hywiki--save-org-link-type-required hywiki-org-link-type-required
		    hywiki-org-link-type-required t
		    hywiki--current-page (hywiki-get-buffer-page-name))
	      (if (and region-start region-end)
		  (narrow-to-region region-start region-end)
		(widen))
	      (goto-char (point-min))
	      (let ((highlight-in-comments-only
		     (and (derived-mode-p 'prog-mode)
			  (not (apply #'derived-mode-p hywiki-highlight-all-in-prog-modes)))))
		(while (re-search-forward hywiki--any-page-regexp nil t)
		  (when (if highlight-in-comments-only
			    ;; Non-nil if match is inside a comment
			    (nth 4 (syntax-ppss))
			  t)
		    (setq hywiki--start (match-beginning 0)
			  hywiki--end   (match-end 0))
		    (save-excursion
		      (goto-char hywiki--start)
		      (if (or (hargs:delimited-p "\\[" "\\]" t t t)
			      (hargs:delimited-p "<" ">" t t t))
			  ;; Clear any HyWikiWord highlighting that may
			  ;; just be a part of a larger square brackets or
			  ;; angle brackets delimited text with multiple words.
			  (hproperty:but-clear-all-in-list
			   (hproperty:but-get-all-in-region hywiki--start hywiki--end
							    'face hywiki-word-face))
			;; Otherwise, highlight any HyWikiWord found, including
			;; any #section.
			(when (hywiki-maybe-at-wikiword-beginning)
			  (with-syntax-table hbut:syntax-table
			    (skip-syntax-forward "^-\)$\>._\"\'"))
			  (skip-chars-forward "-_*#[:alnum:]")
			  (setq hywiki--end (point))
			  ;; Don't highlight current-page matches unless they
			  ;; include a #section.
			  (unless (string-equal hywiki--current-page
						(buffer-substring-no-properties hywiki--start hywiki--end))
			    (hproperty:but-add hywiki--start hywiki--end hywiki-word-face))))))))))
	(setq case-fold-search hywiki--save-case-fold-search
	      hywiki-org-link-type-required hywiki--save-org-link-type-required))

    ;; Otherwise, dehighlight buffers other than HyWiki pages when
    ;; 'hywiki-mode' is disabled. Dehighlight HyWiki page
    ;; buffers when `hywiki-word-highlight-flag' is disabled.
    (hywiki-dehighlight-page-names region-start region-end)))

(defun hywiki-highlight-page-names-in-frame (frame)
  "Highlight all non-Org link HyWiki page names displayed in FRAME.
If FRAME is t, then highlight in all windows across all frames, even
invisible ones.

Use `hywiki-word-face' to highlight.  Does not highlight references to
the current page unless they have sections attached."
  (walk-windows
   (lambda (window)
     (with-selected-window window
       ;; Display buffer before `normal-mode' triggers possibly
       ;; long-running font-locking
       (sit-for 0.1)
       (hywiki-highlight-page-names)))
   nil frame))

(defun hywiki-in-page-p ()
  "Return non-nil if the current buffer is a hywiki page."
  (string-prefix-p (expand-file-name hywiki-directory)
		   (or buffer-file-name "")))

(defun hywiki-is-wikiword (word)
  "Return non-nil if WORD is a HyWiki word and optional #section.
The page for the word may not yet exist.  Use `hywiki-get-page'
to determine whether a HyWiki word page exists."
  (and (stringp word)
       (let (case-fold-search)
	 (or (string-match hywiki-word-with-optional-section-exact-regexp word)
	     (eq (string-match (concat "\\`" hywiki-word-with-optional-section-regexp "\\'") word)
		 0)))))

(defun hywiki-get-buffer-page-name ()
  "Extract the page name from the buffer file name or else buffer name."
  (file-name-sans-extension (file-name-nondirectory
			     (or buffer-file-name (buffer-name)))))

(defun hywiki-get-page (page-name)
  "Return the absolute path of HyWiki PAGE-NAME or nil if it does not exist."
  (if (and (stringp page-name) (not (string-empty-p page-name))
	   (string-match hywiki-word-with-optional-section-exact-regexp page-name))
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
  (make-directory hywiki-directory t)
  (concat (expand-file-name page-name hywiki-directory) hywiki-file-suffix))

(defun hywiki-get-page-files ()
  "Return the list of existing HyWiki page file names.
These may have any alphanumeric file suffix, if files were added manually."
  (when (stringp hywiki-directory)
    (make-directory hywiki-directory t)
    (when (file-readable-p hywiki-directory)
      (directory-files-recursively hywiki-directory (concat "^" hywiki-word-regexp "\\.[A-Za-z0-9]+$")))))

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
	   (string-match hywiki-word-with-optional-section-exact-regexp page-name))
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

(eval-after-load 'org
  '(org-link-set-parameters hywiki-org-link-type
                            :complete #'hywiki-org-link-complete
			    :follow #'hywiki-find-page
			    :store #'hywiki-org-link-store))

(add-hook 'find-file-hook #'hywiki-find-page t)
(add-to-list 'window-buffer-change-functions
	     'hywiki-highlight-page-names-in-frame nil 'eq)

(defun hywiki-word-highlight-flag-changed (symbol set-to-value operation _where)
  "Watch function for variable ``hywiki-word-highlight-flag'.
Function is called with 4 arguments: (SYMBOL SET-TO-VALUE OPERATION WHERE).
Highlight/dehighlight HyWiki page names across all frames on change."
  (unless (memq operation '(let unlet)) ;; not setting global valNue
    (set symbol set-to-value)
    (if set-to-value
	(add-to-list 'yank-handled-properties
		     '(hywiki-word-face . hywiki-highlight-on-yank))
      (setq yank-handled-properties
	    (delete '(hywiki-word-face . hywiki-highlight-on-yank)
		    'yank-handled-properties)))
    (hywiki-highlight-page-names-in-frame t)))

(add-variable-watcher 'hywiki-word-highlight-flag
		      'hywiki-word-highlight-flag-changed)

;; Sets `yank-handled-properties'
(hywiki-word-highlight-flag-changed 'hywiki-word-highlight-flag
				    hywiki-word-highlight-flag 'set nil)

(provide 'hywiki)
