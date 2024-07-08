;;; hywiki.el --- Hyperbole's auto-wikiword note-taking system     -*- lexical-binding: t -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    21-Apr-24 at 22:41:13
;; Last-Mod:      7-Jul-24 at 23:15:29 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  This is Hyperbole's markup-free personal Wiki system for note-taking
;;  and automatic wiki word highlighting and hyperlinking.  It uses Org
;;  mode for note taking and adds automatic hyperlinking of HyWikiWords
;;  within Org files in `hywiki-directory' (default = "~/hywiki"), where
;;  a HyWikiWord is a capitalized word that contains upper and lowercase
;;  letters only and has a corresponding HyWikiWord.org wiki page file
;;  below `hywiki-directory'.  HyWikiWords require no delimiters.
;;
;;  HyWikiWords are also recognized in text buffers after the global
;;  minor mode, `hywiki-mode' is enabled via {M-x hywiki-mode RET}.  To
;;  create or jump to a HyWiki page, simply type out a potential
;;  HyWikiWord or move point onto one and press the Action Key {M-RET}.
;;  This will create the associated page if it does not exist.  This
;;  also highlights any other instances of HyWikiWords across all
;;  visible Emacs windows.  HyWiki is built for scalability and has been
;;  tested to be performant with 10,000 HyWikiWords.
;;
;;  Once Hyperbole has been loaded and activated, HyWikiWords (with or
;;  without delimiters) are automatically highlighted and active in
;;  the following contexts:
;;    - HyWiki page buffers;
;;    - non-special text buffers, when `hywiki-mode' is enabled;
;;    - comments of programming buffers, when `hywiki-mode' is enabled.
;;  
;;  As HyWikiWords are typed, highlighting occurs after a trailing
;;  whitespace or punctuation character is added, or when an opening
;;  or closing parenthesis or curly brace is added to surround the
;;  HyWikiWord.  Since Org links use square brackets and Org targets
;;  use angle brackets, HyWikiWords within these delimiters are ignored.
;;
;;  You can also create Org links to HyWikiWords in any non-special text
;;  buffer by surrounding them with double square brackets and the
;;  'hy:' prefix, as in: [[hy:MyWikiWord]].  If you set
;;  `hywiki-org-link-type-required' to `nil', then you don't need the
;;  prefix, e.g. [[MyWikiWord]]; existing HyWiki page names then will
;;  override Org's standard handling of such links.  To prevent Org
;;  mode's binding of {M-RET} from splitting lines and creating new
;;  headlines when on a HyWiki word whose page has not yet been
;;  created, set `hsys-org-enable-smart-keys' to `t' so that
;;  Hyperbole's Action Key does the right thing in this context.
;;
;;  HyWikiWord links can also link to a section headline within a page
;;  by simply following the page name with a '#' character and then the
;;  section headline name.  For example, if your Emacs page has a "Major
;;  Modes" section, then either Emacs#Major-Modes or [[hy:Emacs#Major
;;  Modes]] will work as a link to that section.  Note that without the
;;  square bracket delimiters, you must convert spaces in section names
;;  to '-' characters.  As long as the page exists, section links are
;;  highlighted regardless of whether associated sections exist or not.
;;  When activating a link with a section reference, you will get an
;;  error if the section does not exist.
;;
;;  The custom setting, `hywiki-word-highlight-flag' (default = t),
;;  means HyWikiWords will be auto-highlighted within HyWiki pages.
;;  Outside of such pages, `hywiki-mode' must also be enabled for such
;;  auto-highlighting.
;;
;;  The custom setting, `hywiki-exclude-major-modes' (default = nil), is
;;  a list of major modes to exclude from HyWikiWord auto-highlighting
;;  and recognition.
;;
;;  Within programming modes, HyWikiWords are highlighted/hyperlinked
;;  within comments only.  For programming modes in which you want
;;  HyWikiWords recognized everywhere, add them to the custom setting,
;;  `hywiki-highlight-all-in-prog-modes' (default =
;;  '(lisp-interaction-mode)).
;;
;;  HyWiki adds two implicit button types to Hyperbole:
;;    `hywiki-word' creates and displays HyWikiWord pages;
;;    `hywiki-file' displays existing `hywiki-directory' non-HyWikiWord
;;  Org files that have been added manually.  For example, if you
;;  add the file, my-file.org, there, you would be able to jump to it
;;  by adding 'my-file' anywhere that HyWikiWords are recognized.  It
;;  won't be highlighted as it isn't a HyWikiWord but it behaves
;;  similarly when activated.
;;
;;  These are the lowest priority implicit button types so they trigger
;;  only when other types are not recognized first.  The hywiki-word
;;  type is recognized ahead of the hywiki-file type.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'cl-lib)     ;; For `cl-find'
(require 'hargs)
(require 'hbut)       ;; For `hbut:syntax-table'
(require 'hasht)
(require 'hpath)
(require 'hypb)
(require 'hproperty)
(require 'hsys-consult)
(require 'outline)    ;; For `outline-mode-syntax-table'
(require 'thingatpt)

(eval-and-compile
  '(when (require 'company nil t)
     (add-to-list 'company-backends 'hywiki-company-hasht-backend)))

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(defvar org-agenda-buffer-tmp-name)  ;; "org-agenda.el"
(declare-function org-link-store-props "ol" (&rest plist))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom hywiki-word-highlight-flag t
  "The default, non-nil value treats HyWikiWords in HyWiki pages as hyperlinks.
A nil value disables HyWikiWord hyperlink buttons in both HyWiki
pages and all other buffers (since it also disables `hywiki-mode').

Outside of HyWiki pages, the global minor mode `hywiki-mode' must be
manually enabled for auto-HyWikiWord highlighting; programmatically,
use `(hywiki-mode 1) to enable it.

Use `hywiki-active-in-current-buffer-p' to determine if HyWikiWord
hyperlinks are currently active in a buffer or not.

Regardless of this flag, HyWikiWords in Org links and targets are not
highlighted nor treated as hyperlinks; they are handled normally by Org."
  :type 'boolean
  :initialize #'custom-initialize-default
  :group 'hyperbole-hywiki)

(defcustom hywiki-exclude-major-modes nil
  "List of major modes to exclude from HyWiki word highlighting and recognition."
  :type '(list symbol)
  :group 'hyperbole-hywiki)

(defcustom hywiki-highlight-all-in-prog-modes '(lisp-interaction-mode)
  "List of programming major modes to highlight HyWikiWords outside of comments."
  :type '(list symbol)
  :group 'hyperbole-hywiki)

(defcustom hywiki-mode-lighter  " HyWiki"
  "String to display in mode line when the HyWiki global minor mode is enabled.
Use nil for no HyWiki mode indicator."
  :type 'string
  :group 'hyperbole-hywiki)

(defvar hywiki-file-suffix ".org"
  "File suffix (including period) to use when creating HyWiki pages.")

(defvar hywiki-directory '"~/hywiki/"
  "Directory in which to find HyWiki page files.")

(defvar-local hywiki-buffer-highlighted-state nil
  "State of HyWikiWords highlighting in the associated buffer.
\\='h means the buffer was already highlighted;
\\='d means the buffer was dehighlighted;
nil means no full buffer highlighting has occurred.")

(defvar hywiki-non-character-commands
  '(;; Org mode
    org-cycle                         ;; TAB
    org-open-line                     ;; C-o
    org-return                        ;; RET, \r
    org-return-and-maybe-indent       ;; C-j, \n
    ;; Markdown mode
    markdown-cycle                    ;; TAB
    markdown-enter-key                ;; RET, \r
    electric-newline-and-maybe-indent ;; C-j, \n
    ;; Global
    newline                           ;; RET, \r
    newline-and-indent                ;; RET, \r
    open-line                         ;; C-o
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

(defvar-local hywiki-page-flag nil
  "Set to t after a `find-file' of a HyWiki page file, else nil.
The file must be below `hywiki-directory'.

For reference, this is set when `window-buffer-change-functions' calls
`hywiki-maybe-highlight-page-names' which calls `hywiki-in-page-p'.")

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
Section may not contain whitespace or square brackets.  Use '-' to
substitute for spaces in the section/headline name.  Grouping 1 is
the HyWiki word and grouping 2 is the #section with the # included.")

(defconst hywiki-word-with-optional-section-exact-regexp
  (concat "\\`" hywiki-word-regexp "\\(#[^][\n\r\f]+\\)?\\'")
  "Exact match regexp for a HyWiki word with an optional #section.
The section may contain spaces or tabs but not square brackets;
it is preferable, however, to substitute '-' for whitespace in
the section/headline name to simplify recognition.  Grouping 1 is
the HyWiki word and grouping 2 is the #section with the # included.")

(defface hywiki--word-face
  '((((min-colors 88) (background dark)) (:foreground "orange"))
    (((background dark)) (:background "orange" :foreground "black"))
    (((min-colors 88)) (:foreground "orange"))
    (t (:background "orange")))
  "Face for HyWiki word highlighting."
  :group 'hyperbole-hywiki)

(defcustom hywiki-word-face 'hywiki--word-face
  "Hyperbole face for HyWiki word highlighting."
  :type 'face
  :initialize #'custom-initialize-default
  :group 'hyperbole-hywiki)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

;; Must be set after `hywiki-get-buttonize-characters' is defined
(defconst hywiki--buttonize-characters nil
  "String of single character keys bound to `hywiki-buttonize-character-commands'.
Each such key self-inserts before highlighting any prior HyWiki word
in `hywiki-mode'.")

(defconst hywiki--buttonize-character-regexp nil
  "Regexp matching a single separating character following a HyWiki word.")

(defconst hywiki--word-and-buttonize-character-regexp
  "Regexp matching HyWikiWord#section plus a valid word separating character.")

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

(defvar hywiki--pages-directory nil)
(defvar hywiki--pages-hasht nil)

;; Globally set these values to avoid using 'let' with stack allocations
;; within `hywiki-maybe-highlight-page-name' frequently.
(defvar hywiki--any-page-regexp-list nil)
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
  (when (and (characterp last-command-event)
	     (cl-find last-command-event hywiki--buttonize-characters))
    (hywiki-maybe-highlight-page-name)))

(defun hywiki-buttonize-non-character-commands ()
  "Turn any HyWikiWord before point into a highlighted Hyperbole button.
Triggered by `post-command-hook' for non-character-commands, e.g.
return/newline."
  (when (memq this-command hywiki-non-character-commands)
    (hywiki-maybe-highlight-page-name)))

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
  :lighter hywiki-mode-lighter
  :keymap hywiki-mode-map
  :group 'hyperbole-hywiki
  (if hywiki-mode
      ;; enable mode
      (progn
	;; Need hyperbole-mode
	(require 'hyperbole)
	(unless hyperbole-mode
	  (hyperbole-mode 1))
	(unless hywiki-mode-map
          (setq hywiki-mode-map (make-sparse-keymap)))
	;; Next line triggers a call to `hywiki-maybe-highlight-page-names-in-frame'
	(set-variable 'hywiki-word-highlight-flag t))
    ;; disable mode
    ;; Dehighlight HyWikiWords in this buffer when 'hywiki-mode' is
    ;; disabled and this is not a HyWiki page buffer. If this is a
    ;; HyWiki page buffer, then dehighlight when
    ;; `hywiki-word-highlight-flag' is nil.
    (hywiki-maybe-highlight-page-names-in-frame t)))

;;; ************************************************************************
;;; Public Implicit Button and Action Types
;;; ************************************************************************

(defib hywiki-word ()
  "When on a HyWiki word, display its page and optional section."
  (let ((page-name (hywiki-word-at)))
    (when page-name
      (ibut:label-set page-name (match-beginning 0) (match-end 0))
      (hact 'hywiki-find-page page-name))))

(defun hywiki-find-page (&optional page-name prompt-flag)
  "Display HyWiki PAGE-NAME or a regular file with PAGE-NAME nil.
Return the absolute path to any page successfully found; nil if
failed or if displaying a regular file (read in via a `find-file'
call)

By default, create any non-existent page.  With optional
PROMPT-FLAG t, prompt to create if non-existent.  If PROMPT-FLAG
is \\='exists, return nil unless the page already exists.  After
successfully finding a page and reading it into a buffer, run
`hywiki-find-page-hook'."
  (interactive (list (completing-read "Find HyWiki page: " (hywiki-get-page-list))))
  (let ((in-page-flag (null page-name))
	(in-hywiki-directory-flag (hywiki-in-page-p)))
    (if (or (stringp page-name) in-hywiki-directory-flag)
	(progn
	  (when in-page-flag
	    ;; Current buffer must be the desired page
	    (unless in-hywiki-directory-flag
	      (error "(hywiki-find-page): No `page-name' given; buffer file must be in `hywiki-directory', not %s"
		     default-directory))
	    (when (null buffer-file-name)
	      (error "(hywiki-find-page): No `page-name' given; buffer must have an attached file"))
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
	      ;; Ensure highlight any page name at point in case called as a
	      ;; Hyperbole action type
	      (hywiki-maybe-highlight-page-name t)
	      (unless in-page-flag (hpath:find (concat page-file section)))
	      (hywiki-maybe-highlight-page-names)
	      (run-hooks 'hywiki-find-page-hook)
	      page-file)))
      ;; When called from without a page-name and outside
      ;; hywiki-directory, just find as a regular file and use next
      ;; line to highlight HyWikiWords only if buffer was not
      ;; previously highlighted.
      (hywiki-maybe-highlight-page-names))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hywiki-active-in-current-buffer-p ()
  "Return non-nil if HyWiki word links are active in the current buffer."
  (and hywiki-word-highlight-flag
       (not (minibuffer-window-active-p (selected-window)))
       (or (derived-mode-p 'kotl-mode)
	   (not (eq (get major-mode 'mode-class) 'special)))
       (not (apply #'derived-mode-p hywiki-exclude-major-modes))
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

(defun hywiki-at-tags-p (&optional at-tag-flag)
  "Return non-nil if point is in a HyWiki buffer and at Org tags."
  (and (or at-tag-flag (hsys-org-at-tags-p))
       (or (hywiki-in-page-p) (string-prefix-p "*HyWiki Tags*" (buffer-name)))))

;;;###autoload
(defun hywiki-consult-grep (&optional regexp max-matches path-list)
  "Interactively search `hywiki-directory' with a consult package grep command.
Search for optional REGEXP up to MAX-MATCHES in PATH-LIST or `hywiki-directory'.

Use ripgrep (rg) if found, otherwise, plain grep.  Initialize search with
optional REGEXP and interactively prompt for changes.  Limit matches
per file to the absolute value of MAX-MATCHES, if given and not 0.  If
0, match to headlines only (lines that start with a '^[*#]+[ \t]+' regexp)."
  (interactive "i\nP")
  (let* ((grep-includes "--include *.org")
	 (ripgrep-globs "--glob *.org"))
    (hsys-consult-grep grep-includes ripgrep-globs
		       regexp max-matches (or path-list (list hywiki-directory)))))

(defun hywiki-maybe-at-wikiword-beginning ()
  "Return non-nil if previous character is one preceding a HyWiki word.
Does not test whether or not a page exists for the HyWiki word.
Use `hywiki-get-page' to determine whether a HyWiki page exists."
  ;; Ignore wikiwords preceded by any non-whitespace character, except
  ;; any of these: ({<"'`'
  (when (or (bolp) (cl-find (char-before) "\(\{\<\"'`\t\n\r\f "))
    t))

(defun hywiki-word-at ()
  "Return HyWiki word and optional #section at point or nil if not on one.
Does not test whether or not a page exists for the HyWiki word; use
`hywiki-get-page' for that.

A call to `hywiki-active-in-current-buffer-p' must return non-nil or
this will return nil."
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
	;; Ignore wikiwords preceded by any non-whitespace
	;; character, except any of these: "([\"'`'"
	(save-excursion
          (let ((case-fold-search nil))
	    (skip-chars-backward "-_*#[:alnum:]")
	    (when (hywiki-maybe-at-wikiword-beginning)
	      (cond ((looking-at hywiki--word-and-buttonize-character-regexp)
		     (string-trim
		      (buffer-substring-no-properties (match-beginning 0)
						      (1- (match-end 0)))))
		    ((looking-at (concat hywiki-word-with-optional-section-regexp "\\'"))
		     ;; No following char
		     (string-trim
		      (buffer-substring-no-properties (match-beginning 0)
						      (match-end 0))))))))))))

;;;###autoload
(defun hywiki-maybe-dehighlight-page-names (&optional region-start region-end)
  "Deighlight any highlighted HyWiki page names in a HyWiki buffer/region.
With optional REGION-START and REGION-END positions (active region
interactively), limit dehighlighting to the region."
  (interactive (when (use-region-p) (list (region-beginning) (region-end))))
  (unless (or (eq hywiki-buffer-highlighted-state 'd)
	      (hywiki-active-in-current-buffer-p))
    (hproperty:but-clear-all-in-list
     (hproperty:but-get-all-in-region (or region-start (point-min))
				      (or region-end (point-max))
				      'face hywiki-word-face))
    (unless (or region-start region-end)
      (setq hywiki-buffer-highlighted-state 'd))))

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

;;;###autoload
(defun hywiki-tags-view (&optional todo-only match view-buffer-name)
  "Prompt for colon-separated Org tags and display matching HyWiki page sections.
With optional prefix arg TODO-ONLY, limit matches to HyWiki Org
todo items only.  With optional VIEW-BUFFER-NAME, use that rather
than the default, \"*HyWiki Tags*\"."
  (interactive "P")
  (require 'org-agenda)
  (let* ((org-agenda-files (list hywiki-directory))
	 (org-agenda-buffer-name (or view-buffer-name "*HyWiki Tags*"))
	 ;; `org-tags-view' is mis-written to require setting this next
	 ;; tmp-name or it will not properly name the displayed buffer.
	 (org-agenda-buffer-tmp-name org-agenda-buffer-name))
    ;; This prompts for the tags to match and uses `org-agenda-files'.
    (org-tags-view todo-only match)
    (when (equal (buffer-name) org-agenda-buffer-name)
      ;; Set up {C-u r} redo cmd
      (let (buffer-read-only)
	(put-text-property (point-min) (point-max) 'org-redo-cmd
			   `(hywiki-tags-view
			       ,todo-only
			       nil
			       ,org-agenda-buffer-name)))
      (forward-line 2))))

(defun hywiki-highlight-on-yank (_prop-value start end)
  "Used in `yank-handled-properties' called with START and END pos of the text."
  (hywiki-maybe-highlight-page-names start end))

;;;###autoload
(defun hywiki-maybe-highlight-page-name (&optional on-page-name)
  "Highlight any non-Org link HyWiki page#section at or one char before point.
With optional ON-PAGE-NAME non-nil, assume point is within the page or
section name.

If in a programming mode, must be within a comment.
Use `hywiki-word-face' to highlight.  Does not highlight references to
the current page unless they have sections attached."
  (interactive)
  (when (and (hywiki-active-in-current-buffer-p)
	     (if (and (derived-mode-p 'prog-mode)
		      (not (apply #'derived-mode-p hywiki-highlight-all-in-prog-modes)))
		 ;; Non-nil if match is inside a comment
		 (nth 4 (syntax-ppss))
	       t)
	     (or on-page-name
		 (cl-find (char-syntax last-command-event)
			  " _()<>$.\"'"))
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
		   ;; Highlight any HyWikiWords within parens or braces
		   (ignore-errors
		     (goto-char (1- (point)))
		     (let* ((sexp-start (point))
			    (sexp-end (scan-sexps sexp-start 1)))
		       (when sexp-end
			 (hywiki-maybe-highlight-page-names sexp-start sexp-end)))))
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
		   ;; Highlight any HyWikiWords within parens or braces
		   (ignore-errors
		     (let* ((sexp-end (point))
			    (sexp-start (scan-sexps sexp-end -1)))
		       (when sexp-start
			 (hywiki-maybe-highlight-page-names sexp-start sexp-end))))))))

	(unless on-page-name
	  ;; May be a closing delimiter that we have to skip past
	  (skip-chars-backward (regexp-quote hywiki--buttonize-characters)))
	;; Skip past HyWikiWord or section
	(skip-syntax-backward "^-$()<>._\"\'")
	(skip-chars-backward "-_*#[:alpha:]")

	(setq hywiki--save-case-fold-search case-fold-search
	      case-fold-search nil
	      hywiki--save-org-link-type-required hywiki-org-link-type-required
	      hywiki-org-link-type-required t)
	(if (and (hywiki-maybe-at-wikiword-beginning)
		 (looking-at hywiki--word-and-buttonize-character-regexp)
		 (progn
		   (setq hywiki--page-name (match-string-no-properties 1)
			 hywiki--start (match-beginning 0)
			 hywiki--end   (1- (match-end 0)))
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
(defun hywiki-maybe-highlight-page-names (&optional region-start region-end)
  "Highlight each non-Org link HyWiki page#section in a buffer/region.
With optional REGION-START and REGION-END positions (active region
interactively), limit highlighting to the region.

Use `hywiki-word-face' to highlight.  Do not highlight references to
the current page unless they have sections attached.

Dehighlight buffers other than HyWiki pages when `hywiki-mode' is
disabled.  Highlight/dehighlight HyWiki page buffers when
`hywiki-word-highlight-flag' is changed."
  (interactive (when (use-region-p) (list (region-beginning) (region-end))))
  ;; Avoid doing any lets for efficiency.
  ;; Highlight HyWiki words in buffers where `hywiki-mode' is enabled
  ;; or with attached files below `hywiki-directory'.
  (if (hywiki-active-in-current-buffer-p)
      (unless (eq hywiki-buffer-highlighted-state 'h)
	(unwind-protect
	    (save-excursion
	      (save-restriction
		(when (or (null hywiki--any-page-regexp-list)
			  (hywiki-directory-modified-p))
		  ;; Compute these expensive regexps (matching 50
		  ;; hywiki words at a time) only if `hywiki-directory'
		  ;; mod time has changed.
		  (setq hywiki--any-page-regexp-list
			(mapcar (lambda (page-sublist)
				  (concat (regexp-opt page-sublist 'words)
					  hywiki--buttonize-character-regexp))
				(hypb:split-seq-into-sublists
				 (hywiki-get-page-list) 50))
			hywiki--directory-mod-time (hywiki-directory-get-mod-time)))
		(setq hywiki--save-case-fold-search case-fold-search
		      case-fold-search nil
		      hywiki--save-org-link-type-required hywiki-org-link-type-required
		      hywiki-org-link-type-required t
		      hywiki--current-page (hywiki-get-buffer-page-name))
		(if (and region-start region-end)
		    (narrow-to-region region-start region-end)
		  (widen))
		(dolist (hywiki-words-regexp hywiki--any-page-regexp-list)
		  (goto-char (point-min))
		  (let ((highlight-in-comments-only
			 (and (derived-mode-p 'prog-mode)
			      (not (apply #'derived-mode-p hywiki-highlight-all-in-prog-modes)))))
		    (while (re-search-forward hywiki-words-regexp nil t)
		      (when (if highlight-in-comments-only
				;; Non-nil if match is inside a comment
				(nth 4 (syntax-ppss))
			      t)
			(setq hywiki--start (match-beginning 1)
			      hywiki--end   (match-end 1))
			(save-excursion
			  (goto-char hywiki--start)
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
			      (hproperty:but-add hywiki--start hywiki--end hywiki-word-face))))))))

		(let (str-start-end)
		  (goto-char (point-min))
		  (while (search-forward "[" nil t)
		    (when (setq str-start-end (hargs:delimited-p "[" "]" nil nil t))
		      (setq hywiki--start (nth 1 str-start-end)
			    hywiki--end   (nth 2 str-start-end))
		      ;; Clear any HyWikiWord highlighting that may
		      ;; just be a part of a larger square brackets
		      ;; delimited text with multiple words.
		      (hproperty:but-clear-all-in-list
		       (hproperty:but-get-all-in-region hywiki--start hywiki--end
							'face hywiki-word-face))
		      (goto-char (min (1+ hywiki--end) (point-max)))))

		  (goto-char (point-min))
		  (while (search-forward "<" nil t)
		    (when (setq str-start-end (hargs:delimited-p "<" ">" nil nil t))
		      (setq hywiki--start (nth 1 str-start-end)
			    hywiki--end   (nth 2 str-start-end))
		      ;; Clear any HyWikiWord highlighting that may
		      ;; just be a part of a larger angle brackets
		      ;; delimited text with multiple words.
		      (hproperty:but-clear-all-in-list
		       (hproperty:but-get-all-in-region hywiki--start hywiki--end
							'face hywiki-word-face))
		      (goto-char (min (1+ hywiki--end) (point-max))))))

		(unless (and region-start region-end
			     (or (/= region-start (point-min))
				 (/= region-end   (point-max))))
		  (setq hywiki-buffer-highlighted-state 'h))))
	  (setq case-fold-search hywiki--save-case-fold-search
		hywiki-org-link-type-required hywiki--save-org-link-type-required))
	)

    ;; Otherwise, dehighlight HyWikiWords in this buffer when
    ;; 'hywiki-mode' is disabled and this is not a HyWiki page
    ;; buffer. If this is a HyWiki page buffer, then dehighlight
    ;; when `hywiki-word-highlight-flag' is nil.
    (hywiki-maybe-dehighlight-page-names region-start region-end)))

(defun hywiki-maybe-highlight-page-names-in-frame (frame)
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
       (hywiki-maybe-highlight-page-names)))
   nil frame))

(defun hywiki-in-page-p ()
  "Return non-nil if the current buffer is a HyWiki page.
If this is a HyWiki page and `hywiki-word-highlight-flag' is non-nil
\(the default), also enable auto-highlighting of HyWiki words as they
are typed in the buffer."
  (or hywiki-page-flag
      (when (string-prefix-p (expand-file-name hywiki-directory)
			     (or buffer-file-name ""))
	(setq hywiki-page-flag t))))

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

(defun hywiki-get-files ()
  "Return the list of existing HyWiki files ending with `hywiki-file-suffix'.
This includes both HyWiki page files and others.  File names returned are
relative to `hywiki-directory'."
  (when (stringp hywiki-directory)
    (make-directory hywiki-directory t)
    (when (file-readable-p hywiki-directory)
      (directory-files
       hywiki-directory nil
       (concat "^[^#]+" (regexp-quote hywiki-file-suffix) "$")))))

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
	    (when (file-readable-p (hywiki-get-file page-name))
	      (hywiki-add-page page-name))))
    (user-error "(hywiki-get-page): Invalid page name: '%s'; must be capitalized, all alpha" page-name)))

(defun hywiki-get-file (file-stem-name)
  "Return possibly non-existent path in `hywiki-directory' from FILE-STEM-NAME.
No validation of FILE-STEM-NAME is done."
  (make-directory hywiki-directory t)
  ;; Remove any #section from `file-stem-name'
  (setq file-stem-name (if (string-match "#" file-stem-name)
			   (substring file-stem-name 0 (match-beginning 0))
			 file-stem-name))
  (if (string-suffix-p hywiki-file-suffix file-stem-name)
      (expand-file-name file-stem-name hywiki-directory)
    (concat (expand-file-name file-stem-name hywiki-directory) hywiki-file-suffix)))

(defun hywiki-get-page-files ()
  "Return the list of existing HyWiki page file names.
These must end with `hywiki-file-suffix'."
  (when (stringp hywiki-directory)
    (make-directory hywiki-directory t)
    (when (file-readable-p hywiki-directory)
      (directory-files-recursively
       hywiki-directory (concat "^" hywiki-word-regexp
				(regexp-quote hywiki-file-suffix) "$")))))

(defun hywiki-get-page-hasht ()
  "Return hash table of existing HyWiki pages."
  (if (equal hywiki--pages-directory hywiki-directory)
      (or hywiki--pages-hasht (hywiki-make-pages-hasht))
    (hywiki-make-pages-hasht)))

(defun hywiki-get-page-list ()
  (hash-map #'cdr (hywiki-get-page-hasht)))

(defun hywiki-add-page (page-name)
  "Add the HyWiki page for PAGE-NAME and return its file.
If file exists already, just return it.  If PAGE-NAME is invalid,
return nil.

Use `hywiki-get-page' to determine whether a HyWiki page exists."
  (if (and (stringp page-name) (not (string-empty-p page-name))
	   (hywiki-is-wikiword page-name))
      (progn
	(when (match-string-no-properties 2 page-name)
	  ;; Remove any #section suffix in PAGE-NAME.
	  (setq page-name (match-string-no-properties 1 page-name)))

	(let ((page-file (hywiki-get-file page-name))
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
    (setq hywiki--pages-directory hywiki-directory
	  hywiki--pages-hasht (hash-make page-elts))))

(eval-and-compile
  '(when (featurep 'company)
     (defun hywiki-company-hasht-backend (command &optional _arg &rest ignored)
       "A `company-mode` backend that completes from the keys of a hash table."
       (interactive (list 'interactive))
       (when (hywiki-word-at)
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
  (when (hywiki-word-at)
    (let* ((page-name (hywiki-word-at))
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

(defun hywiki-word-highlight-flag-changed (symbol set-to-value operation _where)
  "Watch function for variable ``hywiki-word-highlight-flag'.
Function is called with 4 arguments: (SYMBOL SET-TO-VALUE OPERATION WHERE).
Highlight/dehighlight HyWiki page names across all frames on change."
  (unless (memq operation '(let unlet)) ;; not setting global valNue
    (set symbol set-to-value)
    (if set-to-value
	;; enabled
	(progn (add-hook 'post-self-insert-hook 'hywiki-buttonize-character-commands)
	       (add-hook 'post-command-hook     'hywiki-buttonize-non-character-commands 95)
	       (add-hook 'window-buffer-change-functions
			 'hywiki-maybe-highlight-page-names-in-frame)
	       (add-to-list 'yank-handled-properties
			    '(hywiki-word-face . hywiki-highlight-on-yank))
	       (hywiki-maybe-highlight-page-names-in-frame t))
      ;; disabled
      (remove-hook 'post-self-insert-hook 'hywiki-buttonize-character-commands)
      (remove-hook 'post-command-hook     'hywiki-buttonize-non-character-commands)
      (hywiki-mode 0) ;; also dehighlights HyWiki words outside of HyWiki pages
      (remove-hook 'window-buffer-change-functions
		   'hywiki-maybe-highlight-page-names-in-frame)
      (hywiki-maybe-highlight-page-names-in-frame t)
      (setq yank-handled-properties
	    (delete '(hywiki-word-face . hywiki-highlight-on-yank)
		    yank-handled-properties)))))

;;; ************************************************************************
;;; Private initializations
;;; ************************************************************************

;; Must be set after `hywiki-get-buttonize-characters' is defined
(unless hywiki--buttonize-characters
  (setq hywiki--buttonize-characters
	(concat "[]()<>{} \t\r\n'" (hywiki-get-buttonize-characters))
	hywiki--buttonize-character-regexp
	(concat "[]["
		(regexp-quote (substring hywiki--buttonize-characters 2))
		"]")
	hywiki--word-and-buttonize-character-regexp
	(concat hywiki-word-with-optional-section-regexp
		hywiki--buttonize-character-regexp)))

;;; ************************************************************************
;;; Public initializations
;;; ************************************************************************

(add-variable-watcher 'hywiki-word-highlight-flag
		      'hywiki-word-highlight-flag-changed)

;; Sets HyWiki page auto-HyWikiWord highlighting and `yank-handled-properties'
(hywiki-word-highlight-flag-changed 'hywiki-word-highlight-flag
				    hywiki-word-highlight-flag 'set nil)

(provide 'hywiki)

;;; hywiki.el ends here
