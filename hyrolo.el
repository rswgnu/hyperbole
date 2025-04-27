;;; hyrolo.el --- Hierarchical, multi-file, easy-to-use record management system  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     7-Jun-89 at 22:08:29
;; Last-Mod:     27-Apr-25 at 11:12:58 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1991-2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  This is Hyperbole's advanced rolo system, HyRolo, for convenient
;;  management of hierarchical, record-oriented information.  Most
;;  often this is used for contact management but it can quickly be
;;  adapted to most any record-oriented lookup task, for fast, full-text
;;  retrieval.
;;
;;  See all the autoloaded functions herein for interactive commands.
;;  See the Info manual entry "(hyperbole)HyRolo" for usage information.
;;
;;  Note that for Markdown files, HyRolo supports only the modern standard
;;  of headlines that start with '#' characters, not the older, setext
;;  style of underlining headers with '=' or '-' characters.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'custom)   ;; For `defface'
(require 'hversion)
(require 'hmail)
(require 'hsys-consult)
(require 'hsys-org) ;; For `hsys-org-cycle-bob-file-list'
(require 'hypb)     ;; For `hypb:mail-address-regexp' and `hypb:add-to-invisibility-spec'
(eval-when-compile
  `(hyrolo-install-markdown-mode))
(require 'outline)
(require 'package)
(require 'reveal)
;; Avoid any potential library name conflict by giving the load directory.
(require 'set (expand-file-name "set" hyperb:dir))
(require 'sort)
(require 'xml)
(declare-function kotl-mode:to-valid-position "kotl/kotl-mode")

;; Quiet byte compiler warnings for these free variables.
(eval-when-compile
  (unless (require 'bbdb nil t)
    (defvar bbdb-file nil))
  (unless (require 'google-contacts nil t)
    (defvar google-contacts-buffer-name nil))
  (require 'kview nil t))

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function google-contacts  "ext:google-contacts")
(declare-function google-contacts-add-margin-to-text "ext:google-contacts")
(declare-function google-contacts-build-node-list "ext:google-contacts")
(declare-function google-contacts-data  "ext:google-contacts")
(declare-function google-contacts-make-buffer "ext:google-contacts")
(declare-function google-contacts-margin-element "ext:google-contacts")
(declare-function google-contacts-oauth-token "ext:google-contacts")
(declare-function helm-org-rifle-files "ext:helm-org-rifle")
(declare-function helm-org-rifle-org-directory "ext:helm-org-rifle")
(declare-function helm-org-rifle-show-full-contents "ext:helm-org-rifle")
(declare-function kotl-mode:to-valid-position "kotl/kotl-mode")
(declare-function org-fold-initialize "org-fold")
(declare-function org-fold-core-set-folding-spec-property "org-fold")

(declare-function outline-apply-default-state "outline")
(declare-function xml-node-child-string "ext:google-contacts")
(declare-function xml-node-get-attribute-type "ext:google-contacts")

(declare-function find-library-name "find-func")
(declare-function hbut:to-key-src "hbut")
(declare-function hui:hbut-act "hui")
(declare-function ibut:at-p "hbut")
(declare-function kcell-view:indent "kotl/kview")
(declare-function kcell-view:level "kotl/kview")

(declare-function hmouse-pulse-line "hui-window")
(declare-function hpath:find "hpath")
(declare-function hpath:expand-list "hpath")
(declare-function hbut:get-key-src "hbut")
(declare-function org-outline-level "org")

(defvar org-directory)                  ; "org.el"
(defvar org-mode-map)                   ; "org-keys.el"
(defvar org-mode-syntax-table)          ; "org.el"
(defvar org-outline-regexp)             ; "org.el"
(defvar org-outline-regexp-bol)         ; "org.el"
(defvar org-agenda-buffer-tmp-name)     ; "org-agenda.el"
(defvar google-contacts-buffer-name)    ; "ext:google-contacts.el"
(defvar hbut:source-prefix)             ; "hbut.el"

;; markdown-mode.el
(defvar markdown-regex-header)
(defvar markdown-hide-markup)
(defvar markdown-nested-imenu-heading-index)
(defvar markdown-indent-function)
(defvar markdown-make-gfm-checkboxes-buttons)
(declare-function markdown--edit-indirect-after-commit-function "ext:markdown-mode")
(declare-function markdown--indent-region "ext:markdown-mode")
(declare-function markdown--inhibit-electric-quote "ext:markdown-mode")
(declare-function markdown-adaptive-fill-function "ext:markdown-mode")
(declare-function markdown-beginning-of-defun "ext:markdown-mode")
(declare-function markdown-end-of-defun "ext:markdown-mode")
(declare-function markdown-fill-forward-paragraph "ext:markdown-mode")
(declare-function markdown-fill-paragraph "ext:markdown-mode")
(declare-function markdown-gfm-checkbox-after-change-function "ext:markdown-mode")
(declare-function markdown-imenu-create-flat-index "ext:markdown-mode")
(declare-function markdown-imenu-create-nested-index "ext:markdown-mode")
(declare-function markdown-line-is-reference-definition-p "ext:markdown-mode")
(declare-function markdown-live-preview-if-markdown "ext:markdown-mode")
(declare-function markdown-live-preview-remove-on-kill "ext:markdown-mode")
(declare-function markdown-make-gfm-checkboxes-buttons "ext:markdown-mode")
;; This next function is replaced by `hyrolo-outline-function'
;; within `hyrolo-cache-set-major-mode'.
(declare-function markdown-outline-level "ext:markdown-mode")
(declare-function markdown-pipe-at-bol-p "ext:markdown-mode")
(declare-function markdown-remove-gfm-checkbox-overlays "ext:markdown-mode")

(defvar google-contacts-expire-time)
(defvar google-contacts-history)
(defvar google-contacts-query-string)
(defvar helm-org-rifle-show-full-contents)
(defvar helm-org-rifle-show-level-stars)
(defvar hproperty:but-emphasize-flag)
(defvar org-fold-core-style)
(defvar org-link--link-folding-spec)
(defvar org-roam-directory)
(defvar plstore-cache-passphrase-for-symmetric-encryption)
(defvar reveal-auto-hide)

(defvar hyrolo--wconfig)
(defvar hyrolo-entry-group-number)
(defvar hyrolo-entry-trailing-space-group-number)
(defvar hyrolo-hdr-format)
(defvar hyrolo-hdr-regexp)
(defvar hyrolo-hdr-prefix-regexp)
(defvar hyrolo-match-regexp)
(defvar hyrolo-mode-map)
(defvar hyrolo-mode-prefix-map)
(defvar hyrolo-mode-syntax-table)
(defvar hyrolo-reveal-ignore-this-command)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hyrolo-boolean-only-flag nil
  "Set to prevent HyRolo from displaying an error buffer when running tests.
Return a boolean only, indicating whether the test passed or not.
See usage in `hyrolo-any-file-type-problem-p'.")

(defconst hyrolo-markdown-suffix-regexp "md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn"
  "Regexp matching Markdown file suffixes.")

(defcustom hyrolo-file-suffix-regexp (concat "\\.\\(kotl?\\|org\\|ou?tl\\|"
					     hyrolo-markdown-suffix-regexp "\\)$")
  "File suffix regexp used to select files to search with HyRolo."
  :type 'string
  :group 'hyperbole-hyrolo)

(defvar hyrolo-auto-mode-alist
  (list (cons (format "\\.\\(%s\\)$" hyrolo-markdown-suffix-regexp)
	      'hyrolo-markdown-mode)
	'("\\.org$" . hyrolo-org-mode)
	'("\\.ou?tl$" . hyrolo-outline-mode))
  "Entries to prepend to `auto-mode-alist' to invoke file modes used by HyRolo.
Typically, these specialized modes speed loading of files used solely
for HyRolo text matches by avoiding the time-consuming initializations
their standard major modes perform.")

(defvar hyrolo-display-buffer "*HyRolo*"
  "Buffer used to display set of last matching rolo entries.")

;; Need to define the group before the defcustom variable so moved it here.
(defgroup hyperbole-hyrolo nil
  "Hyperbole Rolo hierarchical contact manager customizations."
  :group 'hyperbole)

(defcustom hyrolo-google-contacts-flag t
  "Non-nil means search Google Contacts on each hyrolo query.
The google-contact package must be loaded and a gpg encryption
executable must be found as well (for Oauth security)."
  :type 'boolean
  :group 'hyperbole-hyrolo)

(defcustom hyrolo-file-list nil
  "List of files containing hyrolo entries.
The first file should be a user-specific hyrolo file, typically in the home
directory and must have a suffix of either .org (Org mode) or .otl (Emacs
Outline mode).  Other files in the list may use suffixes of .org, .otl, .md
\(Markdown mode) or .kotl (Koutline mode).

A hyrolo-file consists of:
   (1) an optional header beginning with and ending with a line which matches
       `hyrolo-hdr-regexp';
   (2) one or more rolo entries which each begin with
       `hyrolo-hdr-and-entry-regexp' and may be nested."
  :type '(repeat file)
  :initialize #'custom-initialize-default
  :set #'hyrolo-set-file-list
  :group 'hyperbole-hyrolo)

(defun hyrolo-file-list-changed (symbol set-to-value operation _where)
  "Watch function for variable `hyrolo-file-list'.
Function is called with 4 arguments: (SYMBOL SET-TO-VALUE OPERATION WHERE)."
  (if (memq operation '(let unlet)) ;; not setting global value
      (hyrolo-let-file-list symbol set-to-value)
    (hyrolo-set-file-list symbol set-to-value)))

;; This next line is needed to invoke `hyrolo-set-file-list' when
;; `hyrolo-file-list' is changed via `setq' or `let' rather than
;; `customize-set-variable'.
(add-variable-watcher 'hyrolo-file-list #'hyrolo-file-list-changed)

(defconst hyrolo-hdr-format
  (concat
   "===============================================================================\n"
   "%s\n"
   "===============================================================================\n")
  "Header to insert before a file's first entry match when file has no header.
Used with one argument, the file name.")

(defconst hyrolo-hdr-regexp "^==="
  "Regular expression to match the first and last lines of hyrolo file headers.
This header is inserted into hyrolo-display-buffer before any entries from the
file are added.")

(defvar hyrolo-entry-group-number 1
  "Group number whose length represents the level of any entry matched.
See `hyrolo-hdr-and-entry-regexp'")

(defvar hyrolo-entry-trailing-space-group-number 2
  "Group number within `hyrolo-hdr-and-entry-regexp' containing trailing space.")

(defvar hyrolo-hdr-prefix-regexp
  (concat hyrolo-hdr-regexp
	  "\\|^" (if (boundp 'hbut:source-prefix) hbut:source-prefix "@loc> ")
	  "\\|")
  "Regexp to prefix to `hyrolo-hdr-and-entry-regexp' and `outline-regexp'.
It must not contain any parenthesized match groupings.")

(defvar hyrolo-entry-regexp "^\\([*\^L]+\\)\\([ \t\n\r]+\\)"
  "Regular expression to match the beginning of a HyRolo entry.
This pattern must match the beginning of a line.
`hyrolo-entry-group-number' must capture the entry's level in the
hierarchy.  `hyrolo-entry-trailing-space-group-number' must capture
the entire single line whitespace following the entry hierarchy
level.")

(defvar hyrolo-hdr-and-entry-regexp
  (concat hyrolo-hdr-prefix-regexp hyrolo-entry-regexp)
  "Regular expression to match the beginning of a HyRolo file header or entry.
This pattern must match the beginning of a line.")

(defcustom hyrolo-date-format "%m/%d/%Y"
  "Format of date string used in HyRolo automatic date stamps.
An empty string disables adding or updating HyRolo dates.

Default appearance is MM/DD/YYYY.  See documentation of the function
`format-time-string' for format options."
  :type 'string
  :group 'hyperbole-hyrolo)

(defvar hyrolo-display-format-function
  (lambda (entry)
    (concat (replace-regexp-in-string "[ \t\n\r]+\\'" "" entry nil t) "\n"))
  "*Function of one argument which modifies the string for display.
The argument is a rolo entry string.")

(defcustom hyrolo-email-format "%s\t\t<%s>"
  "Format string to use when adding an entry with e-mail addr from a mail msg.
It must contain a %s indicating where to put the entry name and a second
%s indicating where to put the e-mail address."
  :type 'string
  :group 'hyperbole-hyrolo)

(defvar hyrolo-entry-name-regexp "[-_a-zA-Z0-9@.]+\\( ?, ?[-_a-zA-Z0-9@.]+\\)?"
  "*Regexp matching a hyrolo entry name.
The match is after matching to `hyrolo-hdr-and-entry-regexp'.")

(defcustom hyrolo-find-file-function #'find-file
  "Function to interactively display a `hyrolo-file-list' file for editing.
Use the `hyrolo-edit' function instead to edit a new or existing entry."
  :type 'function
  :group 'hyperbole-hyrolo)

(defcustom hyrolo-find-file-noselect-function #'find-file-noselect
  "Function used by HyRolo to read `hyrolo-file-list' files into Emacs."
  :type 'function
  :group 'hyperbole-hyrolo)

(defvar hyrolo-next-match-function #'hyrolo-next-regexp-match
  "Value is the function to find next match within a HyRolo file.
Must take one argument, `match-pattern', a regular expression.
Must leave point at the end of the match and return the start position
of the match or nil when no match.")

(defvar hyrolo-add-hook nil
  "Hook run when a HyRolo item is added.")

(defvar hyrolo-edit-hook nil
  "Hook run when a HyRolo item is edited.")

(declare-function hyrolo-fgrep-logical "hyrolo-logic")

(defvar hproperty:highlight-face)

(defcustom hyrolo-highlight-face 'match
  "Face used to highlight rolo search matches."
  :type 'face
  :initialize #'custom-initialize-default
  :group 'hyperbole-hyrolo)

(defcustom hyrolo-kill-buffers-after-use nil
  "Non-nil means kill rolo file buffers after searching them for entries.
Only unmodified buffers are killed."
  :type 'boolean
  :group 'hyperbole-hyrolo)

(defvar hyrolo-reveal-ignore-this-command nil
  "Set this non-nil in any command that should ignore `hyrolo-reveal-mode'.")

(defcustom hyrolo-save-buffers-after-use t
  "Non-nil means save rolo file after an entry is killed."
  :type 'boolean
  :group 'hyperbole-hyrolo)

;; Insert or update the entry date each time an entry is added or edited.
(add-hook 'hyrolo-add-hook  #'hyrolo-set-date)
(add-hook 'hyrolo-edit-hook #'hyrolo-edit-date)

(defvar hyrolo-yank-reformat-function #'ignore
  "*A function of two arguments, START and END, invoked after a `hyrolo-yank'.
It should reformat the region given by the arguments to some preferred style.
Default value is to perform no reformatting.")

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hyrolo--expanded-file-list nil
  "List of hyrolo files after directory and file wildcard expansions.
HyRolo sets this internally; never set it outside of this library.")

(defconst hyrolo-match-regexp nil
  "Last regular expression used to search the hyrolo.
Nil before a search is done, including after a logical search is done.
String search expressions are converted to regular expressions.")

(defvar hyrolo--wconfig nil
  "Saves frame's window configuration prior to a hyrolo search.")

(defvar hyrolo-mode-syntax-table nil
  "Syntax table used while in hyrolo match mode.")

(if hyrolo-mode-syntax-table
    ()
  (setq hyrolo-mode-syntax-table (make-syntax-table text-mode-syntax-table)))

(defvar hyrolo-mode-map nil
  "Keymap for the hyrolo display match buffer.")

(defvar hyrolo-mode-prefix-map nil
  "Keymap for hyrolo display match buffer bindings with a prefix, typically \\`C-c'.")

;;; ************************************************************************
;;; Commands
;;; ************************************************************************

;;;###autoload
(defun hyrolo-add (name &optional file)
  "Add a new entry in personal rolo for NAME.
Last name first is best, e.g. \"Smith, John\".
With prefix argument, prompts for optional FILE to add entry within.
NAME may be of the form: parent/child to insert child below a parent
entry which begins with the parent string."
  (interactive
   (progn
     (unless (fboundp 'mail-fetch-field)
       (require 'mail-utils))
     (let* ((lst (hyrolo-name-and-email))
	    (name (car lst))
	    (email (car (cdr lst)))
	    (entry (read-string "Name to add to rolo: "
				(or name email))))
       (list (if (and email name
		      (string-match (concat "\\`" (regexp-quote entry)) name))
		 (format hyrolo-email-format entry email) entry)
	     current-prefix-arg))))
  (when (or (not (stringp name)) (string-equal name ""))
    (error "(hyrolo-add): Invalid name: `%s'" name))
  (when (and (called-interactively-p 'interactive) file)
    (setq file (completing-read "File to add to: "
				(mapcar #'list (hyrolo-get-file-list)))))
  (unless file
    (setq file (car (hyrolo-get-file-list))))
  (cond ((and file (or (not (stringp file)) (string-equal file "")))
	 (error "(hyrolo-add): Invalid file: `%s'" file))
	((and (file-exists-p file) (not (file-readable-p file)))
	 (error "(hyrolo-add): File not readable: `%s'" file))
	((not (file-writable-p file))
	 (error "(hyrolo-add): File not writable: `%s'" file)))
  (set-buffer (or (get-file-buffer file)
		  (hyrolo-find-file-noselect file)))
  (when (called-interactively-p 'interactive)
    (message "Locating insertion point for `%s'..." name))
  (let ((parent "")
	(level "")
	(entry-regexp hyrolo-entry-regexp)
	end)
    (hyrolo-widen)
    (goto-char (point-min))
    ;; If name includes slash level separator character, walk down
    ;; existing matching tree of entries to find insertion point.
    (while (string-match "\\`[^\]\[/<>{}\"]*/" name)
      (setq end (1- (match-end 0))
	    parent (substring name 0 end)
	    name (substring name (min (1+ end) (length name))))
      (if (re-search-forward
	   (concat entry-regexp (regexp-quote parent) "\\s-") nil t)
	  (progn (setq level (match-string-no-properties hyrolo-entry-group-number))
		 (goto-char (match-beginning 0)))
	(error "(hyrolo-add): Insertion failed, `%s' parent entry not found in \"%s\""
	       parent file)))
    (when (looking-at hyrolo-entry-regexp)
      (narrow-to-region (point) (progn (hyrolo-to-entry-end t) (point))))
    (let* ((name-level (concat level "*"))
	   (level-len (length name-level))
	   (first-char (aref name 0))
	   (entry "")
	   (entry-spc "")
	   (entry-level-len)
	   (match)
	   (again t))
      ;; Speed up entry insertion point location if this is a first-level
      ;; entry by moving to an entry with the same (or nearest) first character
      ;; to that of `name'.
      (if (and (= level-len 1)
	       (equal entry-regexp hyrolo-entry-regexp))
	  (let ((case-fold-search))
	    (goto-char (point-min))
	    (if (re-search-forward (concat entry-regexp
					   (regexp-quote (char-to-string first-char)))
				   nil t)
		(goto-char (match-beginning 0))
	      (goto-char (point-max))
	      (when (and (> first-char ?0)
			 (re-search-backward
			  (concat "^\\*[ \t]+["
				  (substring
				   "0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz"
				   0 (min (- first-char ?0) 62))
				  "])")
			  nil t))
		(goto-char (match-end 0))
		(hyrolo-to-entry-end t)
		;; Now at the insertion point, immediately after
		;; the last existing entry whose first character
		;; is less than that of `name'.  Setting `again'
		;; to nil prevents further searching for an
		;; insertion point.
		(setq again nil))))
	(goto-char (point-min)))

      (when again
	(goto-char (point-min)))
      (while (and again (re-search-forward entry-regexp nil 'end))
	(setq entry-level-len (length (match-string-no-properties hyrolo-entry-group-number)))
	(if (/= entry-level-len level-len)
	    (hyrolo-to-entry-end t)
	  (setq entry-spc (match-string-no-properties hyrolo-entry-trailing-space-group-number)
		entry (buffer-substring-no-properties (point)
						      (save-excursion
							(re-search-forward hyrolo-entry-name-regexp nil t)
							(point))))
	  (when (and (derived-mode-p 'markdown-mode)
		     (string-match "\\`[^#]*#+" entry-spc))
	    (setq entry-spc (substring entry-spc (length (match-string 0 entry-spc)))))
	  (cond ((string-lessp entry name)
		 (hyrolo-to-entry-end t))
		((string-lessp name entry)
		 (setq again nil) (beginning-of-line))
		(t ;; found existing entry matching name
		 (setq again nil match t)))))
      (setq buffer-read-only nil)
      (unless match
	(unless (zerop (current-column))
	  (insert "\n"))
	(insert (concat level "*")
		(if (string-equal entry-spc "") "   " entry-spc)
		name "\n")
	(backward-char 1))
      ;; hyrolo-to-buffer may move point from its desired location, so
      ;; restore it.
      (let ((opoint (point)))
	(hyrolo-widen)
	(hyrolo-to-buffer (current-buffer))
	(goto-char opoint))
      (when (derived-mode-p 'kotl-mode)
	(kotl-mode:to-valid-position))
      (set-auto-mode t)
      (run-hooks 'hyrolo-add-hook)
      (when (called-interactively-p 'interactive)
	(message "Edit entry at point.")))))

(defun hyrolo-at-tags-p (&optional at-tag-flag)
  "Return non-nil if point is in a HyRolo buffer and at Org tags."
  (and (or at-tag-flag (hsys-org-at-tags-p)
	   ;; Non-highlighted Org tags in `hyrolo-display-buffer'
	   (and (save-excursion
		  (beginning-of-line)
		  (looking-at org-tag-line-re))
		(>= (point) (match-beginning 1))
		(< (point) (match-end 1))))
       (or (string-prefix-p "*HyRolo" (buffer-name))
	   (and (hypb:buffer-file-name)
		(apply #'derived-mode-p '(org-mode org-agenda-mode))
		(member (hypb:buffer-file-name) (hyrolo-get-file-list))
		t))))

;;;###autoload
(defun hyrolo-consult-grep (&optional regexp max-matches path-list)
  "Interactively search paths with a consult package grep command.
Search for optional REGEXP up to MAX-MATCHES in PATH-LIST or `hyrolo-file-list'.

Use ripgrep (rg) if found, otherwise, plain grep.  Initialize search with
optional REGEXP and interactively prompt for changes.  Limit matches
per file to the absolute value of MAX-MATCHES, if given and not 0.  If
0, match to headlines only (lines that start with a '^[*#]+[ \t]+' regexp)."
  (interactive "i\nP")
  (let* ((grep-includes (concat "--include *.kot --include *.kotl"
				" --include *.md --include *.markdown --include *.mkd --include *.mdown --include *.mkdn --include *.mdwn"
				" --include *.org --include *.otl --include *.outl"))
	 (ripgrep-globs "--glob *.{kot,kotl,md,markdown,mkd,mdown,mkdn,mdwn,org,otl,outl}"))
    (hsys-consult-grep grep-includes ripgrep-globs
		       regexp max-matches (or path-list hyrolo-file-list))))

;;;###autoload
(defun hyrolo-display-matches (&optional display-buf return-to-buffer)
  "Display optional DISPLAY-BUF buffer of previously found rolo matches.
If DISPLAY-BUF is nil, use the value in `hyrolo-display-buffer'.
Second arg RETURN-TO-BUFFER is the buffer to leave point within
after the display."
  (interactive)
  (unless display-buf
    (setq display-buf (get-buffer hyrolo-display-buffer)))
  (unless display-buf
    (error "(hyrolo-display-matches): Search the rolo first"))
  ;; Save current window configuration if rolo match buffer is not
  ;; displayed in one of the windows already.
  (or
   ;; Handle both Emacs V18 and V19 versions of get-buffer-window.
   (condition-case ()
       (get-buffer-window display-buf (selected-frame))
     (error (get-buffer-window display-buf)))
   (setq hyrolo--wconfig (current-window-configuration)))
  (hyrolo-to-buffer display-buf)
  (when (fboundp 'hproperty:but-create)
    (hproperty:but-create))
  (hyrolo-shrink-window)
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (run-hooks 'hyrolo-display-hook)
  ;; Leave point in match buffer unless a specific RETURN-TO-BUFFER has
  ;; been specified.  Use {q} to quit and restore display.
  (when return-to-buffer
    (hyrolo-to-buffer return-to-buffer)))

;;;###autoload
(defun hyrolo-edit (&optional name file-or-buf)
  "Edit a hyrolo entry matching NAME from FILE-OR-BUF.
With prefix argument, prompt for optional FILE-OR-BUF from `hyrolo-file-list',
within which to locate entry.  With no NAME arg, simply display
FILE-OR-BUF or the first entry in `hyrolo-file-list' in an editable
mode.  NAME may be of the form: parent/child to edit child below
a parent entry which begins with the parent string."
  (interactive "sEdit rolo entry named: \nP")
  (when (string-empty-p name)
    (setq name nil))
  (when (and name (not (stringp name)))
    (error "(hyrolo-edit): Invalid name: `%s'" name))

  (let* ((found-point)
	 (all-files-or-bufs (hyrolo-get-file-list))
	 (file-or-buf-list (if file-or-buf (list file-or-buf) all-files-or-bufs)))
    (when (and (called-interactively-p 'interactive) current-prefix-arg)
      (setq file-or-buf
	    (if (cadr all-files-or-bufs) ;; length > 1
		(car all-files-or-bufs)
	      (completing-read "File of entry to edit: "
			       (mapcar #'list all-files-or-bufs)))))
    (unless file-or-buf
      (setq file-or-buf (car file-or-buf-list)))
    (if (or (null name)
	    (setq found-point (hyrolo-to name (list file-or-buf))))
	(cond ((stringp file-or-buf)
	       (unless (file-writable-p file-or-buf)
		 (error "(hyrolo-edit): File not writable: `%s'" file-or-buf))
	       (hpath:find file-or-buf)
	       (setq buffer-read-only nil)
	       (set-auto-mode t))
	      ((bufferp file-or-buf)
	       (unless (buffer-live-p file-or-buf)
		 (error "(hyrolo-edit): Buffer is not live: `%s'" file-or-buf))
	       (when (hyrolo-to-buffer file-or-buf)
		 (barf-if-buffer-read-only)))
	      (t (error "(hyrolo-edit): Second argument must be a file or buffer, not: `%s'" file-or-buf)))
      (message "(hyrolo-edit): `%s' not found." name)
      (beep)
      (hyrolo-to-buffer (or (get-buffer file-or-buf)
			    (hyrolo-find-file-noselect file-or-buf)))
      (setq buffer-read-only nil))
    (when name
      (hyrolo-widen)
      ;; hyrolo-to-buffer may have moved point from its desired location, so
      ;; restore it.
      (when found-point
	(goto-char found-point)
	(set-auto-mode t)
	(hmouse-pulse-line))
      (when (derived-mode-p 'kotl-mode)
	(kotl-mode:to-valid-position))
      (unless (get-text-property 0 'hyrolo-line-entry name)
	;; Run hooks like adding a date only when handling a
	;; delimited (rather than single-line) entry.
	(run-hooks 'hyrolo-edit-hook)))))

(defun hyrolo-edit-entry ()
  "Edit the source entry of the hyrolo match buffer entry at point.
Return entry name, if any, otherwise, trigger an error."
  (interactive)
  (hyrolo-funcall-match
   (lambda ()
     (let* ((name-and-src (hyrolo-name-at-p))
	    (name (car name-and-src))
	    (src (cdr name-and-src)))
       (if name
	   (progn (cond ((and (boundp 'bbdb-file) (stringp bbdb-file) (equal src (expand-file-name bbdb-file)))
			 ;; For now, can't edit an entry from the bbdb database, signal an error.
			 (error "(hyrolo-edit-entry): BBDB entries are not editable"))
			((and (hyrolo-google-contacts-p) (equal src (get-buffer google-contacts-buffer-name)))
			 ;; For now, can't edit an entry from Google Contacts, signal an error.
			 (error "(hyrolo-edit-entry): Google Contacts entries are not editable"))
			(src
			 (hyrolo-edit name src)
			 name)
			(t
			 (error "(hyrolo-edit-entry): Move to an entry to edit it"))))
	 (error "(hyrolo-edit-entry): Move to an entry to edit it"))))
   t))

(defun hyrolo-expand-path-list (paths)
  "Expand and return non-nil PATHS's dirs, file variables and file wildcards.
Return only existing, readable files.  If PATHS is nil, return a
default set of hyrolo files to use.

Single ${env-or-lisp-variable} references are resolved within
each path using `hpath:expand'; this also expands paths to
absolute paths.  Then directories are expanded into the files
they contain that match `hyrolo-file-suffix-regexp'.  Then, if
`find-file-wildcards' is non-nil (the default), any files
containing [char-matches] or * wildcards are expanded to their
matches."
  (let ((default-file (if (file-readable-p "~/.rolo.org")
			  "~/.rolo.org"
			"~/.rolo.otl")))
    (unless paths
      (setq paths
	    (delq nil
		  (list default-file
			(if (and (boundp 'bbdb-file) (stringp bbdb-file)) bbdb-file)
			(when (hyrolo-google-contacts-p) google-contacts-buffer-name)))))
    (or (hpath:expand-list paths hyrolo-file-suffix-regexp #'file-readable-p)
	(list (expand-file-name default-file)))))

;;;###autoload
(defun hyrolo-fgrep (string &optional max-matches hyrolo-file count-only headline-only no-display)
  "Display rolo entries matching STRING or a logical match expression.
Return count of matches.

To a maximum of optional prefix arg MAX-MATCHES, in file(s) from optional
HYROLO-FILE or `hyrolo-file-list'.  Default is to find all matching entries.
Each entry is displayed with all of its sub-entries.  Optional COUNT-ONLY
non-nil skips retrieval of matching entries.  Optional HEADLINE-ONLY searches
only the first line of entries, not the full text.  Optional NO-DISPLAY non-nil
retrieves entries but does not display them.

Nil value of MAX-MATCHES means find all entries that match, t value means find
all matching entries but omit file headers, negative values mean find up to the
inverse of that number of matching entries and omit file headers.

Return number of entries matched.  See also documentation for the variable
`hyrolo-file-list' and the function `hyrolo-fgrep-logical' for documentation
on the logical sexpression matching."
  (interactive "sFind rolo string (or logical sexpression): \nP")
  (setq string (string-trim string "\"" "\""))
  (let ((total-matches 0))
    (if (string-match-p "\(\\(r-\\)?\\(and\\|or\\|xor\\|not\\)\\>" string)
	(progn
	  ;; Search string contains embedded logic operators.
	  ;; First try to match logical sexpression within a single
	  ;; subentry to minimize entries displayed.  If no match,
	  ;; then match across ancestors and descendants.
	  (when (zerop (setq total-matches (hyrolo-fgrep-logical string count-only nil t)))
	    (hyrolo-fgrep-logical string count-only t t)))
      (setq total-matches (hyrolo-grep (regexp-quote string)
				       max-matches hyrolo-file count-only headline-only no-display)))
    (if (called-interactively-p 'interactive)
	(message "%s matching entr%s found in HyRolo."
		 (if (= total-matches 0) "No" total-matches)
		 (if (= total-matches 1) "y" "ies")))
    total-matches))

;;;###autoload
(defun hyrolo-find-file (&optional file find-function &rest args)
  "Find an optional FILE with FIND-FUNCTION and rest of ARGS.
When called interactively, select from the list of files referenced
by `hyrolo-file-list' unless given a prefix argument, in which case
use the first file generated by the list.

FIND-FUNCTION must return the buffer of the file found but need not
select it."
  (interactive "P")
  (let ((all-files (hyrolo-get-file-list)))
    (when (or (called-interactively-p 'interactive)
	      (null file))
      (if (or (not (cadr all-files)) ;; length <= 1
	      current-prefix-arg)
	  (setq file (car all-files))
	(setq file (completing-read "Edit HyRolo file: "
				    (mapcar #'list all-files)))))
    (when (stringp file)
      (let (buf)
	(prog1 (setq buf (apply (or find-function hyrolo-find-file-function) file args))
	  (when buf
	    (with-current-buffer buf
	      (when (equal outline-regexp (default-value 'outline-regexp))
		;; Prevent matching to *word* at the beginning of
		;; lines and hanging hyrolo search functions.  Note this
		;; change adds one to the default `outline-level' function,
		;; so `hyrolo-outline-level' overrides that as well
		;; to get the correct calculation.  -- rsw, 2023-11-17
		(setq-local outline-regexp "\\([*\^L]+\\)\\([ \t\n\r]\\)"
			    outline-level #'hyrolo-outline-level))
	      (setq buffer-read-only nil))))))))

;;;###autoload
(defun hyrolo-find-file-noselect (&optional file)
  "HyRolo function to read a FILE in without selecting it.
It uses the setting of `hyrolo-find-file-noselect-function' and
overrides file major modes with any settings in `hyrolo-auto-mode-alist'."
  ;; In a Lisp program, if you want to be sure of accessing a file’s
  ;; contents literally, you should create a temporary buffer and then read
  ;; the file contents into it using ‘insert-file-contents-literally’.
  (let ((auto-mode-alist (append hyrolo-auto-mode-alist auto-mode-alist))
	(enable-local-variables))
    (hyrolo-find-file file hyrolo-find-file-noselect-function)))

;; This wraps forward-visible-line, making its ARG optional, making
;; its calling convention match that of forward-line and making it
;; usable as an argument to `sort-subr' in `hyrolo-sort-lines' to fix a
;; sorting issue visible in Emacs `sort-lines'.
(defun hyrolo-forward-visible-line (&optional arg)
  "Move forward by optional ARG lines (default = 1).
Ignore currently invisible newlines only.
If ARG is negative, move backward -ARG lines.
If ARG is zero, move to the beginning of the current line."
  (unless arg
    (setq arg 1))
  (forward-visible-line arg))

(defun hyrolo-get-file-list ()
  "Return the current expanded list of HyRolo search files."
  (if (equal hyrolo-file-list (symbol-value 'hyrolo-file-list))
      (or hyrolo--expanded-file-list hyrolo-file-list
	  (hyrolo-expand-path-list nil))
    ;; lexical-binding is enabled and there is a local binding of
    ;; `hyrolo-file-list', so expand it.
    (hyrolo-expand-path-list hyrolo-file-list)))

;;;###autoload
(defun hyrolo-grep (regexp &optional max-matches hyrolo-file-or-bufs count-only headline-only no-display)
  "Display HyRolo entries matching REGEXP and return count of matches.
To a maximum of prefix arg MAX-MATCHES, in buffer(s) from
optional HYROLO-FILE-OR-BUFS or `hyrolo-get-file-list'.  Default
is to find all matching entries.  Each entry is displayed with
all of its sub-entries.  Optional COUNT-ONLY non-nil means don't
retrieve and don't display matching entries.  Optional
HEADLINE-ONLY searches only the first line of entries, not the
full text.  Optional NO-DISPLAY non-nil retrieves entries but
does not display.

Nil value of MAX-MATCHES means find all entries that match, t value means find
all matching entries but omit file headers, negative values mean find up to the
inverse of that number of matching entries and omit file headers.

Return number of entries matched.  See also documentation for the variable
\`hyrolo-file-list'."
  (interactive "sFind rolo regular expression: \nP")
  (unless (or (integerp max-matches) (memq max-matches '(nil t)))
    (setq max-matches (prefix-numeric-value max-matches)))
  (let ((files-or-bufs
	 (cond ((null hyrolo-file-or-bufs) (hyrolo-get-file-list))
	       ((listp hyrolo-file-or-bufs) hyrolo-file-or-bufs)
	       ((list hyrolo-file-or-bufs))))
	(case-fold-search t)
	(display-buf (unless count-only
		       (hyrolo-set-display-buffer)))
	(total-matches 0)
	(num-matched 0)
	(inserting (or (eq max-matches t)
		       (and (integerp max-matches) (< max-matches 0))))
	(file-or-buf))
    (unless count-only
      (setq buffer-read-only nil)
      (unless inserting
	(erase-buffer))
      (hyrolo--cache-initialize))
    (while (and (setq file-or-buf (car files-or-bufs))
		(or (not (integerp max-matches))
		    (< total-matches (max max-matches (- max-matches)))))
      (setq files-or-bufs (cdr files-or-bufs)
	    num-matched (cond ((and (featurep 'bbdb) (equal file-or-buf bbdb-file))
			       (hyrolo-bbdb-grep-file file-or-buf regexp max-matches count-only))
			      ((and (hyrolo-google-contacts-p) (equal file-or-buf google-contacts-buffer-name))
			       (hyrolo-retrieve-google-contacts (regexp-quote regexp))
			       (hyrolo-google-contacts-grep-file file-or-buf regexp max-matches count-only))
			      (t (hyrolo-grep-file file-or-buf regexp max-matches count-only headline-only)))
	    total-matches (+ total-matches num-matched))
      (when (integerp max-matches)
	(setq max-matches
	      (if (>= max-matches 0)
		  (- max-matches num-matched)
		(+ max-matches num-matched)))))
    (unless (or count-only (= total-matches 0))
      (hyrolo--cache-post-display-buffer)
      (unless (or no-display inserting)
	(hyrolo-display-matches display-buf)))
    (when (called-interactively-p 'interactive)
      (message "%s matching entr%s found in HyRolo."
	       (if (= total-matches 0) "No" total-matches)
	       (if (= total-matches 1) "y" "ies")))
    total-matches))

;;;###autoload
(defun hyrolo-grep-or-fgrep (&optional arg)
  "Grep over `hyrolo-file-list' and display the results as rolo entries.
With optional prefix ARG, do an fgrep string match instead of a regexp match."
  (interactive "P")
  (call-interactively (if arg 'hyrolo-fgrep 'hyrolo-grep)))

(defun hyrolo-isearch (&optional arg)
  "Interactively search forward for the next occurrence of current match string.
Then add characters to further narrow the search.  With optional prefix ARG
non-nil, search for the current match regular expression rather than string."
  (interactive "P")
  (if arg
      (hyrolo-isearch-for-regexp hyrolo-match-regexp t)
    (hyrolo-verify)
    (if hyrolo-match-regexp
	(progn (setq unread-command-events
		     (append unread-command-events (string-to-list (regexp-quote hyrolo-match-regexp))))
	       (let ((case-fold-search t))
		 (isearch-forward)))
      (error (substitute-command-keys "(hyrolo-isearch): Use {\\[hyrolo-grep-or-fgrep]} to do an initial search")))))

(defun hyrolo-isearch-regexp (&optional arg)
  "Interactively search forward for the next occurrence of current match string.
Then add characters to further narrow the search.  With optional prefix ARG
non-nil, search for the current match regular expression rather than string."
  (interactive "P")
  (if arg
      (hyrolo-isearch-for-regexp hyrolo-match-regexp t)
    (hyrolo-isearch)))

;;;###autoload
(defun hyrolo-kill (name &optional file)
  "Kill a rolo entry given by NAME within `hyrolo-file-list'.
With prefix argument, prompts for optional FILE to locate entry within.
NAME may be of the form: parent/child to kill child below a parent entry
which begins with the parent string.
Return t if entry is killed, nil otherwise."
  (interactive "sKill rolo entry named: \nP")
  (if (or (not (stringp name)) (string-equal name "") (string-match "\\*" name))
      (error "(hyrolo-kill): Invalid name: `%s'" name))
  (if (and (called-interactively-p 'interactive) current-prefix-arg)
      (setq file (completing-read "Entry's File: "
				  (mapcar #'list (hyrolo-get-file-list)))))
  (let ((file-list (if file (list file) (hyrolo-get-file-list)))
	(killed))
    (unless file
      (setq file (car file-list)))
    (save-excursion
      (if (hyrolo-to name file-list)
	  (progn
	    (setq file (hypb:buffer-file-name))
	    (if (file-writable-p file)
		(let ((kill-op
		       (lambda (start _level-len)
			 (kill-region
			  start (hyrolo-to-entry-end t))
			 (setq killed t)
			 (hyrolo-save-buffer)
			 (hyrolo-kill-buffer)))
		      (case-fold-search)
		      start end level-len)
		  (setq buffer-read-only nil)
		  (re-search-backward hyrolo-hdr-and-entry-regexp nil t)
		  (setq end (match-end 0))
		  (setq start (line-beginning-position)
			level-len (length (buffer-substring-no-properties start end)))
		  (goto-char end)
		  (skip-chars-forward " \t")
		  (if (called-interactively-p 'interactive)
		      (let ((entry-line (buffer-substring-no-properties
					 (point)
					 (min (+ (point) 60)
					      (progn (end-of-line) (point))))))
			(if (y-or-n-p (format "Kill `%s...'? " entry-line))
			    (progn
			      (funcall kill-op start level-len)
			      (message "Killed"))
			  (message "Aborted")))
		    (funcall kill-op start level-len)))
	      (message
	       "(hyrolo-kill): Entry found but file not writable: `%s'" file)
	      (beep)))
	(message "(hyrolo-kill): `%s' not found." name)
	(beep)))
    killed))

(defun hyrolo-locate ()
  "Interactively search for an entry beginning with a set of search characters."
  (interactive)
  (hyrolo-funcall-match
   (lambda () (hyrolo-isearch-for-regexp hyrolo-hdr-and-entry-regexp nil))))


;; Adapted from `set-auto-mode' in "files.el" but greatly simplified.
(defun hyrolo-major-mode-from-file-name (name)
  "Return `major-mode' function for file NAME from file name alone.
If no matching rule in `hyrolo-auto-mode-alist' or `auto-mode-alist'
or NAME is invalid, return nil."
  (when (stringp name)
    (let ((remote-id (file-remote-p name))
	  (case-insensitive-p (file-name-case-insensitive-p
			       name))
	  mode)
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
		 (string-match (regexp-quote remote-id) name))
	(setq name (substring name (match-end 0))))
      (let ((auto-mode-alist (append hyrolo-auto-mode-alist auto-mode-alist)))
	(while name
	  ;; Find first matching alist entry.
	  (setq mode
		(if case-insensitive-p
		    ;; Filesystem is case-insensitive.
		    (let ((case-fold-search t))
		      (assoc-default name auto-mode-alist
				     'string-match))
		  ;; Filesystem is case-sensitive.
		  (or
		   ;; First match case-sensitively.
		   (let ((case-fold-search nil))
		     (assoc-default name auto-mode-alist
				    'string-match))
		   ;; Fallback to case-insensitive match.
		   (and auto-mode-case-fold
			(let ((case-fold-search t))
			  (assoc-default name auto-mode-alist
					 'string-match))))))
	  (if (and mode
		   (consp mode)
		   (cadr mode))
	      (setq mode (car mode)
		    name (substring name 0 (match-beginning 0)))
	    (setq name nil))))
      mode)))

(defun hyrolo-mail-to ()
  "Start composing mail addressed to the first e-mail address at or after point."
  (interactive)
  (let ((opoint (point)) ibut)
    (skip-chars-backward "^ \t\n\r<>")
    (if (and (re-search-forward hypb:mail-address-regexp nil t)
	     (goto-char (match-beginning 1))
	     (setq ibut (ibut:at-p)))
	(hui:hbut-act ibut)
      (goto-char opoint)
      (beep)
      (message "(hyrolo-mail-to): Invalid buffer or no e-mail address found"))))

;;;###autoload
(define-derived-mode hyrolo-markdown-mode text-mode "Markdown"
  "Major mode for editing Markdown files."
  (hyrolo-install-markdown-mode)
  (require 'markdown-mode)

  ;; Don't actually derive from `markdown-mode' to avoid its costly setup
  ;; but set its parent mode property to org-mode so `derived-mode-p' checks
  ;; will pass.
  (put 'hyrolo-markdown-mode 'derived-mode-parent 'markdown-mode)

  (when buffer-read-only
    (when (or (not (hypb:buffer-file-name)) (file-writable-p (hypb:buffer-file-name)))
      (setq-local buffer-read-only nil)))
  ;; Natural Markdown tab width
  (setq tab-width 4)
  ;; Comments
  (setq-local comment-start "<!-- ")
  (setq-local comment-end " -->")
  (setq-local comment-start-skip "<!--[ \t]*")
  (setq-local comment-column 0)
  (setq-local comment-auto-fill-only-comments nil)
  (setq-local comment-use-syntax t)
  ;; Sentence
  (setq-local sentence-end-base "[.?!…‽][]\"'”’)}»›*_`~]*")

  (font-lock-mode -1) ;; Never font-lock in this mode to keep it fast

  (if markdown-hide-markup
      (add-to-invisibility-spec 'markdown-markup)
    (remove-from-invisibility-spec 'markdown-markup))

  ;; For imenu support
  (setq imenu-create-index-function
        (if markdown-nested-imenu-heading-index
            #'markdown-imenu-create-nested-index
          #'markdown-imenu-create-flat-index))

  ;; Defun movement
  (setq-local beginning-of-defun-function #'markdown-beginning-of-defun)
  (setq-local end-of-defun-function #'markdown-end-of-defun)
  ;; Paragraph filling
  (setq-local fill-paragraph-function #'markdown-fill-paragraph)
  (setq-local paragraph-start
              ;; Should match start of lines that start or separate paragraphs
              (mapconcat #'identity
                         '(
                           "\f" ; starts with a literal line-feed
                           "[ \t\f]*$" ; space-only line
                           "\\(?:[ \t]*>\\)+[ \t\f]*$"; empty line in blockquote
                           "[ \t]*[*+-][ \t]+" ; unordered list item
                           "[ \t]*\\(?:[0-9]+\\|#\\)\\.[ \t]+" ; ordered list item
                           "[ \t]*\\[\\S-*\\]:[ \t]+" ; link ref def
                           "[ \t]*:[ \t]+" ; definition
                           "^|" ; table or Pandoc line block
                           )
                         "\\|"))
  (setq-local paragraph-separate
              ;; Should match lines that separate paragraphs without being
              ;; part of any paragraph:
              (mapconcat #'identity
                         '("[ \t\f]*$" ; space-only line
                           "\\(?:[ \t]*>\\)+[ \t\f]*$"; empty line in blockquote
                           ;; The following is not ideal, but the Fill customization
                           ;; options really only handle paragraph-starting prefixes,
                           ;; not paragraph-ending suffixes:
                           ".*  $" ; line ending in two spaces
                           "^#+"
                           "^\\(?:   \\)?[-=]+[ \t]*$" ;; setext
                           "[ \t]*\\[\\^\\S-*\\]:[ \t]*$") ; just the start of a footnote def
                         "\\|"))
  (setq-local adaptive-fill-first-line-regexp "\\`[ \t]*[A-Z]?>[ \t]*?\\'")
  (setq-local adaptive-fill-regexp "\\s-*")
  (setq-local adaptive-fill-function #'markdown-adaptive-fill-function)
  (setq-local fill-forward-paragraph-function #'markdown-fill-forward-paragraph)

  ;; Markdown outlining setup
  (setq-local hyrolo-entry-regexp "^\\(#+\\)\\([ \t\n\r]+\\)"
	      hyrolo-hdr-and-entry-regexp (concat hyrolo-hdr-prefix-regexp hyrolo-entry-regexp)
	      hyrolo-entry-group-number 1
	      ;; `hyrolo-add' handles removing # prefix from
	      ;; trailing-space grouping below
	      hyrolo-entry-trailing-space-group-number 2
	      outline-regexp (concat hyrolo-hdr-prefix-regexp "^\\(#+\\)\\([ \t\n\r]\\)")
	      outline-level #'hyrolo-outline-level)
  ;; Use ellipses for invisible text
  (add-to-invisibility-spec '(outline . t))

  ;; Inhibiting line-breaking:
  ;; Separating out each condition into a separate function so that users can
  ;; override if desired (with remove-hook)
  (add-hook 'fill-nobreak-predicate
            #'markdown-line-is-reference-definition-p nil t)
  (add-hook 'fill-nobreak-predicate
            #'markdown-pipe-at-bol-p nil t)

  ;; Indentation
  (setq-local indent-line-function markdown-indent-function)
  (setq-local indent-region-function #'markdown--indent-region)

  ;; Electric quoting
  (add-hook 'electric-quote-inhibit-functions
            #'markdown--inhibit-electric-quote nil :local)

  ;; drag and drop handler
  (setq-local dnd-protocol-alist  (cons '("^file:///" . markdown--dnd-local-file-handler)
                                        dnd-protocol-alist))

  ;; Make checkboxes buttons
  (when markdown-make-gfm-checkboxes-buttons
    (markdown-make-gfm-checkboxes-buttons (point-min) (point-max))
    (add-hook 'after-change-functions #'markdown-gfm-checkbox-after-change-function t t)
    (add-hook 'change-major-mode-hook #'markdown-remove-gfm-checkbox-overlays t t))

  ;; edit-indirect
  (add-hook 'edit-indirect-after-commit-functions
            #'markdown--edit-indirect-after-commit-function
            nil 'local)

  ;; add live preview export hook
  (add-hook 'after-save-hook #'markdown-live-preview-if-markdown t t)
  (add-hook 'kill-buffer-hook #'markdown-live-preview-remove-on-kill t t)

  ;; Add a custom keymap for `visual-line-mode' so that activating
  ;; this minor mode does not override markdown-mode's keybindings.
  ;; FIXME: Probably `visual-line-mode' should take care of this.
  (let ((oldmap (cdr (assoc 'visual-line-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap [remap move-beginning-of-line] nil)
    (define-key newmap [remap move-end-of-line] nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(visual-line-mode . ,newmap) minor-mode-overriding-map-alist))

  ;; Expose hidden text as move into it
  (hyrolo-reveal-mode 1))

(defun hyrolo-next-match ()
  "Move point forward to the start of the next HyRolo search match.
Raise an error if a match is not found."
  (interactive)
  (hyrolo-verify)
  (let ((start (point))
	(case-fold-search t)
	(prior-regexp-search (stringp hyrolo-match-regexp)))
    (when (and prior-regexp-search (looking-at hyrolo-match-regexp))
      (goto-char (match-end 0)))
    (if (and prior-regexp-search (re-search-forward hyrolo-match-regexp nil t))
	(progn (goto-char (match-beginning 0))
	       ;; !! TODO: Next line temporary until `reveal-mode' works properly
	       (hyrolo-outline-show-subtree))
      (goto-char start)
      (if prior-regexp-search
	  (error
	   "(hyrolo-next-match): No following matches for \"%s\"" hyrolo-match-regexp)
	(error (substitute-command-keys "(hyrolo-next-match): Use {\\[hyrolo-grep-or-fgrep]} to do a search first"))))))

(define-derived-mode hyrolo-outline-mode outline-mode "HyRoloOtl"
  "Set major mode for HyRolo searches of outlines with selective display.
The difference from `outline-mode' is that it does not change the hidden
state of any entries when invoked, as it is used in the HyRolo display
matches buffer when moving through entries.

Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines.

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

When used in the HyRolo display matches buffer, the following commands are
available:

\\{hyrolo-mode-map}The commands `hyrolo-outline-hide-subtree',
`hyrolo-outline-show-subtree', `hyrolo-outline-show-children',
`hyrolo-outline-hide-entry', `hyrolo-outline-show-entry',
`hyrolo-outline-hide-leaves', and `hyrolo-outline-show-branches'
are used when point is on a heading line.

The variable `outline-regexp' can be changed to control what is a heading.
A line is a heading if `outline-regexp' matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on HyRolo outline mode calls the values of `text-mode-hook',
`outline-mode-hook', and then `hyrolo-outline-mode-hook' if they are
non-nil."
  (font-lock-mode -1) ;; Never font-lock in this mode to keep it fast

  ;; Support hyrolo searches in Emacs outline files
  (setq-local hyrolo-entry-regexp "^\\([*\^L]+\\)\\([ \t\n\r]+\\)"
	      hyrolo-hdr-and-entry-regexp (concat hyrolo-hdr-prefix-regexp hyrolo-entry-regexp)
	      hyrolo-entry-group-number 1
	      ;; `hyrolo-add' handles removing * prefix from
	      ;; trailing-space grouping below
	      hyrolo-entry-trailing-space-group-number 2
	      outline-regexp (concat hyrolo-hdr-prefix-regexp "^\\([*\^L]+\\)\\([ \t\n\r]\\)")
	      outline-level #'hyrolo-outline-level)

  (setq-local imenu-generic-expression
	      (if (boundp 'outline-imenu-generic-expression)
		  outline-imenu-generic-expression
		(list (list nil (concat "^\\(?:" outline-regexp "\\).*$") 0))))
  (remove-hook 'change-major-mode-hook #'outline-show-all t)
  (remove-hook 'hack-local-variables-hook #'outline-apply-default-state t)

  ;; Expose hidden text as move into it
  (hyrolo-reveal-mode 1))

(defun hyrolo-overview (levels-to-show)
  "Show the first line of all levels of HyRolo matches.
With a prefix argument of LEVELS-TO-SHOW > 0, show the first
lines of entries only to that depth relative to the first level
of matches for the file of matches at point."
  (interactive "P")
  (when (or (null levels-to-show)
	    (if (called-interactively-p 'interactive)
		(progn (setq levels-to-show (prefix-numeric-value current-prefix-arg))
		       (<= levels-to-show 0))
	      (not (integerp levels-to-show))))
    (setq levels-to-show 100))
  (setq hyrolo-reveal-ignore-this-command t)
  (hyrolo-show-levels levels-to-show))

(defun hyrolo-previous-match ()
  "Move point back to the start of the previous HyRolo search match.
This could be the current match if point is past its `hyrolo-match-regexp'.
Raise an error if a match is not found."
  (interactive)
  (hyrolo-verify)
  (if hyrolo-match-regexp
      (let ((case-fold-search t))
	(if (re-search-backward hyrolo-match-regexp nil t)
	    ;; !! TODO: Next line temporary until `reveal-mode' works properly
	    (hyrolo-outline-show-subtree)
	  (error
	   "(hyrolo-previous-match): No prior matches for \"%s\"" hyrolo-match-regexp)))
    (error (substitute-command-keys "(hyrolo-previous-match): Use {\\[hyrolo-grep-or-fgrep]} to do an initial search"))))

(defun hyrolo-prompt (keyboard-function prompt)
  "Use KEYBOARD-FUNCTION to PROMPT for a yes/no answer."
  (funcall keyboard-function prompt))

(defun hyrolo-quit ()
  "Quit from the rolo match buffer and restore the prior frame display."
  (interactive)
  (hyrolo-verify)
  (bury-buffer)
  (and hyrolo--wconfig (window-configuration-p hyrolo--wconfig)
       (set-window-configuration hyrolo--wconfig)))

(defun hyrolo-rename (old-file new-file)
  "Prompt user to rename OLD-FILE to NEW-FILE."
  (interactive (if hyperb:microsoft-os-p
		   '("c:/_rolo.otl" "~/.rolo.otl")
		 '("~/.rolodex.otl" "~/.rolo.otl")))
  (if (and (equal (car (hyrolo-get-file-list)) new-file)
	   (file-readable-p old-file)
	   (progn (beep)
		  (or (hyrolo-prompt
		       'y-or-n-p
		       (format "(hyrolo-rename): Rename \"%s\" to the new standard \"%s\"? "
			       old-file new-file))
		      ;; Setup to get rolo matches from OLD-FILE.
		      (progn (setq hyrolo-file-list
				   (cons old-file (cdr (hyrolo-get-file-list))))
			     nil))))
      (progn (rename-file old-file new-file 1)
	     ;; Also rename backup file if it exists.
	     (when (file-readable-p (concat old-file "~"))
	       (rename-file (concat old-file "~") (concat new-file "~") 1))
	     (when (get-file-buffer old-file)
	       (with-current-buffer (get-file-buffer old-file)
		 (rename-buffer (file-name-nondirectory new-file))
		 (setq buffer-file-name (expand-file-name new-file))))
	     (message "(HyRolo): Your personal rolo file is now: \"%s\"."
		      new-file))))

(defun hyrolo-refresh-file-list ()
  "Refresh from disk the internal list of files given by `hyrolo-file-list'."
  (setq hyrolo--expanded-file-list (hyrolo-expand-path-list hyrolo-file-list))
  (when (hyrolo-any-file-type-problem-p)
    (error "(HyRolo): Invalid files used in `hyrolo-file-list'; see the *HyRolo Errors* buffer")))

(defun hyrolo-set-display-buffer ()
  "Set display buffer."
  (prog1 (set-buffer (get-buffer-create hyrolo-display-buffer))
    (unless (eq major-mode 'hyrolo-mode)
      (hyrolo-mode))
    (setq buffer-read-only nil)))

;;;###autoload
(defun hyrolo-let-file-list (symbol value)
  (set symbol value)
  (setq hyrolo--expanded-file-list (hyrolo-expand-path-list value))
  (when (hyrolo-any-file-type-problem-p)
    (error "(HyRolo): Invalid files used in `hyrolo-file-list'; see the *HyRolo Errors* buffer")))

;;;###autoload
(defun hyrolo-set-file-list (symbol value)
  (set-default symbol value)
  (setq hyrolo--expanded-file-list (hyrolo-expand-path-list value))
  (unless (symbol-value symbol)
    (set-default symbol hyrolo--expanded-file-list))
  (when (hyrolo-any-file-type-problem-p)
    (error "(HyRolo): Invalid files used in `hyrolo-file-list'; see the *HyRolo Errors* buffer"))
  ;; Prompt user to rename old personal rolo file to new name, if necessary.
  (unless (or noninteractive (hyperb:stack-frame '(hyrolo-rename)))
    (call-interactively 'hyrolo-rename)))

;;;###autoload
(defun hyrolo-sort (&optional hyrolo-file)
  "Sort up to 14 levels of entries in HYROLO-FILE (default is personal hyrolo).
Assume entries are delimited by one or more `*' characters.
Return list of number of groupings at each entry level."
  (interactive
   (list (let ((default "")
	       (file))
	   (setq file
		 (completing-read
		  (format "Sort rolo file (default %s): "
			  (file-name-nondirectory
			   (setq default
				 (if (and (hypb:buffer-file-name)
					  (memq
					   t (mapcar
					      (lambda (file)
						(equal (hypb:buffer-file-name)
						       (expand-file-name file)))
					      (hyrolo-get-file-list))))
				     (hypb:buffer-file-name)
				   (car (hyrolo-get-file-list))))))
		  (mapcar #'list (hyrolo-get-file-list))))
	   (if (string-empty-p file) default file))))
  (when (or (not hyrolo-file) (equal hyrolo-file ""))
    (setq hyrolo-file (car (hyrolo-get-file-list))))
  (unless (and (stringp hyrolo-file) (file-readable-p hyrolo-file))
    (error "(hyrolo-sort): Invalid or unreadable file: %s" hyrolo-file))
  (let ((level-regexp (regexp-quote "**************"))
	(buf-existed-flag (get-file-buffer hyrolo-file))
	(entries-per-level-list)
	(n))
    (with-current-buffer (hyrolo-find-file-noselect hyrolo-file)
      (while (not (string-empty-p level-regexp))
	(setq n (hyrolo-sort-level level-regexp))
	(when (or (/= n 0) entries-per-level-list)
	  (setq entries-per-level-list (cons (list (/ (length level-regexp) 2) n)
					     entries-per-level-list)))
	;; Subtract 2 here because there are two chars per star when
	;; regexp-quoted: \\*
	(setq level-regexp (substring level-regexp 0 (- (length level-regexp) 2))))
      (goto-char (point-min))
      (unless buf-existed-flag
	(hyrolo-kill-buffer (current-buffer)))
      entries-per-level-list)))

(defun hyrolo-sort-level (level-regexp &optional max-groupings)
  "Sort groupings of entries in current buffer at hierarchy level LEVEL-REGEXP.
To a maximum of optional MAX-GROUPINGS.  Nil value of MAX-GROUPINGS means all
groupings at the given level.  LEVEL-REGEXP should simply match the text of
any rolo entry of the given level, not the beginning of a line (^); an
example, might be (regexp-quote \"**\") to match level two.  Return number
of groupings sorted.

Current buffer should be an editable HyRolo source location, not
the match buffer."
  (interactive "sRegexp for level's entries: \nP")
  ;; Divide by 2 in next line because each asterisk character is preceded
  ;; by a regexp-quote backslash character.
  (outline-hide-sublevels (/ (length level-regexp) 2))
  (let ((sort-fold-case t))
    (hyrolo-map-level
     (lambda (start end) (hyrolo-sort-lines nil start end))
     level-regexp
     max-groupings)))

;; Derived from `sort-lines' in "sort.el" since through at least Emacs 25.0
;; invisible lines are not grouped with the prior visible line, making
;; rolo entry (or any record) sorts fail.  This next function fixes that.
;; Only the last line changes from the original `sort-lines' function.
(defun hyrolo-sort-lines (reverse beg end)
  "Sort lines in region alphabetically; REVERSE non-nil means descending order.
Interactively, REVERSE is the prefix argument, and BEG and END are the region.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; To make `end-of-line', etc. ignore fields
      (let ((inhibit-field-text-motion t))
	(sort-subr reverse #'hyrolo-forward-visible-line #'end-of-visible-line)))))

;;;###autoload
(defun hyrolo-tags-view (&optional todo-only match view-buffer-name)
  "Prompt for colon-separated Org tags and display matching HyRolo sections.
With optional prefix arg TODO-ONLY, limit matches to HyRolo Org
todo items only.  With optional MATCH, an Org tags match selector
string, e.g. \":tag1:tag2:tag3:\", match to sections that contain
or inherit all of these tags, regardless of tag order.  With
optional VIEW-BUFFER-NAME, use that rather than the default,
\"*HyRolo Tags*\"."
  (interactive "P")
  (require 'org-agenda)
  (let* ((org-agenda-files (hyrolo-get-file-list))
	 (org-agenda-buffer-name (or view-buffer-name "*HyRolo Tags*"))
	 ;; `org-tags-view' is mis-written to require setting this next
	 ;; tmp-name or it will not properly name the displayed buffer.
	 (org-agenda-buffer-tmp-name org-agenda-buffer-name))
    ;; This prompts for the tags to match and uses `org-agenda-files'.
    (org-tags-view todo-only match)
    (when (equal (buffer-name) org-agenda-buffer-name)
      ;; Set up {C-u r} redo cmd
      (let (buffer-read-only)
	(put-text-property (point-min) (point-max) 'org-redo-cmd
			   `(hyrolo-tags-view
			       ,todo-only
			       nil
			       ,org-agenda-buffer-name)))
      (forward-line 2))))

;;;###autoload
(defun hyrolo-toggle-datestamps (&optional arg)
  "Toggle whether datestamps are updated when rolo entries are modified.
With optional ARG, turn them on iff ARG is positive."
  (interactive "P")
  (if (or (and arg (<= (prefix-numeric-value arg) 0))
	  (and (not (and arg (> (prefix-numeric-value arg) 0)))
	       (boundp 'hyrolo-add-hook) (listp hyrolo-add-hook)
	       (memq 'hyrolo-set-date hyrolo-add-hook)))
      (progn (remove-hook 'hyrolo-add-hook #'hyrolo-set-date)
	     (remove-hook 'hyrolo-edit-hook #'hyrolo-edit-date)
	     (message "HyRolo date stamps are now off."))
    (add-hook 'hyrolo-add-hook  #'hyrolo-set-date)
    (add-hook 'hyrolo-edit-hook #'hyrolo-edit-date)
    (message "HyRolo date stamps are now on.")))

(defun hyrolo-toggle-narrow-to-entry ()
  "Toggle between display of current entry and display of all matched entries.
Useful when bound to a mouse key."
  (interactive)
  (if (buffer-narrowed-p)
      (hyrolo-widen)
    (let (case-fold-search)
      (when (or (looking-at hyrolo-hdr-and-entry-regexp)
		(re-search-backward hyrolo-hdr-and-entry-regexp nil t))
	(forward-char)
	(narrow-to-region (1- (point)) (hyrolo-display-to-entry-end)))))
  (hyrolo-shrink-window)
  (goto-char (point-min)))

(defun hyrolo-top-level ()
  "Show only the first line of all top-level hyrolo matches.
Top-level matches are those with the lowest outline level among the
matched entries."
  (interactive)
  (setq-local hyrolo-reveal-ignore-this-command t)
  (hyrolo-show-levels 1))

(defun hyrolo-verify ()
  "Verify point is in a HyRolo match buffer."
  (when (not (member (buffer-name)
		     (nconc (list hyrolo-display-buffer
				  (and (car (hyrolo-get-file-list))
				       (file-name-nondirectory (car (hyrolo-get-file-list)))))
			    (mapcar #'file-name-nondirectory
				    (hpath:expand-list hsys-org-cycle-bob-file-list)))))
    (error "(HyRolo): Use this command in HyRolo match buffers or primary file buffers")))

(defun hyrolo-widen ()
  "Widen non-special HyRolo buffers mainly for adding entries or editing them."
  (unless (eq (get major-mode 'mode-class) 'special)
    (widen)))

;;;###autoload
(defun hyrolo-word (string &optional max-matches hyrolo-file count-only headline-only no-display)
  "Display rolo entries with whole word match for STRING.
To a maximum of optional prefix arg MAX-MATCHES, in file(s) from optional
HYROLO-FILE or hyrolo-file-list.  Default is to find all matching entries.  Each
entry is displayed with all of its sub-entries.  Optional COUNT-ONLY
non-nil skips retrieval of matching entries.  Optional HEADLINE-ONLY searches
only the first line of entries, not the full text.  Optional NO-DISPLAY non-nil
retrieves entries but does not display them.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Return number of entries matched.  See also documentation for the variable
hyrolo-file-list."
  (interactive "sFind rolo whole word matches of: \nP")
  (let ((total-matches (hyrolo-grep (format "\\b%s\\b" (regexp-quote string))
				    max-matches
				    hyrolo-file count-only headline-only no-display)))
    (when (called-interactively-p 'interactive)
      (message "%s matching entr%s found in HyRolo."
	       (if (= total-matches 0) "No" total-matches)
	       (if (= total-matches 1) "y" "ies")))
    total-matches))

;;;###autoload
(defun hyrolo-yank (name &optional regexp-p)
  "Insert at point the first rolo entry matching NAME.
With optional prefix arg, REGEXP-P, treats NAME as a regular expression instead
of a string."
  (interactive "sInsert rolo entry named: \nP")
  (let ((hyrolo-display-buffer (current-buffer))
	(start (point))
	found)
    (save-excursion
      (setq found (if regexp-p
		      (hyrolo-grep name -1)
		    (hyrolo-grep (regexp-quote name) -1))))
    ;; Let user reformat the region just yanked.
    (if (= found 1)
	(funcall hyrolo-yank-reformat-function start (point)))
    found))

;;; ************************************************************************
;;; Big Brother Database (BBDB) Integration
;;; ************************************************************************

;;;###autoload
(defun hyrolo-bbdb-fgrep (&optional arg)
  "Fgrep over a bbdb database and format the results as rolo entries.
With optional prefix ARG, do a grep regexp match instead of a string match."
  (interactive "P")
  (hyrolo-bbdb-grep (not arg)))

;;;###autoload
(defun hyrolo-bbdb-grep (&optional arg)
  "Grep over a bbdb database and format the results as rolo entries.
With optional prefix ARG, do an fgrep string match instead of a regexp match.

Output looks like so:
======================================================================
@loc> \".bbdb\"
======================================================================
* Jones     Tom                <tj@groovycat.org>
* Sera      Kate               <uptown@singular.net>
* Yako      Maso               <ym@destination.ny>"
  (interactive "P")
  (require 'bbdb)
  (let ((hyrolo-file-list (list bbdb-file))
	(hyrolo-hdr-and-entry-regexp "^\\[")
	(hyrolo-display-format-function #'hyrolo-bbdb-entry-format)
	;; Kill the bbdb file after use if it is not already in a buffer.
	(hyrolo-kill-buffers-after-use
	 (not (get-file-buffer (expand-file-name bbdb-file))))
	(current-prefix-arg))
    (call-interactively (if arg 'hyrolo-fgrep 'hyrolo-grep))
    (read-only-mode 0)
    (re-search-forward "^\\*" nil t)
    (beginning-of-line)
    (shell-command-on-region (point) (point-max) "column -s: -t" nil t)
    (set-buffer-modified-p nil)
    (read-only-mode 1)))

(defun hyrolo-bbdb-grep-file (hyrolo-file-or-buf regexp &optional max-matches count-only)
  "Retrieve entries in bbdb HYROLO-FILE-OR-BUF matching REGEXP.
Find a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.
Return number of matching entries found."
  (let ((hyrolo-hdr-and-entry-regexp "^\\[")
	(hyrolo-display-format-function #'hyrolo-bbdb-entry-format)
	;; Kill the bbdb file after use if it is not already in a buffer.
	(hyrolo-kill-buffers-after-use
	 (not (get-file-buffer (expand-file-name bbdb-file)))))
    (hyrolo-grep-file hyrolo-file-or-buf regexp max-matches count-only)))

(defun hyrolo-bbdb-entry-format (bbdb-entry)
  "Format for a BBDB-ENTRY."
  (let ((v (read bbdb-entry)))
    (format "* %s: %s: <%s>\n" (elt v 1) (elt v 0) (car (elt v 7)))))

;;; ************************************************************************
;;; Google Contacts Integration
;;; ************************************************************************

(defun hyrolo-google-contacts-p ()
  "Non-nil means google contacts package is available and feature is enabled.
Requires `hyrolo-google-contacts-flag' set as non-nil and
google-contacts package and gpg executables to be available for
use."
  (and hyrolo-google-contacts-flag
       (featurep 'google-contacts)
       (boundp 'google-contacts-buffer-name)
       ;; If no gpg encryption executable, Oauth login to Google will fail.
       (or (executable-find "gpg2") (executable-find "gpg"))))

;;;###autoload
(defun hyrolo-google-contacts-fgrep (&optional arg)
  "Fgrep over a buffer of Google Contacts and format the results as rolo entries.
With optional prefix ARG, do a grep regexp match instead of a string match."
  (interactive "P")
  (hyrolo-google-contacts-grep (not arg)))

;;;###autoload
(defun hyrolo-google-contacts-grep (&optional arg)
  "Grep over a buffer of Google Contacts and format the results as rolo entries.
With optional prefix ARG, do an fgrep string match instead of a regexp match.

Output looks like so:
======================================================================
@loc> <buffer *Google Contacts*>
======================================================================
* Jones     Tom
* Sera      Kate
* Yako      Maso"
  (interactive "P")
  (require 'google-contacts)
  (let ((hyrolo-file-list (list google-contacts-buffer-name))
	;; Kill the google-contacts buffer after use if it is not already in use.
	(hyrolo-kill-buffers-after-use (not (get-buffer google-contacts-buffer-name)))
	(current-prefix-arg))
    (call-interactively (if arg 'hyrolo-fgrep 'hyrolo-grep))
    (read-only-mode 0)
    (let ((case-fold-search t))
      (re-search-forward hyrolo-hdr-and-entry-regexp nil t))
    (beginning-of-line)
    (set-buffer-modified-p nil)
    (read-only-mode 1)))

(defun hyrolo-google-contacts-grep-file (hyrolo-file-or-buf regexp &optional max-matches count-only)
  "Retrieve entries in google-contacts HYROLO-FILE-OR-BUF matching REGEXP.
Find a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.
Return number of matching entries found."
  ;; Kill the google-contacts buffer after use if it is not already in use.
  (let ((hyrolo-kill-buffers-after-use (not (get-buffer google-contacts-buffer-name))))
    (hyrolo-grep-file hyrolo-file-or-buf regexp max-matches count-only)))

;; Derived from google-contacts.el.
(defun hyrolo-google-contacts-insert-data (contacts _token contact-prefix-string)
  "Insert google CONTACTS data."
  (if (not contacts)
      ;; No contacts, insert a string and return nil
      (insert "No result.")
    (print contacts (get-buffer-create "*contacts-data*"))
    (dolist (contact contacts)
      (let* ((child nil)
	     (name-value (nth 0 (xml-get-children contact 'gd:name)))
             (fullname (xml-node-child-string (nth 0 (xml-get-children name-value 'gd:fullName))))
             (givenname (xml-node-child-string (nth 0 (xml-get-children name-value 'gd:givenName))))
             (familyname (xml-node-child-string (nth 0 (xml-get-children name-value 'gd:familyName))))

             (nickname (xml-node-child-string (nth 0 (xml-get-children contact 'gContact:nickname))))
             (birthday (xml-get-attribute-or-nil (nth 0 (xml-get-children contact 'gContact:birthday)) 'when))

             (organization-value (nth 0 (xml-get-children contact 'gd:organization)))
             (organization-name (xml-node-child-string (nth 0 (xml-get-children organization-value 'gd:orgName))))
             (organization-title (xml-node-child-string (nth 0 (xml-get-children organization-value 'gd:orgTitle))))

             (notes (xml-node-child-string (nth 0 (xml-get-children contact 'content))))
             ;; Links
             ;; (links (xml-get-children contact 'link))

             ;; Multiple values
             ;; Format is ((rel-type . data) (rel-type . data) … )
             (events (google-contacts-build-node-list contact 'gContact:event
                                                      (xml-get-attribute (nth 0 (xml-get-children child 'gd:when)) 'startTime)))
             (emails (google-contacts-build-node-list contact 'gd:email
                                                      (xml-get-attribute child 'address)))
             (phones (google-contacts-build-node-list contact 'gd:phoneNumber))
             (websites (google-contacts-build-node-list contact 'gContact:website
                                                        (xml-get-attribute child 'href)))
             (relations (google-contacts-build-node-list contact 'gContact:relation))
             (postal-addresses (google-contacts-build-node-list contact 'gd:structuredPostalAddress
                                                                (xml-node-child-string
                                                                 (nth 0 (xml-get-children child 'gd:formattedAddress)))))
             (instant-messaging (google-contacts-build-node-list contact 'gd:im
                                                                 (cons
                                                                  (xml-node-get-attribute-type child 'protocol)
                                                                  (cdr (assoc 'address (xml-node-attributes child))))))
             (beg (point)))
        (unless (and (string-equal familyname "") (string-equal givenname "") (string-equal nickname ""))
	  (insert contact-prefix-string familyname (if (or (string-equal familyname "")
							   (string-equal givenname "")) "" ", ")
		  givenname
		  (if (string-equal nickname "")
		      ""
		    (format " (%s)" nickname))
		  "\n"))

        (unless (and (string-equal organization-name "")
                     (string-equal organization-title ""))
	  (insert (google-contacts-margin-element))
	  (if (string-equal organization-title "")
	      (insert organization-name "\n")
	    (insert organization-title " @ " organization-name "\n")))

        (hyrolo-google-contacts-insert-generic-list emails "E-mails")
        (hyrolo-google-contacts-insert-generic-list phones "Phones")
        (hyrolo-google-contacts-insert-generic-list postal-addresses "Addresses"
						    (lambda (address)
						      (google-contacts-add-margin-to-text (cdr address)
											  (+ 4 (length (car address))))))
        (hyrolo-google-contacts-insert-generic-list websites "Websites"
						    (lambda (website) (cdr website)))
        (hyrolo-google-contacts-insert-generic-list events "Events")
        (hyrolo-google-contacts-insert-generic-list relations "Relations"
						    (lambda (relation)
						      (widget-create 'link
								     :button-prefix "" :button-suffix ""
								     :action (lambda (widget &optional _event)
                                                                               (google-contacts (widget-value widget)))
								     (cdr relation))
						      ""))
        (hyrolo-google-contacts-insert-generic-list instant-messaging "Instant messaging"
						    (lambda (im)
						      (concat (cddr im) " (" (cadr im) ")")))

        (when birthday
          (insert "\n" (google-contacts-margin-element) "Birthday: " birthday "\n"))

        (unless (string-equal notes "")
          (insert "\n" (google-contacts-margin-element) "Notes:  "
                  (google-contacts-add-margin-to-text notes 8)
                  "\n"))

        ;; Insert properties
        (put-text-property beg (1+ beg) 'google-contacts t)
        (when emails
          (put-text-property beg (point)
                             'google-contacts-email (concat fullname " <" (cdr (nth 0 emails)) ">")))))
    (goto-char (point-min)))
  ;; Return contacts
  contacts)

;; Derived from google-contacts.el.
(defun hyrolo-google-contacts-insert-generic-list (items title &optional get-value)
  "Insert a text for rendering ITEMS with TITLE.
Use GET-VALUE fuction to retrieve the value from the cdr of the item,
otherwise just use the cdr of the item."
  (when items
    (insert "\n" (google-contacts-margin-element) (concat title ":\n"))
    (dolist (item items)
      (insert (google-contacts-margin-element) "  "
              (concat (car item) ":") " "
	      (if get-value
                  (funcall get-value item)
                (cdr item))
              "\n"))))

;; Derived from google-contacts.el.
(defun hyrolo-retrieve-google-contacts (&optional query-string force-refresh)
  (interactive
   (list (read-string "Look for: " (car google-contacts-history)
                      'google-contacts-history)
         current-prefix-arg))
  ;; Without this first let binding, the user would be prompted for
  ;; his passphrase on every hyrolo search.  This way it is cached.
  (let* ((plstore-cache-passphrase-for-symmetric-encryption t)
	 (buffer (google-contacts-make-buffer))
         (token (google-contacts-oauth-token))
         (google-contacts-expire-time (if force-refresh 0 google-contacts-expire-time))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (setq google-contacts-query-string query-string)
      (hyrolo-google-contacts-insert-data
       (xml-get-children (google-contacts-data query-string token)
			 'entry)
       token "* "))))

;;; ************************************************************************
;;; Org Package Integrations
;;; ************************************************************************

;;;###autoload
(defun hyrolo-helm-org-rifle (&optional context-only-flag)
  "Search with helm and interactively show all matches from `hyrolo-file-list'.
Prompt for the search pattern.
Search readable .org, .otl and .outl files only.  With optional prefix
arg CONTEXT-ONLY-FLAG, show one extra line only of context around
a matching line, rather than entire entries."
  (interactive "P")
  (unless (package-installed-p 'helm-org-rifle)
    (package-install 'helm-org-rifle))
  (require 'helm-org-rifle)
  (let ((files (seq-filter (lambda (f)
			     (and (stringp f)
				  (string-match "\\.\\(org\\|ou?tl\\)$" f)
				  (file-readable-p f)))
			   (hyrolo-get-file-list)))
	;; Next 2 local settings used by helm-org-rifle-files call below
	(helm-org-rifle-show-level-stars t)
	(helm-org-rifle-show-full-contents (not context-only-flag)))
    (save-excursion
      (mapc (lambda (file)
	      (set-buffer (hyrolo-find-file-noselect file))
	      (hyrolo-org-mode))
	    files))
    (helm-org-rifle-files files)))

;;;###autoload
(defun hyrolo-helm-org-rifle-directory (&optional context-only-flag)
  "Interactively search over `org-directory'.
With optional prefix arg CONTEXT-ONLY-FLAG, show one extra line
only of context around a matching line, rather than entire
entries."
  (interactive)
  (unless (package-installed-p 'helm-org-rifle)
    (package-install 'helm-org-rifle))
  (require 'helm-org-rifle)
  (require 'org)
  (unless (file-readable-p org-directory)
    (make-directory org-directory))
  (if (file-readable-p org-directory)
      (let ((helm-org-rifle-show-level-stars t)
	    (helm-org-rifle-show-full-contents (not context-only-flag)))
	(helm-org-rifle-org-directory))
    (error "(hyrolo-helm-org-rifle-directory): `org-directory', \"%s\", does not exist" org-directory)))

;;;###autoload
(defun hyrolo-helm-org-rifle-directories (&optional context-only-flag &rest dirs)
  "Interactively search over Emacs outline format files in rest of DIRS.
Search readable .org, .otl and .outl files only.  With optional prefix
arg CONTEXT-ONLY-FLAG, show one extra line only of context around
a matching line, rather than entire entries."
  (interactive "P")
  (let ((hyrolo-file-list (hypb:filter-directories "\\.\\(org\\|ou?tl\\)$" dirs)))
    (hyrolo-helm-org-rifle context-only-flag)))

;;;###autoload
(defun hyrolo-org (string &optional max-matches)
  "Search `org-directory' files for STRING or logic-based matches.
OPTIONAL prefix arg, MAX-MATCHES, limits the number of matches
returned to the number given."
  (interactive "sFind Org directory string (or logical sexpression): \nP")
  (require 'org)
  (unless (file-readable-p org-directory)
    (make-directory org-directory))
  (if (file-readable-p org-directory)
      (let ((hyrolo-file-list (directory-files org-directory t "\\.org$")))
	(hyrolo-fgrep string max-matches))
    (error "(hyrolo-org): `org-directory', \"%s\", does not exist" org-directory)))

;;;###autoload
(defun hyrolo-org-roam (string &optional max-matches)
  "Search `org-roam-directory' files for STRING or logical sexpression.
OPTIONAL prefix arg, MAX-MATCHES, limits the number of matches
returned to the number given."
  (interactive "sFind Org Roam directory string (or logical sexpression): \nP")
  (hsys-consult--org-roam-call-function
   (lambda ()
     (let ((hyrolo-file-list (directory-files org-roam-directory t "\\.org$")))
       (hyrolo-fgrep string max-matches)))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hyrolo-back-to-visible-point ()
  (interactive)
  (while (and (not (bobp)) (invisible-p (point)))
    ;; Move back one character at a time here because using this fails
    ;; and ends up at the beginning of buffer every time under Emacs 27.1:
    ;; (goto-char (previous-single-char-property-change (point) 'invisible))))
    (goto-char (1- (point)))))

;;;###autoload
(defun hyrolo-fgrep-directories (file-regexp &rest dirs)
  "String/logical HyRolo search over files matching FILE-REGEXP in rest of DIRS."
  (apply #'hyrolo-search-directories #'hyrolo-fgrep file-regexp dirs))

(defun hyrolo-fgrep-file (hyrolo-file-or-buf string &optional max-matches count-only headline-only)
  "Retrieve entries in HYROLO-FILE-OR-BUF matching STRING.
Retrieve a maximum of optional MAX-MATCHES.

Nil value of MAX-MATCHES means find all matches, t value means
find all matches but omit file headers, negative values mean find
up to the inverse of that number of entries and omit file
headers.  Optional COUNT-ONLY non-nil omits matching entry
display.  Optional HEADLINE-ONLY non-nil searches headlines
only (first line of entries), rather than entire entries.

Return number of matching entries found."
  (hyrolo-grep-file hyrolo-file-or-buf (regexp-quote string) max-matches count-only headline-only))

(defun hyrolo-hdr-to-first-line-p ()
  "If point is within a file header, go to its first line.
Return t in such cases.  Otherwise, don't move and return nil.

The header includes lines matching both `hyrolo-hdr-regexp' and
`hbut:source-prefix'."
  (when (and (hyrolo-hdr-move-after-p)
	     (re-search-backward hyrolo-hdr-regexp nil t 2))
    t))

(defun hyrolo-hdr-to-last-line-p ()
  "If point is within a file header, go to its last line.
Return t in such cases.  Otherwise, don't move and return nil.

The header includes lines matching both `hyrolo-hdr-regexp' and
`hbut:source-prefix'."
  (when (hyrolo-hdr-move-after-p)
    (forward-line -1)
    t))

(defun hyrolo-hdr-in-p ()
  "If point is within a file header, return t, else nil."
  (save-excursion (hyrolo-hdr-move-after-p)))

(defun hyrolo-hdr-move-after-p ()
  "If point is within a file header, move past the hdr and blank lines.
Return non-nil if point moves, else return nil."
  (let ((opoint (point))
	result)
    (if (save-excursion
	  (beginning-of-line)
	  (zerop (% (count-matches hyrolo-hdr-regexp (point-min) (point)) 2)))
	(cond ((save-excursion
		(beginning-of-line)
		(looking-at hyrolo-hdr-regexp))
	       (setq result t)
	       ;; On the first line of a file header pair
	       (beginning-of-line)
	       (when (re-search-forward hyrolo-hdr-regexp nil t 2)
		 (forward-line 1)
		 (when (looking-at hbut:source-prefix)
		   ;; @loc> line after header
		   (forward-line 1))))
	      ((save-excursion
		(beginning-of-line)
		(looking-at hbut:source-prefix))
	       ;; @loc> line after header
	       (setq result t)
	       (forward-line 1)))
      ;; Within a file header pair,
      (beginning-of-line)
      (when (re-search-forward hyrolo-hdr-regexp nil t)
	(setq result t)
	(forward-line 1)
	(when (looking-at hbut:source-prefix)
	  ;; @loc> line after header
	  (forward-line 1))))
    (if (> (point) opoint)
	(progn (while (looking-at-p "^[ \t]*$")
		 (forward-line 1))
	       result)
      (goto-char opoint)
      nil)))

;;;###autoload
(defun hyrolo-grep-directories (file-regexp &rest dirs)
  "Regexp HyRolo search over files matching FILE-REGEXP in rest of DIRS."
  (apply #'hyrolo-search-directories #'hyrolo-grep file-regexp dirs))

(defun hyrolo-grep-file (hyrolo-file-or-buf pattern &optional max-matches count-only headline-only)
  "Retrieve entries in HYROLO-FILE-OR-BUF matching REGEXP.
PATTERN is searched for using the function given by
`hyrolo-next-match-function', so it can be a text property for
example, rather than just a regexp matching buffer text.

Retrieve a maximum of optional MAX-MATCHES.  Nil value of
MAX-MATCHES means find all matches, t value means find all
matches but omit file headers, negative values mean find up to
the inverse of that number of entries and omit file headers.

Optional COUNT-ONLY non-nil skips display of matching entries.
Optional HEADLINE-ONLY non-nil searches only the first line of
entries, rather than the full text.

Return number of matching entries found."
  ;;
  ;; Save pattern as last rolo search expression.
  (setq hyrolo-match-regexp pattern)
  ;;
  (let ((actual-buf)
	;; Temporarily disable hywiki-mode for speed
	(hywiki-mode)
	;; Temporarily disable magit-auto-revert-mode-enable-in-buffers for hyrolo
	;; buffers; not needed and can slow/hang file loading
	(after-change-major-mode-hook
	 (remove 'magit-auto-revert-mode-enable-in-buffers after-change-major-mode-hook)))
    (if (and (or (null max-matches) (eq max-matches t) (integerp max-matches))
	     (or (setq actual-buf (hyrolo-buffer-exists-p hyrolo-file-or-buf))
		 (when (file-exists-p hyrolo-file-or-buf)
		   (setq actual-buf (hyrolo-find-file-noselect hyrolo-file-or-buf)))))
	(let ((num-found 0)
	      (incl-hdr t)
	      (stuck-negative-point 0)
	      (search-pattern pattern)
	      entry-start
	      hdr-pos)
	  (when max-matches
	    (cond ((eq max-matches t)
		   (setq incl-hdr nil max-matches nil))
		  ((< max-matches 0)
		   (setq incl-hdr nil
			 max-matches (- max-matches)))))
	  (set-buffer actual-buf)

	  (when (and headline-only
		     (not (string-match (concat "\\`\\(" (regexp-quote "^") "\\|" (regexp-quote "\\`") "\\)") pattern)))
	    ;; If matching only to headlines and pattern is not already
	    ;; anchored to the beginning of lines, add a file-type-specific
	    ;; headline prefix regexp to the pattern to match.
	    (setq search-pattern (concat hyrolo-entry-regexp ".*" pattern)))

	  (setq stuck-negative-point
		(catch 'stuck
		  (save-excursion
		    (save-restriction
		      (hyrolo-widen)
		      ;; Ensure no entries in outline mode are hidden.
		      (outline-show-all)
		      (goto-char (point-min))
		      (when (hyrolo-hdr-move-after-p)
			(setq hdr-pos (cons (point-min) (point))))
		      (let* ((case-fold-search t)
			     match-end)
			(while (and (or (null max-matches) (< num-found max-matches))
				    (funcall hyrolo-next-match-function search-pattern))
			  (setq match-end (point))
			  ;; If no entry delimiters found, just return
			  ;; the single line of the match alone.
			  (unless (re-search-backward hyrolo-hdr-and-entry-regexp nil t)
			    (goto-char (line-beginning-position)))
			  (setq entry-start (point))
			  (unless (re-search-forward hyrolo-hdr-and-entry-regexp nil t)
			    (goto-char (line-end-position)))
			  (unless (hyrolo-to-entry-end t)
			    ;; If at the end of a line, move to the next line;
			    ;; otherwise, move forward a character if possible.
			    (if (eolp)
				(when (not (eobp))
				  (forward-line 1))
			      (goto-char (1+ (point)))))
			  (when (<= (point) match-end)
			    ;; Stuck looping without moving to next entry,
			    ;; probably *word* at beginning of a line.
			    (throw 'stuck (- (point))))
			  (or count-only
			      (when (and (zerop num-found) incl-hdr)
				(let* ((src (or (hypb:buffer-file-name actual-buf)
						actual-buf))
				       (src-line
					(format
					 (concat (if (boundp 'hbut:source-prefix)
						     hbut:source-prefix
						   "@loc> ")
						 "%s")
					 (prin1-to-string src))))
				  (set-buffer hyrolo-display-buffer)
				  (goto-char (point-max))
				  (if hdr-pos
				      (progn
					(insert-buffer-substring
					 actual-buf (car hdr-pos) (cdr hdr-pos))
					(insert src-line "\n\n"))
				    (insert (format hyrolo-hdr-format src-line)))
				  (set-buffer actual-buf))))
			  (setq num-found (1+ num-found))
			  (or count-only
			      ;; Highlight original pattern only here,
			      ;; not the potentially bol-anchored 'search-pattern'.
			      (hyrolo-add-match pattern entry-start (point) headline-only))))))
		  num-found))
	  (when (and (> num-found 0) (not count-only))
	    (with-current-buffer hyrolo-display-buffer
	      ;; Require a final blank line in `hyrolo-display-buffer'
	      ;; so that `outline-hide-sublevels' won't hide it and
	      ;; combine with any next file header.
	      (when (/= (char-after (1- (point-max))) ?\n)
		(save-excursion
		  (goto-char (point-max))
		  (newline))))
	    (hyrolo--cache-major-mode actual-buf))
	  (when (< stuck-negative-point 0)
	    (pop-to-buffer actual-buf)
	    (goto-char (- stuck-negative-point))
	    (error "(hyrolo-grep-file): Stuck looping in buffer \"%s\" at position %d"
		   (buffer-name) (point)))
	  (hyrolo-kill-buffer actual-buf)
	  num-found)
      0)))

(defun hyrolo-map-level (func level-regexp &optional max-groupings)
  "Perform FUNC in current buffer on groupings of entries at level LEVEL-REGEXP.
Current buffer should be an editable HyRolo source location, not
the match buffer.

Limit to a maximum of optional argument MAX-GROUPINGS.  Nil value
of MAX-GROUPINGS means all groupings at the given level.  FUNC
should take two arguments, the start and the end of the region
that it should manipulate.  LEVEL-REGEXP should match the prefix
text of any rolo entry of the given level, not the beginning of a
line (^); an example, might be (regexp-quote \"**\") to match
level two.

Return number of groupings matched."
  (if (not (or (null max-groupings) (< 0 max-groupings)))
      0
    (let* ((num-found 0)
	   (total-found 0)
	   (exact-level-regexp (concat "^\\(" level-regexp "\\)[ \t\n\r\f]"))
	   (buffer-read-only)
	   (level-len (/ (length level-regexp) 2)))
      (goto-char (point-min))
      ;; Pass buffer header if it exists
      (hyrolo-hdr-move-after-p)
      ;; With 'max-groupings' non-nil, loop over all following entries
      ;; with the same parent matching 'level-regexp'.  Otherwise, maximally
      ;; loop over 'max-groupings' such entries.
      (while (and (> level-len 0) (or (null max-groupings) (< total-found max-groupings))
		  (< 0 (setq num-found
			     (hyrolo-map-single-subtree func exact-level-regexp level-len buffer-read-only))))
	(setq total-found (+ num-found total-found)))
      ;; Caller may have adjusted entry visibility, so don't do this: (outline-show-all)
      total-found)))

(defun hyrolo-map-single-subtree (func exact-level-regexp level-len read-only-flag)
  "See doc for `hyrolo-map-level'.  Return number of groupings matched."
  (let* ((start (point))
	 (end 0)
	 (num-found 0)
	 (higher-level-entry-regexp)
	 (buffer-read-only read-only-flag))
    ;; Move to the next instance of 'level-regexp'.
    ;; Although subtrees are hidden, searches will still see them.
    (when (re-search-forward exact-level-regexp nil t)
      ;; First entry exists
      (save-excursion
	(forward-line 0)
	(setq num-found (1+ num-found)
	      start (point)))
      ;; Move to start of next entry at equal
      ;; or higher level else, to buffer end.
      (setq higher-level-entry-regexp (format "^\\(\\*\\{1,%d\\}\\)[ \t]" (1- level-len)))
      (if (and (/= level-len 1)
	       (re-search-forward higher-level-entry-regexp nil t))
	  (forward-line 0)
	(goto-char (point-max))
	(skip-chars-backward " \t\n\r\f"))
      (save-excursion
	(setq end (point))
	(goto-char start)
	(funcall func start end)))
    num-found))

(define-minor-mode hyrolo-outline-minor-mode
  "Toggle HyRolo Outline minor mode for display matches buffer.
This mode does not add any outline-related font-locking.

See the command `outline-mode' for more information on this mode."
  ;; nil " Outl" nil ;; FIXME: From when is this obsolete?
  :init-value nil
  :lighter " Outl"
  :keymap nil
  (if hyrolo-outline-minor-mode
      ;; enable minor mode
      (progn
	;; Add hook to turn off this mode when we change major modes.
	(add-hook 'change-major-mode-hook
		  (lambda ()
		    (unless (derived-mode-p 'hyrolo-mode 'kotl-mode 'markdown-mode 'org-mode 'outline-mode)
		      (hyrolo-outline-minor-mode -1)))
		  nil t)
	(add-hook 'post-command-hook 'hyrolo-show-post-command nil t)
        (setq-local line-move-ignore-invisible t)
	;; Use ellipses for invisible text
	(hypb:add-to-invisibility-spec '(outline . t)))
    ;; disable minor mode
    (when (and (boundp 'outline-minor-mode-cycle) outline-minor-mode-cycle)
      (remove-overlays nil nil 'outline-overlay t))
    (remove-hook 'post-command-hook 'hyrolo-show-post-command t)
    (setq line-move-ignore-invisible nil)
    ;; Disable use of ellipses for invisible text.
    (remove-from-invisibility-spec '(outline . t))
    ;; Get rid of any outline hiding.
    (hyrolo-outline-show-all)))

(defun hyrolo-mode ()
  "Major mode for the HyRolo display match buffer.
Calls the functions given by `hyrolo-mode-hook'.
\\{hyrolo-mode-map}"
  (interactive)
  (unless (eq major-mode 'hyrolo-mode)
    (push (cons (substring hyrolo-hdr-regexp 1) 1) outline-heading-alist)
    (push (cons (if (boundp 'hbut:source-prefix)
		    hbut:source-prefix
		  "@loc> ")
		1)
	  outline-heading-alist)
    (setq-local hyrolo-entry-regexp (concat "^" "\\([*\^L]+\\)\\([ \t\n\r]+\\)")
		hyrolo-hdr-and-entry-regexp (default-value 'hyrolo-hdr-and-entry-regexp)
		hyrolo-entry-group-number 1
		;; `hyrolo-add' handles removing * prefix from
		;; trailing-space grouping below
		hyrolo-entry-trailing-space-group-number 2
		;; In `outline-regexp', prevent matching to *word*
		;; at the beginning of lines and hanging hyrolo
		;; search functions by adding a whitespace char at
		;; the end of the match.  Note this change adds one
		;; level to the level count, so `hyrolo-outline-level'
		;; decrements it by one.  -- rsw, 2023-11-17
		;; This next local value is dynamically overridden in `hyrolo-grep'.
		outline-regexp "\\([*\^L]+\\)\\([ \t\n\r]\\)"
		outline-level #'hyrolo-outline-level)

    ;; Can't cycle because {TAB} moves to next match
    (when (boundp 'outline-minor-mode-cycle)
      (setq-local outline-minor-mode-cycle nil))

    ;; For speed reasons, don't want to ever font-lock in this mode
    (when (boundp 'outline-minor-mode-highlight)
      (setq-local outline-minor-mode-highlight nil)))

  (use-local-map hyrolo-mode-map)
  (set-syntax-table hyrolo-mode-syntax-table)
  (hyrolo-outline-minor-mode 1) ;; no keymap

  (setq-local reveal-around-mark nil)
  (unless (or (eq major-mode 'hyrolo-mode)
	      hyrolo-reveal-ignore-this-command)
    ;; Expose hidden text as move into it
    (hyrolo-reveal-mode 1))

  ;; Do this after reveal-mode is enabled.
  (setq major-mode 'hyrolo-mode
	mode-name "HyRolo")

  (setq buffer-read-only t)

  (run-mode-hooks 'hyrolo-mode-hook))

(defun hyrolo-next-regexp-match (regexp)
  "In a HyRolo source buffer, Move past next occurrence of REGEXP.
When found, return the match start position; otherwise, return nil."
  (when (re-search-forward regexp nil t)
    (match-beginning 0)))

;; The *HyRolo* buffer uses hyrolo-org-mode and hyrolo-markdown-mode
;; on Org and Markdown files that it reads to speed loading and
;; searching.  This next function switches such buffers to their
;; normal modes whenever they are displayed.
(defun hyrolo-normalize-mode-function (frame)
  (with-selected-frame frame
    (walk-windows
     (lambda (window)
       (with-selected-window window
	 (when (apply #'derived-mode-p '(hyrolo-markdown-mode hyrolo-org-mode))
	   ;; Display buffer before `normal-mode' triggers possibly
	   ;; long-running font-locking
	   (sit-for 0.1)
	   (normal-mode)))))))
(add-to-list 'window-buffer-change-functions 'hyrolo-normalize-mode-function nil 'eq)

;;; In `hyrolo-mode' replace `outline-minor-mode' bindings with hyrolo-* overrides.
;;; Wrap outline movement commands with a `hyrolo-funcall-match' call.
;;; Wrap outline whole buffer commands with a `hyrolo-map-matches' call.
(defun hyrolo-outline-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (hyrolo-funcall-match (lambda () (outline-back-to-heading invisible-ok))))

(defun hyrolo-outline-backward-same-level (arg)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (hyrolo-funcall-match
   (lambda ()
     (outline-back-to-heading)
     (while (> arg 0)
       (let ((point-to-move-to (save-excursion
				 (hyrolo-outline-get-last-sibling))))
	 (if point-to-move-to
	     (progn
	       (goto-char point-to-move-to)
	       (setq arg (1- arg)))
	   (setq arg 0)
	   (error "No previous same-level heading/header")))))
   nil t))

(defun hyrolo-outline-demote (&optional which)
  "Demote headings lower down the tree.
If `transient-mark-mode' is on, and mark is active, demote headings in
the region (from a Lisp program, pass `region' for WHICH).  Otherwise:
without prefix argument, demote current heading and all headings in the
subtree (from a Lisp program, pass `subtree' for WHICH); with prefix
argument, demote just the current heading (from a Lisp program, pass
nil for WHICH, or do not pass any argument)."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (list (if (and transient-mark-mode mark-active) 'region
	     (outline-back-to-heading)
	     (if current-prefix-arg nil 'subtree)))))
  (hyrolo-funcall-match (lambda () (outline-demote which)) t))

(defun hyrolo-outline-forward-same-level (arg)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (hyrolo-funcall-match
   (lambda ()
     (outline-back-to-heading)
     (while (> arg 0)
       (let ((point-to-move-to (save-excursion
				 (hyrolo-outline-get-next-sibling))))
	 (if point-to-move-to
	     (progn
	       (goto-char point-to-move-to)
	       (setq arg (1- arg)))
	   (setq arg 0)
	   (error "No following same-level heading/header")))))))

(defun hyrolo-outline-get-last-sibling ()
  "Move to previous heading of the same level, and return point.
If there is no such heading, do not move and return nil."
  (let ((opoint (point))
	(level (funcall outline-level)))
    (hyrolo-outline-previous-visible-heading 1)
    (when (and (/= (point) opoint) (outline-on-heading-p))
      (while (and (> (funcall outline-level) level)
		  (not (bobp)))
	(hyrolo-outline-previous-visible-heading 1))
      (if (< (funcall outline-level) level)
	  (progn (goto-char opoint) nil)
        (point)))))

(defun hyrolo-outline-get-level (backward-flag)
  "Return the outline level at point.
Return 0 if not on an `outline-regexp' line.
BACKWARD-FLAG is non-nil if moving backward, else nil when moving
forward through the buffer."
  (save-excursion
    (beginning-of-line)
    (hyrolo-funcall-match
     (lambda ()
       (if (looking-at outline-regexp)
	   (hyrolo-outline-level)
	 0))
     backward-flag)))

(defun hyrolo-outline-get-next-sibling ()
  "Move to next heading/header of the same level, and return point.
If there is no such heading/header, do not move and return nil."
  (let ((opoint (point))
	(level (funcall outline-level)))
    (hyrolo-outline-next-visible-heading 1)
    (while (and (not (eobp)) (> (funcall outline-level) level))
      (hyrolo-outline-next-visible-heading 1))
    (if (or (eobp) (< (funcall outline-level) level))
	(progn (goto-char opoint) nil)
      (point))))

(defun hyrolo-outline-hide-body ()
  "Hide all body lines in buffer, leaving all headings visible.
Note that this does not hide the lines preceding the first heading line."
  (interactive)
  (setq hyrolo-reveal-ignore-this-command t)
  (hyrolo-map-matches #'outline-hide-body t))

(defun hyrolo-outline-hide-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (setq hyrolo-reveal-ignore-this-command t)
  (hyrolo-funcall-match #'outline-hide-entry t))

(defun hyrolo-outline-hide-leaves ()
  "Hide the body after this heading and at deeper levels."
  (interactive)
  (setq hyrolo-reveal-ignore-this-command t)
  (hyrolo-funcall-match #'outline-hide-leaves t))

(defun hyrolo-outline-hide-other ()
  "Hide everything except current body and parent and top-level headings.
This also unhides the top heading-less body, if any."
  (interactive)
  (setq hyrolo-reveal-ignore-this-command t)
  (hyrolo-funcall-match #'outline-hide-other))

(defun hyrolo-outline-hide-sublevels (levels)
  "Hide everything but the top LEVELS levels of headers, in whole buffer.
This also unhides the top heading-less body, if any.

Interactively, the prefix argument supplies the value of LEVELS.
When invoked without a prefix argument, LEVELS defaults to the level
of the current heading, or to 1 if the current line is not a heading."
  (interactive (list
		(cond
		 (current-prefix-arg (prefix-numeric-value current-prefix-arg))
		 ((save-excursion (beginning-of-line)
				  (looking-at outline-regexp))
		  (funcall outline-level))
		 (t 1))))
  (setq hyrolo-reveal-ignore-this-command t)
  (hyrolo-map-matches (lambda () (outline-hide-sublevels levels)) t))

(defun hyrolo-outline-hide-subtree ()
  "Move back to the start of current subtree and hide everything after the heading.
If within a file header, hide the whole file after the end of the current line."
  (interactive)
  (setq hyrolo-reveal-ignore-this-command t)
  (if (and (hyrolo-hdr-in-p)
	   (eq (current-buffer) (get-buffer hyrolo-display-buffer)))
      (cl-destructuring-bind (start end)
	  (hyrolo-cache-location-start-and-end)
	(setq start (line-end-position)
	      end (1- (or end (point-max))))
	;; Hide region
	(outline-flag-region start end t))
    (hyrolo-funcall-match
     (lambda ()
       (let ((opoint (point)))
	 (forward-line 0)
	 (unless (looking-at outline-regexp)
	   (outline-previous-visible-heading 1))
	 (if (looking-at outline-regexp)
	     (outline-hide-subtree)
	   (goto-char opoint))))
     t)))

(defun hyrolo-outline-insert-heading ()
  "Insert a new heading at same depth at point."
  (interactive "*")
  (hyrolo-funcall-match #'outline-insert-heading t))

(defun hyrolo-outline-mark-subtree ()
  "Mark the current subtree in an outlined document.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (hyrolo-funcall-match #'outline-mark-subtree t))

(defun hyrolo-outline-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "*p")
  (hyrolo-funcall-match (lambda () (outline-move-subtree-down arg)) t))

(defun hyrolo-outline-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG headlines of the same level."
  (interactive "*p")
  (hyrolo-funcall-match (lambda () (outline-move-subtree-up arg)) t))

(defun hyrolo-outline-next-visible-heading (arg)
  "Move to next visible heading or match buffer header.
With ARG, repeats or can move backward if negative.
Return t if find any matching next heading/header, nil otherwise.

A heading is one that starts with an `outline-regexp' match.
A match buffer header is one that starts with `hyrolo-hdr-regexp'."
  (interactive "p")
  (let ((orig-arg arg)
	(found-heading-p)
	(opoint (point))
	(last-point (point)))
    (condition-case nil
	(progn
	  (if (< arg 0)
	      (beginning-of-line)
	    (end-of-line))
	  (while (and (not (bobp)) (< arg 0))
	    (while (and (not (bobp))
			(progn (hyrolo-hdr-to-first-line-p)
			       (setq last-point (point))
			       (hyrolo-funcall-match
				(lambda ()
				  (re-search-backward
				   (concat "^\\(" outline-regexp "\\)")
				   nil t))
				nil t))
			(when (< (point) last-point)
			  (setq found-heading-p t))
			(setq last-point (point))
			(progn (hyrolo-hdr-to-first-line-p)
			       (outline-invisible-p))))
	    (setq arg (1+ arg)))
	  (while (and (not (eobp)) (> arg 0))
	    (while (and (not (eobp))
			(or (and (hyrolo-hdr-move-after-p)
				 (if (outline-invisible-p (point))
				     ;; Skip any invisible heading at point
				     (progn
				       (goto-char (min (1+ (point)) (point-max)))
				       nil)
				   (setq found-heading-p t)))
			    (progn (setq last-point (point))
				   (hyrolo-funcall-match
				    (lambda ()
				      (re-search-forward
				       (concat "^\\(" outline-regexp "\\)")
				       nil t)))
				   (and (< last-point (point))
					(setq last-point (point))
					(if (outline-invisible-p (match-beginning 0))
					    ;; Skip any invisible heading at point
					    (goto-char (min (1+ (point)) (point-max)))
					  (setq found-heading-p t)))))
			(not found-heading-p)))
	    (setq arg (1- arg)))
	  (cond (found-heading-p
		 (beginning-of-line))
		((> orig-arg 0)
		 (goto-char (point-max)))
		((< orig-arg 0)
		 (goto-char (point-min)))
		(t (goto-char opoint))))
      ;; Prevent error and move to start or end of file header at point,
      ;; if any
      (error (if (>= arg 0)
		 (hyrolo-hdr-move-after-p)
	       (hyrolo-hdr-to-first-line-p))))
    (and found-heading-p (/= (point) opoint) t)))

(defun hyrolo-outline-previous-heading ()
  "Move to the previous (possibly invisible) heading line."
  (interactive)
  (hyrolo-funcall-match #'outline-previous-heading))

(defun hyrolo-outline-previous-visible-heading (arg)
  "Move to the previous visible heading or match buffer header.
With ARG, repeats or can move forward if negative.

A heading is one that starts with an `outline-regexp' match.
A match buffer header is one that starts with `hyrolo-hdr-regexp'."
  (interactive "p")
  (hyrolo-outline-next-visible-heading (- arg)))

(defun hyrolo-outline-promote (&optional which)
  "Promote headings higher up the tree.
If `transient-mark-mode' is on, and mark is active, promote headings in
the region (from a Lisp program, pass `region' for WHICH).  Otherwise:
without prefix argument, promote current heading and all headings in the
subtree (from a Lisp program, pass `subtree' for WHICH); with prefix
argument, promote just the current heading (from a Lisp program, pass
nil for WHICH, or do not pass any argument)."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (list (if (and transient-mark-mode mark-active) 'region
	     (outline-back-to-heading)
	     (if current-prefix-arg nil 'subtree)))))
  (hyrolo-funcall-match (lambda () (outline-promote which)) t))

(defun hyrolo-outline-show-all ()
  "Show all of the text in the HyRolo display buffer."
  (interactive)
  (setq-local hyrolo-reveal-ignore-this-command t)
  (outline-show-all))

;;; Override this function for completeness
(defun hyrolo-outline-show-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (setq hyrolo-reveal-ignore-this-command t)
  (outline-show-children 1000))

(defun hyrolo-outline-show-children (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level should be shown.
Default is enough to cause the following heading to appear."
  (interactive "P")
  (setq hyrolo-reveal-ignore-this-command t)
  (hyrolo-funcall-match (lambda () (outline-show-children level)) t))

(defun hyrolo-outline-show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (setq hyrolo-reveal-ignore-this-command t)
  (hyrolo-funcall-match #'outline-show-entry t))

(defun hyrolo-outline-show-subtree ()
  "Show everything after this heading at deeper levels.
If within a file header, show the whole file starting with the header."
  (interactive)
  (setq hyrolo-reveal-ignore-this-command t)
  (if (and (hyrolo-hdr-in-p)
	   (eq (current-buffer) (get-buffer hyrolo-display-buffer)))
      (cl-destructuring-bind (start end)
	  (hyrolo-cache-location-start-and-end)
	(setq start (or start (line-beginning-position))
	      end (1- (or end (point-max))))
	;; Show region
	(outline-flag-region start end nil))
    (hyrolo-funcall-match #'outline-show-subtree t)))

(defun hyrolo-outline-up-heading (arg &optional invisible-ok)
  "Move to the visible heading line of which the present line is a subheading.
With argument, move up ARG levels.
If INVISIBLE-OK is non-nil, also consider invisible lines."
  (interactive "p")
  (hyrolo-funcall-match
   (lambda ()
     (and (eq this-command 'hyrolo-outline-up-heading)
	  (or (eq last-command 'hyrolo-outline-up-heading) (push-mark)))
     (outline-back-to-heading invisible-ok)
     (let ((start-level (funcall outline-level))
	   (start-point (point)))
       (when (<= start-level 1)
	 (error "Already at top level of this outline tree"))
       (while (and (> start-level 1) (> arg 0) (not (bobp)))
	 (let ((level start-level)
	       (opoint (point)))
	   (while (not (or (< level start-level) (bobp)))
	     (if invisible-ok
		 (outline-previous-heading)
	       (outline-previous-visible-heading 1))
	     (setq level (funcall outline-level))
	     (when (not (looking-at hyrolo-entry-regexp))
	       ;; Have moved into a file header; move back to opoint and stop
	       (goto-char opoint)
	       ;; Exit
	       (when (= opoint start-point)
		 (error "Already at top level of this outline tree"))
	       (setq level -1)))
	   (setq start-level level))
	 (setq arg (- arg 1))))
     (looking-at outline-regexp))
   t))

(defun hyrolo-to (name &optional file-or-buf-list)
  "Move point to entry for NAME within optional FILE-OR-BUF-LIST.
\(hyrolo-get-file-or-buf-list) provides the default when
FILE-OR-BUF-LIST is nil.  Leave point immediately after the first
match of NAME within an entry.  Switches internal current buffer
but does not alter the frame.  Return point where matching entry
begins or nil if not found."
  (when (or (not (stringp name)) (string-blank-p name))
    (error "(hyrolo-to): Invalid name: `%s'" name))
  (unless file-or-buf-list
    (setq file-or-buf-list (hyrolo-get-file-list)))
  (let ((found) file-or-buf)
    (while (and (not found) file-or-buf-list)
      (setq file-or-buf (car file-or-buf-list)
	    file-or-buf-list (cdr file-or-buf-list))
      (cond ((stringp file-or-buf)
	     (when (string-blank-p file-or-buf)
	       (error "(hyrolo-to): Invalid file: `%s'" file-or-buf))
	     (when (and (file-exists-p file-or-buf) (not (file-readable-p file-or-buf)))
	       (error "(hyrolo-to): File not readable: `%s'" file-or-buf)))
	    ((bufferp file-or-buf)
	     (unless (buffer-live-p file-or-buf)
	       (error "(hyrolo-to): Buffer not live: `%s'" file-or-buf)))
	    (t (error "(hyrolo-to): Second argument must be a file or buffer, not: `%s'" file-or-buf)))

      (set-buffer (if (stringp file-or-buf)
		      (or (get-file-buffer file-or-buf) (hyrolo-find-file-noselect file-or-buf))
		    ;; must be a buffer
		    file-or-buf))
      (let ((case-fold-search t) (real-name name) (parent "") (level)
	    col-num end line line-and-col)
	(hyrolo-widen)
	(goto-char (point-min))
	(if (setq col-num (get-text-property 0 'hyrolo-line-entry name))
	    ;; this is a whole line to find without any entry delimiters
	    (when (search-forward name nil t)
	      (move-to-column col-num)
	      (setq found (point)))
	  (while (string-match "\\`[^\]\[<>{}\"]*/" name)
	    (setq end (1- (match-end 0))
		  level nil
		  parent (substring name 0 end)
		  name (substring name (min (1+ end) (length name))))
	    (cond ((progn
		     (while (and (not level) (search-forward parent nil t))
		       (save-excursion
			 (forward-line 0)
			 (when (looking-at (concat hyrolo-hdr-and-entry-regexp (regexp-quote parent)))
			   (setq level (match-string-no-properties hyrolo-entry-group-number)))))
		     level))
		  ((equal name real-name)) ;; Try next file-or-buf.
		  (t ;; Found parent but not child
		   (setq buffer-read-only nil)
		   (hyrolo-to-buffer (current-buffer))
		   (error "(hyrolo-to): `%s' part of name not found in \"%s\""
			  parent file-or-buf)))
	    (when level
	      (narrow-to-region (point)
				(save-excursion
				  (hyrolo-to-entry-end t) (point)))))
	  (goto-char (point-min))
	  (while (and
		  ;; Search for just the leaf part of a name
		  (search-forward name nil t)
		  (not (save-excursion
			 (forward-line 0)
			 (setq found
			       (when (or (looking-at (buffer-local-value
						      'outline-regexp
						      (get-buffer hyrolo-display-buffer)))
					 ;; Jump to non-first line within an entry
					 (progn (back-to-indentation)
						(looking-at (regexp-quote name))))
				 (when (setq line-and-col (get-text-property 0 'hyrolo-name-entry name))
				   ;; this is a whole line to find except for leading whitespace
				   (setq line (car line-and-col)
					 col-num (cdr line-and-col))
				   (when (search-forward line nil t)
				     (move-to-column col-num)))
				 (when (derived-mode-p 'kotl-mode)
				   (kotl-mode:to-valid-position))
				 (point)))))))))
      (unless found
	(hyrolo-kill-buffer))) ;; conditionally kill
    (hyrolo-widen)
    found))

(defun hyrolo-to-entry-beginning (&optional include-sub-entries)
  "Move point to the beginning of the current entry.
With optional prefix arg INCLUDE-SUB-ENTRIES non-nil, move to the
beginning of the highest ancestor level.  Return final point."
  (interactive "P")
  (when (hyrolo-hdr-in-p)
    (hyrolo-hdr-to-first-line-p))
  (if include-sub-entries
      (unless (<= (hyrolo-outline-level) 1)
	(hyrolo-outline-up-heading 80))
    (hyrolo-outline-back-to-heading))
  (point))

(defun hyrolo-to-entry-end (&optional include-sub-entries)
  "Move point past the end of the current entry.
With optional prefix arg INCLUDE-SUB-ENTRIES non-nil, move past
the end of the entire subtree.  Return final point.

When called interactively, leave point one character earlier,
before the final newline of the entry.

Return current point."
  (interactive "P")
  ;; In cases where a file lacks any entry delimiters and we consider
  ;; each line a record, outline commands can falsely move point prior
  ;; to its current location.  Prevent this by moving to the max of
  ;; the (1+ start point) and the final point.
  (let ((opoint (point)))
    (hyrolo-move-forward (lambda () (hyrolo-move-to-entry-end include-sub-entries)))
    (when (and (called-interactively-p 'any)
	       (not (eolp)))
      (goto-char (1- (point))))
    (when (<= (point) opoint)
      (goto-char (min (1+ opoint) (point-max))))
  (point)))

(defun hyrolo-move-to-entry-end (include-sub-entries)
  "Move point past the end of the current entry, if any.
With optional INCLUDE-SUB-ENTRIES non-nil, move to the end of the
entire subtree.  Return INCLUDE-SUB-ENTRIES flag value."
  (if (not include-sub-entries)
      ;; Move to (point-max) if no next heading found and return nil
      (outline-next-heading)
    ;; When point is before the first entry in an Org file,
    ;; `outline-end-of-subtree' can signal an
    ;; `outline-before-first-heading' error within its subcall to
    ;; `outline-back-to-heading' because of advice wrapped around that
    ;; function from "org-compat.el".
    (condition-case ()
	(progn
	  (outline-end-of-subtree)
	  (goto-char (1+ (point))))
      ;; Error or any signal condition (all caught with the 't' symbol)
      ;; means point is before the first buffer heading; move past
      ;; file header to any next entry.
      (t (hyrolo-hdr-move-after-p))))
  include-sub-entries)

(defun hyrolo-to-next-loc ()
  "Move to next file/buffer location header in HyRolo display matches buffer."
  (interactive)
  (if (re-search-forward (concat "^" hbut:source-prefix) nil t
			 (if (looking-at hbut:source-prefix) 2 1))
      (goto-char (match-beginning 0))
    (when (called-interactively-p 'interactive)
      (message "No next file/buffer location") (beep))))

(defun hyrolo-to-previous-loc ()
  "Move to previous file/buffer location header in HyRolo display matches buffer."
  (interactive)
  (let ((opoint (point)))
    (beginning-of-line)
    (unless (re-search-backward (concat "^" hbut:source-prefix) nil t)
      (goto-char opoint)
      (when (called-interactively-p 'interactive)
	(message "No previous file/buffer location") (beep)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hyrolo-add-match (regexp start end headline-only)
  "Add in `hyrolo-display-buffer' an entry matching REGEXP from current region.
Entry is inserted before point.  The region is between START to END."
  (let ((hyrolo-buf (current-buffer))
	(hyrolo-entry (buffer-substring start end))
	opoint)
    (set-buffer (get-buffer-create hyrolo-display-buffer))
    (setq opoint (point))
    (insert (funcall hyrolo-display-format-function hyrolo-entry))
    (hyrolo-highlight-matches regexp opoint
			      (if headline-only
				  (save-excursion (goto-char opoint) (line-end-position))
				(point)))
    (set-buffer hyrolo-buf)))

(defun hyrolo-any-file-type-problem-p ()
  "Return t if any file from `hyrolo-file-list' has an unusable format.

The list of unusable files is displayed in a HyRolo error window
unless `hyrolo-boolean-only-flag' is set to t (used for testing).

This will install `markdown-mode' if any Markdown files are specified and the
package is not installed."
  ;;  1. Ignore files without suffixes in step 2
  (let ((file-suffixes
	 (delq nil (mapcar (lambda (filename) (file-name-extension filename))
			   (hyrolo-get-file-list))))
	file-and-major-mode-list
	files-no-mode-list
	files-invalid-suffix-list
	package-archives)

    ;;  2. Skip this if the markdown-mode package is installed
    (unless (package-installed-p 'markdown-mode)
    ;;  3. If any `hyrolo-file-list' file has a markdown file suffix,
      (when (delq nil (mapcar (lambda (suffix)
				(string-match-p (concat "\\(?:" hyrolo-markdown-suffix-regexp "\\)$")
						suffix))
			      file-suffixes))

	;;  4. if not, ensure nongnu is temporarily added to package
	;;     source list and then install markdown-mode.
	(hyrolo-install-markdown-mode)))

    ;;  5. Check that each file has an entry in `hyrolo-auto-mode-alist' or `auto-mode-alist',
    (setq file-and-major-mode-list
	  (mapcar (lambda (filename) (cons filename
					   (hyrolo-major-mode-from-file-name filename)))
		  (hyrolo-get-file-list))

	  files-invalid-suffix-list
	  (delq nil (mapcar (lambda (item) (when (not (string-match-p hyrolo-file-suffix-regexp (car item)))
					     (car item)))
			    file-and-major-mode-list))

	  files-no-mode-list
	  (cl-set-difference
	   (delq nil (mapcar (lambda (item) (when (null (cdr item)) (car item)))
			     file-and-major-mode-list))
	   files-invalid-suffix-list))

    ;;  6. if not, display a buffer with the invalid file types and return t
    (when (or files-invalid-suffix-list files-no-mode-list)
      (unless (and (boundp 'hyrolo-boolean-only-flag) hyrolo-boolean-only-flag)
	(with-help-window "*HyRolo Errors*"
	  (princ "`hyrolo-file-list' gets its files from these patterns:\n")
	  (mapc (lambda (spec) (princ (format "\t%S\n" spec)))
		hyrolo-file-list)
	  (terpri)
	  (princ "When expanded, it includes the following files that HyRolo cannot process:\n\n")

	  (when files-invalid-suffix-list
	    (princ (format "Files with invalid or no suffixes:\n  (valid suffixes: %S)\n"
			   hyrolo-file-suffix-regexp))
	    (mapc (lambda (file) (princ (format "\t%S\n" file)))
		  files-invalid-suffix-list)
	    (terpri)
	    (princ "Please remove the above files from `hyrolo-file-list'.\n")
	    (terpri))

	  (when files-no-mode-list
	    (princ "Files with invalid modes (file suffixes not in `auto-mode-alist'):\n")
	    (mapc (lambda (file) (princ (format "\t%S\n" file)))
		  files-no-mode-list)
	    (terpri)
	    (princ "Please add appropriate entries for the above files to `auto-mode-alist'.\n")
	    (terpri))

	  (when (hyperb:stack-frame '(hyrolo-file-list-changed))
	    ;; Errors occurred with a let of `hyrolo-file-list' so
	    ;; include backtrace of where this occurred.
	    (princ "Stack trace of where invalid files were referenced:\n")
	    (terpri)
            ;; (setq backtrace-view (plist-put backtrace-view :show-locals t))
	    (backtrace))

	  (when noninteractive
	    (princ (buffer-string)))))
      t)))

(defun hyrolo-buffer-exists-p (hyrolo-buf)
  "Return buffer given by HYROLO-BUF or nil.
HYROLO-BUF may be a file-name, `buffer-name', or buffer."
  (car (memq (get-buffer (or (and (stringp hyrolo-buf)
				  (get-buffer hyrolo-buf))
			     hyrolo-buf))
	     (buffer-list))))

(defun hyrolo-current-date ()
  "Return the current date (a string) in a form used for rolo entry insertion."
  (format-time-string hyrolo-date-format))

(defun hyrolo-display-to-entry-end ()
  "Go to end of current entry, ignoring sub-entries."
  (let (case-fold-search)
    (if (re-search-forward hyrolo-hdr-and-entry-regexp nil t)
	(progn (beginning-of-line) (point))
      (goto-char (point-max)))))

(defun hyrolo-edit-date ()
  "Replace an existing date at the end of the current hyrolo entry.
Suitable for use as an entry in `hyrolo-edit-hook'.

The date format is determined by the setting, `hyrolo-date-format', with
a default of MM/DD/YYYY."
  (hyrolo-set-date t))

(defun hyrolo-format-name (name-str first last)
  "Reverse order of NAME-STR field given my regexp match field FIRST and LAST."
  (when (match-beginning last)
    (concat (substring name-str (match-beginning last) (match-end last))
	    ", "
	    (substring name-str (match-beginning first) (match-end first)))))

(defun hyrolo-highlight-matches (regexp start end)
  "Highlight matches for REGEXP in region from START to END."
  (when (fboundp 'hproperty:but-add)
    (let ((hproperty:but-emphasize-flag))
      (save-excursion
	(goto-char start)
	(while (re-search-forward regexp end t)
	  (hproperty:but-add (match-beginning 0) (match-end 0)
			     (or hyrolo-highlight-face
				 hproperty:highlight-face)))))))

(defun hyrolo-install-markdown-mode ()
  "Install `markdown-mode' package unless already installed."
  (unless (package-installed-p 'markdown-mode)
    (unless (assoc "nongnu" package-archives)
      (setq package-archives (cl-copy-list package-archives))
      (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")
		   t))
    (package-refresh-contents)
    (package-install 'markdown-mode)))

(defun hyrolo-isearch-for-regexp (regexp fold-search-flag)
  "Interactively search forward for the next occurrence of REGEXP.
When FOLD-SEARCH-FLAG is non-nil search ignores case.  Then add
characters to further narrow the search."
  (hyrolo-verify)
  (if (stringp regexp)
      (let ((case-fold-search fold-search-flag)
	    (insert-func (lambda () (insert regexp))))
	(unwind-protect
	    (progn (add-hook 'minibuffer-setup-hook insert-func 100)
		   (isearch-forward-regexp))
	  (remove-hook 'minibuffer-setup-hook insert-func)))
    (error "(hyrolo-isearch-for-regexp): 'regexp' must be a string, not: %s" regexp)))

(defun hyrolo-kill-buffer (&optional hyrolo-buf)
  "Kill HYROLO-BUF if unmodified and `hyrolo-kill-buffers-after-use' is t.
HYROLO-BUF is optional; the default is the current buffer."
  (or hyrolo-buf (setq hyrolo-buf (current-buffer)))
  (and hyrolo-kill-buffers-after-use (not (buffer-modified-p hyrolo-buf))
       (kill-buffer hyrolo-buf)))

(defun hyrolo-name-and-email ()
  "If point is in a mail message, return list of (name email-addr) of sender.
Name is returned as `last, first-and-middle'."
  (let ((email) (name) (from))
    (save-window-excursion
      (if (or (hmail:lister-p) (hnews:lister-p))
	  (other-window 1))
      (save-excursion
	(save-restriction
	  (goto-char (point-min))
	  (if (search-forward "\n\n" nil t)
	      (narrow-to-region (point-min) (point)))
	  (setq email (mail-fetch-field "reply-to")
		from  (mail-fetch-field "from")))))
    (if from
	(cond
	 ;; Match: email, email (name), email "name"
	 ((string-match
	   (concat "^\\([^\"<>() \t\n\r\f]+\\)"
		   "\\([ \t]*[(\"][ \t]*\\([^\"()]+\\)[ \t]+"
		   "\\([^\" \t()]+\\)[ \t]*[)\"]\\)?[ \t]*$")
	   from)
	  (setq name (hyrolo-format-name from 3 4))
	  (or email (setq email (match-string 1 from))))
	 ;; Match: <email>, name <email>, "name" <email>
	 ((string-match
	   (concat "^\\(\"?\\([^\"<>()\n]+\\)[ \t]+"
		   "\\([^\" \t()<>]+\\)\"?[ \t]+\\)?"
		   "<\\([^\"<>() \t\n\r\f]+\\)>[ \t]*$")
	   from)
	  (setq name (hyrolo-format-name from 2 3))
	  (or email (setq email (match-string 4 from))))))
    (if (or name email)
	(list name email))))

(defun hyrolo-name-at-p ()
  "Iff point is at or within an entry in `hyrolo-display-buffer', return non-nil.
Any non-nil value returned is a cons of (<entry-name> . <entry-source>)."
  (when (eq (current-buffer) (get-buffer hyrolo-display-buffer))
    (let ((entry-source (hbut:get-key-src t))
	  (col-num (current-column))
	  (line-start (line-beginning-position))
	  (line-end (line-end-position)))
      (when entry-source
	(save-excursion
	  (forward-line 0)
	  (let (case-fold-search
		entry-line
		entry-name)
	    (if (and (or (looking-at hyrolo-hdr-and-entry-regexp)
			 (re-search-backward hyrolo-hdr-and-entry-regexp nil t))
		     (save-match-data (not (looking-at hyrolo-hdr-regexp))))
		(progn (goto-char (match-end 0))
		       (skip-chars-forward " \t")
		       (when (or (looking-at "[^ \t\n\r]+ ?, ?[^ \t\n\r]+")
				 (looking-at "\\( ?[^ \t\n\r]+\\)+"))
			 (setq entry-name (match-string-no-properties 0)
			       entry-line (buffer-substring-no-properties line-start line-end))
			 ;; Add a text-property of 'hyrolo-name-entry with
			 ;; value of (entry-line . current-column) to entry-name.
			 (put-text-property 0 1 'hyrolo-name-entry
					    (cons entry-line col-num)
					    entry-name)
			 (cons entry-name entry-source)))
	      ;; If not blank, return the current line as the name with
	      ;; a text-property of 'hyrolo-line-entry with value of (current-column).
	      (goto-char line-start)
	      (when (not (looking-at "[ \t\f]*$"))
		(setq entry-line (buffer-substring-no-properties line-start line-end))
		(put-text-property 0 1 'hyrolo-line-entry col-num entry-line)
		(cons entry-line entry-source)))))))))

(define-derived-mode hyrolo-org-mode outline-mode "HyRoloOrg"
  "Basic Org mode for use in HyRolo display match searches."
  (require 'org)
  ;; Don't actually derive from `org-mode' to avoid its costly setup but set
  ;; its parent mode property to org-mode so `derived-mode-p' checks will
  ;; pass.
  (put 'hyrolo-org-mode 'derived-mode-parent 'org-mode)

  (font-lock-mode -1) ;; Never font-lock in this mode to keep it fast

  (when (featurep 'org-fold) ;; newer Org versions
    (setq org-fold-core-style 'overlays) ;; Make compatible with reveal minor mode
    (hypb:add-to-invisibility-spec '(org-link))
    (org-fold-initialize (or (and (stringp org-ellipsis) (not (equal "" org-ellipsis)) org-ellipsis)
                             "..."))
    (make-local-variable 'org-link-descriptive)
    (when (eq org-fold-core-style 'overlays)
      (hypb:add-to-invisibility-spec '(org-hide-block . t)))
    (org-fold-core-set-folding-spec-property
     (cond ((boundp 'org-fold-core--specs)
	    ;; Org 9.7 and up
	    (caar org-fold-core--specs))
	   ((boundp 'org-link--link-folding-spec)
	    ;; Org pre-9.7
	    (car org-link--link-folding-spec)))
     :visible (not org-link-descriptive)))

  (setq-local hyrolo-entry-regexp "^\\(\\*+\\)\\([ \t\n\r]+\\)"
	      hyrolo-hdr-and-entry-regexp (concat hyrolo-hdr-prefix-regexp hyrolo-entry-regexp)
	      hyrolo-entry-group-number 1
	      ;; `hyrolo-add' handles removing * prefix from
	      ;; trailing-space grouping below
	      hyrolo-entry-trailing-space-group-number 2
	      outline-regexp (concat hyrolo-hdr-prefix-regexp "^\\(\\*+\\)\\([ \t\n\r]\\)")
	      outline-level #'hyrolo-outline-level)
  (use-local-map org-mode-map)
  ;; Modify a few syntax entries
  (modify-syntax-entry ?\" "\"")
  (modify-syntax-entry ?\\ "_")
  (modify-syntax-entry ?~ "_")
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<")

  (setq-local imenu-generic-expression
	      (if (boundp 'outline-imenu-generic-expression)
		  outline-imenu-generic-expression
		(list (list nil (concat "^\\(?:" outline-regexp "\\).*$") 0))))
  (remove-hook 'change-major-mode-hook #'outline-show-all t)
  (remove-hook 'hack-local-variables-hook #'outline-apply-default-state t)

  ;; Expose hidden text as move into it
  (hyrolo-reveal-mode 1))

(defun hyrolo-save-buffer (&optional hyrolo-buf)
  "Save optional HYROLO-BUF if changed and `hyrolo-save-buffers-after-use' is t.
Default is current buffer.  Used, for example, after a rolo entry is killed."
  (unless hyrolo-buf
    (setq hyrolo-buf (current-buffer)))
  (and hyrolo-save-buffers-after-use (buffer-modified-p hyrolo-buf)
       (set-buffer hyrolo-buf) (save-buffer)))

(defun hyrolo-set-date (&optional edit-only-flag)
  "Add a line with the current date at the end of the current hyrolo entry.
With optional non-nil EDIT-ONLY-FLAG, edit an existing date but do
not add one if the entry lacks one.

Do nothing if in a Koutline buffer or if `hyrolo-date-format' is an
empty string.

Suitable for use as an entry in `hyrolo-add-hook' and `hyrolo-edit-hook'.
The date format is determined by the setting, `hyrolo-date-format'."
  (unless (or (string-empty-p hyrolo-date-format) (null hyrolo-date-format)
	      (derived-mode-p 'kotl-mode))
    (save-excursion
      (skip-chars-forward "*")
      (hyrolo-to-entry-end)
      (skip-chars-backward " \t\n\r\f")
      (skip-chars-backward "^\n\r\f")
      (if (looking-at "\\s-+[-0-9./]+\\s-*$") ;; a date
	  ;; edit date
	  (progn (delete-region (point) (match-end 0))
		 (insert "\t" (hyrolo-current-date)))
	(unless edit-only-flag
	  ;; add date
	  (end-of-line)
	  (insert "\n\t" (hyrolo-current-date)))))))

(defun hyrolo-min-matched-level ()
  "Return the minimum HyRolo outline level within a single file of matches.
This must be 1 or greater."
  (save-excursion
    (goto-char (point-min))
    (let ((min-level 1000))
      (when (looking-at outline-regexp)
	(setq min-level (min min-level (funcall outline-level))))
      (while (outline-next-heading)
	(setq min-level (min min-level (funcall outline-level))))
      (max min-level 1))))

(defun hyrolo-search-directories (search-cmd file-regexp &rest dirs)
  "Search HyRolo over files using SEARCH-CMD matching FILE-REGEXP in rest of DIRS."
  (when (or (null file-regexp) (string-empty-p file-regexp))
    (setq file-regexp hyrolo-file-suffix-regexp))
  (let ((hyrolo-file-list (hypb:filter-directories file-regexp dirs)))
    (call-interactively search-cmd)))

(defun hyrolo-show-levels (levels-to-show)
  "Show only the first line of up to LEVELS-TO-SHOW of HyRolo matches.
LEVELS-TO-SHOW must be 1 or greater and is relative to the first
level of matches, so if LEVELS-TO-SHOW is 2 and the first level
matched from an outline is level 3, then levels 3 and 4 will be
shown.

Any call to this function should be wrapped in a call to
`hyrolo-map-matches'."
  (save-excursion
    (hyrolo-verify)
    (outline-show-all)
    ;; Use {t} to display top-level cells only
    (hyrolo-map-matches
     (lambda ()
       (save-excursion
	 (save-restriction
	   (goto-char (point-min))
	   (hyrolo-hdr-move-after-p)
	   ;; Prevent collapsing of initial file header
	   (narrow-to-region (point) (point-max))
	   (let ((max-level-to-show (+ (hyrolo-min-matched-level)
				       (1- levels-to-show))))
	     (outline-hide-sublevels max-level-to-show)
	     (goto-char (point-min))))))
     t)))

(defun hyrolo-shrink-window ()
  (let* ((lines (count-lines (point-min) (point-max)))
	 (height (window-height))
	 (window-min-height 2)
	 (desired-shrinkage (1- (min (- height lines)))))
    (and (>= lines 0)
	 (/= desired-shrinkage 0)
	 (> (frame-height) (1+ height))
	 (shrink-window
	  (if (< desired-shrinkage 0)
	      (max desired-shrinkage (- height (/ (frame-height) 2)))
	    (min desired-shrinkage (- height window-min-height)))))))

(defun hyrolo-to-buffer (buffer &optional other-window-flag _frame)
  "Pop to BUFFER.
With optional OTHER-WINDOW-FLAG non-nil, pop to a window other
than the selected one."
  (pop-to-buffer buffer other-window-flag))

(defun hyrolo-move-forward (func &rest args)
  "Move forward past any file header and apply FUNC to ARGS.
If FUNC is a lambda (not a function symbol), then temporarily
narrow to the current match buffer before applying FUNC.

Return final point."
  (hyrolo-hdr-to-last-line-p)
  (condition-case nil
      (hyrolo-funcall-match
       (lambda ()
	 (apply func args))
       ;; Narrow to current match buffer when given a lambda func.
       (not (symbolp func)))
    ;; Error means point is before the first buffer heading; move
    ;; past file header to any next entry.
    (error (hyrolo-hdr-move-after-p)))
  (point))

(defun hyrolo-outline-level ()
  "Return the depth to which an entry is nested in the *HyRolo* buffer.
This is actually either the level specified in `outline-heading-alist'
or else the number of characters matched by `outline-regexp' minus
trailing periods and whitespace.

Point must be at the beginning of a heading line and a regexp match to
`outline-regexp' must have been done prior to calling this.

This function is used for every file-type major-mode supported by HyRolo."
  (or (cdr (assoc (match-string-no-properties 0) outline-heading-alist))
      (when (hyrolo-hdr-in-p) 1)
      (cond ((derived-mode-p 'kotl-mode)
	     (kcell-view:level))
	    ((looking-at hyrolo-hdr-and-entry-regexp)
	     (length (match-string hyrolo-entry-group-number)))
	    (t 0))))

;;; ************************************************************************
;;; Caching of buffer major-modes for use in HyRolo display match buffer
;;; ************************************************************************

(defvar-local hyrolo--cache-loc-match-bounds '(1)
  "Ordered list of the bounds of each matched buffer in Hyrolo display buffer.
First entry represents the start of the first matched buffer and the
remaining entries are the end points of each matched buffer with the
HyRolo display matches buffer.")

;; Next line prevents `kill-all-local-variables' run on each major
;; mode change in the HyRolo display buffer from removing the given
;; cache variable.
(put 'hyrolo--cache-loc-match-bounds 'permanent-local t)

(defvar-local hyrolo--cache-major-mode-indexes '(0)
  "Ordered list of major-mode-indexes `hyrolo--cache-loc-match-bounds' positions.")
(put 'hyrolo--cache-major-mode-indexes 'permanent-local t)

(defvar-local hyrolo--cache-major-mode-index 1
  "Next index value to use when caching buffer-local values.")
(put 'hyrolo--cache-major-mode-index 'permanent-local t)

(defvar-local hyrolo--cache-major-mode-to-index-hasht nil
  "Hash table with `major-mode' name keys and integer major-mode index values.")
(put 'hyrolo--cache-major-mode-to-index-hasht 'permanent-local t)

(defvar-local hyrolo--cache-index-to-major-mode-hasht nil
  "Hash table with integer major-mode index keys and `major-mode' values.")
(put 'hyrolo--cache-index-to-major-mode-hasht 'permanent-local t)

(defun hyrolo-cache-get-major-mode-from-pos (pos)
  "Get the `major-mode' associated with POS in the current HyRolo display buffer."
  (hyrolo--cache-get-major-mode-from-index
   (nth (or (seq-position hyrolo--cache-loc-match-bounds pos (lambda (e pos) (< pos e)))
	    (error "(hyrolo-cache-get-major-mode): pos=%d >= max display buffer pos=%d"
		   pos (car hyrolo--cache-loc-match-bounds)))
	hyrolo--cache-major-mode-indexes)))

(defun hyrolo-cache-location-start-and-end ()
  "Return a list of the (start end) of location matches that point is within.
Assume point is in the HyRolo display matches buffer.

Both positions may be nil if there are no matches yet found."
  (let ((end-seq-pos (or (seq-position hyrolo--cache-loc-match-bounds (point) (lambda (e pos) (< pos e)))
			 ;; At (point-max), (= pos e) for final bound in cache
			 (when (>= (point) (car (last hyrolo--cache-loc-match-bounds)))
			   (1- (length hyrolo--cache-loc-match-bounds))))))
    (if end-seq-pos
	(list (nth (1- end-seq-pos) hyrolo--cache-loc-match-bounds)
	      (nth end-seq-pos hyrolo--cache-loc-match-bounds))
      (list nil nil))))

(defun hyrolo-cache-set-major-mode (pos)
  "Set the `major-mode' for POS in the current HyRolo display buffer.
Add `hyrolo-hdr-regexp' to `hyrolo-hdr-and-entry-regexp' and `outline-regexp'."
  (funcall (hyrolo-cache-get-major-mode-from-pos pos))
  (unless (string-prefix-p hyrolo-hdr-regexp hyrolo-hdr-and-entry-regexp)
    (setq-local hyrolo-hdr-and-entry-regexp (concat hyrolo-hdr-prefix-regexp hyrolo-hdr-and-entry-regexp)))
  (unless (string-prefix-p hyrolo-hdr-regexp outline-regexp)
    (setq-local outline-regexp (concat hyrolo-hdr-prefix-regexp outline-regexp)))
  (when (eq outline-level #'markdown-outline-level)
    (setq-local outline-level #'hyrolo-outline-level)))

(defun hyrolo-funcall-match (func &optional narrow-flag backward-flag)
  "Apply FUNC with no arguments to the entry at point.
If on a display match entry or file header, set the appropriate
major mode based on its source location prior to applying FUNC.

With point in the HyRolo display matches buffer and optional
NARROW-FLAG non-nil, narrow to the current file of matches
prior to applying FUNC.

With optional BACKWARD-FLAG, FUNC is moving point backwards; when
on a file boundary, move point back a character to select the
proper major mode."
  (let ((display-buf (get-buffer hyrolo-display-buffer)))
    (if (eq (current-buffer) display-buf)
	(progn
	  (when (< (length hyrolo--cache-loc-match-bounds) 1)
	    (error "(hryolo-funcall-match): No HyRolo matches in display buffer"))
	  (let ((ofont-lock font-lock-mode)
		(omode major-mode)
		(ostart (point-min))
		(oend (point-max)))
	    (unwind-protect
		(cl-destructuring-bind (start end)
		    (hyrolo-cache-location-start-and-end)
		  (setq end (1- (or end (point-max))))
		  (when narrow-flag
		    (narrow-to-region start end))
		  (let ((font-lock-mode))
		    ;; (message "%s" (hyrolo-cache-get-major-mode-from-pos
		    ;;		   (funcall (if backward-flag '1- '1+) start)))
		    (if (and backward-flag (looking-at hyrolo-hdr-regexp))
			(hyrolo-cache-set-major-mode (max (1- start) 1))
		      (hyrolo-cache-set-major-mode (min (1+ start) (point-max))))
		    ;; Prevent Org and Outline minor modes from font-locking
		    (setq font-lock-mode nil)
		    (hyrolo--funcall-with-outline-regexp func)))
	      (with-current-buffer display-buf
		;; func may have changed the current buffer
		(when narrow-flag
		  ;; Restore original restriction
		  (narrow-to-region ostart oend))
		;; Restore original mode and font-locking
		(funcall omode)
		(font-lock-mode (if ofont-lock 1 0))
		(when (and (fboundp 'orgtbl-mode) orgtbl-mode)
		  ;; Disable as overrides single letter keys
		  (orgtbl-mode 0))
		;; Need to leave point on a visible character or since
		;; hyrolo uses reveal-mode, redisplay will rexpand
		;; hidden entries to make point visible.
		;; (hyrolo-back-to-visible-point)
		;; This pause forces a window redisplay that maximizes the
		;; entries displayed for any final location of point.
		;; Comment it out for now and see how well movement
		;; cmds work.
		;; (sit-for 0.0001)
		))))
      (hyrolo--funcall-with-outline-regexp func))))

(defun hyrolo-map-matches (func &optional narrow-flag)
  "Map FUNC with no arguments over the current buffer of entries.
FUNC must not move point, as this function will restore it.  If
on a display match entry, set the appropriate major mode based on
its source location.

With point in the HyRolo display matches buffer and optional
NARROW-FLAG non-nil, narrow to the current file of matches
prior to applying FUNC."
  (when (zerop (buffer-size (current-buffer)))
    (error "(hryolo-map-matches): No HyRolo matches in current buffer"))
  (let ((display-buf (get-buffer hyrolo-display-buffer)))
    (if (eq (current-buffer) display-buf)
	(let ((bounds hyrolo--cache-loc-match-bounds)
	      (ofont-lock font-lock-mode)
	      (omode major-mode)
	      (ostart (point-min))
	      (oend (point-max))
	      start
	      end)
	  (unwind-protect
	      (save-excursion
		(while (setq start (car bounds)
			     end (cadr bounds))
		  (setq end (1- (or end (point-max)))
			bounds (cdr bounds))
		  (when narrow-flag
		    (narrow-to-region start end))
		  (goto-char start)
		  (let ((font-lock-mode))
		    (hyrolo-cache-set-major-mode (1+ start))
		    (setq font-lock-mode nil) ;; Prevent Org mode from font-locking
		    (hyrolo--funcall-with-outline-regexp func))))
	    (when narrow-flag
	      ;; Restore original restriction
	      (narrow-to-region ostart oend))
	    ;; Restore original mode and font-locking
	    (funcall omode)
	    (font-lock-mode (if ofont-lock 1 0))
	    (when (and (fboundp 'orgtbl-mode) orgtbl-mode)
	      ;; Disable as overrides single letter keys
	      (orgtbl-mode 0))
	    ;; Need to leave point on a visible character or since
	    ;; hyrolo uses reveal-mode, redisplay will rexpand
	    ;; hidden entries to make point visible.
	    (hyrolo-back-to-visible-point)
	    ;; This pause forces a window redisplay that maximizes the
	    ;; entries displayed for any final location of point.
	    (sit-for 0.001)))
      (save-excursion
	(hyrolo--funcall-with-outline-regexp func)))))

(defun hyrolo--cache-get-major-mode-from-index (major-mode-index)
  "Return `major-mode' key from hash table entry with key MAJOR-MODE-INDEX.
Return nil if not found."
  (gethash major-mode-index hyrolo--cache-index-to-major-mode-hasht))

(defun hyrolo--cache-initialize ()
  "Init cache hash table of (major-mode-name . loc-seq-number) key value pairs.
Call whenever `hyrolo-display-buffer' is changed."
  (with-current-buffer hyrolo-display-buffer
    (setq-local hyrolo--cache-major-mode-to-index-hasht
		(if (hash-table-p hyrolo--cache-major-mode-to-index-hasht)
		    (clrhash hyrolo--cache-major-mode-to-index-hasht)
		  (make-hash-table))

		hyrolo--cache-index-to-major-mode-hasht
		(if (hash-table-p hyrolo--cache-index-to-major-mode-hasht)
		    (clrhash hyrolo--cache-index-to-major-mode-hasht)
		  (make-hash-table))

		;; Don't use '(1) on the next line or the code will not initialize properly
		hyrolo--cache-loc-match-bounds (list 1)
		hyrolo--cache-major-mode-indexes (list 0)
		hyrolo--cache-major-mode-index 1)))

(defun hyrolo--cache-major-mode (matched-buf)
  "Cache buffer `major-mode' for MATCHED-BUF with point in HyRolo display buffer.
MATCHED-BUF must be a live buffer, not a buffer name.

Push (point-max) of `hyrolo-display-buffer' onto
`hyrolo--cache-loc-match-bounds'.  Push hash table's index key to
`hyrolo--cache-major-mode-indexes'.  Ensure MATCHED-BUF's
`major-mode' is stored in the hash table."
  (with-current-buffer hyrolo-display-buffer
    (let* ((matched-buf-file-name (buffer-local-value 'buffer-file-name matched-buf))
	   (matched-buf-major-mode (or (hyrolo-major-mode-from-file-name matched-buf-file-name)
				       (buffer-local-value 'major-mode matched-buf)))
	   (matched-buf-major-mode-name (symbol-name matched-buf-major-mode))
	   (matched-buf-major-mode-index
	    (gethash matched-buf-major-mode-name hyrolo--cache-major-mode-to-index-hasht)))
      (push (point-max) hyrolo--cache-loc-match-bounds)
      (push (or matched-buf-major-mode-index hyrolo--cache-major-mode-index) hyrolo--cache-major-mode-indexes)
      (unless matched-buf-major-mode-index
	(puthash matched-buf-major-mode-name
		 hyrolo--cache-major-mode-index hyrolo--cache-major-mode-to-index-hasht)
	(puthash hyrolo--cache-major-mode-index matched-buf-major-mode hyrolo--cache-index-to-major-mode-hasht)
	(setq-local hyrolo--cache-major-mode-index (1+ hyrolo--cache-major-mode-index))))))

(defun hyrolo--cache-post-display-buffer ()
  "Cache update to make after display buffer modifications are finished."
  ;; Reverse both of the above lists to order them properly.
  (with-current-buffer hyrolo-display-buffer
    (setq-local hyrolo--cache-loc-match-bounds   (nreverse hyrolo--cache-loc-match-bounds)
		hyrolo--cache-major-mode-indexes (nreverse hyrolo--cache-major-mode-indexes))))

(defun hyrolo--funcall-with-outline-regexp (func)
  "Call FUNC with `outline-regexp' temporarily set to support HyRolo file hdrs."
  (let ((saved-outline-regexp outline-regexp))
    (unwind-protect
	(progn (setq outline-regexp hyrolo-hdr-and-entry-regexp)
	       (funcall func))
      (setq outline-regexp saved-outline-regexp))))

;;; ************************************************************************
;;; hyrolo-mode key bindings - set after all library functions have
;;; been defined
;;; ************************************************************************

(if hyrolo-mode-map
    nil
  (setq hyrolo-mode-map (make-sparse-keymap "HyRolo")
	hyrolo-mode-prefix-map (copy-keymap outline-mode-prefix-map))
  (suppress-keymap hyrolo-mode-map)

  (define-key hyrolo-mode-map "\C-c"     hyrolo-mode-prefix-map)
  (define-key hyrolo-mode-map ","        'hyrolo-to-entry-beginning)
  (define-key hyrolo-mode-map "."        'hyrolo-to-entry-end)
  (define-key hyrolo-mode-map "<"        'beginning-of-buffer)
  (define-key hyrolo-mode-map ">"        'end-of-buffer)
  (define-key hyrolo-mode-map "["        'hyrolo-to-previous-loc)
  (define-key hyrolo-mode-map "]"        'hyrolo-to-next-loc)
  (define-key hyrolo-mode-map "?"        'describe-mode)
  (define-key hyrolo-mode-map "\177"     'scroll-down)
  (define-key hyrolo-mode-map " "        'scroll-up)
  (define-key hyrolo-mode-map "a"        'outline-show-all)
  (define-key hyrolo-mode-map "b"        'hyrolo-outline-backward-same-level)
  (define-key hyrolo-mode-map "e"        'hyrolo-edit-entry)
  (define-key hyrolo-mode-map "f"        'hyrolo-outline-forward-same-level)
  (define-key hyrolo-mode-map "h"        'hyrolo-outline-hide-subtree)
  (define-key hyrolo-mode-map "l"        'hyrolo-locate)
  (define-key hyrolo-mode-map "m"        'hyrolo-mail-to)
  (define-key hyrolo-mode-map "n"        'hyrolo-outline-next-visible-heading)
  (define-key hyrolo-mode-map "o"        'hyrolo-overview)
  (define-key hyrolo-mode-map "p"        'hyrolo-outline-previous-visible-heading)
  (define-key hyrolo-mode-map "q"        'hyrolo-quit)
  (define-key hyrolo-mode-map "r"        'hyrolo-grep-or-fgrep)
  (define-key hyrolo-mode-map "s"        'hyrolo-outline-show-subtree)
  (define-key hyrolo-mode-map "\M-s"     'hyrolo-isearch)
  (define-key hyrolo-mode-map "t"        'hyrolo-top-level)
  (define-key hyrolo-mode-map "\C-i"     'hyrolo-next-match) ;; {TAB}
  (define-key hyrolo-mode-map "\M-\C-i"  'hyrolo-previous-match) ;; {M-TAB}
  (define-key hyrolo-mode-map [backtab]  'hyrolo-previous-match) ;; {Shift-TAB}
  (define-key hyrolo-mode-map "u"        'hyrolo-outline-up-heading)

  ;; Rebind all `outline-mode-prefix-map' keys to hyrolo equivalents
  (let (otl-cmd-name
	hyrolo-cmd-name
	hyrolo-cmd)
    (map-keymap
     (lambda (event otl-cmd)
       (when (and (event-basic-type event) ;; key or mouse event
		  (symbolp otl-cmd) (setq otl-cmd-name (symbol-name otl-cmd))
		  (setq hyrolo-cmd-name (concat "hyrolo-" otl-cmd-name)
			hyrolo-cmd (intern-soft hyrolo-cmd-name)))
	 (substitute-key-definition otl-cmd hyrolo-cmd hyrolo-mode-map)))
     outline-mode-prefix-map)))

;;; ************************************************************************
;;; hyrolo-reveal - Extend reveal-mode to support Org mode org-fold
;;; ************************************************************************

(define-minor-mode hyrolo-reveal-mode
  "Toggle uncloaking of invisible text near point (Reveal mode).

Reveal mode is a buffer-local minor mode.  When enabled, it
reveals invisible text around point.

Also see the `reveal-auto-hide' variable."
  :init-value nil
  :keymap nil
  nil) ;; Make this a no-op until can debug `reveal-mode' in *HyRolo* buffer

(unless (boundp 'reveal-auto-hide)
(defcustom reveal-auto-hide t
  "Automatically hide revealed text when leaving it.
If nil, the `reveal-hide-revealed' command can be useful to hide
revealed text manually."
  :type 'boolean
  :version "28.1"))

(unless (fboundp 'reveal-hide-revealed)
(defun reveal-hide-revealed ()
  "Hide all revealed text.
If there is revealed text under point, this command does not hide
that text."
  (interactive)
  (let ((reveal-auto-hide t))
    (reveal-post-command))))

(defun hyrolo-reveal-open-new-overlays (old-ols)
  (let ((repeat t))
    (while repeat
      (setq repeat nil)
      (dolist (ol (nconc (when (and reveal-around-mark mark-active)
                           (overlays-at (mark)))
                         (overlays-at (point))))
        (setq old-ols (delq ol old-ols))
        (when (overlay-start ol)        ;Check it's still live.
          ;; We either have an invisible overlay, or a display
          ;; overlay.  Always reveal invisible text, but only reveal
          ;; display properties if `reveal-toggle-invisible' is
          ;; present.
          (let ((inv (overlay-get ol 'invisible))
                (disp (and (overlay-get ol 'display)
                           (overlay-get ol 'reveal-toggle-invisible)))
                open)
            (when (and (or (and inv
                                ;; There's an `invisible' property.
                                ;; Make sure it's actually invisible,
                                ;; and ellipsized.
                                (and (consp buffer-invisibility-spec)
                                     (cdr (assq inv buffer-invisibility-spec))))
                           disp)
                       (or (setq open
                                 (or (overlay-get ol 'reveal-toggle-invisible)
                                     (and (symbolp inv)
                                          (get inv 'reveal-toggle-invisible))
                                     (overlay-get
                                      ol 'isearch-open-invisible-temporary)))
                           (overlay-get ol 'isearch-open-invisible)
                           (and (consp buffer-invisibility-spec)
                                (cdr (assq inv buffer-invisibility-spec)))))
              (when inv
                (overlay-put ol 'reveal-invisible inv))
              (push (cons (selected-window) ol) reveal-open-spots)
              (if (null open)
                  (overlay-put ol 'invisible nil)
                ;; Use the provided opening function and repeat (since the
                ;; opening function might have hidden a subpart around point
                ;; or moved/killed some of the overlays).
                (setq repeat t)
                (condition-case err
                    (funcall open ol nil)
                  (error (message "!!Reveal-show (funcall %s %s nil): %s !!"
                                  open ol err)
                         ;; Let's default to a meaningful behavior to avoid
                         ;; getting stuck in an infinite loop.
                         (setq repeat nil)
			 (overlay-put ol 'invisible nil))))))))))
  old-ols)

(defun hyrolo-reveal-close-old-overlays (old-ols)
  (if (or track-mouse                   ;Don't close in the middle of a click.
          (not (eq reveal-last-tick
                   (setq reveal-last-tick (buffer-modified-tick)))))
      ;; The buffer was modified since last command: let's refrain from
      ;; closing any overlay because it tends to behave poorly when
      ;; inserting text at the end of an overlay (basically the overlay
      ;; should be rear-advance when it's open, but things like
      ;; outline-minor-mode make it non-rear-advance because it's
      ;; a better choice when it's closed).
      nil
    ;; The last command was only a point motion or some such
    ;; non-buffer-modifying command.  Let's close whatever can be closed.
    (dolist (ol old-ols)
      (if (and (overlay-start ol)       ;Check it's still live.
               (>= (point) (save-excursion
                             (goto-char (overlay-start ol))
                             (line-beginning-position 1)))
               (<= (point) (save-excursion
                             (goto-char (overlay-end ol))
                             (line-beginning-position 2)))
               ;; If the application has moved the overlay to some other
               ;; buffer, we'd better reset the buffer to its
               ;; original state.
               (eq (current-buffer) (overlay-buffer ol)))
          ;; Still near the overlay: keep it open.
          nil
        ;; Really close it.
        (let* ((inv (overlay-get ol 'reveal-invisible))
               (open (or (overlay-get ol 'reveal-toggle-invisible)
                         (get inv 'reveal-toggle-invisible)
                         (overlay-get ol 'isearch-open-invisible-temporary))))
          (if (and (overlay-start ol)   ;Check it's still live.
                   open)
              (condition-case err
                  (hyrolo-funcall-match (lambda () (funcall open ol t)) t)
		(error (message "!!Reveal-hide (funcall %s %s t): %s !!"
                                open ol err)))
            (overlay-put ol 'invisible nil))
          ;; Remove the overlay from the list of open spots.
          (overlay-put ol 'reveal-invisible nil)
          (setq reveal-open-spots
                (delq (rassoc ol reveal-open-spots)
                      reveal-open-spots)))))))

;; Note that `outline-reveal-toggle-invisible' is the function
;; stored in the `outline' `reveal-toggle-invisible' property.  It
;; is called from `hyrolo-reveal-open-new-overlays' and
;; `hyrolo-reveal-close-old-overlays' which are called from within
;; `reveal-post-command' on `post-command-hook'.  Below we update
;; `reveal-post-command' to work with HyRolo.

(defun reveal-post-command ()
  (if hyrolo-reveal-ignore-this-command
      (setq hyrolo-reveal-ignore-this-command nil)
    ;; Refresh the spots that might have changed.
    ;; `Refreshing' here means to try and re-hide the corresponding text.
    ;; We don't refresh everything correctly:
    ;; - we only refresh spots in the current window.
    ;; FIXME: do we actually know that (current-buffer) = (window-buffer) ?
    (with-local-quit
      (with-demoted-errors "Reveal: %s"
	(let ((old-ols
               (delq nil
                     (mapcar
                      (lambda (x)
			;; We refresh any spot in the current window as well
			;; as any spots associated with a dead window or
			;; a window which does not show this buffer any more.
			(cond
			 ((eq (car x) (selected-window)) (cdr x))
			 ((not (and (window-live-p (car x))
                                    (eq (window-buffer (car x))
					(current-buffer))))
                          ;; Adopt this since it's owned by a window that's
                          ;; either not live or at least not showing this
                          ;; buffer any more.
                          (setcar x (selected-window))
                          (cdr x))))
                      reveal-open-spots))))
          (setq old-ols (hyrolo-reveal-open-new-overlays old-ols))
          (when reveal-auto-hide
	    (hyrolo-reveal-close-old-overlays old-ols)))))))

(defun hyrolo-show-post-command ()
  "Post command hook function to expand subtree if point is in invisible text.
Used in *HyRolo* match buffer."
  (when (outline-invisible-p)
    (hyrolo-outline-show-subtree)))

;;; ************************************************************************
;;; hyrolo-file-list - initialize cache if this is already set when loading
;;; ************************************************************************

(when (and hyrolo-file-list (null hyrolo--expanded-file-list))
  (hyrolo-set-file-list 'hyrolo-file-list hyrolo-file-list))

(provide 'hyrolo)

;;; hyrolo.el ends here
