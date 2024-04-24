;;; hywiki.el --- Hyperbole's auto-wikiword note-taking system   -*- lexical-binding: t -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     21-Apr-24 at 22:41:13
;; Last-Mod:     24-Apr-24 at 02:26:23 by Bob Weiner
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
;;    - a whitespace character is inserted following a HyWiki word
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
(require 'hui-em-but)
(require 'ol)

(when (package-installed-p 'company)
  (package-activate 'company)
  (require 'company)
  (add-to-list 'company-backends 'hywiki-company-hasht-backend))

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

(defvar hywiki-allowed-modes '(text-mode wiki-mode)
  "Parent modes where HyWiki words are recognized without delimiters.
Applies only when the file is below `hywiki-directory'.")

(defconst hywiki-directory '"~/hywiki/"
  "Directory in which to find HyWiki page files.")

(defconst hywiki-org-link-type "hy"
  "HyWiki string prefix type for Org links.  Excludes trailing colon.")

(defvar hywiki-org-link-type-required t
  "When non-nil, HyWiki Org links must start with `hywiki-org-link-type'.
Otherwise, this prefix is not needed and HyWiki word Org links
override standard Org link lookups.  See \"(org)Internal Links\".")

(defvar hywiki-pages-hasht nil)

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
;;; Public Implicit Button and Action Types
;;; ************************************************************************

(defib hywiki ()
  "When on a HyWiki word, display its page and optional section."
  (let* ((page-name (hywiki-at-wikiword)))
    (when page-name
      (ibut:label-set page-name (match-beginning 0) (match-end 0))
      (hywiki-highlight-page-name t)
      (hact 'hywiki-find-page page-name))))

(defun hywiki-find-page (page-name &optional prompt-flag)
  "Display HyWiki PAGE-NAME.  By default, create any non-existent page.
With optional PROMPT-FLAG t, prompt to create if non-existent.  If
PROMPT-FLAG is 'exists, return nil unless the page already exists."
  (interactive (list (completing-read "Find HyWiki page: " (hywiki-get-page-list))))
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
      (hpath:find (concat page-file section)))))

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
	(set-buffer page-buf)
	(barf-if-buffer-read-only)
	(save-restriction
	  (widen)
	  (goto-char (if start-flag (point-min) (point-max)))
	  (unless (bolp) (insert (newline)))
	  (insert text)
	  (unless (bolp) (insert (newline)))
	  (goto-char (if start-flag (point-min) (point-max)))
	  page-file)))))

(defun hywiki-at-wikiword (&optional org-link-flag)
  "Return HyWiki word and optional #section at point or nil if not on one.
Does not test whether or not a page exists for the HyWiki word.
Use `hywiki-get-page' to determine whether a HyWiki page exists."
  (let (wikiword)
    (if (or org-link-flag (hsys-org-link-at-p))
	;; Handle an Org link [[HyWiki word]] [[hy:HyWiki word]] or [[HyWiki word#section]]
	(progn
	  (setq wikiword
		(org-link-expand-abbrev
		 (org-link-unescape
		  (string-trim (match-string-no-properties 1)))))
	  ;; Ignore hy:word hywiki:word since Org mode will display those
	  (when (hywiki-is-wikiword wikiword)
	    wikiword))
      ;; Handle a HyWiki word with optional #section; if it is an Org
      ;; link, it may or may not have a hy: link-type prefix.
      (and (apply #'derived-mode-p hywiki-allowed-modes)
	   (string-prefix-p (expand-file-name hywiki-directory)
			    (or buffer-file-name ""))
	   (save-excursion
             (let ((case-fold-search nil))
	       (skip-chars-backward "-*#[:alnum:]")
	       ;; Ignore wikiwords preceded by any non-whitespace character
	       (and (or (bolp) (memq (preceding-char) '(?\[ ?\  ?\t ?\n ?\r ?\f)))
		    (looking-at hywiki-word-optional-section-regexp)
		    (string-trim (match-string-no-properties 0)))))))))

;; Globally set these values to avoid using 'let' with stack allocations
;; within `hywiki-highlight-page-name' frequently.
(setq hywiki--any-page-regexp nil
      hywiki--but nil
      hywiki--but-end nil
      hywiki--but-start nil
      hywiki--current-page nil
      hywiki--end nil
      hywiki--page-name nil
      hywiki--save-case-fold-search nil
      hywiki--save-org-link-type-required nil
      hywiki--start nil)

(defun hywiki-highlight-page-names ()
  "Highlight all non-Org link HyWiki page names in the buffer.
Use `hywiki-word-face' to highlight.  Does not highlight references to
the current page unless they have sections attached.

Used as a `find-file-hook'."
  (interactive)
  ;; Avoid doing any lets in this `post-self-insert-function' for efficiency

  ;; Highlight HyWiki words only in files below `hywiki-directory'
  (when (and hywiki-word-highlight-flag
	     (string-prefix-p (expand-file-name hywiki-directory)
			      (or buffer-file-name "")))
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
	    ;; Ignore wikiwords preceded by any non-whitespace character
	    (when (or (bolp) (memq (preceding-char) '(?\  ?\t)))
	      ;; Include any #section
	      (skip-syntax-forward "^-\)$\>.\"\'")
	      (skip-chars-forward "-#[:alnum:]")
	      (setq hywiki--end (point))
	      ;; Don't highlight current-page matches unless
	      ;; they include a #section.
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
the current page unless they have sections attached.

Used as a `post-self-insert-hook'."
  (interactive)
  (when (and hywiki-word-highlight-flag
	     (or on-page-name
		 (and (eq (char-before) last-command-event) ; Sanity check
		      (not (eq ?# last-command-event))
		      (memq (char-syntax last-command-event) '(?\  ?\) ?\$ ?\> ?. ?\" ?\'))))
             (not executing-kbd-macro)
             (not noninteractive)
	     (string-prefix-p (expand-file-name hywiki-directory)
			      (or buffer-file-name "")))
    (save-excursion
      (unless on-page-name
	;; after page name
	(goto-char (max (1- (point)) (point-min))))
      (skip-syntax-backward "^-\)$\>.\"\'")
      (skip-chars-backward "#[:alpha:]")

      (setq hywiki--save-case-fold-search case-fold-search
	    case-fold-search nil
	    hywiki--save-org-link-type-required hywiki-org-link-type-required
	    hywiki-org-link-type-required t)
      (if (and (looking-at hywiki-word-optional-section-regexp)
	       ;; Ignore wikiwords preceded by any non-whitespace character
	       (or (bolp) (memq (preceding-char) '(?\  ?\t ?\n ?\r ?\f)))
	       (progn
		 (setq hywiki--page-name (match-string-no-properties 1)
		       hywiki--start (match-beginning 0)
		       hywiki--end   (match-end 0))
		 (and (hywiki-get-page hywiki--page-name)
		      ;; Ignore wikiwords preceded by any non-whitespace character
		      (or (bolp) (memq (preceding-char) '(?\  ?\t))))))
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
	(skip-syntax-backward "^-\)$\>.\"\'")
	(hproperty:but-clear (point) 'face hywiki-word-face)))))

(defun hywiki-is-wikiword (word)
  "Return non-nil if WORD is a HyWiki word and optional #section.
The page for the word may not yet exist.  Use `hywiki-get-page'
to determine whether a HyWiki word page exists."
  (and (stringp word)
       (or (eq (string-match (concat "\\`" hywiki-word-org-link-regexp "\\'") word)
	       0)
	   (eq (string-match (concat "\\`" hywiki-word-optional-section-regexp "\\'") word)
	       0))))

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
  (directory-files-recursively hywiki-directory (concat "^" hywiki-word-regexp "\\.[A-Za-z0-9]+$")))

(defun hywiki-get-page-hasht ()
  "Return hash table of existing HyWiki pages."
  (or hywiki-pages-hasht (hywiki-make-pages-hasht)))

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
    (setq hywiki-pages-hasht (hash-make page-elts))))

(defun hywiki-company-hasht-backend (command &optional _arg &rest ignored)
 "A `company-mode` backend that completes from the keys of a hash table."
 (interactive (list 'interactive))
 (when (hywiki-at-wikiword)
   (case command
     ('interactive (company-begin-backend 'company-hash-table-backend))
     ('prefix (company-grab-word))
     ('candidates
      (let ((prefix (company-grab-word)))
	(when prefix 
          (cl-loop for key being the hash-keys in (hywiki-get-page-list)
                   when (string-prefix-p prefix key)
                   collect key))))
     ('sorted t))))

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

(add-hook 'org-mode-hook
	  (lambda ()
	    (make-local-variable 'find-file-hook)
	    (make-local-variable 'post-self-insert-hook)
	    (add-hook 'find-file-hook #'hywiki-highlight-page-names t)
	    (add-hook 'post-self-insert-hook #'hywiki-highlight-page-name t)))

(provide 'hywiki)
