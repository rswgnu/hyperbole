;;; hsys-org.el --- GNU Hyperbole support functions for Org mode  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     2-Jul-16 at 14:54:14
;; Last-Mod:     24-Apr-25 at 15:31:03 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2016-2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   Support functions for "hui-mouse.el#smart-org".
;;
;;   smart-org is triggered with the press of a Smart Key within an org-mode
;;   (or org-mode derived) buffer.
;;
;;   See the doc for smart-org for details of what it does and its
;;   compatibility with org-mode.
;;
;;   For a good tutorial on basic use of Org-mode, see:
;;     https://orgmode.org/worg/org-tutorials/orgtutorial_dto.html

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-when-compile (require 'hmouse-drv))
(require 'hproperty) ;; requires 'hbut
(require 'hsys-consult)
(require 'hypb)
(require 'org)
(require 'org-element)
(require 'org-fold nil t)
(require 'org-macs)
(require 'package)
(require 'warnings)
;; Avoid any potential library name conflict by giving the load directory.
(require 'set (expand-file-name "set" hyperb:dir))

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

;; Bind {C-c C-@} to create a user-specified sized grid of windows
;; displaying different buffers.
;; Don't override prior bindings of this key.
(unless (lookup-key org-mode-map "\C-c\C-@")
  (define-key org-mode-map "\C-c\C-@" #'hycontrol-windows-grid))

(defvar hyperbole-mode-map)             ; "hyperbole.el"
(defvar org--inhibit-version-check)     ; "org-macs.el"
(defvar hywiki-org-link-type-required)  ; "hywiki.el"
(defvar org-agenda-buffer-tmp-name)     ; "org-agenda.el"

(declare-function hycontrol-windows-grid "hycontrol")
(declare-function hyrolo-tags-view "hyrolo")
(declare-function hyrolo-at-tags-p "hyrolo")
(declare-function hywiki-at-tags-p "hywiki")
(declare-function hywiki-tags-view "hywiki")
(declare-function hsys-org-roam-tags-view "hsys-org")

(declare-function org-babel-get-src-block-info "org-babel")
(declare-function org-fold-show-context "org-fold")
(declare-function org-link-open-from-string "ol")
(declare-function outline-on-heading-p "outline")

(declare-function smart-eobp "hui-mouse")
(declare-function smart-eolp "hui-mouse")
(declare-function hargs:read-match "hargs")
(declare-function symset:add "hact")
(declare-function symtable:add "hact")
(declare-function action-key "hmouse-drv")
(declare-function hkey-either "hmouse-drv")

(declare-function find-library-name "find-func")
(declare-function hyperb:stack-frame "hversion.el")

;;;###autoload
(defcustom hsys-org-enable-smart-keys 'unset
  "This applies in Org major/minor modes only when `hyperbole-mode' is active.
If set to \\='unset prior to loading Hyperbole, then Hyperbole initialization
will set its value.

The following table shows what the Smart Keys do in various contexts with
different settings of this option.  For example, a nil value makes {M-RET}
operate as it normally does within Org mode contexts.

|---------+-------------------+------------------+----------+------------------|
| Setting | Smart Key Context | Hyperbole Button | Org Link | Fallback Command |
|---------+-------------------+------------------+----------+------------------|
| buttons | Ignore            | Activate         | Activate | org-meta-return  |
| nil     | Ignore            | Ignore           | Ignore   | org-meta-return  |
| t       | Activate          | Activate         | Activate | None             |
|---------+-------------------+------------------+----------+------------------|"
  :type '(choice (const :tag "buttons - In Org, enable Smart Keys within Hyperbole buttons only" buttons)
		 (const :tag "nil     - In Org, disable Smart Keys entirely" nil)
		 (const :tag "t       - In Org, enable all Smart Key contexts" t))
  :initialize #'custom-initialize-default
  :group 'hyperbole-buttons)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;;;###autoload
(defvar hsys-org-mode-function #'hsys-org-mode-p
  "*Boolean function that returns non-nil when point is in an Org-related buffer.")

;;; ************************************************************************
;;; Public Action Types
;;; ************************************************************************

(defcustom hsys-org-cycle-bob-file-list
  '("${hyperb:dir}/DEMO" "${hyperb:dir}/FAST-DEMO" "${hyperb:dir}/MANIFEST"
     "${hyperb:dir}/HY-ABOUT" "${hyperb:dir}/HY-NEWS")
  "List of files to globally `org-cycle' when at the beginning of the buffer."
  :type '(repeat file)
  :group 'hyperbole)

(defact org-link (&optional link)
  "Follow an optional Org mode LINK to its target.
If LINK is nil, follow any link at point.  Otherwise, trigger an error."
  (if (stringp link)
      ;; open as if in Org mode even if not
      (org-link-open-from-string link)
    ;; autoloaded, open link at point whether in or out of Org mode
    (if (derived-mode-p 'org-mode)
	(org-open-at-point)
      (org-open-at-point-global))))

(defact org-internal-target-link (&optional internal-target)
  "Follow an optional Org mode <<INTERNAL-TARGET>> back to any first link to it.
If INTERNAL-TARGET is nil, follow any internal target link at point. Otherwise,
trigger an error."
  (let (start-end)
    (cond ((stringp internal-target)
	   (setq start-end t)
	   (hsys-org-search-internal-link-p internal-target))
	  ((null internal-target)
	   (when (setq start-end (hsys-org-internal-target-def-at-p))
	     (hsys-org-search-internal-link-p (buffer-substring-no-properties
					       (car start-end) (cdr start-end))))))
    (unless start-end
      (error "(org-internal-target-link): Point must be on a link target (not the link itself)"))))

(defact org-radio-target-link (&optional target)
  "Follow an optional Org mode radio <<TARGET>> back to any first link to it.
If TARGET is nil, follow any radio target link at point.  Otherwise, trigger
an error."
  (let (start-end)
    (cond ((stringp target)
	   (setq start-end t)
	   (hsys-org-to-next-radio-target-link target))
	  ((null target)
	   (when (setq start-end (hsys-org-radio-target-at-p))
	     (hsys-org-to-next-radio-target-link (buffer-substring-no-properties
					          (car start-end) (cdr start-end))))))
    (unless start-end
      (error "(org-radio-target-link): Point must be on a radio target definition or link"))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun hsys-org-agenda-tags-p ()
  "When on an Org tag, return appropriate `org-tags-view'-like function.
Use `default-directory' and buffer name to determine which function to
call."
  (when (hsys-org-at-tags-p)
    (cond ((hsys-org-directory-at-tags-p t)
	   #'hsys-org-agenda-tags)
	  ((hsys-org-roam-directory-at-tags-p t)
	   #'hsys-org-roam-agenda-tags)
	  ((hywiki-at-tags-p t)
	   #'hsys-org-hywiki-agenda-tags)
	  ((hyrolo-at-tags-p t)
	   #'hsys-org-hyrolo-agenda-tags))))

(defun hsys-org-get-agenda-tags (agenda-tags-view-function)
  "When on an Org tag, call AGENDA-TAGS-VIEW-FUNCTION to find matches.
If on a colon, match to sections with all tags around point;
otherwise, just match to the single tag around point.

The function determines the org files searched for matches and is
given two arguments when called: a null value to tell it to match to
all headlines, not just todos and an Org tags match selector string,
e.g. \":tag1:tag2:tag3:\".  Headlines that contain or inherit all of
these tags will be displayed, regardless of tag order."
  (interactive)
  (when (hsys-org-at-tags-p)
    (funcall agenda-tags-view-function nil (hsys-org--agenda-tags-string))))

(defun hsys-org-hyrolo-agenda-tags ()
  "When on a HyRolo tag, use `hyrolo-tags-view' to list all HyRolo tag matches.
If on a colon, match to sections with all tags around point;
otherwise, just match to the single tag around point."
  (interactive)
  (hsys-org-get-agenda-tags #'hyrolo-tags-view))

(defun hsys-org-hywiki-agenda-tags ()
  "When on a HyWiki tag, use `hywiki-tags-view' to list all HyWiki tag matches.
If on a colon, match to sections with all tags around point;
otherwise, just match to the single tag around point."
  (interactive)
  (hsys-org-get-agenda-tags #'hywiki-tags-view))

(defun hsys-org-agenda-tags ()
  "On an `org-directory' tag, use `hsys-org-tags-view' to list dir tag matches.
If on a colon, match to sections with all tags around point;
otherwise, just match to the single tag around point."
  (interactive)
  (hsys-org-get-agenda-tags #'hsys-org-tags-view))

(defun hsys-org-roam-agenda-tags ()
  "On an `org-roam-directory' tag, use `hsys-org-roam-tags-view' to list matches.
If on a colon, match to sections with all tags around point;
otherwise, just match to the single tag around point."
  (interactive)
  (hsys-org-get-agenda-tags #'hsys-org-roam-tags-view))

;;;###autoload
(defun hsys-org-tags-view (&optional todo-only match view-buffer-name)
  "Prompt for colon-separated Org tags and display matching Org headlines.
With optional prefix arg TODO-ONLY, limit matches to Org todo
items only.  With optional MATCH, an Org tags match selector
string, e.g. \":tag1:tag2:tag3:\", match to sections that contain
or inherit all of these tags, regardless of tag order.  With
optional VIEW-BUFFER-NAME, use that rather than the default,
\"*Org Tags*\"."
  (interactive "P")
  (require 'org-agenda)
  (let* ((org-agenda-files (list org-directory))
	 (org-agenda-buffer-name (or view-buffer-name "*Org Tags*"))
	 ;; `org-tags-view' is mis-written to require setting this next
	 ;; tmp-name or it will not properly name the displayed buffer.
	 (org-agenda-buffer-tmp-name org-agenda-buffer-name))
    ;; This prompts for the tags to match and uses `org-agenda-files'.
    (org-tags-view todo-only match)
    (when (equal (buffer-name) org-agenda-buffer-name)
      ;; Set up {C-u r} redo cmd
      (let (buffer-read-only)
	(put-text-property (point-min) (point-max) 'org-redo-cmd
			   `(hsys-org-tags-view
			       ,todo-only
			       nil
			       ,org-agenda-buffer-name)))
      (forward-line 2))))

;;;###autoload
(defun hsys-org-fix-version ()
  "If multiple Org versions are loaded, use the one first on `load-path'.
Always ensure Org libraries have been required.
Return t if Org is reloaded, else nil."
  ;; Not all versions of org include these variables, so set them
  (setq org--inhibit-version-check nil
	org-list-allow-alphabetical nil)
  (let ((org-dir (ignore-errors (org-find-library-dir "org")))
	(org-install-dir
	 (ignore-errors (org-find-library-dir "org-loaddefs"))))
    (cond ((and org-dir org-install-dir (string-equal org-dir org-install-dir)
		;; Still may have a situation where the Org version matches the
		;; builtin Org but the directories are for a newer Org
		;; package version.
		(if (string-match "[\\/]org-\\([0-9.]+-?[a-z]*\\)" org-dir)
		    (string-equal (match-string 1 org-dir) ;; org-dir version
				  (remove ?- (org-release)))
		  t))
	   ;; Ensure Org folding is configured for `reveal-mode' compatibility
	   (hsys-org--set-fold-style)
	   ;; Just require these libraries used for Hyperbole testing
	   ;; (when they are available) to ensure they are loaded from
	   ;; the single Org version used.
	   (mapc (lambda (lib-sym) (require lib-sym nil t))
		 '(org-version org-macs org-keys org-compat ol org-table org-id
		   org-element org-list org-element org-src org-fold org))
	   nil)
	  (t
	   ;; Ensure using any local available packaged version of Org mode
	   ;; rather than built-in which may have been activated before
	   ;; load-path was set correctly.  Avoids mixed version load of Org.
	   (let ((org-libraries-to-reload (hsys-org-get-libraries-to-reload))
		 lib-sym)
	     ;; Unload org libraries loaded with wrong path
	     (mapc (lambda (lib)
		     (setq lib-sym (intern-soft lib))
		     (when (featurep lib-sym) (unload-feature lib-sym t)))
		   org-libraries-to-reload)

	     ;; Ensure user's external Org package version is configured for loading
	     (unless (and package--initialized (not after-init-time))
	       (package-initialize))
	     ;; Ensure Org folding is configured for `reveal-mode' compatibility
	     (hsys-org--set-fold-style)
	     (let ((pkg-desc (car (cdr (assq 'org package-archive-contents)))))
	       (package-activate pkg-desc t))

	     ;; Load org libraries with right path but save "org" for last
	     (mapc #'load (remove "org" org-libraries-to-reload))
	     (load "org")
	     ;; Next setting may have been deleted with the library
	     ;; unloading, so restore it.
	     (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
	     t)))))

(defun hsys-org-get-libraries-to-reload ()
  "Return all org libraries that need to be reloaded to avoid mixed versions."
  (interactive)
  (let* ((builtin-org-dir (expand-file-name "../lisp/org/" data-directory))
	 (default-directory builtin-org-dir)
	 (builtin-org-files (nconc (file-expand-wildcards "*.el.gz")
				   (file-expand-wildcards "*.el")))
	 (feature-sym)
	 (file-to-load)
	 (builtin-org-libraries-loaded
	  (delq nil (mapcar (lambda (f)
			      (setq file-to-load
				    ;; Get rid of both .el and .el.gz suffixes
				    (file-name-sans-extension
				     (file-name-sans-extension f))
				    feature-sym (intern-soft file-to-load))
			      (and (featurep feature-sym)
				   (string-prefix-p builtin-org-dir
						    (symbol-file feature-sym))
				   file-to-load))
			    builtin-org-files))))
    builtin-org-libraries-loaded))

;;;###autoload
(defun hsys-org-log-and-fix-version ()
  "Log before/after state of Org libraries when fixing a mixed installation."
  (terpri)
  (princ (format "Org source dir = %S" (ignore-errors (org-find-library-dir "org"))))
  (terpri)
  (princ (format "Org load dir   = %S" (ignore-errors (org-find-library-dir "org-loaddefs"))))
  (terpri)
  (princ (format "Org version    = %S" (org-release)))
  (terpri)

  (let ((org-reloaded (hsys-org-fix-version)))
    (if org-reloaded
	(princ (format "Mixed Org versions fixed and reloaded\n  version is now %s\n  source dir is now %S"
		       org-version (ignore-errors (org-find-library-dir "org"))))
      (princ "The above is the active, single version of Org")))
  (terpri)
  (terpri))

;;;###autoload
(defun hsys-org-meta-return-shared-p ()
  "Return non-nil if hyperbole-mode is active and shares the org-meta-return key."
  (let ((org-meta-return-keys (where-is-internal #'org-meta-return org-mode-map)))
    (when (or (set:intersection org-meta-return-keys
				(where-is-internal #'hkey-either hyperbole-mode-map))
	      (set:intersection org-meta-return-keys
				(where-is-internal #'action-key hyperbole-mode-map)))
      t)))

;;;###autoload
(defun hsys-org-meta-return ()
  "Call `org-meta-return' with the numeric value of any prefix arg when given.
Do nothing if called outside of `org-mode'."
  (interactive "P")
  (when (apply #'derived-mode-p '(org-mode))
    (when buffer-read-only
      (error "(hsys-org-meta-return): \"%s\" must not be read-only; toggle with {%s}"
	     (buffer-name)
	     (key-description (car (where-is-internal #'read-only-mode)))))
   (if current-prefix-arg
	(org-meta-return (prefix-numeric-value current-prefix-arg))
      (org-meta-return))))

;;;###autoload
(defun hsys-org-consult-grep (&optional regexp max-matches path-list)
  "Interactively search `org-directory' with a consult package grep command.
Search for optional REGEXP up to MAX-MATCHES in PATH-LIST or `org-directory'.

Use ripgrep (rg) if found, otherwise, plain grep.  Initialize search with
optional REGEXP and interactively prompt for changes.  Limit matches
per file to the absolute value of MAX-MATCHES, if given and not 0.  If
0, match to headlines only (lines that start with a '^[*#]+[ \t]+' regexp)."
  (interactive "i\nP")
  (let* ((grep-includes "--include *.org")
	 (ripgrep-globs "--glob *.org"))
    (hsys-consult-grep grep-includes ripgrep-globs
		       regexp max-matches (or path-list (list org-directory)))))

;;;###autoload
(defun hsys-org-mode-p ()
  "Return non-nil if point is within an Org major or minor-mode buffer."
  (or (derived-mode-p 'org-mode)
      (derived-mode-p 'org-agenda-mode)
      (and (boundp 'outshine-mode) outshine-mode)
      (and (boundp 'poporg-mode) poporg-mode)))

;;;###autoload
(defun hsys-org-at-read-only-p ()
  "Return non-nil if point is in an Org read-only context."
  (and (derived-mode-p 'org-mode)
       (featurep 'hsys-org)
       (or (hsys-org-src-block-start-at-p)
	   (hsys-org-block-start-at-p)
	   (let ((contexts (org-context)))
	     (and contexts
		  (delq nil (mapcar (lambda (ctxt) (assq ctxt contexts))
				    '(:checkbox
				      :headline-stars
				      :item-bullet
				      :keyword
				      :link
				      :priority
				      :table-special
				      :tags
				      :todo-keyword))))))))

(defun hsys-org-at-tags-p ()
  "Return non-nil if point is within a fontified set of Org tags."
  (hproperty:char-property-contains-p (point) 'face 'org-tag))

(defun hsys-org-cycle ()
  "Call `org-cycle' and set as `this-command' to cycle through all states."
  (setq this-command 'org-cycle)
  (save-excursion
    (org-back-to-heading)
    (org-cycle)))

(defun hsys-org-directory-at-tags-p (&optional at-tag-flag)
  "Return non-nil if point is in an `org-directory' buffer and at Org tags."
  (and (featurep 'org)
       (or at-tag-flag (hsys-org-at-tags-p))
       (or (and (hypb:buffer-file-name)
		(string-prefix-p (expand-file-name org-directory)
				 (hypb:buffer-file-name)))
	   (string-prefix-p "*Org Agenda*" (buffer-name)))))

(defun hsys-org-get-value (attribute)
  "Within the current Org context, return the ATTRIBUTE value."
  (let ((src-block-info (org-babel-get-src-block-info)))
    (when src-block-info
      (if (memq attribute '(:lang :language))
	  ;; Return lowercase string of programming language name
	  (car src-block-info)
	(cdr (assq attribute (caddr src-block-info)))))))

(defun hsys-org-global-cycle ()
  "Call `org-global-cycle' and set as `this-command' to cycle through all states."
  (setq this-command 'org-cycle)
  (save-excursion
    (org-back-to-heading)
    (org-global-cycle nil)))

(defun hsys-org-todo-cycle ()
  "Call `org-todo' and set as `this-command' to cycle through all states."
  (setq this-command 'org-todo)
  (org-todo))

(defun hsys-org-todo-set-cycle ()
  "Call `org-todo' to switch to the next set of keywords.
Force it to be set as `this-command'."
  (setq this-command 'org-todo)
  (org-call-with-arg 'org-todo 'nextset))

(defun hsys-org-todo-occur (&optional keyword)
  "Filter to a tree of todos matching optional KEYWORD.
The tree will include all higher headlines above each match.
Match to all todos if `keyword' is nil or the empty string."
  (interactive
   (list (hargs:read-match "List todos matching keyword: " org-todo-keywords-1)))
  (unless keyword (setq keyword ""))
  (message "%d '%s' TODO entries found"
	   (org-occur (concat "^" org-outline-regexp " *" (regexp-quote keyword)))
	   keyword))

(defun hsys-org-agenda-item-at-p ()
  "Return non-nil if point is on an Org Agenda view item line, else nil."
  (and (apply #'derived-mode-p '(org-agenda-mode))
       (org-get-at-bol 'org-marker)
       t))

(defun hsys-org-block-start-at-p ()
  "Return non-nil if point is on the first line of an Org block definition."
  (save-excursion
    (forward-line 0)
    (let ((case-fold-search t))
      (or (looking-at org-block-regexp)
	  (looking-at org-dblock-start-re)))))

(defun hsys-org-src-block-start-at-p ()
  "Return non-nil if point is on the first line of an Org source block definition."
  (save-excursion
    (forward-line 0)
    (let ((case-fold-search t))
      (looking-at org-babel-src-block-regexp))))

(defun hsys-org-link-at-p ()
  "Return (start . end) iff point is on an Org mode link, else nil.
Start and end are the buffer positions of the label that point is on
delimited by square brackets.

Ignore [[hy:HyWiki]] buttons and return nil (handle these as
implicit buttons).  Assume caller has already checked that the
current buffer is in `org-mode' or is looking for an Org link in
a non-Org buffer type."
  (unless (or (smart-eolp) (smart-eobp))
    (if (derived-mode-p 'org-mode)
	(let* ((org-plist (hsys-org-thing-at-p))
	       (type (plist-get org-plist :type))
	       label-start-end)
	  (when (eq type 'link)
	    (save-match-data
	      ;; If this Org link matches a potential HyWiki word, ignore it.
	      (when (not (and (fboundp 'hywiki-word-at) (hywiki-word-at)))
		(if (setq label-start-end (ibut:label-p t "[" "]" t))
		    (cons (nth 1 label-start-end) (nth 2 label-start-end))
		  t)))))
      ;; non-Org mode (can't call org-element (which
      ;; hsys-org-thing-at-p calls) outside of Org mode
      (when (bound-and-true-p org-link-bracket-re)
        (let ((pos (point)))
          (when (save-excursion
		  (or
		   ;; Check if point is inside a link
		   (and (re-search-backward org-link-bracket-re
					    (line-beginning-position) t)
			(> pos (point))
			(< pos (match-end 0)))
		   ;; If not found before, check if we're in the middle of a link
		   (and (forward-line 0)
			(re-search-forward org-link-bracket-re
					   (line-end-position 2) t)
			(> (point) pos)
			(< pos (match-end 0)))))
	    (save-match-data
	      ;; If this Org link matches a potential HyWiki word, ignore it.
	      (when (not (and (fboundp 'hywiki-word-at) (hywiki-word-at)))
                (let ((label-start-end (ibut:label-p t "[" "]" t)))
		  (if label-start-end
		      (cons (nth 1 label-start-end) (nth 2 label-start-end))
		    t))))))))))

;; Assume caller has already checked that the current buffer is in org-mode.
(defun hsys-org-heading-at-p (&optional _)
  "Non-nil when on a headline."
  (unless (or (smart-eolp) (smart-eobp))
    (outline-on-heading-p t)))

;; Assume caller has already checked that the current buffer is in org-mode.
(defun hsys-org-target-at-p ()
  "Return non-nil iff point is on an Org target or target link.
The target is the definition and the target link is the referent.
Assume caller has already checked that the current buffer is in
`org-mode'."
  (hsys-org-face-at-p 'org-target))

;; Derived from `org-open-at-point' in "org.el".
(defun hsys-org-thing-at-p ()
  "Return a plist of properties for Org thing at point or nil if none.
This must work on Org links outside of Org mode.

The plist form is: (:type <type> :value <value> :context <context>).
The thing can be a link, citation, timestamp, footnote, src-block or
tags.

On top of syntactically correct links, this function also works
on links and timestamps in comments, node properties, and
keywords if point is on something looking like a timestamp or a
link."
  (ignore-errors
    (org-load-modules-maybe)
    ;; Org regex matching can fail in non-thing contexts, so we ignore
    ;; these errors and return nil then.  Org-element will also warn to
    ;; not use it outside of Org mode although it works, so we suppress
    ;; those warnings as well.
    (let* ((warning-minimum-level :error)
	   (warning-suppress-types '(org-element rx))
	   (warning-suppress-log-types '(org-element rx))
	   (context
	    ;; Only consider supported types, even if they are not the
	    ;; closest one.
	    (org-element-lineage
	     (org-element-context)
	     '(citation citation-reference clock comment comment-block
			footnote-definition footnote-reference headline
			inline-src-block inlinetask keyword link node-property
			planning src-block timestamp)
	     t))
	   (type (org-element-type context))
	   (value (org-element-property :value context)))
      (when type
	(list :type type :value value :context context)))))

(defun hsys-org-todo-at-p ()
  "Return non-nil iff point is on an Org mode todo keyword.
Assume caller has already checked that the current buffer is in `org-mode'."
  (and (apply #'derived-mode-p '(org-mode))
       (assq :todo-keyword (org-context))
       t))

(defun hsys-org-radio-target-link-at-p ()
  "Return link text region iff point is on an Org mode radio target link.
Link region is (start . end) and includes delimiters, else nil."
  (and (hsys-org-face-at-p 'org-link)
       (equal (get-text-property (point) 'help-echo) "Radio target link")
       (hproperty:char-property-range (point) 'face 'org-link)))

(defun hsys-org-radio-target-def-at-p ()
  "Return target region iff point is on a <<<radio target>>> definition.
Target region is (start . end) and includes any delimiters, else nil."
  (when (hsys-org-target-at-p)
    (save-excursion
      (unless (looking-at org-radio-target-regexp)
	(goto-char (or (hproperty:char-property-start (point) 'face 'org-target)
                       (point-min))))
      (when (looking-at "<<<")
	(goto-char (match-end 0)))
      (and (hsys-org-face-at-p 'org-target)
	   (hproperty:char-property-range (point) 'face 'org-target)))))

(defun hsys-org-radio-target-at-p ()
  "Return region iff point is on a <<<radio target>>> or a link to one.
The region is (start . end) and includes any delimiters, else nil."
  (and (or (hsys-org-radio-target-def-at-p)
	   (hsys-org-radio-target-link-at-p))
       (hproperty:char-property-range (point) 'face 'org-target)))

(defun hsys-org-internal-target-link-at-p ()
  "Return link text region iff point is on an Org mode internal target link.
Link region is (start . end) and includes delimiters, else nil."
  (and (hsys-org-face-at-p 'org-link)
       (not (equal (get-text-property (point) 'help-echo) "Radio target link"))
       (hsys-org-link-at-p)
       (hproperty:char-property-range (point) 'face 'org-link)))

(defun hsys-org-internal-target-def-at-p ()
  "Return target region iff point is on <<internal target>> definition.
Target region is (start . end) and includes any delimiters, else nil."
  (when (hsys-org-target-at-p)
    (save-excursion
      (unless (looking-at org-target-regexp)
	(goto-char (or (hproperty:char-property-start (point) 'face 'org-target)
		       (point-min))))
      (when (looking-at "<<")
	(goto-char (match-end 0)))
      (and (hsys-org-face-at-p 'org-target)
	   (hproperty:char-property-range (point) 'face 'org-target)))))

(defun hsys-org-internal-target-at-p ()
  "Return target region iff point is on an <<internal target>> or a link to one.
The region is (start . end) and includes any delimiters, else nil."
  (and (or (hsys-org-internal-target-def-at-p)
	   (hsys-org-internal-target-link-at-p))
       (hproperty:char-property-range (point) 'face 'org-target)))

(defun hsys-org-face-at-p (org-face-type)
  "Return ORG-FACE-TYPE iff point is on a character with that face, else nil.
ORG-FACE-TYPE must be a symbol, not a symbol name."
  (hproperty:char-property-contains-p (point) 'face org-face-type))

;; Adapted from Org code
(defun hsys-org-search-internal-link-p (target)
  "Search buffer start for the first Org internal link matching <<TARGET>>.
White spaces are insignificant.  Return t if a link is found, else nil."
  (when (string-match "<<.+>>" target)
    (setq target (substring target 2 -2)))
  (let ((re (format "%s" (mapconcat #'regexp-quote
			            (split-string target)
			            "[ \t]+\\(?:\n[ \t]*\\)?")))
	(origin (point)))
    (goto-char (point-min))
    (catch :link-match
      (while (re-search-forward re nil t)
	(backward-char)
	(let ((object (org-element-context)))
	  (when (eq (org-element-type object) 'link)
	    (when (featurep 'org-fold) ;; newer Org versions
              (org-fold-show-context 'link-search))
	    (goto-char (or (previous-single-property-change (point) 'face) (point-min)))
	    (throw :link-match t))))
      (goto-char origin)
      nil)))

;; Adapted from Org code
(defun hsys-org-search-radio-target-link-p (target)
  "Search from point for a radio target link matching TARGET.
White spaces are insignificant.  Return t if a target link is found, else nil."
  (when (string-match "<<<.+>>>" target)
    (setq target (substring target 3 -3)))
  (let ((re (format "%s" (mapconcat #'regexp-quote
			            (split-string target)
			            "[ \t]+\\(?:\n[ \t]*\\)?")))
	(origin (point)))
    (catch :radio-match
      (while (re-search-forward re nil t)
	(backward-char)
	(let ((object (org-element-context)))
	  (when (eq (org-element-type object) 'link)
	    (when (featurep 'org-fold) ;; newer Org versions
              (org-fold-show-context 'link-search))
	    (throw :radio-match t))))
      (goto-char origin)
      nil)))

(defun hsys-org-set-ibut-label (start-end)
  "Record the label and START-END positions of any implicit button at point."
  (when (consp start-end)
    (ibut:label-set (ibut:key-to-label
		     (ibut:label-to-key
		      (buffer-substring-no-properties
		       (car start-end) (cdr start-end))))
		    (car start-end) (cdr start-end))))

(defun hsys-org-to-next-radio-target-link (target)
  "Move to the start of the next radio TARGET link if found.
TARGET must be a string."
  (when (string-match "<<<.+>>>" target)
    (setq target (substring target 3 -3)))
  (let ((opoint (point))
	(start-end (hsys-org-radio-target-at-p))
	found)
    (when start-end
      ;; Move past any current target link
      (goto-char (cdr start-end)))
    (while (and (hsys-org-search-radio-target-link-p target)
		(setq found t)
		(not (hsys-org-radio-target-link-at-p))))
    (when found
      (if (hsys-org-radio-target-link-at-p)
	  (goto-char (or (previous-single-property-change (point) 'face) (point-min)))
	(goto-char opoint)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hsys-org--agenda-tags-string ()
  "When on or between Org tags, return an agenda match string for them.
If on a colon, return a match string for all tags around point,
e.g. \":tag1:tag2:tag3:\", that will match to these tags in any order.
Otherwise, just match to the single tag around point, e.g. tag2."
  (let ((range (hproperty:char-property-range nil 'face 'org-tag))
	tags)
    (when range
      (if (equal (char-after) ?:)
	  ;;  On colon, search for HyWiki headings with all tags on line
	  (setq tags (when range (buffer-substring-no-properties (car range) (cdr range))))
	;;   Else on a specific tag, search for HyWiki headings with that tag only
	(setq range (hargs:delimited ":" ":" nil nil t)
	      tags (nth 0 range)
	      range (cons (nth 1 range) (nth 2 range))))
      (when (and tags range)
	(ibut:label-set tags (car range) (cdr range))
	tags))))

(defun hsys-org--set-fold-style ()
  "Set `org-fold-core-style' to \\='overlays for `reveal-mode' compatibility.
This must be called before Org mode is loaded."
  (when (and (ignore-errors (find-library-name "org-fold-core"))
	     (not (boundp 'org-fold-core-style)))
    (load "org-fold-core"))
  (custom-set-variables '(org-fold-core-style 'overlays)))

(provide 'hsys-org)

;;; hsys-org.el ends here
