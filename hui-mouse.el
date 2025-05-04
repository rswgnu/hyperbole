;;; hui-mouse.el --- Use key or mouse key for many functions, e.g. GNU Hyperbole menus  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    04-Feb-89
;; Last-Mod:      4-May-25 at 10:38:03 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1991-2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  This code is machine independent.  It works best with a pointing device
;;  but may also be used from a keyboard.  When used with a pointing device it
;;  requires an Emacs command that sets point to the location of the pointing
;;  device's cursor.
;;
;;  If you want to use your shift-middle mouse button to select Hyperbole menu
;;  items and Hyperbole buttons, follow these instructions.
;;
;;  If you plan to use a mouse only with the X window system or macOS and you
;;  want to use the shift-middle and shift-right buttons, you need not do any
;;  mouse configuration.  Your Emacs executable must have been built so as to
;;  include the mouse support files for your window system, however.  These
;;  are in the Emacs "src" directory: for X - "x*.c", for macOS - "ns*.c".
;;
;;  To use a different mouse key or a different window system, modify the
;;  mouse key bindings in "hmouse-sh.el".
;;
;;  Using the Action Mouse Key to browse through and delete files from Dired
;;  listings is exceptionally nice, just as it is when reading mail.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'ert-results nil t) ;; Optional Action Key support in ERT result buffers
;; Library in next line (for code optionally used in `smart-eol') uses
;; `repeat-map' which was not added to the "repeat.el" library in Emacs 27.1,
;; so don't use if it fails to load properly.
(ignore-errors (require 'hsys-flymake))
(require 'hload-path)
(require 'hsys-xref)
(require 'hsys-org)
(require 'hbut)
(unless (fboundp 'smart-info)
  (require 'hmouse-info))
(unless (fboundp 'smart-c-at-tag-p)
  (require 'hmouse-tag))
(require 'imenu)
(eval-when-compile
  (require 'eieio)) ;; magit uses this and thus smart-magit does

(eval-when-compile (require 'tar-mode))

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

;; Functions from optional, external ert-results package
(declare-function ert-results-filter                 "ext:ert-results")
(declare-function ert-results-filter-status-p        "ext:ert-results")
(declare-function ert-results-display                "ext:ert-results")
(declare-function ert-results-hide                   "ext:ert-results")
(declare-function ert-results-show                   "ext:ert-results")
(declare-function ert-results-toggle                 "ext:ert-results")
(declare-function ert-results-describe-test-at-point "ext:ert-results")

;; Hyperbole functions and keymap for `flymake-mode'
(declare-function hsys-flymake-get-issue-at-position "hsys-flymake")

;; Functions from Hyperbole's abstract mail and news interface.
;; See "hmail.el"
(declare-function lmail:delete "hmail")
(declare-function lmail:undelete "hmail")
(declare-function rmail:msg-prev "hmail")
(declare-function lmail:goto "hmail")
(declare-function lmail:expunge "hmail")
(declare-function rmail:msg-next "hmail")
(declare-function lmail:undelete-all "hmail")

;; Functions and variables from the helm package
(declare-function helm-get-actions-from-current-source "ext:helm-core")
(declare-function helm-get-default-action "ext:helm-core")

(defvar helm-selection-point)

;; Functions from Hyperbole's HyRolo and Koutliner
(declare-function hyrolo-edit-entry "hyrolo")
(declare-function hyrolo-hdr-in-p "hyrolo")
(declare-function hyrolo-hdr-to-first-line-p "hyrolo")
(declare-function kotl-mode:eobp "kotl-mode")
(declare-function kotl-mode:eolp "kotl-mode")

;; Emacs functions
(declare-function tar-flag-deleted "tar")
(declare-function tar-unflag "tar")
(declare-function tar-extract-other-window "tar")
(declare-function tar-expunge "tar")
(declare-function outline-invisible-in-p "hyperbole")
(declare-function Custom-newline "cus-edit")
(declare-function Custom-buffer-done "cus-edit")

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hmouse-set-point-command nil
  "*Command that sets point to the mouse cursor position.")

(defun action-key-error ()
  "If in Org mode and Hyperbole shares {M-RET}, run `org-meta-return'.
In other context signal an error."
  (if (and (funcall hsys-org-mode-function)
	   (hsys-org-meta-return-shared-p))
      (hact 'hsys-org-meta-return)
    (hypb:error "(Hyperbole Action Key): No action defined for this context; try another location")))

(defun assist-key-error ()
  "If in Org mode and Hyperbole shares {M-RET}, run `org-meta-return'.
In other context, signal an error."
  (if (and (funcall hsys-org-mode-function)
	   (hsys-org-meta-return-shared-p))
      (hact 'hsys-org-meta-return)
    (hypb:error "(Hyperbole Assist Key): No action defined for this context; try another location")))

(defcustom action-key-default-function #'action-key-error
  "*Function run by the Action Key in an unspecified context.
Set it to `hyperbole' if you want it to display the Hyperbole minibuffer menu."
  :type 'function
  :group 'hyperbole-keys)

(defcustom assist-key-default-function #'assist-key-error
  "*Function run by the Assist Key in an unspecified context.
Set it to `hkey-summarize' if you want it to display a summary of
Smart Key behavior."
  :type 'function
  :group 'hyperbole-keys)

(defcustom action-key-modeline-buffer-id-function #'dired-jump
  "*Function to call for Action Key clicks on the buf id portion of a modeline.
Its default value is `dired-jump'; set it to `smart-treemacs-modeline'
to use the Treemacs file manager package instead."
  :type 'function
  :group 'hyperbole-keys)

(defcustom action-key-eol-function #'smart-scroll-up
  "*Function run by the Action Key at the end of a line.
Its default value is `smart-scroll-up'.  To disable it, set it to
#\\='ignore."
  :type 'function
  :group 'hyperbole-keys)

(defcustom assist-key-eol-function #'smart-scroll-down
  "*Function run by the Assist Key at the end of a line.
Its default value is `smart-scroll-down'.  To disable it, set it to
#\\='ignore."
  :type 'function
  :group 'hyperbole-keys)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function todotxt-archive "ext:todotxt")
(declare-function todotxt-bury "ext:todotxt")
(declare-function todotxt-complete-toggle "ext:todotxt")
(declare-function todotxt-edit-item "ext:todotxt")

(declare-function magit-current-section "magit-selection")
(declare-function magit-diff-visit-file "etx:magit-diff")
(declare-function magit-diff-visit-file--internal "etx:magit-diff")
(declare-function magit-file-at-point "etx:magit-git")
(declare-function magit-section-cycle-diffs "etx:magit-diff")
(declare-function magit-section-cycle-global "etx:magit-selection")
(declare-function magit-section-hide "etx:magit-selection")
(declare-function magit-section-show "etx:magit-selection")
(defvar magit-root-section)
(defvar magit-display-buffer-function)

(declare-function -flatten "ext:dash")

(declare-function imenu--make-index-alist "imenu")

(declare-function image-dired-thumbnail-display-external "image-dired")
(declare-function image-dired-display-thumbnail-original-image "image-dired")
(declare-function image-dired-mouse-select-thumbnail "image-dired")

(declare-function helm-action-window "ext:helm-lib")
(declare-function helm-buffer-get "ext:helm-lib")
(declare-function helm-get-selection "ext:helm")
(declare-function helm-mark-current-line "ext:helm")
(declare-function helm-next-line "ext:helm")
(declare-function helm-pos-candidate-separator-p "ext:helm")
(declare-function helm-pos-header-line-p "ext:helm")
(declare-function helm-resume "ext:helm")
(declare-function helm-window "ext:helm-lib")
(declare-function with-helm-buffer "ext:helm-lib")
(defvar helm-action-buffer)
(defvar helm-alive-p)
(defvar helm-buffer)
(defvar helm-saved-action)

(declare-function ibuffer-mark-for-delete "ibuffer")
(declare-function ibuffer-unmark-forward "ibuffer")
(declare-function ibuffer-unmark-all "ibuffer")
(declare-function ibuffer-do-view "ibuffer")
(declare-function ibuffer-mark-forward "ibuffer")
(declare-function ibuffer-do-kill-on-deletion-marks "ibuffer")
(declare-function ibuffer-get-marked-buffers "ibuffer")
(declare-function ibuffer-current-buffer "ibuffer")

(declare-function br-buffer-menu-select "ext:br")

(declare-function gnus-topic-read-group "gnus-topic")

(declare-function company-show-doc-buffer "ext:company")
(declare-function company-quickhelp-manual-begin "ext:company-quickhelp")
(declare-function company-show-location "ext:company")
(declare-function company-select-mouse "ext:company")

(declare-function unix-apropos-get-man "ext:man-apropos")

;;; ************************************************************************
;;; Hyperbole context-sensitive keys dispatch table
;;; ************************************************************************

(defvar hkey-value nil
  "Communicates a value between a Smart Key predicate and its actions.")

(defvar hkey-alist
  '(
    ;; Company completion mode
    ((and (boundp 'company-active-map)
	  (memq company-active-map (current-minor-mode-maps)))
     . ((smart-company-to-definition) . (smart-company-help)))
    ;;
    ;; Treemacs hierarchical file manager
    ((eq major-mode 'treemacs-mode)
     . ((smart-treemacs) . (smart-treemacs)))
    ;;
    ;; dired-sidebar-mode
    ((eq major-mode 'dired-sidebar-mode)
     . ((smart-dired-sidebar) . (smart-dired-sidebar)))
    ;;
    ;; Handle Emacs push buttons in buffers
    ((and (fboundp 'button-at) (button-at (point)))
     . ((smart-push-button nil (mouse-event-p last-command-event))
	. (smart-push-button-help nil (mouse-event-p last-command-event))))
    ;;
    ;; If in a window reading an argument with vertico, run the
    ;; vertico command on {M-RET} which by default accepts the
    ;; existing input at the prompt rather than the candidate pointed
    ;; to.
    ((and (bound-and-true-p vertico-mode)
	  ;; Is vertico prompting for an argument?
	  (vertico--command-p nil (current-buffer)))
     . ((funcall (lookup-key vertico-map (kbd "M-RET")))
	. (funcall (lookup-key vertico-map (kbd "M-RET")))))
    ;;
    ;; If in the minibuffer and reading a non-menu Hyperbole argument
    ;; (aside from with vertico or ivy), accept the argument or give
    ;; completion help.
    ((and hargs:reading-type
	  (> (minibuffer-depth) 0)
	  (eq (selected-window) (minibuffer-window))
	  (not (bound-and-true-p ivy-mode))
	  (not (and (bound-and-true-p vertico-mode)
		    ;; Is vertico prompting for an argument?
		    (vertico--command-p nil (current-buffer))))
	  (not (eq hargs:reading-type 'hmenu))
	  (not (smart-helm-alive-p)))
     . ((funcall (key-binding (kbd "RET"))) . (smart-completion-help)))
    ;;
    ;; If reading a Hyperbole menu item or a Hyperbole completion-based
    ;; argument, allow selection of an item at point.
    ((and (> (minibuffer-depth) 0) (setq hkey-value (hargs:at-p)))
     . ((hargs:select-p hkey-value) . (hargs:select-p hkey-value 'assist)))
    ;;
    ;; If reading a Hyperbole menu item and nothing is selected, just return.
    ;; Or if in a helm session with point in the minibuffer, quit the
    ;; session and activate the selected item.
    ((and (> (minibuffer-depth) 0)
	  (eq (selected-window) (minibuffer-window))
	  (or (eq hargs:reading-type 'hmenu)
	      (smart-helm-alive-p)))
     . ((funcall (key-binding (kbd "RET"))) . (funcall (key-binding (kbd "RET")))))
    ;;
    ;; If at the end of a line (eol), invoke the associated Smart Key
    ;; handler EOL handler.
    ((and (smart-eolp)
          (not (and (funcall hsys-org-mode-function)
                    (not (equal hsys-org-enable-smart-keys t)))))
     . ((hact action-key-eol-function) . (hact assist-key-eol-function)))
    ;;
    ;; Handle any Org mode-specific contexts but give priority to Hyperbole
    ;; buttons prior to cycling Org headlines
    ((and (not (hyperb:stack-frame '(smart-org)))
	  (let ((hrule:action #'actype:identity))
	    (smart-org)))
     . ((smart-org) . (smart-org)))
    ;;
    ;; The ID-edit package supports rapid killing, copying, yanking and
    ;; display management. It is available only as a part of InfoDock.
    ;; It is not included with Hyperbole.
    ((and (boundp 'id-edit-mode) id-edit-mode
	  (not buffer-read-only)
	  (not (smart-helm-alive-p)))
     . ((id-edit-yank) . (id-edit-yank)))
    ;;
    ;; If in an xref buffer on a listing of matching identifier lines, go to
    ;; the source line referenced by the current entry.
    ((hsys-xref-item-at-point)
     . ((xref-goto-xref) . (xref-show-location-at-point)))
    ;;
    ;; The Smart Menu system is an attractive in-buffer menu system
    ;; that works on any display system that supports Emacs.  It
    ;; predates Emacs' menu systems; it is a part of InfoDock.
    ;; It is not included with Hyperbole but is compatible with the
    ;; Smart Keys.
    ;;
    ;; This selects or gives help for a menu item.
    ((eq major-mode 'smart-menu-mode)
     . ((smart-menu-select) . (smart-menu-help)))
    ;;
    ((derived-mode-p 'dired-mode)
     . ((smart-dired) . (smart-dired-assist)))
    ;;
    ((string-prefix-p "magit-" (symbol-name major-mode))
     . ((smart-magit) . (smart-magit-assist)))
    ;;
    ;; If on a Hyperbole button, perform action or give help.
    ((hbut:at-p)
     . ((hui:hbut-act 'hbut:current) . (hui:hbut-help 'hbut:current)))
    ;;
    ;; This potentially displays a Smart Menu.
    ((and (fboundp 'smart-menu-choose-menu)
	  (setq hkey-value (and hkey-always-display-menu
				(smart-menu-choose-menu)))
	  (not (and (get-buffer-window *smart-menu-buffer*)
		    (eq hkey-value *smart-menu-curr*))))
     . ((smart-menu hkey-value)
	. (smart-menu hkey-value)))
    ;;
    ;; View minor mode
    ((bound-and-true-p view-mode)
     . ((cond ((last-line-p)
	       (view-quit))
	      ((pos-visible-in-window-p (point-max))
	       (goto-char (point-max)))
	      (t (View-scroll-page-forward)))
	. (View-scroll-page-backward)))
    ;;
    ;; Direct access selection of helm-major-mode completions
    ((setq hkey-value (and (or (eq major-mode 'helm-major-mode)
			       (and (featurep 'helm) (equal helm-action-buffer (buffer-name))))
			   (or (eolp)
			       (smart-helm-at-header)
			       (smart-helm-line-has-action))))
     . ((smart-helm) . (smart-helm-assist)))
    ;;
    ((and (eq major-mode 'ert-results-mode)
	  (featurep 'ert-results)
	  (setq hkey-value (ert-results-filter-status-p)))
     . ((smart-ert-results hkey-value) . (smart-ert-results-assist hkey-value)))
    ;;
    ;; Support the OO-Browser when available.  It is a separate Emacs
    ;; package not included with Hyperbole.  Within an OO-Browser
    ;; OOBR-FTR buffer, an *Implementors* listing buffer, or an
    ;; Element signatures listing buffer of the OO-Browser, display
    ;; the associated element.
    ((or (string-equal (buffer-name) "*Implementors*")
	 (string-match "-Elements\\'" (buffer-name))
	 (and (boundp 'br-feature-tags-file)
	      (stringp br-feature-tags-file)
	      (equal br-feature-tags-file (hypb:buffer-file-name))))
     . ((smart-element) . (hkey-help)))
    ;;
    ;; Select or select-and-kill a markup pair (e.g. hmtl tags), list,
    ;; array/vector, set, function, comment or string that begins or
    ;; ends at point.  For markup pairs, point must be at the first
    ;; character of the opening or closing tag.
    ((hui-select-at-delimited-thing-p)
     . ((hui-select-thing) . (progn (hui-select-thing)
				    (hmouse-kill-region))))
    ;;
    ;; If the prior test failed and point is at the start or end of an
    ;; sexpression, mark it for editing or kill it (assist key).  This
    ;; only handles the special case where point is just after the
    ;; closing delimiter and not at an end-of-line, so this may be
    ;; removed someday.
    ((hui-select-at-delimited-sexp-p)
     . ((hui-select-mark-delimited-sexp)
	. (progn (hui-select-mark-delimited-sexp) (hmouse-kill-region))))
    ;;
    ((eq major-mode 'occur-mode)
     . ((occur-mode-goto-occurrence) . (occur-mode-goto-occurrence)))
    ;;
    ((eq major-mode 'moccur-mode)
     . ((moccur-mode-goto-occurrence) . (moccur-mode-goto-occurrence)))
    ((eq major-mode 'amoccur-mode)
     . ((amoccur-mode-goto-occurrence) . (amoccur-mode-goto-occurrence)))
    ;;
    ((eq major-mode 'kotl-mode)
     . ((kotl-mode:action-key) . (kotl-mode:assist-key)))
    ;;
    ;; If in the flymake linter list of issues buffer, jump to or show
    ;; issue at point.
    ((eq major-mode 'flymake-diagnostics-buffer-mode)
     . ((flymake-goto-diagnostic (point)) . (flymake-show-diagnostic (point) t)))
    ;;
    ;; Rdb-mode supports direct selection and viewing of in-memory relational
    ;; databases.  Rdb-mode is available as a part of InfoDock.
    ;; It is not included with Hyperbole.
    ((eq major-mode 'rdb-mode)
     . ((rdb:action-key) . (rdb:assist-key)))
    ;;
    ;; Restore window config and hide help buffer when click at buffer end.
    ((if (= (point) (point-max))
	 (string-match "^\\*Help\\|Help\\*$" (buffer-name)))
     . ((hkey-help-hide) . (hkey-help-hide)))
    ;;
    ;; Handle widgets in Custom-mode
    ((eq major-mode 'Custom-mode)
     . ((smart-custom) . (smart-custom-assist)))
    ;;
    ;; Emacs bookmarks menu (bookmark.el)
    ((eq major-mode 'bookmark-bmenu-mode)
     . ((bookmark-jump (bookmark-bmenu-bookmark) (hpath:display-buffer-function))
	.
	;; Below we want the Assist Key to show what the Action Key does.
	(hkey-help)))
    ;;
    ;; Pages directory listing mode (page-ext.el)
    ((eq major-mode 'pages-directory-mode)
     . ((pages-directory-goto) . (pages-directory-goto)))
    ;;
    ;; Python files - ensure this comes before Imenu for more advanced
    ;; definition lookups
    ((and (or (and (derived-mode-p '(python-mode python-ts-mode)) (hypb:buffer-file-name))
	      (and (featurep 'hsys-org) (hsys-org-mode-p)
		   (equal (hsys-org-get-value :language) "python"))
	      (let ((case-fold-search))
		(string-match "\\`\\([ *]?Pydoc[: ]\\|\\*?Python\\)" (buffer-name))))
	  (setq hkey-value (smart-python-at-tag-p)))
     . ((smart-python hkey-value) . (smart-python hkey-value 'next-tag)))
    ;;
    ((and (memq major-mode '(c-mode c-ts-mode))
	  (hypb:buffer-file-name) (smart-c-at-tag-p))
     . ((smart-c) . (smart-c nil 'next-tag)))
    ;;
    ((and (memq major-mode '(c++-mode c++-ts-mode)) (hypb:buffer-file-name)
	  ;; Don't use smart-c++-at-tag-p here since it will prevent #include
	  ;; lines from matching.
	  (smart-c-at-tag-p))
     . ((smart-c++) . (smart-c++ nil 'next-tag)))
    ;;
    ((and (eq major-mode 'asm-mode)
	  (hypb:buffer-file-name) (smart-asm-at-tag-p))
     . ((smart-asm) . (smart-asm nil 'next-tag)))
    ;;
    ((setq hkey-value nil
	   hkey-value
	   (or (when (smart-lisp-mode-p)
		 (or (setq hkey-value (smart-lisp-at-load-expression-p))
		     (smart-lisp-at-tag-p)))
	       ;; Tightly limit Lisp matches in change-log-mode but
	       ;; only call this if hkey-value is true since
	       ;; otherwise, already know there is no tag at point.
	       (when hkey-value
	         (smart-lisp-at-change-log-tag-p))))
     . ((smart-lisp) . (smart-lisp 'show-doc)))
    ;;
    ;;
    ((and (memq major-mode '(java-mode java-ts-mode)) (hypb:buffer-file-name)
	  (or (smart-java-at-tag-p)
	      ;; Also handle Java @see cross-references.
	      (looking-at "@see[ \t]+")
	      (save-excursion
		(and (re-search-backward "[@\n\r\f]" nil t)
		     (looking-at "@see[ \t]+")))))
     . ((smart-java) . (smart-java nil 'next-tag)))
    ;;
    ((and (memq major-mode '(html-mode javascript-mode js-mode js-ts-mode js2-mode js3-mode web-mode))
	  (hypb:buffer-file-name)
	  (smart-javascript-at-tag-p))
     . ((smart-javascript) . (smart-javascript nil 'next-tag)))
    ;;
    ((and (eq major-mode 'objc-mode) (hypb:buffer-file-name)
	  (smart-objc-at-tag-p))
     . ((smart-objc) . (smart-objc nil 'next-tag)))
    ;;
    ;; Imenu listing in GNU Emacs
    ((smart-imenu-item-at-p)
     . ((smart-imenu-display-item-where (car hkey-value) (cdr hkey-value)) .
	(imenu-choose-buffer-index)))
    ;;
    ((and (memq major-mode '(fortran-mode f90-mode))
	  (hypb:buffer-file-name) (smart-fortran-at-tag-p))
     . ((smart-fortran) . (smart-fortran nil 'next-tag)))
    ;;
    ((eq major-mode 'calendar-mode)
     . ((smart-calendar) . (smart-calendar-assist)))
    ;;
    ;; Part of InfoDock
    ((eq major-mode 'unix-apropos-mode)
     . ((smart-apropos) . (smart-apropos-assist)))
    ;;
    ((eq major-mode 'outline-mode)
     . ((smart-outline) . (smart-outline-assist)))
    ;;
    ((eq major-mode 'Info-mode)
     . ((smart-info) .  (smart-info-assist)))
    ;;
    ((if (boundp 'hmail:reader)
	 (or (eq major-mode hmail:reader)
	     (eq major-mode hmail:lister)))
     . ((smart-hmail) . (smart-hmail-assist)))
    ;;
    ((eq major-mode 'gnus-group-mode)
     (smart-gnus-group) . (smart-gnus-group-assist))
    ;;
    ((eq major-mode 'gnus-summary-mode)
     . ((smart-gnus-summary) . (smart-gnus-summary-assist)))
    ;;
    ((eq major-mode 'gnus-article-mode)
     . ((smart-gnus-article) . (smart-gnus-article-assist)))
    ;;
    ((eq major-mode 'Buffer-menu-mode)
     . ((smart-buffer-menu) . (smart-buffer-menu-assist)))
    ;;
    ((eq major-mode 'ibuffer-mode)
     . ((smart-ibuffer-menu) . (smart-ibuffer-menu-assist)))
    ;;
    ((eq major-mode 'tar-mode)
     . ((smart-tar) . (smart-tar-assist)))
    ;;
    ;; Follow references in man pages.
    ((setq hkey-value (smart-man-entry-ref))
     . ((smart-man-display hkey-value) . (smart-man-display hkey-value)))
    ;;
    ((eq major-mode 'w3-mode)
     . ((w3-follow-link) . (w3-goto-last-buffer)))
    ;;
    ;; Future Hyperbole mode, not yet released
    ;; ((eq major-mode 'hynote-mode)
    ;; . ((smart-hynote) . (smart-hynote-assist)))
    ;;
    ((eq major-mode 'hyrolo-mode)
     . ((smart-hyrolo) . (smart-hyrolo-assist)))
    ;;
    ((eq major-mode 'image-dired-thumbnail-mode)
     . ((smart-image-dired-thumbnail) . (smart-image-dired-thumbnail-assist)))
    ;;
    ;; Gomoku game
    ((eq major-mode 'gomoku-mode)
     . ((gomoku-human-plays) . (gomoku-human-takes-back)))
    ;;
    ;; Support the OO-Browser when available.  It is a separate Emacs
    ;; package not included with Hyperbole.  Hyperbole supplies a stub
    ;; `br-in-browser' test for use here.
    ((or (br-in-browser) (eq major-mode 'br-mode))
     . ((smart-br-dispatch) . (smart-br-assist-dispatch)))
    ;;
    ;; Outline minor mode is on and usable.
    ((and (boundp 'outline-minor-mode) outline-minor-mode)
     . ((smart-outline) . (smart-outline-assist)))
    ;;
    ;; Todotxt
    ((eq major-mode 'todotxt-mode)
     . ((smart-todotxt) . (smart-todotxt-assist)))
    ;;
    ;; Any other programming modes not specially supported
    ;; Use xref which supports various Language Servers
    ((setq hkey-value (smart-prog-at-tag-p))
     . ((smart-prog-tag hkey-value) . (smart-prog-tag hkey-value))))
  "Alist of predicates and form-conses for the Action and Assist Keyboard Keys.
Each element is: (PREDICATE-FORM . (ACTION-KEY-FORM . ASSIST-KEY-FORM)).
When the Action or Assist Key is pressed, the first or second form,
respectively, associated with the first non-nil predicate is evaluated.

See also `hmouse-alist' for a superset of this list utilized by the
Action and Assist Mouse Keys.")

;;; ************************************************************************
;;; driver code
;;; ************************************************************************

(require 'hargs)
(defvar hmouse-alist hkey-alist
  "Alist of predicates and form-conses for the Action and Assist Mouse Keys.
When the Action Mouse Key or Assist Mouse Key is pressed, the first or second
form, respectively, associated with the first non-nil predicate is
evaluated.

The function `hmouse-alist-add-window-handlers' adds the mouse context
handlers to this variable.

The `hkey-alist' variable is the subset of this alist used by the
smart keyboard keys.")

;; This must be required after hmouse-alist is defined in this file
;; since this will recursively require hmouse-drv which requires
;; hui-window when being compiled and that library requires that
;; hmouse-alist be defined.
(require 'hmouse-key)

;; This next library adds drag actions to `hmouse-alist'.
(require 'hui-window)

;;; ************************************************************************
;;; support code
;;; ************************************************************************

;; The `load' line below loads any local Smart Key function definitions.
;; The public distribution contains none.  You may leave it commented out if
;; you prefer.
;; (load "smart-local" t)

;;; ************************************************************************
;;; Required Init functions
;;; ************************************************************************

(defun first-line-p ()
  "Return t if point is on the first line of the buffer."
  (save-excursion (beginning-of-line) (bobp)))

(defun last-line-p ()
  "Return t if point is on the last line of the buffer."
  (save-excursion (end-of-line) (smart-eobp)))

(defun smart-completion-help ()
  "Offer completion help for current minibuffer argument, if any."
  (when (where-is-internal 'minibuffer-completion-help (current-local-map))
    (minibuffer-completion-help)))

;;; ************************************************************************
;;; smart-buffer-menu functions
;;; ************************************************************************

(defun smart-buffer-menu-no-marks ()
  "Display this line's buffer in this window.
Bury the buffer menu unless other buffers are marked.  If buffer
menu items are marked, return nil, else t."
  (let* ((this-buffer (Buffer-menu-buffer t))
	 (menu-buffer (current-buffer))
	 (others (delq this-buffer (Buffer-menu-marked-buffers t))))
    (unless others
      (switch-to-buffer this-buffer)
      (unless (eq menu-buffer this-buffer)
	(bury-buffer menu-buffer))
      t)))

(defun smart-buffer-menu ()
  "Use a single key or mouse key to manipulate `buffer-menu' entries.

Invoked via a key press when in `Buffer-menu-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) on the first column of an entry, the selected buffer is marked for
     display; 
 (2) on the second column of an entry, the selected buffer is marked to be
     saved;
 (3) anywhere else within an entry line, all saves and deletes are done, and
     selected buffers are displayed, including the one just clicked on (if
     within the OO-Browser user interface, only the selected buffer is
     displayed);
 (4) on or after the last line in the buffer, all saves and deletes are done."

  (interactive)
  (cond ((last-line-p) (Buffer-menu-execute))
	((bolp) (Buffer-menu-mark))
	((save-excursion
	   (goto-char (1- (point)))
	   (bolp))
	 (Buffer-menu-save))
	((br-in-browser) (br-buffer-menu-select))
	((smart-buffer-menu-no-marks))
	(t (Buffer-menu-select))))

(defun smart-buffer-menu-assist ()
  "Use assist key or mouse assist key to manipulate `buffer-menu' entries.

Invoked via an assist key press when in `Buffer-menu-mode'.  It assumes that its
caller has already checked that the assist key was pressed in an appropriate
buffer and has moved the cursor there.

If assist key is pressed:
 (1) on the first or second column of an entry, the selected buffer is unmarked
     for display and for saving or deletion; 
 (2) anywhere else within an entry line, the selected buffer is marked for
     deletion;
 (3) on or after the last line in the buffer, all display, save, and delete
     marks on all entries are undone."

  (interactive)
  (cond ((last-line-p) (progn (list-buffers) (forward-line 3)))
	((bolp) (Buffer-menu-unmark))
        ((save-excursion
	   (goto-char (1- (point)))
	   (bolp))
	 (Buffer-menu-unmark))
	(t (Buffer-menu-delete))))

;;; ************************************************************************
;;; smart-ibuffer-menu functions
;;; ************************************************************************

(defun smart-ibuffer-menu-no-marks ()
  "Display this line's buffer in this window.
Bury the buffer menu unless other buffers are marked.  If buffer
menu items are marked, return nil, else t."
  (let* ((this-buffer (ibuffer-current-buffer t))
	 (menu-buffer (current-buffer))
	 (others (delq this-buffer (ibuffer-get-marked-buffers))))
    (unless others
      (switch-to-buffer this-buffer)
      (unless (eq menu-buffer this-buffer)
	(bury-buffer menu-buffer))
      t)))

(defun smart-ibuffer-menu ()
  "Use a single key or mouse key to manipulate ibuffer entries.

Invoked via a key press when in ibuffer-mode.  It assumes that
its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor there.

If key is pressed:
 (1) on the first or second column of an entry, the selected buffer is
     marked for display;
 (2) anywhere else within an entry line, all saves and deletes are done, and
     selected buffers are displayed, including the one just clicked on (if
     within the OO-Browser user interface, only the selected buffer is
     displayed);
 (3) on the first or last line in the buffer, all deletes are done."

  (interactive)
  (cond ((or (first-line-p) (last-line-p))
	 (ibuffer-do-kill-on-deletion-marks))
	((or (bolp) (save-excursion
		      (goto-char (1- (point)))
		      (bolp)))
	 (ibuffer-mark-forward nil nil 1))
	((br-in-browser) (br-buffer-menu-select))
	((smart-ibuffer-menu-no-marks))
	(t (ibuffer-do-view))))

(defun smart-ibuffer-menu-assist ()
  "Use assist key or mouse assist key to manipulate `buffer-menu' entries.

Invoked via an assist key press when in ibuffer-mode.  It assumes that
its caller has already checked that the assist key was pressed in an
appropriate buffer and has moved the cursor there.

If assist key is pressed:
 (1) on the first or second column of an entry, the selected buffer is unmarked
     for display or deletion; 
 (2) anywhere else within an entry line, the selected buffer is marked for
     deletion;
 (3) on the first or last line in the buffer, all display, save, and delete
     marks on all entries are undone."

  (interactive)
  (cond ((or (first-line-p) (last-line-p))
	 (if (fboundp 'ibuffer-unmark-all-marks)
	     (ibuffer-unmark-all-marks)
	   (ibuffer-unmark-all 0)))
	((or (bolp) (save-excursion
		      (goto-char (1- (point)))
		      (bolp)))
	 (ibuffer-unmark-forward nil nil 1))
	(t (ibuffer-mark-for-delete nil nil 1))))

;;; ************************************************************************
;;; smart-custom and widget functions
;;; ************************************************************************

(defun smart-custom ()
  "Use a single key or mouse key to manipulate customizable settings.

Invoked via a key press when in `Custom-mode'.  It assumes that
its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor there.

If key is pressed:
 (1) on the last line of the buffer, exit Custom mode, potentially
     prompting to save any changes;
 (2) at the end of any other line, scroll the window down down a windowful;
 (3) if a mouse event on a widget, activate the widget or display a menu;
 (4) anywhere else, execute the command bound to {RETURN}."
  (interactive)
  (cond ((last-line-p) (Custom-buffer-done))
	((eolp) (smart-scroll-up))
	((mouse-event-p last-command-event)
	 (widget-button-click action-key-release-args))
	 ;; Handle widgets in Custom-mode
	(t (Custom-newline (point)))))

(defun smart-custom-assist ()
  "Use an assist key or mouse assist key to manipulate customizable settings.

Invoked via a key press when in `Custom-mode'.  It assumes that
its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor there.

If key is pressed:
 (1) on the last line of the buffer, exit Custom mode, potentially
     prompting to save any changes;
 (2) at the end of any other line, scroll the window down down a windowful;
 (3) if a mouse event on a widget, activate the widget or display a menu;
 (4) anywhere else, execute the command bound to {RETURN}."
  (interactive)
  (cond ((last-line-p) (Custom-buffer-done))
	((eolp) (smart-scroll-down))
	((mouse-event-p last-command-event)
	 (widget-button-click action-key-release-args))
	 ;; Handle widgets in Custom-mode
	(t (Custom-newline (point)))))




;;; ************************************************************************
;;; smart-calendar functions
;;; ************************************************************************

(defun smart-calendar ()
  "Use a single key or mouse key to manipulate the scrolling calendar.

Invoked via a key press when in `calendar-mode'.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) at the end of the buffer, the calendar is scrolled forward 3 months;
 (2) to the left of any dates on a calendar line, the calendar is scrolled
     backward 3 months;
 (3) on a date, the diary entries for the date, if any, are displayed."

  (interactive)
  (cond ((smart-eobp) (calendar-cursor-to-nearest-date)
	 (calendar-scroll-left-three-months 1))
	((< (current-column) 5) (calendar-cursor-to-nearest-date)
	 (calendar-scroll-right-three-months 1))
	(t (calendar-cursor-to-nearest-date)
	   (diary-view-entries 1))))

(defun smart-calendar-assist ()
  "Use the assist key or mouse assist key to manipulate the scrolling calendar.

Invoked via an assist key press when in `calendar-mode'.  It assumes that its
caller has already checked that the assist key was pressed in an appropriate
buffer and has moved the cursor there.

If assist key is pressed:
 (1) at the end of the buffer, the calendar is scrolled backward 3 months;
 (2) to the left of any dates on a calendar line, the calendar is scrolled
     forward 3 months;
 (3) anywhere else, all dates with marking diary entries are marked in the
     calendar window."

  (interactive)
  (cond ((smart-eobp) (calendar-cursor-to-nearest-date)
	 (calendar-scroll-right-three-months 1))
	((< (current-column) 5) (calendar-cursor-to-nearest-date)
	 (calendar-scroll-left-three-months 1))
	(t (diary-mark-entries))))

;;; ************************************************************************
;;; smart-company mode functions
;;; ************************************************************************

;; These functions are called from hkey-alist when keyboard Smart Keys
;; are used.  For mouse keys, they are bound to local keys in
;; company-mode's minor mode map.

(defun smart-company-to-definition (event)
  "Action Key binding for `company-mode' completions to show item definition.
Use left mouse key, RET or TAB key to select a completion and exit."
  (interactive "e")
  (when (mouse-event-p last-command-event)
    (company-select-mouse event))
  (company-show-location))

(defun smart-company-help (event)
  "Assist Key binding for `company-mode' completions popup to show item doc."
  (interactive "e")
  (when (mouse-event-p last-command-event)
    (company-select-mouse event))
  (if (featurep 'company-quickhelp)
      (company-quickhelp-manual-begin)
    (company-show-doc-buffer)))

;;; ************************************************************************
;;; smart-dired functions
;;; ************************************************************************

(defun smart-dired-pathname-up-to-point (&optional no-default)
  "Return the part of the pathname up through point, else current directory.
Use for direct selection of an ancestor directory of the
Dired directory at point, if any.

With optional NO-DEFAULT, do not default to current directory
path; instead return nil."
  (interactive)
  (when (not (derived-mode-p 'dired-mode))
    (error "(smart-dired-pathname-up-to-point): Called from non-dired buffer, '%s'"
	   (buffer-name)))
  (if (dired-get-subdir) ;; On a dired directory line
      (save-excursion
	(re-search-forward "[/:\n\r\t\f]" (line-end-position) t)
	(buffer-substring-no-properties
	  (if (and (not (bobp)) (= (preceding-char) ?/))
	      (point)
	    (1- (point)))
	  (progn (beginning-of-line)
		 (skip-syntax-forward "-")
		 (point))))
    (unless no-default
      (expand-file-name default-directory))))

(defun smart-dired ()
  "Use a single key or mouse key to manipulate directory entries.

Invoked via a key press when in `dired-mode'.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) within an entry line, the selected file/directory is displayed
     for editing, normally in another window but if an entry has been dragged
     for display in another window, then this entry is displayed in the current
     window (DisplayHere minor mode is shown in the mode-line; use {g}
     to disable it)
 (2) on a Dired header line (other than the end of line):
     (a) within the leading whitespace, then if any deletes are to be
         performed, they are executed after user verification; otherwise,
         nothing is done;
     (b) otherwise, Dired is run in another window on the ancestor directory
         of the current directory path up through the location of point;
         if point is on the first character, then the / root directory
         is used.
 (3) on or after the last line in the buffer, this Dired invocation is quit."

  (interactive)
  (cond ((save-excursion
	   (forward-line 0)
	   (looking-at dired-subdir-regexp))
	 (cond ((and (looking-at "\\s-")
		     (save-excursion
		       (skip-syntax-backward "-"))
		     (bolp))
		;; In whitespace at beginning of a directory header line, perform deletes.
		(if (save-excursion
		      (goto-char (point-min))
		      (re-search-forward "^D" nil t))
		    (cond ;; For Tree-dired compatibility
		     ((fboundp 'dired-do-flagged-delete)
		      (dired-do-flagged-delete))
		     ((fboundp 'dired-do-deletions)
		      (dired-do-deletions))
		     (t (error "(smart-dired): No Dired expunge function")))))
	       (t (hpath:find (smart-dired-pathname-up-to-point)))))
	((last-line-p) (quit-window))
	(t (hpath:find (or (dired-get-filename nil t) "")))))

(defun smart-dired-assist ()
  "Use a single assist key or mouse assist key to manipulate directory entries.

Invoked via an assist key press when in `dired-mode'.  It assumes that its
caller has already checked that the assist key was pressed in an appropriate
buffer and has moved the cursor there.

If assist key is pressed:
 (1) on a `~' character, all backup files in the directory are marked for
     deletion;
 (2) on a `#' character, all auto-save files in the directory are marked for
     deletion;
 (3) anywhere else within an entry line, the current entry is marked for
     deletion;
 (4) on or after the last line in the buffer, all delete marks on all entries
     are undone."

  (interactive)
  (cond ((or (last-line-p) (and (first-line-p) (eolp)))
	 (dired-unmark-all-files ?D)
	 (goto-char (point-max)))
	((looking-at "~") (dired-flag-backup-files))
	((looking-at "#") (dired-flag-auto-save-files))
	(t
	 ;; Prevent any region selection from causing multiple files
	 ;; to be marked for deletion; we want to mark only one.
	 (deactivate-mark t)
	 (dired-flag-file-deletion 1))))

;;; ************************************************************************
;;; smart-ert-results functions
;;; ************************************************************************

(defalias 'smart-ert-results 'ert-results-filter)

(defun smart-ert-results-assist (_status-symbol)
  "Display help documentation for the `ert-results-mode' test at point, if any.
Trigger an error if there is no test result at or before point."
  (ert-results-describe-test-at-point))

;;; ************************************************************************
;;; smart-gnus functions
;;; ************************************************************************

(defun smart-gnus-group ()
  "Use a key or mouse key to move through Gnus Newsgroup listings.
Invoked via a key press when in `gnus-group-mode'.  It assumes that its caller
has already checked that the key was pressed in an appropriate buffer and has
moved the cursor to the selected buffer.

If key is pressed within:
 (1) a GNUS-GROUP line, that newsgroup is read;
 (2) if `gnus-topic-mode' is active, and on a topic line, the topic is
     expanded or collapsed as needed;
 (3) to the left of any GNUS-GROUP line, on any of the whitespace, the current
     group is unsubscribed or resubscribed;
 (4) at the end of the GNUS-GROUP buffer, after all lines, checks for new
     news."

  (interactive)
  (cond ((last-line-p) (gnus-group-get-new-news))
	((progn (skip-chars-backward " U") (bolp))
	 (gnus-group-unsubscribe-current-group))
	((gnus-topic-mode-p) (gnus-topic-read-group))
	(t (gnus-group-read-group nil))))

(defun smart-gnus-group-assist ()
  "Use assist key or assist-mouse key to move through Gnus Newsgroup listings.
Invoked via an assist key press when in `gnus-group-mode'.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor to the selected buffer.

If key is pressed within:
 (1) a GNUS-GROUP line, that newsgroup is read;
 (2) if `gnus-topic-mode' is active, and on a topic line, the topic is
     expanded or collapsed as needed;
 (3) to the left of any GNUS-GROUP line, on any of the whitespace, the user is
     prompted for a group name to subscribe or unsubscribe to;
 (4) at the end of the GNUS-GROUP buffer, after all lines, quits from the
     newsreader."

  (interactive)
  (cond ((last-line-p) (gnus-group-exit))
	((progn (skip-chars-backward " U") (bolp))
	 (call-interactively 'gnus-group-unsubscribe-group))
	((gnus-topic-mode-p) (gnus-topic-read-group))
	(t (gnus-group-read-group nil))))

(defun smart-gnus-summary ()
  "Use a key or mouse key to move through Gnus News article listings.
Invoked via a key press when in `gnus-summary-mode'.  It assumes that its caller
has already checked that the key was pressed in an appropriate buffer and has
moved the cursor to the selected buffer.

If key is pressed within:
 (1) to the left of an article number, that article is marked as unread;
 (2) a GNUS-SUMMARY line, that article is read, marked deleted, and scrolled
     forward;
 (3) at the end of the GNUS-SUMMARY buffer, the next undeleted article
     is read or the next group is entered."

  (interactive)
  (cond ((last-line-p)
	 (if gnus-current-article
	     (progn (goto-char (point-min))
		    (re-search-forward
		     (format "^.[ ]+%d:" gnus-current-article) nil t)
		    (setq this-command 'gnus-summary-next-page)
		    (call-interactively 'gnus-summary-next-page))
	   (goto-char (point-min))
	   (setq this-command 'gnus-summary-first-unread-article)
	   (call-interactively 'gnus-summary-first-unread-article)))
	((save-excursion (skip-chars-backward " D") (bolp))
	 (gnus-summary-mark-article-as-unread ?\ )
	 (forward-line 1))
	(t (setq this-command 'gnus-summary-next-page)
	   (call-interactively 'gnus-summary-next-page))))

(defun smart-gnus-summary-assist ()
  "Use an assist key or assist-mouse key to move through Gnus News articles.
Invoked via an assist key press when in `gnus-summary-mode'.  It assumes that
its caller has already checked that the key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If key is pressed within:
 (1) to the left of an article number, that article is marked as unread;
 (2) a GNUS-SUMMARY line, that article is read and scrolled backward;
 (3) at the end of the GNUS-SUMMARY buffer, the summary is exited, the user
     is returned to group mode."

  (interactive)
  (cond ((last-line-p)
	 (setq this-command 'gnus-summary-prev-page)
	 (call-interactively 'gnus-summary-exit))
	((save-excursion (skip-chars-backward " D") (bolp))
	 (gnus-summary-mark-article-as-unread ?\ )
	 (forward-line -1))
	(t (setq this-command 'gnus-summary-prev-page)
	   (call-interactively 'gnus-summary-prev-page))))

(defun smart-gnus-article ()
  "Use a key or mouse key to move through Gnus netnews articles.

Invoked via a key press when in `gnus-article-mode'.
It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed within:
 (1) the first line or end of an article, the next unread message is displayed;
 (2) the first line of an Info cross reference, the reference is followed;
 (3) anywhere else, the window is scrolled up a windowful."
  (interactive)
  (cond ((or (last-line-p) (and (not (eolp)) (first-line-p)))
	 (unwind-protect
	     (progn (set-buffer gnus-summary-buffer)
		    (setq this-command 'gnus-summary-next-unread-article)
		    (gnus-summary-next-unread-article)
		    (gnus-summary-goto-subject gnus-current-article))
	   (let ((artic (get-buffer-window gnus-article-buffer)))
	     (if artic (select-window artic)))))
	((and (not (eolp)) (Info-handle-in-note)))
	(t (smart-scroll-up))))

(defun smart-gnus-article-assist ()
  "Use an assist key or mouse assist key to move through Gnus netnews articles.

Invoked via an assist key press when in `gnus-article-mode'.  It
assumes that its caller has already checked that the assist key
was pressed in an appropriate buffer and has moved the cursor to
the selected buffer.

If assist key is pressed within:
 (1) the first line or end of an article, the previous message is displayed;
 (2) the first line of an Info cross reference, the reference is followed;
 (3) anywhere else, the window is scrolled down a windowful."
  (interactive)
  (cond ((or (last-line-p) (and (not (eolp)) (first-line-p)))
	 (unwind-protect
	     (progn (set-buffer gnus-summary-buffer)
		    (setq this-command 'gnus-summary-prev-article)
		    (gnus-summary-prev-article nil)
		    (gnus-summary-goto-subject gnus-current-article))
	   (let ((artic (get-buffer-window gnus-summary-buffer)))
	     (if artic (select-window artic)))))
	((and (not (eolp)) (Info-handle-in-note)))
	(t (smart-scroll-down))))

;;; ************************************************************************
;;; smart-helm functions
;;; ************************************************************************

(defun smart-helm-at-header ()
  "Return t iff Action Key depress was on a helm header.
To be precise, if Action Key depress was on the first fixed header or
a helm section header."
  (or (helm-pos-header-line-p)
      (and (eventp action-key-depress-args)
	   (eq (posn-area (event-start action-key-depress-args))
	       'header-line))))

(defun smart-helm-get-current-action (&optional action)
  "Return the helm default action.
Get it from optional ACTION, the helm saved action or from the
selected helm item."
  (helm-get-default-action (or action
                               helm-saved-action
                               (if (get-buffer helm-action-buffer)
                                   (helm-get-selection helm-action-buffer)
                                 (helm-get-actions-from-current-source)))))

(defun smart-helm-line-has-action ()
  "Mark and return the actions for the helm selection item at the point.
Point is where Action Key was depress.  Return nil if line lacks
any action.  Assume Hyperbole has already checked that helm is
active."
  (let ((helm-buffer (if (equal helm-action-buffer (buffer-name)) helm-buffer (buffer-name))))
    (save-excursion
      (with-helm-buffer
	(setq cursor-type hkey-debug) ; For testing where mouse presses set point.
	(and (eventp action-key-depress-args)
	     (goto-char (posn-point (event-start action-key-depress-args))))
	(when (not (or (smart-eobp)
		       (smart-helm-at-header)
		       (helm-pos-candidate-separator-p)))
	  (let ((helm-selection-point (point)))
	    (helm-mark-current-line)
	    (smart-helm-get-current-action)))))))

(defun smart-helm-alive-p ()
  "Return t if `helm' completion is actively prompting."
  ;; Handles case where helm-action-buffer is visible but helm-buffer
  ;; is not; fixed in helm with commit gh#emacs-helm/helm/cc15f73.
  (and (featurep 'helm)
       helm-alive-p
       (window-live-p (helm-window))
       (minibuffer-window-active-p (minibuffer-window))))

(defun smart-helm-resume-helm ()
  "Resumes helm session for the current buffer if not already active."
  (unless (smart-helm-alive-p)
    (unless (equal helm-action-buffer (buffer-name))
      ;; helm-resume doesn't seem to set this properly.
      (setq helm-buffer (buffer-name)))
    (helm-resume helm-buffer)
    (sit-for 0.2)))

(defun smart-helm-at (depress-event)
  "Return non-nil iff Smart Mouse DEPRESS-EVENT was on a helm section header.
This also includes if DEPRESS-EVENT was on a candidate separator
or at eob or eol.
If non-nil, returns a property list of the form: (section-header
<bool> separator <bool> eob <bool> or eol <bool>).  If a
section-header or separator, selects the first following
candidate line.
Assume Hyperbole has already checked that helm is active."
  (and (eventp depress-event)
       ;; Nil means in the buffer text area
       (not (posn-area (event-start depress-event)))
       (with-helm-buffer
	 (let ((opoint (point))
	       things)
	   (mouse-set-point depress-event)
	   (setq things (list 'section-header (helm-pos-header-line-p)
			      'separator (helm-pos-candidate-separator-p)
			      'eob (smart-eobp)
			      'eol (eolp)))
	   (cond ((or (plist-get things 'section-header) (plist-get things 'separator))
		  (helm-next-line 1)
		  things)
		 ((plist-get things 'eol)
		  (helm-mark-current-line)
		  things)
		 ((plist-get things 'eob)
		  things)
		 (t
		  (goto-char opoint)
		  nil))))))

(defun smart-helm-to-minibuffer ()
  "Select minibuffer window when it is active."
  (if (> (minibuffer-depth) 0)
      (select-window (minibuffer-window))))

(defun smart-helm()
  "Execute helm actions based on Action Key click locations.
Locations are:
- At the end of the buffer, quits from helm and exits the minibuffer.
- On a candidate line, performs the candidate's first action and
  remains in the minibuffer;
- On the top, fixed header line, toggles display of the selected
  candidate's possible actions;
- On an action list line, performs the action after exiting the minibuffer;
- On a source section header, moves to the next source section or
  first if on last.
- On a candidate separator line, does nothing.
- In the minibuffer window, ends the helm session and performs
  the selected item's action."
  (unless (hmouse-check-action-key)
    (error "(smart-helm): Hyperbole Action Mouse Key - either depress or release event is improperly configured"))
  (let* ((non-text-area-p (and (eventp action-key-depress-args)
			       (posn-area (event-start action-key-depress-args))))
	 (at-plist (smart-helm-at action-key-depress-args))
	 (section-hdr (plist-get at-plist 'section-header))
	 (separator (plist-get at-plist 'separator))
	 (eob (plist-get at-plist 'eob))
	 (eol (plist-get at-plist 'eol)))
    (smart-helm-resume-helm)
    ;; Handle end-of-line clicks.
    (if (and eol (not eob) (not non-text-area-p))
	(progn (with-helm-buffer (funcall action-key-eol-function))
	       (smart-helm-to-minibuffer))
      (smart-helm-to-minibuffer)
      (when (and (smart-helm-alive-p) (not separator))
	(let* ((key (kbd (cond
			  ;; Exit
			  (eob "C-g")
			  ;; Move to next source section or first if on last.
			  (section-hdr "C-o")
			  ;; If line of the key press is the first /
			  ;; header line in the window or outside the
			  ;; buffer area, then use {TAB} command to
			  ;; switch between match list and action list.
			  (non-text-area-p "TAB")
			  ;; RET: Performs action of selection and exits the minibuffer.
			  ;; C-j: Performs action of selection and stays in minibuffer.
			  (hkey-value
			   (if (helm-action-window) "RET" "C-j"))
			  (t "RET"))))
	       (binding (key-binding key)))
	  (if hkey-debug
	      (message "(HyDebug): In smart-helm, key to execute is: {%s}; binding is: %s"
		       (key-description key) binding))
	  (call-interactively binding))))))

(defun smart-helm-assist()
  "Execute helm actions based on Assist Key click locations.
Locations are:
- At the end of the buffer, quits from helm and exits the minibuffer.
- On a candidate line, display's the candidate's first action and
  remains in the minibuffer;
- On the top, fixed header line, toggles display of the selected
  candidate's possible actions;
- On an action list line, performs the action after exiting the minibuffer;
- On a source section header, moves to the previous source
  section or last if on first.
- On a candidate separator line, does nothing.
- In the minibuffer window, ends the helm session and performs
  the selected item's action."
  ;; Hyperbole has checked that this line has an action prior to
  ;; invoking this function.
  (unless (hmouse-check-assist-key)
    (error "(smart-helm-assist): Hyperbole Assist Mouse Key - either depress or release event is improperly configured"))
  (let* ((non-text-area-p (and (eventp assist-key-depress-args)
			       (posn-area (event-start assist-key-depress-args))))
	 (at-plist (smart-helm-at assist-key-depress-args))
	 (section-hdr (plist-get at-plist 'section-header))
	 (separator (plist-get at-plist 'separator))
	 (eob (plist-get at-plist 'eob))
	 (eol (plist-get at-plist 'eol))
	 (key))
    (unwind-protect
	(smart-helm-resume-helm)
      ;; Handle end-of-line clicks.
      (cond ((and eol (not eob) (not non-text-area-p))
	     (with-helm-buffer (funcall assist-key-eol-function)))
	    ((and (smart-helm-alive-p) (not separator))
	     (setq key (cond
			;; Exit
			(eob "C-g")
			;; Move to previous source section or last if on last.
			(section-hdr "M-o")
			;; If line of the key press is the first /
			;; header line in the window or outside the
			;; buffer area, then use {TAB} command to
			;; switch between match list and action list.
			(non-text-area-p "TAB")
			;; Display action for the current line and
			;; return nil.
			(t (with-help-window "*Helm Help*"
			     (let ((helm-buffer (if (equal helm-action-buffer (buffer-name))
						    helm-buffer (buffer-name))))
			       (with-helm-buffer
				 (princ "The current helm selection item is:\n\t")
				 (princ (helm-get-selection (helm-buffer-get)))
				 (princ "\nwith an action of:\n\t")
				 (princ (smart-helm-get-current-action)))
			       nil)))))
	     (if hkey-debug
		 (message "(HyDebug): In smart-helm-assist, key to execute is: {%s}; binding is: %s"
			  (if key (key-description key) "Help")
			  (if key (key-binding key) "None")))))
      (smart-helm-to-minibuffer))
    (if key (call-interactively (key-binding (kbd key))))))

;;; ************************************************************************
;;; smart-hmail functions
;;; ************************************************************************

(defun smart-hmail ()
  "Use a key or mouse key to move through e-mail messages and summaries.

Invoked via a key press when in `hmail:reader' or `hmail:lister' mode.
It assumes that its caller has already checked that the key was pressed
in an appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed within:
 (1) a msg buffer, within the first line or at the end of a message,
     the next undeleted message is displayed;
 (2) a msg buffer within the first line of an Info cross reference, the
     reference is followed;
 (3) anywhere else in a msg buffer, the window is scrolled up a windowful; 
 (4) a msg summary buffer on a header entry, the message corresponding to
     the header is displayed in the msg window;
 (5) a msg summary buffer, on or after the last line, the messages marked
     for deletion are expunged."

  (interactive)
  ;;
  ;; Branch on buffer type
  ;;
  (cond ((eq major-mode hmail:reader)
	 (cond ((or (last-line-p) (and (not (eolp)) (first-line-p)))
		(rmail:msg-next))
	       ((and (not (eolp)) (Info-handle-in-note)))
	       ((smart-scroll-up))))
	;;
	;; Assume are in msg summary buffer
	;;
	((last-line-p) (lmail:expunge))
	(t (lmail:goto))))

(defun smart-hmail-assist ()
  "Use an assist key or mouse key to move through e-mail messages and summaries.

Invoked via an assist key press when in `hmail:reader' or
`hmail:lister' mode.  It assumes that its caller has already
checked that the assist key was pressed in an appropriate buffer
and has moved the cursor to the selected buffer.

If assist key is pressed within:
 (1) a msg buffer, within the first line or at the end of a message,
     the previous undeleted message is displayed;
 (2) a msg buffer within the first line of an Info cross reference, the
     reference is followed;
 (3) anywhere else in a msg buffer, the window is scrolled down a windowful;
 (4) a msg summary buffer on a header entry, the message corresponding to
     the header is marked as deleted;
 (5) a msg summary buffer, on or after the last line, all messages are
     marked undeleted."

  (interactive)
  ;;
  ;; Branch on buffer type
  ;;
  (cond ((eq major-mode hmail:reader)
	 (cond ((or (last-line-p) (and (not (eolp)) (first-line-p)))
		(rmail:msg-prev))
	       ((and (not (eolp)) (Info-handle-in-note)))
	       ((smart-scroll-down))))
	;;
	;; Assume are in msg summary buffer
	;;
	((last-line-p) (lmail:undelete-all))
	(t (lmail:delete))))

;;; ************************************************************************
;;; smart-hyrolo functions
;;; ************************************************************************

(defun smart-hyrolo ()
  "In hyrolo match buffer, edit current entry.
If on a file header, edit the file.  Uses one key or mouse key.

Invoked via a key press when in the `hyrolo-display-buffer'.  Assume that
its caller has already checked that the key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer."
  (interactive)
  (if (hyrolo-hdr-in-p)
      (hact 'hyp-source (save-excursion
			  (when (and (hyrolo-hdr-to-first-line-p)
				     (search-forward hbut:source-prefix nil t))
			    (hbut:source t))))
    (hyrolo-edit-entry)))

(defalias 'smart-hyrolo-assist #'smart-hyrolo)

;;; ************************************************************************
;;; smart-image-dired functions
;;; ************************************************************************

(defun smart-image-dired-thumbnail ()
  "Select thumbnail and scale its image for display in another Emacs window."
  (image-dired-mouse-select-thumbnail action-key-release-args)
  (image-dired-display-thumbnail-original-image))

(defun smart-image-dired-thumbnail-assist ()
  "Select thumbnail and use the external viewer to display it.
External viewer is named by `image-dired-external-viewer'"
  (image-dired-mouse-select-thumbnail assist-key-release-args)
  (image-dired-thumbnail-display-external))

;;; ************************************************************************
;;; smart-imenu functions
;;; ************************************************************************

(defun smart-imenu-display-item-where (item-name item-pos)
  "Display an ITEM-NAME defined at ITEM-POS within the current buffer.
Uses the imenu library and the value of `hpath:display-where' to display."
  (hpath:display-buffer (current-buffer))
  (funcall imenu-default-goto-function item-name item-pos))

(defun smart-imenu-item-at-p (&optional variable-flag)
  "Return marker position for identifier at point in current buffer's imenu.
If no identifier at point is in the current buffer's imenu, return nil.
With optional VARIABLE-FLAG non-nil, matches to variable definitions only.

Does nothing unless imenu has been loaded and an index has been
created for the current buffer.  When return value is non-nil, also
sets `hkey-value' to (identifier . identifier-definition-buffer-position)."
  (and (featurep 'imenu) imenu--index-alist
       (setq hkey-value (smart-imenu-item-p hkey-value variable-flag))
       (setq hkey-value (cons (hargs:find-tag-default) hkey-value))
       (cdr hkey-value)))

;; Derived from `imenu' function in the imenu library.
(defun smart-imenu-item-p (index-key &optional variable-flag no-recurse-flag)
  "Return the definition marker pos for INDEX-KEY in current buffer's imenu.
Return nil if INDEX-KEY is not in the imenu.  If INDEX-KEY is
both a function and a variable, the function definition is used
by default; in such a case, when optional VARIABLE-FLAG is
non-nil, the variable definition is used instead.
NO-RECURSE-FLAG non-nil prevents infinite recursions."
  (when (stringp index-key)
    (let (index-item
	  index-position)
      ;; Convert a string to an alist element.
      (if variable-flag
	  ;; Match to variable definitions only.
	  (setq index-item (assoc index-key
				  (cdr (assoc "Variables" (imenu--make-index-alist)))))
	;; First try to match a function definition and if that fails,
	;; then allow variable definitions and other types of matches as well.
	(let ((alist (imenu--make-index-alist)))
	  (setq index-item (or (assoc index-key alist)
			       ;; Does nothing unless the dash Emacs Lisp
			       ;; library is available for the -flatten function.
			       (and (require 'dash nil t)
			            (progn
			              (declare-function -flatten "ext:dash")
				      (assoc index-key (-flatten alist))))))))
      (when index-item
	(setq index-position (when (markerp (cdr index-item))
			       (marker-position (cdr index-item))))
	(if (memq index-position '(1 -99))
	    ;; If index position is 1 or -99, this means the index markers have
	    ;; become out of date after buffer edits (likely imenu-auto-rescan
	    ;; is nil), so do a single rescan to try to fix this.
	    (unless no-recurse-flag
	      (setq imenu--index-alist nil)
	      (imenu--make-index-alist t)
	      (smart-imenu-item-p index-key variable-flag t))
	  index-position)))))

;;; ************************************************************************
;;; smart-info functions
;;; ************************************************************************
;;; In "hmouse-info.el".


;;; ************************************************************************
;;; smart-magit functions
;;; ************************************************************************

(defun smart-magit-display-file (return-command)
  "Execute `magit' RETURN-COMMAND possibly using `hpath:display-buffer'."
  (cond ((eq return-command #'magit-diff-visit-file)
	 ;; Use Hyperbole display variable to determine where
	 ;; to display the file of the diff.
	 (magit-diff-visit-file--internal
	  (magit-file-at-point t t) nil #'hpath:display-buffer))
	(t (call-interactively return-command))))

(defun smart-magit ()
  "Use a key or mouse key to jump to source and to hide/show changes.

Invoked via a key press when in a magit mode and not on a button.
It assumes that its caller has already checked that the key was
pressed in an appropriate buffer and has moved the cursor to the
selected buffer.

If key is pressed:
 (1) on the last line, quit from the magit mode (\"q\" key binding);
 (2) at the end of a line, scroll up a windowful;
 (3) in a `magit-status-mode' buffer on a merge conflict marker, keep
     either the upper, both or the lower version of the conflict.
 (4) on an initial read-only header line, cycle visibility of diff sections;
 (5) anywhere else, hide/show the thing at point (\"TAB\" key binding)
     unless that does nothing in the mode, then jump to the thing at
     point (\"RET\" key binding) but display based on the value of
     `hpath:display-where'."
  (interactive)
  (let (op)
    (cond ((last-line-p)
	   (call-interactively (key-binding "q")))
	  ((eolp)
	   (smart-scroll-up))
          ((and
            (eq major-mode 'magit-status-mode)
            (save-excursion
              (beginning-of-line)
              (cond ((looking-at (regexp-quote "++>>>>>>>"))
                     (setq op 'magit-smerge-keep-lower))
                    ((looking-at (regexp-quote "++<<<<<<<"))
                     (setq op 'magit-smerge-keep-upper))
                    ((looking-at (regexp-quote "++======="))
                     (setq op 'magit-smerge-keep-all)))))
           (call-interactively op))
	  (t
	   (let ((magit-display-buffer-function #'hpath:display-buffer))
	     (call-interactively #'smart-magit-tab))))))

(defun smart-magit-assist ()
  "Use an assist key or mouse key to jump to source and to hide/show changes.

Invoked via an assist key press when in a magit mode and not on a
button.  It assumes that its caller has already checked that the
assist key was pressed in an appropriate buffer and has moved the
cursor to the selected buffer.

If assist key is pressed:
 (1) on the last line, quit from the magit mode (\"q\" key binding);
 (2) at the end of a line, scroll down a windowful;
 (3) on an initial read-only header line, cycle visibility of all sections;
 (4) anywhere else, jump to the thing at point (\"RET\" key binding)
     but display based on the value of `hpath:display-where'."
  (interactive)
  (cond ((last-line-p)
	 (call-interactively (key-binding "q")))
	((eolp)
	 (smart-scroll-down))
	(t
	 (let ((magit-display-buffer-function #'hpath:display-buffer)
	       (current-prefix-arg)
	       (non-text-area-p (and (eventp action-key-depress-args)
				     (posn-area (event-start action-key-depress-args)))))
	   (if non-text-area-p
	       (magit-section-cycle-global)
	     (smart-magit-display-file (key-binding (kbd "RET"))))))))

;; Silence compiler about unknown slot. From eieio-tests.el
(eval-when-compile
  (dolist (slot '(content washer hidden start))
    (cl-pushnew slot eieio--known-slot-names)))

;; Thanks to Jonas Bernoulli <tarsius>, magit author, for most of this
;; next function.
;; Usage: (define-key magit-section-mode-map "TAB" 'smart-magit-tab)
(defun smart-magit-tab (section)
  "Toggle visibility of the body of the current SECTION."
  (interactive (list (magit-current-section)))
  (require 'magit)
  (let* ((magit-display-buffer-function #'hpath:display-buffer)
	 (non-text-area-p (and (eventp action-key-depress-args)
			       (posn-area (event-start action-key-depress-args)))))
    (cond (non-text-area-p (magit-section-cycle-diffs))
	  ((eq section magit-root-section)
	   (user-error "Cannot hide root section"))
          ((or (oref section content)
	       (oref section washer))
	   (goto-char (oref section start))
	   (if (oref section hidden)
	       (magit-section-show section)
	     (magit-section-hide section)))
          (t
	   (if-let* ((command (key-binding (kbd "RET"))))
	       (progn (setq last-command-event ?\()
		      (setq this-command command)
		      (smart-magit-display-file command))
	     (user-error "Nothing to visit either"))))))

;;; ************************************************************************
;;; smart-man functions
;;; ************************************************************************

;; "man-apropos.el" which contains the unix-apropos functions below is a
;; part of InfoDock; these functions are not called unless this
;; library has been loaded and is in use.  It generates a buffer of apropos
;; listing and allows selection and associated man page display.
;;
;; Man page cross-references in Emacs man buffers are handled
;; separately via the 'man-apropos' implicit button type.
;;

(defun smart-apropos ()
  "Move through UNIX man apropos listings by using one key or mouse key.

Invoked via a key press when in `unix-apropos-mode'.  It assumes that
its caller has already checked that the key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) on a UNIX man apropos entry, the man page for that entry is displayed in
     another window;
 (2) on or after the last line, the buffer in the other window is scrolled up
     a windowful."

  (interactive)
  (if (last-line-p)
      (scroll-other-window)
    ;; Called only if man-apropos.el of InfoDock is loaded
    (when (fboundp #'unix-apropos-get-man)
      (unix-apropos-get-man))))

(defun smart-apropos-assist ()
  "Move through man apropos listings by using assist key or mouse assist key.

Invoked via an assist key press when in `unix-apropos-mode'.  It assumes that
its caller has already checked that the assist key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If assist key is pressed:
 (1) on a UNIX man apropos entry, the man page for that entry is displayed in
     another window;
 (2) on or after the last line, the buffer in the other window is scrolled down
     a windowful."

  (interactive)
  (if (last-line-p)
      (scroll-other-window (- 3 (window-height)))
    ;; Called only if man-apropos.el of InfoDock is loaded
    (when (fboundp #'unix-apropos-get-man)
      (unix-apropos-get-man))))

(defun smart-man-display (lisp-form)
  "Evaluate LISP-FORM returned from `smart-man-entry-ref' to display a man page."
  (eval lisp-form))

(defun smart-man-entry-ref ()
  "Return form which displays referenced manual entry that point is on or nil.
Handles references in sections: NAME, SEE ALSO, or PACKAGES USED.  Also can
display C routine definitions selected in a man page, see
`smart-man-c-routine-ref'.

Man page buffer must either have an attached file or else a `man-path'
local variable containing its pathname."
  (interactive)
  (let ((ref ""))
    (if (not (or (if (string-match "Manual Entry\\|\\*man "
				   (buffer-name (current-buffer)))
		     (progn (and (boundp 'man-path) man-path
				 (setq ref (hpath:symlink-referent man-path)))
			    t))
		 (if (hypb:buffer-file-name)
		     (string-match "/man/" (setq ref (hpath:symlink-referent
						      (hypb:buffer-file-name)))))))
	(setq ref nil)
      (or (setq ref (or (smart-man-file-ref)
			(smart-man-c-routine-ref)))
	  (save-excursion
	    (let ((opoint (point))
		  (case-fold-search))
	      (and
	       (re-search-backward "^[.A-Z]" nil t)
	       (looking-at
		"\\(\\.SH[ \t]+\\)?\\(SEE ALSO\\|NAME\\|PACKAGES USED\\)")
	       (progn (goto-char opoint)
		      (skip-chars-backward "-_a-zA-Z0-9?.(")
		      (let ((start (point)))
			(skip-chars-forward "-_a-zA-Z0-9?.()")
			(setq ref (buffer-substring start (point)))
			;; Leave only one char within ref parens
			(if ref
			    (if (string-match "(\\(.\\)\\(.+\\))" ref)
				(setq ref (concat (substring ref 0 (match-end 1))
						  "\)")))))))))))
    (cond ((equal ref "") nil)
	  ((stringp ref) (list 'manual-entry ref))
	  (t ref))))

(defun smart-man-c-routine-ref ()
  "Return form to jump to the definition of the named C function at point.
Valid sections within the man page are: ROUTINES, MACROS or FUNCTIONS.
Uses (smart-tags-file-list) function to determine the tags file from which to
locate the definition."
  (let ((ref)
	(opoint (point))
	(case-fold-search))
    (save-excursion
      (and (re-search-backward "^[.A-Z]" nil t)
	   (looking-at "^\\(FUNCTIONS\\|ROUTINES\\|MACROS\\)[ \t\n\r]")
	   (progn (goto-char opoint)
		  (skip-chars-backward "_~<>:a-zA-Z0-9(")
		  (if (or (looking-at "\\([_~<>:a-zA-Z0-9]+\\)[ \t\n\r]*(")
			  (looking-at "\\([_~<:A-Z][_<>:A-Z0-9]+\\)"))
		      (setq ref (buffer-substring
				 (match-beginning 1) (match-end 1)))))))
    (if ref
	(list 'smart-tags-display ref nil
	      (smart-tags-file-list (or (and (boundp 'Man-header-file-path)
					     Man-header-file-path)
					(and (boundp 'man-path) man-path)))))))

(defun smart-man-file-ref ()
  "Return form to eval to display file whose name is at point.
Enabled within a FILES man page section.
If not on a file name, returns nil."
  (let ((ref)
	(opoint (point))
	(case-fold-search))
    (save-excursion
      (and (re-search-backward "^[.A-Z]" nil t)
	   (looking-at "^FILES[ \t\n\r]")
	     (progn (goto-char opoint)
		    (skip-chars-backward "^ \t")
		    (if (looking-at "/[^ \t\n\r]+")
			(setq ref (buffer-substring
				   (match-beginning 0) (match-end 0)))))))
    (if ref
	(list (if (br-in-browser)
		  'find-file 'find-file-other-window)
	      ref))))

;;; ************************************************************************
;;; smart-org functions
;;; ************************************************************************

(defun smart-org-agenda-item-action ()
  "Action/Assist Key action when point is on an Org Agenda item.
On Action Key press, invoke Agenda's <return> key binding, typically
`org-agenda-switch-to'.  On Assist Key press, display `smart-org' help."
  (hsys-org-set-ibut-label (cons (line-beginning-position)
				 (line-end-position)))
  (if assist-flag
      (hact (lambda () (describe-function 'smart-org)))
    ;; Execute Agenda's {RET} binding using Hyperbole display setting
    (cl-letf (((symbol-function 'pop-to-buffer-same-window)
	       #'hpath:display-buffer))
      (hact (lambda () (call-interactively (key-binding (kbd "RET"))))))))

(defun smart-org ()
  "Follow Org mode references, cycle outline visibility and execute code blocks.
Active when `hsys-org-enable-smart-keys' is non-nil,

When the Action Key is pressed and `hsys-org-enable-smart-keys' is t:

  1. On an Org tag, call `hsys-org-agenda-tags-p' and an
     appropriate view tags function based on current directory or
     buffer name, matching to the tag.  If on a colon between
     tags, match to all tags on the line.

  2. On an Org todo keyword, cycle through the keywords in that
     set or if final done keyword, remove it.

  3. On an Org agenda view item, jump to the item for editing.

When the Action Key is pressed and `hsys-org-enable-smart-keys' is
either `t' or `:buttons':

  4. Within a radio or internal target or a link to it, jump between
     the target and the first link to it, allowing two-way navigation.

  5. On another internal link in an Org mode file, jump to its referent.

  6. On an Org mode external link, jump to its referent.

  7. On a Hyperbole button, activate the button.

  8. With point on the :dir path of a code block definition, display the
     directory given by the path.

  9. With point on any #+BEGIN_SRC, #+END_SRC, #+RESULTS, #+begin_example
     or #+end_example header, execute the code block via the Org mode
     standard binding of {\\`C-c' \\`C-c'}, (`org-ctrl-c-ctrl-c').
  
 10. With point on an Org mode heading, cycle the view of the subtree at
     point.

 11. In any other context besides the end of a line, invoke the Org mode
     standard binding of {M-RET}, (`org-meta-return').

When the Assist Key is pressed, it behaves just like the Action Key except
in these contexts:

  1. On an Org tag, call `hsys-consult-org-grep-tags-p' and an
     appropriate consult grep function based on current directory
     or buffer name, prompting with and matching to the tag.  If on a
     colon between tags, prompt and match to all tags on the line.

  2. On an Org todo keyword, move to the first todo keyword in the next
     set, if any.

  3. On an Org mode link or agenda view item, display Hyperbole
     context-sensitive help.

  4. On a Hyperbole button, perform the Assist Key function, generally
     showing help for the button.

  5. With point on the :dir value of a code block definition, display
     a help summary of this implicit directory button.

  6. With point on any #+BEGIN_SRC, #+END_SRC, #+RESULTS, #+begin_example
     or #+end_example header, remove source block results.

  7. Not on a Hyperbole button but on an Org mode heading, cycle
     through views of the whole buffer outline.

To disable ALL Hyperbole support within Org major and minor modes, set the
custom option `hsys-org-enable-smart-keys' to nil.  Then in Org modes, this
will invoke `org-meta-return'.

Org links may be used outside of Org mode buffers.  Such links are
handled by the separate implicit button type, `org-link-outside-org-mode'."
  (when (funcall hsys-org-mode-function)
    (let (start-end)
      (cond ((not hsys-org-enable-smart-keys)
	     (when (hsys-org-meta-return-shared-p)
	       (hact 'hsys-org-meta-return))
	     ;; Ignore any further Smart Key non-Org contexts
	     t)
	    ((eq hsys-org-enable-smart-keys t)
	     (cond ((setq hkey-value
			  (if (not assist-flag)
			      (hsys-org-agenda-tags-p)
			    (hsys-consult-org-grep-tags-p)))
		    (hact hkey-value))
		   ((or (hsys-org-todo-at-p)
			(eq last-command #'org-todo))
		    (if (not assist-flag)
			(hact 'hsys-org-todo-cycle)
		      (hact 'hsys-org-todo-set-cycle)))
		   ((hsys-org-agenda-item-at-p)
		    (smart-org-agenda-item-action)
		    ;; Ignore any further Smart Key non-Org contexts
		    t)
		   ((hsys-org-radio-target-def-at-p)
		    (hact 'org-radio-target-link)
		    t)
		   ((setq start-end (hsys-org-internal-target-def-at-p))
		    (hsys-org-set-ibut-label start-end)
		    (hact 'org-internal-target-link)
		    t)
		   ((setq start-end (hsys-org-link-at-p))
		    (if (not assist-flag)
			(progn (hsys-org-set-ibut-label start-end)
			       (hact 'org-link))
		      (hact 'hkey-help))
		    t)
		   ((hbut:at-p)
		    ;; Fall through until Hyperbole button context and
		    ;; activate normally.
		    nil)
		   ((or (hsys-org-src-block-start-at-p)
			(save-excursion (forward-line 0)
					(or (looking-at org-babel-result-regexp)
					    (looking-at "^[	 ]*#\\+\\(end_src\\|begin_example\\|end_example\\)"))))
		    (hact (lambda ()
			    (save-excursion
			      (unless (hsys-org-src-block-start-at-p)
				(re-search-backward org-babel-src-block-regexp nil t))
			      (cond ((not assist-flag)
				     (org-ctrl-c-ctrl-c))
				    ((org-babel-where-is-src-block-result)
				     (org-babel-remove-result)
				     (message "Code block results removed."))
				    (t (message "No results to remove for this code block."))))))
		    t)
		   ((hsys-org-heading-at-p)
		    (if (not assist-flag)
			(hact 'hsys-org-cycle)
		      (hact 'hsys-org-global-cycle))
		    t)
		   ((equal (hsys-org-get-value :language) "python")
		    (setq hkey-value (smart-python-at-tag-p))
		    (hact 'smart-python hkey-value))
		   (t
		    ;; Continue with any further Smart Key non-Org contexts
		    nil)))
	    ((memq hsys-org-enable-smart-keys '(:buttons buttons))
	     (cond ((hsys-org-radio-target-def-at-p)
		    (hact 'org-radio-target-link)
		    t)
		   ((setq start-end (hsys-org-link-at-p))
		    (if (not assist-flag)
			(progn (hsys-org-set-ibut-label start-end)
			       (hact 'org-link))
		      (hact 'hkey-help))
		    t)
 		   ((hbut:at-p)
		    ;; Fall through until Hyperbole button context and
		    ;; activate normally.
		    nil)
		   ((and (apply #'derived-mode-p '(org-mode))
			 (equal (hsys-org-get-value :language) "python"))
		    (setq hkey-value (smart-python-at-tag-p))
		    (hact 'smart-python hkey-value ''next-tag))
		   ((apply #'derived-mode-p '(org-mode))
		    (when (hsys-org-meta-return-shared-p)
		      (hact 'hsys-org-meta-return))
		    ;; Ignore any further Smart Key non-Org contexts
		    t)
		   ((hsys-org-agenda-item-at-p)
		    (smart-org-agenda-item-action)
		    ;; Ignore any further Smart Key non-Org contexts
		    t)
		   ;; Ignore any further Smart Key non-Org contexts
		   (t)))
	    (t
	     ;; hsys-org-enable-smart-keys is set to t, so try other Smart
	     ;; contexts
	     nil)))))

(defun smart-org-bob-and-non-heading-p ()
  "Check whether {TAB} should globally `org-cycle' at point."
  (and (bobp)
       (hypb:buffer-file-name)
       (derived-mode-p 'org-mode)
       (not (org-at-heading-p))
       (member (hypb:buffer-file-name)
	       (hpath:expand-list hsys-org-cycle-bob-file-list))))

;;; ************************************************************************
;;; smart-outline functions
;;; ************************************************************************

;; The functions in this section may require InfoDock's version of outline.el
;; for smart-outline-cut to work.

(defvar smart-outline-cut nil
  "Non-nil means outline region was cut and is ready to be pasted at point.")

(eval-after-load "outline"
  '(mapc (lambda (hook-var)
	   (add-hook hook-var (lambda ()
				(make-local-variable 'smart-outline-cut)
				;; Non-nil means outline region was cut
				;; and is available to be pasted at point.
				(setq smart-outline-cut nil))))
	 '(outline-mode-hook outline-minor-mode-hook)))

(defun smart-outline-level ()
  "Return outline level if point is on a line that begins with `outline-regexp'.
If on other line return 0."
  (save-excursion
    (beginning-of-line)
    (if (looking-at outline-regexp)
	(funcall outline-level)
      0)))

(defun smart-outline ()
  "Collapse, expand, and move outline entries.
Invoked via a key press when in `outline-mode'.  It assumes that
its caller has already checked that the key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) after an outline heading has been cut via the Action Key, then paste the
     cut heading at point;
 (2) at the end of buffer, show all buffer text
 (3) at the beginning of a heading line, cut the headings subtree from the
     buffer;
 (4) on a heading line but not at the beginning or end, if headings subtree is
     hidden then show it, otherwise hide it;
 (5) at the end of a line, invoke `action-key-eol-function', typically to
     scroll up a windowful."

  (interactive)
  (cond (smart-outline-cut
	 (setq smart-outline-cut nil) (yank))
	((smart-eobp) (outline-show-all))
	((and (bolp) (looking-at outline-regexp))
	 (setq smart-outline-cut t)
	 (kill-region
	  (point)
	  (or (outline-get-next-sibling)
	      ;; Skip past start of current entry
	      (progn (re-search-forward outline-regexp nil t)
		     (smart-outline-to-entry-end t)))))
	((eolp)
	 (funcall action-key-eol-function))
	((zerop (smart-outline-level))
	 nil)
	;; On an outline heading line but not at the start/end of line.
	((smart-outline-subtree-hidden-p)
	 (outline-show-subtree))
	(t (outline-hide-subtree))))


(defun smart-outline-assist ()
  "Collapse, expand, and move outline entries.
Invoked via an assist key press when in `outline-mode'.  It assumes that
its caller has already checked that the assist key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If assist key is pressed:
 (1) after an outline heading has been cut via the action key, allow
     multiple pastes throughout the buffer (last paste should be done with
     the Action Key, not the Assist Key);
 (2) at the end of buffer, hide all bodies in buffer;
 (3) at the beginning of a heading line, cut the current heading (sans
     subtree) from the buffer;
 (4) on a heading line but not at the beginning or end, if heading body is
     hidden then show it, otherwise hide it;
 (5) at the end of a line, invoke `assist-key-eol-function', typically to
     scroll down a windowful."

  (interactive)
  (cond (smart-outline-cut (yank))
	((smart-eobp) (outline-hide-body))
	((and (bolp) (looking-at outline-regexp))
	 (setq smart-outline-cut t)
	 (kill-region (point)
		      ;; Skip past start of current entry
		      (progn (re-search-forward outline-regexp nil t)
			     (smart-outline-to-entry-end))))
	((eolp)
	 (funcall assist-key-eol-function))
	((zerop (smart-outline-level))
	 nil)
	;; On an outline heading line but not at the start/end of line.
	((smart-outline-subtree-hidden-p)
	 (outline-show-entry))
	(t (outline-hide-entry))))

(defun smart-outline-to-entry-end (&optional include-sub-entries)
  "Move point past the end of the current entry, if any.
With optional INCLUDE-SUB-ENTRIES non-nil, move to the end of the
entire subtree.  Return final point."
  (if (not include-sub-entries)
      (outline-next-heading)
    (condition-case ()
	(progn (outline-end-of-subtree)
	       (goto-char (1+ (point))))
      (error "")))
  (point))

(defun smart-outline-subtree-hidden-p ()
  "Return t if at least initial subtree of heading is hidden, else nil."
  (and (outline-on-heading-p t)
       (outline-invisible-in-p
	(point) (or (save-excursion (re-search-forward "[\n\r]" nil t)) (point)))))

(defun smart-outline-char-invisible-p (&optional pos)
  "Return t if the character after point is invisible/hidden, else nil.
With optional POS, use that instead of point."
  (or pos (setq pos (point)))
  (when (or
	 ;; New-style Emacs outlines with invisible properties to hide lines
	 (outline-invisible-p pos)
	 (delq nil (mapcar (lambda (o) (overlay-get o 'invisible))
			   (overlays-at (or pos (point)))))
	 ;; Old-style Emacs outlines using \r (^M) characters to hide lines
	 (and selective-display (eq (following-char) ?\r)))
    t))

;;; ************************************************************************
;;; smart-push-button functions
;;; ************************************************************************

;; Emacs push button support
(defun smart-push-button (&optional _pos _use-mouse-action)
  "Activate an Emacs `push-button', including text-property follow-link buttons.
Button is at optional POS or at point.  USE-MOUSE-ACTION prefers
mouse-action to action property."
  (or
   ;; Handle Emacs text-property buttons which don't work with 'button-activate'.
   ;; Use whatever command is bound to RET within the button's keymap.
   (call-interactively (or (lookup-key (get-text-property (point) 'keymap) (kbd "RET"))
			   (lambda () (interactive) nil)))
   ;; non-text-property push-buttons
   (push-button nil (mouse-event-p last-command-event))))

(defun smart-push-button-help (&optional pos use-mouse-action)
  "Show help about a push button's action.
Button is at optional POS or at point.  USE-MOUSE-ACTION prefers
mouse-action to action property."
  (let* ((button (button-at (or pos (point))))
	 (action (or (and use-mouse-action (button-get button 'mouse-action))
		     (button-get button 'action)))
	 ;; Ensure these do not invoke with-output-to-temp-buffer a second time.
	 (temp-buffer-show-hook)
	 (temp-buffer-show-function))
    (if (functionp action)
	(describe-function action)
      (hkey-help t))))

;;; ************************************************************************
;;; smart-tar functions
;;; ************************************************************************

(defun smart-tar ()
  "Use a single key or mouse key to manipulate tar file entries.

Invoked via a key press when in `tar-mode'.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) within an entry line, the selected file/directory is displayed for
     editing in the other window;
 (2) on or after the last line in the buffer, if any deletes are to be
     performed, they are executed after user verification, otherwise, this
     tar file browser is quit."

  (interactive)
  (cond ((last-line-p)
	 (let (flagged)
	   (save-excursion
	     (goto-char 1)
	     (setq flagged (re-search-forward "^D" nil t)))
	   (if flagged
	       (tar-expunge)
	     (kill-buffer nil))))
	(t (tar-extract-other-window))))

(defun smart-tar-assist ()
  "Use a single assist key or mouse assist key to manipulate tar file entries.

Invoked via an assist key press when in `dired-mode'.  It assumes that its
caller has already checked that the assist key was pressed in an appropriate
buffer and has moved the cursor there.

If assist key is pressed:
 (1) on an entry line, the current entry is marked for deletion;
 (2) on or after the last line in the buffer, all delete marks on all entries
     are undone."

  (interactive)
  (cond ((last-line-p)
	 (tar-unflag (- (count-lines (point-min) (point-max))))
	 (goto-char (point-max)))
	(t (tar-flag-deleted 1))))

;;; ************************************************************************
;;; smart-todotxt functions
;;; ************************************************************************

(defun smart-todotxt ()
  "Use a single key or mouse key to manipulate `todotxt' items.

If key is pressed:
 (1) at the end of buffer, bury buffer
 (2) on a todo item, toggle the completion"
  (interactive)
  (cond ((smart-eobp) (todotxt-bury))
	(t (todotxt-complete-toggle))))

(defun smart-todotxt-assist ()
  "Use a single assist key or mouse assist key to manipulate `todotxt' items.

If key is pressed:
 (1) at the end of buffer, archive completed items
 (2) on a todo item, edit it"

  (interactive)
  (cond ((smart-eobp) (todotxt-archive))
	(t (todotxt-edit-item))))

;;;###autoload
(defun smart-eobp ()
  "Return t if point is past the last visible non-whitespace buffer line."
  (and (or (and (eobp) (bolp))
	   ;; On a blank line and nothing but whitespace until eob
	   (save-excursion
	     (beginning-of-line)
	     (looking-at "[ \t\n\r\f]+\\'")))
       (or (not (smart-outline-char-invisible-p))
	   (not (smart-outline-char-invisible-p (1- (point)))))))

(defun smart-eolp ()
  "Return t if point is at the end of a visible line.
This includes the last buffer line if it contains any non-whitespace
characters.  It excludes a blank line at the end of the buffer."
  ;; smart-helm handles eol for helm buffers
  (unless (or (and (smart-helm-alive-p) (equal (helm-buffer-get) (buffer-name)))
	      ;; Allow for org global cycling at start of buffer on a
	      ;; non-heading line in Hyperbole doc files when
	      ;; displayed from Hyperbole menu items.
	      (smart-org-bob-and-non-heading-p)
	      ;; If there is a flymake diagnostic issue at eol,
	      ;; drop through this clause to handle it later.
	      (and (featurep 'hsys-flymake)
		   (boundp 'flymake-mode)
		   flymake-mode
		   (eolp)
		   (hsys-flymake-get-issue-at-position)))
    (if (eq major-mode 'kotl-mode)
	(and (not (kotl-mode:eobp)) (kotl-mode:eolp t))
      (and (not (smart-eobp)) (eolp)
	   (or (not (smart-outline-char-invisible-p))
	       (not (smart-outline-char-invisible-p (1- (point)))))))))

(provide 'hui-mouse)

;;; hui-mouse.el ends here
