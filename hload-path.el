;;; hload-path.el --- GNU Hyperbole load-path and autoload early initializations  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    29-Jun-16 at 14:39:33
;; Last-Mod:     25-Jul-22 at 17:50:26 by Mats Lidell
;;
;; Copyright (C) 1992-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;;;###autoload
(defvar hyperb:microsoft-os-p
  (memq system-type '(ms-windows windows-nt ms-dos win32))
  "Non-nil iff Hyperbole is running under a Microsoft OS but not for WSL.
WSL is Windows Subsystem for Linux.
Use `hyperb:wsl-os-p' to test if running under WSL.")

;;;###autoload
(defvar hyperb:wsl-os-p
  (and (eq system-type 'gnu/linux) (executable-find "wsl.exe") t)
  "T iff Hyperbole is running under Microsoft Windows Subsystem for Linux (WSL).")

;;; ************************************************************************
;;; Hyperbole Directory Setting (dynamically computed)
;;; ************************************************************************

(defconst hyperb:dir (or (file-name-directory
			  (or (and (stringp load-file-name) load-file-name)
			      (locate-file "hmouse-tag.el" load-path)
			      (hyperb:path-being-loaded)
			      ""))
			 (error
			  "(Hyperbole): Failed to set hyperb:dir.  Try setting it manually"))
  "Directory where the Hyperbole executable code is kept.
Valid values end with a directory separator character.")

;; Add hyperb:dir to load-path so other Hyperbole libraries can be
;; found unless it is already there since the Emacs Package Manager
;; may have already added it.
(add-to-list 'load-path (directory-file-name hyperb:dir))

;;; ************************************************************************
;;; Koutliner mode and file suffix importation settings
;;; ************************************************************************

;; Perform Koutliner initializations.

(add-to-list 'load-path (expand-file-name "kotl" hyperb:dir))
;; Invoke kotl-mode for files ending in ".kotl".
;; Also allow ".kot" for DOS and Windows users.
(add-to-list 'auto-mode-alist '("\\.kotl?\\'" . kotl-mode))

;;; ************************************************************************
;;; Hyperbole test importation settings
;;; ************************************************************************

(add-to-list 'load-path (expand-file-name "test" hyperb:dir))

;; Ensure final name (after resolving all links) of hyperb:dir is
;; used after setting up load-path; otherwise, Hyperbole may fail
;; to substitute this as a variable into link path buttons.
(when (stringp hyperb:dir)
  (setq hyperb:dir (file-truename hyperb:dir)))

;;; ************************************************************************
;;; Autoloads
;;; ************************************************************************

(defmacro hyperb:with-suppressed-warnings (warnings &rest body)
  "Like `progn', but prevents compiler WARNINGS in BODY.

Defined here for elpa build compatibility which uses Emacs 26 and
does not include `with-suppressed-warnings'.

WARNINGS is an associative list where the first element of each
item is a warning type, and the rest of the elements in each item
are symbols they apply to.  For instance, if you want to suppress
byte compilation warnings about the two obsolete functions `foo'
and `bar', as well as the function `zot' being called with the
wrong number of parameters, say

\(with-suppressed-warnings ((obsolete foo bar)
                           (callargs zot))
  (foo (bar))
  (zot 1 2))

The warnings that can be suppressed are a subset of the warnings
in `byte-compile-warning-types'; see the variable
`byte-compile-warnings' for a fuller explanation of the warning
types.  The types that can be suppressed with this macro are
`free-vars', `callargs', `redefine', `obsolete',
`interactive-only', `lexical', `mapcar', `constants' and
`suspicious'.

For the `mapcar' case, only the `mapcar' function can be used in
the symbol list.  For `suspicious', only `set-buffer' can be used."

  (declare (debug (sexp &optional body)) (indent 1))
  (if (fboundp 'with-suppressed-warnings)
      `(with-suppressed-warnings ,warnings ,@body)
    `(with-no-warnings ,@body)))
;; New autoload generation function defined only in Emacs 28

(defalias 'hload-path--make-directory-autoloads
  (cond ((fboundp 'loaddefs-generate)
         #'loaddefs-generate)
        ((fboundp #'make-directory-autoloads)
         #'make-directory-autoloads)
        (t
         #'hload-path--internal-make-directory-autoloads)))

(defun hload-path--internal-make-directory-autoloads (dir output-file)
  "Update autoload definitions for Lisp files in the directories DIRS.
DIR can be either a single directory or a list of
directories.  (The latter usage is discouraged.)

The autoloads will be written to OUTPUT-FILE.  If any Lisp file
binds ‘generated-autoload-file’ as a file-local variable, write
its autoloads into the specified file instead.

The function does NOT recursively descend into subdirectories of the
directory or directories specified."
  ;; Don't use a 'let' on this next line or it will fail.
  (setq generated-autoload-file output-file)
  (hyperb:with-suppressed-warnings ((obsolete update-directory-autoloads))
    (update-directory-autoloads dir)))

;; Menu items could call this function before Info is loaded.
(autoload 'Info-goto-node   "info" "Jump to specific Info node."  t)

;; Auto-autoload doesn't work for next item because it is defined
;; within a condition-case, so autoload it here.
(autoload 'Vm-init          "hvm"  "Initializes Hyperbole Vm support." t)

(defun hyperb:autoloads-exist-p ()
  "Return t if all Hyperbole autoload files exist or nil otherwise."
  (and (file-readable-p (expand-file-name "hyperbole-autoloads.el" hyperb:dir))
       (file-readable-p (expand-file-name "kotl-autoloads.el"
					  (expand-file-name "kotl" hyperb:dir)))))

(defun hyperb:maybe-generate-autoloads ()
  "Ensure Hyperbole *-autoload.el files are already generated or generate them.
This is used only when running from git source and not a package release."
  (unless (hyperb:autoloads-exist-p)
    (hyperb:generate-autoloads)))

(defun hyperb:generate-autoloads ()
  "Renerate Hyperbole *-autoloads.el files whether they already exist or not."
  (let* ((default-directory hyperb:dir)
	 (backup-inhibited t)
	 (find-file-hook) ;; Prevent header insertion
	 (al-file (expand-file-name "hyperbole-autoloads.el")))
    ;; (make-local-variable 'generated-autoload-file)
    (with-current-buffer (find-file-noselect al-file)
      (hload-path--make-directory-autoloads "." al-file))
    (setq al-file (expand-file-name "kotl/kotl-autoloads.el"))
    (with-current-buffer (find-file-noselect al-file)
      (hload-path--make-directory-autoloads "." al-file)))
  (unless (hyperb:autoloads-exist-p)
    (error (format "Hyperbole failed to generate autoload files; try running 'make src' in a shell in %s" hyperb:dir))))

(defun hyperb:maybe-load-autoloads ()
  "Load Hyperbole autoload files that have not already been loaded."
  (let* ((default-directory hyperb:dir)
	 (hypb-autoloads (expand-file-name "hyperbole-autoloads.el"))
	 (kotl-autoloads (expand-file-name "kotl/kotl-autoloads.el")))
    (unless (featurep 'hyperbole-autoloads)
      (when (file-readable-p hypb-autoloads)
        (load-file hypb-autoloads)))
    (unless (featurep 'kotl-autoloads)
      (when (file-readable-p kotl-autoloads)
        (load-file kotl-autoloads)))))

;; Ensure *-autoloads.el files are already generated or generate them.
;; Then ensure they are loaded.
(hyperb:maybe-generate-autoloads)
(hyperb:maybe-load-autoloads)

(provide 'hload-path)

;;; hload-path.el ends here
