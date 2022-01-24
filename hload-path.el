;;; hload-path.el --- GNU Hyperbole load-path setup  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    29-Jun-16 at 14:39:33
;; Last-Mod:     24-Jan-22 at 00:18:33 by Bob Weiner
;;
;; Copyright (C) 1992-2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
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
It must end with a directory separator character.")

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

(provide 'hload-path)

;;; hload-path.el ends here
