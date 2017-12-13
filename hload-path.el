;;; hload-path.el --- GNU Hyperbole load-path setup
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    29-Jun-16 at 14:39:33
;;
;; Copyright (C) 1992-2017  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;;; Support mouse handling and Koutlines under GNU Emacs V19 or higher.
;;;
;;;###autoload
(defconst hyperb:emacs-p
  (and (not (featurep 'xemacs)) emacs-version)
  "Version string if running under GNU Emacs, else nil")

;;; ************************************************************************
;;; Hyperbole Directory Setting (dynamically computed)
;;; ************************************************************************

(defconst hyperb:dir (or (file-name-directory
			  (or (and (stringp load-file-name) load-file-name)
			      (hyperb:path-being-loaded)
			      (locate-file "hmouse-tag.el" load-path)
			      ""))
			 (error
			  "(Hyperbole): Failed to set hyperb:dir.  Try setting it manually."))
  "Directory where the Hyperbole executable code is kept.
It must end with a directory separator character.")

;; Ensure final name (after resolving all links) of hyperb:dir is
;; used; otherwise, Hyperbole may fail to substitute this as a
;; variable into link path buttons.
(if (stringp hyperb:dir) (setq hyperb:dir (file-truename hyperb:dir)))

;; Add hyperb:dir to load-path so other Hyperbole libraries can be
;; found unless it is already there since the Emacs Package Manager
;; may have already added it.
(unless (member (directory-file-name hyperb:dir) load-path)
  (add-to-list 'load-path hyperb:dir))

;;; ************************************************************************
;;; Koutliner mode and file suffix importation settings
;;; ************************************************************************

;; Perform Koutliner initializations.

(add-to-list 'load-path (expand-file-name "kotl/" hyperb:dir))
;; Invoke kotl-mode for files ending in ".kotl".
;; Also allow ".kot" for DOS and Windows users.
(setq auto-mode-alist (cons '("\\.kotl$\\|\\.kot$" . kotl-mode)
			    auto-mode-alist))

(provide 'hload-path)

;;; hload-path.el ends here
