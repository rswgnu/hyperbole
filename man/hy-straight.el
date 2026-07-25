;;; hy-straight.el --- Hyperbole Straight installation and configuration instructions  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    15-Jul-26 at 12:28:58
;; Last-Mod:     25-Jul-26 at 22:25:45 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2026  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; ************************************************************************
;;; Section 1: Straight setup
;;; ************************************************************************

;;; ========================================================================
;;; NOTE: This section is only if you have not yet setup the Straight
;;;       package manager.  If you have, ignore this and skip to "#Section 2"
;;;       for the Hyperbole `straight-use-package' recipe.
;;; ========================================================================

;; Step 1: Add the function definition below near the top of your
;; "~/.emacs" or "~/.emacs.d/early-init.el" file.

;; Step 2: Add a call to the function below its definition:
;;         (setup-straight)

(defun setup-straight ()
  (when (< emacs-major-version 28)
    (error "Hyperbole requires Emacs 28 or above, not %d"
           emacs-major-version))
  (defvar bootstrap-version)

  ;; Prevent double loading of libraries
  (setq package-enable-at-startup nil)

  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el"
                           user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;;; ************************************************************************
;;; Section 2: Straight Hyperbole installation and configuration
;;; ************************************************************************

;; Step 1: Add the following expression to your "~/.emacs" or
;; "~/.emacs.d/init.el" file.

(straight-use-package
 '(hyperbole
   :host nil

   :repo "https://git.savannah.gnu.org/git/hyperbole.git"

   :config
   (progn
     (hywiki-mode :all)
     (unless hywiki-mode
       (hyperbole-mode 1)))

   :bind
   (("M-RET" . hkey-either))

   :custom
   (hsys-org-enable-smart-keys t)))

;;; End
