;;; hy-package.el --- Hyperbole package.el installation and configuration instructions  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    15-Jul-26 at 12:28:58
;; Last-Mod:     30-Sep-26 at 12:37:33 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2026  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; ************************************************************************
;;; Requirements
;;; ************************************************************************

(require 'package)

;;; ************************************************************************
;;; Section 1: package.el setup
;;; ************************************************************************

;;; ========================================================================
;;; NOTE: This section is only if you have not yet setup the package.el
;;;       package manager.  If you have, ignore this and skip to "#Section 2"
;;;       for the Hyperbole `use-package' recipe.
;;; ========================================================================

;; Step 1: Add the function definition below near the top of your
;; "~/.emacs" or "~/.emacs.d/early-init.el" file.

;; Step 2: Add a call to the function below its definition:
;;         (setup-package)

(defun setup-package ()
  (require 'package)
  (setq package-enable-at-startup nil) ;; Prevent double loading of libraries
  (add-to-list 'package-archives
	       ;; Leave only one of the following two lines uncommented
	       '("elpa-devel" . "https://elpa.gnu.org/devel/"))
	       ;; '("elpa" . "https://elpa.gnu.org/packages/"))
  (unless (and (boundp 'package--initialized) package--initialized)
    (package-initialize))
  ;; To ensure you have the latest index of packages, you'll have to
  ;; uncomment the next line.  It is commented because retrieving the
  ;; list is slow.
  ;; (package-refresh-contents)
  )

;;; End
