;;; hui-mouse-tests.el --- unit tests for hui-mouse -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    15-Mar-25 at 22:39:37
;; Last-Mod:     16-Mar-25 at 01:14:39 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2025 Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Unit tests for "../hui-mouse.el".

;;; Code:

(require 'ert)
(require 'el-mock)

(defun hui-mouse-tests--hkey-get-action ()
  "Return the action given by the predicate that is true.
See `hkey-execute' for where this type of lookup is used."
  (let ((hkey-forms hkey-alist)
	pred-value hkey-actions hkey-form pred)
    (while (and (null pred-value) (setq hkey-form (car hkey-forms)))
      (if (setq hkey-actions (cdr hkey-form)
	        pred (car hkey-form)
		pred-value (hypb:eval-debug pred))
          nil
	(setq hkey-forms (cdr hkey-forms))))
    hkey-actions))

;; FIXME: Add more predicate cases from hkey-alist.
(ert-deftest hui-mouse-tests--hkey-alist ()
  "Verify that given predicate values triggers the proper action."
  ;; Treemacs
  (let ((major-mode 'treemacs-mode))
    (should (equal (hui-mouse-tests--hkey-get-action)
                   (cons '(smart-treemacs) '(smart-treemacs)))))

  ;; dired-sidebar-mode
  (let ((major-mode 'dired-sidebar-mode))
    (should (equal (hui-mouse-tests--hkey-get-action)
                   (cons '(smart-dired-sidebar) '(smart-dired-sidebar)))))

  ;; Vertico
  (defvar ivy-mode)
  (defvar vertico-mode)
  (let ((ivy-mode nil)
        (vertico-mode t))
    (mocklet (((minibuffer-depth) => 1)
              ((selected-window) => t)
              ((minibuffer-window) => t)
              (vertico--command-p => t))
      (should (equal (hui-mouse-tests--hkey-get-action)
                     (cons '(vertico-exit-input) '(vertico-exit-input)))))))

(provide 'hui-mouse-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hui-mouse-tests.el ends here
