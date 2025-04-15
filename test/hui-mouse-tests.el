;;; hui-mouse-tests.el --- unit tests for hui-mouse -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    15-Mar-25 at 22:39:37
;; Last-Mod:     15-Apr-25 at 13:13:21 by Mats Lidell
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

;; !! FIXME: Add more predicate cases from hkey-alist.
(ert-deftest hui-mouse-tests--hkey-alist ()
  "Verify that given predicate values triggers the proper action."
  ;; Treemacs
  (let ((major-mode 'treemacs-mode))
    (should (equal (hkey-actions)
                   (cons '(smart-treemacs) '(smart-treemacs)))))

  ;; dired-sidebar-mode
  (let ((major-mode 'dired-sidebar-mode))
    (should (equal (hkey-actions)
                   (cons '(smart-dired-sidebar) '(smart-dired-sidebar)))))

  ;; Vertico
  (defvar vertico-mode)
  (let ((ivy-mode nil)
        (vertico-mode t))
    (mocklet ((vertico--command-p => t))
      (should (equal (hkey-actions)
		     (cons '(funcall (lookup-key vertico-map (kbd "M-RET")))
			   '(funcall (lookup-key vertico-map (kbd "M-RET"))))))))
  )

(provide 'hui-mouse-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hui-mouse-tests.el ends here
