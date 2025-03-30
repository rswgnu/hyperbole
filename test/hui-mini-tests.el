;;; hui-mini-tests.el --- Unit test for hui-mini -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    30-Jan-25 at 22:39:37
;; Last-Mod:      4-Mar-25 at 17:06:50 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

(require 'hui-mini)
(require 'ert)
(require 'el-mock)

(ert-deftest hui--menu-read-from-minibuffer ()
  "Verify prompt shows proper active selection."
  (defvar menu-string)
  (dolist (v '((nil "None")
               (:buttons "Hyperbole-Buttons-Only")
               (t "All-Hyperbole-Contexts")))
    (let ((hsys-org-enable-smart-keys (car v))
          (menu-string (cadr v)))
      (mocklet (((read-from-minibuffer "" (format "Org M-RET ==%s==" menu-string) hui:menu-mode-map nil t nil nil) => t))
        (should (hui:menu-read-from-minibuffer "" (format "Org M-RET %s" menu-string) hui:menu-mode-map nil t))))))

(provide 'hui-mini-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hui-mini-tests.el ends here
