;;; hy-test-dependencies.el --- Dependencies for running the tests  -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    20-Feb-21 at 23:16:00
;; Last-Mod:     23-Jul-22 at 18:37:43 by Bob Weiner
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Load prerequisites for running the tests.

;;; Code:

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defun hy-test-ensure-package-installed (pkg-symbol)
  (unless (package-installed-p pkg-symbol)
    (package-refresh-contents)
    (package-install pkg-symbol)))

(mapc (lambda (sym) (hy-test-ensure-package-installed sym))
      '(el-mock package-lint with-simulated-input))

(provide 'hy-test-dependencies)
;;; hy-test-dependencies.el ends here
