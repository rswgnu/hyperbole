;;; hy-test-dependencies.el --- Dependencies for running the tests  -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    20-Feb-21 at 23:16:00
;; Last-Mod:     24-Jan-22 at 00:40:43 by Bob Weiner
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

(unless (package-installed-p 'el-mock)
  (package-refresh-contents)
  (package-install 'el-mock))

(unless (package-installed-p 'with-simulated-input)
  (package-refresh-contents)
  (package-install 'with-simulated-input))

(provide 'hy-test-dependencies)
;;; hy-test-dependencies.el ends here
