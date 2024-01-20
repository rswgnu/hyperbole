;;; hsettings-test.el --- one line summary                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    20-Jan-24 at 12:28:01
;; Last-Mod:     20-Jan-24 at 14:25:44 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'el-mock)
(require 'hsettings)

(ert-deftest hsettings-test--hyperbole-web-search ()
  "Verify `hyperbole-web-search´."
  (mocklet (((browse-url "http://www.google.com/search?q=hyperbole") => "return"))
    (should (string= (hyperbole-web-search "google" "hyperbole" nil) "return")))
  (should (string= (hyperbole-web-search "google" "hyperbole" t) "http://www.google.com/search?q=hyperbole"))
  (should-error (hyperbole-web-search "unknown" "hyperbole" nil))
  (should-error (hyperbole-web-search "unknown" "hyperbole" t))

  ;; Jump
  ;; Fails due to being called with argument: See test below.
  ;; (mocklet (((webjump) => "return"))
  ;;   (should (string= (hyperbole-web-search "Jump" "arg" nil) "return")))
  (should (equal (hyperbole-web-search "Jump" "arg" t) '(webjump "arg"))))

(ert-deftest hsettings-test--hyperbole-web-search-webjump-called-with-arg ()
  "Verify `hyperbole-web-search´."
  :expected-result :failed
  ;; Jump
  (mocklet (((webjump) => "return"))
    (should (string= (hyperbole-web-search "Jump" "arg" nil) "return"))))

(provide 'hsettings-test)
;;; hsettings-test.el ends here
