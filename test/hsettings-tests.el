;;; hsettings-tests.el --- one line summary                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    20-Jan-24 at 12:28:01
;; Last-Mod:     16-Aug-25 at 15:34:14 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024-2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'el-mock)
(require 'hsettings)

(ert-deftest hsettings-tests--hyperbole-web-search ()
  "Verify `hyperbole-web-search´."
  (mocklet (((browse-url "http://www.google.com/search?q=hyperbole") => "return"))
    (should (string= (hyperbole-web-search "google" "hyperbole" nil) "return")))
  (should (string= (hyperbole-web-search "google" "hyperbole" t)
		   "http://www.google.com/search?q=hyperbole"))
  (should-error (hyperbole-web-search "unknown" "hyperbole" nil))
  (should-error (hyperbole-web-search "unknown" "hyperbole" t))

  ;; Jump
  (mocklet (((webjump) => "return"))
    (should (string= (hyperbole-web-search "Jump" "arg" nil) "return")))
  (should (equal (hyperbole-web-search "Jump" "arg" t) '(webjump))))

(provide 'hsettings-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hsettings-tests.el ends here
