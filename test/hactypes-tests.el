;;; hactypes-test.el -- Ert tests for hactypes        -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-Jan-21 at 12:00:00
;; Last-Mod:      1-Jun-25 at 23:31:35 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'el-mock)
(require 'hactypes)
(require 'hy-test-helpers "test/hy-test-helpers")

(ert-deftest display-boolean-true-test ()
  (ert-with-message-capture cap
    (should (actypes::display-boolean t))
    (hy-test-helpers:should-last-message "Result = t; Boolean value = True" cap)))

(ert-deftest display-boolean-false-test ()
  (ert-with-message-capture cap
    (should (actypes::display-boolean nil))
    (hy-test-helpers:should-last-message "Result = nil; Boolean value = False" cap)))

(ert-deftest hactypes-tests--link-to-Info-index-item ()
  "Verify `actypes::link-to-Info-index-item'."
  (should-error (actypes::link-to-Info-index-item "wrong-format") :type 'error)
  (should-error (actypes::link-to-Info-index-item "(unknown-file)unknown-index-item") :type 'error)
  (mocklet (((id-info-item "(infofile)index-item") => t))
    (actypes::link-to-Info-index-item "(infofile)index-item"))
  (unwind-protect
      (progn
        (actypes::link-to-Info-index-item "(hyperbole)hyperb:dir")
        (should (string-prefix-p "*info*" (buffer-name)))
        (should (string= "hyperbole" (file-name-nondirectory Info-current-file)))
        (should (string= "Documentation" Info-current-node))
        (should (looking-at-p "The Hyperbole Manual is a reference manual, not a simple introduction\\.")))
    (kill-matching-buffers "^\\*info\\*" nil t)))

(provide 'hactypes-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hactypes-tests.el ends here
