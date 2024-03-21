;;; hsys-org-tests.el --- hsys-org tests            -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    23-Apr-21 at 20:55:00
;; Last-Mod:     12-Mar-24 at 23:04:29 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Tests for "../hsys-org.el"

;;; Code:

(require 'ert)
(require 'hsys-org)
(if t (require 'el-mock))

(ert-deftest hsys-org:cycle-on-header-cycles-visibility ()
  "Hide an outline header."
  (with-temp-buffer
    (org-mode)
    (insert "* 1\n** 2\n*** 3\n")
    (goto-char 1)
    (should (not (org-check-for-hidden 'headlines)))
    (hsys-org-cycle)
    (should (org-check-for-hidden 'headlines))
    (forward-visible-line 1)
    (should (= (line-number-at-pos) 4))))

(ert-deftest hsys-org:region-with-text-property-value ()
  "Should get the region with the specific text property."
  (with-temp-buffer
    (org-mode)
    (insert "* 1\n** 2\n*** 3\n")
    (goto-char 1)
    (font-lock-ensure)
    (should (equal (hsys-org-region-with-text-property-value 1 'face) '(1 . 4)))))

;; TODO: org-agenda-item-at-p

(ert-deftest hsys-org:block-start-at-p ()
  "Should be t if point is on the start of a block."
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN_BLK\n text\n#+END_BLK\n")
    (goto-char 1)
    (should (hsys-org-block-start-at-p))))

(ert-deftest hsys-org:src-block-start-at-p ()
  "Should be t if point is on the start of a source block."
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN_SRC python\n text\n#+END_SRC\n")
    (goto-char 1)
    (should (hsys-org-src-block-start-at-p))))

(ert-deftest hsys-org:org-link-at-p ()
  "Should be t if point is within an org-link."
  (with-temp-buffer
    (org-mode)
    (insert "[[Link]]\n")
    (goto-char 3)
    (should (hsys-org-link-at-p))))

(ert-deftest hsys-org:org-target-at-p ()
  "Should be non nil if point is within an org-radio-target."
  (with-temp-buffer
    (org-mode)
    (insert "<<<link>>>\n")
    (goto-char 6)
    (org-ctrl-c-ctrl-c)
    (font-lock-ensure)
    (should (hsys-org-target-at-p))))

(ert-deftest hsys-org:org-radio-target-link-at-p ()
  "Should return not nil if point is within an org radio target link."
  (with-temp-buffer
    (org-mode)
    (insert " <<<link>>>\nlink\n")
    (goto-char 6)
    (org-ctrl-c-ctrl-c)
    (font-lock-ensure)
    (goto-char 16)
    (should (equal (hsys-org-radio-target-link-at-p) '(13 . 17)))))

(ert-deftest hsys-org:org-radio-target-def-at-p ()
  "Should return (start . end) iff point is within an org radio target definition."
  (with-temp-buffer
    (org-mode)
    (insert " <<<link>>>\n")
    (goto-char 6)
    (org-ctrl-c-ctrl-c)
    (font-lock-ensure)
    (should (equal (hsys-org-radio-target-def-at-p) '(2 . 12)))))

(ert-deftest hsys-org:org-radio-target-at-p ()
  "Should return (start . end) iff point is within an org radio target definition."
  (with-temp-buffer
    (org-mode)
    (insert " <<<link>>>\nlink\n")
    (goto-char 6)
    (org-ctrl-c-ctrl-c)
    (font-lock-ensure)
    (should (equal (hsys-org-radio-target-at-p) '(2 . 12)))))

(ert-deftest hsys-org:org-internal-target-at-p ()
  "Should return (start . end) iff point is within an org internal target definition."
  (with-temp-buffer
    (org-mode)
    (insert " <<target>>\n")
    (goto-char 6)
    (font-lock-ensure)
    (should (hsys-org-internal-target-def-at-p))))

(ert-deftest hsys-org:org-face-at-p ()
  "Should return face type iff point is within an org target definition."
  (with-temp-buffer
    (org-mode)
    (insert " <<target>>\n")
    (goto-char 6)
    (font-lock-ensure)
    (should (hsys-org-face-at-p 'org-target))))

(provide 'hsys-org-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hsys-org-tests.el ends here
