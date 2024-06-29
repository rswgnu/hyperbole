;;; hsys-org-tests.el --- hsys-org tests            -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    23-Apr-21 at 20:55:00
;; Last-Mod:     29-Jun-24 at 15:13:29 by Bob Weiner
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
    (should (equal (hproperty:char-property-range 1 'face 'org-level-1)
		   '(1 . 4)))))

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

(ert-deftest hsys-org--org-outside-org-mode-tmp-buffer ()
  "Org links in a temp buffer should work.
This is independent of the setting of `hsys-org-enable-smart-keys'."
  (let ((browse-url-browser-function nil)) ;; Don't open browser on failure
      (dolist (v '('unset 'button t nil))
        (let ((hsys-org-enable-smart-keys v))
          (with-temp-buffer
            (insert "[[file:/tmp/abc][file]]\n")
            (goto-char 6)
            (mocklet (((org-open-at-point-global) => t))
              (should (equal hsys-org-enable-smart-keys v)) ; Traceability
              (should (action-key))))))))

(ert-deftest hsys-org--org-outside-org-mode-tmp-file ()
  "Org links in a non `org-mode' file should work.
This is independent of the setting of `hsys-org-enable-smart-keys'."
  (let ((file (make-temp-file "hypb" nil ".txt" "[[file:/tmp/abc][file]]\n"))
        (browse-url-browser-function nil)) ;; Don't open browser on failure
    (unwind-protect
        (progn
          (find-file file)
          (goto-char 6)
          (dolist (v '('unset 'button t nil))
            (let ((hsys-org-enable-smart-keys v))
              (mocklet (((org-open-at-point-global) => t))
                (should (equal hsys-org-enable-smart-keys v)) ; Traceability
                (should (action-key))))))
      (hy-delete-file-and-buffer file))))

(provide 'hsys-org-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hsys-org-tests.el ends here
