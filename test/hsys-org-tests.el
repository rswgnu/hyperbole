;;; hsys-org-tests.el --- hsys-org tests            -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    23-Apr-21 at 20:55:00
;; Last-Mod:     13-Apr-25 at 16:20:45 by Bob Weiner
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
(require 'org-agenda)
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
            (should (equal hsys-org-enable-smart-keys v)) ; Traceability
            (should (action-key))
	    (should (hattr:ibtype-is-p 'org-link-outside-org-mode)))))))

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
                (should (action-key))
		(should (hattr:ibtype-is-p 'org-link-outside-org-mode))))))
      (hy-delete-file-and-buffer file))))

(ert-deftest hsys-org--at-tags-p ()
  "Verify `hsys-org-at-tags-p'."
  (with-temp-buffer
    (org-mode)
    (save-excursion (insert "* header :tag:"))
    (font-lock-ensure)
    (should-not (hsys-org-at-tags-p))
    (should (search-forward ":"))
    (should (hsys-org-at-tags-p))))

(ert-deftest hsys-org--directory-at-tags-p ()
  "Verify `hsys-org-directory-at-tags-p'."
  (mocklet ((hsys-org-at-tags-p => nil)
            (string-prefix-p not-called))
    (should-not (hsys-org-directory-at-tags-p)))
  (let ((buffer-file-name (expand-file-name "buff" org-directory)))
    (mocklet ((hsys-org-at-tags-p not-called))
      (should (hsys-org-directory-at-tags-p t))))
  (mocklet ((hsys-org-at-tags-p => t))
    (let ((buffer-file-name (expand-file-name "buff" org-directory)))
      (should (hsys-org-directory-at-tags-p))))
  (mocklet ((hsys-org-at-tags-p => t)
            (buffer-name => "*Org Agenda*"))
    (let ((buffer-file-name nil))
      (should (hsys-org-directory-at-tags-p))))
  (mocklet ((hsys-org-at-tags-p => t)
            (buffer-name => "*Another Agenda*"))
    (let ((buffer-file-name nil))
      (should-not (hsys-org-directory-at-tags-p)))))

(ert-deftest hsys-org--mode-p ()
  "Verify `hsys-org-mode-p' identifies an org major or minor-mode."
  (with-temp-buffer
    (org-mode)
    (should (hsys-org-mode-p))
    (org-agenda-mode)
    (should (hsys-org-mode-p))))

(ert-deftest hsys-org---agenda-tags-string ()
  "Verify `hsys-org--agenda-tags-string'."
  (with-temp-buffer
    (org-mode)
    (save-excursion (insert "* header :tag1:tag2:tag3:"))
    (font-lock-ensure)

    (should (and (search-forward "header ") (looking-at-p ":tag1:")))
    (should (string= ":tag1:tag2:tag3:" (hsys-org--agenda-tags-string)))

    (forward-char)
    (should (looking-at-p "tag1:"))
    (should (string= "tag1" (hsys-org--agenda-tags-string)))

    (should (and (search-forward "tag1") (looking-at-p ":tag2:")))
    (should (string= ":tag1:tag2:tag3:" (hsys-org--agenda-tags-string)))

    (forward-char)
    (should (looking-at-p "tag2:"))
    (should (string= "tag2" (hsys-org--agenda-tags-string)))))

(ert-deftest hsys-org--get-agenda-tags ()
  "Verify `hsys-org-get-agenda-tags' calls org-consult-agenda-function."
  (mocklet ((agenda-func => "agenda-func")
            (hsys-org-at-tags-p => t)
            (hsys-org--agenda-tags-string => ":tag"))
    (should (string= "agenda-func" (hsys-org-get-agenda-tags #'agenda-func)))))

(ert-deftest hsys-org--meta-return-on-end-of-line ()
  "Verify end-of-line behaves as `org-mode' when smart keys are not enabled."
  (dolist (v '(nil :buttons))
    (let ((hsys-org-enable-smart-keys v))
      ;; One line with text, no return: smart-org triggers with nil or :buttons setting
      (with-temp-buffer
        (org-mode)
        (insert "* h1")
        (goto-char 1)
        (end-of-line)
        (with-mock
          (mock (hsys-org-meta-return) => t)
          (should (equal hsys-org-enable-smart-keys v)) ; Ert traceability
          (should (action-key))
	  (should (hattr:actype-is-p 'hsys-org-meta-return))))
      ;; Two lines with text and returns: smart-org triggers with nil or :buttons setting
      (with-temp-buffer
        (org-mode)
        (insert "* h1\n* h2\n")
        (goto-char 1)
        (end-of-line)
        (with-mock
          (mock (hsys-org-meta-return) => t)
          (should (equal hsys-org-enable-smart-keys v)) ; Ert traceability
          (should (action-key))
	  (should (hattr:actype-is-p 'hsys-org-meta-return))))))

  (let ((hsys-org-enable-smart-keys t)
        (v t))
    ;; One line with text, no return: smart-eolp triggers with t setting
    (with-temp-buffer
      (org-mode)
      (insert "* h1")
      (goto-char 1)
      (end-of-line)
      (with-mock
        (mock (smart-scroll-up) => t)
	(should (equal hsys-org-enable-smart-keys v)) ; Ert traceability
	(should (action-key))
	(should (hattr:actype-is-p 'smart-scroll-up))))
    ;; Two lines with text and returns: smart-eolp triggers with t setting
    (with-temp-buffer
      (org-mode)
      (insert "* h1\n* h2\n")
      (goto-char 1)
      (end-of-line)
      (with-mock
        (mock (smart-scroll-up) => t)
	(should (equal hsys-org-enable-smart-keys v)) ; Ert traceability
	(should (action-key))
	(should (hattr:actype-is-p 'smart-scroll-up))))))

(provide 'hsys-org-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hsys-org-tests.el ends here
