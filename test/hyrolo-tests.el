;;; hyrolo-tests.el --- unit tests for hyrolo.el         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    19-Jun-21 at 22:42:00
;; Last-Mod:      1-Dec-23 at 23:20:39 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; "../hyrolo.el"

;;; Code:

(require 'ert)
(require 'hact)
(require 'hyrolo)
(require 'hyrolo-demo)
(require 'hy-test-helpers "test/hy-test-helpers")
(require 'with-simulated-input)

(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")
(declare-function hy-test-helpers:should-last-message "hy-test-helpers")

(ert-deftest hyrolo-add-items-at-multiple-levels ()
  "`hyrolo-add` can add items at different levels."
  (let ((hyrolo-file (make-temp-file "hypb" nil ".otl")))
    (unwind-protect
        (let ((hyrolo-file-list (list hyrolo-file)))
          (find-file (car (hyrolo-get-file-list)))
          (insert "===\nHdr\n===\n")
          (goto-char (point-min))
          (should (looking-at "==="))
          (hyrolo-add "a")
          (hyrolo-add "a/b")
          (hyrolo-add "a/b/c")
          (beginning-of-line)
          (should (looking-at-p "\*\*\*   c")))
      (hy-delete-file-and-buffer hyrolo-file))))

(ert-deftest hyrolo-demo-search-work ()
  "Use demo example and search for work should match work."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r work RET"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) hyrolo-display-buffer))
        (should (looking-at "======"))
        (forward-line 5)
        (should (looking-at "\\*.*Work")))
    (hyrolo-demo-quit)))

(ert-deftest hyrolo-demo-tab-jump-to-first-match ()
  "{TAB} shall jump to first match."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r work RET TAB"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) hyrolo-display-buffer))
        (should (looking-at "Work")))
    (hyrolo-demo-quit)))

(ert-deftest hyrolo-demo-toggle-visibility ()
  "Keys {h} and {a} shall toggle visibility."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r work RET TAB"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) hyrolo-display-buffer))
        (should (looking-at "Work"))

        (should (hact 'kbd-key "h"))
        (end-of-line)
        (should (get-char-property (point) 'invisible))

        (should (hact 'kbd-key "a"))
        (should-not (get-char-property (point) 'invisible))

        (should (hact 'kbd-key "h"))
        (end-of-line)
        (should (get-char-property (point) 'invisible))

        (should (hact 'kbd-key "s"))
        (should-not (get-char-property (point) 'invisible)))
    (hyrolo-demo-quit)))

(ert-deftest hyrolo-demo-show-overview ()
  "Key {o} shall show overview."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r work RET TAB"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) hyrolo-display-buffer))
        (should (looking-at "work"))

        (should (hact 'kbd-key "o"))
        (hy-test-helpers:consume-input-events)
	(forward-line 1)
        (end-of-line)
        (should (get-char-property (point) 'invisible))

        ;; Check next match is an outline
        (should (hact 'kbd-key "TAB"))
        (end-of-line)
        (should (get-char-property (point) 'invisible))

        ;; Check next line is end of buffer
        (should (hact 'kbd-key "n"))
        (should (equal (point) (point-max))))
    (hyrolo-demo-quit)))

(ert-deftest hyrolo-demo-move-to-beginning-and-end-of-file ()
  "*HyRolo* keys {<} and {>} move to beginning and end of file, respectively.
{,} and {.} move to beginning and end of current entry, respectively."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r work RET TAB"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) hyrolo-display-buffer))
        (should (looking-at "work"))

        (should (hact 'kbd-key "<"))
        (should (equal (point) (point-min)))

        (should (hact 'kbd-key ">"))
        (should (equal (point) (point-max)))

        (should (hact 'kbd-key "\C-u,n"))
        (hy-test-helpers:consume-input-events)
	(should (looking-at "\\*\\*\\s-+Hansen"))

        (should (hact 'kbd-key "."))
        (hy-test-helpers:consume-input-events)
	(should (looking-at "\\s-?\\*\\*\\*\\s-+Dunn")))
    (hyrolo-demo-quit)))

(ert-deftest hyrolo-demo-move-between-entries-on-same-level ()
  "Key {n} shall move to the next cell, {f} the next same level cell,
and {b} the previous same level cell."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r com RET TAB"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) hyrolo-display-buffer))
        (should (hact 'kbd-key "<"))
        (should (equal (point) (point-min)))

	(re-search-forward hyrolo-hdr-regexp nil t 2)
        (should (hact 'kbd-key "n"))
        (should (looking-at "\\*\\*\\s-+Strong"))

        (should (hact 'kbd-key "f"))
        (should (looking-at "\\*\\*\\s-+Hansen"))

        (should (hact 'kbd-key "b"))
        (should (looking-at "\\*\\*\\s-+Strong")))
    (hyrolo-demo-quit)))

(ert-deftest hyrolo-demo-no-following-same-level-heading ()
  "Error when trying to move to non existing next level heading."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r com RET TAB"))
        (hy-test-helpers:consume-input-events)

        (should (string= (buffer-name) hyrolo-display-buffer))
        (should (hact 'kbd-key "<"))
        (should (equal (point) (point-min)))

	(re-search-forward hyrolo-hdr-regexp nil t 2)
        (should (hact 'kbd-key "n"))
        (should (looking-at "\\*\\*\\s-+Strong"))

        (should (hact 'kbd-key "n"))
        (should (looking-at "\\*\\*\\*\\s-+Smith"))

        (condition-case err
            (should (hact 'kbd-key "f"))
          (error
           (progn
             (should (equal (car err) 'error))
             (should (string-match "No following same-level heading" (cadr err)))))))
    (hyrolo-demo-quit)))

(ert-deftest hyrolo-sort-test ()
  "HyRolo files can be sorted."
  (let ((hyrolo-file (make-temp-file "hypb" nil ".otl")))
    (unwind-protect
        (let ((hyrolo-file-list (list hyrolo-file))
              (hyrolo-date-format "%m/%d/%Y"))
          (hyrolo-find-file (car (hyrolo-get-file-list)))
          (insert "===\nHdr\n===\n")
          (goto-char (point-min))
          (should (looking-at "==="))
          (hyrolo-add "c")
          (hyrolo-add "b")
          (hyrolo-add "a")
          (hyrolo-add "b/d")

          ; Verify insertion order and following date on separate line
          (goto-char (point-min))
          (should (looking-at "==="))
          (dolist (insertion-order '("a" "b" "d" "c"))
            (goto-char (1+ (should (search-forward insertion-order))))
            (should (looking-at-p "^\t[0-9/]+$")))

          (hyrolo-sort)

          ; Verify sorted order and following date on separate line
          (goto-char (point-min))
          (should (looking-at "==="))
          (dolist (sorted-order '("a" "b" "d" "c"))
            (goto-char (1+ (should (search-forward sorted-order))))
            (should (looking-at-p "^\t[0-9/]+$"))))
      (hy-delete-file-and-buffer hyrolo-file))))

(ert-deftest hyrolo-sort-records-at-different-levels ()
  "HyRolo can sort records at different levels."
  (let* ((hyrolo-file (make-temp-file "hypb" nil ".otl"
                                      (concat "* 2\n\t2022-03-20\n"
                                              "** 2\n\t2022-03-20\n"
                                              "*** 2\n\t2022-03-20\n"
                                              "*** 1\n\t2022-03-20\n"
                                              "** 1\n\t2022-03-20\n"
                                              "*** 2\n\t2022-03-20\n"
                                              "*** 1\n\t2022-03-20\n"
                                              "* 1\n\t2022-03-20\n"
                                              "** 2\n\t2022-03-20\n"
                                              "*** 2\n\t2022-03-20\n"
                                              "*** 1\n\t2022-03-20\n"
                                              "** 1\n\t2022-03-20\n"
                                              "*** 2\n\t2022-03-20\n"
                                              "*** 1\n\t2022-03-20\n")))
	 (hyrolo-file-list (list hyrolo-file))
         (sorted-hyrolo-file (concat "* 1\n\t2022-03-20\n"
                                     "** 1\n\t2022-03-20\n"
                                     "*** 1\n\t2022-03-20\n"
                                     "*** 2\n\t2022-03-20\n"
                                     "** 2\n\t2022-03-20\n"
                                     "*** 1\n\t2022-03-20\n"
                                     "*** 2\n\t2022-03-20\n"
                                     "* 2\n\t2022-03-20\n"
                                     "** 1\n\t2022-03-20\n"
                                     "*** 1\n\t2022-03-20\n"
                                     "*** 2\n\t2022-03-20\n"
                                     "** 2\n\t2022-03-20\n"
                                     "*** 1\n\t2022-03-20\n"
                                     "*** 2\n\t2022-03-20\n")))
    (unwind-protect
	(progn (hyrolo-find-file hyrolo-file)
	       (hyrolo-sort hyrolo-file)
               (should (string= (buffer-string) sorted-hyrolo-file)))
      (hy-delete-file-and-buffer hyrolo-file))))

(ert-deftest hyrolo-fgrep-find-all-types-of-files ()
  "Verify that all types of files are found in an fgrep search."
  (let* ((temporary-file-directory (make-temp-file "hypb" t))
         (org-file (make-temp-file "hypb" nil ".org" "string\n"))
         (kotl-file (make-temp-file "hypb" nil ".kotl" "string"))
         (md-file (make-temp-file "hypb" nil ".md" "string\n"))
         (outl-file (make-temp-file "hypb" nil ".otl" "string\n"))
         (hyrolo-file-list (list temporary-file-directory)))
    (unwind-protect
        (progn
          (hyrolo-fgrep "string")
          (with-current-buffer "*HyRolo*"
            (should (= (how-many "@loc>") 4))
            (dolist (f (list org-file kotl-file md-file outl-file))
              (should (= (how-many (concat "@loc> \"" f "\"")) 1)))))
      (dolist (f (list org-file kotl-file md-file outl-file))
        (hy-delete-file-and-buffer f))
      (kill-buffer "*HyRolo*")
      (delete-directory temporary-file-directory))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-org-heading ()
  "Verify move to next heading, then action-key to go to record for org mode."
  (let* ((temporary-file-directory (make-temp-file "hypb" t))
         (org-file (make-temp-file "hypb" nil ".org" "* heading\nstring\nmore\n"))
         (hyrolo-file-list (list temporary-file-directory)))
    (unwind-protect
        (progn
          (hyrolo-fgrep "string")
          (with-current-buffer "*HyRolo*"
            (should (= (how-many "@loc>") 1))
            (should (looking-at-p "==="))
            (hyrolo-next-visible-heading 1)
            (should (looking-at-p "* heading")))
          (with-simulated-input "y RET" ; Do you want to revisit the file normally now?
            (action-key)
            (should (equal (current-buffer) (find-buffer-visiting org-file)))
            (should (looking-at-p "* heading"))))
      (hy-delete-file-and-buffer org-file)
      (kill-buffer "*HyRolo*")
      (delete-directory temporary-file-directory))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-kotl-heading ()
  "Verify move to next heading, then action-key to go to record for kotl mode."
  (let* ((temporary-file-directory (make-temp-file "hypb" t))
         (kotl-file (make-temp-file "hypb" nil ".kotl"))
         (hyrolo-file-list (list temporary-file-directory)))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "heading")
          (kotl-mode:newline 1)
          (insert "string")
          (kotl-mode:newline 1)
          (insert "more")
          (hyrolo-fgrep "string")
          (with-current-buffer "*HyRolo*"
            (should (= (how-many "@loc>") 1))
            (should (looking-at-p "==="))
            (hyrolo-next-visible-heading 1)
            (should (looking-at-p ".*1\\. heading")))
          (with-simulated-input "y RET" ; Do you want to revisit the file normally now?
            (action-key)
            (should (equal (current-buffer) (find-buffer-visiting kotl-file)))
            (should (looking-at-p "heading"))))
      (hy-delete-file-and-buffer kotl-file)
      (kill-buffer "*HyRolo*")
      (delete-directory temporary-file-directory))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-outl-heading ()
  "Verify move to next heading, then action-key to go to record for outline mode."
  (let* ((temporary-file-directory (make-temp-file "hypb" t))
         (outl-file (make-temp-file "hypb" nil ".otl" "* heading\nstring\nmore\n"))
         (hyrolo-file-list (list temporary-file-directory)))
    (unwind-protect
        (progn
          (hyrolo-fgrep "string")
          (with-current-buffer "*HyRolo*"
            (should (= (how-many "@loc>") 1))
            (should (looking-at-p "==="))
            (hyrolo-next-visible-heading 1)
            (should (looking-at-p "* heading")))
          (with-simulated-input "y RET" ; Do you want to revisit the file normally now?
            (action-key)
            (should (equal (current-buffer) (find-buffer-visiting outl-file)))
            (should (looking-at-p "* heading"))))
      (hy-delete-file-and-buffer outl-file)
      (kill-buffer "*HyRolo*")
      (delete-directory temporary-file-directory))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-md-heading ()
  "Verify move to next heading, then action-key to go to record for markdown mode."
  :expected-result :failed
  (let* ((temporary-file-directory (make-temp-file "hypb" t))
         (md-file (make-temp-file "hypb" nil ".md" "# heading\nstring\nmore\n"))
         (hyrolo-file-list (list temporary-file-directory)))
    (unwind-protect
        (progn
          (hyrolo-fgrep "string")
          (with-current-buffer "*HyRolo*"
            (should (= (how-many "@loc>") 1))
            (should (looking-at-p "==="))
            (hyrolo-next-visible-heading 1)
            (should (looking-at-p "## heading")))
          (with-simulated-input "y RET" ; Do you want to revisit the file normally now?
            (action-key)
            (should (equal (current-buffer) (find-buffer-visiting md-file)))
            (should (looking-at-p "# heading"))))
      (hy-delete-file-and-buffer md-file)
      (kill-buffer "*HyRolo*")
      (delete-directory temporary-file-directory))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-kotl-heading-level-2 ()
  "Verify move to next heading, then action-key to go to record for kotl mode.
Match a string in a level 2 child cell."
  (let* ((temporary-file-directory (make-temp-file "hypb" t))
         (kotl-file (make-temp-file "hypb" nil ".kotl"))
         (hyrolo-file-list (list temporary-file-directory)))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-child)
          (insert "heading")
          (kotl-mode:newline 1)
          (insert "string")
          (kotl-mode:newline 1)
          (insert "more")
          (hyrolo-fgrep "string")
          (with-current-buffer "*HyRolo*"
            (should (= (how-many "@loc>") 1))
            (should (looking-at-p "==="))
            (hyrolo-next-visible-heading 1)
            (should (looking-at-p ".*1a\\. heading")))
          (with-simulated-input "y RET" ; Do you want to revisit the file normally now?
            (action-key)
            (should (equal (current-buffer) (find-buffer-visiting kotl-file)))
            (should (looking-at-p "heading"))))
      (hy-delete-file-and-buffer kotl-file)
      (kill-buffer "*HyRolo*")
      (delete-directory temporary-file-directory))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-kotl-heading-cell-2 ()
  "Verify move to next heading, then action-key to go to record for kotl mode.
Match a string in the second cell."
  (let* ((temporary-file-directory (make-temp-file "hypb" t))
         (kotl-file (make-temp-file "hypb" nil ".kotl"))
         (hyrolo-file-list (list temporary-file-directory)))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-cell)
          (insert "heading")
          (kotl-mode:newline 1)
          (insert "string")
          (kotl-mode:newline 1)
          (insert "more")
          (hyrolo-fgrep "string")
          (with-current-buffer "*HyRolo*"
            (should (= (how-many "@loc>") 1))
            (should (looking-at-p "==="))
            (hyrolo-next-visible-heading 1)
            (should (looking-at-p ".*2\\. heading")))
          (with-simulated-input "y RET" ; Do you want to revisit the file normally now?
            (action-key)
            (should (equal (current-buffer) (find-buffer-visiting kotl-file)))
            (should (looking-at-p "heading"))))
      (hy-delete-file-and-buffer kotl-file)
      (kill-buffer "*HyRolo*")
      (delete-directory temporary-file-directory))))

(provide 'hyrolo-tests)
;;; hyrolo-tests.el ends here
