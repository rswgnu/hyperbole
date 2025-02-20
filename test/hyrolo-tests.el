;;; hyrolo-tests.el --- unit tests for hyrolo.el         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    19-Jun-21 at 22:42:00
;; Last-Mod:     19-Feb-25 at 21:08:39 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2025  Free Software Foundation, Inc.
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
(require 'hy-test-dependencies) ;; can install el-mock
(require 'hy-test-helpers "test/hy-test-helpers")
(require 'hib-kbd)
(require 'kotl-mode)

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
          (should (looking-at-p "\\*\\*\\*   c")))
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
        (should-not (get-char-property (point) 'invisible)))
    (hyrolo-demo-quit)))

(ert-deftest hyrolo-demo-move-to-beginning-and-end-of-file ()
  "*HyRolo* keys {<} and {>} move to begin and end of file, respectively."
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
        (should (equal (point) (point-max))))
    (hyrolo-demo-quit)))

(ert-deftest hyrolo-demo-move-to-beginning-and-end-of-entry ()
  "*HyRolo* keys {,} and {.} move to begin and end of an entry, respectively."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r work RET TAB"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) hyrolo-display-buffer))

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

	(hyrolo-hdr-move-after-p)
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

	(hyrolo-hdr-move-after-p)
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

	  ;; Verify insertion order and following date on separate line
          (goto-char (point-min))
          (should (looking-at "==="))
          (dolist (insertion-order '("a" "b" "d" "c"))
            (goto-char (1+ (should (search-forward insertion-order))))
            (should (looking-at-p "^\t[0-9/]+$")))

          (hyrolo-sort)

	  ;; Verify sorted order and following date on separate line
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
  (let* ((folder (make-temp-file "hypb" t))
         (prefix (expand-file-name "hypb" folder))
         (org-file (make-temp-file prefix nil ".org" "* string\n"))
         (kotl-file (make-temp-file prefix nil ".kotl" "1.  string"))
         (md-file (make-temp-file prefix nil ".md" "# string\n"))
         (outl-file (make-temp-file prefix nil ".otl" "* string\n"))
         (hyrolo-file-list (list folder)))
    (unwind-protect
        (progn
          (hyrolo-fgrep "string")
          (should (string= (buffer-name) hyrolo-display-buffer))
          (should (= (how-many "@loc>") 4))
          (dolist (f (list org-file kotl-file md-file outl-file))
            (should (= (how-many (concat "@loc> \"" f "\"")) 1))))
      (dolist (f (list org-file kotl-file md-file outl-file))
        (hy-delete-file-and-buffer f))
      (kill-buffer hyrolo-display-buffer)
      (delete-directory folder))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-org-heading ()
  "Verify move to next heading, then action-key to go to record for org mode."
  (let* ((folder (make-temp-file "hypb" t))
         (prefix (expand-file-name "hypb" folder))
         (org-file (make-temp-file prefix nil ".org" "* heading\nstring\nmore\n"))
         (hyrolo-file-list (list folder)))
    (unwind-protect
        (progn
          (hyrolo-fgrep "string")
          (should (string= (buffer-name) hyrolo-display-buffer))
          (should (= (how-many "@loc>") 1))
          (should (looking-at-p "==="))
          (hyrolo-outline-next-visible-heading 1)
          (should (looking-at-p "* heading"))
          (mocklet ((y-or-n-p => t))
            (action-key))
          (should (equal (current-buffer) (find-buffer-visiting org-file)))
          (should (looking-at-p "* heading")))
      (hy-delete-file-and-buffer org-file)
      (kill-buffer hyrolo-display-buffer)
      (delete-directory folder))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-kotl-heading ()
  "Verify move to next heading, then action-key to go to record for kotl mode."
  (let* ((folder (make-temp-file "hypb" t))
         (prefix (expand-file-name "hypb" folder))
         (kotl-file (make-temp-file prefix nil ".kotl"))
         (hyrolo-file-list (list folder)))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "heading")
          (kotl-mode:newline 1)
          (insert "string")
          (kotl-mode:newline 1)
          (insert "more")
          (hyrolo-fgrep "string")
          (should (string= (buffer-name) hyrolo-display-buffer))
          (should (= (how-many "@loc>") 1))
          (should (looking-at-p "==="))
          (hyrolo-outline-next-visible-heading 1)
          (should (looking-at-p ".*1\\. heading"))
          (action-key)
          (should (equal (current-buffer) (find-buffer-visiting kotl-file)))
          (should (looking-at-p "heading")))
      (hy-delete-file-and-buffer kotl-file)
      (kill-buffer hyrolo-display-buffer)
      (delete-directory folder))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-outl-heading ()
  "Verify move to next heading, then action-key to go to record for outline mode."
  (let* ((folder (make-temp-file "hypb" t))
         (prefix (expand-file-name "hypb" folder))
         (outl-file (make-temp-file prefix nil ".otl" "* heading\nstring\nmore\n"))
         (hyrolo-file-list (list folder)))
    (unwind-protect
        (progn
          (hyrolo-fgrep "string")
          (should (string= (buffer-name) hyrolo-display-buffer))
          (should (= (how-many "@loc>") 1))
          (should (looking-at-p "==="))
          (hyrolo-outline-next-visible-heading 1)
          (should (looking-at-p "* heading"))
          (action-key)
          (should (equal (current-buffer) (find-buffer-visiting outl-file)))
          (should (looking-at-p "* heading")))
      (hy-delete-file-and-buffer outl-file)
      (kill-buffer hyrolo-display-buffer)
      (delete-directory folder))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-md-heading ()
  "Verify move to next heading, then action-key to go to record for markdown mode."
  (let* ((folder (make-temp-file "hypb" t))
         (prefix (expand-file-name "hypb" folder))
         (md-file (make-temp-file prefix nil ".md" "# heading\nstring\nmore\n"))
         (hyrolo-file-list (list folder)))
    (unwind-protect
        (progn
          (hyrolo-fgrep "string")
          (should (string= (buffer-name) hyrolo-display-buffer))
          (should (= (how-many "@loc>") 1))
          (should (looking-at-p "==="))
          (hyrolo-outline-next-visible-heading 1)
          (should (looking-at-p "# heading"))
          (action-key)
          (should (equal (current-buffer) (find-buffer-visiting md-file)))
          (should (looking-at-p "# heading")))
      (hy-delete-file-and-buffer md-file)
      (kill-buffer hyrolo-display-buffer)
      (delete-directory folder))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-kotl-heading-level-2 ()
  "Verify move to next heading, then action-key to go to record for kotl mode.
Match a string in a level 2 child cell."
  (let* ((folder (make-temp-file "hypb" t))
         (prefix (expand-file-name "hypb" folder))
         (kotl-file (make-temp-file prefix nil ".kotl"))
         (hyrolo-file-list (list folder)))
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
          (should (string= (buffer-name) hyrolo-display-buffer))
          (should (= (how-many "@loc>") 1))
          (should (looking-at-p "==="))
          (hyrolo-outline-next-visible-heading 1)
          (should (looking-at-p ".*1a\\. heading"))
          (action-key)
          (should (equal (current-buffer) (find-buffer-visiting kotl-file)))
          (should (looking-at-p "heading")))
      (hy-delete-file-and-buffer kotl-file)
      (kill-buffer hyrolo-display-buffer)
      (delete-directory folder))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-kotl-heading-cell-2 ()
  "Verify move to next heading, then action-key to go to record for kotl mode.
Match a string in the second cell."
  (let* ((folder (make-temp-file "hypb" t))
         (prefix (expand-file-name "hypb" folder))
         (kotl-file (make-temp-file prefix nil ".kotl"))
         (hyrolo-file-list (list folder)))
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
          (should (string= (buffer-name) hyrolo-display-buffer))
          (should (= (how-many "@loc>") 1))
          (should (looking-at-p "==="))
          (hyrolo-outline-next-visible-heading 1)
          (should (looking-at-p ".*2\\. heading"))
          (action-key)
          (should (equal (current-buffer) (find-buffer-visiting kotl-file)))
          (should (looking-at-p "heading")))
      (hy-delete-file-and-buffer kotl-file)
      (kill-buffer hyrolo-display-buffer)
      (delete-directory folder))))

(ert-deftest hyrolo-tests--get-file-list-change ()
  "Verify a change to hyrolo-file-list is noticed by hyrolo-get-file-list."
  (let* ((tmp-file (make-temp-file "hypb" nil ".org"))
         (hyrolo-file-list (list tmp-file)))
    (unwind-protect
        (let ((hl (hyrolo-get-file-list)))
          (should (= 1 (length hl)))
          (should (string= (car hl) tmp-file)))
      (hy-delete-file-and-buffer tmp-file))))

(ert-deftest hyrolo-tests--get-file-list-wrong-suffix ()
  "Verify files need to have the proper suffix in hyrolo-file-list."
  (let ((tmp-file (make-temp-file "hypb" nil)))
    (unwind-protect
        (should-error
         (let* ((hyrolo-boolean-only-flag t)
		(hyrolo-file-list (list tmp-file)))
           ()))
      (hy-delete-file-and-buffer tmp-file))))

(ert-deftest hyrolo-tests--get-file-list ()
  "Verify `hyrolo-get-file-list` includes added files."
  (let* ((folder (make-temp-file "hypb" t))
         (prefix (expand-file-name "hypb" folder))
         (org-file (make-temp-file prefix nil ".org"))
         (hyrolo-file-list (list folder)))
    (unwind-protect
        (progn
          (should (= 1 (length (hyrolo-get-file-list))))
          (let ((org2-file (make-temp-file prefix nil ".org")))
            (unwind-protect
		(progn (hyrolo-refresh-file-list)
                       (should (= 2 (length (hyrolo-get-file-list)))))
              (hy-delete-file-and-buffer org2-file))))
      (dolist (f (list org-file))
        (hy-delete-file-and-buffer f))
      (delete-directory folder))))

;; Outline movement tests
(defun hyrolo-tests--level-number (section depth)
  "Generate the number for the SECTION at DEPTH.

The format is the section followed by the depth given by the
sequence up to depth starting from 2.
  Depth 1:        section
  Depth <depth>:  section.2.3.4..<depth>"
  (let (result)
    (dotimes (d depth)
      (setq result
            (if (= 0 d)
                (number-to-string section)
              (concat result
                      "."
                      (number-to-string (+ 1 d))))))
    result))

(defun hyrolo-tests--generate-heading-contents-for-tests (heading-prefix-char heading section body depth)
  "Generate the HEADING and BODY contents for the SECTION with DEPTH."
  (let (result)
    (dotimes (d depth)
      (setq result
            (concat result
                    (make-string (1+ d) heading-prefix-char)
		    " " heading " " (hyrolo-tests--level-number section (1+ d)) "\n"
                    body " " (hyrolo-tests--level-number section (1+ d)) "\n")))
    result))

(defun hyrolo-tests--gen-outline (heading-prefix-char heading sections body depth)
  "Generate an outline structure suitable for hyrolo outline test.

The contents is constructed with an outline HEADING-PREFIX-CHAR,
HEADING and BODY text.  Each is repeated in SECTIONS with one set
of hierarchical headings to the specified DEPTH.

Example:
   * heading 1
   body 1
   ** heading 2
   body 1.2
   [...]
   * heading <sections>
   body <sections>
   ** heading <sections>.2
   body <section>.2
   [...]"
  (let (result)
    (dotimes (section sections)
      (setq result
            (concat result
                    (hyrolo-tests--generate-heading-contents-for-tests
		     heading-prefix-char heading (1+ section) body depth))))
    result))

(ert-deftest hyrolo-tests--outline-next-visible-heading ()
  "Verify movement to next visible heading."
  (let* ((org-file (make-temp-file "hypb" nil ".org"
                                   (hyrolo-tests--gen-outline ?* "heading" 2 "body" 2)))
         (hyrolo-file-list (list org-file)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Move down
          (should (looking-at-p "==="))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^\\* heading 1"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^\\*\\* heading 1\\.2"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^\\* heading 2"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^\\*\\* heading 2\\.2"))
          (should (hact 'kbd-key "n"))
          (should (eobp))

          ;; Move back up
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^\\*\\* heading 2\\.2"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^\\* heading 2"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^\\*\\* heading 1\\.2"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^\\* heading 1"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "==="))
          (should (bobp)))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--outline-next-visible-heading-md ()
  "Verify movement to next visible heading."
  (let* ((md-file (make-temp-file "hypb" nil ".md"
                                  (hyrolo-tests--gen-outline ?# "heading" 2 "body" 2)))
         (hyrolo-file-list (list md-file)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Move down
          (should (looking-at-p "==="))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^# heading 1"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^## heading 1\\.2"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^# heading 2"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^## heading 2\\.2"))
          (should (hact 'kbd-key "n"))
          (should (eobp))

          ;; Move back up
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^## heading 2\\.2"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^# heading 2"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^## heading 1\\.2"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^# heading 1"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "==="))
          (should (bobp)))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--outline-next-visible-heading-all ()
  "Verify movement to next visible heading."
  (let* ((md-file (make-temp-file "hypb" nil ".md"
                                  (hyrolo-tests--gen-outline ?# "heading" 2 "body" 2)))
         (hyrolo-file-list (list md-file)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Move down
          (should (looking-at-p "==="))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^# heading 1"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^## heading 1\\.2"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^# heading 2"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^## heading 2\\.2"))
          (should (hact 'kbd-key "n"))
          (should (eobp))

          ;; Move back up
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^## heading 2\\.2"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^# heading 2"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^## heading 1\\.2"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^# heading 1"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "==="))
          (should (bobp)))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--outline-up-heading ()
  "Verify movement from sub heading to next heading one level above."
  (let* ((org-file (make-temp-file "hypb" nil ".org"
                                   (hyrolo-tests--gen-outline ?* "heading" 2 "body" 3)))
         (hyrolo-file-list (list org-file)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Move to last heading
          (goto-char (point-max))
          (forward-line -2)
          (should (looking-at-p "^\\*\\*\\* heading 2\\.2\\.3$"))
          (should (hact 'kbd-key "u"))
          (should (looking-at-p "^\\*\\* heading 2\\.2$"))
          (should (hact 'kbd-key "u"))
          (should (looking-at-p "^\\* heading 2$"))
          (should-error (hact 'kbd-key "u")))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-file-and-buffer org-file))))

(ert-deftest hyrolo-tests--outline-next-visible-heading-two-sections ()
  "Verify movement to next visible heading with two sections."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org"
                                    (hyrolo-tests--gen-outline ?* "heading-a" 1 "body-a" 2)))
         (md-file1 (make-temp-file "hypb" nil ".md"
                                   (hyrolo-tests--gen-outline ?# "heading-b" 1 "body-b" 2)))
         (hyrolo-file-list (list org-file1 md-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Move down
          (should (looking-at-p "==="))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^\\* heading-a 1$"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^\\*\\* heading-a 1\\.2$"))
          (should (hact 'kbd-key "n"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^# heading-b 1$"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^## heading-b 1\\.2$"))
          (should (hact 'kbd-key "n"))
          (should (eobp))

          ;; Move back up
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^## heading-b 1\\.2$"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^# heading-b 1$"))
          (should (hact 'kbd-key "p"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^\\*\\* heading-a 1\\.2$"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^\\* heading-a 1$"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "==="))
          (should (= 1 (line-number-at-pos))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(defun hyrolo-tests--gen-kotl-outline (heading body &optional depth)
  "Generate a temp file with kotl outline structure for hyrolo outline test.
Make cell start with HEADING and follow by next line BODY.  With
optional DEPTH the number of sub cells are created to that depth."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (find-file kotl-file)
    (insert heading)
    (kotl-mode:newline 1)
    (insert body)
    (when (and depth (< 0 depth))
      (dotimes (d depth)
        (kotl-mode:add-child)
        (insert (format "%s %d" heading (1+ d)))
        (kotl-mode:newline 1)
        (insert (format "%s %d" body (1+ d)))))
    (save-buffer)
    kotl-file))

(ert-deftest hyrolo-tests--outline-next-visible-heading-kotl ()
  "Verify movement to next visible heading with a kotl file."
  (let* ((kotl-file1 (hyrolo-tests--gen-kotl-outline "heading-kotl" "body-kotl"))
         (hyrolo-file-list (list kotl-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          (should (looking-at-p "==="))
          (should (and (hact 'kbd-key "n") (looking-at-p "^ +1\\. heading-kotl$")))
          (should (and (hact 'kbd-key "n") (eobp)))
          (should (and (hact 'kbd-key "p") (looking-at-p "^ +1\\. heading-kotl$")))
          (should (and (hact 'kbd-key "p") (looking-at-p "==="))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--outline-next-visible-heading-all-file-types ()
  "Verify movement to next visible heading with all files types present."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org"
                                    (hyrolo-tests--gen-outline ?* "heading-org" 1 "body-org" 1)))
         (otl-file1 (make-temp-file "hypb" nil ".otl"
                                    (hyrolo-tests--gen-outline ?* "heading-otl" 1 "body-otl" 1)))
         (md-file1 (make-temp-file "hypb" nil ".md"
                                   (hyrolo-tests--gen-outline ?# "heading-md" 1 "body-md" 1)))
         (kotl-file1 (hyrolo-tests--gen-kotl-outline "heading-kotl" "body-kotl"))
         (hyrolo-file-list (list org-file1 otl-file1 md-file1 kotl-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Move down
          (dolist (v '("===" "^\\* heading-org 1$" "===" "^\\* heading-otl 1$"
                       "===" "^# heading-md 1$" "===" "^ +1\\. heading-kotl$"))
            (should (and (looking-at-p v) (hact 'kbd-key "n"))))
          (should (eobp))

          ;; Move up
          (dolist (v '("^ +1\\. heading-kotl$" "===" "^# heading-md 1$" "==="
                       "^\\* heading-otl 1$" "===" "^\\* heading-org 1$" "==="))
            (should (and (hact 'kbd-key "p") (looking-at-p v))))
          (should (= 1 (line-number-at-pos))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(defun hyrolo-tests--verify-hidden-line ()
  "Verify that a line is hidden."
  (save-excursion
    (end-of-line)
    (should (get-char-property (point) 'invisible))))

(defun hyrolo-tests--verify-not-hidden-line ()
  "Verify that a line is hidden."
  (save-excursion
    (end-of-line)
    (should-not (get-char-property (point) 'invisible))))

(ert-deftest hyrolo-tests--outline-hide-show-heading ()
  "Verify hiding and showing headings."
  (let* ((org-file (make-temp-file "hypb" nil ".org"
                                   (hyrolo-tests--gen-outline ?* "heading" 1 "body" 2)))
         (hyrolo-file-list (list org-file)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Hide/Show first line hides whole section
          (should (looking-at-p "==="))
          (should (hact 'kbd-key "h"))
	  (hyrolo-tests--verify-hidden-line)
          (should (hact 'kbd-key "s"))
	  (hyrolo-tests--verify-not-hidden-line)

	  ;; Hide/Show first section heading
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^\\* heading 1$"))
          (should (hact 'kbd-key "h"))
	  (hyrolo-tests--verify-hidden-line)
	  (save-excursion
	    (forward-visible-line 1)
	    (should (eobp)))
          (should (hact 'kbd-key "s"))
	  (hyrolo-tests--verify-not-hidden-line)

	  ;; Hide/Show level 2 heading
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^\\*\\* heading 1\\.2$"))
          (should (hact 'kbd-key "h"))
	  (hyrolo-tests--verify-hidden-line)
	  (save-excursion
	    (forward-visible-line 1)
	    (should (eobp)))
          (should (hact 'kbd-key "s"))
	  (hyrolo-tests--verify-not-hidden-line))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--outline-show-when-moving-out-of-hidden-line ()
  "Verify region is shown after moving out of hidden area."
  (let* ((org-file (make-temp-file "hypb" nil ".org"
                                   (hyrolo-tests--gen-outline ?* "heading" 1 "body" 2)))
         (hyrolo-file-list (list org-file)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Hide first line hides whole section
          (should (looking-at-p "==="))
          (should (hact 'kbd-key "h"))
	  (hyrolo-tests--verify-hidden-line)

	  ;; Now expose just top-level headings and move to buffer beginning
          (should (hact 'kbd-key "t"))
          (should (hact 'kbd-key "<"))

	  ;; Move to first heading and back to top
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^\\* heading 1$"))
	  (should-not (get-char-property (point) 'invisible))
          (should (hact 'kbd-key "p"))
	  (should (and (looking-at-p "===") (= 1 (line-number-at-pos))))
	  (hyrolo-tests--verify-not-hidden-line))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--tab-through-matches ()
  "Verify tabbing through search matches."
  (let* ((org-file (make-temp-file "hypb" nil ".org"
                                   (hyrolo-tests--gen-outline ?* "heading" 2 "body" 2)))
         (hyrolo-file-list (list org-file)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Search Down
          (should (looking-at-p "==="))
          (should (hact 'kbd-key "TAB"))
          (should (looking-at-p "^body 1$"))
          (should (hact 'kbd-key "TAB"))
          (should (looking-at-p "^body 1\\.2"))
          (should (hact 'kbd-key "TAB"))
          (should (looking-at-p "^body 2$"))
          (should (hact 'kbd-key "TAB"))
          (should (looking-at-p "^body 2\\.2"))
          (should-error (hact 'kbd-key "TAB"))
          (should (looking-at-p "^body 2\\.2"))

          ;; Search Up
          (should (hact 'kbd-key "<backtab>"))
          (should (looking-at-p "^body 2$"))
          (should (hact 'kbd-key "<backtab>"))
          (should (looking-at-p "^body 1\\.2"))
          (should (hact 'kbd-key "<backtab>"))
          (should (looking-at-p "^body 1$"))
          (should-error (hact 'kbd-key "<backtab>")))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--edit-entry ()
  "Verify {e} brings up entry in new window."
  (let* ((org-file (make-temp-file "hypb" nil ".org"
                                   (hyrolo-tests--gen-outline ?* "heading" 1 "body" 2)))
         (hyrolo-file-list (list org-file)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Search Down
          (should (looking-at-p "==="))
          (should (hact 'kbd-key "TAB"))
          (should (looking-at-p "^body 1$"))

          ;; Edit record
          (mocklet ((y-or-n-p => t))
            (should (hact 'kbd-key "e")))
          (should (string= (buffer-name) (file-name-nondirectory org-file)))
          (should (looking-at-p "^body 1$"))

          ;; Edit next record
          (switch-to-buffer hyrolo-display-buffer)
          (should (hact 'kbd-key "TAB"))
          (should (looking-at-p "^body 1\\.2$"))
          (should (hact 'kbd-key "e"))
          (should (string= (buffer-name) (file-name-nondirectory org-file)))
          (should (looking-at-p "^body 1\\.2$"))
          )
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--forward-same-level-all-file-types-level1 ()
  "Verify forward and backward to first level headers and section lines.
All files types are present."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org"
                                    (hyrolo-tests--gen-outline ?* "heading-org" 1 "body-org" 1)))
         (md-file1 (make-temp-file "hypb" nil ".md"
                                   (hyrolo-tests--gen-outline ?# "heading-md" 1 "body-md" 1)))
         (otl-file1 (make-temp-file "hypb" nil ".otl"
                                    (hyrolo-tests--gen-outline ?* "heading-otl" 1 "body-otl" 1)))
         (kotl-file1 (hyrolo-tests--gen-kotl-outline "heading-kotl" "body-kotl"))
         (hyrolo-file-list (list org-file1 md-file1 otl-file1 kotl-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Move forward
          (dolist (v '("===" "^\\* heading-org 1$" "===" "^# heading-md 1$"
                       "===" "^\\* heading-otl 1$" "==="))
            (should (and (looking-at-p v) (hact 'kbd-key "f"))))
          (should (looking-at-p "^ +1\\. heading-kotl$")) ; When on last match do not move further

          ;; Move backward
          (dolist (v '("===" "^\\* heading-otl 1$" "===" "^# heading-md 1$"
                       "===" "^\\* heading-org 1$" "==="))
            (should (and (hact 'kbd-key "b") (looking-at-p v))))
          (should (= 1 (line-number-at-pos))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--forward-same-level-all-file-types-level1-depth2 ()
  "Verify forward and backward to first level headers and section lines.
All files types are present with a max depth of 2 of the outline
structure."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org"
                                    (hyrolo-tests--gen-outline ?* "heading-org" 1 "body-org" 2)))
         (md-file1 (make-temp-file "hypb" nil ".md"
                                   (hyrolo-tests--gen-outline ?# "heading-md" 1 "body-md" 2)))
         (otl-file1 (make-temp-file "hypb" nil ".otl"
                                    (hyrolo-tests--gen-outline ?* "heading-otl" 1 "body-otl" 2)))
         (kotl-file1 (hyrolo-tests--gen-kotl-outline "heading-kotl" "body-kotl" 2))
         (hyrolo-file-list (list org-file1 md-file1 otl-file1 kotl-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Move forward
          (dolist (v '("===" "^\\* heading-org 1$" "===" "^# heading-md 1$"
                       "===" "^\\* heading-otl 1$" "==="))
            (should (and (looking-at-p v) (hact 'kbd-key "f"))))
          (should (looking-at-p "^ +1\\. heading-kotl$")) ; When on last match do not move further

          ;; Move backward
          (dolist (v '("===" "^\\* heading-otl 1$" "===" "^# heading-md 1$"
                       "===" "^\\* heading-org 1$" "==="))
            (should (and (hact 'kbd-key "b") (looking-at-p v))))
          (should (= 1 (line-number-at-pos))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(defconst hyrolo-tests--outline-content-org
  "\
* h-org 1
body
** h-org 1.1
body
** h-org 1.2
body
*** h-org 1.2.1
body
* h-org 2
body
** h-org 2.1
body
"
  "Outline content for org files.")

(defun hyrolo-tests--modify-test-data (star type str)
  "Replace * with STAR and org with TYPE in STR.
Useful for creating outline and markdown test data from org examples."
  (replace-regexp-in-string
   "org" type
   (replace-regexp-in-string (regexp-quote "*") star str)))

(defconst hyrolo-tests--outline-content-otl
  (hyrolo-tests--modify-test-data "*" "otl" hyrolo-tests--outline-content-org)
  "Outline content for otl files.")

(defconst hyrolo-tests--outline-content-md
  (hyrolo-tests--modify-test-data "#" "md" hyrolo-tests--outline-content-org)
  "Outline content for markdown files.")

(ert-deftest hyrolo-tests--forward-same-level-org-level2 ()
  "Verify forward and backward to second level headers with org files."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org" hyrolo-tests--outline-content-org))
         (org-file2 (make-temp-file "hypb" nil ".org" hyrolo-tests--outline-content-org))
         (hyrolo-file-list (list org-file1 org-file2)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Move to first second level header
          (search-forward "** h-org 1.1")
          (beginning-of-line)
          (should (looking-at-p "^\\*\\* h-org 1\\.1"))

          ;; Move forward same level
          (should (and (hact 'kbd-key "f") (looking-at-p "^\\*\\* h-org 1\\.2")))

          ;; Multiple times does not move point when there are no more headers at the same level
          (should-error (hact 'kbd-key "f"))
	  (should (looking-at-p "^\\*\\* h-org 1\\.2"))

          ;; Move back on same level
          (should (and (hact 'kbd-key "b") (looking-at-p "\\*\\* h-org 1\\.1")))

          ;; Moving up from first header on a level errors, also when repeated.
          (should-error (and (hact 'kbd-key "b") (looking-at-p "^\\*\\* h-org 1\\.1")))
          (should-error (and (hact 'kbd-key "b") (looking-at-p "^\\*\\* h-org 1\\.1"))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--forward-same-level-all-file-types-level2 ()
  "Verify forward and backward to second level headers with org files."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org" hyrolo-tests--outline-content-org))
         (otl-file1 (make-temp-file "hypb" nil ".otl" hyrolo-tests--outline-content-otl))
         (md-file1 (make-temp-file "hypb" nil ".md" hyrolo-tests--outline-content-md))
         (kotl-file1 (hyrolo-tests--gen-kotl-outline "heading-kotl" "body-kotl" 2))
         (hyrolo-file-list (list org-file1 otl-file1 md-file1 kotl-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= hyrolo-display-buffer (buffer-name)))

          ;; Move to first second level header
          (search-forward "** h-org 1.1")
          (beginning-of-line)
          (should (looking-at-p "^\\*\\* h-org 1\\.1"))

          ;; Move forward same level
          (should (and (hact 'kbd-key "f") (looking-at-p "^\\*\\* h-org 1\\.2")))

          ;; Multiple times does not move point when there are no more headers at the same level
          (should-error (hact 'kbd-key "f"))
	  (should (looking-at-p "^\\*\\* h-org 1\\.2"))

          ;; Move back on same level
          (should (and (hact 'kbd-key "b") (looking-at-p "\\*\\* h-org 1\\.1")))

          ;; Moving up from first header on a level errors, also when repeated.
          (should-error (and (hact 'kbd-key "b") (looking-at-p "^\\*\\* h-org 1\\.1")))
          (should-error (and (hact 'kbd-key "b") (looking-at-p "^\\*\\* h-org 1\\.1"))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(defun hyrolo-tests--outline-as-string (&optional begin end)
  "Return buffer content as a string with hidden text replaced by ellipses.
The string contains what the outline actually looks like.  This
enables `string-match' tests for verifying text is hidden.  With
optional BEGIN and END only return that part of the buffer."
  (if (not begin) (setq begin (point-min)))
  (if (not end) (setq end (point-max)))
  (save-excursion
    (let ((result "")
          in-invisible-section)
      (goto-char begin)
      (while (< (point) end)
        (setq result
              (concat result
                      (if (get-char-property (point) 'invisible)
                          (cond ((not in-invisible-section)
                                 (setq in-invisible-section t)
                                 "...")
                                (t nil))
                        (progn
                          (if in-invisible-section
                              (setq in-invisible-section nil))
                          (char-to-string (char-after))))))
        (goto-char (1+ (point))))
      result)))

(ert-deftest hyrolo-tests--outline-hide-other ()
  "Verify `hyrolo-outline-hide-other' hides except current body, parent and top-level headings."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org" hyrolo-tests--outline-content-org))
         (hyrolo-file-list (list org-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")

          ;; First line
          (should (= (point) 1))
          (hyrolo-outline-hide-other)
          (should (string-match-p
                   (concat "^\\(===+.*[^*]\\|" (regexp-quote "...\n") "\\)"
                           (regexp-quote "\
* h-org 1...
* h-org 2...
"
                                         ) "$")
                   (hyrolo-tests--outline-as-string)))

          ;; On first header
          (goto-char (point-min))
          (hyrolo-outline-show-all)
          (search-forward "* h-org 1")
          (beginning-of-line)
          (hyrolo-outline-hide-other)
          (should (string= "\
* h-org 1
body...
* h-org 2...
"
                           (hyrolo-tests--outline-as-string (point))))

          ;; On second header
          (goto-char (point-min))
          (hyrolo-outline-show-all)
          (search-forward "** h-org 1.1")
          (beginning-of-line)
          (hyrolo-outline-hide-other)
          (should (string= "\
** h-org 1.1
body...
* h-org 2...
"
                           (hyrolo-tests--outline-as-string (point)))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))


(ert-deftest hyrolo-tests--outline-hide-sublevels ()
  "Verify `hyrolo-outline-hide-sublevels' hides everything but the top levels."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org" hyrolo-tests--outline-content-org))
         (hyrolo-file-list (list org-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")

          ;; First line
          (should (= (point) 1))
          (hyrolo-outline-hide-sublevels 1)
          (should (= (point) 1))
          (should (string-match-p
                   (concat "^\\(===+.*[^*]\\|" (regexp-quote "...\n") "\\)"
                           (regexp-quote "\
* h-org 1...
* h-org 2...
"
                                         ) "$")
                   (hyrolo-tests--outline-as-string)))

          ;; On first header
          (goto-char (point-min))
          (hyrolo-outline-show-all)
          (search-forward "* h-org 1")
          (beginning-of-line)
          (hyrolo-outline-hide-sublevels 1)
          (should (string= "\
* h-org 1...
* h-org 2...
"
                           (hyrolo-tests--outline-as-string (point))))

          ;; On second header
          (goto-char (point-min))
          (hyrolo-outline-show-all)
          (search-forward "** h-org 1.1")
          (beginning-of-line)
          (hyrolo-outline-hide-sublevels 1)
          (should (string= "\
1...
* h-org 2...
"
                           (hyrolo-tests--outline-as-string (point))))

          ;; First line - 2 levels
          (goto-char (point-min))
          (should (= (point) 1))
          (hyrolo-outline-hide-sublevels 2)
          (should (= (point) 1))
          (should (string-match-p
                   (concat "^\\(===+.*[^*]\\|" (regexp-quote "...\n") "\\)"
                           (regexp-quote "\
* h-org 1...
** h-org 1.1...
** h-org 1.2...
* h-org 2...
** h-org 2.1...
"
                                         ) "$")
                   (hyrolo-tests--outline-as-string)))

          ;; On first header - 2 levels
          (goto-char (point-min))
          (hyrolo-outline-show-all)
          (search-forward "* h-org 1")
          (beginning-of-line)
          (hyrolo-outline-hide-sublevels 2)
          (should (string= "\
* h-org 1...
** h-org 1.1...
** h-org 1.2...
* h-org 2...
** h-org 2.1...
"
                           (hyrolo-tests--outline-as-string (point))))

          ;; On second header - 2 levels
          (goto-char (point-min))
          (hyrolo-outline-show-all)
          (search-forward "** h-org 1.1")
          (beginning-of-line)
          (hyrolo-outline-hide-sublevels 2)
          (should (string= "\
** h-org 1.1...
** h-org 1.2...
* h-org 2...
** h-org 2.1...
"
                           (hyrolo-tests--outline-as-string (point)))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--hyrolo-outline-show-subtree ()
  "Verify `hyrolo-hyrolo-outline-show-subtree' shows everything after heading at deeper levels."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org" hyrolo-tests--outline-content-org))
         (hyrolo-file-list (list org-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")

          ;; First line - show all
          (should (= (point) 1))
          (let ((original-look (hyrolo-tests--outline-as-string)))
            (hyrolo-outline-hide-subtree)
            (should (string-match-p
                     (concat "^===+" (regexp-quote "...") "\n$")
                     (hyrolo-tests--outline-as-string)))
            (hyrolo-outline-show-subtree)
            (should (string= original-look (hyrolo-tests--outline-as-string))))

          ;; On first header
          (goto-char (point-min))
          (hyrolo-outline-show-all)
          (hyrolo-outline-hide-sublevels 1)
          (search-forward "* h-org 1")
          (beginning-of-line)
          (let ((original-look (hyrolo-tests--outline-as-string)))
            (hyrolo-outline-show-subtree)
            (should (string= "\
* h-org 1
body
** h-org 1.1
body
** h-org 1.2
body
*** h-org 1.2.1
body
* h-org 2...
"
                             (hyrolo-tests--outline-as-string (point))))
            ;; Hide it again
            (hyrolo-outline-hide-subtree)
            (should (string= original-look (hyrolo-tests--outline-as-string))))

          ;; On second level header
          (goto-char (point-min))
          (hyrolo-outline-show-all)
          (hyrolo-outline-hide-sublevels 2)
          (search-forward "** h-org 1.2")
          (beginning-of-line)
          (let ((original-look (hyrolo-tests--outline-as-string)))
            (hyrolo-outline-show-subtree)
            (should (string= "\
** h-org 1.2
body
*** h-org 1.2.1
body
* h-org 2...
** h-org 2.1...
"
                             (hyrolo-tests--outline-as-string (point))))
            ;; Hide it again
            (hyrolo-outline-hide-subtree)
            (should (string= original-look (hyrolo-tests--outline-as-string)))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(defun hyrolo-tests--hyrolo-section-header (filename)
  "Return a hyrolo section header for FILENAME."
  (concat
   "===============================================================================\n@loc> \""
   filename
   "\"\n"
   "===============================================================================\n"))

(ert-deftest hyrolo-tests--hyrolo-reveal-mode ()
  "Verify hidden sections are shown when using {TAB} to move through matches."
  (skip-unless (not noninteractive))
  (let* ((org-file1 (make-temp-file "hypb" nil ".org" hyrolo-tests--outline-content-org))
         (hyrolo-file-list (list org-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (hyrolo-top-level)

          (should (string=
                   (concat (hyrolo-tests--hyrolo-section-header org-file1)
                           "* h-org 1...\n* h-org 2...\n")
                   (hyrolo-tests--outline-as-string)))

          (should (hact 'kbd-key "TAB"))
	  (hy-test-helpers:consume-input-events)
	  (hy-test-helpers:consume-input-events)
          (should (string=
                   (concat (hyrolo-tests--hyrolo-section-header org-file1)
                           "* h-org 1\nbody\n** h-org 1.1\nbody\n** h-org 1.2\nbody\n*** h-org 1.2.1\nbody\n* h-org 2...\n")
                   (hyrolo-tests--outline-as-string)))

          ;; (should (hact 'kbd-key "TAB"))
          ;; (hy-test-helpers:consume-input-events)
          ;; (should (string=
          ;;          (concat (hyrolo-tests--hyrolo-section-header org-file1)
          ;;                  "* h-org 1\nbody\n** h-org 1.1\nbody\n** h-org 1.2...\n* h-org 2...\n")
          ;;          (hyrolo-tests--outline-as-string)))

          ;; (should (hact 'kbd-key "TAB"))
          ;; (hy-test-helpers:consume-input-events)
          ;; (should (string=
          ;;          (concat (hyrolo-tests--hyrolo-section-header org-file1)
          ;;                  "* h-org 1\nbody\n** h-org 1.1\nbody\n** h-org 1.2\nbody\n*** h-org 1.2.1...\n* h-org 2...\n")
          ;;          (hyrolo-tests--outline-as-string)))

          ;; (should (hact 'kbd-key "TAB"))
          ;; (hy-test-helpers:consume-input-events)
          ;; (should (string=
          ;;          (concat (hyrolo-tests--hyrolo-section-header org-file1)
          ;;                  "* h-org 1\nbody\n** h-org 1.1\nbody\n** h-org 1.2\nbody\n*** h-org 1.2.1\nbody\n* h-org 2...\n")
          ;;          (hyrolo-tests--outline-as-string)))

          ;; (should (hact 'kbd-key "TAB"))
          ;; (hy-test-helpers:consume-input-events)
          ;; (should (string=
          ;;          (concat (hyrolo-tests--hyrolo-section-header org-file1)
          ;;                  "* h-org 1\nbody\n** h-org 1.1\nbody\n** h-org 1.2\nbody\n*** h-org 1.2.1\nbody\n* h-org 2\nbody\n** h-org 2.1...\n")
          ;;          (hyrolo-tests--outline-as-string)))

          (should (hact 'kbd-key "f TAB"))
          (hy-test-helpers:consume-input-events)
          (should (string=
                   (concat (hyrolo-tests--hyrolo-section-header org-file1)
                           "* h-org 1\nbody\n** h-org 1.1\nbody\n** h-org 1.2\nbody\n*** h-org 1.2.1\nbody\n* h-org 2\nbody\n** h-org 2.1\nbody\n")
                   (hyrolo-tests--outline-as-string))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--hyrolo-reveal-mode-kotl-file ()
  "Verify hidden sections are shown when using {TAB} to move through matches in kotl."
  (let* ((kotl-file1 (hyrolo-tests--gen-kotl-outline "h-kotl" "body" 1))
         (hyrolo-file-list (list kotl-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (hyrolo-top-level)

          (should (string= (concat
                            (hyrolo-tests--hyrolo-section-header kotl-file1)
                            "\
   1. h-kotl...
")
                           (hyrolo-tests--outline-as-string)))
          (hyrolo-next-match)
          (should (string= (concat
                            (hyrolo-tests--hyrolo-section-header kotl-file1)
                            "\
   1. h-kotl
      body

     1a. h-kotl 1
         body 1
")
                           (hyrolo-tests--outline-as-string))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(defconst hyrolo-tests---org-expected-outline-for-top-level
  "\
* h-org 1...
* h-org 2...
"
  "Expected outline for org test data.")

(ert-deftest hyrolo-tests--top-level-outline-for-all-file-types ()
  "Verify `hyrolo-top-level' shows first level for all supported file types."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org" hyrolo-tests--outline-content-org))
         (otl-file1 (make-temp-file "hypb" nil ".otl" hyrolo-tests--outline-content-otl))
         (md-file1 (make-temp-file "hypb" nil ".md" hyrolo-tests--outline-content-md))
         (kotl-file1 (hyrolo-tests--gen-kotl-outline "h-kotl" "body" 2))
         (hyrolo-file-list (list org-file1 otl-file1 md-file1 kotl-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (hyrolo-top-level)

          (should (string=
                   (concat (hyrolo-tests--hyrolo-section-header org-file1)
                           hyrolo-tests---org-expected-outline-for-top-level
                           (hyrolo-tests--hyrolo-section-header otl-file1)
                           (hyrolo-tests--modify-test-data "*" "otl" hyrolo-tests---org-expected-outline-for-top-level)
                           (hyrolo-tests--hyrolo-section-header md-file1)
                           (hyrolo-tests--modify-test-data "#" "md" hyrolo-tests---org-expected-outline-for-top-level)
                           (hyrolo-tests--hyrolo-section-header kotl-file1)
                           "\
   1. h-kotl...
")
                   (hyrolo-tests--outline-as-string))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(defconst hyrolo-tests---org-expected-outline-for-overview
  "\
* h-org 1...
** h-org 1.1...
** h-org 1.2...
*** h-org 1.2.1...
* h-org 2...
** h-org 2.1...
"
  "Expected outline for org test data.")

(ert-deftest hyrolo-tests--overview-outline-for-all-file-types ()
  "Verify `hyrolo-overview' shows all level headings for all supported file types."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org" hyrolo-tests--outline-content-org))
         (otl-file1 (make-temp-file "hypb" nil ".otl" hyrolo-tests--outline-content-otl))
         (md-file1 (make-temp-file "hypb" nil ".md" hyrolo-tests--outline-content-md))
         (kotl-file1 (hyrolo-tests--gen-kotl-outline "h-kotl" "body" 2))
         (hyrolo-file-list (list org-file1 otl-file1 md-file1 kotl-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (hyrolo-overview nil)

          (should (string=
                   (concat (hyrolo-tests--hyrolo-section-header org-file1)
                           hyrolo-tests---org-expected-outline-for-overview
                           (hyrolo-tests--hyrolo-section-header otl-file1)
                           (hyrolo-tests--modify-test-data "*" "otl" hyrolo-tests---org-expected-outline-for-overview)
                           (hyrolo-tests--hyrolo-section-header md-file1)
                           (hyrolo-tests--modify-test-data "#" "md" hyrolo-tests---org-expected-outline-for-overview)
                           (hyrolo-tests--hyrolo-section-header kotl-file1)
                           "\
   1. h-kotl...
     1a. h-kotl 1...
       1a1. h-kotl 2...
")
                   (hyrolo-tests--outline-as-string))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--mail-to ()
  "Verify composing mail works."
  (let* ((org-file (make-temp-file "hypb" nil ".org" "\
* h-org 1
<first@receiver.org>
* h-org 2
<second@receiver.org>
"))
         (hyrolo-file-list (list org-file)))
    (unwind-protect
        (let ((hypb:mail-address-mode-list '(hyrolo-mode)))
          (hyrolo-grep "receiver\\.org")
          (mocklet (((compose-mail-other-window "first@receiver.org") => t))
            (hyrolo-mail-to))
          (forward-line)
          (mocklet (((compose-mail-other-window "second@receiver.org") => t))
            (hyrolo-mail-to)))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--location-movement ()
  "Verify movement between location headers."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org" hyrolo-tests--outline-content-org))
         (otl-file1 (make-temp-file "hypb" nil ".otl" hyrolo-tests--outline-content-otl))
         (hyrolo-file-list (list org-file1 otl-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (hyrolo-to-next-loc)
          (should (looking-at-p (concat "@loc> \"" org-file1 "\"")))
          (hyrolo-to-next-loc)
          (should (looking-at-p (concat "@loc> \"" otl-file1 "\"")))
          (hyrolo-to-previous-loc)
          (should (looking-at-p (concat "@loc> \"" org-file1 "\""))))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--goto-org-body-match ()
  "Move from body match to target at org file."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org" hyrolo-tests--outline-content-org))
         (hyrolo-file-list (list org-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (hyrolo-next-match)
          (action-key)
          (should (string= (hypb:buffer-file-name) org-file1))
          (should (looking-at-p "body")))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--goto-org-header-match ()
  "Move from heading match to target at org file."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org" hyrolo-tests--outline-content-org))
         (hyrolo-file-list (list org-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "h-org")
          (hyrolo-next-match)
          (action-key)
          (should (string= (hypb:buffer-file-name) org-file1))
          (should (looking-at-p "h-org 1$")))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--goto-kotl-body-match ()
  "Move from body match to target at kotl file."
  (let* ((kotl-file1 (hyrolo-tests--gen-kotl-outline "h-kotl" "body" 1))
         (hyrolo-file-list (list kotl-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (hyrolo-next-match)
          (action-key)
          (should (string= (hypb:buffer-file-name) kotl-file1))
          (should (looking-at-p "body$"))
          (should (string= (buffer-substring-no-properties (point-min) (point-max))
                           "\
   1. h-kotl
      body

     1a. h-kotl 1
         body 1

"                           )))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--goto-kotl-header-match ()
  "Move from heading match to target at first line in kotl file."
  (let* ((kotl-file1 (hyrolo-tests--gen-kotl-outline "h-kotl" "body" 1))
         (hyrolo-file-list (list kotl-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "h-kotl")
          (hyrolo-next-match)
          (action-key)
          (should (string= (hypb:buffer-file-name) kotl-file1))
          (should (looking-at-p "h-kotl$"))
          (should (string= (buffer-substring-no-properties (point-min) (point-max))
                           "\
   1. h-kotl
      body

     1a. h-kotl 1
         body 1

"                           )))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--goto-kotl-body-with-slash-match ()
  "Move from body match to target line with slash in kotl file."
  (let* ((kotl-file1 (hyrolo-tests--gen-kotl-outline "h-kotl" "body1 / body2" 1))
         (hyrolo-file-list (list kotl-file1)))
    (unwind-protect
        (progn
          (hyrolo-grep "body2")
          (hyrolo-next-match)
          (action-key)
          (should (string= (hypb:buffer-file-name) kotl-file1))
          (should (looking-at-p "body2$"))
          (should (string= (buffer-substring-no-properties (point-min) (point-max))
                           "\
   1. h-kotl
      body1 / body2

     1a. h-kotl 1
         body1 / body2 1

"                           )))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-tests--goto-kotl-header-with-slash-match ()
  "Move from heading match to target line with a slash in kotl file."
  (let* ((kotl-file1 (hyrolo-tests--gen-kotl-outline "h1 / h2" "body" 1))
         (hyrolo-file-list (list kotl-file1)))
    (unwind-protect
        (progn
	  (kotl-mode:beginning-of-buffer)
          (hyrolo-grep "h2")
          (action-key)
          (should (string= (hypb:buffer-file-name) kotl-file1))
          (should (looking-at-p "h1 / h2$"))
          (should (string= (buffer-substring-no-properties (point-min) (point-max))
                           "\
   1. h1 / h2
      body

     1a. h1 / h2 1
         body 1

"                           )))
      (kill-buffer hyrolo-display-buffer)
      (hy-delete-files-and-buffers hyrolo-file-list))))

(ert-deftest hyrolo-test--grep-count ()
  "Verify number of matched entries are correct."
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "\
* match
match
* match
other
* other
match
")
        ;; Count number of entries that have a match
        (should (= (hyrolo-grep "match" nil (current-buffer) t nil) 3))
        ;; Count number of entries that only match on the first line
        (should (= (hyrolo-grep "match" nil (current-buffer) t t) 2))
        ;; Count max number of entries
        (should (= (hyrolo-grep "match" 1 (current-buffer) t nil) 1))
        ;; Nothing if there is no match
        (should (= (hyrolo-grep "nothing" nil (current-buffer) t nil) 0)))
    (and (get-buffer hyrolo-display-buffer)
         (kill-buffer hyrolo-display-buffer)
         (ert-fail "Buffer %s should not have been created" hyrolo-display-buffer))))

(ert-deftest hyrolo-test--expand-path-list ()
  "Verify `hyrolo-expand-path-list'."
  (should (equal (hyrolo-expand-path-list nil)
		 (list (expand-file-name "~/.rolo.otl"))))
  (let ((bbdb-file nil))
    (mocklet (((hpath:expand-list
                '("/file1")
                "\\.\\(kotl?\\|org\\|ou?tl\\|md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)$"
                #'file-readable-p)
               => (list "/file1")))
      (should (equal (hyrolo-expand-path-list '("/file1")) '("/file1")))))
  (let ((bbdb-file nil))
    (mocklet (((hpath:expand-list
                '("/file1")
                "\\.\\(kotl?\\|org\\|ou?tl\\|md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)$"
                #'file-readable-p)
               => (list "/file1")))
      (should (equal (hyrolo-expand-path-list '("/file1")) '("/file1"))))))

(ert-deftest hyrolo-test--at-tags-p ()
  "Verify `hyrolo-at-tags-p´."
  (let ((orig-buffer-name (symbol-function 'buffer-name)))
    (cl-letf (((symbol-function 'buffer-name)
               (lambda (&optional buffer)
                 (if (string-prefix-p " *temp*" (funcall orig-buffer-name))
                     "*HyRolo*"
                   (funcall orig-buffer-name)))))
      (with-temp-buffer
        (should (hyrolo-at-tags-p t)))
      (with-temp-buffer
        (insert "* header  :tag1:tag2:\n")
        (goto-char (point-min))
        (search-forward ":tag1")
        (should (hyrolo-at-tags-p)))))
  (with-temp-buffer
    (org-mode)
    (mocklet (((hyrolo-get-file-list) => '("file")))
      (let ((buffer-file-name "file"))
        (should (hyrolo-at-tags-p t))))))

(provide 'hyrolo-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hyrolo-tests.el ends here
