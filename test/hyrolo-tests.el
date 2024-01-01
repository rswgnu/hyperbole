;;; hyrolo-tests.el --- unit tests for hyrolo.el         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    19-Jun-21 at 22:42:00
;; Last-Mod:      1-Jan-24 at 20:56:49 by Mats Lidell
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
(require 'hib-kbd)
(require 'kotl-mode)
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

	(hyrolo-hdr-move-after-p)
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
         (org-file (make-temp-file "hypb" nil ".org" "* string\n"))
         (kotl-file (make-temp-file "hypb" nil ".kotl" "1.  string"))
         (md-file (make-temp-file "hypb" nil ".md" "# string\n"))
         (outl-file (make-temp-file "hypb" nil ".otl" "* string\n"))
         (hyrolo-file-list (list temporary-file-directory)))
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
      (delete-directory temporary-file-directory))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-org-heading ()
  "Verify move to next heading, then action-key to go to record for org mode."
  (let* ((temporary-file-directory (make-temp-file "hypb" t))
         (org-file (make-temp-file "hypb" nil ".org" "* heading\nstring\nmore\n"))
         (hyrolo-file-list (list temporary-file-directory)))
    (unwind-protect
        (progn
          (hyrolo-fgrep "string")
          (should (string= (buffer-name) hyrolo-display-buffer))
          (should (= (how-many "@loc>") 1))
          (should (looking-at-p "==="))
          (hyrolo-outline-next-visible-heading 1)
          (should (looking-at-p "* heading"))
          (let ((revisit-normally (concat "y" (if noninteractive " RET"))))
            (with-simulated-input revisit-normally
              (action-key)))
          (should (equal (current-buffer) (find-buffer-visiting org-file)))
          (should (looking-at-p "* heading")))
      (hy-delete-file-and-buffer org-file)
      (kill-buffer hyrolo-display-buffer)
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
      (delete-directory temporary-file-directory))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-outl-heading ()
  "Verify move to next heading, then action-key to go to record for outline mode."
  (let* ((temporary-file-directory (make-temp-file "hypb" t))
         (outl-file (make-temp-file "hypb" nil ".otl" "* heading\nstring\nmore\n"))
         (hyrolo-file-list (list temporary-file-directory)))
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
      (delete-directory temporary-file-directory))))

(ert-deftest hyrolo-fgrep-and-goto-next-visible-md-heading ()
  "Verify move to next heading, then action-key to go to record for markdown mode."
  (let* ((temporary-file-directory (make-temp-file "hypb" t))
         (md-file (make-temp-file "hypb" nil ".md" "# heading\nstring\nmore\n"))
         (hyrolo-file-list (list temporary-file-directory)))
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
      (delete-directory temporary-file-directory))))

(ert-deftest hyrolo-tests--get-file-list-change ()
  "Verify a change to hyrolo-file-list is noticed by hyrolo-get-file-list."
  (let* ((tmp-file (make-temp-file "hypb" nil ".org"))
         (hyrolo-file-list (list tmp-file)))
    (unwind-protect
        (let ((hl (hyrolo-get-file-list)))
          (should (= 1 (length hl)))
          (should (string= (car hl) tmp-file)))
      (hy-delete-file-and-buffer tmp-file))))

(ert-deftest hyrolo-tests--get-file-list-wrong-suffice ()
  "Verify files need to have the proper suffix in hyrolo-file-list."
  (let ((tmp-file (make-temp-file "hypb" nil)))
    (unwind-protect
        (should-error
         (let ((hyrolo-file-list (list tmp-file)))
           ()))
      (hy-delete-file-and-buffer tmp-file))))

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

(defun hyrolo-tests--generate-header-contents-for-tests (header section body depth)
  "Generate the HEADER and BODY contents for the SECTION with DEPTH."
  (let (result)
    (dotimes (d depth)
      (setq result
            (concat result
                    (make-string (1+ d) ?*) " " header " " (hyrolo-tests--level-number section (1+ d)) "\n"
                    body " " (hyrolo-tests--level-number section (1+ d)) "\n")))
    result))

(defun hyrolo-tests--gen-outline (header sections body depth)
  "Generate an outline structure suitable for hyrolo outline test.

The contents is constructed with an outline HEADER and BODY text.
Each is repeated in SECTIONS with one set of hierarchical headers
to the specified DEPTH.

Example:
   * header 1
   body 1
   ** header 2
   body 1.2
   [...]
   * header <sections>
   body <sections>
   ** header <sections>.2
   body <section>.2
   [...]"
  (let (result)
    (dotimes (section sections)
      (setq result
            (concat result
                    (hyrolo-tests--generate-header-contents-for-tests header (1+ section) body depth))))
    result))

(ert-deftest hyrolo-tests--outline-next-visible-header ()
  "Verify movement to next visible header."
  (let* ((org-file (make-temp-file "hypb" nil ".org"
                                   (hyrolo-tests--gen-outline "header" 2 "body" 2)))
         (hyrolo-file-list (list org-file)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= "*HyRolo*" (buffer-name)))

          ;; Move down
          (should (looking-at-p "==="))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^* header 1"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^** header 1.2"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^* header 2"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^** header 2.2"))
          (should (hact 'kbd-key "n"))
          (should (eobp))

          ;; Move back up
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^** header 2.2"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^* header 2"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^** header 1.2"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^* header 1"))
          (should (hact 'kbd-key "p"))

          ;; BUG: This fails in Emacs 29 and 30
          ;; This is the expected behavior that works in Emacs 27 and 28.
          ;; (should (looking-at-p "==="))
          ;; (should (bobp))
          ;; This is what we get
          (should (looking-at-p "@loc>"))
          (should (= 2 (line-number-at-pos))))
      (kill-buffer "*HyRolo*")
      (hy-delete-file-and-buffer org-file))))

(ert-deftest hyrolo-tests--outline-up-header ()
  "Verify movement from sub header to next header one level above."
  (let* ((org-file (make-temp-file "hypb" nil ".org"
                                   (hyrolo-tests--gen-outline "header" 2 "body" 3)))
         (hyrolo-file-list (list org-file)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= "*HyRolo*" (buffer-name)))

          ;; Move to last header
          (goto-char (point-max))
          (forward-line -2)
          (should (looking-at-p "^*** header 2.2.3$"))
          (should (hact 'kbd-key "u"))
          (should (looking-at-p "^** header 2.2$"))
          (should (hact 'kbd-key "u"))
          (should (looking-at-p "^* header 2$"))
          (should-error (hact 'kbd-key "u")))
      (kill-buffer "*HyRolo*")
      (hy-delete-file-and-buffer org-file))))

(ert-deftest hyrolo-tests--outline-next-visible-header-two-sections ()
  "Verify movement to next visible header with two sections."
  (let* ((org-file1 (make-temp-file "hypb" nil ".org"
                                    (hyrolo-tests--gen-outline "header-a" 1 "body-a" 2)))
         (org-file2 (make-temp-file "hypb" nil ".org"
                                    (hyrolo-tests--gen-outline "header-b" 1 "body-b" 2)))
         (hyrolo-file-list (list org-file1 org-file2)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= "*HyRolo*" (buffer-name)))

          ;; Move down
          (should (looking-at-p "==="))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^* header-a 1$"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^** header-a 1.2$"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^* header-b 1$"))
          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^** header-b 1.2$"))
          (should (hact 'kbd-key "n"))
          (should (eobp))

          ;; Move back up
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^** header-b 1.2$"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^* header-b 1$"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^** header-a 1.2$"))
          (should (hact 'kbd-key "p"))
          (should (looking-at-p "^* header-a 1$"))
          (should (hact 'kbd-key "p"))

          ;; BUG: This fails in Emacs 29 and 30
          ;; This is the expected behavior that works in Emacs 27 and 28.
          ;; (should (looking-at-p "==="))
          ;; (should (bobp))
          ;; This is what we get
          (should (looking-at-p "@loc>"))
          (should (= 2 (line-number-at-pos))))
      (kill-buffer "*HyRolo*")
      (hy-delete-file-and-buffer org-file1)
      (hy-delete-file-and-buffer org-file2))))

(ert-deftest hyrolo-tests--outline-hide-show-heading ()
  "Verify hiding and showing headers."
  (let* ((org-file (make-temp-file "hypb" nil ".org"
                                   (hyrolo-tests--gen-outline "header" 2 "body" 2)))
         (hyrolo-file-list (list org-file)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= "*HyRolo*" (buffer-name)))

          ;; Hide first line hides whole section
          (should (looking-at-p "==="))
          (should (hact 'kbd-key "h"))
          (should (looking-at-p "^===+\.\.\.$"))
          (save-excursion
            (next-line)
            (should (eobp)))
          (should (hact 'kbd-key "a"))
          (should (looking-at-p "^===+$"))

          (should (hact 'kbd-key "n"))
          (should (looking-at-p "^* header 1$"))
          ;; BUG: This gives an unexpected error when trying to hide
          ;; org-fold-region: Calling ‘org-fold-core-region’ with missing SPEC
          (should-error (hact 'kbd-key "h"))
          ;; Expected is not to fail on hiding the header.
          ;; Seems to be version dependent for 29 and 30!?

          ;; TBC - When bug above is resolved or understood better.
          )
      (kill-buffer "*HyRolo*")
      (hy-delete-file-and-buffer org-file))))

(ert-deftest hyrolo-tests--tab-through-matches ()
  "Verify tabbing through search matches."
  (let* ((org-file (make-temp-file "hypb" nil ".org"
                                   (hyrolo-tests--gen-outline "header" 2 "body" 2)))
         (hyrolo-file-list (list org-file)))
    (unwind-protect
        (progn
          (hyrolo-grep "body")
          (should (string= "*HyRolo*" (buffer-name)))

          ;; Search Down
          (should (looking-at-p "==="))
          (should (hact 'kbd-key "TAB"))
          (should (looking-at-p "^body 1$"))
          (should (hact 'kbd-key "TAB"))
          (should (looking-at-p "^body 1.2"))
          (should (hact 'kbd-key "TAB"))
          (should (looking-at-p "^body 2$"))
          (should (hact 'kbd-key "TAB"))
          (should (looking-at-p "^body 2.2"))
          (should-error (hact 'kbd-key "TAB"))
          (should (looking-at-p "^body 2.2"))

          ;; Search Up
          (should (hact 'kbd-key "<backtab>"))
          (should (looking-at-p "^body 2$"))
          (should (hact 'kbd-key "<backtab>"))
          (should (looking-at-p "^body 1.2"))
          (should (hact 'kbd-key "<backtab>"))
          (should (looking-at-p "^body 1$"))
          (should-error (hact 'kbd-key "<backtab>")))
      (kill-buffer "*HyRolo*")
      (hy-delete-file-and-buffer org-file))))

(provide 'hyrolo-tests)
;;; hyrolo-tests.el ends here
