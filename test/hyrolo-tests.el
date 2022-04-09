;;; hyrolo-tests.el --- unit tests for hyrolo.el         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    19-Jun-21 at 22:42:00
;; Last-Mod:      9-Apr-22 at 22:52:56 by Mats Lidell
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

(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")
(declare-function hy-test-helpers:should-last-message "hy-test-helpers")

(ert-deftest hyrolo-add-items-at-multiple-levels ()
  "`hyrolo-add` can add items at different levels."
  (let ((hyrolo-file (make-temp-file "hypb" nil ".otl")))
    (unwind-protect
        (let ((hyrolo-file-list (list hyrolo-file)))
          (find-file hyrolo-file)
          (insert "===\nHdr\n===\n")
          (goto-char (point-min))
          (should (looking-at "==="))
          (hyrolo-add "a")
          (hyrolo-add "a/b")
          (hyrolo-add "a/b/c")
          (beginning-of-line)
          (should (looking-at-p "\*\*\*   c")))
      (delete-file hyrolo-file))))

(ert-deftest hyrolo-demo-search-work ()
  "Use demo example and search for work should match work."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r work RET"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) "*Hyperbole Rolo*"))
        (should (looking-at "======"))
        (forward-line 5)
        (should (looking-at "\\*.*Work")))
    (hyrolo-demo-quit)))

(ert-deftest hyrolo-demo-tab-jump-to-first-match ()
  "Tab shall jump to first match."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r work RET TAB"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) "*Hyperbole Rolo*"))
        (should (looking-at "Work")))
    (hyrolo-demo-quit)))

(ert-deftest hyrolo-demo-toggle-visibility ()
  "Keys h and a shall toggle visibility."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r work RET TAB"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) "*Hyperbole Rolo*"))
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
  "Keys o shall show overview."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r work RET TAB"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) "*Hyperbole Rolo*"))
        (should (looking-at "Work"))

        (should (hact 'kbd-key "o"))
        (hy-test-helpers:consume-input-events)
        (end-of-line)
        (should-not (get-char-property (point) 'invisible))

        ;; Check second line is an outline
        (should (hact 'kbd-key "n"))
        (end-of-line)
        (should (get-char-property (point) 'invisible))

        ;; Check third line is an outline
        (should (hact 'kbd-key "n"))
        (end-of-line)
        (should (get-char-property (point) 'invisible))

        ;; Check fourth line is end of buffer
        (should (hact 'kbd-key "n"))
        (should (equal (point) (point-max))))
    (hyrolo-demo-quit)))

(ert-deftest hyrolo-demo-move-to-beginning-and-end-of-file ()
  "Keys '<', '.' and '>', ',' shall move to beginning and end of file respectively."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r work RET TAB"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) "*Hyperbole Rolo*"))
        (should (looking-at "Work"))

        (should (hact 'kbd-key ">"))
        (should (equal (point) (point-max)))

        (should (hact 'kbd-key "<"))
        (should (equal (point) (point-min)))

        (should (hact 'kbd-key ","))
        (should (equal (point) (point-max)))

        (should (hact 'kbd-key "."))
        (should (equal (point) (point-min))))
    (hyrolo-demo-quit)))

(ert-deftest hyrolo-demo-move-between-entries-on-same-level ()
  "Keys '<', '.' and '>', ',' shall move to beginning and end of file respectively."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (load "../hyrolo-demo")
        (should (hact 'kbd-key "C-x 4r com RET TAB"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name) "*Hyperbole Rolo*"))
        (should (hact 'kbd-key "<"))
        (should (equal (point) (point-min)))

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
        (should (string= (buffer-name) "*Hyperbole Rolo*"))
        (should (hact 'kbd-key "<"))
        (should (equal (point) (point-min)))

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
  "Rolo files can be sorted."
  (let ((hyrolo-file (make-temp-file "hypb" nil ".otl")))
    (unwind-protect
        (let ((hyrolo-file-list (list hyrolo-file)))
          (find-file hyrolo-file)
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
          (dolist (insertion-order '("c" "b" "d" "a"))
            (goto-char (1+ (should (search-forward insertion-order))))
            (should (looking-at-p "^\t[0-9/]+$")))

          (hyrolo-sort)

          ; Verify sorted order and following date on separate line
          (goto-char (point-min))
          (should (looking-at "==="))
          (dolist (sorted-order '("a" "b" "d" "c"))
            (goto-char (1+ (should (search-forward sorted-order))))
            (should (looking-at-p "^\t[0-9/]+$"))))
      (delete-file hyrolo-file))))

(ert-deftest hyrolo-sort-records-at-different-levels ()
  "Rolo can sort records at different levels."
  (let ((hyrolo-file (make-temp-file "hypb" nil ".otl"
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
        (let ((hyrolo-file-list (list hyrolo-file)))
          (find-file hyrolo-file)
          (hyrolo-sort)
          (should (string= (buffer-string) sorted-hyrolo-file)))
      (delete-file hyrolo-file))))

(provide 'hyrolo-tests)
;;; hyrolo-tests.el ends here
