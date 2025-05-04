;;; kotl-mode-tests.el --- kotl-mode-el tests            -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    18-May-21 at 22:14:10
;; Last-Mod:      4-May-25 at 11:16:56 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Tests for kotl-mode in "../kotl/kotl-mode.el"

;;; Code:

(require 'ert)
(require 'hy-test-dependencies) ;; can install el-mock
(require 'kotl-mode "kotl/kotl-mode")
(require 'hy-test-helpers "test/hy-test-helpers")

(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")

(defmacro setup-kotl-mode-example-test (&rest body)
  "Setup for test using kotl-mode:example and run BODY."
  `(unwind-protect
       (progn
         ,@body
         (should (equal major-mode 'kotl-mode))
         (should (string-prefix-p "EXAMPLE.kotl" (buffer-name (current-buffer)))))
     (kill-buffer (current-buffer))))

(ert-deftest smart-menu-loads-kotl-example ()
  "Loading kotl-mode example file works."
  (skip-unless (not noninteractive))
  (setup-kotl-mode-example-test
   ;; The additional no op key C-a below avoids ert results window to
   ;; be set as current
   (should (hact 'kbd-key "C-h h k e C-a"))
   (hy-test-helpers:consume-input-events)))

(ert-deftest kotl-mode-example-loads-kotl-example ()
  "Loading kotl-mode example file works."
  (setup-kotl-mode-example-test
   (kotl-mode:example nil t)))

(ert-deftest kotl-mode-move-between-cells ()
  "Loading kotl-mode example file works."
  (setup-kotl-mode-example-test
   ;; Start in first cell
   (kotl-mode:example temporary-file-directory t)
   (kotl-mode:beginning-of-buffer)
   (should (kotl-mode:first-cell-p))

   ;; Move to next cell
   (kotl-mode:next-cell 1)
   (should (not (kotl-mode:first-cell-p)))
   (should (equal (kcell-view:level) 1))
   (should (string= (kcell-view:visible-label) "2"))

   ;; Move to next cell
   (kotl-mode:next-cell 1)
   (should (not (kotl-mode:first-cell-p)))
   (should (equal (kcell-view:level) 2))
   (should (string= (kcell-view:visible-label) "2a")))
  )

(ert-deftest kotl-mode-indent-cell-changes-level ()
  "Loading kotl-mode example file works."
  (skip-unless (not noninteractive))
  (setup-kotl-mode-example-test
   (kotl-mode:example temporary-file-directory t)
   (kotl-mode:beginning-of-buffer)
   (should (kotl-mode:first-cell-p))
   (kotl-mode:next-cell 1)
   (should (hact 'kbd-key "TAB"))
   (hy-test-helpers:consume-input-events)
   (should (equal (kcell-view:level) 2))
   (should (string= (kcell-view:visible-label) "1a"))
   ;; Cleanup
   (set-buffer-modified-p nil)))

(ert-deftest kotl-mode-extension-open-buffer-in-kotl-mode ()
  "When a file with kotl extension is created it enters kotl mode."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl" "hej")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (should (equal major-mode 'kotl-mode)))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-set-view-with-kbd ()
  "When the view mode is changed the label is changed too."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (should (string= (kcell-view:label (point)) "1"))
          (should (hact 'kbd-key "C-c C-v 0 RET"))
          (hy-test-helpers:consume-input-events)
          (should (eq (kview:label-type kotl-kview) 'id))
          (should (string= (kcell-view:label (point)) "01")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-idstamp-saved-with-file ()
  "The active view mode is saved with the buffer."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)

          ;; Verify default label
          (should (string= (kcell-view:label (point)) "1"))

          ;; Verify idstamp label
          (kvspec:activate "ben0")
          (should (equal (kview:label-type kotl-kview) 'id))
          (should (string= (kcell-view:idstamp) "01"))
          (should (string= (kcell-view:label (point)) "01"))

          ;; Verify idstamp view is active when file is visited next time.
          (set-buffer-modified-p t)
          (save-buffer)
          (kill-buffer)
          (find-file kotl-file)
          (should (eq (kview:label-type kotl-kview) 'id))
          (should (string= (kcell-view:idstamp) "01"))
          (should (string= (kcell-view:label (point)) "01")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-kview-buffer-local ()
  "Verify kotl-kview is buffer local."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (should (string-match-p (concat "Local in buffer " (file-name-nondirectory (hypb:buffer-file-name)))
                                  (describe-variable 'kotl-kview))))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-kvspec-saved-with-file ()
  "The active view mode is saved with the file."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (should (equal (kview:label-type kotl-kview) 'alpha))
          (should (equal kvspec:current "ben"))

          (kvspec:activate "en.")
          (should (equal (kview:label-type kotl-kview) 'legal))

          ;; Verify kvspec is kept when saving and opening
          (set-buffer-modified-p t)
          (save-buffer)
          (kill-buffer)
          (find-file kotl-file)
          (should (equal kvspec:current "en."))
          (should (equal (kview:label-type kotl-kview) 'legal)))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-kvspec-independent-between-files ()
  "Modifying kvspec in one file does not affect another."
  (let ((kotl-file-a (make-temp-file "hypb" nil ".kotl"))
        (kotl-file-b (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file-a)
          (should (equal (kview:label-type kotl-kview) 'alpha))
          (should (equal kvspec:current "ben"))
          (kvspec:activate "en.")
          (should (equal (kview:label-type kotl-kview) 'legal))
          (should (equal kvspec:current "en."))

          (find-file kotl-file-b)
          (should (equal (kview:label-type kotl-kview) 'alpha))
          (should (equal kvspec:current "ben"))
          (kvspec:activate "en0")
          (should (equal (kview:label-type kotl-kview) 'id))
          (should (equal kvspec:current "en0"))

          ;; Verify kvspec is kept in kotl-file-a
          (find-file kotl-file-a)
          (should (equal (kview:label-type kotl-kview) 'legal))
          (should (equal kvspec:current "en.")))
      (hy-delete-file-and-buffer kotl-file-a)
      (hy-delete-file-and-buffer kotl-file-b))))

(ert-deftest kotl-mode-demote-keeps-idstamp ()
  "When tree is demoted the idstamp label is not changed."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (kotl-mode:add-cell)

          ;; Verify default label
          (should (string= (kcell-view:idstamp) "02"))
          (should (string= (kcell-view:label (point)) "2"))

          ;; Verify idstamp label
          (kvspec:activate "ben0")
          (should (string= (kcell-view:idstamp) "02"))
          (should (string= (kcell-view:label (point)) "02"))

          ;; Verify demote does not change idstamp label
          (kotl-mode:demote-tree 0)
          (should (string= (kcell-view:idstamp) "02"))
          (should (string= (kcell-view:label (point)) "02")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-demote-change-label ()
  "When tree is demoted the label is changed."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (kotl-mode:add-cell)

          ;; Verify default label
          (should (string= (kcell-view:label (point)) "2"))

          ;; Verify demote change label
          (kotl-mode:demote-tree 0)
          (should (string= (kcell-view:label (point)) "1a")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-label-type-activation ()
  "Kotl-mode test label type activation."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (kotl-mode:add-cell)
          (kotl-mode:demote-tree 0)

          (should (string= (kcell-view:label (point)) "1a"))

          (kvspec:activate "ben.")
          (should (string= (kcell-view:label (point)) "1.1"))

          (kvspec:activate "ben0")
          (should (string= (kcell-view:label (point)) "02")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-move-cell-before-cell ()
  "Move cell before cell."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-cell)
          (insert "second")

          (kotl-mode:move-before "2" "1" nil)
          (kotl-mode:beginning-of-buffer)

          (should (string= (kcell-view:label (point)) "1"))
          (should (looking-at-p "second")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-move-cell-after-cell ()
  "Move cell after cell."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-cell)
          (insert "second")

          (kotl-mode:beginning-of-buffer)
          (kotl-mode:move-after "1" "2" nil)

          (should (string= (kcell-view:label (point)) "2"))
          (should (looking-at-p "first")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-copy-cell-after-cell ()
  "Copy cell after cell."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-cell)
          (insert "second")

          (kotl-mode:beginning-of-buffer)
          (kotl-mode:copy-after "1" "2" nil)

          (should (string= (kcell-view:label (point)) "3"))
          (should (looking-at-p "first")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-copy-cell-before-cell ()
  "Copy cell after cell."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-cell)
          (insert "second")

          (kotl-mode:copy-before "2" "1" nil)

          (should (string= (kcell-view:label (point)) "1"))
          (should (looking-at-p "second")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-jump-to-cell ()
  "Kotl-mode jump to cell."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (kotl-mode:add-cell)

          (kotl-mode:goto-cell "1")
          (should (string= (kcell-view:label (point)) "1"))

          (kotl-mode:goto-cell "2")
          (should (string= (kcell-view:label (point)) "2")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-goto-child-and-parent ()
  "Kotl-mode goto child and goto parent."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (kotl-mode:add-child)

          (should (string= (kcell-view:label (point)) "1a"))

          (kotl-mode:up-level 1)
          (should (string= (kcell-view:label (point)) "1"))

          (kotl-mode:down-level 1)
          (should (string= (kcell-view:label (point)) "1a")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-kill-contents ()
  "Kotl-mode kill contents shall remove rest of a cell."
  (with-temp-buffer
    (kotl-mode)
    (insert "first line")
    (kotl-mode:backward-word)
    (should (looking-at-p "line"))
    (let ((transient-mark-mode nil))
      ;; kotl-mode:kill-contents uses kotl-mode:kill-region which
      ;; depends on transient-mark-mode
      (kotl-mode:kill-contents nil))
    (kotl-mode:beginning-of-cell)
    (should (looking-at-p "first $"))))

(ert-deftest kotl-mode-kill-contents-all ()
  "Kotl-mode kill contents with prefix argument shall remove the cell."
  (with-temp-buffer
    (kotl-mode)
    (insert "first line")
    (kotl-mode:backward-word)
    (should (looking-at-p "line"))
    (let ((transient-mark-mode nil))
      ;; kotl-mode:kill-contents uses kotl-mode:kill-region which
      ;; depends on transient-mark-mode
      (kotl-mode:kill-contents t))
    (kotl-mode:beginning-of-cell)
    (should (looking-at-p "$"))))

(ert-deftest kotl-mode-kill-cell ()
  "Kotl-mode kill a cell test."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (should (string= (kcell-view:idstamp) "01"))
          (kotl-mode:add-child)
          (should (string= (kcell-view:label (point)) "1a"))

          (kotl-mode:kill-tree)
          (should (string= (kcell-view:label (point)) "1"))
          (kotl-mode:beginning-of-cell)
          (should (looking-at-p "first"))
          (should (string= (kcell-view:idstamp) "01"))

          (kotl-mode:kill-tree)
          (kotl-mode:beginning-of-cell)
          (should (string= (kcell-view:idstamp) "04"))
          (should (looking-at-p "$")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-kill-tree-and-reopen ()
  "Remove first cell, reopen file, verify idstamp of first cell."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-cell)
          (insert "second")
          (should (string= (kcell-view:idstamp) "02"))
          (kotl-mode:beginning-of-buffer)
          (kotl-mode:kill-tree)
          (should (string= (kcell-view:idstamp) "02"))
          (save-buffer)
          (kill-buffer)
          (find-file kotl-file)
          (should (looking-at-p "second"))
          (should (string= (kcell-view:idstamp) "02")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-kill-tree-on-empty-file-creates-new-cell ()
  "Kill tree on empty kotl file creates new cell."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (should (string= (kcell-view:idstamp) "01"))
          (kotl-mode:kill-tree)
          (should (string= (kcell-view:idstamp) "02"))
          (kotl-mode:kill-tree)
          (should (string= (kcell-view:idstamp) "03")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-split-cell ()
  "Kotl-mode split cell."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "firstsecond\n")
          (backward-char 7)
          (kotl-mode:split-cell)
          (should (string= (kcell-view:label (point)) "2"))
          (kotl-mode:demote-tree 0)
          (should (string= (kcell-view:label (point)) "1a"))
          (should (string= (kcell-view:idstamp) "02")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-append-cell ()
  "Kotl-mode append cell to cell."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (let ((ids (kcell-view:idstamp)))
            (kotl-mode:add-cell)
            (kotl-mode:append-cell ids (kcell-view:idstamp))
            (should (looking-at-p "1"))
            (insert "2")
            (kotl-mode:append-cell ids (kcell-view:idstamp))
            (should (string= "21\n1" (substring-no-properties (kcell-view:contents))))))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-previous-cell-from-invalid-position ()
  "When in an invalid position previous cell should move back to first valid cell."
    (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (kotl-mode:add-cell)
          (insert "2")
          (kotl-mode:add-cell)
          (insert "3")

          (kotl-mode:previous-cell 1)
          (kotl-mode:end-of-cell)
          (goto-char (1+ (point)))
          (should-not (kview:valid-position-p))

          (kotl-mode:previous-cell 1)
          (should (string= (kcell-view:label (point)) "2")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-backward-cell-from-invalid-position ()
  "When in an invalid position backward cell should move back to first valid cell."
    (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (kotl-mode:add-child)
          (insert "1a")
          (kotl-mode:add-after-parent)
          (insert "2")

          (kotl-mode:previous-cell 1)
          (kotl-mode:end-of-cell)
          (goto-char (1+ (point)))
          (should-not (kview:valid-position-p))

          (kotl-mode:backward-cell 1)
          (should (string= (kcell-view:label (point)) "1a")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-backward-cell-from-invalid-pos-leave-point-in-valid-pos ()
  "From invalid pos backward cell leaves point in valid pos on error."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (kotl-mode:add-child)
          (insert "1a")
          (kotl-mode:add-after-parent)
          (insert "2")

          (kotl-mode:previous-cell 1)
          (kotl-mode:end-of-cell)
          (goto-char (1+ (point)))
          (should-not (kview:valid-position-p))

          (condition-case err
              (funcall-interactively #'kotl-mode:backward-cell 2)
            (error
             (progn
               (should (equal (car err) 'error))
               (should (string-match "(kotl-mode:backward-cell): No prior cell at same level" (cadr err))))))
          (should (kotl-mode:bocp)) ;; Point is moved to begining of cell
          (should (string= (kcell-view:label (point)) "1a")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-transpose-cell ()
  "Transpose cells and leave point in cell."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-cell)
          (insert "second")
          (kotl-mode:beginning-of-cell)
          (should (string= (kcell-view:idstamp) "02"))
          (should (looking-at-p "second"))

          (kotl-mode:transpose-cells 1)

          (should (string= (kcell-view:idstamp) "01"))
          (should (looking-at-p "first")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-transpose-cell-with-mark ()
  "Transpose cell with cell with mark and change point to mark."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (push-mark)
          (kotl-mode:add-cell)
          (insert "second")
          (kotl-mode:add-cell)
          (insert "third")
          (kotl-mode:beginning-of-cell)
          (should (string= (kcell-view:idstamp) "03"))
          (should (looking-at-p "third"))

          (kotl-mode:transpose-cells 0)

          (should (string= (kcell-view:idstamp) "03"))
          (should (looking-at-p "third"))
          (should (kotl-mode:first-cell-p)))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-transpose-cell-past-multiple-cells ()
  "Transpose cell past multiple cells."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-cell)
          (insert "second")
          (kotl-mode:add-cell)
          (insert "third")
          (kotl-mode:beginning-of-buffer)
          (should (string= (kcell-view:idstamp) "01"))
          (should (looking-at-p "first"))

          (kotl-mode:transpose-cells 2)

          ; Point moves to cell two
          (should (string= (kcell-view:idstamp) "03"))
          (should (looking-at-p "third"))

          ; Verify first cells was moved last
          (kotl-mode:end-of-buffer)
          (should (string= (kcell-view:idstamp) "01"))
          (kotl-mode:beginning-of-cell)
          (should (looking-at-p "first")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-copy-kotl-file-updates-root-id-attributes ()
  "Verify root id-attribute is updated when kotl mode is copied."
  (let* ((kotl-file (make-temp-file "hypb" nil ".kotl"))
         (indent (kcell:get-attr (kcell-view:cell-from-ref 0) 'level-indent))
         (new-name (concat (make-temp-name (concat temporary-file-directory "hypb")) ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "a cell")
          (save-buffer)
          (should (string= (kcell:get-attr (kcell-view:cell-from-ref 0) 'level-indent) indent))

          (copy-file kotl-file new-name)
          (find-file new-name)
          (should (string= (kcell:get-attr (kcell-view:cell-from-ref 0) 'level-indent) indent)))
      (hy-delete-file-and-buffer kotl-file)
      (hy-delete-file-and-buffer new-name))))

(ert-deftest kotl-mode-hide-cell ()
  "Verify cell is hidden and unhidden on `action-key' press."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (kotl-mode:newline 1)
          (insert "2")
          (kotl-mode:beginning-of-buffer)
          (action-key)                  ; Hide cell
          (forward-char 1)
          (should (outline-invisible-p))
          (action-key)                  ; Unhide cell
          (should-not (outline-invisible-p)))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-move-tree-forward ()
  "Should move tree forward."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (kotl-mode:add-cell)
          (insert "2")
          (kotl-mode:beginning-of-buffer)

          (should (string= (kcell-view:idstamp) "01"))
          (should (string= (kcell-view:label (point)) "1"))
          (should (looking-at "1"))
          (kotl-mode:move-tree-forward)
          (should (string= (kcell-view:idstamp) "01"))
          (should (string= (kcell-view:label (point)) "2"))
          (should (looking-at "1"))

          (kotl-mode:beginning-of-buffer)
          (should (string= (kcell-view:idstamp) "02"))
          (should (string= (kcell-view:label (point)) "1"))
          (should (looking-at "2")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-move-tree-backward ()
  "Should move tree backward."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (kotl-mode:add-cell)
          (insert "2")
          (kotl-mode:beginning-of-cell)

          (should (string= (kcell-view:idstamp) "02"))
          (should (string= (kcell-view:label (point)) "2"))
          (should (looking-at "2"))
          (kotl-mode:move-tree-backward)
          (should (string= (kcell-view:idstamp) "02"))
          (should (string= (kcell-view:label (point)) "1"))
          (should (looking-at "2"))

          (kotl-mode:next-cell 1)
          (should (string= (kcell-view:idstamp) "01"))
          (should (string= (kcell-view:label (point)) "2"))
          (should (looking-at "1")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode--add-cell-set-fill-attribute ()
  "Add cell shall set the fill attribute."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (should-not (kcell-view:get-attr 'no-fill))
          (kotl-mode:add-cell)
          (should-not (kcell-view:get-attr 'no-fill)))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-cell-help-displays-help-in-temp-buffer ()
  "Verify that kotl-mode:cell-help shows help in a temp buffer."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert kotl-file)
          (kotl-mode:add-child)
          (insert "1a")
          (kotl-mode:add-child)
          (insert "1a1")
          (kotl-mode:beginning-of-buffer)
          (kotl-mode:cell-help "1" nil)
          (with-current-buffer "*Help: Hyperbole Koutliner*"
            (should (looking-at-p (concat "\\W+1\\. " kotl-file)))))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-cell-help-displays-help-from-root ()
  "Verify that kotl-mode:cell-help shows help from root cell."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (kotl-mode:add-child)
          (insert kotl-file)
          (kotl-mode:add-child)
          (insert "1a1")
          (kotl-mode:beginning-of-buffer)
          (kotl-mode:cell-help "1a" 2)
          (with-current-buffer "*Help: Hyperbole Koutliner*"
            (should (looking-at-p (concat "\\W+1a\\. " kotl-file)))
            (should (= (count-matches "idstamp") 2))))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-cell-help-displays-help-for-all-cells ()
  "Verify that kotl-mode:cell-help shows help for all cells."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert kotl-file)
          (kotl-mode:add-child)
          (insert "1a")
          (kotl-mode:add-child)
          (insert "1a1")
          (kotl-mode:beginning-of-buffer)
          (kotl-mode:cell-help "1a" -1)
          (with-current-buffer "*Help: Hyperbole Koutliner*"
            (should (looking-at-p "\\W+idstamp:\\W+0"))
            (should (= (count-matches "idstamp") 4))
            (forward-line 5)
            (should (looking-at-p (concat "\\W+1\\. " kotl-file)))))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-move-between-lines ()
  "Verify movements between lines and cells."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "12")
          (kotl-mode:newline 1)
          (insert "34")
          (kotl-mode:add-cell)
          (insert "56")
          (kotl-mode:beginning-of-buffer)
          (should (looking-at-p "1"))
          (kotl-mode:previous-line 1)
          (should (looking-at-p "1"))
          (kotl-mode:next-line 1)
          (should (looking-at-p "3"))
          (kotl-mode:next-line 1)
          (should (looking-at-p "5"))
          (kotl-mode:next-line 1)
          (should (looking-at-p "5"))
          (kotl-mode:previous-line 2)
          (should (looking-at-p "1"))
          (kotl-mode:forward-char 1)
          (should (looking-at-p "2"))
          (kotl-mode:next-line 1)
          (should (looking-at-p "4"))
          (kotl-mode:next-line 1)
          (should (looking-at-p "6"))
          (kotl-mode:previous-line 1)
          (should (looking-at-p "4")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-move-up-from-first-line-shall-message-and-beep ()
  "Trying to move up from first line shall beep and output a message.
In non interactive mode there shall be no beep (nor message)"
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (with-mock
            (mock (message "(kotl-mode:previous-line): Beginning of buffer") => t)
            (mock (beep) => t)
            (funcall-interactively 'kotl-mode:previous-line 1))
          (should-error ;; Verifies no beep
           (with-mock
             (mock (beep) => t)
             (kotl-mode:previous-line 1)))
          (should-error ;; Verifies no message
           (with-mock
             (mock (message "(kotl-mode:previous-line): Beginning of buffer") => t)
             (kotl-mode:previous-line 1))))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-move-up-to-first-line ()
  "Move up to first line shall succeed with no beep nor message."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (kotl-mode:newline 1)
          (insert "2")
          (should-error ;; Verifies no beep
           (with-mock
             (mock (beep) => t)
             (funcall-interactively 'kotl-mode:previous-line 1)))
	  (should (= (line-number-at-pos) 1))
          (kotl-mode:next-line 1)
          (should-error ;; Verifies no message
           (with-mock
             (mock (message "(kotl-mode:previous-line): Beginning of buffer") => t)
             (funcall-interactively 'kotl-mode:previous-line 1)))
	  (should (= (line-number-at-pos) 1)))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-trying-to-move-down-from-last-line-shall-message-and-beep ()
  "Trying to move down from last line shall beep and output a message.
In non-interactive mode there shall be no beep nor message."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (with-mock
            (mock (message "(kotl-mode:next-line): End of buffer") => t)
            (mock (beep) => t)
            (funcall-interactively 'kotl-mode:next-line 1))
          (should-error ;; Verifies no beep
           (with-mock
             (mock (beep) => t)
             (kotl-mode:next-line 1)))
          (should-error ;; Verifies no message
           (with-mock
             (mock (message "(kotl-mode:next-line): End of buffer") => t)
             (kotl-mode:next-line 1))))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-move-down-to-last-line-shall-not-beep ()
  "Moving down to last line shall not beep."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (kotl-mode:newline 1)
          (insert "2")
          (kotl-mode:beginning-of-buffer)
          (should-error ;; Verifies no beep
           (with-mock
             (mock (beep) => t)
             (funcall-interactively 'kotl-mode:next-line 1)))
          (should (kotl-mode:last-line-p))
          (kotl-mode:beginning-of-buffer)
          (should-error ;; Verifies no message
           (with-mock
             (mock (message "(kotl-mode:next-line): End of buffer") => t)
             (funcall-interactively 'kotl-mode:next-line 1)))
          (should (kotl-mode:last-line-p)))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-move-cursor-forward-over-ellipsis ()
  "Moving cursor forward over hidden cell shall move passed ellipsis.
There is no way in a test to move past the ellipsis like a user
does when using the keyboard.  This is because the point movement
actually depends on the point adjustment heuristics."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (kotl-mode:newline 1)
          (insert "1")
          (kotl-mode:hide-tree)
          (kotl-mode:add-cell)
          (insert "2")
          (kotl-mode:beginning-of-buffer)
          (kotl-mode:forward-char)
          (should (outline-invisible-p))
          (kotl-mode:end-of-line)
          (should-not (outline-invisible-p))
          (kotl-mode:forward-char)
          (should (looking-at-p "2")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-move-cursor-backward-over-ellipsis ()
  "Moving cursor over backwards hidden cell shall move passed ellipsis.
There is no way in a test to move past the ellipsis like a user
does when using the keyboard.  This is because the point movement
actually depends on the point adjustment heuristics."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (kotl-mode:newline 1)
          (insert "1")
          (kotl-mode:hide-tree)
          (kotl-mode:add-cell)
          (insert "2")
          (kotl-mode:beginning-of-cell)
          (kotl-mode:backward-char)
          (should-not (outline-invisible-p))
          (kotl-mode:backward-char)
          (should (outline-invisible-p))
          (kotl-mode:beginning-of-line)
          (should-not (outline-invisible-p))
          (should (looking-at-p "1")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode-end-of-visible-portion ()
  "Return point if at end of visible kview cell."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (should (kotl-mode:eocp))
          (kotl-mode:newline 1)
          (insert "1")
          (should (kotl-mode:eocp))
          (kotl-mode:hide-tree)
          (should (kotl-mode:eocp))
          (kotl-mode:backward-char)
          (should-not (kotl-mode:eocp)))
      (hy-delete-file-and-buffer kotl-file))))

(defun kotl-mode-tests--gen-kotl-outline (heading body &optional depth)
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

(cl-defstruct kotl-mode-tests--func
  "Test definition struct."
  func args ignore)

(defconst kotl-mode-tests--sanity-check-function-list
  (list
    (make-kotl-mode-tests--func :func #'kfile:write :ignore t)
    (make-kotl-mode-tests--func :func #'kimport:insert-file :ignore t)
    (make-kotl-mode-tests--func :func #'kimport:insert-register :args '(?a))
    (make-kotl-mode-tests--func :func #'klink:create :args '("1a"))
    (make-kotl-mode-tests--func :func #'kotl-mode:add-after-parent)
    (make-kotl-mode-tests--func :func #'kotl-mode:add-cell)
    (make-kotl-mode-tests--func :func #'kotl-mode:add-child)
    (make-kotl-mode-tests--func :func #'kotl-mode:add-below-parent)
    (make-kotl-mode-tests--func :func #'kotl-mode:append-cell :args '("1a" "1a1"))
    (make-kotl-mode-tests--func :func #'kotl-mode:back-to-indentation)
    (make-kotl-mode-tests--func :func #'kotl-mode:backward-cell :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:backward-char)
    (make-kotl-mode-tests--func :func #'kotl-mode:backward-kill-word :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:backward-paragraph)
    (make-kotl-mode-tests--func :func #'kotl-mode:backward-sentence)
    (make-kotl-mode-tests--func :func #'kotl-mode:backward-word)
    (make-kotl-mode-tests--func :func #'kotl-mode:beginning-of-buffer)
    (make-kotl-mode-tests--func :func #'kotl-mode:beginning-of-cell)
    (make-kotl-mode-tests--func :func #'kotl-mode:beginning-of-tree)
    (make-kotl-mode-tests--func :func #'kotl-mode:cell-help)
    (make-kotl-mode-tests--func :func #'kotl-mode:center-line)
    (make-kotl-mode-tests--func :func #'kotl-mode:center-paragraph)
    (make-kotl-mode-tests--func :func #'kotl-mode:copy-after :args '("1a" "1a1" nil))
    (make-kotl-mode-tests--func :func #'kotl-mode:copy-before :args '("1a" "1a1" nil))
    (make-kotl-mode-tests--func :func #'kotl-mode:copy-to-register :args (list ?a 130 131))
    (make-kotl-mode-tests--func :func #'kotl-mode:copy-tree-or-region-to-buffer :ignore t)
    (make-kotl-mode-tests--func :func #'kotl-mode:delete-backward-char :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:delete-blank-lines)
    (make-kotl-mode-tests--func :func #'kotl-mode:delete-char :args '(-1))
    (make-kotl-mode-tests--func :func #'kotl-mode:delete-forward-char :args '(-1))
    (make-kotl-mode-tests--func :func #'kotl-mode:delete-horizontal-space)
    (make-kotl-mode-tests--func :func #'kotl-mode:delete-indentation)
    (make-kotl-mode-tests--func :func #'kotl-mode:demote-tree :ignore t)
    (make-kotl-mode-tests--func :func #'kotl-mode:down-level :ignore t)
    (make-kotl-mode-tests--func :func #'kotl-mode:end-of-buffer)
    (make-kotl-mode-tests--func :func #'kotl-mode:end-of-cell)
    (make-kotl-mode-tests--func :func #'kotl-mode:end-of-tree)
    (make-kotl-mode-tests--func :func #'kotl-mode:exchange-cells :args '("1a" "1a1"))
    (make-kotl-mode-tests--func :func #'kotl-mode:fill-cell)
    (make-kotl-mode-tests--func :func #'kotl-mode:fill-paragraph)
    (make-kotl-mode-tests--func :func #'kotl-mode:fill-tree)
    (make-kotl-mode-tests--func :func #'kotl-mode:first-sibling)
    (make-kotl-mode-tests--func :func #'kotl-mode:forward-cell :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:forward-char :args '(-1))
    (make-kotl-mode-tests--func :func #'kotl-mode:forward-paragraph)
    (make-kotl-mode-tests--func :func #'kotl-mode:forward-sentence)
    (make-kotl-mode-tests--func :func #'kotl-mode:forward-word)
    (make-kotl-mode-tests--func :func #'kotl-mode:goto-cell :args '("1a"))
    (make-kotl-mode-tests--func :func #'kotl-mode:hide-sublevels :args '(0))
    (make-kotl-mode-tests--func :func #'kotl-mode:hide-subtree)
    (make-kotl-mode-tests--func :func #'kotl-mode:hide-tree)
    (make-kotl-mode-tests--func :func #'kotl-mode:kill-contents :args '(t))
    (make-kotl-mode-tests--func :func #'kotl-mode:kill-line)
    (make-kotl-mode-tests--func :func #'kotl-mode:kill-region :args '(130 131))
    (make-kotl-mode-tests--func :func #'kotl-mode:kill-ring-save :ignore t)
    (make-kotl-mode-tests--func :func #'kotl-mode:kill-sentence :ignore t)
    (make-kotl-mode-tests--func :func #'kotl-mode:kill-tree)
    (make-kotl-mode-tests--func :func #'kotl-mode:kill-whole-line)
    (make-kotl-mode-tests--func :func #'kotl-mode:kill-word :args '(-1))
    (make-kotl-mode-tests--func :func #'kotl-mode:last-sibling)
    (make-kotl-mode-tests--func :func #'kotl-mode:left-char)
    (make-kotl-mode-tests--func :func #'kotl-mode:mail-tree :ignore t)
    (make-kotl-mode-tests--func :func #'kotl-mode:mark-paragraph)
    (make-kotl-mode-tests--func :func #'kotl-mode:mark-whole-buffer)
    (make-kotl-mode-tests--func :func #'kotl-mode:move-after :args '("1a1" "1a" nil))
    (make-kotl-mode-tests--func :func #'kotl-mode:move-before :args '("1a1" "1a" nil))
    (make-kotl-mode-tests--func :func #'kotl-mode:move-beginning-of-line)
    (make-kotl-mode-tests--func :func #'kotl-mode:move-end-of-line)
    (make-kotl-mode-tests--func :func #'kotl-mode:move-tree-backward :ignore t)
    (make-kotl-mode-tests--func :func #'kotl-mode:move-tree-forward :ignore t)
    (make-kotl-mode-tests--func :func #'kotl-mode:newline :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:next-cell :args '(-1))
    (make-kotl-mode-tests--func :func #'kotl-mode:next-line :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:open-line :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:overview)
    (make-kotl-mode-tests--func :func #'kotl-mode:previous-cell :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:previous-line :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:promote-tree :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:right-char :args '(-1))
    (make-kotl-mode-tests--func :func #'kotl-mode:scroll-down-command :ignore t)
    (make-kotl-mode-tests--func :func #'kotl-mode:scroll-up-command :ignore t)
    (make-kotl-mode-tests--func :func #'kotl-mode:set-fill-prefix :args '(t))
    (make-kotl-mode-tests--func :func #'kotl-mode:set-or-remove-cell-attribute :ignore t)
    (make-kotl-mode-tests--func :func #'kotl-mode:show-all)
    (make-kotl-mode-tests--func :func #'kotl-mode:show-tree)
    (make-kotl-mode-tests--func :func #'kotl-mode:split-cell)
    (make-kotl-mode-tests--func :func #'kotl-mode:tab-command :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:top-cells)
    (make-kotl-mode-tests--func :func #'kotl-mode:transpose-cells :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:transpose-chars :args '(-1))
    (make-kotl-mode-tests--func :func #'kotl-mode:transpose-lines :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:transpose-words :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:untab-command :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:up-level :args '(1))
    (make-kotl-mode-tests--func :func #'kotl-mode:yank)
    (make-kotl-mode-tests--func :func #'kotl-mode:yank-pop :ignore t)
    (make-kotl-mode-tests--func :func #'kotl-mode:zap-to-char :args '(1 ?y))
    (make-kotl-mode-tests--func :func #'kotl-popup-menu :ignore t)
    (make-kotl-mode-tests--func :func #'kview:set-label-separator :args '(" - "))
    (make-kotl-mode-tests--func :func #'kview:set-label-type :ignore t)
    (make-kotl-mode-tests--func :func #'kvspec:activate)
    (make-kotl-mode-tests--func :func #'kvspec:toggle-blank-lines))
  "List of functions to sanity check and their arguments if needed.
Functions that do not allow themselves to be checked in this way are
marked with :ignore t")

(defun kotl-mode--sanity-check-function (function args)
  "Check that FUNCTION called with ARGS does not throw an error."
  (let ((kotl-file (kotl-mode-tests--gen-kotl-outline "h" "body" 2)))
    (unwind-protect
        (condition-case err
            (progn
              (find-file kotl-file)
              ;; Move to middle of last line in cell to be a good
              ;; point for many commands to be able to succeed.
              (kotl-mode:beginning-of-line)
              (kotl-mode:forward-char 1)
              (let ((transient-mark-mode nil))
                (apply function args)))
          (error
           (ert-fail (format "Function %s called with args %s fails due to %s" function args err))))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest kotl-mode--sanity-check-key-bound-commands ()
  "Verify that commands bound to keys in `kotl-mode' does not throw an error."
  (set-register ?a "text")
  (dolist (f kotl-mode-tests--sanity-check-function-list)
    (unless (kotl-mode-tests--func-ignore f)
      (kotl-mode--sanity-check-function (kotl-mode-tests--func-func f)
                                        (kotl-mode-tests--func-args f)))))

(provide 'kotl-mode-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; kotl-mode-tests.el ends here
