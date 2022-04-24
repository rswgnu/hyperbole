;;; kotl-mode-tests.el --- kotl-mode-el tests            -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    18-May-21 at 22:14:10
;; Last-Mod:     18-Apr-22 at 22:40:33 by Mats Lidell
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Tests for kotl-mode in "../kotl/kotl-mode.el"

;;; Code:

(require 'ert)
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
      (delete-file kotl-file))))

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
          (should (eq (kview:label-type kview) 'id))
          (should (string= (kcell-view:label (point)) "01")))
      (delete-file kotl-file))))

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
          (should (eq (kview:label-type kview) 'id))
          (should (string= (kcell-view:idstamp) "01"))
          (should (string= (kcell-view:label (point)) "01"))

          ;; Verify idstamp view is active when file is visited next time.
          (set-buffer-modified-p t)
          (save-buffer)
          (kill-buffer)
          (find-file kotl-file)
          (should (eq (kview:label-type kview) 'id))
          (should (string= (kcell-view:idstamp) "01"))
          (should (string= (kcell-view:label (point)) "01")))
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))

(ert-deftest kotl-mode-kill-cell ()
  "Kotl-mode kill a cell test."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-child)
          (should (string= (kcell-view:label (point)) "1a"))

          (kotl-mode:kill-tree)
          (should (string= (kcell-view:label (point)) "1"))
          (kotl-mode:beginning-of-cell)
          (should (looking-at-p "first"))

          (kotl-mode:kill-tree)
          (kotl-mode:beginning-of-cell)
          (should (looking-at-p "$")))
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))

(ert-deftest kotl-mode-backward-cell-from-invalid-position ()
  "When in an invalid position backward cell should move back to first valid cell."
    (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (kotl-mode:add-child)
          (insert "1a")
          (kotl-mode:add-parent)
          (insert "2")

          (kotl-mode:previous-cell 1)
          (kotl-mode:end-of-cell)
          (goto-char (1+ (point)))
          (should-not (kview:valid-position-p))

          (kotl-mode:backward-cell 1)
          (should (string= (kcell-view:label (point)) "1a")))
      (delete-file kotl-file))))

(ert-deftest kotl-mode-backward-cell-from-invalid-pos-leave-point-in-valid-pos ()
  "From invalid pos backward cell leaves point in valid pos on error."
    (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")
          (kotl-mode:add-child)
          (insert "1a")
          (kotl-mode:add-parent)
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
      (delete-file kotl-file))))

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
          (should (looking-at-p "first"))))
      (delete-file kotl-file)))

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
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))

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
      (delete-file kotl-file)
      (delete-file new-name))))

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
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))


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
      (delete-file kotl-file))))

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
      (delete-file kotl-file))))

(provide 'kotl-mode-tests)
;;; kotl-mode-tests.el ends here
