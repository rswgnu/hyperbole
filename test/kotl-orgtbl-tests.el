;;; kotl-orgtbl-tests.el --- kotl orgtbl tests            -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:     2-Nov-21 at 17:04:30
;; Last-Mod:      1-Mar-22 at 23:24:01 by Mats Lidell
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Tests for kotl-orgtbl in "../kotl/kotl-orgtbl.el"

;;; Code:

(require 'ert)
(require 'kotl-mode "kotl/kotl-mode")
(require 'hy-test-helpers "test/hy-test-helpers")

(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")

(ert-deftest kotl-orgtbl-enabled-uses-kotl-mode-delete-char-outside-of-table ()
  "kotl-mode:delete-char is used outside of org table."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "1")

          ;; Create an org table and leave point at end of cell
          (should (hact 'kbd-key "RET |field1|field2| RET"))
          (hy-test-helpers:consume-input-events)

          ;; Verify that kotl-mode:delete-char is used outside of the
          ;; table
          (condition-case err
              (progn
                (should (hact 'kbd-key "C-d"))
                (hy-test-helpers:consume-input-events))
            (error
             (progn
               (should (equal (car err) 'error))
               (should (string-match "(kotl-mode:delete-char): End of cell" (cadr err)))))
            (:success (ert-fail "C-d shall fail when deleting at the end of a cell."))))
      (delete-file kotl-file))))

(ert-deftest kotl-orgtbl-action-key-on-vertical-bar-toggles-orgtbl-mode ()
  "Action key on vertical bar toggles orgtbl-mode."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (should orgtbl-mode)

          ;; Create an org table
          (should (hact 'kbd-key "RET |field1|field2| RET"))
          (hy-test-helpers:consume-input-events)

          (left-char 1)
          (action-key)
          (should-not orgtbl-mode)
          (action-key)
          (should orgtbl-mode))
      (delete-file kotl-file))))

(ert-deftest kotl-orgtbl-shift-tab-demotes-tree-outside-table ()
  "Shift tab demotes tree outside of org table."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (should orgtbl-mode)

          (kotl-mode:add-child)
          (should (string= (kcell-view:label (point)) "1a"))

          (should (hact 'kbd-key "<S-iso-lefttab>"))
          (hy-test-helpers:consume-input-events)

          (should (equal (kcell-view:level) 1))
          (should (string= (kcell-view:label (point)) "2")))
      (delete-file kotl-file))))

(provide 'kotl-orgtbl-tests)
;;; kotl-orgtbl-tests.el ends here
