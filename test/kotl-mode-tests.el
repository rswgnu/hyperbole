;;; kotl-mode-tests.el --- kotl-mode-el tests            -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Mats Lidell

;; Author: Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date: 18-May-21 at 22:14:10
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Tests for kotl-mode in "../kotl/kotl-mode.el"

;;; Code:

(require 'ert)
(require 'kotl-mode)

(load (expand-file-name "hy-test-helpers"
                        (file-name-directory (or load-file-name
                                                 default-directory))))
(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")

(defmacro setup-kotl-mode-example-test (&rest body)
  "Setup for test using kotl-mode:example and run BODY."
  `(unwind-protect
       (progn
         ,@body
         (should (equal major-mode 'kotl-mode))
         (should (string= (buffer-name (current-buffer)) "EXAMPLE.kotl")))
     (kill-buffer "EXAMPLE.kotl")))

(ert-deftest smart-menu-loads-kotl-example ()
  "Loading kotl-mode example file works."
  (skip-unless (not noninteractive))
  (setup-kotl-mode-example-test
   (should (hact 'kbd-key "C-h h k e"))
   (hy-test-helpers:consume-input-events)))

(ert-deftest kotl-mode-example-loads-kotl-example ()
  "Loading kotl-mode example file works."
  (setup-kotl-mode-example-test
   (kotl-mode:example)))

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

(provide 'kotl-mode-tests)
;;; kotl-mode-tests.el ends here



