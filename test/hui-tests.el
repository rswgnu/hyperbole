;;; hui-tests.el --- Tests for hui.el                -*- lexical-binding: t; -*-
;;
;; Author: Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date: 30-Jan-21 at 12:00:00
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Tests for "../hui.el"

;;; Code:

(require 'ert)
(require 'with-simulated-input)
(require 'hui)

(load (expand-file-name "hy-test-helpers"
                        (file-name-directory (or load-file-name
                                                 default-directory))))
(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")


(ert-deftest hui-ibut-label-create ()
  "Create a label for an implicit button."
  (with-temp-buffer
    (insert "\"/tmp\"\n")
    (goto-char 3)
    (with-simulated-input "TMP RET"
      (hui:ibut-label-create)
      (should (string= "<[TMP]> \"/tmp\"\n" (buffer-string))))))

(ert-deftest hui-ibut-label-create-fails-if-label-exists ()
  "Creation of a label for an implicit button fails if a label exists."
  (with-temp-buffer
    (insert "<[LBL]>: \"/tmp\"\n")
    (goto-char 14)
    (with-simulated-input "TMP RET"
      (condition-case err
          (hui:ibut-label-create)
        (error
         (progn
           (should (equal (car err) 'error))
           (should (string-match "ibutton at point already has a label" (cadr err)))))))))

(ert-deftest hui-ebut-create-link-to-directory ()
  "Create an ebut with link-to-directory."
  (skip-unless (not noninteractive))
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (should (hact 'kbd-key "C-h h e c label RET RET link-to-directory RET RET"))
          (hy-test-helpers:consume-input-events)
          (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
          (should (equal (hattr:get (hbut:at-p) 'args) '("./")))
          (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label")))
      (delete-file file))))

(ert-deftest hui-ebut-create-link-to-www-url ()
  "Create an ebut with link to www-url."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (find-file file)
        (with-simulated-input "label RET RET www-url RET www.hypb.org RET"
          (hui:ebut-create)
          (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::www-url))
          (should (equal (hattr:get (hbut:at-p) 'args) '("www.hypb.org")))
          (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label")))
      (delete-file file))))

(ert-deftest hui-ebut-modify-link-to-www-url-keeping-all-values-should-not-modify-buffer-or-ebut ()
  "Modify an ebut keeping all initial values should not modify buffer or ebut.
Modifying the button but keeping the label creates a dubbel label."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (find-file file)
        (with-simulated-input "label RET RET www-url RET www.hypb.org RET"
          (hui:ebut-create)
          (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::www-url))
          (should (equal (hattr:get (hbut:at-p) 'args) '("www.hypb.org")))
          (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label")))
        (with-simulated-input "RET RET RET RET"
          (hui:ebut-modify "label")
          (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::www-url))
          (should (equal (hattr:get (hbut:at-p) 'args) '("www.hypb.org")))
          (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label"))
          (should (string= "<(label)>" (buffer-string)))))
    (delete-file file)))

(ert-deftest hui-ebut-use-region-as-label ()
  "Create an ebut using region as label."
  (skip-unless (not noninteractive))
  (let ((file (make-temp-file "hypb_" nil ".txt" "label")))
    (unwind-protect
        (progn
          (find-file file)
          (beginning-of-buffer)
          (mark-word)
          (should (hact 'kbd-key "C-h h e c RET link-to-directory RET RET"))
          (hy-test-helpers:consume-input-events)
          (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
          (should (equal (hattr:get (hbut:at-p) 'args) '("./")))
          (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label")))
      (delete-file file))))

(ert-deftest hui-ebut-www-link ()
  "Create an ebut with an url."
  (skip-unless (not noninteractive))
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (should (hact 'kbd-key "C-h h e c label RET RET www-url RET www.example.com RET"))
          (hy-test-helpers:consume-input-events)
          (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::www-url))
          (should (equal (hattr:get (hbut:at-p) 'args) '("www.example.com")))
          (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label")))
      (delete-file file))))

(ert-deftest hui-ebut-create-exec-shell-cmd ()
  "Create an ebut that executes a command."
  (skip-unless (not noninteractive))
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (should (hact 'kbd-key "C-h h e c label RET RET exec-shell-cmd RET ls SPC /tmp RET y n C-x C-s"))
          (hy-test-helpers:consume-input-events)
          (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::exec-shell-cmd))
          (should (equal (hattr:get (hbut:at-p) 'args) '("ls /tmp" t nil)))
          (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label")))
      (delete-file file))))

;; This file can't be byte-compiled without `with-simulated-input' which
;; is not part of the actual dependencies, so:
;;   Local Variables:
;;   no-byte-compile: t
;;   End:

(provide 'hui-tests)
;;; hui-tests.el ends here
