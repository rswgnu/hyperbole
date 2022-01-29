;;; hui-tests.el --- Tests for hui.el                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-Jan-21 at 12:00:00
;; Last-Mod:     24-Jan-22 at 00:40:28 by Bob Weiner
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
(require 'el-mock)
(require 'hui)

(load (expand-file-name "hy-test-helpers"
                        (file-name-directory (or load-file-name
                                                 default-directory))))
(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")


(defun hui-tests--verify-hattr-at-p (actype args loc lbl-key)
  "Verify the attribute of hbut at point.
Checks ACTYPE, ARGS, LOC and LBL-KEY."
  (should (eq (hattr:get (hbut:at-p) 'actype) actype))
  (should (equal (hattr:get (hbut:at-p) 'args) args))
  (should (equal (hattr:get (hbut:at-p) 'loc) loc))
  (should (equal (hattr:get (hbut:at-p) 'lbl-key) lbl-key)))

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
          (hui-tests--verify-hattr-at-p 'actypes::link-to-directory '("./") file "label"))
      (delete-file file))))

(ert-deftest hui-ebut-create-link-to-www-url ()
  "Create an ebut with link to www-url."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (find-file file)
        (with-simulated-input "label RET RET www-url RET www.hypb.org RET"
          (hui:ebut-create)
          (hui-tests--verify-hattr-at-p 'actypes::www-url '("www.hypb.org") file "label"))
      (delete-file file))))

(ert-deftest hui-ebut-modify-link-to-www-url-keeping-all-values-should-not-modify-buffer-or-ebut ()
  "Modify an ebut keeping all initial values should not modify buffer or ebut.
Modifying the button but keeping the label creates a dubbel label."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (find-file file)
        (with-simulated-input "label RET RET www-url RET www.hypb.org RET"
          (hui:ebut-create)
          (hui-tests--verify-hattr-at-p 'actypes::www-url '("www.hypb.org") file "label"))
        (with-simulated-input "RET RET RET RET"
          (hui:ebut-modify "label")
          (hui-tests--verify-hattr-at-p 'actypes::www-url '("www.hypb.org") file "label")
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
          (hui-tests--verify-hattr-at-p 'actypes::link-to-directory '("./") file "label"))
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
          (hui-tests--verify-hattr-at-p  'actypes::www-url '("www.example.com") file "label"))
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
          (hui-tests--verify-hattr-at-p 'actypes::exec-shell-cmd '("ls /tmp" t nil) file "label"))
      (delete-file file))))

(ert-deftest hui-ebut-create-link-to-info-index-using-completion ()
  "Create an ebut with link to Info index using completion for the index item."
  (skip-unless (not noninteractive))
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (should (hact 'kbd-key "C-h h e c emacs-package-button RET RET link-to-Info-index-item RET (emacs)packag TAB RET"))
          (hy-test-helpers:consume-input-events)
          (hui-tests--verify-hattr-at-p 'actypes::link-to-Info-index-item '("(emacs)Package") file "emacs-package-button"))
      (progn
        (kill-buffer "*info*")
        (delete-file file)))))

(ert-deftest hui-gibut-create-link-to-file ()
  "Programatically create implicit button link to file."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (find-file-noselect gbut:file) => test-buffer)
            (hui:gibut-create "global" test-file))
	  (with-current-buffer test-buffer
            (hui-tests--verify-hattr-at-p 'actypes::link-to-file (list test-file) test-file "global")))
      (delete-file test-file))))

(ert-deftest hui-gibut-create-link-to-file-line ()
  "Programatically create implicit button link to file and line."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (find-file-noselect gbut:file) => test-buffer)
            (hui:gibut-create "global" (concat test-file ":10")))
	  (with-current-buffer test-buffer
            (hui-tests--verify-hattr-at-p 'actypes::link-to-file-line (list test-file 10) test-file "global")))
      (delete-file test-file))))

(ert-deftest hui-gibut-create-link-to-file-line-and-column ()
  "Programatically create implicit button link to file, line and column."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (find-file-noselect gbut:file) => test-buffer)
            (hui:gibut-create "global" (concat test-file ":10:20")))
	  (with-current-buffer test-buffer
            (hui-tests--verify-hattr-at-p 'actypes::link-to-file-line-and-column (list test-file 10 20) test-file "global")))
      (delete-file test-file))))

(ert-deftest hui-gibut-create-info-node ()
  "Programatically create implicit button link to info node."
  (let ((test-file (make-temp-file "gbut" nil ".txt"))
        (info-node "(hyperbole)Implicit Button"))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (find-file-noselect gbut:file) => test-buffer)
            (hui:gibut-create "global" (concat "\"" info-node "\"")))
	  (with-current-buffer test-buffer
            (hui-tests--verify-hattr-at-p 'actypes::link-to-Info-node (list info-node) test-file "global")))
      (delete-file test-file))))

;; This file can't be byte-compiled without `with-simulated-input' which
;; is not part of the actual dependencies, so:
;;   Local Variables:
;;   no-byte-compile: t
;;   End:

(provide 'hui-tests)
;;; hui-tests.el ends here
