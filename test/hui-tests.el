;ui-tests.el --- tests for hui.el Hyperbole UI          -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-Jan-21 at 12:00:00
;; Last-Mod:     25-Apr-25 at 19:50:39 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Tests for "../hui.el"

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'el-mock)
(require 'hy-test-helpers "test/hy-test-helpers")
(require 'hibtypes)
(require 'hib-kbd)
(require 'hui)
(require 'hact)
;; Remove klink actype and reload below to ensure klink priority is
;; higher than pathname (won't be if loaded before hibtypes).
(ibtype:delete 'klink)
(load "klink")

(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")

(ert-deftest hui-gbut-edit-link-to-file-button ()
  "A global button with action type link-to-file shall be possible to edit."
  (skip-unless (not noninteractive))
  (let* ((enable-recursive-minibuffers t)
	 (old-home (getenv "HOME"))
         (default-directory "/tmp")
         (hbmap:dir-user (make-temp-file "HHHHhyperbole" t))
         (hbmap:dir-filename (expand-file-name  "HBMAP" hbmap:dir-user))
         (gbut-file-buffer (find-file (gbut:file)))
         ;; (linked-file "/var/folders/8s/b7pm6fms2nsc1x2651dpvrd00000gq/T/HHHH86evcO")
         (linked-file (make-temp-file "HHHH")))
    (unwind-protect
	(progn (make-directory default-directory t)
	       (should (file-readable-p default-directory))
	       (should (file-readable-p hbmap:dir-user))
	       (cl-letf (((symbol-function 'kbd)
			  (symbol-function 'kbd-key:kbd)))
		 (write-region "" nil linked-file) ;; Ensure linked file has been created
		 (let ((create-gbut (format "abcd RET link-to-file RET %s RET y C-x C-s" linked-file))
		       (edit-gbut (format "abcd RET RET RET M-: (delete-minibuffer-contents) RET %s RET y" linked-file)))
		   (setenv "HOME" "/tmp")

		   (set-buffer gbut-file-buffer)
		   (ert-simulate-keys (kbd create-gbut)
		     (hact (lambda () (call-interactively 'hui:gbut-create))))

		   ;; Create using program
		   ;; (gbut:ebut-program "abcd" 'link-to-file linked-file)

		   (forward-char 2)
		   (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-file))

		   (goto-char (point-max)) ;; Move past button so does not prompt with label
		   (ert-simulate-keys (kbd edit-gbut)
		     (hact (lambda () (call-interactively 'hui:gbut-edit))))

		   ;; (set-buffer gbut-file-buffer)
		   (goto-char (+ (point-min) 2))
		   (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-file))
		   t)))
      (setenv "HOME" old-home)
      ;; Mainly save the temp gbut file even though it is going to be
      ;; deleted to ensure file-write-hooks are run cleanly and no
      ;; unsaved buffers are left open
      (save-excursion
	(set-buffer gbut-file-buffer)
	(save-buffer))
      (hy-delete-file-and-buffer linked-file)
      (when (file-writable-p hbmap:dir-user)
	(delete-directory hbmap:dir-user t)))))

(ert-deftest hui-gbut-number-of-gbuts-with-no-buttons ()
  "Verify number of gbuts with no buttons created."
  (defvar global-but-file)
  (let ((global-but-file (make-temp-file "gbut" nil ".txt")))
    (unwind-protect
        (mocklet ((gbut:file => global-but-file))
          (should (= 0 (length (gbut:key-list)))))
      (hy-delete-file-and-buffer global-but-file))))

(ert-deftest hui-gbut-number-of-gibuts-when-one-button ()
  "Verify number of ibuts when one button is created."
  (defvar file)
  (let ((file (make-temp-file "gbut" nil ".txt")))
    (unwind-protect
        (with-mock
          (stub gbut:file => file)
          (hui:gibut-create "global" "/tmp")
          (should (= 1 (length (gbut:ibut-key-list)))))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui-gbut-number-of-gebuts-when-one-button ()
  "Verify number of ebuts when one button is created."
  (defvar global-but-file)
  (let ((global-but-file (make-temp-file "gbut" nil ".txt")))
    (unwind-protect
        (mocklet ((gbut:file => global-but-file)
                  (hpath:find-noselect => (find-file-noselect global-but-file)))
          (gbut:ebut-program "label" 'link-to-directory "/tmp")
          (should (= 1 (length (gbut:ebut-key-list)))))
      (hy-delete-file-and-buffer global-but-file))))

(ert-deftest hui-gbut-number-of-gibuts-from-mail-mode ()
  "Verify number of global ibuts from within Hyperbole mail mode."
  (defvar global-but-file)
  (let ((global-but-file (make-temp-file "gbut" nil ".txt"))
        (message-mode-file (make-temp-file "gbut" nil ".txt")))
    (unwind-protect
        (mocklet ((gbut:file => global-but-file))
          (hui:gibut-create "global" "/tmp")
          (find-file message-mode-file)
          (message-mode)
          (should (= 1 (length (gbut:ibut-key-list)))))
      (hy-delete-file-and-buffer global-but-file)
      (hy-delete-file-and-buffer message-mode-file))))

(ert-deftest hui-gbut-number-of-gebuts-from-mail-mode ()
  "Verify number of global ebuts from within Hyperbole mail mode."
  (defvar global-but-file)
  (let ((global-but-file (make-temp-file "gbut" nil ".txt"))
        (message-mode-file (make-temp-file "gbut" nil ".txt")))
    (unwind-protect
        (mocklet ((gbut:file => global-but-file)
                  (hpath:find-noselect => (find-file-noselect global-but-file)))
          (gbut:ebut-program "label" 'link-to-directory "/tmp")
          (find-file message-mode-file)
          (message-mode)
          (should (= 1 (length (gbut:ebut-key-list)))))
      (hy-delete-file-and-buffer global-but-file)
      (hy-delete-file-and-buffer message-mode-file))))

(ert-deftest hui-ibut-label-create ()
  "Create a label for an implicit button."
  (with-temp-buffer
    (insert "\"/tmp\"\n")
    (goto-char 3)
    (ert-simulate-keys "TMP\r"
      (hui:ibut-label-create)
      (should (string= "<[TMP]> - \"/tmp\"\n" (buffer-string))))))

(ert-deftest hui-ibut-label-create-fails-if-label-exists ()
  "Creation of a label for an implicit button fails if a label exists."
  (with-temp-buffer
    (insert "<[LBL]>: \"/tmp\"\n")
    (goto-char 14)
    (ert-simulate-keys "TMP\r"
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
          (should (hact 'kbd-key "C-h h e c label RET link-to-directory RET RET"))
          (hy-test-helpers:consume-input-events)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-directory :args '("./") :loc file :lbl-key "label"))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui-ebut-create-link-to-www-url ()
  "Create an ebut with link to www-url."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (find-file file)
        (ert-simulate-keys "label\rwww-url\rwww.hypb.org\r"
          (hui:ebut-create)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::www-url :args '("www.hypb.org") :loc file :lbl-key "label"))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui-ebut-edit-link-to-www-url-keeping-all-values-should-not-modify-buffer-or-ebut ()
  "Edit an ebut keeping all initial values should not modify buffer or ebut.
Ensure modifying the button but keeping the label does not create a double label."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (find-file file)
        (ert-simulate-keys "label\rwww-url\rwww.hypb.org\r"
          (hui:ebut-create)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::www-url :args '("www.hypb.org") :loc file :lbl-key "label"))
        (ert-simulate-keys "\r\r\r\r"
          (hui:ebut-edit "label")
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::www-url :args '("www.hypb.org") :loc file :lbl-key "label")
          (should (string= "<(label)>" (buffer-string)))))
    (hy-delete-file-and-buffer file)))

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
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-directory :args '("./") :loc file :lbl-key "label"))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui-ebut-www-link ()
  "Create an ebut with an url."
  (skip-unless (not noninteractive))
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (should (hact 'kbd-key "C-h h e c label RET www-url RET www.example.com RET"))
          (hy-test-helpers:consume-input-events)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::www-url  :args '("www.example.com") :loc file :lbl-key "label"))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui-ebut-create-exec-shell-cmd ()
  "Create an ebut that executes a command."
  (skip-unless (not noninteractive))
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (should (hact 'kbd-key "C-h h e c label RET exec-shell-cmd RET ls SPC /tmp RET y n C-x C-s"))
          (hy-test-helpers:consume-input-events)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::exec-shell-cmd
					     :args '("ls /tmp" t nil) :loc file :lbl-key "label"))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui-ebut-create-link-to-info-index-using-completion ()
  "Create an ebut with link to Info index using completion for the index item."
  (skip-unless (not noninteractive))
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (should (hact 'kbd-key "C-h h e c emacs-package-button RET link-to-Info-index-item RET (emacs)packag TAB RET"))
          (hy-test-helpers:consume-input-events)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-Info-index-item
					     :args '("(emacs)Package") :loc file :lbl-key "emacs-package-button"))
      ;; There may be multiple *info* buffers, e.g. *info*<2>
      (kill-matching-buffers "^\\*info\\*" nil t)
      (hy-delete-file-and-buffer file))))

(ert-deftest hui-gibut-create-link-to-file ()
  "Programatically create implicit button link to file."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (gbut:file)) => test-buffer)
            (hui:gibut-create "global" test-file))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file :args (list test-file) :loc test-file
					       :lbl-key (ibut:label-to-key test-file)
					       :name "global")))
      (hy-delete-file-and-buffer test-file))))

(ert-deftest hui-gibut-create-link-to-file-line ()
  "Programatically create implicit button link to file and line."
  (let* ((test-file (make-temp-file "gbut" nil ".txt"))
	 (file-and-line-num (concat test-file ":10")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (gbut:file)) => test-buffer)
            (hui:gibut-create "global" file-and-line-num))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file-line :args (list test-file 10) :loc test-file
					       :lbl-key (ibut:label-to-key test-file)
					       :name "global")))
      (hy-delete-file-and-buffer test-file))))

(ert-deftest hui-gibut-create-link-to-file-line-and-column ()
  "Programatically create implicit button link to file, line and column."
  (let* ((test-file (make-temp-file "gbut" nil ".txt"))
	 (file-and-line-num-col-num (concat test-file ":10:20")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (gbut:file)) => test-buffer)
            (hui:gibut-create "global" file-and-line-num-col-num))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file-line-and-column
					       :args (list test-file 10 20) :loc test-file
					       :lbl-key (ibut:label-to-key test-file)
					       :name "global")))
      (hy-delete-file-and-buffer test-file))))

(ert-deftest hui-gibut-create-info-node ()
  "Programatically create implicit button link to info node."
  (let ((test-file (make-temp-file "gbut" nil ".txt"))
        (info-node "(hyperbole)Implicit Button"))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (gbut:file)) => test-buffer)
            (hui:gibut-create "global" (concat "\"" info-node "\"")))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-Info-node :args (list info-node) :loc test-file
					       :lbl-key (ibut:label-to-key info-node) :name "global")))
      (hy-delete-file-and-buffer test-file))))

(ert-deftest hui--delimited-selectable-thing--in-cell-return-ref ()
  "In kotl cell return klink ref."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (setq klink (klink:parse (hui:delimited-selectable-thing)))
          (should (string-match kotl-file (car klink)))
          (should (string= (cadr klink) "1=01")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest hui--delimited-selectable-thing--in-ibut-return-ibut-text ()
  "In ibut return ibut text."
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (insert file)
          (goto-char 2)
          (should (equal (hui:delimited-selectable-thing) file)))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--delimited-selectable-thing--ibut-label-return-ibut-text ()
  "In ibut label return ibut text without label."
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "<[lnk]>: " file "\n")
          (beginning-of-buffer)
          (should (equal (hui:delimited-selectable-thing) file)))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--delimited-selectable-thing--in-ebut-return-ebut-text ()
  "In ebut return ebut text."
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'exec-shell-cmd "echo abc")
          (beginning-of-buffer)
          (should (equal (hui:delimited-selectable-thing) "<(label)>")))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--delimited-selectable-thing--start-of-paired-delimiter ()
  "At start of paired delimiter return text with delimiters."
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "(xyz)\n")
          (beginning-of-buffer)
          (emacs-lisp-mode)
          (should (equal (hui:delimited-selectable-thing) "(xyz)")))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--delimited-selectable-thing--in-kcell-link-return-link ()
  "In kcell link return link."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl"))
        klink)
    (unwind-protect
        (progn
          (find-file kotl-file)
          (klink:create "1")
          (kotl-mode:beginning-of-cell)

          ;; Outside of link
          (setq klink (klink:parse (hui:delimited-selectable-thing)))
          (should (string= (cadr klink) "1=01"))
          (should (string-match kotl-file (car klink)))

          ;; Within link
          (forward-char 1)
          (should (looking-at-p "#1"))
          (setq klink (klink:parse (hui:delimited-selectable-thing)))
          (should (string= (cadr klink) "1"))
          (should (string-match kotl-file (car klink))))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest hui--kill-ring-save--yank-in-same-kotl ()
  "Yank saved klink into same kotl file."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (klink:create "1")
          (kotl-mode:beginning-of-cell)

          (forward-char 1)
          (call-interactively #'hui-kill-ring-save)

          (kotl-mode:add-cell)
          (yank)
          (kotl-mode:beginning-of-cell)
          (should (looking-at-p "<#1>"))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest hui--kill-ring-save--yank-in-other-kotl ()
  "Yank saved klink into other kotl file."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl"))
        (other-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (klink:create "1")
          (kotl-mode:beginning-of-cell)

          (forward-char 1)
          (call-interactively #'hui-kill-ring-save)

          (find-file other-file)
          (yank)
          (kotl-mode:beginning-of-cell)
          (should (looking-at-p (concat "<" (file-name-nondirectory kotl-file) "#1>")))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (hy-delete-file-and-buffer kotl-file)
      (hy-delete-file-and-buffer other-file))))

(ert-deftest hui--kill-ring-save--yank-in-other-file ()
  "Yank saved klink into other file."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl"))
        (other-file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (klink:create "1")
          (kotl-mode:beginning-of-cell)

          (forward-char 1)
          (call-interactively #'hui-kill-ring-save)

          (find-file other-file)
          (yank)
          (beginning-of-buffer)
          (should (looking-at-p (concat "<" (file-name-nondirectory kotl-file) "#1>")))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (hy-delete-file-and-buffer kotl-file)
      (hy-delete-file-and-buffer other-file))))

(ert-deftest hui--kill-ring-save--yank-in-other-file-other-dir ()
  "Yank saved klink into other file in other dir."
  (skip-unless (not noninteractive))
  (let* ((kotl-file (make-temp-file "hypb" nil ".kotl"))
         (other-dir (make-temp-file "hypb" t))
         (other-file (expand-file-name "other-file" other-dir)))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (klink:create "1")
          (kotl-mode:beginning-of-cell)

          (forward-char 1)
          (call-interactively #'hui-kill-ring-save)

          (find-file other-file)
          (yank)
          (save-buffer 0)
          (beginning-of-buffer)
          (should (looking-at-p (concat "<" kotl-file "#1>")))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (hy-delete-file-and-buffer kotl-file)
      (hy-delete-file-and-buffer other-file)
      (delete-directory other-dir))))

(ert-deftest hui--copy-to-register--yank-in-same-kotl ()
  "Yank klink in register into same kotl file."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (klink:create "1")
          (kotl-mode:beginning-of-cell)

          (forward-char 1)
          (with-mock
            (mock (register-read-with-preview  "Copy to register: ") => ?a)
            (call-interactively #'hui-copy-to-register))

          (kotl-mode:add-cell)
          (insert-register ?a)
          (kotl-mode:beginning-of-cell)
          (should (looking-at-p "<#1>"))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest hui--copy-to-register--yank-in-other-kotl ()
  "Yank klink in register into other kotl file."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl"))
        (other-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (klink:create "1")
	  (save-buffer)

          (kotl-mode:beginning-of-cell)
          (forward-char 1)
          (with-mock
            (mock (register-read-with-preview  "Copy to register: ") => ?a)
            (call-interactively #'hui-copy-to-register))

          (find-file other-file)
          (insert-register ?a)
          (kotl-mode:beginning-of-cell)
          (should (looking-at-p (concat "<" (file-name-nondirectory kotl-file) "#1>")))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (hy-delete-file-and-buffer kotl-file)
      (hy-delete-file-and-buffer other-file))))

(ert-deftest hui--copy-to-register--yank-in-other-file ()
  "Yank klink in regiuster into other file."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl"))
        (other-file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (klink:create "1")
          (kotl-mode:beginning-of-cell)

          (forward-char 1)
          (with-mock
            (mock (register-read-with-preview  "Copy to register: ") => ?a)
            (call-interactively #'hui-copy-to-register))

          (find-file other-file)
          (insert-register ?a)
          (beginning-of-buffer)
          (should (looking-at-p (concat "<" (file-name-nondirectory kotl-file) "#1>")))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (hy-delete-file-and-buffer kotl-file)
      (hy-delete-file-and-buffer other-file))))

(ert-deftest hui--copy-to-register--yank-in-other-file-other-dir ()
  "Yank klink in register into other file in other dir."
  (skip-unless (not noninteractive))
  (let* ((kotl-file (make-temp-file "hypb" nil ".kotl"))
         (other-dir (make-temp-file "hypb" t))
         (other-file (expand-file-name "other-file" other-dir)))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (klink:create "1")
          (kotl-mode:beginning-of-cell)

          (forward-char 1)
          (with-mock
            (mock (register-read-with-preview  "Copy to register: ") => ?a)
            (call-interactively #'hui-copy-to-register))

          (find-file other-file)
          (insert-register ?a)
          (save-buffer 0)
          (beginning-of-buffer)
          (should (looking-at-p (concat "<" kotl-file "#1>")))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (hy-delete-file-and-buffer kotl-file)
      (hy-delete-file-and-buffer other-file)
      (delete-directory other-dir))))

(ert-deftest hui--kill-ring-save-in-kotl-mode-copies-region ()
  "Copy region in kotl-mode does not copy left margin."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (set-mark (point))
          (insert "a")
          (kotl-mode:newline 1)
          (insert "b")
          (setq last-command #'ignore)
          (hui-kill-ring-save (region-beginning) (region-end))
          (should (string= (current-kill 0 t) "a\nb")))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest hui--kill-ring-save-in-kotl-mode-between-cells-fails ()
  "Copy region in kotl-mode between cells fails."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (set-mark (point))
          (insert "a")
          (kotl-mode:add-cell)
          (insert "b")
          (should-error (hui-kill-ring-save (region-beginning) (region-end)) :type 'error))
      (hy-delete-file-and-buffer kotl-file))))

(ert-deftest hui--ibut-create-interactive ()
  "Create an implicit button interactively."
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
	  (ert-simulate-keys "ibut\rlink-to-rfc\r123\r"
	    (hact (lambda () (call-interactively 'hui:ibut-create))))
          (should (string= "<[ibut]> - rfc123" (buffer-string))))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--ibut-create-interactive-label-using-region ()
  "Create an implicit button interactively with label from region."
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "ibut")
          (set-mark (point-min))
          (goto-char (point-max))
	  (ert-simulate-keys "\rlink-to-rfc\r123\r"
	    (hact (lambda () (call-interactively 'hui:ibut-create))))
          (should (string= "<[ibut]> - rfc123" (buffer-string))))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--ibut-create-interactive-add-comment-char ()
  "Create an implicit button interactively in program mode adds comment char."
  (let ((file (make-temp-file "hypb" nil ".el"))
        (auto-insert nil))
    (unwind-protect
        (progn
          (find-file file)
          (insert "(sexp)")
	  (ert-simulate-keys "ibut\rlink-to-rfc\r123\r"
	    (hact (lambda () (call-interactively 'hui:ibut-create))))
          (should (string= "(sexp); <[ibut]> - rfc123" (buffer-string))))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--ibut-create-interactive-create-label ()
  "Create a label for an implicit button interactively."
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "\"/tmp\"")
          (goto-char 3)
	  (ert-simulate-keys "label\r"
	    (hact (lambda () (call-interactively 'hui:ibut-label-create))))
          (should (string= "<[label]> - \"/tmp\"" (buffer-string))))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--ibut-rename-label-at-point ()
  "Rename a label for an implicit button interactively.
With point on label suggest that ibut for rename."
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "<[label]> - rfc123")
          (goto-char 3)
	  (ert-simulate-keys (kbd "M-DEL renamed RET")
	    (hact (lambda () (call-interactively 'hui:ibut-rename))))
          (should (string= "<[renamed]> - rfc123" (buffer-string))))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--ibut-rename-label ()
  "Rename a label for an implicit button interactively."
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "<[label]> - rfc123")
          (goto-char (point-max))
	  (ert-simulate-keys (kbd "label RET M-DEL renamed RET")
	    (hact (lambda () (call-interactively 'hui:ibut-rename))))
          (should (string= "<[renamed]> - rfc123" (buffer-string))))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--ibut-rename-label-not-in-buffer-errors ()
  "Rename a label not in buffer should error."
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "<[label]> - rfc123")
          (goto-char (point-max))
          (ert-simulate-keys "\r"
	    (should-error (hui:ibut-rename "notalabel") :type 'error)))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--ebut-rename ()
  "Rename an ebut shall change the name."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp")
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label"))
    (hui:ebut-rename "label" "new")
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "new"))))

(ert-deftest hui--ebut-rename-only-button-with-that-label ()
  "Rename an ebut shall change the name of only button with that label."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp")
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label"))
    (goto-char (point-max))
    (ebut:program "label2" 'link-to-directory "/tmp")
    (goto-char 3)
    (hui:ebut-rename "label" "new")
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "new"))
    (goto-char (- (point-max) 2))
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label2"))))

(ert-deftest hui--ebut-rename-nonumbered-label ()
  "Rename an ebut shall rename the label with no number."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp")
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label"))
    (goto-char (point-max))
    (ebut:program "label" 'link-to-directory "/tmp")
    (goto-char 3)
    (hui:ebut-rename "label" "new")
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "new"))
    (goto-char (- (point-max) 2))
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label:2"))))

(ert-deftest hui--ebut-rename-numbered-label ()
  "Rename an ebut shall rename the label with number."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp")
    (goto-char (point-max))
    (ebut:program "label" 'link-to-directory "/tmp")
    (goto-char (- (point-max) 1))
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label:2"))
    (hui:ebut-rename "label:2" "new")
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "new"))
    (goto-char (point-min))
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label"))))

(ert-deftest hui--ebut-rename-all-copies ()
  "Rename an ebut shall rename all copies."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp")
    (end-of-line)
    (hui-kill-ring-save (point-min) (point))
    (yank)
    (goto-char (point-min))
    (should (looking-at-p "<(label)><(label)>"))
    (hui:ebut-rename "label" "new")
    (goto-char (point-min))
    (should (looking-at-p "<(new)><(new)>"))))

(ert-deftest hui--ibut-link-directly-to-file ()
  "Create a direct link to a file."
  (let ((filea (make-temp-file "hypb" nil ".txt"))
        (fileb (make-temp-file "hypb" nil ".txt" "1234567890")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (find-file fileb)
          (goto-char (point-max))
          (split-window)
          (find-file filea)
          (hui:ibut-link-directly (get-buffer-window)
           (get-buffer-window (get-file-buffer fileb)))
          (should (string= (buffer-string) (concat "\""
				       (file-name-nondirectory fileb)
				       ":L1:C10\""))))
      (hy-delete-file-and-buffer filea)
      (hy-delete-file-and-buffer fileb))))

(ert-deftest hui--ibut-link-directly-to-dired ()
  "Create a direct link to a directory in Dired."
  (let* ((file (make-temp-file "hypb" nil ".txt"))
         (dir hyperb:dir)
         dir-buf)
    (unwind-protect
        (progn
          (delete-other-windows)
          (setq dir-buf (dired dir))
	  (goto-char (point-min))
	  ;; Move point just prior to last colon on the first dired directory line;
	  ;; With some dired formats, there may be text after the last colon.
	  (goto-char (line-end-position))
	  (skip-chars-backward "^:")
	  (when (/= (point) (point-min))
	    (goto-char (1- (point))))
          (split-window)
          (find-file file)
          (hui:ibut-link-directly (get-buffer-window) (get-buffer-window dir-buf))
	  ;; Implicit link should be the `dir' dired directory,
	  ;; possibly minus the final directory '/'.
	  (goto-char (point-min))
          (should (and (looking-at "\"")
		       (string-prefix-p (read (current-buffer)) dir))))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--ibut-link-directly-with-label ()
  "Create a direct link with a label."
  (let ((filea (make-temp-file "hypb" nil ".txt"))
        (fileb (make-temp-file "hypb" nil ".txt" "1234567890")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (find-file fileb)
          (goto-char (point-max))
          (split-window)
          (find-file filea)
          (ert-simulate-keys "label\r"
            (hui:ibut-link-directly (get-buffer-window) (get-buffer-window (get-file-buffer fileb)) 4))
          (should (string= (buffer-string) (concat "<[label]> - " "\""
				       (file-name-nondirectory fileb)
				       ":L1:C10\""))))
      (hy-delete-file-and-buffer filea)
      (hy-delete-file-and-buffer fileb))))

(ert-deftest hui--ibut-link-directly-to-org-header-first-column ()
  "Create a direct link to an org file header point in first column."
  (let ((filea (make-temp-file "hypb" nil ".txt"))
        (fileb (make-temp-file "hypb" nil ".org" "* header\nbody\n")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (find-file fileb)
          (goto-char 1)
          (split-window)
          (find-file filea)
          (hui:ibut-link-directly (get-buffer-window)
                                  (get-buffer-window (get-file-buffer fileb)))
          (should (string= (buffer-string) (concat "\"" (file-name-nondirectory fileb) "#header\"")))
          (goto-char (point-min))
          (search-forward "#")
          (action-key)
          (should (string= (buffer-name) (file-name-nondirectory fileb)))
          (should (= (point) 1)))
      (hy-delete-file-and-buffer filea)
      (hy-delete-file-and-buffer fileb))))

(ert-deftest hui--ibut-link-directly-to-org-header-second-column ()
  "Create a direct link to an org file header point in second column."
  (let ((filea (make-temp-file "hypb" nil ".txt"))
        (fileb (make-temp-file "hypb" nil ".org" "* header\nbody\n")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (find-file fileb)
          (goto-char 2)
          (split-window)
          (find-file filea)
          (hui:ibut-link-directly (get-buffer-window)
                                  (get-buffer-window (get-file-buffer fileb)))
          (should (string= (buffer-string) (concat "\"" (file-name-nondirectory fileb) "#header\"")))
          (goto-char (point-min))
          (search-forward "#")
          (action-key)
          (should (string= (buffer-name) (file-name-nondirectory fileb)))
          (should (= (point) 1)))
      (hy-delete-file-and-buffer filea)
      (hy-delete-file-and-buffer fileb))))

(ert-deftest hui--ibut-link-directly-to-org-body ()
  "Create a direct link to an org file body."
  (let ((filea (make-temp-file "hypb" nil ".txt"))
        (fileb (make-temp-file "hypb" nil ".org" "* header\nbody\n")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (find-file fileb)
          (goto-char (point-min))
          (forward-line 1)
          (should (looking-at-p "body"))
          (split-window)
          (find-file filea)
          (hui:ibut-link-directly (get-buffer-window)
                                  (get-buffer-window (get-file-buffer fileb)))
          (should (string= (buffer-string) (concat "\""
				       (file-name-nondirectory fileb)
				       ":L2\"")))
          (goto-char (point-min))
          (search-forward ":")
          (action-key)
          (should (string= (buffer-name) (file-name-nondirectory fileb)))
          (should (= (line-number-at-pos) 2)))
      (hy-delete-file-and-buffer filea)
      (hy-delete-file-and-buffer fileb))))

(ert-deftest hui--ebut-link-directly-to-file ()
  "Create a direct link to a file."
  (let ((filea (make-temp-file "hypb" nil ".txt"))
        (fileb (make-temp-file "hypb" nil ".txt" "1234567890")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (find-file fileb)
          (goto-char (point-max))
          (split-window)
          (find-file filea)
          (ert-simulate-keys "button\r"
            (hui:ebut-link-directly (get-buffer-window)
                                    (get-buffer-window (get-file-buffer fileb)))
            (should (string= (buffer-string) "<(button)>"))
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file
                                               :args (list fileb 11)
                                               :loc filea
                                               :lbl-key "button")))
      (hy-delete-file-and-buffer filea)
      (hy-delete-file-and-buffer fileb))))

(ert-deftest hui--ebut-link-directly-to-dired ()
  "Create a direct link to a directory in Dired."
  (let* ((file (make-temp-file "hypb" nil ".txt"))
         (dir hyperb:dir)
         dir-buf)
    (unwind-protect
        (progn
          (delete-other-windows)
          (setq dir-buf (dired dir))
	  (goto-char (point-min))
	  ;; Move point just prior to last colon on the first dired directory line;
	  ;; With some dired formats, there may be text after the last colon.
	  (goto-char (line-end-position))
	  (skip-chars-backward "^:")
	  (when (/= (point) (point-min))
	    (goto-char (1- (point))))
          (split-window)
          (find-file file)
          (ert-simulate-keys "button\r"
            (hui:ebut-link-directly (get-buffer-window) (get-buffer-window dir-buf))
	    ;; Implicit link should be the `dir' dired directory,
	    ;; possibly minus the final directory '/'.
            (should (string= (buffer-string) "<(button)>"))
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-directory
                                               :args (list (directory-file-name hyperb:dir)) ; Remove trailing slash!?
                                               :loc file
                                               :lbl-key "button")))
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--buf-writable-err ()
  "Verify error is signaled if buffer is not writable."
  (with-temp-buffer
    (read-only-mode)
    (condition-case err
        (hui:buf-writable-err (current-buffer) "func")
      (error
       (progn
         (should (equal (car err) 'error))
         (should (string-match
                  "(func) Read-only error in Hyperbole button buffer"
                  (cadr err))))))))

(ert-deftest hui--gbut-link-directly-ibut ()
  "Verify an ibut is created last in the global but file."
  (defvar global-but-file)
  (let ((global-but-file (make-temp-file "gbut" nil ".txt" "First\n"))
        (file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (mocklet ((gbut:file => global-but-file))
          (delete-other-windows)
          (find-file file)
          (ert-simulate-keys "button\r"
            (hui:gbut-link-directly t)
            (with-current-buffer (find-buffer-visiting global-but-file)
              (should (string= (buffer-string)
                               (concat "First\n<[button]> - \""
				       (file-name-nondirectory file)
				       ":L1\""))))))
      (hy-delete-file-and-buffer global-but-file)
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--gbut-link-directly-ebut ()
  "Verify an ebut is created last in the global but file."
  (defvar global-but-file)
  (let ((global-but-file (make-temp-file "gbut" nil ".txt" "First\n"))
        (file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (mocklet ((gbut:file => global-but-file))
          (delete-other-windows)
          (find-file file)
          (ert-simulate-keys "button\r"
            (hui:gbut-link-directly)
            (with-current-buffer (find-buffer-visiting global-but-file)
              (should (string= (buffer-string) "First\n<(button)>\n"))
              (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file
                                                 :args (list file 1)
                                                 :loc global-but-file
                                                 :lbl-key "button"))))
      (hy-delete-file-and-buffer global-but-file)
      (hy-delete-file-and-buffer file))))

(ert-deftest hui--link-possible-types ()
  "Verify right type is selected from referent buffer."

  (hsys-org-fix-version)

  ;; Org Roam or Org Id       link-to-org-id
  (let ((file (make-temp-file "hypb" nil ".org")))
    (unwind-protect
        (progn
          (find-file file)
	  (erase-buffer)
          (org-id-get-create nil)
          (re-search-forward ":ID:")
	  (hy-test-helpers:ensure-link-possible-type 'link-to-org-id))
      (hy-delete-file-and-buffer file)))

  ;; Global Button            link-to-gbut
  (defvar global-but-file)
  (let ((global-but-file (make-temp-file "gbut" nil ".txt")))
    (unwind-protect
        (mocklet ((gbut:file => global-but-file))
          (hui:gibut-create "global" "/tmp")
          (find-file global-but-file)
          (hy-test-helpers:ensure-link-possible-type 'link-to-gbut))
      (hy-delete-file-and-buffer global-but-file)))

  ;; Explicit Button          link-to-ebut
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp")
    (hy-test-helpers:ensure-link-possible-type 'link-to-ebut))

  ;; Implicit Button          link-to-ibut
  (with-temp-buffer
    (insert "<[ibut]> - \"/tmp\"")
    (goto-char 5)
    (hy-test-helpers:ensure-link-possible-type 'link-to-ibut))

  ;; Bookmarks List           link-to-bookmark
  (with-temp-buffer
    (insert "   bookmark    ~/bookmarked\n")
    (bookmark-bmenu-mode)
    (hy-test-helpers:ensure-link-possible-type 'link-to-bookmark))

  ;; Info Node                link-to-Info-node
  (with-temp-buffer
    (insert "(info)node\n")
    (goto-char 5)
    (Info-mode)
    (hy-test-helpers:ensure-link-possible-type 'link-to-Info-node))

  ;; Texinfo Node             link-to-texinfo-node
  (with-temp-buffer
    (insert "@node node\n")
    (goto-char 5)
    (texinfo-mode)
    (hy-test-helpers:ensure-link-possible-type 'link-to-texinfo-node))

  ;; Mail Reader Message      link-to-mail
  (let ((hmail:reader 'gnus-article-mode))
    (with-temp-buffer
      (gnus-article-mode)
      (mocklet ((rmail:msg-id-get => "msg-id"))
        (hy-test-helpers:ensure-link-possible-type 'link-to-mail))))

  ;; Directory Name           link-to-directory
  (let ((dir (make-temp-file "hypb" t)))
    (unwind-protect
        (let ((hargs:reading-type 'directory))
          ;; The dired case looks identical to the general dired case
          ;; below i.e. (let ((hargs:reading-type 'directory))
          ;; (hui:link-possible-types)) with cursor on a line with a
          ;; file in dired returns 'link-to-file. What is the expected
          ;; behavior?
          (with-current-buffer (dired dir)
            (goto-char 1)
            (hy-test-helpers:ensure-link-possible-type 'link-to-directory))
          (with-temp-buffer
            (insert dir)
            (goto-char 4)
            (hy-test-helpers:ensure-link-possible-type 'link-to-ibut)) ;; Expected: link-to-directory
          (with-temp-buffer
            (insert "/ssh:user@host.org:/home/user/file\n")
            (goto-char 4)
            (hy-test-helpers:ensure-link-possible-type 'link-to-ibut))) ;; Expected: link-to-directory
      (hy-delete-dir-and-buffer dir)))

  ;; File Name                link-to-file
  (let* ((temporary-file-directory (make-temp-file "hypb" t))
         (file (make-temp-file "hypb")))
    (unwind-protect
        (let ((hargs:reading-type 'file))
          (with-current-buffer (dired temporary-file-directory)
            (hy-test-helpers:ensure-link-possible-type 'link-to-file))
          (with-temp-buffer
            (insert temporary-file-directory)
            (goto-char 4)
            (hy-test-helpers:ensure-link-possible-type 'link-to-ibut)) ;; Expected: link-to-file
          (with-temp-buffer
            (insert "/ssh:user@host.org:/home/user/\n")
            (goto-char 4)
            (hy-test-helpers:ensure-link-possible-type 'link-to-ibut))) ;; Expected: link-to-file
      (hy-delete-file-and-buffer file)
      (hy-delete-dir-and-buffer temporary-file-directory)))

  ;; Koutline Cell            link-to-kcell
  (let ((file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "first")
          (hy-test-helpers:ensure-link-possible-type 'link-to-kcell))
      (hy-delete-file-and-buffer file)))

  ;; Single-line Region       link-to-string-match
  (with-temp-buffer
    (insert "a word")
    (goto-char 4)
    (mark-word)
    (hy-test-helpers:ensure-link-possible-type 'link-to-string-match))

  ;; Outline Heading          link-to-file
  (let ((file (make-temp-file "hypb" nil ".otl" "* heading\nbody\n")))
    (unwind-protect
        (progn
          (find-file file)
          (goto-char 1)
          (hy-test-helpers:ensure-link-possible-type 'link-to-file))
      (hy-delete-file-and-buffer file)))

  ;; Buffer attached to File  link-to-file
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (hy-test-helpers:ensure-link-possible-type 'link-to-file))
      (hy-delete-file-and-buffer file)))

  ;; EOL in Dired Buffer      link-to-directory (dired dir)
  (let ((dir (make-temp-file "hypb" t)))
    (unwind-protect
        (with-current-buffer (dired dir)
          (goto-char 1) ;; EOL does not seem to matter!?
          (hy-test-helpers:ensure-link-possible-type 'link-to-directory))
      (hy-delete-dir-and-buffer dir)))

  ;; Buffer without File      link-to-buffer-tmp"
  (with-temp-buffer
    (hy-test-helpers:ensure-link-possible-type 'link-to-buffer-tmp)))

(ert-deftest hui--gbut-should-execute-in-current-folder ()
  "Verify global button executes in scope of current buffer."
  (defvar global-but-file)
  (defvar global-but-buffer)
  (defvar current-folder)
  (let* ((global-but-file (make-temp-file "gbut" nil ".txt"))
         (global-but-buffer (find-file-noselect global-but-file))
         (current-folder default-directory))
    (unwind-protect
        (mocklet ((gbut:file => global-but-file)
                  ((hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => global-but-buffer))
          (gbut:ebut-program "global" 'eval-elisp ''(should (string= current-folder default-directory)))
          (gbut:act "global"))
      (hy-delete-file-and-buffer global-but-file))))

(ert-deftest hui--kill-highlighted-region-default-settings ()
  "Verify `hui-kill-region'.
The Emacs default settings are used, i.e. both `transient-mark-mode' and
`mark-even-if-inactive' are enabled."
  (with-temp-buffer
    (let ((transient-mark-mode t)
	  (mark-even-if-inactive t))
      (insert "abc{def}ghi")
      (goto-char 1)
      (set-mark nil)

      ;; No mark set
      (condition-case err
          (call-interactively #'hui-kill-region)
        (error
         (progn
           (should (memq (car err) (list 'error 'user-error)))
           (should (string-match "The mark is not set now, so there is no region" (cadr err))))))

      (set-mark (point))
      (goto-char 4)
      (call-interactively #'hui-kill-region)
      (should (string= "{def}ghi" (buffer-string)))

      (erase-buffer)
      (insert "abc{def}hig")
      (goto-char 1)
      (set-mark (point))
      (goto-char 4)
      (deactivate-mark)
      (call-interactively #'hui-kill-region)
      (should (string= "abchig" (buffer-string)))

      (erase-buffer)
      (insert "abc{def}igh")
      (goto-char 1)
      (set-mark (point))
      (goto-char 4)
      (activate-mark)
      (call-interactively #'hui-kill-region)
      (should (string= "{def}igh" (buffer-string)))

      (erase-buffer)
      (insert "bca{def}ghi")
      (goto-char 1)
      (set-mark (point))
      (goto-char 5)
      (deactivate-mark)
      (call-interactively #'hui-kill-region)
      (should (string= "def}ghi" (buffer-string)))

      ;; Not interactive
      (erase-buffer)
      (insert "cab{efd}ghi")
      (goto-char 1)
      (set-mark (point))
      (goto-char 4)
      (activate-mark)
      (hui-kill-region (mark t) (point))
      (should (string= "{efd}ghi" (buffer-string)))

      ;; Pick up region if beg or end is not set.
      (erase-buffer)
      (insert "bac{def}ghi")
      (goto-char 1)
      (set-mark (point))
      (goto-char 4)
      (deactivate-mark)
      (hui-kill-region nil nil)
      (should (string= "{def}ghi" (buffer-string))))))


(ert-deftest hui--kill-highlighted-region ()
  "Verify `hui-kill-region'.
`transient-mark-mode' is enabled and `mark-even-if-inactive' is
disabled."
  (with-temp-buffer
    (let ((transient-mark-mode t)
	  (mark-even-if-inactive nil))
      (insert "abc{def}ghi")
      (goto-char 1)
      (set-mark nil)

      ;; No mark set
      (should-error (call-interactively #'hui-kill-region) :type 'error)

      (set-mark (point))
      (goto-char 4)
      (call-interactively #'hui-kill-region)
      (should (string= "{def}ghi" (buffer-string)))

      (erase-buffer)
      (insert "abc{def}hig")
      (goto-char 1)
      (set-mark (point))
      (goto-char 4)
      (deactivate-mark)
      (call-interactively #'hui-kill-region)
      (should (string= "abchig" (buffer-string)))

      (erase-buffer)
      (insert "abc{def}igh")
      (goto-char 1)
      (set-mark (point))
      (goto-char 4)
      (activate-mark)
      (call-interactively #'hui-kill-region)
      (should (string= "{def}igh" (buffer-string)))

      (erase-buffer)
      (insert "bca{def}ghi")
      (goto-char 1)
      (set-mark (point))
      (goto-char 5)
      (deactivate-mark)
      (should-error (call-interactively #'hui-kill-region) :type 'error)

      ;; Not interactive
      (erase-buffer)
      (insert "cab{efd}ghi")
      (goto-char 1)
      (set-mark (point))
      (goto-char 4)
      (activate-mark)
      (hui-kill-region (mark t) (point))
      (should (string= "{efd}ghi" (buffer-string)))

      (erase-buffer)
      (insert "bac{def}ghi")
      (goto-char 1)
      (set-mark (point))
      (goto-char 4)
      (deactivate-mark)
      (should-error (hui-kill-region nil (point)) :type 'error))))

(ert-deftest hui--kill-non-highlighted-region ()
  "Verify `hui-kill-region'.
`transient-mark-mode' is disabled and `mark-even-if-inactive' is
enabled."
  (with-temp-buffer
    (let ((transient-mark-mode nil)
	  (mark-even-if-inactive t))
      (erase-buffer)
      (insert "abc{def}ghi")
      (goto-char 1)
      (set-mark nil)

      ;; No mark set
      (should-error (call-interactively #'hui-kill-region) :type 'error)

      (set-mark (point))
      (goto-char 4)
      (call-interactively #'hui-kill-region)
      (should (string= "{def}ghi" (buffer-string)))

      (erase-buffer)
      (insert "bca{def}hig")
      (goto-char 1)
      (set-mark (point))
      (goto-char 4)
      (call-interactively #'hui-kill-region)
      (should (string= "{def}hig" (buffer-string)))

      (erase-buffer)
      (insert "cab{def}igh")
      (goto-char 1)
      (set-mark (point))
      (goto-char 5)
      (call-interactively #'hui-kill-region)
      (should (string= "def}igh" (buffer-string)))

      ;; Not interactive

      (erase-buffer)
      (insert "acb{def}gih")
      (goto-char 1)
      (set-mark (point))
      (goto-char 4)
      (hui-kill-region (mark t) (point))
      (should (string= "{def}gih" (buffer-string)))

      (erase-buffer)
      (insert "abc{def}ghi")
      (goto-char 1)
      (set-mark nil)
      (goto-char 4)
      (should-error (hui-kill-region nil (point)) :type 'error)

      (erase-buffer)
      (insert "bca{def}hig")
      (goto-char 1)
      (set-mark (point))
      (goto-char 5)
      (hui-kill-region (mark t) (point))
      (should (string= "def}hig" (buffer-string)))

      (hui-kill-region (mark t) (point))
      (should (string= "def}hig" (buffer-string))))))

(ert-deftest hui--kill-empty-region-twice ()
  "Verify that an empty region can be killed twice.
Mimics the test case of setting a mark and hitting `C-w' twice."
  (with-temp-buffer
    (let ((transient-mark-mode t)
	  (mark-even-if-inactive t)
          last-command)
      (insert "foo bar")
      (goto-char 4)
      (set-mark (point))
      (call-interactively #'hui-kill-region)
      ;; Prepare second call to be setup as kill-region would leave
      ;; the state when calling it using C-w.
      (setq mark-active nil)
      (setq last-command #'kill-region)
      (call-interactively #'hui-kill-region))))

(ert-deftest hui--kill-region-multiple-kill ()
  "Verify `hui-kill-region' saves to the yank ring on multiple kills.
See test case `kill-whole-line-after-other-kill' and others in
simple-tests.el for prior art of forcing values on `last-command'."
  ;; Two regions
  (with-temp-buffer
    (let ((transient-mark-mode t)
	  (mark-even-if-inactive nil))
      (insert "123456")
      (goto-char 2)
      (set-mark (point))
      (goto-char 4)
      (call-interactively #'hui-kill-region)
      (goto-char 2)
      (set-mark (point))
      (goto-char 4)
      (setq last-command #'kill-region)
      (call-interactively #'hui-kill-region)
      (should (string= "16" (buffer-string)))
      (should (string= "2345" (car kill-ring)))))

  ;; Kill line followed by kill of a region
  (with-temp-buffer
    (let ((transient-mark-mode t)
	  (mark-even-if-inactive nil))
      (insert "\
line 1
1234")
      (goto-char 1)
      (set-mark (point))
      (setq last-command #'ignore)
      (kill-line 1)
      (goto-char 2)
      (set-mark (point))
      (goto-char 4)
      (setq last-command #'kill-region)
      (call-interactively #'hui-kill-region)
      (should (string= "14" (buffer-string)))
      (should (string= "line 1\n23" (car kill-ring)))))

  ;; Two consecutive kill thing
  (with-temp-buffer
    (let ((transient-mark-mode t)
	  (mark-even-if-inactive nil))
      (insert "abc{def}{ghi}jkl")
      (goto-char 1)
      (set-mark (point))
      (goto-char 4)
      (deactivate-mark)
      (setq last-command #'ignore)
      (call-interactively #'hui-kill-region)
      (setq last-command #'kill-region)
      (call-interactively #'hui-kill-region)
      (should (string= "abcjkl" (buffer-string)))
      (should (string= "{def}{ghi}" (car kill-ring))))))

;; This file can't be byte-compiled without the `el-mock' which
;; is not part of the actual dependencies, so:
;;   Local Variables:
;;   no-byte-compile: t
;;   End:

(provide 'hui-tests)
;;; hui-tests.el ends here
