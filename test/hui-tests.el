;;; hui-tests.el --- Tests for hui.el                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-Jan-21 at 12:00:00
;; Last-Mod:     23-Jul-22 at 20:06:18 by Bob Weiner
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
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
(require 'hy-test-helpers "test/hy-test-helpers")
(require 'hib-kbd)
(require 'hui)

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
		   (with-simulated-input create-gbut
		     (hact (lambda () (call-interactively 'hui:gbut-create))))

		   ;; Create using program
		   ;; (gbut:ebut-program "abcd" 'link-to-file linked-file)

		   (forward-char 2)
		   (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-file))

		   (goto-char (point-max)) ;; Move past button so does not prompt with label
		   (with-simulated-input edit-gbut
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
      (delete-file linked-file)
      (when (file-writable-p hbmap:dir-user)
	(delete-directory hbmap:dir-user t)))))

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
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-directory :args '("./") :loc file :lbl-key "label"))
      (delete-file file))))

(ert-deftest hui-ebut-create-link-to-www-url ()
  "Create an ebut with link to www-url."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (find-file file)
        (with-simulated-input "label RET RET www-url RET www.hypb.org RET"
          (hui:ebut-create)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::www-url :args '("www.hypb.org") :loc file :lbl-key "label"))
      (delete-file file))))

(ert-deftest hui-ebut-edit-link-to-www-url-keeping-all-values-should-not-modify-buffer-or-ebut ()
  "Edit an ebut keeping all initial values should not modify buffer or ebut.
Ensure modifying the button but keeping the label does not create a double label."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (find-file file)
        (with-simulated-input "label RET RET www-url RET www.hypb.org RET"
          (hui:ebut-create)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::www-url :args '("www.hypb.org") :loc file :lbl-key "label"))
        (with-simulated-input "RET RET RET RET"
          (hui:ebut-edit "label")
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::www-url :args '("www.hypb.org") :loc file :lbl-key "label")
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
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-directory :args '("./") :loc file :lbl-key "label"))
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
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::www-url  :args '("www.example.com") :loc file :lbl-key "label"))
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
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::exec-shell-cmd :args '("ls /tmp" t nil) :loc file :lbl-key "label"))
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
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-Info-index-item :args '("(emacs)Package") :loc file :lbl-key "emacs-package-button"))
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
            (mock (hpath:find-noselect (gbut:file)) => test-buffer)
            (hui:gibut-create "global" test-file))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file :args (list test-file) :loc test-file :lbl-key "global")))
      (delete-file test-file))))

(ert-deftest hui-gibut-create-link-to-file-line ()
  "Programatically create implicit button link to file and line."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (gbut:file)) => test-buffer)
            (hui:gibut-create "global" (concat test-file ":10")))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file-line :args (list test-file 10) :loc test-file :lbl-key "global")))
      (delete-file test-file))))

(ert-deftest hui-gibut-create-link-to-file-line-and-column ()
  "Programatically create implicit button link to file, line and column."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (gbut:file)) => test-buffer)
            (hui:gibut-create "global" (concat test-file ":10:20")))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file-line-and-column :args (list test-file 10 20) :loc test-file :lbl-key "global")))
      (delete-file test-file))))

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
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-Info-node :args (list info-node) :loc test-file :lbl-key "global")))
      (delete-file test-file))))

(ert-deftest hui--delimited-selectable-thing--in-cell-return-ref ()
  "In kotl cell return klink ref."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (setq klink (klink:parse (hui:delimited-selectable-thing)))
          (should (string-match kotl-file (car klink)))
          (should (string= (cadr klink) "1=01")))
      (delete-file kotl-file))))

(ert-deftest hui--delimited-selectable-thing--in-ibut-return-ibut-text ()
  "In ibut return ibut text."
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (insert file)
          (goto-char 2)
          (should (equal (hui:delimited-selectable-thing) file)))
      (delete-file file))))

(ert-deftest hui--delimited-selectable-thing--ibut-label-return-ibut-text ()
  "In ibut label return ibut text without label."
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "<[lnk]>: " file "\n")
          (beginning-of-buffer)
          (should (equal (hui:delimited-selectable-thing) file)))
      (delete-file file))))

(ert-deftest hui--delimited-selectable-thing--in-ebut-return-ebut-text ()
  "In ebut return ebut text."
  (let ((file (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'exec-shell-cmd "echo abc")
          (beginning-of-buffer)
          (should (equal (hui:delimited-selectable-thing) "<(label)>")))
      (delete-file file))))

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
      (delete-file file))))

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
          (should (looking-at-p "@ 1"))
          (setq klink (klink:parse (hui:delimited-selectable-thing)))
          (should (string= (cadr klink) "1"))
          (should (string-match kotl-file (car klink))))
      (delete-file kotl-file))))

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
          (should (looking-at-p "<@ 1>"))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (delete-file kotl-file))))

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
          (should (looking-at-p (concat "<" (file-name-nondirectory kotl-file) ", 1>")))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (delete-file kotl-file)
      (delete-file other-file))))

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
          (should (looking-at-p (concat "<" (file-name-nondirectory kotl-file) ", 1>")))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (delete-file kotl-file)
      (delete-file other-file))))

(ert-deftest hui--kill-ring-save--yank-in-other-file-other-dir ()
  "Yank saved klink into other file in other dir."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl"))
        (other-dir (make-temp-file "hypb" t))
        (other-file "other-file"))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (klink:create "1")
          (kotl-mode:beginning-of-cell)

          (forward-char 1)
          (call-interactively #'hui-kill-ring-save)

          (find-file (concat (file-name-as-directory other-dir) other-file))
          (yank)
          (save-buffer 0)
          (beginning-of-buffer)
          (should (looking-at-p (concat "<" kotl-file ", 1>")))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (delete-file kotl-file)
      (delete-file other-file)
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
          (should (looking-at-p "<@ 1>"))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (delete-file kotl-file))))

(ert-deftest hui--copy-to-register--yank-in-other-kotl ()
  "Yank klink in register into other kotl file."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl"))
        (other-file (make-temp-file "hypb" nil ".kotl")))
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
          (kotl-mode:beginning-of-cell)
          (should (looking-at-p (concat "<" (file-name-nondirectory kotl-file) ", 1>")))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (delete-file kotl-file)
      (delete-file other-file))))

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
          (should (looking-at-p (concat "<" (file-name-nondirectory kotl-file) ", 1>")))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (delete-file kotl-file)
      (delete-file other-file))))

(ert-deftest hui--copy-to-register--yank-in-other-file-other-dir ()
  "Yank klink in register into other file in other dir."
  (skip-unless (not noninteractive))
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl"))
        (other-dir (make-temp-file "hypb" t))
        (other-file "other-file"))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (klink:create "1")
          (kotl-mode:beginning-of-cell)

          (forward-char 1)
          (with-mock
            (mock (register-read-with-preview  "Copy to register: ") => ?a)
            (call-interactively #'hui-copy-to-register))

          (find-file (concat (file-name-as-directory other-dir) other-file))
          (insert-register ?a)
          (save-buffer 0)
          (beginning-of-buffer)
          (should (looking-at-p (concat "<" kotl-file ", 1>")))
          (forward-char 1)
          (should (equal (hattr:get (hbut:at-p) 'actype) 'klink:act)))
      (delete-file kotl-file)
      (delete-file other-file)
      (delete-directory other-dir))))

;; This file can't be byte-compiled without `with-simulated-input' which
;; is not part of the actual dependencies, so:
;;   Local Variables:
;;   no-byte-compile: t
;;   End:

(provide 'hui-tests)
;;; hui-tests.el ends here
