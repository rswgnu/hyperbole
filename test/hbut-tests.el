;;; hbut-tests.el --- hbut unit tests         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-may-21 at 09:33:00
;; Last-Mod:     27-May-23 at 00:58:54 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; "../hbut.el"

;;; Code:

(require 'ert)
(require 'hbut)
(require 'hactypes)
(require 'hpath)
(require 'el-mock)

(defun hbut-tests:should-match-tmp-folder (tmp)
  "Check that TMP matches either a list of a single element of \"/tmp\" or \"private/tmp\".
Needed since hyperbole expands all links to absolute paths and
/tmp can be a symbolic link."
  (should (member tmp '(("/tmp") ("./tmp") ("/private/tmp")))))

(ert-deftest ebut-program-link-to-directory ()
  "Programatically create ebut with link-to-directory."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'link-to-directory "/tmp")
          (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
          (hbut-tests:should-match-tmp-folder (hattr:get (hbut:at-p) 'args))
          (should (equal (hattr:get (hbut:at-p) 'loc) file))
          (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label")))
      (delete-file file))))

(ert-deftest ebut-program-link-to-directory-2 ()
  "Programatically create ebut with link-to-directory using `temporary-file-directory`."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'link-to-directory temporary-file-directory)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-directory :args (list temporary-file-directory) :loc file :lbl-key "label"))
      (delete-file file))))

(ert-deftest ebut-program-shell-cmd ()
  "Programatically create ebut running shell command."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'exec-shell-cmd "ls /tmp")
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::exec-shell-cmd :args '("ls /tmp") :loc file :lbl-key "label"))
      (delete-file file))))

(ert-deftest ebut-delete-removes-ebut-and-returns-button-data ()
  "Remove an ebut."
  (let ((file (make-temp-file "hypb_" nil ".txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "label" 'link-to-directory "/tmp")
          (should (hbut:at-p))
          (let ((success-flag (hui:hbut-delete)))
            (should-not (hbut:at-p))
            (should (eq success-flag t))))
      (delete-file file))))

(ert-deftest gbut-program-calls-ebut-program ()
  "Programatically create gbut calls ebut:program."
  (let ((test-file (make-temp-file "test-file")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
        (with-mock
          (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
          (mock (ebut:program "label" 'link-to-directory "/tmp") => t)
          (gbut:ebut-program "label" 'link-to-directory "/tmp"))
      (delete-file test-file))))

(ert-deftest gbut-program-link-to-directory ()
  "Programatically create gbut with link-to-directory."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
            (gbut:ebut-program "global" 'link-to-directory "/tmp"))
	  (with-current-buffer test-buffer
            (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
            (hbut-tests:should-match-tmp-folder (hattr:get (hbut:at-p) 'args))
            (should (equal (hattr:get (hbut:at-p) 'loc) test-file))
            (should (equal (hattr:get (hbut:at-p) 'lbl-key) "global"))))
      (delete-file test-file))))

(ert-deftest gbut-program-eval-elisp ()
  "Programatically create gbut with eval-elisp."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
            (gbut:ebut-program "global" 'eval-elisp '()))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::eval-elisp :args  '(()) :loc test-file :lbl-key "global")))
      (delete-file test-file))))

(ert-deftest gbut-program-link-to-file ()
  "Programatically create gbut with eval-elisp."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
            (gbut:ebut-program "global" 'link-to-file test-file))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file :args (list test-file) :loc test-file :lbl-key "global")))
      (delete-file test-file))))

(ert-deftest gbut-program-link-to-file-line ()
  "Programatically create gbut with eval-elisp."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
            (gbut:ebut-program "global" 'link-to-file-line test-file 10))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file-line :args (list test-file 10) :loc test-file :lbl-key "global")))
      (delete-file test-file))))

(ert-deftest gbut-program-link-to-file-line-and-column ()
  "Programatically create gbut with eval-elisp."
  (let ((test-file (make-temp-file "gbut" nil ".txt")))
    (setq test-buffer (find-file-noselect test-file))
    (unwind-protect
	(progn
          (with-mock
            (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
            (gbut:ebut-program "global" 'link-to-file-line-and-column test-file 10 20))
	  (with-current-buffer test-buffer
            (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file-line-and-column :args (list test-file 10 20) :loc test-file :lbl-key"global")))
      (delete-file test-file))))

(ert-deftest hypb:program-create-ebut-in-buffer ()
  "Create button with hypb:program in buffer."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp")
    (should (eq (hattr:get (hbut:at-p) 'actype) 'actypes::link-to-directory))
    (hbut-tests:should-match-tmp-folder (hattr:get (hbut:at-p) 'args))
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label"))))

(ert-deftest hypb:program-create-ebut-in-buffer-with-same-label ()
  "Create button with same label shall add number so it is unique."
  (with-temp-buffer
    (ebut:program "label" 'link-to-directory "/tmp")
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label"))
    (ebut:program "label" 'link-to-directory "/tmp")
    (should (equal (hattr:get (hbut:at-p) 'lbl-key) "label:2"))))

(ert-deftest hypb:program-create-link-to-file-line-and-column-but-in-file ()
  "Create button that links to file with line and column with hypb:program in buffer."
  (let ((test-file (make-temp-file "test-file")))
    (unwind-protect
        (progn
          (find-file test-file)
          (ebut:program "label" 'link-to-file-line-and-column test-file 2 3)
          (hy-test-helpers-verify-hattr-at-p :actype 'actypes::link-to-file-line-and-column :args (list test-file 2 3) :loc test-file :lbl-key "label"))
      (delete-file test-file))))

(ert-deftest hypb--ebut-at-p-should-not-insert-hbdata-section-in-non-file-buffers ()
  "Verify that ebut:at-p does not insert a hbdata section in a non file buffer."
  (with-temp-buffer
    (let ((button "<(unknown)>"))
      (insert button)
      (goto-char 3)
      (should-not (ebut:at-p))
      (should (string= button (buffer-string))))))

(ert-deftest hbut-tests-ibut-program-link-to-directory ()
  "Programmatically create ibut link-to-directory."
  (with-temp-buffer
    (ibut:program "label" 'link-to-directory "/tmp")

    ;; Expected result.
    ;; (should (string= "<[label]> - \"/tmp\"" (buffer-string)))

    (should (string= "<[label]>\"/tmp\"" (buffer-string)))))

(ert-deftest hbut-tests-ibut-program-link-to-file ()
  "Programatically create ibut link to file."
  (let ((test-file (make-temp-file "ibut" nil ".txt")))
    (unwind-protect
        (with-temp-buffer
          (ibut:program "label" 'link-to-file test-file)

          ;; Expected result.
          ;; (should (string=
          ;;          (concat "<[label]> - \"" test-file "\"")
          ;;          (buffer-string))))

          (should (string=
                   (concat "<[label]>\"" test-file "\"")
                   (buffer-string))))
      (delete-file test-file))))

(ert-deftest hbut-tests-ibut-insert-text-link-to-dir ()
  "Insert link to dir."
  (with-temp-buffer
    (ibut:program "label" 'link-to-directory "/tmp")

    ;; Expected result.
    ;; (should (string= "<[label]> - \"/tmp\"" (buffer-string)))

    (should (string= "<[label]>\"/tmp\"" (buffer-string)))

    ;; Picking up ibut with ibut:at-p and inserting it again.
    ;; (goto-char 3)
    ;; (let ((but (ibut:at-p)))
    ;;   (with-temp-buffer
    ;;     (ibut:insert-text but)
    ;;     (should (string= "\"/tmp\"" (buffer-string)))))
    ))

(ert-deftest hbut-tests-ibut-insert-annot-bib ()
  "Insert ibut to annot-bib."
  (with-temp-buffer
    (ibut:program "label" 'annot-bib "arg")

    ;; Expected result.
    ;; (should (string= "<[label]> - [arg]" (buffer-string)))

    (should (string= "<[label]>[arg]" (buffer-string)))

    ;; Picking up ibut with ibut:at-p and inserting it again.
    ;; (goto-char 3)
    ;; (let ((but (ibut:at-p)))
    ;;   (with-temp-buffer
    ;;     (ibut:insert-text but)
    ;;     (should (string= "[arg]" (buffer-string)))))
    ))

(ert-deftest hbut-tests-ibut-insert-kbd-key ()
  "Insert ibut to kbd-key."
  (with-temp-buffer
    (ibut:program "label" 'kbd-key "{ C-h h }")

    ;; Expected result.
    ;; (should (string= "<[label]> - { C-h h }" (buffer-string)))

    (should (string= "<[label]>{{ C-h h }}" (buffer-string)))

    ;; Picking up ibut with ibut:at-p and inserting it again.
    ;; (goto-char 3)
    ;; (let ((but (ibut:at-p)))
    ;;   (with-temp-buffer
    ;;     (ibut:insert-text but)
    ;;     (should (string= "" (buffer-string)))))
    ))


(ert-deftest hbut-tests-ibut-insert-text-temp-buffer ()
  "Insert ibut text using an ibut in a temp buffer as source."
  (dolist (bd
           ;; Test data is in the format:
           ;;  - action-type implicit-button-text resulting-button-text
           ;; The third parameter is optional and if missing the
           ;; implicit-button-text is used as the result as
           ;; well. i.e. Works when the input string is preserved.
           '(
             ('actypes::kbd-key "{C-h h}")
             ; ('actypes::annotate-bib "[FSF 12]") ;; Requires a file!?
             ; ('actypes::exec-shell-cmd "!/bin/bash") ;; Not identified as an ibut
             ; ('actypes::exec-window-cmd "&/bin/bash") ;; Not identified as an ibut
             ('actypes::link-to-gbut "<glink:arg1>")
             ('actypes::link-to-ebut "<elink:arg1>")
             ('actypes::link-to-ebut "<elink:arg1:arg2>" "<elink:arg1: arg2>")
             ('actypes::link-to-ibut "<ilink:arg1>")
             ('actypes::link-to-ibut "<ilink:arg1:arg2>" "<ilink:arg1: arg2>")
             ('actypes::link-to-kcell "<@ 3b=06>" "<nil \"@ 3b=06\" 13>")

             ;; Verified manually. Produces nil when run by ert!?
             ; ('actypes::link-to-kcell "<EXAMPLE.kotl, 4=012>" "<nil EXAMPLE.kotl, 4=012 13>")

             ; ('actypes::link-to-org-id "id:arg1") ;; Can links to org id be created using text only?
             ('actypes::link-to-rfc "rfc123")
             ; ('actypes::man-show "rm(1)	- remove")
             ('actypes::link-to-file-line "/etc/passwd:10" "\"/etc/passwd:10\"")
             ('actypes::link-to-file "/etc/passwd" "\"/etc/passwd\"")))
    (with-temp-buffer
      ; (ibut:program "label" (car but) (cdar but))
      ; (should (string= (concat "<[label]> - " (cdar but)) (buffer-string)))
      (insert (format "<[label]> - %s" (cadr bd)))
      (goto-char 3)
      (let ((but (ibut:at-p))
            (match (or (caddr bd) (cadr bd))))
        (with-temp-buffer
          (ibut:insert-text but)
          (should (string= (concat " - " match) (buffer-string))))))))

(ert-deftest hbut-tests-ibut-insert-text-temp-file ()
  "Insert ibut text using an ibut in a temp file as source."
  (dolist (bd
           ;; Test data is in the format:
           ;;  - action-type implicit-button-text resulting-button-text
           ;; The third parameter is optional and if missing the
           ;; implicit-button-text is used as the result as
           ;; well. i.e. Work when the input string is preserved.
           '(
             ('actypes::kbd-key "{C-h h}")
             ('actypes::annotate-bib "[FSF 12]") ;; Requires a file!
             ; ('actypes::exec-shell-cmd "!/bin/bash") ;; Not identified as an ibut
             ; ('actypes::exec-window-cmd "&/bin/bash") ;; Not identified as an ibut
             ('actypes::link-to-gbut "<glink:arg1>")
             ('actypes::link-to-ebut "<elink:arg1>")
             ('actypes::link-to-ebut "<elink:arg1:arg2>" "<elink:arg1: arg2>")
             ('actypes::link-to-ibut "<ilink:arg1>")
             ('actypes::link-to-ibut "<ilink:arg1:arg2>" "<ilink:arg1: arg2>")
             ('actypes::link-to-kcell "<@ 3b=06>" "<nil \"@ 3b=06\" 13>")

             ;; Verified manually. Produces nil when run by ert!?
             ; ('actypes::link-to-kcell "<EXAMPLE.kotl, 4=012>" "<nil EXAMPLE.kotl, 4=012 13>")

             ; ('actypes::link-to-org-id "id:arg1") ;; Can links to org id be created using text only?
             ('actypes::link-to-rfc "rfc123")
             ; ('actypes::man-show "rm(1)	- remove")
             ('actypes::link-to-file-line "/etc/passwd:10" "\"/etc/passwd:10\"")
             ('actypes::link-to-file "/etc/passwd" "\"/etc/passwd\"")))
    (let ((file (make-temp-file "hypb" nil ".txt"
                                (format "<[label]> - %s" (cadr bd)))))
      (unwind-protect
          (progn
            (find-file file)
            (goto-char 3)
            (let ((but (ibut:at-p))
                  (match (or (caddr bd) (cadr bd))))
              (with-temp-buffer
                (ibut:insert-text but)
                (should (string= (concat " - " match) (buffer-string))))))
        (delete-file file)))))

;; This file can't be byte-compiled without the `el-mock' package (because of
;; the use of the `with-mock' macro), which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'hbut-tests)
;;; hbut-tests.el ends here
