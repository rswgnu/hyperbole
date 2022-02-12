;;; demo-tests.el --- unit tests from examples in the DEMO         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-Jan-21 at 12:00:00
;; Last-Mod:     12-Feb-22 at 13:33:53 by Bob Weiner
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Runs unit tests on some of the examples given in the DEMO file.

;;; Code:

(require 'ert)
(require 'hib-kbd)
(require 'hmouse-drv)
(require 'hhist)
(require 'hload-path)
(require 'hypb)
(require 'hib-social)
(require 'eww)
(require 'compile)
(require 'el-mock)
(require 'hy-test-helpers "test/hy-test-helpers")

(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")
(declare-function hy-test-helpers:should-last-message "hy-test-helpers")
(declare-function hyrolo-demo-quit "hyrolo-demo.el")
(declare-function org-check-for-hidden "org-el")

(ert-deftest demo-smart-mouse-keys-ref-test ()
  "Go to the header from a #ref."
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (goto-char (point-min))
        (re-search-forward "#Smart Keys")
        (action-key)
        (should (bolp))
        (should (looking-at "^\\* Smart")))
    (kill-buffer "DEMO")))

(ert-deftest demo-smart-mouse-keys-ebut-test ()
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (goto-char (point-min))
        (re-search-forward "<(Smart")
        (action-key)
        (should (bolp))
        (should (looking-at "^ +\\* Smart")))
    (kill-buffer "DEMO")))

(ert-deftest demo-table-of-contents-test ()
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (goto-char (point-min))
        (re-search-forward " \\* Koutl")
        (action-key)
        (should (bolp))
        (should (looking-at "^* Koutliner")))
    (kill-buffer "DEMO")))

;; Smart scrolling
(ert-deftest demo-smart-scrolling-proportional-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (goto-char (point-min))
        (re-search-forward "Table of Contents")
        (beginning-of-line)
        (let ((smart-scroll-proportional t)
              (pos (point)))
          (end-of-line)
          (action-key)
          (should (= pos (window-start)))))
    (kill-buffer "DEMO")))

(ert-deftest demo-smart-scrolling-non-proportional-test ()
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (goto-char (point-min))
        (re-search-forward "Table of Contents")
        (beginning-of-line)
        (let ((smart-scroll-proportional nil)
              (pos (window-start)))
          (end-of-line)
          (action-key)
          (should (< pos (window-start)))))
    (kill-buffer "DEMO")))

;; Hyperbole menus

;; Help Buffer
(ert-deftest demo-action-key-help ()
  (let ((help-buffer "*Help: Hyperbole Action Key*"))
    (if (get-buffer help-buffer)
        (kill-buffer help-buffer))
    (unwind-protect
        (with-temp-buffer
          (insert "Text")
          (hkey-help)
          (should (get-buffer help-buffer))
      (kill-buffer help-buffer)))))

(ert-deftest demo-assist-key-help ()
  (let ((help-buffer "*Help: Hyperbole Assist Key*"))
    (if (get-buffer help-buffer)
        (kill-buffer help-buffer))
    (unwind-protect
        (with-temp-buffer
          (insert "Text")
          (hkey-help t)
          (should (get-buffer help-buffer))
      (kill-buffer help-buffer)))))

;; Hy-control
(ert-deftest demo-window-grid-22-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (should (hact 'kbd-key "C-h h s f @ 22 RET Q"))
        (hy-test-helpers:consume-input-events)
        (should (eq 4 (length (window-list)))))
    (kill-buffer "DEMO")))

(ert-deftest demo-window-grid-33-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (should (hact 'kbd-key "C-h h s f @ 33 RET Q"))
        (hy-test-helpers:consume-input-events)
        (should (eq 9 (length (window-list)))))
    (kill-buffer "DEMO")))

;; Hy-rolo
(ert-deftest demo-hy-rolo-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (with-temp-buffer
        (load (expand-file-name "hyrolo-demo.el" hyperb:dir))
        (should (hact 'kbd-key "C-x 4 r work RET"))
        (hy-test-helpers:consume-input-events)
        (should (string= "*Hyperbole Rolo*" (buffer-name)))
        (should (search-forward "Dunn, John")))
    (hyrolo-demo-quit)))

;; Info
(ert-deftest demo-hy-info-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "\"(hyperbole)HyRolo Keys\"")
        (goto-char 5)
        (action-key)
        (should (string= "*info*" (buffer-name))))
    (kill-buffer "*info*")))

;; History
(ert-deftest demo-hy-history-test ()
  (unwind-protect
      (with-temp-buffer
        (let ((tmp-buf-name (buffer-name))
              (pm))
          (hhist:init)
          (insert "\"(hyperbole)HyRolo Keys\"")
          (goto-char 5)
          (setq pm (point-marker))
          (action-key)
          (should (string= "*info*" (buffer-name)))
          (hhist:remove)
          (should (string= tmp-buf-name (buffer-name)))
          (should (equal pm (point-marker)))
          ))
    (kill-buffer "*info*")))

;; Implicit Buttons
(ert-deftest demo-implicit-button-test ()
  (unwind-protect
      (with-temp-buffer
        (insert (format "\"%s\"" (expand-file-name "DEMO" hyperb:dir)))
        (goto-char 2)
        (action-key)
        (should (string= "DEMO" (buffer-name))))
    (kill-buffer "DEMO")))

(ert-deftest demo-implicit-button-action-button-action-type-invocation-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "<link-to-file-line \"${hyperb:dir}/DEMO\" 5>")
        (goto-char 5)
        (action-key)
        (should (string= "DEMO" (buffer-name)))
        (should (= 5 (line-number-at-pos (point)))))
    (kill-buffer "DEMO")))

(ert-deftest demo-implicit-button-action-button-function-call-test ()
  (with-temp-buffer
    (insert "<message \"%d\" (eval (+ 2 2))>")
    (goto-char 2)
    (action-key)
    (hy-test-helpers:should-last-message "4")))

(ert-deftest demo-implicit-button-action-button-sexp-test ()
  (with-temp-buffer
    (insert
     "<progn (mapc (lambda (f) (bury-buffer (find-file-noselect
                                             (expand-file-name f hyperb:dir))))
		   '(\"hibtypes.el\" \"hactypes.el\" \"hsettings.el\"))
	     (message \"Last 3 buffers are: %S\"
		      (mapcar #'buffer-name
			      (nthcdr (- (length (buffer-list)) 3) (buffer-list))))>")
    (goto-char 2)
    (action-key)
    (let* ((bufs (reverse (buffer-list)))
 	   (hsettings-buf (buffer-name (nth 0 bufs)))
	   (hactypes-buf  (buffer-name (nth 1 bufs)))
	   (hibtypes-buf  (buffer-name (nth 2 bufs))))
      (should (and (hy-test-helpers:should-last-message "Last 3 buffers are")
		   (string-match-p "hsettings\\.el" hsettings-buf)
		   (string-match-p "hactypes\\.el"  hactypes-buf)
		   (string-match-p "hibtypes\\.el"  hibtypes-buf))))))

(ert-deftest demo-implicit-button-action-button-boolean-function-call-test ()
  (with-temp-buffer
    (insert "<string-empty-p \"False\">")
    (goto-char 2)
    (action-key)
    (hy-test-helpers:should-last-message "Boolean result (False) = nil")))

(ert-deftest demo-implicit-button-action-button-variable-display-test ()
  (with-temp-buffer
    (insert "<fill-column>")
    (goto-char 2)
    (action-key)
    (hy-test-helpers:should-last-message (format "fill-column = %d" (current-fill-column)))))

(ert-deftest demo-implicit-button-hash-link-test ()
  (unwind-protect
      (with-temp-buffer
        (insert (format "\"%s%s\"" (expand-file-name "README.md" hyperb:dir) "#why-was-hyperbole-developed"))
        (goto-char 5)
        (action-key)
        (should (string= "README.md" (buffer-name)))
        (should (looking-at "## Why was Hyperbole developed\\?")))
    (kill-buffer "README.md")))

(ert-deftest demo-implicit-button-line-and-column-test ()
  (unwind-protect
      (with-temp-buffer
        (insert (format "\"%s%s\"" (expand-file-name "HY-ABOUT" hyperb:dir) ":5:46"))
        (goto-char 5)
        (action-key)
        (should (string= "HY-ABOUT" (buffer-name)))
        (should (looking-at "hyperbole/")))
    (kill-buffer "HY-ABOUT")))

;; org
(ert-deftest demo-org-hide-header-test ()
  "Hide org mode header."
  (with-temp-buffer
    (org-mode)
    ;; Without this next let, default Org behavior may occur which
    ;; inserts a new heading when org-meta-return is called rather
    ;; than collapsing the existing tree of headings.
    (let ((hsys-org-enable-smart-keys t))
      (insert "* 1\n** 2\n*** 3\n")
      (goto-char 1)
      (should (not (org-check-for-hidden 'headlines)))
      (save-excursion
	(action-key))
;;; (org-hide-entry)
      (should (org-check-for-hidden 'headlines)))))

;; Manifest
(ert-deftest demo-manifest-test ()
  (unwind-protect
      (progn
        (find-file (expand-file-name "MANIFEST" hyperb:dir))
        (beginning-of-buffer)
        (forward-line 1)
        (should (looking-at "COPYING"))
        (action-key)
        (should (string= "COPYING" (buffer-name)))
        (should (looking-at ".*GNU GENERAL PUBLIC LICENSE")))
    (progn
      (kill-buffer "MANIFEST")
      (kill-buffer "COPYING"))))

;; Email compose
(ert-deftest demo-mail-compose-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "receiver@mail.org")
        (goto-char 2)
        (action-key)
        (should (string= "*mail*" (buffer-name))))
    (kill-buffer "*mail*")))


(defun demo-should-browse-twitter-url (url &optional new-window)
  "Verify call with proper URL and optional NEW-WINDOW."
  (should (equal url "https://twitter.com/search?q=@fsf"))
  (should (equal new-window nil)))

;; Social
(ert-deftest demo-social-twitter-test ()
  (with-temp-buffer
    (insert "tw@fsf")
    (goto-char 2)
    (let ((browse-url-browser-function 'demo-should-browse-twitter-url))
      (action-key))))


(defun demo-should-browse-github-url (url &optional new-window)
  "Verify call with proper URL and optional NEW-WINDOW."
  (should (equal url "https://github.com/rswgnu/hyperbole"))
  (should (equal new-window nil)))

;; WWW
(ert-deftest demo-www-test ()
  (with-temp-buffer
    (insert "https://github.com/rswgnu/hyperbole")
    (goto-char 4)
    (let ((browse-url-browser-function 'demo-should-browse-github-url)
          (hibtypes-github-default-user "rswgnu"))
      (action-key))))

;; Github
(ert-deftest demo-github-user-default-test ()
  (with-temp-buffer
    (insert "gh#/hyperbole")
    (goto-char 4)
    (let ((browse-url-browser-function 'demo-should-browse-github-url)
          (hibtypes-github-default-user "rswgnu"))
      (action-key))))

(ert-deftest demo-github-ignore-default-test ()
  (with-temp-buffer
    (insert "gh#/rswgnu/hyperbole")
    (goto-char 4)
    (let ((browse-url-browser-function 'demo-should-browse-github-url)
          (hibtypes-github-default-user "whatever"))
      (action-key))))

;; Occur
(ert-deftest demo-occur-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (should (hact 'kbd-key "C-h h f o Hyperbole RET"))
        (hy-test-helpers:consume-input-events)
        (set-buffer "*Occur*")
        (should (looking-at "[0-9]+ matches in [0-9]+ lines for \"Hyperbole\" in buffer: DEMO")))
    (progn
      (kill-buffer "DEMO")
      (kill-buffer "*Occur*"))))

;; Annotated references
(ert-deftest demo-annotated-reference-test ()
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (re-search-forward "\\[FSF 19\\]")
        (backward-char 1)
        (action-key)
        (should (looking-at "\\[FSF 19\\] Free Software Foundation"))
        (forward-line -2)
        (should (looking-at "\\* References")))
    (kill-buffer "DEMO")))

;; Man appropos
(ert-deftest demo-man-appropos-test ()
  (with-temp-buffer
    (insert "rm (1)   - remove")
    (goto-char 4)
    (with-mock
      (mock (man "rm(1)") => t)
      (action-key))))

;; Explicit buttons
(ert-deftest demo-factorial-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (should (hact 'kbd-key "C-h h a factorial RET"))
        (hy-test-helpers:consume-input-events)
        (hy-test-helpers:should-last-message "Factorial of 5 = 120"))
    (kill-buffer "DEMO")))

(ert-deftest demo-factorial-ebutton-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (re-search-forward "<(factorial)>")
        (forward-char -5)
        (action-key)
        (hy-test-helpers:should-last-message "Factorial of 5 = 120"))
    (kill-buffer "DEMO")))

;; This file can't be byte-compiled without the `el-mock' package (because of
;; the use of the `with-mock' macro), which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'demo-tests)
;;; demo-tests.el ends here
