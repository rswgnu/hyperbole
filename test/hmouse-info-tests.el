;;; hmouse-info-tests.el --- hmouse-info unit tests         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    29-Dec-21 at 09:02:00
;; Last-Mod:     22-May-22 at 11:11:53 by Mats Lidell
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; See "../hmouse-info.el"

;;; Code:

(require 'ert)
(require 'hmouse-info)
(require 'with-simulated-input)

(ert-deftest hmouse-info-read-index-with-completion ()
  "Read a completion that completes."
  (with-simulated-input "(emacs)regex TAB RET"
    (should (string= "(emacs)regexp" (Info-read-index-item-name "Prompt: ")))))

(ert-deftest hmouse-info-build-completions-no-match ()
  "Build completions."
  (unwind-protect
      (progn
        (info "(emacs)")
        (setq Info-complete-menu-buffer (clone-buffer))
        (should (eq '() (Info-build-menu-item-completions "nothinglikethis" nil t)))
    (kill-buffer "*info*"))))

(ert-deftest hmouse-info-build-completions-multiple-matches ()
  "Build completions."
  (unwind-protect
      (progn
        (info "(emacs)")
        (setq Info-complete-menu-buffer (clone-buffer))
        (dolist (m (Info-build-menu-item-completions "regexp" nil t))
          (should (string-prefix-p "regexp" m t))))
    (kill-buffer "*info*")))

;; This file can't be byte-compiled without the `with-simulated-input'
;; package (because of the use of the `with-simulated-input' macro),
;; which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'hmouse-info-tests)
;;; hmouse-info-tests.el ends here
