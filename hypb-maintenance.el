;;; hypb-maintenance.el --- functions for maintenance tasks  -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    31-Mar-21 at 21:11:00
;; Last-Mod:     12-Feb-22 at 14:09:43 by Bob Weiner
;;
;; Copyright (C) 1991-2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
(require 'kexport "kotl/kexport")
(require 'kimport "kotl/kimport")

(defvar hypb:web-repo-location "../hyweb/hyperbole/"
  "The location of hyperbole repo for the web pages.")

(defconst hypb:hy-news-header
  "<HTML><HEAD>
  <A ID=\"top\"></A><A ID=\"k0\"></A>
  <LINK REL=\"stylesheet\" TYPE=\"text/css\" HREF=\"man/hyperbole.css\">
</HEAD>

<BODY BGCOLOR=\"#FFFFFF\">

  <CENTER><H1>What's New in GNU Hyperbole</H1></CENTER>
  <PRE>
"
  "HY_NEWS html header.")

(defconst hypb:hy-news-footer
  "  </PRE>

</BODY></HTML>
"
  "HY_NEWS html footer.")


(defun hypb:web-repo-update ()
  "Update the Hyperbole web repository from sources.
Point `hypb:web-repo-location' to where the web repo is located."
  (interactive)

  ;; HY_NEWS
  (with-temp-file (concat hypb:web-repo-location "HY-NEWS.html")
    (insert hypb:hy-news-header)
    (insert-file-contents "HY-NEWS")
    (save-excursion
      (let ((beg (point)))
	(search-forward "====")
	(beginning-of-line)
	(delete-region beg (point))))
    (indent-region (point) (point-max) 3)
    (goto-char (point-max))
    (insert hypb:hy-news-footer))

  ;; hyperbole.html
  (copy-file "README.md.html" (concat hypb:web-repo-location "hyperbole.html") t)

  ;; DEMO DEMO-ROLO.otl
  (copy-file "DEMO" hypb:web-repo-location t)
  (copy-file "DEMO-ROLO.otl" hypb:web-repo-location t)
  (copy-file "FAST-DEMO" hypb:web-repo-location t)

  ;; man recursive
  (copy-directory "man" hypb:web-repo-location nil t nil)
  (dolist (file
	   (file-expand-wildcards (concat hypb:web-repo-location "man/im/*.eps")))
    (delete-file file))

  ;; DEMO.html and FAST-DEMO.html
  (dolist (file '("DEMO" "FAST-DEMO"))
    (let ((export-buffer (make-temp-name "export")))
      (kimport:star-outline file export-buffer)
      (kexport:html export-buffer (concat hypb:web-repo-location file ".html") nil)
      (with-current-buffer export-buffer
	(set-buffer-modified-p nil)
	(kill-buffer))))

  ;; koutline-example.html
  (kexport:html "kotl/EXAMPLE.kotl" (concat hypb:web-repo-location "koutline-example.html") nil)
 
  ;; HY-WHY.html
  (kexport:html "HY-WHY.kotl" (concat hypb:web-repo-location "HY-WHY.html") nil))

(provide 'hypb-maintenance)
;;; hypb-maintenance.el ends here
