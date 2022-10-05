;;; hui-register.el --- register support for Hyperbole    -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:     6-Oct-91 at 03:42:38
;; Last-Mod:     18-Sep-22 at 00:40:52 by Mats Lidell
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Code:

;;; Commentary:
;;
;; Implements a struct for ebut and ibut, a content type of a
;; register.  See "(Emacs) Registers"
;;

(eval-when-compile (require 'cl-lib))

(require 'hbut)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(cl-defstruct hui-register-but
  "Button register struct."
  label file mpos link)

;;;###autoload
(defun hui-register-struct-at-point ()
  "Make a Hyperbole link to button register struct for button at point."
  (let* ((ebut-label (ebut:label-p))
         (ibut-label (ibut:label-p))
         (label (or ebut-label ibut-label)))
    (unless label
      (hypb:error "Point must be at a Hyperbole button"))
    (make-hui-register-but
     :label label
     :file (buffer-file-name)
     :mpos (point-marker)
     :link (if ebut-label 'link-to-ebut 'link-to-ibut))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(cl-defmethod register-val-jump-to ((val hui-register-but) _arg)
  "Move point to location for Hyperbole button stored in VAL."
  (let ((buf (marker-buffer (hui-register-but-mpos val)))
        (pos (marker-position (hui-register-but-mpos val))))
    (unless buf
      (user-error "That Hyperbole button's buffer no longer exists"))
    (switch-to-buffer buf)
    (goto-char pos)))

(cl-defmethod register-val-describe ((val hui-register-but) _verbose)
  "Print description of Hyperbole button register value VAL to `standard-output'."
  (princ "Hyperbole button\n    ")
  (princ (format "%s in file %s\n"
                 (hui-register-but-label val)
                 (hui-register-but-file val))))

(cl-defmethod register-val-insert ((val hui-register-but))
  "Insert an ebut linking to the register button stored in VAL."
  (ebut:program (hui-register-but-label val)
                (hui-register-but-link val)
                (hui-register-but-label val)
                (hui-register-but-file val)))

(provide 'hui-register)
;;; hui-register.el ends here
