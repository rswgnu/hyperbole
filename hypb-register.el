;;; hypb-register.el --- register support for Hyperbole    -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:     6-Oct-91 at 03:42:38
;; Last-Mod:     20-Aug-22 at 23:52:31 by Mats Lidell
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Code:

;;; Commentary:
;;
;; Proof of concept: Implements a register struct for ebut.  To be
;; completed with similar for ibut.
;;

(eval-when-compile (require 'cl-lib))

(require 'hbut)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(cl-defstruct hypb-register-but
  "Button register struct."
  label file mpos)

(defun hypb-register-struct-at-point ()
  "Make a Hyperbole link-to-ebut register struct for button at point."
  (let ((label (ebut:label-p)))
    (when (null label)
      (hypb:error "Point must be on a Hyperbole button"))
    (make-hypb-register-but :label label :file (buffer-file-name) :mpos (point-marker))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(cl-defmethod register-val-jump-to ((val hypb-register-but) _arg)
  "Move point to location for hypb button."
  (let ((buf (marker-buffer (hypb-register-but-mpos val)))
        (pos (marker-position (hypb-register-but-mpos val))))
    (switch-to-buffer buf)
    (goto-char pos)))

(cl-defmethod register-val-describe ((val hypb-register-but) _verbose)
  "Print description of hypb button register value VAL to `standard-output'."
  (princ "Hyperbole button\n    ")
  (princ (format "%s in file %s\n"
                 (hypb-register-but-label val)
                 (hypb-register-but-file val))))

(cl-defmethod register-val-insert ((val hypb-register-but))
  "Insert an ebut linking to the register button."
  (ebut:program (hypb-register-but-label val)
                'link-to-ebut
                (hypb-register-but-label val)
                (hypb-register-but-file val)))

(provide 'hypb-register)
;;; hypb-register.el ends here
