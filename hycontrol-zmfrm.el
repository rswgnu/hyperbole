;;; hycontrol-zmfrm.el --- Frame zoom support for Emacs 28  -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    14-Sep-25 at 23:03:15
;; Last-Mod:     15-Sep-25 at 13:56:35 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.
;;
;; This files is based on zoom-frm.el and frame-cmds.el. Both under
;; GPLv2 or later:
;;   - https://www.emacswiki.org/emacs/zoom-frm.el
;;     Copyright (C) 2005-2022, Drew Adams, all rights reserved.
;;   - https://www.emacswiki.org/emacs/frame-cmds.el
;;     Copyright (C) 1996-2023, Drew Adams, all rights reserved.
;;
;; It has been simplified to only support zoom in and out on all
;; frames to support versions of Emacs where global-text-scale-adjust
;; is not available, i.e. Emacs 28.

;;; Commentary:
;;
;; This file can be removed when and if Hyperbole stops support for
;; Emacs 28.

;;; Code:

(defcustom hycontrol-frame-zoom-font-difference 1
  "*Number of points to change the frame font size when zooming.
This applies to commands `zoom-in/out', `zoom-in', `zoom-out',
`hycontrol-zoom-frm-in', and `hycontrol-zoom-frm-out' when zooming a frame.

The absolute value of the value must be less than the current font
size for the frame, because the new font size cannot be less than one
point."
  :type 'integer :group 'hyperbole-screen)

(defcustom hycontrol-enlarge-font-tries 100
  "*Number of times to try to change font-size, when looking for a font.
The font-size portion of a font name is incremented or decremented at
most this many times, before giving up and raising an error."
  :type 'integer :group 'hyperbole-screen)

(defun hyconytrol-frcmds-enlarged-font-name (fontname frame increment)
  "FONTNAME, after enlarging font size of FRAME by INCREMENT.
FONTNAME is the font of FRAME."
  (when (query-fontset fontname)
    (let ((ascii  (assq 'ascii (aref (fontset-info fontname frame) 2))))
      (when ascii (setq fontname  (nth 2 ascii)))))
  (let ((xlfd-fields  (x-decompose-font-name fontname)))
    (unless xlfd-fields (error "Cannot decompose font name"))
    (let ((new-size  (+ (string-to-number (aref xlfd-fields xlfd-regexp-pixelsize-subnum))
                        increment)))
      (unless (> new-size 0) (signal 'font-too-small (list new-size)))
      (aset xlfd-fields xlfd-regexp-pixelsize-subnum (number-to-string new-size)))
    ;; Set point size & width to "*", so frame width will adjust to new font size
    (aset xlfd-fields xlfd-regexp-pointsize-subnum "*")
    (aset xlfd-fields xlfd-regexp-avgwidth-subnum "*")
    (x-compose-font-name xlfd-fields)))

;; This does not work 100% well.  For instance, set frame font to
;; "-raster-Terminal-normal-r-normal-normal-12-90-96-96-c-50-ms-oemlatin",
;; then decrease font size.  The next smaller existing font on my
;; machine is
;; "-raster-Terminal-normal-r-normal-normal-11-*-96-96-c-*-ms-oemlatin".
;; Decrease size again.  Next smaller font is
;; "-raster-Terminal-bold-r-normal-normal-5-37-96-96-c-60-ms-oemlatin".
;; Notice the switch to bold from regular.  Cannot decrease any more.
;; Increase size.  Next larger font is
;; "-raster-Terminal-bold-r-normal-normal-8-*-96-96-c-*-ms-oemlatin".
;; Can no longer increase size.
;;
(defun hycontrol-enlarge-font (increment frame)
  "Increase size of font in FRAME by INCREMENT.
Interactively, INCREMENT is given by the prefix argument.
Optional FRAME parameter defaults to current frame."
  (let ((fontname (cdr (assq 'font (frame-parameters frame))))
        (count hycontrol-enlarge-font-tries))
    (setq fontname (hyconytrol-frcmds-enlarged-font-name fontname frame increment))
    (while (and (not (x-list-fonts fontname)) (wholenump (setq count  (1- count))))
      (setq fontname (hyconytrol-frcmds-enlarged-font-name fontname frame increment)))
    (unless (x-list-fonts fontname)
      (error "Cannot change font size"))
    (modify-frame-parameters frame (list (cons 'font fontname)))))

(defun hycontrol-zoom-frm-unzoom (&optional frame)
  "Cancel zoom of FRAME."
  (interactive)
  (setq frame  (or frame  (selected-frame)))
  (let ((zoom-factor  (frame-parameter frame 'zoomed)))
    (if (not zoom-factor)
        (error "Frame is not zoomed")
      (hycontrol-enlarge-font (- zoom-factor) frame)
      (modify-frame-parameters frame '((zoomed))))))

(defun hycontrol-zoom-frm-in (frame)
  "Zoom FRAME in by `hycontrol-frame-zoom-font-difference', making text larger.
If `hycontrol-frame-zoom-font-difference' is negative, make text smaller.
This is equal but opposite to `hycontrol-zoom-frm-out'."
  (let ((zoom-factor (frame-parameter frame 'zoomed))
        (increment hycontrol-frame-zoom-font-difference))
    (unless zoom-factor (setq zoom-factor  0))
    (setq zoom-factor (+ zoom-factor increment))
    (hycontrol-enlarge-font increment frame)
    (modify-frame-parameters frame (list (cons 'zoomed zoom-factor)))))

(defun hycontrol-zoom-frm-out (frame)
  "Zoom FRAME out by `hycontrol-frame-zoom-font-difference', making text smaller.
This is equal but opposite to `hycontrol-zoom-frm-in'."
  (let ((hycontrol-frame-zoom-font-difference (- hycontrol-frame-zoom-font-difference)))
    (hycontrol-zoom-frm-in frame)))

(defun hycontrol-zoom-all-frames-in ()
  "Zoom all visible frames in, making text larger.
Zoom by `hycontrol-frame-zoom-font-difference' points.
This is equal but opposite to `zoom-all-frames-out'."
  (interactive "P")
  (dolist (fr (visible-frame-list))
    (hycontrol-zoom-frm-in fr)))

(defun hycontrol-zoom-all-frames-out ()
  "Zoom all frames out, making text smaller.
Zoom by `hycontrol-frame-zoom-font-difference' points.
This is equal but opposite to `hycontrol-zoom-all-frames-in'."
  (interactive "P")
  (dolist (fr  (visible-frame-list))
    (hycontrol-zoom-frm-out fr)))

(defun hycontrol-zoom-all-frames-unzoom ()
  "Cancel zoom on all frames."
  (dolist (fr (visible-frame-list))
    (hycontrol-zoom-frm-unzoom fr)))

(provide 'hycontrol-zmfrm)
;;; hycontrol-zmfrm.el ends here
