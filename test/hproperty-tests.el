;;; hproperty-tests.el --- one line summary                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:     6-Aug-24 at 20:32:51
;; Last-Mod:     23-Jun-25 at 01:38:38 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'el-mock)
(require 'hproperty)

(ert-deftest hproperty-tests--but-add ()
  "Verify `hproperty:but-add'."
  (let ((hproperty:but-emphasize-flag t))
    (with-temp-buffer
      (insert "1234")
      (should (hproperty:but-add (point-min) (point-max) hproperty:but-face))
      (goto-char 3)
      (should (hproperty:but-p nil 'face hproperty:but-face))
      (should (hproperty:but-get nil 'mouse-face 'highlight))))
  (let ((hproperty:but-emphasize-flag nil))
    (with-temp-buffer
      (insert "1234")
      (should (hproperty:but-add (point-min) (point-max) hproperty:but-face))
      (goto-char 3)
      (should-not (hproperty:but-get nil 'mouse-face 'highlight)))))

(provide 'hproperty-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hproperty-tests.el ends here
