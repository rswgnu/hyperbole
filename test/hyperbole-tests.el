;;; hyperbole-tests.el --- Tests for hyperbole.el  -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:     8-Jan-22 at 23:40:00
;; Last-Mod:     24-Jan-22 at 00:41:15 by Bob Weiner
;;
;; Copyright (C) 2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Tests for "../hyperbole.el"

;;; Code:

(require 'ert)
(require 'hyperbole)

;; The reason verify-keybinding in these tests is a macro instead of a
;; function is that in the event of a failure, it makes the keybinding
;; that failed show up in ERT's output. -- Copied from edebug-tests.el

(defun hyperbole-key-bindings (set)
  "If SET is t, verify keys are bound and if nil, verify they are unbound."
  (let ((hyperbole-mode-map (make-sparse-keymap))
        (hkey-init set))
    (cl-letf (((symbol-function 'where-is-internal) (lambda (_func &optional _map) nil))
              ((symbol-function 'hyperb:window-system) (lambda () t)))
      (hkey-initialize)
      (cl-macrolet ((verify-keybinding (verify key binding)
                      `(if ,verify
                           (should (eq (lookup-key hyperbole-mode-map ,key)
                                       ,binding))
                         (should-not (eq (lookup-key hyperbole-mode-map ,key)
                                         ,binding)))))
        (verify-keybinding set "\C-c@" #'hycontrol-windows-grid)
        (verify-keybinding set "\C-c\C-m" #'hui-select-thing)
        (verify-keybinding set "\C-c\\" #'hycontrol-enable-windows-mode)
        (verify-keybinding set "\C-c/" #'hui-search-web)
        (verify-keybinding set "\C-c." #'hui-select-goto-matching-delimiter)
        (mapc (lambda (key) (verify-keybinding set key #'hkey-either))
              '("\M-\C-m"
                ;; Todo: How to verify these bindings.
                ;;"ESC RET" "M-RET" "ESC <return>" "M-<return>"
                ))
        (verify-keybinding set "\C-hA" #'hkey-help)
        (verify-keybinding set "\M-o" #'hkey-operate)))))

(ert-deftest hyperbole-keymap-tests ()
  "Verify all key bindings are set."
  (hyperbole-key-bindings t))

(ert-deftest hyperbole-hkey-init-controls-tests ()
  "Verify all key bindings are set."
  (hyperbole-key-bindings nil))

(ert-deftest hyperbole-global-key-binding-tests ()
  "Verify the global keys are bound."
  (let ((hyperbole-mode-map (make-sparse-keymap))
        (hkey-init t))
    (hkey-initialize)
    (cl-macrolet ((verify-keybinding (key binding)
                    `(should (eq (lookup-key global-map ,key) ,binding))))
      (verify-keybinding "\C-hh" #'hyperbole))))

(provide 'hyperbole-tests)
;;; hyperbole-tests.el ends here
