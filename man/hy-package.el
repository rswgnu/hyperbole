;;; ************************************************************************
;;; Requirements
;;; ************************************************************************

(require 'package)

;;; ************************************************************************
;;; Section 1: package.el setup
;;; ************************************************************************

;;; ========================================================================
;;; NOTE: This section is only if you have not yet setup the package.el
;;;       package manager.  If you have, ignore this and skip to "#Section 2"
;;;       for the Hyperbole `use-package' recipe.
;;; ========================================================================

;; Step 1: Add the function definition below near the top of your
;; "~/.emacs" or "~/.emacs.d/early-init.el" file.

;; Step 2: Add a call to the function below its definition:
;;         (setup-package)

(defun setup-package ()
  (require 'package)
  (setq package-enable-at-startup nil) ;; Prevent double loading of libraries
  (add-to-list 'package-archives
	       ;; Leave only one of the following two lines uncommented
	       '("elpa-devel" . "https://elpa.gnu.org/devel/"))
	       ;; '("elpa" . "https://elpa.gnu.org/packages/"))
  (unless (and (boundp 'package--initialized) package--initialized)
    (package-initialize))
  ;; To ensure you have the latest index of packages, you'll have to
  ;; uncomment the next line.  It is commented because retrieving the
  ;; list is slow.
  ;; (package-refresh-contents)
  )

;;; ************************************************************************
;;; Section 2: package.el Hyperbole installation and configuration
;;; ************************************************************************

;; Step 1: Add the following expression to your "~/.emacs" or
;; "~/.emacs.d/init.el" file.

(use-package hyperbole

  ;; ensure Hyperbole is installed
  :ensure t

  ;; Initialize the global HyWiki minor mode to one of these values:
  ;;   :all   - highlights and makes HyWikiWord links active in
  ;;            all text and programming buffer comments
  ;;   :pages - highlights and makes HyWikiWord links active in
  ;;            only within HyWiki page buffers
  ;;   nil    - leaves HyWiki mode disabled.
  :config
  (progn
    ;; Older Hyperbole releases do not have HyWiki
    (when (fboundp 'hywiki-mode)
      (hywiki-mode :all))

    ;; Initialize the global Hyperbole minor mode to one of these values:
    ;;   1 - enabled on startup so the Action and Assist Smart Keys are active
    ;;   0 - off until you toggle it on
    ;; If you left hywiki-mode enabled above, that enables hyperbole-mode too.
    (unless (and (fboundp 'hywiki-mode) hywiki-mode)
      (hyperbole-mode 1)))

  :bind
  (("M-RET" . hkey-either)
   ;; Uncomment the next binding if you want to emulate Hyperbole mouse drag
   ;; events from your keyboard.
   ;; See "https://www.gnu.org/s/hyperbole/man/hyperbole.html#Keyboard-Drags".
   ;; ("M-o"   . hkey-operate)
   )

  ;; Customize how Hyperbole and Org mode share the M-RET key:
  ;;   t        - With hyperbole-mode enabled, Hyperbole controls the key
  ;;   'buttons - With hyperbole-mode enabled, Hyperbole controls the key
  ;;              only when on a button or link; otherwise, Org controls it
  ;;   nil      - In Org mode, Org controls the key
  :custom
  (hsys-org-enable-smart-keys t)
  )

;;; End
