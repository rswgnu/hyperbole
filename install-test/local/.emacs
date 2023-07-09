;; Use this in your Emacs init file to install Straight
(progn
  (when (< emacs-major-version 27)
    (error "Hyperbole requires Emacs 27 or above; you are running version %d" emacs-major-version))
  (defvar bootstrap-version)
  (setq package-enable-at-startup nil)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(setq package-native-compile t)

(straight-use-package
 (list
  'hyperbole
  :host nil
  :repo (getenv "LOCAL_HYPB_REPO")
  :branch (getenv "LOCAL_HYPB_BRANCH")
  :config '(hyperbole-mode 1)))

(hyperbole-mode 1)

(message "%s" "Hyperbole successfully installed and activated")
