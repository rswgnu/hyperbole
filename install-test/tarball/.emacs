;;; tarball -- hyperbole installed from a tar ball

(when (< emacs-major-version 27)
  (error "Hyperbole requires Emacs 27 or above; you are running version %d" emacs-major-version))
(unless (and (featurep 'hyperbole) hyperbole-mode)
  (push (expand-file-name "hyperbole" (getenv "HOME")) load-path)
  (require 'hyperbole)
  (hyperbole-mode 1))

(message "%s" "Hyperbole successfully installed and activated")
