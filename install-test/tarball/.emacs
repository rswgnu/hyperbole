;;; tarball -- hyperbole installed from a tar ball

(unless (and (featurep 'hyperbole) hyperbole-mode)
  (push (expand-file-name "hyperbole" (getenv "HOME")) load-path)
  (require 'hyperbole)
  (hyperbole-mode 1))

(message "%s" "Hyperbole successfully installed")
