;;; python --- Configuration to work with Python
;;;
;;; Commentary:
;;; Python is a pain in the ass to handle properly so far.
;;; There can be multiple versions of Python, managed in virtual environments,
;;; and it’s not straightforward to have Emacs find the environment
;;; and have intellisense working and so on.

;;; Code:

(use-package python-mode
  :after flycheck
  :mode "\\.py\\'"
  :hook (python-mode . subword-mode))

(use-package blacken
  :hook
  (python-mode-hook))

(use-package reformatter)

(use-package ruff-format
  :after reformatter
  )

(use-package lsp-pyright
  ;; :config
  ;; (put 'lsp-pyright-python-executable-cmd 'safe-local-variable #'stringp)
  :custom (lsp-pyright-langserver-command "pyright")
  :hook
  (
   (python-mode . (lambda ()
                    (require 'lsp-pyright)
                    (lsp-deferred)))))

(provide 'init-python)
;;; init-python.el ends here
