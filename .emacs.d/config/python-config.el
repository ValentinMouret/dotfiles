;;; python-config.el -- Configurations related to Python.
;;; Commentary:
;;; Code:


(use-package blacken
  :hook
  (python-hook-mode . blacken-mode))

(use-package jedi
  :hook
  (python-mode-hook . jedi:setup)
  :config
  (setq jedi:complete-on-dot t))

(provide 'python-config)

;;; python-config.el ends here
