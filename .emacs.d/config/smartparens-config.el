;;; smartparens-config.el -- Configurations related to smartparens.
;;; Commentary:
;;; Code:

(use-package smartparens
  :diminish smartparens-mode
  :hook ((js-mode . smartparens-mode)
         (ruby-mode . smartparens-mode)
         (emacs-lisp-mode . smartparens-mode)
	 (python-mode . smartparens-mode)
         (scheme-mode . smartparens-mode)))

(provide 'smartparens-config)
;;; smartparens-config.el ends here
