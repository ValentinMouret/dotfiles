;;; ruby-config.el -- Configurations related to Ruby and Rails.
;;; Commentary:
;;; Code:

(use-package rubocopfmt
  :hook
  (ruby-mode-hook . rubocopfmt-mode))

(use-package ruby-mode
  :mode ("\\.\\(rb\\|ru\\|builder\\|rake\\|thor\\|gemspec\\)\\'"
         "\\(rake\\|thor\\|guard\\|gem\\|cap\\|vagrant\\)file\\'")

  ;; These package are used only with ruby-mode, load them along with ruby-mode
  :init
  (use-package ruby-hash-syntax
    :bind (("C-c h" . ruby-hash-syntax-toggle)))
  (use-package projectile-rails)

  ;; Evaluate this after ruby-mode is loaded
  :config
  (setq ruby-use-smie 't)
  (setq ruby-align-chained-calls 't)
  (setq ruby-insert-encoding-magic-comment nil)
  (exec-path-from-shell-copy-env "GEM_PATH")
  (defun insert-pry ()
    (interactive)
    (insert "binding.pry"))
  (defun insert-frozen-string-literal ()
    (interactive)
    (insert "# frozen_string_literal: true\n\n"))
  (defun my-ruby-mode-hook()
    (projectile-rails-mode)
    (add-to-list 'flycheck-disabled-checkers 'ruby-reek 'ruby-rubylint)
    (add-to-list 'ac-modes 'ruby-mode)
    (global-set-key (kbd "C-c d") 'insert-pry)
    (global-set-key (kbd "C-c f r") 'insert-frozen-string-literal))
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))

(provide 'ruby-config)

;;; ruby-config.el ends here
