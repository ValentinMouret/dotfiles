;;; base-setup.el -- Sets up the foundations.
;;; Commentary:
;;; Code:


;; Delete trailing whitespaces on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Always end files with a newline.
(setq require-final-newline t)

;; Install if they are not available.
(setq use-package-always-ensure t)

;; Don’t use tabs for indentation.
(setq-default indent-tabs-mode nil)

;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; Shorten yes/no questions to y/n.
(defalias 'yes-or-no-p 'y-or-no-p)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Show line numbers.
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes.
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/"))
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(use-package 'emojify)

;; Load the dark theme from base16.
(load-theme 'base16-default-dark t)
;; Load the dark theme from base16.
(setq base16-highlight-mode-line 'false)
(setq base16-theme-256-color-source "colors")
(load-theme 'base16-default-dark t)
(setq ring-bell-function 'ignore)

(provide 'common-setup)

;;; common-setup.el ends here
