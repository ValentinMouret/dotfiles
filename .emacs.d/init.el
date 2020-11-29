(let ((setup (concat user-emacs-directory (convert-standard-filename "setup/")))
      (configs (concat user-emacs-directory (convert-standard-filename "config/"))))
  (add-to-list 'load-path setup)
  (add-to-list 'load-path configs))

;; Setups
(require 'common-setup)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("tromey", "https://tromey.come/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(setq package-check-signature nil)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Configs

(require 'shellcheck-config)
(require 'python-config)
(require 'ruby-config)
(require 'smartparens-config)


(use-package swiper
  :ensure t)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-mini-buffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         :map ivy-switch-buffer-map
         :map ivy-reverse-i-search-map
         ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1))
(ivy-mode 1)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
(use-package ivy-hydra)
(use-package avy)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Run: M-x all-the-icons-install-font
(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(doom-modeline-mode 1)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . org-mode-setup))

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

 (add-hook 'term-mode-hook
 	      (function
 	       (lambda ()
 	             (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
 	             (setq-local mouse-yank-at-point t)
 	             (setq-local transient-mark-mode nil)
 	             (auto-fill-mode -1)
 	             (setq tab-width 8 ))))

(use-package yaml-mode
  :ensure t
  :hook
  (yaml-mode-hook . (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package pyvenv)
(use-package blacken
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  ;;    (add-hook 'flycheck-mode-hook 'jc/use-eslint-from-node-modules)
  ;;    (add-to-list 'flycheck-checkers 'proselint)
  (setq-default flycheck-highlighting-mode 'lines)
  ;; Define fringe indicator / warning levels
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-info))

(use-package prettier-js
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "es5"
                           "--single-quote" "true"
                           "--print-width" "100"
                           ))
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'js-mode-hook 'subword-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))

(use-package cider)

(use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))
