(require 'package)

(setq package-check-signature nil)

(add-to-list 'package-archives
	     '("tromey" . "https://tromey.come/elpa/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)

(package-initialize)

(add-to-list 'load-path "~/.emacs.d")

(when (not package-archive-contents)
  (package-refresh-contents))

;; Load the dark theme from base16.
(load-theme 'base16-default-dark t)

(defvar my-packages
  '(better-defaults ;; Set up better Emacs defaults.
    elpy            ;; Python mode.
    flycheck        ;; Code completion for Python.
    blacken         ;; Black formatting for Python.
    magit           ;; Git.
    paredit         ;; Makes handling list expressions much easier.
    clojure-mode    ;; Key binding and colorization for Clojure.
    clojure-mode-extra-font-locking
    cider           ;; Integration with the Clojure REPL.
    ido-completing-read+
    smex
    projectile      ;; Project navigation.
    rainbow-delimiters
    tagedit
    rubocopfmt
    js2-mode
    js2-refactor
    xref-js2
    )
  )

(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/customizations")

(load "shell-integration.el")
(load "navigation.el")
(load "ui.el")
(load "editing.el")
(load "misc.el")
(load "elisp-editing.el")
(load "setup-clojure.el")
(load "setup-js.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   (quote
    (xref-js2 multiple-cursors js2-refactor js2-mode wispjs-mode markdown-mode go-mode yaml-mode rubocopfmt flymake-shellcheck dockerfile-mode docker grails-mode magit tagedit rainbow-delimiter projectile smex ido-completing-read+ cider clojure clojure-mode-extra-font-locking clojure-mode paredit exec-path-from-shell))))

;; Python
(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'elpy-mode-hook 'blacken-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'rubocopfmt)
(add-hook 'ruby-mode-hook #'rubocopfmt-mode)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
