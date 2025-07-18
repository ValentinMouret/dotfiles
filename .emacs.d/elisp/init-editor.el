;;; init-editor.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-editor.el
;; Description: Configures the editor part of Emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(menu-bar-mode nil)

(unless (and (display-graphic-p) *sys/mac?*)
  (push '(menu-bar-lines . 0) default-frame-alist))

(push '(tool-bar-lines . 0) default-frame-alist)

(push '(vertical-scroll-bars) default-frame-alist)

(setq mac-left-option-modifier  'meta
      mac-left-command-modifier 'meta
      mac-right-option-modifier nil)

(global-prettify-symbols-mode 1)

(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols.  See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
        '(("lambda" . 955)
          ("delta" . 120517)
          ("epsilon" . 120518)
          ("->" . 8594)
          ("<=" . 8804)
          (">=" . 8805))))
(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

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
      backup-by-copying t    ;; Don't delink hardlinks
      version-control t      ;; Use version numbers on backups
      delete-old-versions t  ;; Automatically delete excess backups
      kept-new-versions 20   ;; how many of the newest versions to keep
      kept-old-versions 5    ;; and how many of the old
      )

; (use-package base16-theme)
; (setq base16-highlight-mode-line 'false)
; (setq base16-theme-256-color-source "colors")
; (load-theme 'base16-nord t)

(load-theme 'modus-vivendi-tinted)

;; (use-package nord-theme
;;   :config
;;   (load-theme 'nord t))

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(set-frame-font "JetBrains Mono 14" nil t)
(setq font-use-system-font t)
(setq mac-allow-anti-aliasing t)  ; For macOS
(setq x-use-underline-position-properties nil)
(setq x-underline-at-descent-line t)

(setq tab-width 2)

(require 'uniquify)

(setq uniquify-buffer-name-style 'forward)

(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)


(defun reset-buffer-width ()
  "Reset the width of single buffers based on the screen resolution."
  (interactive)
  (let ((ratio (if (> (display-pixel-width) 1500) 0.6 0.8)))
    (setq olivetti-body-width ratio)))

;; This package makes the buffer centered when there is only one.
;; As soon as more buffers are opened, it will be disabled.
(use-package olivetti
  :init
    (reset-buffer-width) ; adjust to taste
  (defun adjust-window-width ()
    (if (one-window-p)
        (olivetti-mode)
      (olivetti-mode -1)))
  :config
  (add-hook 'window-configuration-change-hook 'adjust-window-width))

(use-package golden-ratio)
(golden-ratio-mode 1)

(setq split-height-threshold nil)  ; Disable horizontal splitting
(setq split-width-threshold 0)     ; Enable vertical splitting regardless of window width

;; Optional: Make 'split-window-sensibly the preferred function
(setq split-window-preferred-function 'split-window-sensibly)

(global-auto-revert-mode 1)

(blink-cursor-mode -1)

(provide 'init-editor)

;;; init-editor ends here
