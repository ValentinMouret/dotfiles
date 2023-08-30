;; init.el --- -*- lexical-binding: t -*-
;;
;; Filename: init.el
;; Description: File launched by Emacs on every startup.
;; Author: Valentin Mouret
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; It installs the package manager `use-package` and sets up
;; the different packages for the core functionalities, the editor,
;; and the support for each language.
;;
;; A lot of the content here comes from https://github.com/MatthewZMD/.emacs.d#org14e341b.
;;
;; An Emacs configuration is a funny thing.
;; It is a very zen thing.  The editor does not change, or barely.
;; What changes is the understanding of the editor.
;;
;; It is the record of what I learned about programming, written in a
;; way that makes it easy to setup on another computer (mostly).
;; It is reinforced by the fact that Emacs has a gravity.  It pulls stuff inwards.
;; You want to do as much as possible from Emacs to increase the compounding effect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'early-init)

(require 'init-const)

(require 'init-package)

(require 'init-global-config)

(require 'init-editor)

;; Initialize the package archives:
(require 'package)

;;;; Editor configuration.

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-env "GOOGLE_APPLICATION_CREDENTIALS")
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs
     '("PATH"))))

;; crux: https://github.com/bbatsov/crux
;; Provides useful replacement of functions.
(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-x 4 t" . crux-transpose-windows)
   ("C-x K" . crux-kill-other-buffers)
   ("C-k" . crux-smart-kill-line))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode latex-mode org-mode markdown-mode) . flyspell-mode))
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_UK" "--camel-case"))
  :config
  (use-package flyspell-correct-ivy
    :after ivy
    :bind
    (:map flyspell-mode-map
          ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper)
          ("C-." . flyspell-correct-wrapper))
    :custom (flyspell-correct-interface #'flyspell-correct-ivy)))

(use-package swiper)

(use-package diminish)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
         ("C-M-j" . counsel-switch-buffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-mini-buffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package ivy
  :diminish
  :init
  (use-package amx :defer t)
  (use-package counsel :diminish :config (counsel-mode 1))
  (use-package swiper :defer t)
  (ivy-mode 1)
  :bind (("C-s" . swiper-isearch)
         :map ivy-minibuffer-map
         :map ivy-switch-buffer-map
         :map ivy-reverse-i-search-map
         ("TAB" . ivy-alt-done))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "【%d/%d】")
  (ivy-wrap t)
  :config
  (defun counsel-goto-local-home ()
      "Go to the $HOME of the local machine."
      (interactive)
    (ivy--cd "~/")))

(use-package emojify)
(emojify-mode t)

(use-package evil
  :init (setq evil-want-keybinding nil)
  :bind
  ("M-." . xref-find-definitions))

(evil-mode 1)

(use-package evil-collection
  :after evil
  :config
  (define-key evil-normal-state-map "c" nil)
  (define-key evil-normal-state-map "C" nil)
  (define-key evil-normal-state-map "s" nil)
  (define-key evil-normal-state-map "S" nil)
  (define-key evil-normal-state-map "r" nil)
  (define-key evil-normal-state-map "R" nil)
  (define-key evil-normal-state-map "j" nil)
  (define-key evil-normal-state-map "J" nil)
                                        ;je redéfinis certaines fonctions pour l’état normal
  (define-key evil-normal-state-map "h" 'evil-change)
  (define-key evil-normal-state-map "H" nil)
  (define-key evil-normal-state-map "T" 'evil-join)
  (define-key evil-normal-state-map "l" 'evil-replace)
  (define-key evil-normal-state-map "L" nil)
  (define-key evil-normal-state-map "k" 'evil-substitute)
  (define-key evil-normal-state-map "K" 'evil-change-whole-line)
                                        ;même chose mais cette fois pour l’état motion
  (define-key evil-motion-state-map "c" 'evil-backward-char)
  (define-key evil-motion-state-map "C" 'evil-window-top)
  (define-key evil-motion-state-map "t" 'evil-next-line)
  (define-key evil-motion-state-map "s" 'evil-previous-line)
  (define-key evil-motion-state-map "r" 'evil-forward-char)
  (define-key evil-motion-state-map "R" 'evil-window-bottom)
  (define-key evil-motion-state-map "j" 'evil-find-char-to)
  (define-key evil-motion-state-map "gd" 'xref-find-definitions)
  (define-key evil-motion-state-map "." 'xref-go-back)
  (define-key evil-motion-state-map "J" 'evil-find-char-to-backward)

  ;; Bindings for neotree.
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

(evil-mode 0)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package ivy-hydra)

(use-package avy)

(use-package dired
  :straight (:type built-in)
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump))
  :custom
  ;; Always delete and copy recursively
  (dired-listing-switches "-lah")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify nil)
  (auto-revert-interval 3) ; Auto revert every 3 sec
  :config
  ;; Enable global auto-revert
  (global-auto-revert-mode t)
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Run: M-x all-the-icons-install-font
(use-package all-the-icons)

(use-package doom-modeline
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-minor-modes t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-height 15)
  :config
  (doom-modeline-mode))

;; (hidden-mode-line-mode 1)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  (setq projectile-after-switch-project-hook 'magit-status)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  :custom ((projectile-completion-system 'swiper))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Code")
    (setq projectile-project-search-path '("~/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
  :bind (:map rust-mode-map ("C-c C-c" . rust-run))
  :config
  (use-package flycheck-rust
    :after flycheck
    :config
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

(use-package lsp-mode
  :ensure t
  :custom
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (lsp-keymap-prefix "C-c l")
  (lsp-prefer-flymake nil)
  (lsp-enable-folding nil)
  (read-process-output-max (* 1024 1024))
  (lsp-eldoc-hook nil)
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook
  ((c-mode
    clojure-mode
    clojurescript-mode
    java-mode
    go-mode
    rust-mode
    typescript-mode
    zig-mode) . lsp-deferred)
  (before-save . lsp-format-buffer)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  (defun lsp-update-server ()
    "Update LSP server."
    (interactive)
    ;; Equals to `C-u M-x lsp-install-server'
    (lsp-install-server t)))

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu)
        ("M-i" . lsp-ui-doc-focus-frame))
  (:map lsp-mode-map
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph))
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (when (display-graphic-p)
    (setq lsp-ui-doc-use-webkit t))
  (setq lsp-zig-zls-executable "/Users/valentinmouret/bin/zls")
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

(use-package multiple-cursors
 :bind
 ("C-t" . set-rectangular-region-anchor)
 ("C-z" . mc/mark-next-like-this))

(use-package dap-mode
  :diminish
  :bind
  (:map dap-mode-map
        (("<f12>" . dap-debug)
         ("<f8>" . dap-continue)
         ("<f9>" . dap-next)
         ("<M-f11>" . dap-step-in)
         ("C-M-<f11>" . dap-step-out)
         ("<f7>" . dap-breakpoint-toggle))))

;; (use-package lsp-treemacs)

(use-package neotree
  :bind
  (:map global-map
        (("C-c C-t" . neotree-toggle))))

;; Haskell

(use-package haskell-mode)

(use-package all-the-icons)

;;;; Language support
;; General
(use-package flycheck
  :defer t
  :diminish
  :hook (after-init . global-flycheck-mode)
  :commands (flycheck-add-mode)
  :custom
  (flycheck-global-modes
   '(not outline-mode diff-mode shell-mode eshell-mode term-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode (if (display-graphic-p) 'right-fringe 'right-margin))
  :init
  (if (display-graphic-p)
      (use-package flycheck-posframe
        :custom-face
        (flycheck-posframe-face ((t (:foreground ,(face-foreground 'success)))))
        (flycheck-posframe-info-face ((t (:foreground ,(face-foreground 'success)))))
        :hook (flycheck-mode . flycheck-posframe-mode)
        :custom
        (flycheck-posframe-position 'window-bottom-left-corner)
        (flycheck-posframe-border-width 3)
        (flycheck-posframe-inhibit-functions
         '((lambda (&rest _) (bound-and-true-p company-backend)))))

    (use-package flycheck-pos-tip
      :defines flycheck-pos-tip-timeout
      :hook (flycheck-mode . flycheck-pos-tip-mode)
      :custom (flycheck-pos-tip-timeout 30)))

  :config
  (use-package flycheck-popup-tip
    :hook (flycheck-mode . flycheck-popup-tip-mode))

  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  (when (executable-find "vale")
    (use-package flycheck-vale
      :config
      (flycheck-vale-setup)
      (flycheck-add-mode 'vale 'latex-mode))))

;; SQL

(use-package sqlformat
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "--no-extra-line")))

;; Set Postgres as the default SQL product.
(setq sql-product "postgres")

;; Emacs

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Dart

(use-package lsp-dart)

;; Lisp

(use-package paredit
  :config
  (show-paren-mode t)
  :diminish nil
  :hook
  ((emacs-lisp-mode
    lisp-mode
    clojure-mode
    slime-repl-mode
    cider-repl-mode) . enable-paredit-mode))

(use-package slime
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

;; Clojure

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo)
  (setq clojure-align-forms-automatically t))

(use-package cider
  :commands (cider cider-connect cider-jack-in)
  :config
  (setq cider-repl-pop-to-buffer-on-connect t
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t)
  ; (add-to-list 'cider-jack-in-nrepl-middlewares '("shadow.cljs.devtools.server.nrepl/middleware"))
  :hook
  ((cider-mode . clj-refactor-mode)))

(use-package clj-refactor
  :diminish clj-refactor-mode)

;; ASCIIDoc

(use-package adoc-mode)

;; Racket

(use-package racket-mode)

;; YAML
(use-package yaml-mode)

;; Python

(use-package python-mode
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))

(use-package pyvenv)

(use-package blacken
  :hook
  (python-mode-hook))

(use-package lsp-pyright
  :config
  (put 'lsp-pyright-python-executable-cmd 'safe-local-variable #'stringp)
  :hook
  (
   (python-mode . (lambda ()
                    (require 'lsp-pyright)
                    (lsp-deferred)
		    ))
   ;; if .dir-locals exists, read it first, then activate mspyls
   (hack-local-variables . (lambda ()
			     (setq indent-tabs-mode nil)  ; disable tabs
			     ))))

;; python-black
(use-package python-black
  :hook
  (python-mode . python-black-on-save-mode)
  :init
  (put 'python-black-command 'safe-local-variable #'stringp)
  (put 'python-black-extra-args 'safe-local-variable #'stringp)
  (put 'python-black-on-save-mode 'safe-local-variable #'booleanp))

(use-package jedi
  :config
  (setq jedi:complete-on-dot t)
  :hook
  (python-mode-hook . jedi:setup))

(use-package poetry
  :hook
  (python-mode . poetry-tracking-mode))

(setq pyvenv-mode nil)

;; Go
(use-package go-mode
  :hook
  (before-save-hook . gofmt-before-save)
  (subword-mode))

(use-package nix-mode)

(use-package restclient)

;; JavaScript

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package prettier-js
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "all"
                           "--single-quote" "true"
			   "--semi" "false"
                           "--print-width" "100"
                           ))
  :hook
  (((typescript-mode-hook js-mode-hook js2-mode-hook rjsx-mode-hook) . prettier-js-mode)
   (typescript-mode . prettier-js-mode)
   (js-mode-hook . subword-mode)))

;; (straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))

;; Typescript

(use-package web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'"))

(use-package json-mode
  :mode "\\.json\\'")

(use-package company
  :diminish company-mode
  :hook (((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
         ((prog-mode) . yas-minor-mode))
  :bind
  (:map company-active-map
        ([tab] . smarter-tab-to-complete)
        ("TAB" . smarter-tab-to-complete))
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  :config
  ;; (unless clangd-p (delete 'company-clang company-backends))
  (global-company-mode 1)
  (defun smarter-tab-to-complete ()
    "Try to `org-cycle', `yas-expand', and `yas-next-field' at current cursor position.

If all failed, try to complete the common part with `company-complete-common'"
    (interactive)
    (when yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick))
            (func-list
             (if (equal major-mode 'org-mode) '(org-cycle yas-expand yas-next-field)
               '(yas-expand yas-next-field))))
        (catch 'func-suceed
          (dolist (func func-list)
            (ignore-errors (call-interactively func))
            (unless (and (eq old-point (point))
                         (eq old-tick (buffer-chars-modified-tick)))
              (throw 'func-suceed t)))
          (company-complete-common))))))

(use-package typescript-mode
  :hook
  ((typescript-mode . company-mode)
   (typescript-subword-mode)
   (typescript-mode . prettier-js-mode)))

;; (use-package tide
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          ;(before-save . tide-format-before-save)
;; 	 )
;;   :config
;;   (setq company-idle-delay 0))

;; Markdown
(use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.mdx\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

;; Java

;;
;; switch java
;;
(setq JAVA_BASE "/Library/Java/JavaVirtualMachines")

(use-package lsp-java
  :after lsp-mode
  :if (executable-find "mvn")
  :init
  (use-package request :defer t)
  :hook
  (subword-mode)
  :custom
  (lsp-java-server-install-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/server/"))
  (lsp-java-workspace-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/workspace/")))

;;
;; This function returns the list of installed
;;
(defun switch-java--versions ()
  "Return the list of installed JDK."
  (seq-remove
   (lambda (a) (or (equal a ".") (equal a "..")))
   (directory-files JAVA_BASE)))

(defun switch-java--save-env ()
  "Store original PATH and JAVA_HOME."
  (when (not (boundp 'SW_JAVA_PATH))
    (setq SW_JAVA_PATH (getenv "PATH")))
  (when (not (boundp 'SW_JAVA_HOME))
    (setq SW_JAVA_HOME (getenv "JAVA_HOME"))))

(defun switch-java ()
  "List the installed JDKs and enable to switch the JDK in use."
  (interactive)
  ;; store original PATH and JAVA_HOME
  (switch-java--save-env)

  (let ((ver (completing-read
              "Which Java: "
              (seq-map-indexed
               (lambda (e i) (list e i)) (switch-java--versions))
              nil t "")))
    ;; switch java version
    (setenv "JAVA_HOME" (concat JAVA_BASE "/" ver "/Contents/Home"))
    (setenv "PATH" (concat (concat (getenv "JAVA_HOME") "/bin/java")
                           ":" SW_JAVA_PATH)))
  ;; show version
  (switch-java-which-version?))

(defun switch-java-default ()
  "Restore the default Java version."
  (interactive)
  ;; store original PATH and JAVA_HOME
  (switch-java--save-env)

  ;; switch java version
  (setenv "JAVA_HOME" SW_JAVA_HOME)
  (setenv "PATH" SW_JAVA_PATH)
  ;; show version
  (switch-java-which-version?))

(defun switch-java-which-version? ()
  "Display the current version selected Java version."
  (interactive)
  ;; displays current java version
  (message (concat "Java HOME: " (getenv "JAVA_HOME"))))

;; Shell
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; AI
(use-package copilot
  :straight (:host github
             :repo "zerolfx/copilot.el"
             :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :config (setq copilot-idle-delay 0.01)
  :bind (("C-c ." . copilot-accept-completion)
         ("C-c x" . copilot-complete)))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook
  (sh-mode-hook . flymake-shellcheck-load))

(use-package zig-mode)

;; Run Emacs in server mode.
;; This allows other programs to use it using the `EDITOR` environment variable.
(unless server-process
  (server-start))

;; Custom
;; This area is set by Custom. Don’t touch it.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(custom-safe-themes
   '("2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" default))
 '(package-selected-packages
   '(nix-mode flyspell-correct-ivy crux diminish amx flycheck-popup-tip flycheck-posframe json-mode web-mode flycheck-rust lsp-java java-lsp rust-mode haskell-mode adoc adoc-mode csv-mode lsp-dart evil-collection treemacs-evil evil-mode slack oauth2 dockerfile-mode docker python-black lsp-pyright flymake-shellcheck company tide poetry flycheck-clj-kondo clj-refactor cider-mode cider clojure-mode paredit emojify exec-path-from-shell smex uniquify prettier-js flycheck go-mode jedi blacken pyvenv yaml-mode forge markdown-mode magit counsel-projectile projectile which-key rainbow-delimiters doom-modeline all-the-icons expand-region avy ivy-hydra ivy-rich counsel swiper base16-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-selector ((t (:inherit default :foreground "#66CCFF"))))
 '(flycheck-posframe-face ((t (:foreground "#a1b56c"))))
 '(flycheck-posframe-info-face ((t (:foreground "#a1b56c"))))
 '(font-lock-comment-face ((t (:foreground "#828282"))))
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))

(require 'init-magit)

(provide 'init)
;;; init.el ends here.
