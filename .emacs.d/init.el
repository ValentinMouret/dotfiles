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

(require 'init-const)

(require 'init-package)

(require 'init-global-config)

;; Initialize the package archives:
(require 'package)

(defvar minimal-emacs-gc-cons-threshold (megabytes 16)
  "The value of `gc-cons-threshold' after Emacs startup.")

(use-package auto-compile
  :demand t
  :custom
  (auto-compile-check-parens nil)
  (auto-compile-display-buffer nil)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Garbase collector magic package.
;; Unclear if it’s of any use.
(use-package gcmh
  :diminish
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-low-cons-threshold minimal-emacs-gc-cons-threshold))

(use-package sqlite3)

;; Provides a list API.
;; Dependency of other packages.
(use-package dash)

(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

(use-package org
  :bind
  (("C-c a" . org-agenda))
  :config
  (setq org-directory "~/Documents/Notes")
  (setq org-agenda-files '("/Users/valentinmouret/Documents/Notes"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer '())

  (org-toggle-pretty-entities)

  (setq org-todo-keywords
	;; The pipe `|` signifies that entries to the right are «completed» states.
        '((sequence "BACKLOG(b)" "TODO(t)" "ACTIVE(a)" "|" "DONE(d!)")))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)

  (setq org-habit-graph-column 60))


(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (file-truename "~/Documents/Notes"))
  (setq org-roam-dailies-directory "Journal")

  :custom
  (org-roam-dailies-capture-templates '(("d" "default" entry
                                         "* %?"
                                         :target (file+head "%<%Y-%m-%d>.org"
                                                            "#+title: %<%Y-%m-%d>\n"))))

  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n d" . org-roam-dailies-goto-today)))

;;
(use-package rg
  :config
  ;; Nix installs `rg` in a place that might be hard to find?
  ;; Setting it explicitly.
  (setq rg-executable "/run/current-system/sw/bin/rg"))

(require 'init-editor)

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

(defconst flyspell-modes
  '(latex-mode
    markdown-mode
    org-mode
    outline-mode
    text-mode))
(use-package flyspell
  :diminish
  :if (executable-find "aspell")
  :hook (flyspell-modes . flyspell-mode)
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_UK" "--camel-case")))

(use-package diminish)

(use-package emojify
  :bind ("C-x e" . emoji-insert))
(emojify-mode t)

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

(use-package nerd-icons
  :config (nerd-icons-install-fonts 1))

(use-package doom-modeline
  :after nerd-icons
  :init (doom-modeline-mode 1)
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-minor-modes t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-height 15)
  :config
  (doom-modeline-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init     (which-key-mode)
  :diminish which-key-mode
  :config   (setq which-key-idle-delay 0.3))

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

(use-package nushell-mode)

(use-package lsp-mode
  :custom
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (lsp-keymap-prefix "C-c l")
  (lsp-prefer-flymake nil)
  (lsp-enable-folding nil)
  (read-process-output-max (* 1024 1024))
  (lsp-eldoc-hook nil)
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-enable-indentation nil) ; Test
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
  (setq lsp-zig-zls-executable "/run/current-system/sw/bin/zls")
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

(use-package treemacs
  :bind
  ("C-c t" . treemacs))

(use-package treemacs-projectile
  :after (:all treemacs projectile))

;; AI
(use-package gptel
  :config
  (setq gptel-model 'claude-3-5-sonnet-20240620
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (auth-source-pick-first-password :host "anthropic.com"))))


;; Haskell

(use-package haskell-mode)

(use-package lsp-haskell
  :hook (haskell-mode))

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

(use-package flymake-hadolint
  :hook
  (dockerfile-mode-hook . flymake-mode))

;; SQL

;; Set Postgres as the default SQL product.
(setq sql-product "postgres")

;; Emacs

;; (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(global-eldoc-mode -1)

;; Lisp

(use-package paredit
  :diminish
  :config
  (show-paren-mode t)
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
  :hook
  ((cider-mode . clj-refactor-mode)))

(use-package clj-refactor
  :diminish clj-refactor-mode)

;; YAML
(use-package yaml-mode)

;; Go
(use-package go-mode
  :hook
  (before-save-hook . gofmt-before-save))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

;; JavaScript

;; (use-package rjsx-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

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

(use-package prettier-js)

(use-package typescript-mode
  :custom
  (typescript-indent-level 2)
  (indent-tabs-mode nil)
  (subword-mode)
  :hook
  (typescript-mode . (lambda ()
                      (prettier-js-mode 1)
                      (setq-local lsp-enable-on-type-formatting nil)
                      (setq-local lsp-format-on-save nil))))


;; (use-package typescript-mode
;;   :config
;;   (defun my-typescript-newline-and-indent ()
;;     (interactive)
;;     (newline)
;;     (typescript-indent-line))
;;   (setq lsp-enable-on-type-formatting nil)
;;   (setq lsp-typescript-format-enable nil)

;;   (define-key typescript-mode-map (kbd "RET") 'my-typescript-newline-and-indent)

;;   ;; Use this if you're using typescript-mode
;;   (setq typescript-indent-level 2)
;;   ;; Use this if you're using typescript-ts-mode
;;   ;; (setq typescript-ts-mode-indent-offset 2)

;;   :hook
;;   ((typescript-mode . (lambda ()
;;                         (company-mode 1)
;;                         (subword-mode 1)
;; 			(prettier-js-mode 1)
;;                         (electric-indent-local-mode -1)
;; 			(setq-local lsp-enable-on-type-formatting nil
;;                                 lsp-format-on-save nil))
;; 		    )
;; 					; (before-save . lsp-format-buffer)
;;    (before-save . prettier-js)
;;    ))

;; (use-package typescript-mode
;;   :custom
;;   (setq typescript-indent-level 2)
;;   :config
;;   (defun my-typescript-newline-and-indent ()
;;     (interactive)
;;     (newline)
;;     (typescript-indent-line))

;;   ;; Remove global format on save
;;   :hook
;;   (typescript-mode . (lambda ()
;;                       (setq-local lsp-enable-on-type-formatting nil)
;;                       (setq-local lsp-format-on-save nil)
;;                       (prettier-js-mode 1))))

;; (use-package prisma-ts-mode)

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mdx\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t))

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

(use-package sbt-mode)

(use-package scala-mode)

(use-package lsp-metals
  :ensure t
  :custom
  ;; You might set metals server options via -J arguments. This might not always work, for instance when
  ;; metals is installed using nix. In this case you can use JAVA_TOOL_OPTIONS environment variable.
  (lsp-metals-server-args '(;; Metals claims to support range formatting by default but it supports range
                            ;; formatting of multiline strings only. You might want to disable it so that
                            ;; emacs can use indentation provided by scala-mode.
                            "-J-Dmetals.allow-multiline-string-formatting=off"
                            ;; Enable unicode icons. But be warned that emacs might not render unicode
                            ;; correctly in all cases.
                            "-J-Dmetals.icons=unicode"))
  ;; In case you want semantic highlighting. This also has to be enabled in lsp-mode using
  ;; `lsp-semantic-tokens-enable' variable. Also you might want to disable highlighting of modifiers
  ;; setting `lsp-semantic-tokens-apply-modifiers' to `nil' because metals sends `abstract' modifier
  ;; which is mapped to `keyword' face.
  (lsp-metals-enable-semantic-highlighting t)
  :hook
  (scala-mode . lsp)
  (scala-mode . subword-mode))

;; Shell
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook
  (sh-mode-hook . flymake-shellcheck-load))

;; Zig
(use-package zig-mode)

;; Run Emacs in server mode.
;; This allows other programs to use it using the `EDITOR` environment variable.
(unless (boundp 'server-process)
  (server-start))

(use-package dockerfile-mode)

(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode)
  ;; Set the directory where undo-tree should save history files
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  ;; Ensure the directory exists
  (unless (file-exists-p "~/.emacs.d/undo")
    (make-directory "~/.emacs.d/undo" t)))

(use-package helpful
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command))

(use-package visual-regexp)

(use-package age
  :demand t
  :custom
  (age-program "rage")
  (age-default-recipient
   '("~/.ssh/id_ed25519.pub"))
  :config
  (age-file-enable))

;; These functions come from here: https://github.com/anticomputer/age.el
(defun my/age-github-keys-for (username)
  "Turn GitHub USERNAME into a list of ssh public keys."
  (let* ((res (shell-command-to-string
               (format "curl -s https://api.github.com/users/%s/keys"
                       (shell-quote-argument username))))
         (json (json-parse-string res :object-type 'alist)))
    (cl-assert (arrayp json))
    (cl-loop for alist across json
             for key = (cdr (assoc 'key alist))
             when (and (stringp key)
                       (string-match-p "^\\(ssh-rsa\\|ssh-ed25519\\) AAAA" key))
             collect key)))

(defun my/age-save-with-github-recipient (username)
  "Encrypt an age file to the public keys of GitHub USERNAME."
  (interactive "MGitHub username: ")
  (cl-letf (((symbol-value 'age-default-recipient)
             (append (if (listp age-default-recipient)
                         age-default-recipient
                       (list age-default-recipient))
                     (my/age-github-keys-for username))))
    (save-buffer)))

(use-package terraform-mode)

(use-package git-gutter)
(global-git-gutter-mode +1)

;; Trial zone
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (consult embark)
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-s" . consult-line)
         ("C-r" . (lambda () (interactive) (consult-line nil t)))

         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package vterm
  :ensure t
  :defer t
  :commands vterm
  :config
  ;; Speed up vterm
  (setq vterm-timer-delay 0.01))

(use-package coverlay)

(use-package s)

(use-package origami)

(use-package corfu)

(straight-use-package '(css-in-js-mode :type git :host github :repo "orzechowskid/tree-sitter-css-in-js"))

(straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))

;; end of trial zone

(require 'init-magit)

(require 'init-python)

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
   '(nix-mode flyspell-correct-ivy crux diminish amx flycheck-popup-tip flycheck-posframe json-mode web-mode flycheck-rust lsp-java java-lsp rust-mode haskell-mode adoc adoc-mode csv-mode lsp-dart evil-collection treemacs-evil evil-mode slack oauth2 dockerfile-mode docker python-black lsp-pyright flymake-shellcheck company poetry flycheck-clj-kondo clj-refactor cider-mode cider clojure-mode paredit emojify exec-path-from-shell smex uniquify prettier-js flycheck go-mode jedi blacken pyvenv yaml-mode forge markdown-mode magit counsel-projectile projectile which-key rainbow-delimiters doom-modeline all-the-icons expand-region avy ivy-hydra ivy-rich counsel swiper base16-theme use-package)))
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


(provide 'init)
;;; init.el ends here.
