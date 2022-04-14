;;;; init.el --- File launched by Emacs on every startup.

;;;; Commentary:
;;
;; It installs the package manager `use-package` and sets up
;; the different packages for the core functionalities, the editor,
;; and the support for each language.

;;;; Code:

;; Initialize the package archives:

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(defun recompile-source ()
  "Recompiles sources as they sometimes get stuck."
  (interactive "p")
  (byte-recompile-directory package-user-dir nil 'force))

(setq package-check-signature nil)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Install if they are not available.
(setq use-package-always-ensure t)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;;;; General setup
;;
;; Delete trailing whitespaces on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Always end files with a newline.
(setq require-final-newline t)

;; Don’t use tabs for indentation.
; (setq-default indent-tabs-mode nil)

;; Shorten yes/no prompts to y/n.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable ring bell noises.
(setq ring-bell-function 'ignore)


;;;; End of general setup.

;;;; Editor configuration.

(setq mac-left-option-modifier 'meta)
(setq mac-left-command-modifier 'meta)
(setq mac-right-option-modifier nil)

(use-package exec-path-from-shell)
;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

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

(use-package emojify)

;; Load the dark theme from base16.
(use-package base16-theme)
(setq base16-highlight-mode-line 'false)
(setq base16-theme-256-color-source "colors")
(load-theme 'base16-default-dark t)
;; (load-theme 'base16-default-light t)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(set-frame-font "Iosevka Term 16" nil t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-everywhere t)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package auto-package-update
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 1)
   (auto-package-update-maybe))

(use-package smex
  ;; Enhances M-x to allow easier execution of commands. Provides
  ;; a filterable list of possible commands in the minibuffer
  ;; http://www.emacswiki.org/emacs/Smex
  :config
  (smex-initialize)
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  :bind
  ("M-x" . smex))

(use-package swiper)
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
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         :map ivy-switch-buffer-map
         :map ivy-reverse-i-search-map
         ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1))

(use-package evil
  :init (setq evil-want-keybinding nil)
  :bind
  ("M-." . xref-find-definitions))

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
  (define-key evil-motion-state-map "J" 'evil-find-char-to-backward))

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
  (when (file-directory-p "~/Code")
    (setq projectile-project-search-path '("~/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :init
  (setq auth-sources '("~/.authinfo"))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package forge)

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")  
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (go-mode . lsp)
         (typescript-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  (dolist (m '(clojure-mode
	       typescript-mode
               javascript-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode
               go-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-treemacs)

;; Treemacs

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package all-the-icons)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

;; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;   :after (treemacs)
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Tabs))

;;;; Language support
;; General
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
        111000000000
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


(use-package emojify
  :hook (after-init . global-emojify-mode))
(add-hook 'after-init-hook #'global-emojify-mode)

;; SQL

(use-package sqlformat
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "--no-extra-line")))

;; Emacs

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

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
  :hook
  ((cider-mode . clj-refactor-mode)
   (before-save . cider-format-buffer))
  )


(use-package clj-refactor
  :diminish clj-refactor-mode
  )

;; Racket

(use-package racket-mode)

;; YAML
(use-package yaml-mode)

;; Python
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
			     ))
   )
  )

;; python-black
(use-package python-black
  :hook
  (python-mode . python-black-on-save-mode)
  :init
  (put 'python-black-command 'safe-local-variable #'stringp)
  (put 'python-black-extra-args 'safe-local-variable #'stringp)
  (put 'python-black-on-save-mode 'safe-local-variable #'booleanp)
  )

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
  (before-save-hook . gofmt-before-save))

;; JavaScript
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
   (js-mode-hook . subword-mode))
)

;; Typescript
(use-package company)
(use-package typescript-mode
  :hook
  ((typescript-mode . company-mode)
   (typescript-mode . subword-mode)
   (typescript-mode . prettier-js-mode))
  )

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ;(before-save . tide-format-before-save)
	 )
  :config
  (setq company-idle-delay 0))

;; Markdown
(use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

;; Java

;;
;; switch java
;;
(setq JAVA_BASE "/Library/Java/JavaVirtualMachines")

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

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook
  (sh-mode-hook . flymake-shellcheck-load))

;; Custom
;; This area is set by Custom. Don’t touch it.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" default))
 '(package-selected-packages
   '(evil-collection treemacs-evil evil-mode slack oauth2 dockerfile-mode docker python-black lsp-pyright flymake-shellcheck company auto-package-update tide poetry flycheck-clj-kondo clj-refactor cider-mode cider clojure-mode paredit emojify exec-path-from-shell smex uniquify prettier-js flycheck go-mode jedi blacken pyvenv yaml-mode forge markdown-mode magit counsel-projectile projectile which-key rainbow-delimiters doom-modeline all-the-icons expand-region avy ivy-hydra ivy-rich counsel swiper base16-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here.
