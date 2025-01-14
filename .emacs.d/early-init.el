;;; early-init.el --- -*- lexical-binding: t -*-
;;
;; Filename: early-init.el
;; Description: Early initialization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file is run before init.el.
;; Draws inspiration from:
;; - https://github.com/jamescherti/minimal-emacs.d
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun power (x n)
  "Raise X to the power of N."
  (expt x n))

(defun kilobytes (n)
  "Return N kilobytes in bytes."
  (* n (power 10 3)))

(defun megabytes (n)
  "Return N megabytes in bytes."
  (* n (power 10 6)))

(defun gigabytes (n)
  "Return N gigabytes in bytes."
  (* n (power 10 9)))

;; Increase the garbage collection threshold to speed up the initilization.
(defvar better-gc-cons-threshold (megabytes 16))

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)))

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (kilobytes 512))  ; 512kb

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)
(setq warning-suppress-types '((lexical-binding)))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; By default, Emacs "updates" its ui more often than it needs to
(setq idle-update-delay 1.0)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

;; Allow for shorter responses: "y" for yes and "n" for no.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))
(defalias #'view-hello-file #'ignore)  ; Never show the hello file

;; LoadPath
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "elisp" user-emacs-directory))
;; -LoadPath

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
