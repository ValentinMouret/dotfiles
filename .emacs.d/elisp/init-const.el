;;; init-const.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-const.el
;; Description: Initialize Constants
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes constants.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; UserInfo
(setq user-full-name "Valentin Mouret")
(setq user-mail-address "valentin.mouret@hey.com")
;; -UserInfo

;; Consts
(defconst *sys/linux?*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac?*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")
;; -Consts

(provide 'init-const)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
