;;; encryption.el --- -*- lexical-binding: t -*-
;;
;; Filename: encryption.el
;; Description: Handles basic encryption
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Provides utilities for working with encryption from Emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun vm/age-github-keys-for (username)
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

(defun vm/age-save-with-github-recipient (username)
  "Encrypt an age file to the public keys of GitHub USERNAME."
  (interactive "MGitHub username: ")
  (cl-letf (((symbol-value 'age-default-recipient)
             (append (if (listp age-default-recipient)
                         age-default-recipient
                       (list age-default-recipient))
                     (vm/age-github-keys-for username))))
    (save-buffer)))

(provide 'encryption)
;;; encryption.el ends here
