;;; bash-config.el -- Configurations related to bash.
;;; Commentary:
;;; Code:

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook
  (sh-mode-hook . flymake-shellcheck-load))

(provide 'bash-config)
;;; bash-config.el ends here
