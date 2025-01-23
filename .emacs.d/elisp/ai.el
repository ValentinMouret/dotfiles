;;; package --- Summary

;;; Commentary:

;;; Code:

(defconst ai-buffer "*ai-buffer*")

(defun ask
    ()
  "Asks stuf."
  (interactive)
  (switch-to-buffer ai-buffer)
  )

(provide 'ai)

;;; ai.el ends here
