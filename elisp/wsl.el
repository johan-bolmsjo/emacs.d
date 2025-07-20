;;; wsl.el --- Interact with Windows Subsystem for Linux (wsl) -*- lexical-binding: t; -*-

;;; Code:

;;;###autoload

(defun wsl-copy-kill-ring (&rest _args)
  "Copy kill-ring to windows clipboard in WSL."
  (interactive)
  (setq mytemp (make-temp-file "winclip"))
  (write-region (current-kill 0 t) nil mytemp)
  (shell-command (concat "clip.exe<" mytemp))
  (delete-file mytemp))

(provide 'wsl)

;;; wsl.el ends here
