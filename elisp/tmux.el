;;; tmux.el --- Interact with tmux -*- lexical-binding: t; -*-

;; Copyright (C) 2025 by Johan Bolmsjö
;; Copyright (C) 2016 by Syohei YOSHIDA
;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-emamux

;;; Code:

(require 'cl-lib)

(defsubst tmux-running-p ()
  (zerop (process-file "tmux" nil nil nil "has-session")))

(defun tmux-run-command (output &rest args)
  (let ((retval (apply 'process-file "tmux" nil output nil args)))
    (unless (zerop retval)
      (error (format "Failed: %s(status = %d)"
                     (mapconcat 'identity (cons "tmux" args) " ")
                     retval)))))

(defun tmux-check-running ()
  (unless (tmux-running-p)
    (error "'tmux' does not run on this machine!!")))

(defun tmux-set-buffer-argument (index data)
  (if (zerop index)
      (list data)
    (list "-b" (number-to-string index) data)))

(defun tmux-set-buffer (data index)
  (let ((args (tmux-set-buffer-argument index data)))
    (apply 'tmux-run-command nil "set-buffer" args)))

;;;###autoload
(defun tmux-copy-kill-ring (arg)
  "Set (car kill-ring) to tmux buffer"
  (interactive "P")
  (tmux-check-running)
  (when (null kill-ring)
    (error "kill-ring is nil!!"))
  (let ((index (or arg 0))
        (data (substring-no-properties (car kill-ring))))
    (tmux-set-buffer data index)))

(provide 'tmux)

;;; tmux.el ends here
