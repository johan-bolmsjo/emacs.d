(defun my/set-font (height)
  "Set default fonts with the specified height"
  (set-face-attribute 'default nil :family "Go Mono" :height height)
  (set-face-attribute 'variable-pitch nil :family "Go" :height height))

(defun my/font+0 ()
  "Set default fonts"
  (interactive)
  (my/set-font 90))

(defun my/font+1 ()
  "Set default fonts with increased size."
  (interactive)
  (my/set-font 100))

(defun my/font+2 ()
  "Set default fonts with increased size."
  (interactive)
  (my/set-font 110))

(defun my/font+3 ()
  "Set default fonts with increased size."
  (interactive)
  (my/set-font 120))

(when (display-graphic-p)
  (my/font+0))
