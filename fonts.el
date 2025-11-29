(defun my/set-font (height)
  "Set default fonts with the specified height"
  (set-face-attribute 'default nil :family "Go Mono" :height height)
  (set-face-attribute 'variable-pitch nil :family "Go" :height height))

(defun my/font+00 ()
  "Set default font with relative size 0.0"
  (interactive)
  (my/set-font 90))

(defun my/font+05 ()
  "Set default font with relative size 0.5"
  (interactive)
  (my/set-font 95))

(defun my/font+10 ()
  "Set default font with relative size 1.0"
  (interactive)
  (my/set-font 100))

(defun my/font+15 ()
  "Set default font with relative size 1.5"
  (interactive)
  (my/set-font 105))

(defun my/font+20 ()
  "Set default font with relative size 2.0"
  (interactive)
  (my/set-font 110))

(defun my/font+25 ()
  "Set default font with relative size 2.5"
  (interactive)
  (my/set-font 115))

(defun my/font+30 ()
  "Set default font with relative size 3.0"
  (interactive)
  (my/set-font 120))

(when (display-graphic-p)
  (my/font+00))
