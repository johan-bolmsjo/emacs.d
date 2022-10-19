;; ----------------------------------------------------------------------------
;; ~/.emacs.d/init.el -- Snippets from from many emacsen files.
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Straight package manager: boot strapping
;; ----------------------------------------------------------------------------

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ----------------------------------------------------------------------------
;; Straight package manager: package imports
;; ----------------------------------------------------------------------------

;; About use-package coexistence with straight:
;;
;; NOTE! The ":ensure t" clause does not work as it is hard-coded to
;;       install packages using package.el. Use ":ensure nil" instead
;;       and an explicit straight-use-package before the use-package.
;;       See https://github.com/raxod502/straight.el/issues/128 for
;;       details.
(straight-use-package 'use-package)

;; Various support functions
(straight-use-package 'corfu)
(straight-use-package 'eldoc)
(straight-use-package 'flycheck)
(straight-use-package 'flymake)
(straight-use-package 'orderless)
(straight-use-package 'project)
(straight-use-package 'treemacs)
(straight-use-package 'which-key)
(straight-use-package 'xref)
(straight-use-package 'yasnippet)

;; Themes
(straight-use-package 'solarized-theme)
(straight-use-package 'color-theme-solarized)

;; Documentation formats
(straight-use-package 'markdown-mode)
(straight-use-package 'org)
(straight-use-package 'ox-jira) ; Org Jira export plugin
(straight-use-package 'plantuml-mode)

;; Programming language support
(straight-use-package 'eglot)
(straight-use-package 'magit)
(straight-use-package 'paredit)
(straight-use-package 'pyenv)
;; Only enable if used; it clones the entire CMake repo which is large!
;;(straight-use-package 'cmake-mode)

;; Programming languages
(straight-use-package 'go-mode)
(straight-use-package 'zig-mode)

;; ----------------------------------------------------------------------------
;; Miscellaneous minor tweaks
;; ----------------------------------------------------------------------------

;; Silence!
(setq ring-bell-function 'ignore)

;; Disable pager for "git grep"
(setenv "PAGER" "cat")

;; Emacs <= 26.3 does not recognize st-256color terminal.
(add-to-list 'term-file-aliases
    '("st-256color" . "xterm-256color"))

;; Disable startup message
(setq inhibit-startup-message t)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't create .saves-xxx files
(setq auto-save-list-file-prefix nil)

;; Open unidentified files in fundamental mode
(setq default-major-mode 'fundamental-mode)

;; Use UTF8 character encoding.
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; No new lines when moving past end of buffer
(setq next-line-add-newlines nil)

;; Display marked region
(setq transient-mark-mode t)

;; Allow upcase, downcase commands (we are not morons).
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Set fill column used by fill-paragraph ("M-q").
;; Value choosed for Git commit messages, change with "M-x f".
(setq-default fill-column 72)

;; Turn off stupid double space rules!
(setq sentence-end-double-space nil)

;; Follow symlinks to version controlled controlled files.
(setq vc-follow-symlinks t)

;; Display both line number and column number
(line-number-mode 1)
(column-number-mode 1)

;; Show matching parenthesis
(show-paren-mode 1)

;; Show the function we are in when editing source code.
(which-function-mode 1)

;; Compilation output
(setq compilation-scroll-output t)

;; Turn off menu and tool bar by default
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; multi-line help text in the echo area
(setq tooltip-use-echo-area t)

;; Turn on auto fill mode when editing text files.
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; Keep some context above cursor when scrolling pages.
(setq scroll-margin 5)

;; Not a big fan of pulse animations, find it slightly distracting.
(setq pulse-iterations 0)

;; There is no keybinding for this, just do M-x reload-file if you need it.
(defun reload-file ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))

;; ----------------------------------------------------------------------------
;; Keybindings
;; ----------------------------------------------------------------------------

(global-set-key (kbd "C-c a") 'align-current)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c g") 'find-grep)
(global-set-key (kbd "C-c i") 'ispell-region)
(global-set-key (kbd "C-c j") 'goto-line)
(global-set-key (kbd "C-c s") 'sort-lines)
(global-set-key (kbd "C-c t") 'treemacs)

(defun my/isearch-mark-and-exit ()
  (interactive)
  (isearch-done)
  (push-mark isearch-other-end 'no-message 'activate))

(define-key isearch-mode-map (kbd "C-c k") 'my/isearch-mark-and-exit)
(define-key isearch-mode-map (kbd "C-c o") 'isearch-occur)
(define-key isearch-mode-map (kbd "C-c SPC") 'isearch-toggle-lax-whitespace)

(defun my/lock-frame ()
  "Lock current frame for automatic splitting"
  (interactive)
  (set-frame-parameter nil 'unsplittable t)
  (message "Frame is locked for automatic splitting"))

(defun my/unlock-frame ()
  "Unlock current frame for automatic splitting"
  (interactive)
  (set-frame-parameter nil 'unsplittable nil)
  (message "Frame is unlocked for automatic splitting"))

(defun my/lock-window ()
  "Lock window to its current buffer"
  (interactive)
  (message
   (let (window (get-buffer-window (current-buffer)))
     (set-window-dedicated-p window t)
     "Window '%s' is locked")
   (current-buffer)))

(defun my/unlock-window ()
  "Unlock window from its current buffer"
  (interactive)
  (message
   (let (window (get-buffer-window (current-buffer)))
     (set-window-dedicated-p window nil)
     "Window '%s' is unlocked")
   (current-buffer)))

(global-set-key (kbd "C-c F") 'my/lock-frame)
(global-set-key (kbd "C-c f") 'my/unlock-frame)
(global-set-key (kbd "C-c W") 'my/lock-window)
(global-set-key (kbd "C-c w") 'my/unlock-window)

(global-set-key (kbd "C-c n") 'org-capture)
(global-set-key (kbd "C-c p") 'org-agenda)
(global-set-key (kbd "C-c L") 'org-store-link)

(global-set-key (kbd "<f9>") 'next-error)
(global-set-key (kbd "<f10>") 'previous-error)

(global-set-key (kbd "M-n") 'scroll-up-command)
(global-set-key (kbd "M-p") 'scroll-down-command)

(defun my/backward-symbol (&optional arg)
  "Move backward until encountering the beginning of a symbol.
With argument, do this that many times."
  (interactive "p")
  (forward-symbol (- (or arg 1))))

(global-set-key (kbd "M-f") 'forward-symbol)
(global-set-key (kbd "M-b") 'my/backward-symbol)
(global-set-key (kbd "M-C-f") 'forward-paragraph)
(global-set-key (kbd "M-C-b") 'backward-paragraph)

;; Kill the default buffer without questions.
(defun my/fast-kill-buffer ()
   (interactive)
   (kill-buffer (buffer-name)))

(define-key global-map [C-delete] 'my/fast-kill-buffer)
(substitute-key-definition 'kill-buffer 'my/fast-kill-buffer (current-global-map))

;; Invoke "M-x list-buffers" to see the full buffer list.
(global-set-key (kbd "C-x C-b") 'bs-show)

;; ----------------------------------------------------------------------------
;; Interactive keybinding reminder support
;; ----------------------------------------------------------------------------

(which-key-mode)

;; ----------------------------------------------------------------------------
;; Fonts
;; ----------------------------------------------------------------------------

(require 'cl-lib)
(defun my/font-candidate (&rest fonts)
  "Return existing font which first match."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(when (display-graphic-p)
  (set-face-attribute 'default nil :font (my/font-candidate "Go Mono-9" "DejaVu Sans Mono-10")))

;; ----------------------------------------------------------------------------
;; Color theme
;; ----------------------------------------------------------------------------

(if window-system
    (progn
      (setq x-underline-at-descent-line t)
      ;; Don't change the font for some headings and titles
      (setq solarized-use-variable-pitch nil)
      (load-theme 'solarized-light t))
  (progn
    ;; Use an alternative solarized theme that handle terminals that are
    ;; in solarized color mode better. The theme works fine for me in
    ;; PuTTY but have some problems in urxvt.
    (load-theme 'solarized t)
    ;;(setq frame-background-mode 'light)
    ))

;; ----------------------------------------------------------------------------
;; Whitespace config
;; ----------------------------------------------------------------------------

(setq-default
 whitespace-line-column 120
 whitespace-action '(auto-cleanup)
 whitespace-style
 '(face empty trailing))

(add-hook 'prog-mode-hook 'whitespace-mode)

;; ----------------------------------------------------------------------------
;; Improved interactive file and buffer completion
;; ----------------------------------------------------------------------------

(require 'ido)

;; Prevent IDO from looking for non existing files in other directories.
;; I find it annoying as most of the time I'm about to create a new file.
(setq ido-auto-merge-work-directories-length -1)

(setq ido-enable-flex-matching t)
(ido-mode t)

;; ----------------------------------------------------------------------------
;; Text templates
;; ----------------------------------------------------------------------------

(use-package yasnippet
	     :ensure nil
	     :hook
	     ((prog-mode org-mode) . yas-minor-mode)
	     :config
	     (yas-reload-all))

;; ----------------------------------------------------------------------------
;; Indexed grep search tool
;; ----------------------------------------------------------------------------

(defvar my/grep-find-command "~/.emacs.d/bin/csearch-color "
  "Grep find command.")

;; Override the grep-find (and find-grep) command to call csearch instead.
(let ((cmd (bound-and-true-p my/grep-find-command)))
  (when cmd
    (setq grep-find-command cmd)))

;; ----------------------------------------------------------------------------
;; markdown-mode (https://github.com/jrblevin/markdown-mode)
;;
;; Markdown documentation mode
;; ----------------------------------------------------------------------------

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;;(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; ----------------------------------------------------------------------------
;; Org documentation mode
;; ----------------------------------------------------------------------------

;; Hide markup characters for bold, italics, underline etc.
;;(setq org-hide-emphasis-markers t)

;; Word wrap mode.
(add-hook 'org-mode-hook 'visual-line-mode)

;; Indent according to headline level.
(add-hook 'org-mode-hook 'org-indent-mode)

;; Put all task logs into LOGBOOK drawer.
(setq org-log-into-drawer t)

;; Log time stamp when marking a task as done.
;; Control time stamping per org mode file using TODO keyword specification instead.
;; See https://orgmode.org/manual/Tracking-TODO-state-changes.html for details.
;;(setq org-log-done 'time)

;; Ask for log note when marking a task as done.
;; Control logging per org mode file using TODO keyword specification instead.
;; See https://orgmode.org/manual/Tracking-TODO-state-changes.html for details.
;;(setq org-log-done 'note)

;; Prevent parent tasks from being marked as DUNE unless all child tasks are DONE.
(setq org-enforce-todo-dependencies t)

;; Syntax highlight src blocks.
(setq org-src-fontify-natively t)

;; HTML output: Don't emit validation link
(setq org-html-validation-link nil)

;; Export via ODT format
(setq org-odt-preferred-output-format "pdf")
(setq org-odt-fontify-srcblocks nil)

;; Text file export line width
(setq org-ascii-text-width 80)

;; org-agenda
(setq org-agenda-span 14)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-deadline-warning-days 2)
(setq org-agenda-show-future-repeats 'next)
(setq org-agenda-prefer-last-repeat '("REPEATED"))

;; System locale to use for formatting time values. Make sure that the weekdays
;; in the time stamps of Org mode files and in the agenda appear in English.
(setq system-time-locale "C")

;; yasnippet org-mode TAB key workaround
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

;; yasnippet org-mode TAB key workaround
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet (using the new org-cycle hooks)
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

;; yasnippet org-mode workaround:
;;
;; Yasnippet grabs the C-& prefix which is normally bound to org-mark-ring-goto.
;; Bind the org-mode navigation commands to the keys normally used for xref based navigation.
;;
;; FUTURE: Find a way to push entries on the xref stack when navigating form an
;;         Org file to some other file. This way it would work to navigate back
;;         from the other file to the Org file.
;;
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-,") 'org-mark-ring-goto) ;; xref-pop-marker-stack
	    (local-set-key (kbd "M-.") 'org-open-at-point)  ;; xref-find-definitions
	    ))

(require 'org-tempo)

;; ----------------------------------------------------------------------------
;; PlantUML (https://plantuml.com/)
;; ----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

;; ----------------------------------------------------------------------------
;; Code completion
;; - https://github.com/minad/corfu
;; - https://github.com/oantolin/orderless
;; ----------------------------------------------------------------------------

(use-package corfu
             :ensure nil
	     :custom
	     (corfu-cycle t)          ;; Enable cycling for `corfu-next/previous'
	     (corfu-separator ?\s)    ;; Set to orderless separator
	     (corfu-scroll-margin 5)  ;; Use scroll margin
	     :bind
	     ;; Configure SPC for separator insertion
	     (:map corfu-map ("SPC" . corfu-insert-separator))
	     :init
	     (global-corfu-mode))

;; TAB cycle if there are only few candidates.
;; Disabled because it can hide that there are more candidates.
;;(setq completion-cycle-threshold 3)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
;; Unfortunately this does not work for the C/C++ mode.
(setq tab-always-indent 'complete)

;; Use the `orderless' completion style.
(use-package orderless
             :ensure nil
	     :custom
	     (completion-styles '(orderless basic))
	     (completion-category-overrides '((file (styles basic partial-completion))
					      (eglot (styles orderless basic)))))

;; ----------------------------------------------------------------------------
;; Diagnostics such as compiler errors
;; ----------------------------------------------------------------------------

(use-package flymake
	     :ensure nil
	     :config
	     ;; Match often used keybindings with flycheck
	     (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-buffer-diagnostics)
	     (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
	     (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
	     )

;; ----------------------------------------------------------------------------
;; Langauge server protocol support
;; ----------------------------------------------------------------------------

;; Do not allow expansion of the echo area for displaying documentation
;; for the symbol at point. Invoke "C-h ." to show the full documentation at point.
(setq eldoc-echo-area-use-multiline-p 1)
(setq eldoc-echo-area-display-truncation-message nil)

(use-package eglot
  :ensure nil
  :config
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'ocaml-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'zig-mode-hook 'eglot-ensure)
  ;; format on save
  (add-hook 'go-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (define-key eglot-mode-map (kbd "C-c e a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e i") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c e t") 'eglot-find-typeDefinition)
  )

;; ----------------------------------------------------------------------------
;; Shell scripts
;; ----------------------------------------------------------------------------

(add-hook 'sh-mode-hook 'flycheck-mode)

;; ----------------------------------------------------------------------------
;; C programming language (common C/C++)
;; ----------------------------------------------------------------------------

(defun my/c-mode-common-hook ()
  (c-set-style "Linux")
  (setq
   indent-tabs-mode nil
   c-basic-offset 4)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)
  (local-set-key  (kbd "C-c o") 'ff-find-other-file))

(add-hook 'c-mode-common-hook 'my/c-mode-common-hook)

(setq c-doc-comment-style '((c-mode . doxygen)
                            (c++-mode . doxygen)))

;; ----------------------------------------------------------------------------
;; C++ programming language
;; ----------------------------------------------------------------------------

(defun my/c++-setup ()
  (c-set-offset 'innamespace [0]))

(add-hook 'c++-mode-hook 'my/c++-setup)

;; ----------------------------------------------------------------------------
;; Go programming language
;; ----------------------------------------------------------------------------

(require 'go-mode-autoloads)

;; ----------------------------------------------------------------------------
;; OCaml programming language
;; ----------------------------------------------------------------------------

(let ((opam-share-dir (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share-dir (file-directory-p opam-share-dir))
    (let* ((opam-site-list-dir (concat opam-share-dir "/emacs/site-lisp"))
	   (opam-tuareg-site-file (format "%s/%s" opam-site-list-dir "tuareg-site-file")))

      ;; tuareg
      (when (file-exists-p (concat opam-tuareg-site-file ".el"))
	(load opam-tuareg-site-file))

      ;; ocp-indent
      (add-to-list 'load-path opam-site-list-dir)
      (require 'ocp-indent)

      ;; merlin
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share-dir))
      (autoload 'merlin-mode "merlin" nil t nil)
      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)
      (setq merlin-command 'opam))))

;; ----------------------------------------------------------------------------
;; Python programming language
;; ----------------------------------------------------------------------------

(use-package pyenv
  :ensure nil
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

;; ----------------------------------------------------------------------------
;; Zig programming language
;; ----------------------------------------------------------------------------

;; No configuration except from package import

;; ----------------------------------------------------------------------------
;; Makefile
;; ----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-mode))

;; ----------------------------------------------------------------------------
;; GDB
;; ----------------------------------------------------------------------------

;; Activate by "M-x gdb" followed by "M-x gdb-many-windows".
;;
;; NOTE: The toolbar contain debugger navigation icons so you may want to enable
;;       that while debugging using "M-x tool-bar-mode".

;; Continue program being debugged.
(global-set-key (kbd "<f5>") 'gud-cont)

;; Step till next source line, do not enter subroutine (next).
(global-set-key (kbd "<f6>") 'gud-next)

;; Step till next source line, enter subroutine (step).
(global-set-key (kbd "<f7>") 'gud-step)

;; Execute until current stack frame returns.
(global-set-key (kbd "<f8>") 'gud-finish)

;; Stop debugging
(global-set-key (kbd "<S-f8>") (lambda ()
                               (interactive)
                               (gud-stop-subjob)
                               (comint-interrupt-subjob)))

;; ----------------------------------------------------------------------------
;; Customized Variables
;; ----------------------------------------------------------------------------

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
