;; ----------------------------------------------------------------------------
;; ~/.emacs.d/init.el -- Snippets from from many emacsen files.
;; ----------------------------------------------------------------------------

;; Set load path for locally maintained packages.
(add-to-list 'load-path (concat user-emacs-directory "/elisp"))

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
(straight-use-package 'consult)
(straight-use-package 'corfu)
(straight-use-package 'corfu-terminal)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'flycheck)
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package 'treemacs)
(straight-use-package 'vertico)
(straight-use-package 'yasnippet)
(straight-use-package 'visual-fill-column)
(straight-use-package 'xclip)

;; Documentation formats
(straight-use-package 'markdown-mode)
(straight-use-package 'denote)
(straight-use-package 'ox-gfm)  ; Org Git flavored markdown export plugin
(straight-use-package 'plantuml-mode)
(straight-use-package 'gnuplot)

;; Data formats
(straight-use-package 'csv-mode)
(straight-use-package 'yaml-mode)

;; Programming language support
(straight-use-package 'magit)
(straight-use-package 'paredit)
(straight-use-package 'ppindent)
(straight-use-package 'pyenv)
;; Only enable cmake-mode if used as the entire source repo is cloned!
;;(straight-use-package 'cmake-mode)
;; Only enable protobuf-mode if used as the entire source repo is cloned!
;;(straight-use-package 'protobuf-mode)

;; Programming languages
(straight-use-package 'go-mode)
(straight-use-package 'zig-mode)
(straight-use-package 'slime)

;; ----------------------------------------------------------------------------
;; Miscellaneous minor tweaks
;; ----------------------------------------------------------------------------

;; Silence!
(setq ring-bell-function 'ignore)

;; Disable pager for "git grep"
(setenv "PAGER" "cat")

;; Mostly for eshell commands launching an editor; requires "M-x server-start" to be effective.
(setenv "EDITOR" "emacsclient -n")

;; Persist minibuffer history over Emacs restarts.
;; This can help completion modes to remember often used commands.
(savehist-mode)

;; Disable startup message
(setq inhibit-startup-message t)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't create .saves-xxx files
(setq auto-save-list-file-prefix nil)

;; Open unidentified files in fundamental mode
(setq default-major-mode 'fundamental-mode)

;; Use UTF8 character encoding
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; No new lines when moving past end of buffer
(setq next-line-add-newlines nil)

;; Display marked region
(setq transient-mark-mode t)

;; Allow upcase, downcase commands (we are not morons)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Set fill column used by fill-paragraph ("M-q").
;; Value choosed for Git commit messages, change with "M-x f".
(setq-default fill-column 72)

;; Turn off stupid double space rules!
(setq sentence-end-double-space nil)

;; Follow symlinks to version controlled controlled files
(setq vc-follow-symlinks t)

;; Don't look for obsolete version control systems
(setq vc-handled-backends '(Git Hg))

;; Disable use of lock files to speed up remote file access
(setq remote-file-name-inhibit-locks t)

;; Display both line number and column number
(line-number-mode 1)
(column-number-mode 1)

;; Highlight the current line
(global-hl-line-mode)

;; Display line numbers for each line
(global-display-line-numbers-mode)

;; Show matching parenthesis
(show-paren-mode 1)

;; Show the function we are in when editing source code
;;
;; TODO: This triggers an error message when treemacs is enabled:
;;       which-func-ff-hook error: (wrong-type-argument arrayp nil)
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

(defun my/split-policy-wide ()
    "Frame split policy for wide displays."
    (interactive)
    (setq split-height-threshold nil))

(defun my/split-policy-tall ()
    "Frame split policy for tall displays (e.g. LG DualUp monitor)."
    (interactive)
    (setq split-height-threshold 60))

(my/split-policy-wide)

;; Keep some context above cursor when scrolling pages.
(setq scroll-margin 5)

;; Not a big fan of pulse animations, find it slightly distracting.
(setq pulse-iterations 0)

;; Mouse wheel scroll speed.
(setq mouse-wheel-scroll-amount '(0.05))
(setq mouse-wheel-progressive-speed t)

;; Listen to terminal mouse events.
(xterm-mouse-mode t)

;; There is no keybinding for this, just do M-x reload-file if you need it.
(defun reload-file ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))

(setq x-underline-at-descent-line t)

;; ----------------------------------------------------------------------------
;; Keybindings
;; ----------------------------------------------------------------------------

(global-set-key (kbd "C-c a") 'align-current)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c g") 'find-grep)
(global-set-key (kbd "C-c i") 'ispell-region)
(global-set-key (kbd "C-c s") 'sort-lines)
(global-set-key (kbd "C-c t") 'treemacs)

(defun my/isearch-mark-and-exit ()
  (interactive)
  (isearch-done)
  (push-mark isearch-other-end 'no-message 'activate))

(define-key isearch-mode-map (kbd "C-c k") 'my/isearch-mark-and-exit)

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

;; ----------------------------------------------------------------------------
;; Copy Pasta
;; ----------------------------------------------------------------------------

(require 'tmux)
(require 'wsl)

(global-set-key (kbd "<f12>") 'tmux-copy-kill-ring)
(global-set-key (kbd "<S-f12>") 'wsl-copy-kill-ring)

(xclip-mode)

;; ----------------------------------------------------------------------------
;; Interactive keybinding reminder support
;; ----------------------------------------------------------------------------

(which-key-mode)

;; ----------------------------------------------------------------------------
;; Fonts
;; ----------------------------------------------------------------------------

(load (expand-file-name "fonts.el" user-emacs-directory))

;; ----------------------------------------------------------------------------
;; Color theme
;; ----------------------------------------------------------------------------

(load-theme 'modus-operandi t)

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
;; Text templates
;; ----------------------------------------------------------------------------

(use-package yasnippet
  :ensure nil
  :hook
  ((prog-mode org-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

;; ----------------------------------------------------------------------------
;; Markdown: Documentation format
;; - https://github.com/jrblevin/markdown-mode
;; ----------------------------------------------------------------------------

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;;(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; ----------------------------------------------------------------------------
;; Denote: Structured note taking
;; - https://protesilaos.com/emacs/denote
;; ----------------------------------------------------------------------------

;; This is a lightly edited sample configuration from https://protesilaos.com/emacs/denote.
;;
;; Denote provides integration with many other packages that is not
;; configured here. See the documentation linked to above for details.

(use-package denote
  :ensure nil
  :hook
  (;; If you use Markdown or plain text files, then you want to make
   ;; the Denote links clickable (Org renders links as buttons right
   ;; away)
   (text-mode . denote-fontify-links-mode-maybe)
   ;; Apply colours to Denote names in Dired.  This applies to all
   ;; directories.  Check `denote-dired-directories' for the specific
   ;; directories you may prefer instead.  Then, instead of
   ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
   (dired-mode . denote-dired-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  ( :map global-map
    ("C-c n n" . denote)
    ("C-c n N" . denote-type)
    ("C-c n j" . my/denote-journal)
    ("C-c n J" . my/denote-journal-type)
    ("C-c n d" . denote-sort-dired)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    ("C-c n l" . denote-link)
    ("C-c n L" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ;; Note that `denote-rename-file' can work from any context, not just
    ;; Dired bufffers.  That is why we bind it here to the `global-map'.
    ("C-c n r" . denote-rename-file)
    ("C-c n R" . denote-rename-file-using-front-matter)

    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))

  :config
  ;; Remember to check the doc string of each of those variables.
  ;;(setq denote-directory (expand-file-name "~/notes"))  ; Configured in custom.el
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("metanote"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.
  (setq denote-backlinks-show-context t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))


;; Custom command to create a journal entry with a predefined "journal"
;; keyword in a dedicated sub directory.
(defun my/denote-journal ()
  "Create an entry tagged 'journal', while prompting for a title and additional keywords."
  (interactive)
  (denote
   (denote-title-prompt)
   (denote-keywords-sort (cons "journal" (denote-keywords-prompt)))
   nil
   (concat denote-directory "/journal")))

;; Custom command to create a journal entry with a predefined "journal"
;; keyword in a dedicated sub directory, asking for the file type.
(defun my/denote-journal-type ()
  "Create an entry tagged 'journal', while prompting for the file type, a title and additional keywords."
  (interactive)
  (let ((file-type (denote-file-type-prompt)))
    (denote
     (denote-title-prompt)
     (denote-keywords-sort (cons "journal" (denote-keywords-prompt)))
     file-type
     (concat denote-directory "/journal"))))

;; ----------------------------------------------------------------------------
;; PlantUML: Diagrams etc
;; - https://plantuml.com/
;; ----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

;; ----------------------------------------------------------------------------
;; Org: Documentation format, planning etc
;; - https://orgmode.org/
;; ----------------------------------------------------------------------------

;; Hide markup characters for bold, italics, underline etc.
;; I find this results in a confusing editing experience, so is disabled.
;;(setq org-hide-emphasis-markers t)

;; Word wrap mode.
(add-hook 'org-mode-hook 'visual-line-mode)

;; Indent according to headline level.
(add-hook 'org-mode-hook 'org-indent-mode)

;; Put all task logs into LOGBOOK drawer.
(setq org-log-into-drawer t)

;; Prevent parent tasks from being marked as DUNE unless all child tasks are DONE.
(setq org-enforce-todo-dependencies t)

;; Syntax highlight src blocks.
(setq org-src-fontify-natively t)

;; Org mode integration of PlantUML and gnuplot.
;; Also see customization of org-plantuml-jar-path in custom.el
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (gnuplot . t)
   (shell . t)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))))

;; org-agenda
(setq org-agenda-prefer-last-repeat '("REPEATED"))
(setq org-agenda-show-future-repeats 'next)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-span 14)
(setq org-deadline-warning-days 2)

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

;; See https://orgmode.org/manual/Structure-Templates.html
(require 'org-tempo)

;; Markdown export
(eval-after-load "org"
  '(require 'ox-md nil t))

;; Set default export options.
;; Override on a per Org file basis with "#+options:".
;; See https://orgmode.org/manual/Export-Settings.html
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers 0)
(setq org-export-headline-levels 4)
(setq org-export-with-author nil)
(setq org-export-time-stamp-file nil)
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-date nil)
(setq org-export-with-smart-quotes t)

(setq org-odt-preferred-output-format "pdf")
(setq org-odt-fontify-srcblocks nil)

(setq org-ascii-text-width 80)

(setq org-html-head-include-default-style nil)
(setq org-html-metadata-timestamp-format "%Y-%m-%d")
(setq org-html-validation-link nil)
(load (expand-file-name "css/org-main-optimized.css.el" user-emacs-directory))

;; Export LaTeX with normal-sized margins (fullpage package).
;; Depends on: texlive-latex-extra
(add-to-list 'org-latex-packages-alist '("" "fullpage"))
(add-to-list 'org-latex-packages-alist '("avoid-all" "widows-and-orphans"))
(add-to-list 'org-latex-packages-alist '("" "svg"))

;; Export LaTeX without paragraph indentation; use vertical skip instead.
;; Depends on: texlive-latex-recommended
(add-to-list 'org-latex-packages-alist '("" "parskip"))

;; Export LaTeX with left aligned tables
(setq org-latex-tables-centered nil)

;; Export LaTeX with A4 paper size
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("a4-article" "\\documentclass[11pt,a4paper]{article}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; "plain-article" requires an org document setup file to be loaded, such as ./latex-setupfile.org
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("plain-article" "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-default-class "a4-article")

;; ----------------------------------------------------------------------------
;; Vertico: VERTical Interactive COmpletion
;; - https://github.com/minad/vertico
;; ----------------------------------------------------------------------------

(use-package vertico
  :ensure nil
  :init
  (vertico-mode))

;; ----------------------------------------------------------------------------
;; Consult: Consulting completing-read
;; - https://github.com/minad/consult
;; ----------------------------------------------------------------------------

(use-package consult
  :ensure nil
  :bind (
         ;; Buffer commands
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

         ;; Navigation
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         )
  )

;; ----------------------------------------------------------------------------
;; Embark: Emacs Mini-Buffer Actions Rooted in Keymaps
;; - https://github.com/oantolin/embark
;; ----------------------------------------------------------------------------

(use-package embark
  :ensure nil

  :bind
  (("C-c v" . embark-act)         ;; pick some comfortable binding
   ("C-c d" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure nil ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ----------------------------------------------------------------------------
;; Marginalia: Margin comments for minibuffer completions.
;; - https://github.com/minad/marginalia
;; ----------------------------------------------------------------------------

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure nil
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  :init
  (marginalia-mode))

;; ----------------------------------------------------------------------------
;; Corfu: Code completion
;; - https://github.com/minad/corfu
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

;; Enable alternative completion frame rendering in terminals.
(unless (display-graphic-p)
  (corfu-terminal-mode +1))

;; ----------------------------------------------------------------------------
;; Orderless: Completion style
;; - https://github.com/oantolin/orderless
;; ----------------------------------------------------------------------------

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
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error))

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
  (setq-default eglot-workspace-configuration
                '((:gopls . (:usePlaceholders t))))
  ;; The pylsp language server discover pytest fixtures.
  ;; The pyright language server has better type checking.
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp")))

  ;; Eglot major mode activation
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'ocaml-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'zig-mode-hook 'eglot-ensure)
  ;; format on save
  (add-hook 'go-mode-hook #'(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (add-hook 'c-mode-common-hook #'(lambda()
                                    (progn
                                      ;; Format buffer on save according to LSP server configuration.
                                      (add-hook 'before-save-hook 'eglot-format-buffer nil t)
                                      ;; Don't let the LSP server format each line on return keypress.
                                      (setq-local eglot-ignored-server-capabilities
                                                  '(:documentOnTypeFormattingProvider)))))
  (define-key eglot-mode-map (kbd "C-c e a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e i") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c e t") 'eglot-find-typeDefinition))

;; Example: Custom clangd language server configuration
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;             '((c-mode c++-mode)
;;              . ("clangd-18"
;;                 "--query-driver=/cross/compiler/root/**" ;; These compilers are accepted from compile_commands.json
;;                 "-j=8"
;;                 "--log=error" ;; error|verbose
;;                 "--background-index"
;;                 "--clang-tidy"
;;                 "--completion-style=detailed"
;;                 "--header-insertion=never"
;;                 "--header-insertion-decorators=0"))))

;; ----------------------------------------------------------------------------
;; Shell scripts
;; ----------------------------------------------------------------------------

(add-hook 'sh-mode-hook 'flycheck-mode)

;; ----------------------------------------------------------------------------
;; C programming language (common C/C++)
;; ----------------------------------------------------------------------------

(require 'ppindent)

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
;; LISP
;; ----------------------------------------------------------------------------

(use-package paredit
  :hook
  (emacs-lisp-mode                  . paredit-mode) ; Elisp buffers.
  (lisp-mode                        . paredit-mode) ; Common Lisp buffers.
  (lisp-interaction-mode            . paredit-mode) ; Scratch buffers.
  (ielm-mode-hook                   . paredit-mode) ; ELM buffers.
  (eval-expression-minibuffer-setup . paredit-mode) ; Eval minibuffers.
  )

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun my/override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'my/override-slime-repl-bindings-with-paredit)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

(use-package slime
  :ensure nil
  :config
  (setq inferior-lisp-program "sbcl"))

;; Workaround for paredit breaking RETURN for minibuffer commands, e.g. "M-x :".
(defun my/paredit-minibuffer-fix ()
  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline))

 (add-hook 'paredit-mode-hook 'my/paredit-minibuffer-fix)

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
;; Emacs client/server mode
;; ----------------------------------------------------------------------------

;; Start Emacs server unless already running.
(load "server")
(unless (server-running-p) (server-start))

;; ----------------------------------------------------------------------------
;; Customized Variables
;; ----------------------------------------------------------------------------

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
