;; Example customization of org-mode file open actions:
;;
;; '(org-file-apps
;;   '((auto-mode . emacs)
;;     (directory . emacs)
;;     ("\\.mm\\'" . default)
;;     ("\\.x?html?\\'" . system)
;;     ("\\.pdf\\'" . "okular %s")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bookmark-bmenu-file-column 60)
 '(cmake-tab-width 4)
 '(denote-directory "~/notes/")
 '(denote-excluded-files-regexp "/attachments/")
 '(marginalia-field-width 160)
 '(org-agenda-files '("~/notes/plan.org"))
 '(org-default-notes-file "~/org/notes.org")
 '(org-id-link-to-org-use-id 'use-existing)
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
 '(org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
 '(org-startup-folded 'nofold)
 '(plantuml-default-exec-mode 'jar)
 '(plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
 '(plantuml-server-url "")
 '(split-height-threshold 72)
 '(split-width-threshold 180)
 '(xref-search-program 'ripgrep))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
