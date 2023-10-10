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
 '(cmake-tab-width 4)
 '(denote-directory "~/notes/")
 '(denote-dired-directories
   '("~/notes/" "~/notes/journal"))
 '(marginalia-field-width 160)
 '(org-agenda-files '("~/org/plan.org"))
 '(org-default-notes-file "~/org/notes.org")
 '(org-id-link-to-org-use-id 'use-existing)
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
 '(org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
 '(plantuml-default-exec-mode 'jar)
 '(plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
 '(plantuml-server-url ""))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
