(require 'ox-publish)

;; Export options:
;; - https://orgmode.org/manual/Export-Settings.html
;; - https://orgmode.org/manual/Publishing-options.html

(setq org-export-with-toc nil)
(setq org-export-with-section-numbers 0)
(setq org-export-headline-levels 4)
(setq org-export-with-author nil)
(setq org-export-time-stamp-file nil)
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-date t)
(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://bitmaster.se/css/org-mini.css\" />")
(setq org-html-metadata-timestamp-format "%Y-%m-%d")
(setq org-html-validation-link nil)

(setq org-publish-project-alist
      '(("emacs.d"
	 :auto-sitemap nil
         :publishing-function org-html-publish-to-html
         :recursive nil
         :base-directory "./"
         :base-extension "org"
	 :exclude "^plan"
         :publishing-directory "html/")
	("emacs.d-docs"
	 :auto-sitemap nil
         :publishing-function org-html-publish-to-html
         :recursive nil
         :base-directory "docs/"
         :base-extension "org"
         :publishing-directory "html/docs")
        ("all" :components ("emacs.d" "emacs.d-docs"))))
