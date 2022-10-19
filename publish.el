(require 'ox-publish)

(setq org-publish-project-alist
      '(("emacs.d"
	 :auto-sitemap nil
	 :html-metadata-timestamp-format "%Y-%m-%d %H:%M"
	 :html-validation-link nil
         :publishing-function org-html-publish-to-html
         :recursive nil
         :base-directory "./"
         :base-extension "org"
	 :exclude "^plan"
         :publishing-directory "html/")
	("emacs.d-docs"
	 :auto-sitemap nil
	 :html-metadata-timestamp-format "%Y-%m-%d %H:%M"
	 :html-validation-link nil
         :publishing-function org-html-publish-to-html
         :recursive nil
         :base-directory "docs/"
         :base-extension "org"
         :publishing-directory "html/docs")
        ("all" :components ("emacs.d" "emacs.d-docs"))))
