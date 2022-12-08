SOURCES := \
	README.org \
	docs/howto-dir-locals.org \
	docs/howto-org-mode.org

TARGETS := \
	css/org-main-optimized.css \
	css/org-main-optimized.css.el \
	html/README.html

.PHONY: docs
docs: $(TARGETS)

css/org-main-optimized.css.el : ELISP_VAR := org-html-head

%.css.el : %.css
	@echo Generating $@
	@rm -f $@
	@echo -n '(setq $(ELISP_VAR) "<style type=\"text/css\">' >> $@
	@sed 's,",\\",g' $< >> $@
	@echo '</style>")' >> $@

%-optimized.css : %.css
	@echo Generating $@
	@yui-compressor $< > $@

# Touch README.org because org-publish is clever about timestamps while
# not understanding all dependencies.
#
# This rule is incorrect but describing multiple input, multiple output
# rules is back-breaking in make.
#
html/README.html: $(SOURCES) org-publish.el css/org-main-optimized.css.el
	@touch $(SOURCES)
	emacs --batch --load org-publish.el --funcall org-publish-all

$(TARGETS): $(MAKEFILE_LIST)
