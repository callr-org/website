CALLR_WEBSITE_PATH=callr.org:public_html/callr.org

all: pages site/assets

pages: site/index.html site/install.html site/render.html site/rsource.html site/eval.html site/use.html site/debug.html

site/assets: assets/* site/assets/ico/favicon.png
	rsync -av assets/ site/assets/

site/assets/ico/favicon.png: templates/assets/ico/favicon.R
	Rscript templates/assets/ico/favicon.R

site/%.html: content/%.md.rsp content/incl/* templates/*.rsp
	@echo "Building $(*F)"
	Rscript -e R.rsp::rfile "templates/{{page}}.html.rsp" --page=$(*F)
	mv -f $(@F) $@
	if test -f $(*F); then mv $(*F) site/$*; fi

beta:
	rsync -avvz --perms --chmod=ugo+rx --progress site/ $(CALLR_WEBSITE_PATH)/beta/
deploy:
	rsync -avvz --perms --chmod=ugo+rx --progress site/ $(CALLR_WEBSITE_PATH)/
