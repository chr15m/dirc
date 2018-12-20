ICONS=comment times-circle circle-o-notch
COLOR=\#b8b8b8

assets: public/fonts/fonts.css $(foreach i,$(ICONS),public/icons/$(i).svg) public/icons/favicon.png

public/icons/%.svg: ./node_modules/.bin/font-awesome-svg-png
	./node_modules/.bin/font-awesome-svg-png --svg --color "$(COLOR)" --dest public/icons --icons $(basename $(notdir $@))
	mv public/icons/$(COLOR)/svg/$(basename $(notdir $@)).svg public/icons/
	rm -rf public/icons/$(COLOR)

public/icons/favicon.png: ./node_modules/.bin/font-awesome-svg-png
	./node_modules/.bin/font-awesome-svg-png --png --color "#1BE3C6" --dest public/icons --icons comments-o --sizes=512
	mv "public/icons/#1BE3C6/png/512/comments-o.png" public/icons/favicon.png
	rm -rf "public/icons/#1BE3C6"

public/fonts/fonts.css: ./node_modules/.bin/goofoffline
	cd public && ../node_modules/.bin/goofoffline "http://fonts.googleapis.com/css?family=Cutive+Mono"

./node_modules/.bin/goofoffline:
	npm install google-fonts-offline

./node_modules/.bin/font-awesome-svg-png:
	npm install font-awesome-svg-png

STATICFILES=$(shell find public/ -type f -o -type l | grep -v public/js)
STATICDEST=$(foreach f,$(STATICFILES),$(subst public,build,$(f)))

build: $(STATICDEST) build/js/app.js

build/%: public/%
	mkdir -p $(@D)
	cp -avL $< $@

build/js/app.js: src/**/** project.clj
	lein clean
	lein package

.PHONY: clean

clean:
	lein clean
	rm -rf build public/fonts public/icons
