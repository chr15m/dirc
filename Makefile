ICONS=bars cog comment
COLOR=\#b8b8b8

all: public/fonts/fonts.css $(foreach i,$(ICONS),public/icons/$(i).svg)

public/icons/%.svg:
	./node_modules/.bin/font-awesome-svg-png --svg --color "$(COLOR)" --dest public/icons --icons $(basename $(notdir $@))
	mv public/icons/$(COLOR)/svg/$(basename $(notdir $@)).svg public/icons/
	rm -rf public/icons/$(COLOR)

public/fonts/fonts.css: ./node_modules/.bin/goofoffline
	cd public && goofoffline "http://fonts.googleapis.com/css?family=Cutive+Mono"

./node_modules/.bin/goofoffline:
	npm install google-fonts-offline

./node_modules/.bin/font-awesome-svg-png:
	npm install font-awesome-svg-png

