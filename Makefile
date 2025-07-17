ICONS := org/favicon.ico org/apple-touch-icon.png

.PHONY: all
all: $(ICONS)

$(ICONS):
	convert -size 1024x1024 \
		-undercolor none xc:'hsl(24,10%,5%)' \
		-font FreeSerif-Bold \
		-kerning 0 -pointsize 1000 \
		-fill 'hsl(24,100%,98%)' -annotate +150+860 'A' \
		\( -clone 0 -resize 180x180 -write org/apple-touch-icon.png \) \
		\( -clone 0 -define icon:auto-resize=64,48,32,16 -write org/favicon.ico \) \
		null:

.PHONY: dev dev/postcss dev/server
dev:; $(MAKE) -j2 dev/postcss dev/server
dev/postcss: ; npx postcss *.css -w --dir public/
dev/server:  ; npx live-server public/
