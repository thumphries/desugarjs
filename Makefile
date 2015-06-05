# Good lord, what have I done?
SANDBOX := .cabal-sandbox
JSOUT   := $(SANDBOX)/bin/desugar.jsexe/all.js
GZIP    = zopfli -c -i1000

ifndef $(DESTDIR)
	DESTDIR := ./_site
endif

# IN
HS_SRC := src/Main.hs $(wildcard src/*/*.hs)
STATIC := $(wildcard static/*)

# OUT
JSMIN := $(DESTDIR)/desugar.js
STATIC_OUT := $(DESTDIR)/index.html

all: $(JSMIN).gz $(STATIC_OUT)

$(SANDBOX):
	cabal sandbox init

$(JSOUT): $(HS_SRC) | $(SANDBOX)
	cabal install

$(DESTDIR):
	mkdir -p $(DESTDIR)

$(JSMIN): $(JSOUT) | $(SANDBOX) $(DESTDIR)
	ccjs $(JSOUT) > $@

$(JSMIN).gz: $(JSMIN) | $(SANDBOX)
	$(GZIP) $(JSMIN) > $@

$(STATIC_OUT): $(STATIC) | $(DESTDIR)
	cp -rf static/* $(DESTDIR)

clean:
	-rm -rf $(DESTDIR)
