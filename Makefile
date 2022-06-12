IDRIS2	= idris2

DAT_FILES	= $(wildcard data/*.dat) $(wildcard data/disks/*.dat)

all: build/files.js
	$(IDRIS2) --build kastely.ipkg

build/files.js: $(DAT_FILES)
	mkdir -p build
	@(echo "let files = {"; \
	$(foreach file, $(DAT_FILES), \
		printf "\t'%s': base64ToArrayBuffer('" $(file) ; \
		base64 $(file) | sed -e 's/$$/\\/' ; \
		printf "'),\n" ; \
		) \
	echo "};"; \
	) > $@
