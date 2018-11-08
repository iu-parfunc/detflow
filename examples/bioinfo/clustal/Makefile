
CODENAME=clustalw-2.1
CODEFILE=$(CODENAME).tar.gz
CODEURL=http://www.clustal.org/download/current/$(CODEFILE)

# TODO: could find a way to share this download with ../raxml
DATAFILE=
DATAURL=

include ../Makefile.common

build: bin/clustalw2
bin/clustalw2: $(CODEFILE)
	tar xf $(CODEFILE)
	(cd $(CODENAME) && ./configure)
	(cd $(CODENAME) && make -j8)
	mkdir -p ./bin
	mv -f $(CODENAME)/src/clustalw2 ./bin/
	touch bin/clustalw

# Override this rule
in_expected:
	(cd ../raxml && $(MAKE) in_expected)
	cp -a ../raxml/in_expected .
