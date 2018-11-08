
CODENAME=hmmer-3.1b2-linux-intel-x86_64
CODEFILE=$(CODENAME).tar.gz
CODEURL=http://eddylab.org/software/hmmer3/3.1b2/$(CODEFILE)

# TODO: could find a way to share this download with ../raxml
# DATAFILE=codeml_dS_fa.tgz
# DATAURL=http://cs.indiana.edu/~rrnewton/datasets/$(DATAFILE)

# We must start execution with /dev/shm/bwactl nonexistent.
EXTRA_DELETES = /dev/shm/bwactl

include ../Makefile.common

build: bin/hmmbuild
bin/hmmbuild: $(CODEFILE)
	tar xf $(CODEFILE)
	rm -rf ./bin
	mkdir bin
	mv $(CODENAME)/binaries/* ./bin/
	rm -rf ./$(CODENAME)
	touch bin/hmmbuild

# Override this rule
in_expected:
	(cd ../raxml && $(MAKE) in_expected)
	cp -a ../raxml/in_expected .
