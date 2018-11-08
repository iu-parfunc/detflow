# Could make these "src" and "build":
IN=in
OUT=out

# Run all files by default.  Set to an integer to restrict the program to fewer.
HOWMANY=

SRCDIR=standard-RAxML-8.2.10
CODEFILE=v8.2.10.tar.gz
CODEURL=https://github.com/stamatak/standard-RAxML/archive/$(CODEFILE)

DATAFILE=codeml_dS_fa.tgz
DATAURL=http://cs.indiana.edu/~rrnewton/datasets/$(DATAFILE)

DETFLOW_PERMS_FLAGS= -o .
EXTRA_DELETES=$(SRCDIR)
include ../Makefile.common

# WHICH=AVX
WHICH=SSE3


# Build the raxml CLI executable.
build: bin/raxml
bin/raxml: $(CODEFILE)
	tar xf $(CODEFILE)
	mkdir -p ./bin
	cd $(SRCDIR)/ && \
          make -f Makefile.$(WHICH).gcc && \
          mv raxmlHPC-$(WHICH) ../bin/raxml

# Override this rule
in_expected: $(DATAFILE)
	mkdir -p $@
	cd $@ && tar xf ../$(DATAFILE)
