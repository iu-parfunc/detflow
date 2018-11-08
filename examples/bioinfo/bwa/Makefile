
# Precompiled:
# CODEFILE=bwakit-0.7.15_x64-linux.tar.bz2
# CODEURL=https://github.com/lh3/bwa/releases/download/v0.7.15/$(CODEFILE)

# PATCHED version: remove /dev/shm optimization:
# CODEFILE=bwa-0.7.15-patched.tgz
# CODEURL=https://cs.indiana.edu/~rrnewton/datasets/$(CODEFILE)

# Older versions don't have the same problem.
# VER=0.7.9
VER=0.7.10
# Uses dev/shm:
# VER=0.7.11
# VER=0.7.12
CODEFILE=$(VER).tar.gz
CODEURL=https://github.com/lh3/bwa/archive/$(CODEFILE)

DATAFILE=bwa_sample_input.tar
DATAURL=http://cs.indiana.edu/~rrnewton/datasets/$(DATAFILE)

# We must start execution with /dev/shm/bwactl nonexistent.
EXTRA_DELETES = /dev/shm/bwactl

include ../Makefile.common

build: bin/bwa
bin/bwa: $(CODEFILE)
	tar xf $(CODEFILE)
	rm -rf ./bin
#	mv -f bwa-0.7.15-patched bin
	mv -f bwa-$(VER) bin
	(cd bin && make -j)
