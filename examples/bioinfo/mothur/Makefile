
VER=0.0.1
IMGNAME=parfunc/mothur:$(VER)

# must NOT use static version:
# CODEFILE=Mothur.linux_64_static.zip
CODEFILE=Mothur.linux_64.zip
CODEURL=https://github.com/mothur/mothur/releases/download/v1.39.5/$(CODEFILE)

DATAFILE=MiSeqSOPData.zip
# DATAURL=https://www.mothur.org/w/images/d/d6/$(DATAFILE)
DATAURL=http://cs.indiana.edu/~rrnewton/datasets/$(DATAFILE)

DATA_UNPACK_DIR= MiSeq_SOP

# https://www.mothur.org/w/images/9/98/Silva.bacteria.zip
# https://www.mothur.org/w/images/5/59/Trainset9_032012.pds.zip

include ../Makefile.common

docker_img:
	docker build -t $(IMGNAME) .

build: bin/mothur
bin/mothur: $(CODEFILE)
	rm -rf mothur bin __MACOSX
	unzip -o $(CODEFILE)
	mv mothur bin
	rm -rf __MACOSX
	touch bin/mothur
