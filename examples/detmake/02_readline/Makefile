# Could make these "src" and "build":
SRC=in
BUILD=out
WGET=wget --progress=dot:giga --no-clobber  --no-check-certificate


# Build readline using make or detmake.
all: get plain

configure: $(SRC) $(BUILD) $(SRC)/Makefile

$(SRC)/Makefile:
	cd $(SRC) && ./configure --prefix=$(PWD)/$(BUILD)

# Build with plain GNU make.
plain: configure
	cd $(SRC) && make -j
	cd $(SRC) && make install
	$(MAKE) hash

detmake: configure
	cd $(SRC) && stack exec detmake
#	cd $(SRC) && stack exec -- detmake install
#	$(MAKE) hash

rebuild:
	cd $(SRC) && make clean
	$(MAKE) plain

$(BUILD):
	mkdir -p $(BUILD)/

hash:
	../../../bin/hashdir.sh out/
	../../../bin/dirsize.sh out/

get: $(SRC)

$(SRC): readline-readline-7.0.tar.gz
	tar -xf readline-readline-7.0.tar.gz
	mv readline-readline-7.0 $(SRC)

readline-readline-7.0.tar.gz:
	$(WGET) http://git.savannah.gnu.org/cgit/readline.git/snapshot/readline-readline-7.0.tar.gz -O $@
	md5sum readline-readline-7.0.tar.gz | tee download.md5sum

clean:
	rm -rf $(SRC) $(OUT)

distclean: clean
	rm -rf readline-readline-7.0.tar.gz readline-readline-7.0.tar.gz download.md5sum

.PHONY: clean all get plain hash distclean rebuild detmake
