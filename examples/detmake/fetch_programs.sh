#!/bin/bash

## Fetch code.  MUST remain idempotent!

set -xe

WGET="wget --progress=dot:giga --no-clobber  --no-check-certificate"

(cd 02_readline && make get)

# TODO: Check SHAs
if ! [ -e 03_raxml ]; then
    $WGET https://github.com/stamatak/standard-RAxML/archive/v8.2.10.tar.gz
    tar xf v8.2.10.tar.gz
    mv standard-RAxML-8.2.10 03_raxml
    cp patches/raxml_modified_Makefile 03_raxml/Makefile
fi

# Exhibiting the deadlock-during-rules-preprocessing:
if ! [ -e 04_aspell ]; then
    $WGET ftp://ftp.gnu.org/gnu/aspell/aspell-0.60.6.1.tar.gz
    tar xf aspell-0.60.6.1.tar.gz
    mv aspell-0.60.6.1 04_aspell
fi


## PARSEC 3.0 / Splash 2
# ================================================================================
# These have some fishy ambiguious wildcard rules.  Commenting those
# out because they don't seem to be needed.
#
# On a four core desktop:
#   raytrace - 0.5 sec build, 3X speedup
#   ocean_cp - 1 sec build, 2X speedup
#   cholesky - 0.6s / 3X
# fft - too small

if ! [ -e 06_splash ]; then
#    git clone git@github.com:iu-parfunc/splash_hacks.git 06_splash
    SPLASH_HACKS_VER=0.2
    $WGET https://github.com/iu-parfunc/splash_hacks/archive/${SPLASH_HACKS_VER}.tar.gz
    tar xf ${SPLASH_HACKS_VER}.tar.gz
    mv splash_hacks-${SPLASH_HACKS_VER} 06_splash
fi



# Currently have problems:
# ================================================================================

# [2017.04.05] Ncurses uses very weird makefile syntax:
# ftp://ftp.gnu.org/gnu/ncurses/ncurses-6.0.tar.gz


# Exhibiting the deadlock-during-rules-preprocessing:
# http://ftp.gnu.org/gnu/diffutils/diffutils-3.5.tar.xz



