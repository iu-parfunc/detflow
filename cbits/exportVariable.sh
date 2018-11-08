# Do not execute this script. Instead do source!
LIB="/lib/:/lib64/"
USR="/usr/"
#PROC="/proc/"
DEV="/dev/"
PROC="/proc/"
TMP="/tmp/"
# For debuggin only! Allows all permissions.
ALL="/"
# Get directory of this script. It is expected to be ihn the same location as replaceRand.txt.
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export DETIO_URAND_PATH=$DIR/replaceRand.txt
export DETIO_R_PERMS_LIST=$DEV":"$TMP":"$DIR":"$PROC":"$USR":"$LIB":"$ALL
export DETIO_W_PERMS_LIST=$DEV":"$TMP":"$DIR":"$PROC":"$USR":"$LIB":"$ALL
export DEBUG="5"
