#!/bin/sh

if [ "$THREADSETTINGS" == "" ]; then
#  THREADSETTINGS="{1..16} {24..24} {32..32}";
  THREADSETTINGS="1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16   24 32";
fi

function run_make_something() {
    START=$(date +%s%N)
    DEBUG=0 stack $STACK_ARGS $DOCKERARGS exec -- $@
    END=$(date +%s%N)
    DURATION=`echo $((END-START)) | awk '{printf "%f\n",$0/1000000000}'`
    echo "SELFTIMED $DURATION"
}

# Arguments for run_make_* functions:
# $1: THREADS
# $2: make target

# NB: Doesn't work yet, sadly
function run_make_nondet() {
    run_make_something detmake -j$1 --nondet $2
}

function run_make_det_ld_preload() {
    run_make_something detmake -j$1 $2
}

function run_make_gnumake() {
    run_make_something make -j$1 $2
}

# function run_make_det_ptrace() {
#     # run_make_something ???
# }

# $1: PROGNAME
# $2: where the program is located
# $3: VARIANT
# $4: make target
function hsbencher_make() {
    set -xe
    progname=$1
    progdir=$2
    variant=$3
    # This may be empty:
    the_target=$4

    if [ -z "$variant" ]; then
       echo "hsbencher_make: variant argument must be provided!"; exit 1; fi
    
    TRIALS=${TRIALS:-30}
    for threads in $THREADSETTINGS ; 
    do
        echo "START_BENCHMARK"
        echo "PROGNAME: $progname"
        echo "VARIANT: $variant"
        echo "THREADS: $threads"
	echo "TRIALS: $TRIALS"
        echo "CI_BUILD_ID: " ${BUILD_TAG}
        for ix in `seq 1 ${TRIALS}` ;
        do
            cd $top/examples/detmake/$progdir
            make clean # Hacky way to get back to a "starting state"
            run_make_$variant $threads $the_target
        done
        echo "END_BENCHMARK"
    done
}

# $1: PROGNAME
# $2: where the program is located
# $3: VARIANT
function hsbencher_bioinfo() {
    set -xe
    progname=$1
    progdir=$2
    variant=$3

    case "$variant" in
        "nondet")
            detflow_variant="--nondet";;
        "det_ld_preload")
            detflow_variant="--det";;
        "rr_record")
            detflow_variant="--rr-record";;
        "rr_replay")
            detflow_variant="--rr-replay";;
        *)
            echo "Unrecognized variant: \"$variant\"";
            echo "Expected one of: nondet, det_ld_preload, rr_record, rr_replay";
            echo "(Pass it in with SUB_VARIANT)";
            exit 1;;
    esac;

    TRIALS=${TRIALS:-9}
    for threads in $THREADSETTINGS ;
    do	
	set +x
        echo "START_BENCHMARK"
        echo "PROGNAME: $progname"
        echo "VARIANT: $variant"
        echo "ARGS: \"${HOWMANY}\"" 
        echo "THREADS: $threads"
	echo "TRIALS: $TRIALS"
        echo "CI_BUILD_ID: " ${BUILD_TAG}
	set -x
        for ix in `seq 1 ${TRIALS}` ;
        do	    
	    set +x
	    echo; echo "Running trial $ix"
	    echo "----------------------------------"
            cd $top/examples/bioinfo/$progdir
	    set -x
            make clean # Hacky way to get back to a "starting state"
            DETFLOW_THREADS="-j$threads" DETFLOW_VARIANT="$detflow_variant" DETFLOW_OPTIONS="--compile" make HOWMANY=$HOWMANY $TESTMODE
        done
        echo "END_BENCHMARK"
    done
}


# Flaky network things
function retry_it()
{
    for ix in {1..5}; do
        set +e    
        $@
        code=$?
        set -e
        if [ "$code" == 0 ]; then 
            break;
        elif [ $ix == 5 ]; then
            echo "retry_it: failed after several tries";
            exit 1;
        fi
    done
}
