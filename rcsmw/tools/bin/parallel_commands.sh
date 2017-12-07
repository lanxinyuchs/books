#!/usr/bin/env bash

set -euo pipefail

## WORKSPACE MUST be defined at invocation!
if [[ ! ${WORKSPACE-} && ! -d ${WORKSPACE-} ]]; then
    echo "WORKSPACE must be set to an existing directory before invocation"
    exit 1
fi

#source ${MODULESHOME:-/app/modules/0}/init/bash

# Print basic usage information
#

print_usage () {
    echo "Usage: $(basename $0) { --help | -h }"
    echo "       $(basename $0) [ -f ]"
    echo "       $(basename $0) [ -d ]"
    echo "       $(basename $0) [ -k ]"
    echo "Options:"
    echo "  -h:    show this help"
    echo "  -e:    end execution per row when command returns exit != 0"
    echo "         This means that using -e gives no error code when failure"
    echo "  -f:    A file containing commands, the will be run in background per row"
    echo "  -d:    enable debug printouts"
    echo "  -k:    keep the output files, default is to write them to stdout, and remove" 
}

# Declared variables

file=''
keep=FALSE
end=''

# Print arguments to stderr and exit with a non-zero value
# param: text to print
die() {
    echo "$@" 1>&2
    exit 1
}

# Parse options
#

handle_options () {

    # Program options
    GETOPT=$(getopt --options 'def:hks' --long file: -n $0 -- "$@")

    if [[ $? != 0 ]] ; then echo "Error parsing arguments." >&2 ; exit 1 ; fi

    eval set -- "$GETOPT"

    while true ; do
        case "$1" in
          -d)                    set -x;;
          -e)                  end=TRUE;;
          -f|--file) shift;     file=$1;;
          -h)        print_usage; exit ;;
          -k)                 keep=TRUE;;
          --)        shift;      break ;;
          *) echo "Internal error!" ; exit 1 ;;
        esac
        shift
    done

    # $@ is local since we are in a function. Copy it to the global variable.
    args="$@"
}


# Main program.

pidlist=''
main () {

    if [[ ! ${file-} || ! -e ${file} ]]; then
        echo "An existing file must be specified"
        exit 2
    fi

    outdir=$( mktemp -d --tmpdir=${WORKSPACE} )
    echo "outpurdir with logfiles per row: $outdir"
    i=1
    while read line ; do
        echo "line $i = $line"
        if [[ $end != TRUE ]]; then
            set +e
        fi
        ( echo "--Start of line $i --------" >> ${outdir}/${i}      ; \
          eval "$line"                       >> ${outdir}/${i} 2>&1 ; \
          echo "$?"                          >> ${outdir}/${i}      ; \
          echo "----End of line $i---------" >> ${outdir}/${i}          ) &
        pidlist="$pidlist $!"
        set -e
        let i++
    done < "$file"

    wait $pidlist
    cat ${outdir}/*
    if [[ $keep == FALSE ]]; then
        rm -rf ${outdir}
    fi
}


handle_options "$@"
main $args

