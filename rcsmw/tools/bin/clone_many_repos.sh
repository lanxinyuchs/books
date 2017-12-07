#!/usr/bin/env bash

set -euo pipefail

# Print basic usage information
#

print_usage () {
    echo "Usage: $(basename $0) --repolist <repo;sha1 [,repo;sha1]"
    echo "                           --local_dir  <the directory to perform clone>"
    echo "                          [--sleep_time <in seconds>]"
    echo "                          [--nof_trials_sha1  <a number>]"
    echo "                          [--nof_trials_clone <a number>]"
    echo "                          [--base_sleep   <a number>]"
    echo "                          [--random_sleep <a number>]"
    echo "                          [--nodeCI <a string for nodeCI track, eg. DURA/5G/main>]"
    echo "                          [--wipeout ]"
    echo ""
    echo "Clone and checkout a number of repos"
    echo "Options:"
    echo "  --repolist        at least one repo. Repos ;separated. Each repo must have a sha1 (after a ,)"
    echo "                    repo,sha1 [ ; repo,sha1 [ ; repo,sha1  [more repos allowed]]]"
    echo "                    sha1 can also be a refspec, eg. refs/changes/09/2072109/2"
    echo "  --local_dir       <the directory to perform clone>"
    echo "  --sleep_time      Number of seconds between retrials, if sha1 is not found"
    echo "  --nof_trials_sha1  Number of retrials before giving up"
    echo "  --sleep_clone     Number of seconds between retrials, if any clone fails"
    echo "  --nof_trials_clone  Number of retrials before giving up"
    echo "  --base_sleep      Minimum secondes to wait after any failed clone (or fetch)"
    echo "  --random_sleep    Range (0-random_sleep) to be added to base_sleep"
    echo "  --nodeCI          This option overrules all the others."
    echo "                    Currently only two parameters are supported:"
    echo "                    DURA/G2/main"
    echo "                    DURA/5G/main"
    echo "  --wipeout         start to wipe out local_dir, to guarantee new clone"
    echo ""
}

calculate_branch() {
    case "$1" in
      DURA/G2/main) echo "master";;
      DURA/5G/main) echo "master";;
      DURA/5G/VRAN_R1) echo "vran_r1";;
      DURA/5G/MTR_17.27) echo "vran_r2";;
      *) echo "ERROR"; print_usage; exit 1;;
    esac
}

clone_repos () {

    local __resultvar=$1

    # Don't start to set i to zero. "let i++" gives exitcode when i goes from zero to one!!!
    i=1
    back=$(mktemp -d)
    pidlist=''

    for repo_sha1 in $( echo $repolist | tr ',' ' ' ); do
        clone_addr=$(echo $repo_sha1 | awk -F\; '{print $1}')
        repo=$(basename $clone_addr)

        #### If repo/&.git directory exists, perform only fetch
        #### elsif repo exists , remove old repo and clone
        #### else clone

        if [[ -d $repo/.git ]]; then
            cd $repo

            out="${back}/$i"
            c="git fetch"
            set +e
            ( echo "pwd ; $c" > $out 2>&1; pwd >> $out 2>&1; $c >> $out 2>&1; echo $? >> $out 2>&1 ) &
            set -e
            pidlist="$pidlist $!"
            cd ..
        else
            if [[ -d $repo ]]; then
                temp=$(mktemp -d --tmpdir=.)
                mv $repo $temp

                out="${back}/$i"
                c1="rm -rf $repo"
                c2="rm -rf $temp"
                set +e
                ( echo $c1  > $out 2>&1; $c2  >> $out 2>&1; echo $?  >> $out 2>&1 ) &
                set -e
                pidlist="$pidlist $!"
                let i++
            fi

            out="${back}/$i"
            c="git clone $clone_addr"
            set +e
            ( echo $c  > $out 2>&1; $c  >> $out 2>&1; echo $?  >> $out 2>&1) &
            set -e
            pidlist="$pidlist $!"
        fi
        let i++
    done

    echo "Waiting for $pidlist (performing clone in background processes)"
    wait $pidlist
    failed_commands=0
    for file in `ls -d ${back}/*`; do
        if [[ `tail -1 ${file}` != 0 ]]; then
            failed_commands=1
            echo ""
            echo "----------- Output from failing clone command follows ----------"
            cat $file
        fi
    done

    # Remove the temporary files from background processes
    rm -rf ${back}

    if [[ $failed_commands != 0 ]]; then
        echo ""
        echo "------------- END OF output from failing clone command follows------"
        echo ""
        ## Set return value
        eval $__resultvar=1
    else
        ## Set return value
        eval $__resultvar=0
    fi
}

# Declared variables with default values

args=''
repolist=''
local_dir=''
sleep_time='5'
nof_trials_sha1='200'
nof_trials_clone='3'
base_sleep='50'
random_sleep='20'
wipeout=''


# Parse options
#

handle_options () {

    # Program options
    GETOPT=$(getopt --options 'dh' --long repolist:,sleep_time:,nof_trials_sha1:,nof_trials_clone:,base_sleep:,random_sleep:,local_dir:,nodeCI:,wipeout,help -n $0 -- "$@")
    if [[ $? != 0 ]] ; then echo "Error parsing arguments." >&2 ; exit 1 ; fi

    eval set -- "$GETOPT"

    while true ; do
        case "$1" in
          --repolist)         shift;         repolist=$1;;
          --local_dir)        shift;        local_dir=$1;;
          --sleep_time)       shift;       sleep_time=$1;;
          --nof_trials_sha1)  shift;  nof_trials_sha1=$1;;
          --nof_trials_clone) shift; nof_trials_clone=$1;;
          --base_sleep)       shift;       base_sleep=$1;;
          --random_sleep)     shift;     random_sleep=$1;;
          --wipeout)			       wipeout=1;;
          --nodeCI)     calculate_branch $2;        exit;;
          -d)                         set -x;;
          -h)           print_usage;  exit  ;;
          --help)       print_usage;  exit  ;;
          --)           shift;        break ;;
          *) echo "Internal error!" ; exit 1;;
        esac
        shift
    done

    # repolist and local_dir are compulsury parameters
    if [[ $repolist == ''  ||  $local_dir == '' ]]; then
        echo ""
        print_usage
        echo ""
        echo "Both repolist and local_dir are compulsory to specify"
        echo ""
        exit 1
    fi

    # $@ is local since we are in a function. Copy it to the global variable.
    args="$@"
}


# Main program.

main () {
    ######################################################################################
    # First we must make sure that there are no duplicated repos in --repolist parameter #
    ######################################################################################

   
    temp=$(mktemp)
    for repo_sha1 in $( echo $repolist | tr ',' ' ' ); do
        clone_addr=$(echo $repo_sha1 | awk -F\; '{print $1}')
        echo $clone_addr >> $temp
    done
    if [[ "`uniq -d $temp`" != "" ]]; then
        echo "--repolist parameter has the same repo duplicated:"
        uniq -d $temp
        \rm $temp
        exit 1
    fi
    \rm $temp


    if [[ $wipeout != ''  &&  -d $local_dir ]]; then
        echo "Erasing $local_dir"
        rm -rf $local_dir
    fi

    mkdir -p $local_dir
    cd $local_dir

    #####################################################
    # Perform cloning and retry $nof_trials_clone times #
    #####################################################
    while (( 1 )); do
        clone_repos result
        if [[ $result == 0 ]]; then
            break
        elif [[ $nof_trials_clone == 1 ]]; then
            exit 1
        fi
        let nof_trials_clone--
        local sleep_clone=$[ ( $RANDOM % $random_sleep )  + $base_sleep ]
        echo "sleep $sleep_clone ($nof_trials_clone trials left)"
        sleep $sleep_clone
    done

    #########################################
    # Then perform checkout of correct sha1 #
    #########################################
    for repo_sha1 in $( echo $repolist | tr ',' ' ' ); do
        clone_addr=$(echo $repo_sha1 | awk -F\; '{print $1}')
        sha1=$(      echo $repo_sha1 | awk -F\; '{print $2}')
        repo=$(basename $clone_addr)

        cd $repo
        while (( 1 )); do
            git fetch
            set +e
            if [[ $sha1 == "refs/changes/"* ]]; then
                if git fetch $clone_addr $sha1 && git checkout FETCH_HEAD; then
                    set -e
                    break
                fi
            else
                if git checkout $sha1; then
                    set -e
                    break
                fi
            fi
            set -e
            echo "sleep $sleep_time"
            sleep $sleep_time
            let nof_trials_sha1--
            if (( $nof_trials_sha1 == 0 )); then
                echo "Wrong sha1 in repo $repo"
                exit 1
            fi
        done
        cd ..
    done
}

handle_options "$@"
main $args

