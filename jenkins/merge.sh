#!/bin/bash

#--------------------------------------------------
# Merge rcs-yocto and updated rcs components into remote. Intended
# to be used by jenkins. Starting directory: top of rcs-yocto.
#
# The patch to be merged should be at HEAD and the branch to push to
# should have a fresh state (just fetched).
#
# Usage: merge [-n] <branch>
#        -n     - dry run
#        branch - branch to merge into
#
#--------------------------------------------------

#----------------------------------------
# Various hardcodings etc
#----------------------------------------
start_dir=$PWD
error_repos=""

# Find and source some utility functions
functions_file=`dirname $0`/functions
if [ ! -r $functions_file ]; then
  echo "Couldn't find $functions_file"
  exit 1
fi
. $functions_file

#----------------------------------------
# Local functions
#----------------------------------------
printUsage() {
  echo "Usage: $0 <branch>"
}

# Check that a change ('commit') is based on upstream HEAD ('branch')
# usage: check_parent <commit> <branch>
check_parent() {
  local commit
  local branch
  local base
  local orig_head
  commit=$1
  branch=$2
  orig_head=`git rev-parse origin/$2`
  base=`git merge-base $commit $orig_head`
  if [ "$base" != "$orig_head" ]; then
    echo "ERROR: $commit is not based on $branch HEAD"
    echo "       origin/$branch HEAD = $orig_head"
    echo "       patch parent  = `git rev-parse ${commit}~1`"
    return 1
  else
    return 0
  fi
}

#----------------------------------------
# Option parsing, initial checks
#----------------------------------------
dry_run=""
debug=0
while getopts "dnh" opt; do
  case $opt in
    d) debug=1;;
    n) dry_run=--dry-run;;
    *) printUsage; exit 2;;
  esac
done
shift `expr $OPTIND - 1`

if [ $# -ne 1 ]; then
  printUsage
  exit 2
fi
if [ -n "$dry_run" ]; then
  echo "Dry run, nothing will be pushed"
fi

#----------------------------------------
# Start working
#----------------------------------------
[ $debug -eq 1 ] && set -x
branch=$1
head=`git rev-parse HEAD`
# Create a root where we can clone components
mkdir -p components
rm -rf components/*

# Start by checking rcs-yocto: is this patch based on upstream HEAD?
echo "Checking rcs-yocto"
if ! check_parent $head $branch; then
  exit 1
fi
# Test push rcs-yocto
push_cmd="git push -q --dry-run origin HEAD:refs/heads/$branch"
echo "$push_cmd"
$push_cmd
if [ $? -ne 0 ]; then
  echo "ERROR: Test push of rcs-yocto failed"
  exit 1
else
  echo "Test push of rcs-yocto OK"
fi

# Work on changed components
changed_repos=`changed_components`
already_merged=""
for component in $changed_repos; do
  echo
  echo "This commit involves $component"
  set -e
  # Some recipes contain more than one SRCREV, 'get_srcrev' will return
  # all in a list like this: "SRCREV=123...abc SRCREV_cpm1=999...012"
  for srcrev_str in `get_srcrev $component`; do
    # Get the sha only
    srcrev_this=`echo $srcrev_str | sed 's/.*=//'`
    # Get the SRCREV name ('SRCREV_cpm1', 'SRCREV', ...)
    srcrev_name=`echo $srcrev_str | sed 's/=.*//'`
    echo "Getting patch info for $component, $srcrev_this"
    patch_info=`get_patch_info $srcrev_this`
    if [ -z "$patch_info" ]; then
      echo "ERROR: Couldn't get patch info for $srcrev_this"
      error_repos="$error_repos $component"
      continue
    fi
    eval $patch_info
    echo "  refspec = $comp_ref, target branch = $comp_branch"
    echo "  open = $comp_open, status = $comp_status, parent = $parent"
    # Is the change already MERGED?
    if [ "$comp_status" = "MERGED" ]; then
      echo "$comp_ref of $component is already merged into $comp_branch"
      already_merged="$already_merged $component"
      # Go to next iteration in the loop
      continue
    fi
    if [ "$comp_open" = "false" ]; then
      echo "ERROR: $comp_ref of $component is closed and not merged"
      error_repos="$error_repos $component"
      # Go to next iteration in the loop
      continue
    fi
    component_uri=`repo2uri $component`
    if [ -z "$component_uri" ]; then
      echo "ERROR: Couldn't find upstream URI for $component"
      error_repos="$error_repos $component"
      continue
    fi
    if [ ! -d components/$component/.git ]; then
      echo "Cloning $component"
      git clone -q $component_uri components/$component
    else
      echo "$component already cloned"
    fi
    #-----------------------
    cd components/$component
    #-----------------------
    # Adjust URI for push (we can't push to the mirror)
    git remote set-url --push origin ssh://$comp_host_push:$comp_port/$comp_top/$component
    echo "Fetching $comp_ref"
    git fetch origin $comp_ref
    git checkout $srcrev_this
    # Check that our parent equals branch HEAD upstream
    if ! check_parent $srcrev_this $comp_branch; then
      error_repos="$error_repos $component"
    else
      # Save which branch this component uses (needed for the 'real'
      # push outside of this for loop). Note that we may accumulate
      # lines in 'comp-data' if the repo has changed multiple branches.
      echo "comp_branch=$comp_branch;comp_rev=$srcrev_this" >> comp-data
      echo "Doing a test push of $component"
      set +e
      push_cmd="git push -q --dry-run origin $srcrev_this:refs/heads/$comp_branch"
      echo "$push_cmd"
      $push_cmd
      if [ $? -ne 0 ]; then
	echo "ERROR: test push of $component failed"
	error_repos="$error_repos $component"
      else
	echo "Test push OK"
	echo
      fi
    fi
    #------------
    cd $start_dir
    #------------
  done
done
#------------
cd $start_dir
#------------

if [ -n "$error_repos" ]; then
  echo "ERROR: the following repo(s) had problems:"
  echo "$error_repos" | sed 's/ /\n    /g'
  exit 1
fi

if [ -z "$changed_repos" ]; then
  echo "This patch doesn't touch any rcs component"
fi

# Everything looks good, push
set -e
for i in $changed_repos; do
  if echo "$already_merged" | tr " " "\n" | grep -q ^$component\$; then
    echo "Not pushing $component, is already merged"
    continue
  fi
  #--------------------------
  cd $start_dir/components/$i
  #--------------------------
  # A component may have had more than one branch modified, so loop
  # through 'comp-data'
  for j in `sort -u comp-data`; do
    eval $j
    push_cmd="git push $dry_run origin $comp_rev:refs/heads/$comp_branch"
    echo "$push_cmd"
    #------------------------------
    $push_cmd
    #------------------------------
    echo
  done
done
#------------
cd $start_dir
#------------
push_cmd="git push $dry_run origin HEAD:refs/heads/$branch"
echo "$push_cmd"
#------------------------------
$push_cmd
#------------------------------

