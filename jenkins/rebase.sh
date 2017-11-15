#!/bin/bash -e

#--------------------------------------------------
# rebase current rcs-yocto commit NB! It is assumed all required yocto
# commits are available locally! I. e. fetch is *not* performed by the
# script.
#
# First, components are rebased if needed and the new versions pushed
# (unless dry run). Then a plain "git rebase" of rcs-yocto is
# tried. If it fails, the script checks if the conflict involves
# SRCREV. If so, we're dealing with the common situation in which
# another commit has modified the same SRCREV that the current HEAD
# modifies. The script resolves it by using the SRCREV of the current
# patch and by editing PV. Only rcs repos get this handling (not crl).
#
# Usage: rebase.sh [-c commit-hash] [branch]
#        -c commit-hash - commit to rebase upon (default: HEAD of branch)
#        -n             - no action: don't push rebased components
#        branch         - upstream branch to rebase onto (default: master)
#
#--------------------------------------------------

trap cleanup SIGHUP SIGINT SIGTERM EXIT

cleanup() {
  if [ $? -ne 0 ]; then
    cd $start_dir
    if [ -z "$dry_run" ]; then
      echo "Cleaning up (rebase --abort)"
      git rebase --abort
    fi
  fi
}

printUsage() {
  echo "Usage: $0 [-c commit-hash] [branch]"
}

dry_run=""
commit=""
while getopts "hnc:" opt; do
  case $opt in
    c) commit=$OPTARG;;
    n) echo "Dry-run, will not push rebased components"; dry_run="--dry-run";;
    *) printUsage; exit 2;;
  esac
done
shift `expr $OPTIND - 1`

if [ $# -gt 1 ]; then
  printUsage
  exit 2
elif [ $# -eq 1 ]; then
  branch=$1
else
  branch=master
fi

if [ -z "$commit" ]; then
  newbase=origin/$branch
else
  newbase=$commit
fi

# Find and source some utility functions
functions_file=`dirname $0`/functions
if [ ! -r $functions_file ]; then
  echo "Couldn't find $functions_file"
  exit 1
fi
. $functions_file

start_dir=$PWD


# Start by rebasing (possibly) any components touched by this commit
# Prepare a workspace for rebasing components
mkdir -p components
error_repos=""
changed_repos=""
# Clean up in case we're re-using a workspace
rm -rf components/*
for component in `changed_components`; do
  echo "This commit touches $component, checking whether we need to rebase it"
  component_uri=`repo2uri $component`
  if [ -z "$component_uri" ]; then
    echo "ERROR: Couldn't find upstream URI for $component"
    exit 1
  fi
  # Some recipes contain more than one SRCREV, 'get_srcrev' will return
  # all in a list like this: "SRCREV=123...abc SRCREV_cpm1=999...012"
  srcrev_this=""
  srcrev_prev=""
  for srcrev_str in `get_srcrev $component`; do
    # Save SRCREV from previous round (happens only if we have more
    # than one SRCREV in this repo)
    srcrev_prev=$srcrev_this
    # Get the sha only
    srcrev_this=`echo $srcrev_str | sed 's/.*=//'`
    # Get the SRCREV name ('SRCREV_cpm1', 'SRCREV', ...)
    srcrev_name=`echo $srcrev_str | sed 's/=.*//'`
    # Don't treat the same SRCREV twice (this depends on the order in
    # which cpm1 and cpm2 is taken, not entirely safe?)
    if [ "$srcrev_prev" = "$srcrev_this" ]; then
      echo "Aha! SRCREV $srcrev_this is already taken care of - skip"
      # But make sure we update both SRCREVs in the recipe after rebase
      if [ -f components/$component/newsha ]; then
	# But only if we actually *did* a rebase for the previous SRCREV
	sed 's/^.*=/'${srcrev_name}=/ components/$component/newsha >> components/$component/newsha
      fi
      continue
    fi
    echo "Getting patch info for $component_uri,  $srcrev_this"
    patch_info=`get_patch_info $srcrev_this`
    if [ -z "$patch_info" ]; then
      echo "ERROR: Couldn't get patch info for $srcrev_this"
      exit 1
    fi
    eval $patch_info
    echo "  refspec = $comp_ref, target branch = $comp_branch"
    echo "   open = $comp_open, status = $comp_status, parent = $parent"
    # Is the change already MERGED?
    if [ "$comp_status" = "MERGED" ]; then
      echo "$comp_ref of $component is already merged into $comp_branch"
      # Go to next iteration in the loop
      continue
    fi
    if [ "$comp_open" = "false" ]; then
      echo "ERROR: $comp_ref of $component is closed and not merged"
      error_repos="$error_repos $component"
      # Go to next iteration in the loop
    fi
    uri=`repo2uri $component`
    mysha=`git rev-parse HEAD`
    echo "Checking out rcs-yocto $newbase to get its opinion about sha of $component"
    git checkout -q $newbase
    # The variable name 'upstream_head' is a bit of a misnomer, because
    # it might be a not yet merged commit (in case we're doing a
    # speculative rebase).
    # Find SRCREV in upstream head. The actual key is in
    # ${srcrev_name} (might be e. g. SRCREV_cpm1). Bail out
    # if we don't find the same type of SRCREV in upstream
    # (SRCREV_cpm1, plain SRCREV etc).
    upstream_head_str=`get_srcrev $component | grep ${srcrev_name}= || true`
    if [ -z "$upstream_head_str" ]; then
      echo "Found the following SRCREV entries in upstream ($newbase):"
      get_srcrev $component
      echo "  ... but not '$srcrev_name' which is used in the current patch ($mysha)"
      error_repos="$error_repos $component"
      echo "(Restoring rcs-yocto to $mysha)"
      git checkout -q $mysha
      continue
    fi
    # Get the sha only
    upstream_head=`echo $upstream_head_str | sed 's/.*=//'`
    # Get the SRCREV name ('SRCREV_cpm1', 'SRCREV', ...)
    upstream_head_name=`echo $upstream_head_str | sed 's/=.*//'`
    echo "(Restoring rcs-yocto to $mysha)"
    git checkout -q $mysha
    echo "Parent-to-be yocto says $component is at $upstream_head"
    echo "My $component has $parent for parent"
    if [ "$parent" = "$upstream_head" ]; then
      echo "No need to rebase $component"
    elif [ `echo $parent | tr -cd [0-9a-f,] | wc -c` -eq 81 ]; then
      # In a merge commit, $parent will look like "sha1,sha2"
      echo "Whoops, this looks like a merge commit"
      if ! echo $parent | grep -q $upstream_head; then
	echo "None of the parents equals upstream head ($upstream_head)"
	echo "Sorry, can't handle that. Be sure to base $component on the current HEAD."
	exit 1
      else
	echo "Good - one of the parents is the current upstream HEAD, no need to fuzz"
      fi
    else
      echo "Need to rebase $component"
      #*** Changing current dir! ***
      pushd components > /dev/null
      if [ ! -d $component -o `find . -maxdepth 1 -name $component -empty | wc -l` -ne 0 ]; then
	echo "Cloning $component"
	git clone -q $component_uri
      else
	echo "$component already cloned"
      fi
      #------------
      cd $component
      #------------
      # Adjust URI for push (we can't push to the mirror)
      git remote set-url --push origin ssh://$comp_host_push:$comp_port/$comp_top/$component
      echo "Fetching $component $comp_ref"
      git fetch origin $comp_ref
      srcrev_fetch=`git rev-parse FETCH_HEAD`
      if [ "$srcrev_fetch" != "$srcrev_this" ]; then
	echo "ERROR: sha of current patch set of $comp_ref is $srcrev_fetch,"
	echo "       but expected $srcrev_this"
	exit 1
      fi
      git checkout $srcrev_this
      # First, check if there already is a rebased version (speculative flow!)
      change_id=`git --no-pager log -n 1 | awk '/^ +Change-Id:/ {print $2;}'`
      newsha=""
      if [ -z "$change_id" ]; then
	echo "Failed to get change ID of $component, $srcrev_this"
	# Let's just hope for the best :-(
	echo "... so can't check if there is already a rebase available, continuing anyway"
      else
	newsha=`is_rebase_available $change_id $upstream_head`
      fi
      if [ -n "$newsha" ]; then
	echo "Excellent - there is already a rebase of $change_id"
	echo "   on $upstream_head: $newsha"
	echo "   fetch it and check out"
	reused_patch_info=`get_patch_info $newsha`
	if [ -z "$reused_patch_info" ]; then
	  echo "ERROR: Couldn't get patch info for $newsha"
	  exit 1
	fi
	eval $reused_patch_info
	echo "Fetching $comp_ref"
	if ! git fetch -q origin $comp_ref; then
	  echo "Failed to fetch $comp_ref"
	  exit 1
	fi
	if ! git checkout $newsha; then
	  echo "Failed to checkout $newsha"
	  exit 1
	fi
	echo -n ${srcrev_name}= > newsha
	git rev-parse HEAD >> newsha
	touch already_pushed
	changed_repos="$changed_repos $component"
      else
	# 'upstream_head' (which we want to rebase this component on)
	# might not be merged (speculative, remember?), we may need to fetch
	# it from refs/changes
	if ! git cat-file -t $upstream_head > /dev/null 2>&1; then
	  echo "$upstream_head not in local repo, find refspec and fetch"
	  rebase_patch_info=`get_patch_info $upstream_head`
	  if [ -z "$rebase_patch_info" ]; then
	    echo "ERROR: Couldn't get patch info for $upstream_head"
	    exit 1
	  fi
	  eval $rebase_patch_info
	  echo "  refspec = $comp_ref"
	  git fetch origin $comp_ref
	  # Restore patch info values
	  eval $patch_info
	fi
	echo "Rebasing $component onto $upstream_head"
	if git rebase $upstream_head; then
	  echo "Rebase of $component succeeded"
	  echo -n ${srcrev_name}= > newsha
	  git rev-parse HEAD >> newsha
	  srcrev_new=`git rev-parse HEAD`
	  if [ $srcrev_new != $srcrev_this ]; then
	    echo "New sha after rebase is $srcrev_new"
	    echo "Doing test push of rebased $component to drafts/$comp_branch"
	    git push --dry-run origin HEAD:refs/drafts/$comp_branch || \
	      error_repos="$error_repos $component"
	    # Save which branch this component uses (needed for the 'real'
	    # push outside of this for loop)
	    echo "comp_branch=$comp_branch" > orig-branch
	    changed_repos="$changed_repos $component"
	  else
	    # This can happen if the component is advanced more than one commit
	    echo "Rebase was actually not necessary, didn't change sha of $component"
	  fi
	else
	  echo "Rebase of $component failed"
	  error_repos="$error_repos $component"
	fi
      fi
      popd > /dev/null
      echo
    fi
    #*** Restored current dir! ***
  done
done

if [ -n "$error_repos" ]; then
  echo "ERROR: the following repo(s) had problems:"
  echo "$error_repos" | tr -s " " "\n" | sed 's/^/    /'
  exit 1
fi

# Update SRCREV for rebased components
for i in $changed_repos; do
  newsha=`cat components/$i/newsha`
  # 'newsha' may be a list ("SRCREV_cpm1=... SRCREV_cpm2=...")
  for j in $newsha; do
    if ! set_srcrev $j $i; then
      echo "ERROR: failed to set SRCREV for $i ($j)"
      exit 1
    fi
  done
  echo "$i is rebased, so update SRCREV in its recipe"
done
if git --no-pager status --porcelain | grep -q '^.M'; then
  echo "Amending rcs-yocto commit because of rebased component(s)"
  git add -u
  git commit --amend --no-edit
  echo "New HEAD in rcs-yocto is `git rev-parse HEAD`"
fi

echo
echo "Rebasing rcs-yocto onto $newbase"
sha_original=`git rev-parse HEAD`
# First, try trivial rebase
if ! git rebase $newbase; then
  # Didn't work. Well, we can fix if somebody making it before us
  # through the delivery flow has tampered with the same SRCREV that
  # this patch modifies.
  echo "Trying to resolve rebase conflict automatically"

  # Loop through files with rebase conflict
  for i in `git --no-pager status --porcelain | awk '/^UU/ {print $2;}'`; do
    # We do 'cd' in various places below, so get back to starting point
    #------------
    cd $start_dir
    #------------
    echo "Conflict in $i, checking whether it involves SRCREV"
    # Try to find two SRCREV from between the conflict markers, example:
    # <<<<<<< HEAD
    # SRCREV = "43196d849ea489fe485e210cdf1679a0168084ae"
    # =======
    # SRCREV = "d849ea48431965e210cdf1679a0168084ae9fe48"
    # >>>>>>> This patch
    srcrev_head_str=`sed -n '/^<<<</,/^====/s/^SRCREV\(_[^ =]\{0,\}\)\{0,1\} *= *"\([0-9a-f]\{40,40\}\)".*/SRCREV\1=\2/p' $i`
    srcrev_this_str=`sed -n '/^====/,/^>>>>/s/^SRCREV\(_[^ =]\{0,\}\)\{0,1\} *= *"\([0-9a-f]\{40,40\}\)".*/SRCREV\1=\2/p' $i`
    if [ -n "$srcrev_head_str" -a -n "$srcrev_this_str" ]; then
      if [ `echo "$srcrev_head_str" | wc -w` -gt `echo "$srcrev_this_str" | wc -w` ]; then
	echo "Fewer SRCREV entries in the patch than in upstream head"
	if ! grep -i "#.*intentionally removing.*srcrev" $i; then
	  echo "Can't say if that's intended, so cowardly bailing out"
	  exit 1
	fi
      fi
      # Between the conflict markers, keep our SRCREV only
      perl -ni -e 'BEGIN {$print=1;} $print=0 if /^<<<<<<< HEAD/; \
       print if $print and not /^>>>>>>>/; $print=1 if /^=======/;' $i
      # ... and update it (possibly new because of rebase)
      for j in $srcrev_this_str; do
	srcrev_this=`echo $j | sed 's/.*=//'`
	srcrev_this_name=`echo $j | sed 's/=.*//'`
	echo "Updating $srcrev_this_name with value $srcrev_this in $i"
	sed -i /^$srcrev_this_name/'s/"[0-9a-f]\{40,40\}"/"'$srcrev_this'"/' $i
      done
      # NB! We assume PV is in the same file as SRCREV!
      # Handling of major part of PV not implemented, bail out if changed
      pv1=`getpv_major $i`
      pv2=`git cat-file -p $newbase:$i | getpv_major`
      if [ "$pv1" != "$pv2" ]; then
	echo "ERROR: Major part of PV differs between the conflicting versions of $i"
	echo "       in current patch, PV (major) is $pv1"
	echo "       in version to rebase upon, PV (major) is $pv2"
	echo "Can't deal with that :-("
	exit 1
      fi
      echo "Fixing PV"
      pv1=`getpv_minor $i`
      echo "PV (minor) in $i is $pv1"
      pv2=`git cat-file -p $newbase:$i | getpv_minor`
      echo "PV (minor) in version to rebase upon is $pv2"
      if ! echo "$pv1 $pv2" | egrep -q '[0-9]+ [0-9]+'; then
	echo "ERROR: Trouble finding PV (minor)in $i:"
	echo "       None, multiple or non-numerical"
	exit 1
      fi
      if [ $pv1 -le $pv2 ]; then
	# A greater or equal PV compared to ours exists, so create a new greatest
	newpv=`expr $pv2 + 1`
      else
	# My number *is* greatest, it'll do
	newpv=$pv1
      fi
      echo "Updating  PV (minor) in $i to $newpv"
      setpv_minor $newpv $i
      git add $i
    fi
  done
fi

# All conflicts gone?
conflicts=`git --no-pager status --porcelain | awk '/^UU/ {print $2;}'`
if [ `echo $conflicts | wc -w` -ne 0 ]; then
  echo "Failed to resolve rebase conflicts for the following file(s):"
  echo "$conflicts" | tr -s " " "\n" | sed 's/^/    /'
  exit 1
fi
if [ $sha_original = `git rev-parse HEAD` ]; then
  echo "rcs-yocto was up to date (no rebase done)"
else
  echo "Rebase succeeded"
fi
if [ -d .git/rebase-apply ]; then
  git rebase --continue
fi
# Everything OK, push rebased components
for i in $changed_repos; do
  #*** Changing current dir! ***
  #-------------------------
  cd $start_dir/components/$i
  #-------------------------
  if [ -f already_pushed ]; then
    echo "Not pushing $i, it is already available in gerrit"
  else
    echo "Pushing rebased draft of $i"
    . orig-branch
    push_cmd="git push $dry_run origin HEAD:refs/drafts/$comp_branch"
    echo "$push_cmd"
    #------------------------------
    $push_cmd
    #------------------------------
  fi
  echo
done
exit 0

