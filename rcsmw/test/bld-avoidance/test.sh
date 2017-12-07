#!/usr/bin/env bash

#set -x

# The top dir for build avoidance tests
testdirtop="$TOPDIR/test/bld-avoidance"

# Run tests from repo top
cd $TOPDIR

# File which we modify, example: test/FAKE/esrc/fakeDataInit.erl
file=$1

# Make sure that file isn't changed first time
git checkout -- $file

# Do a reference build with unmodified file
/usr/bin/time -p -o build.time bash -c "waf; ./tools/package.sh"

# List resulting files with timestamps and md5 sums from reference build
ls --full-time out | sort >a.time
md5sum out/* | sort >a.md5

# Modify source file
# check if patch file exists for the actual file
# if it does then apply the patch
# add a blank line to the file otherwise
# and notify the user that id did the default action
filename=$(basename $file)
patchfile=$testdirtop/patch/$filename.patch
if [[ -e $patchfile ]]; then
    patch -p1 < $patchfile
else
    echo "Note: No patch file for $file exist, adding a blank line only"
    echo "" >>$file
fi

# Rebuild with changed file
/usr/bin/time -p -o rebuild.time bash -c "waf; ./tools/package.sh"

# List resulting files with timestamps and md5 sums from rebuild
ls --full-time out | sort >b.time
md5sum out/* | sort >b.md5

# Remove modifications from source file
git checkout -- $file

# Display build times
echo
echo "======== Reference build time ======================="
cat build.time
echo "======== Rebuild time ======================"
cat rebuild.time

# Display modified result files (and also recreated files from timestamps)
comm -1 -3 a.time b.time | awk '{print $9}'| sort >regenerated.txt
comm -1 -3 a.md5 b.md5 | cut -d' ' -f3 | sort | sed 's,out/,,' >modified.txt
echo "========= Modified artifacts ======================"
cat modified.txt
echo "========= Regenerated (but not modified) artifacts ======================"
comm -2 -3 regenerated.txt modified.txt
