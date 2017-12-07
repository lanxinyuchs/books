#/usr/bin/env bash
# ===================================================
# Remove duplicated new components from PATH variable
# This makes it possible to source same file without
# letting the PATH variable to grow.
# ===================================================
# First source the file
. $1
# Let awk remove duplicates and let sed remove any trailing :
export PATH=$(echo -n $PATH | awk -v RS=: -v ORS=: '!arr[$0]++' | sed -e 's/:\+$//')