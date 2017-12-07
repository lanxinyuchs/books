# Source baseline fo EE for RCS MW
# Check if we have "set -u" first and save state
if shopt -oq nounset; then
    NOUNSET=1
fi
set +u
export CCACHE_PATH=""
source /proj/5G_rcs/3pp/brcs-ee/CXA11448_5_R7A39/env.sh
if [[ "${NOUNSET-}" ]]; then
    set -u
fi
unset NOUNSET

# restore working python. TODO: fix Yocoto SDK-host python install. 
unset PYTHONHOME
source ${MODULESHOME-/app/modules/0}/init/bash
module remove python
module add python/2.7.6
source $(dirname "${BASH_SOURCE[0]}")/env-brcs-ee-version.sh
