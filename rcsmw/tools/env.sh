# Note: this file should only contain a minimal amount of settings to have
# "sane" access to the tools in the "tools" part of the repository. It must not
# rely on that any other settings file have been sourced.

# Most tools require the TOPDIR environment variabke to be set. This must
# overwrite any existing TOPDIR variable.
export TOPDIR="$(readlink -f $(dirname ${BASH_SOURCE[0]})/..)"

# Expose the bin directory to the shell
export PATH=${PATH}:${TOPDIR}/tools/bin

# Remove duplicate PATH entries
export PATH=$(echo -n $PATH | awk -v RS=: -v ORS=: '!arr[$0]++' | sed -e 's/:\+$//')

#
# CICDSYS configuration
#

if [[ ! ${CICDSYS_ROOT-} ]]; then
    root="$(readlink -f $(dirname ${BASH_SOURCE[0]}))"

    # CICDSYS_ROOT
    # The directory where this file resides, and all other cicdsys functions
    export CICDSYS_ROOT=$root
fi

# CICDSYS_OUTDIR
#
# Location of the OUTDIR is in the repo root directory. This is
# where CICDSYS expected built products to be located, and is where downloaded
# products will be store, e.g. for running SBC tests etc.

git_root=$(cd $CICDSYS_ROOT && git rev-parse --show-toplevel)
export CICDSYS_OUTDIR=$git_root/out

# Support for artifact-specific configuration files

if [[ ${ARTIFACT_NAME:-} ]]; then
    if [[ -e ${CICDSYS_ROOT}/config/${ARTIFACT_NAME}-delivery.config ]]; then
        export CICDSYS_DEPLOY_CONFIG_FILE=${CICDSYS_ROOT}/config/${ARTIFACT_NAME}-delivery.config
    fi
    if [[ -e ${CICDSYS_OUTDIR}/${ARTIFACT_NAME}-baseline.config ]]; then
        export CICDSYS_BASELINE_CONFIG_FILE=${CICDSYS_OUTDIR}/${ARTIFACT_NAME}-baseline.config
    fi
fi
export CICDSYS_DEPLOY_CONFIG_FILE=${CICDSYS_DEPLOY_CONFIG_FILE:-${CICDSYS_ROOT}/config/delivery.config}
export CICDSYS_BASELINE_CONFIG_FILE=${CICDSYS_BASELINE_CONFIG_FILE:-${CICDSYS_OUTDIR}/baseline.config}

# Export the config file variables to the build
cicd-set CICDSYS_DEPLOY_CONFIG_FILE ${CICDSYS_DEPLOY_CONFIG_FILE}
cicd-set CICDSYS_BASELINE_CONFIG_FILE ${CICDSYS_BASELINE_CONFIG_FILE}
