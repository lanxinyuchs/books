#!/usr/bin/env bash

set -euo pipefail

SHASH=$(git rev-parse --short HEAD)
REV=${VERSION-$SHASH}
HASH=$(git rev-parse HEAD)
DATE=$(date +%FT%T)


MW_CXP=VRCS-MW_CXP9029176_4
DUMMY_CXP=VRCS-DUMMY_CXP9029207_4
EE_CXP=VRCS-EE_CXP9029177_4
UP_CXP=VRCS-UP_CXS101657_4
up_xml=cxs101657_4-up.xml
EE_REV=$(echo $EE_CXP_LABEL |cut -d- -f2)
GROUP=com/ericsson

# Args: UP version, MW version, DUMMY version, EE version, metadata file
function update_meta()
{
  up_version=$1; shift
  mw_version=$1; shift
  dummy_version=$1; shift
  ee_version=$1; shift
  sed -i "s/__DATE__/$DATE/" $*
  sed -i "s/__REV__/$up_version/" $*
  sed -i "s/__VSN__/$up_version/" $*
  sed -i "s/__COMMIT__/$HASH/" $*
  sed -i "s/__MW_VSN__/$mw_version/" $*
  sed -i "s/__DUMMY_VSN__/$dummy_version/" $*
  sed -i "s/__EE_REV__/$ee_version/" $*
}


ARM_URL=${ARM_URL-https://arm001-eiffel002.rnd.ki.sw.ericsson.se:8443/nexus/content/repositories/releases}

cd $TOPDIR/out

if [[ ${MW_CXP_VERSION-} && ${DUMMY_CXP_VERSION-} && ${EE_CXP_VERSION-} ]]; then
  # Clean all cxp artifacts
  rm -rf *.cxp
  # Download from Nexus
  cp ${up_xml}_tmpl $up_xml
  echo wget -q $ARM_URL/$GROUP/$MW_CXP/${MW_CXP_VERSION}/$MW_CXP-${MW_CXP_VERSION}.cxp
  wget -q $ARM_URL/$GROUP/$MW_CXP/${MW_CXP_VERSION}/$MW_CXP-${MW_CXP_VERSION}.cxp
  echo wget -q $ARM_URL/$GROUP/$EE_CXP/${EE_CXP_VERSION}/$EE_CXP-${EE_CXP_VERSION}.cxp
  wget -q $ARM_URL/$GROUP/$EE_CXP/${EE_CXP_VERSION}/$EE_CXP-${EE_CXP_VERSION}.cxp
  echo wget -q $ARM_URL/$GROUP/$DUMMY_CXP/${DUMMY_CXP_VERSION}/$DUMMY_CXP-${DUMMY_CXP_VERSION}.cxp
  wget -q $ARM_URL/$GROUP/$DUMMY_CXP/${DUMMY_CXP_VERSION}/$DUMMY_CXP-${DUMMY_CXP_VERSION}.cxp
  echo Updating metadata file cxs*.xml
  update_meta $VERSION ${MW_CXP_VERSION} ${DUMMY_CXP_VERSION} $EE_CXP_VERSION $up_xml
  echo tar cvfz $UP_CXP-$VERSION.cxs $DUMMY_CXP-${DUMMY_CXP_VERSION}.cxp $EE_CXP-${EE_CXP_VERSION}.cxp $MW_CXP-${MW_CXP_VERSION}.cxp $up_xml
  tar cvfz $UP_CXP-$VERSION.cxs $DUMMY_CXP-${DUMMY_CXP_VERSION}.cxp $EE_CXP-${EE_CXP_VERSION}.cxp $MW_CXP-${MW_CXP_VERSION}.cxp $up_xml
else
  # create the bootable VM image.
  /app/rcs-ee/vrcs/tools/rcs-ee_make_vm.sh -U . -i -o vrcs_installed_$REV.qcow2
fi

