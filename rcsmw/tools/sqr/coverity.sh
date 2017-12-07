VARIANT=${1:-vrcs}

D=$(mktemp -d -t $USER-rcsmw-covXXXX)
mkdir -p $D/cov
source ${MODULESHOME-/app/modules/0}/init/bash
source env-$VARIANT.sh ${VARIANT}_coverity
module add coverity
cov-configure --config=$D/conf.xml --comptype gcc --compiler $(echo $CC |cut -d\  -f1) --template
cov-build --config $D/conf.xml --dir $D/cov waf
cov-analyze --dir $D/cov/
cov-format-errors --html-output $D/output --dir $D/cov/
rm -rf $BLDDIR

echo "Results: $D/output/index.html"
