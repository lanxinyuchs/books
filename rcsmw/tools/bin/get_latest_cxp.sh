P=$1
CL=$2
FLOW=${3:-DURA/$PROJECT/main}
$TOPDIR/tools/bin/eventrepo_query.py\
   -q "?eventType=EiffelConfidenceLevelModifiedEvent&eventData.gav.artifactId=$P&eventData.confidenceLevels.$CL=SUCCESS&eventData.flowContext=$FLOW"\
   -i 0\
   eventData gav version
