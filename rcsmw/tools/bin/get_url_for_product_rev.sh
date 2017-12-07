P=$1
R=$2
FLOW=${3:-DURA/$PROJECT/main}
$TOPDIR/tools/bin/eventrepo_query.py\
   -q "?eventType=EiffelArtifactNewEvent&eventData.gav.artifactId=$P&eventData.gav.version=$R&eventData.flowContext=$FLOW"\
   -i 0 eventData optionalParameters packageurl
