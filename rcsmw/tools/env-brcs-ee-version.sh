CCPRX=https://rbs-rde.rnd.ki.sw.ericsson.se
export EE_ID=CXP9029438_5
eval "export PROJECT=5G"
export EE_REV=${EE_REV_FORCE:-$($TOPDIR/tools/bin/get_latest_cxp.sh $EE_ID 0)}
export EE_URL=${EE_URL_FORCE:-$($TOPDIR/tools/bin/get_url_for_product_rev.sh $EE_ID $EE_REV)}

export COBRA_ID=CXP102171_1
export COBRA_REV=R42B01
export COBRA_CXP_LABEL=$COBRA_ID-$COBRA_REV
COBRA_CXP_CC_PATH=/vobs/rcs/delivery/RCP_CSX10179_1/RCP-ALL_CXS101553/COBRA_CXP102171_1/doc/19010/COBRA_CXP102171_1.cxp
export COBRA_URL=$CCPRX/$COBRA_CXP_CC_PATH@@/$COBRA_CXP_LABEL
