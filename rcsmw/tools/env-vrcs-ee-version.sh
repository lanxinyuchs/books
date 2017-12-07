EE_ID=CXP9029177_4
eval "export PROJECT=5G"
export EE_REV=${EE_REV_FORCE:-$($TOPDIR/tools/bin/get_latest_cxp.sh $EE_ID 0)}
export EE_URL=${EE_URL_FORCE:-$($TOPDIR/tools/bin/get_url_for_product_rev.sh $EE_ID $EE_REV)}
export EE_CXP_LABEL=$EE_ID-$EE_REV
export EE_CXA_LABEL=CXA11448_4-R1A01
eval "export ${EE_CXP_LABEL%-*}_VERSION=${EE_CXP_LABEL##*-}"
eval "export ${EE_CXA_LABEL%-*}_VERSION=${EE_CXA_LABEL##*-}"
