eval "export PROJECT=G2"

export EE2_ID=CXP2030010_1
export EET_ID=CXP2030011_1
export EE3_ID=CXP2020231_1

if [[ ${EE_REV_FORCE-} ]]; then
   if [[ ${ARTIFACT_GAVS-} ]]; then

       TMP_ID=$(echo $ARTIFACT_GAVS | python -c 'import json,sys; obj=json.load(sys.stdin);print obj[0]["artifactId"]')

       case $TMP_ID in
         $EE2_ID)
            export RCS_DUS2_REV=${EE_REV_FORCE}
            export RCS_DUS2_URL=$($TOPDIR/tools/bin/get_url_for_product_rev.sh $EE2_ID $RCS_DUS2_REV)


            ##### export RCS_T_REV=$($TOPDIR/tools/bin/get_latest_cxp.sh $EET_ID 0)
            export RCS_T_REV=$RCS_DUS2_REV
            export RCS_T_URL=$($TOPDIR/tools/bin/get_url_for_product_rev.sh $EET_ID $RCS_T_REV)

            ##### export RCS_DUS3_REV=$($TOPDIR/tools/bin/get_latest_cxp.sh $EE3_ID 0)
            export RCS_DUS3_REV=$RCS_DUS2_REV
            export RCS_DUS3_URL=$($TOPDIR/tools/bin/get_url_for_product_rev.sh $EE3_ID $RCS_DUS3_REV)
            ;;

         $EET_ID)
            export RCS_T_REV=${EE_REV_FORCE}
            export RCS_T_URL=$($TOPDIR/tools/bin/get_url_for_product_rev.sh $EET_ID $RCS_T_REV)
            ;;

         $EE3_ID)
            export RCS_DUS3_REV=${EE_REV_FORCE}
            export RCS_DUS3_URL=$($TOPDIR/tools/bin/get_url_for_product_rev.sh $EE3_ID $RCS_DUS3_REV)
            ;;

         *)
            echo "Cannot match EE_ID=$TMP_ID with any official product"
            exit 1
       esac
   fi
else
    # This is executed when we're triggered by SCE event.
    # In that case, pick latest CL1 of all EE products.
    # 2017-02-24: New order, pick CL0 / erahent

    export RCS_DUS2_REV=$($TOPDIR/tools/bin/get_latest_cxp.sh $EE2_ID 0)
    export RCS_DUS2_URL=$($TOPDIR/tools/bin/get_url_for_product_rev.sh $EE2_ID $RCS_DUS2_REV)
    export RCS_T_REV=$($TOPDIR/tools/bin/get_latest_cxp.sh $EET_ID 0)
    export RCS_T_URL=$($TOPDIR/tools/bin/get_url_for_product_rev.sh $EET_ID $RCS_T_REV)
    export RCS_DUS3_REV=$($TOPDIR/tools/bin/get_latest_cxp.sh $EE3_ID 0)
    export RCS_DUS3_URL=$($TOPDIR/tools/bin/get_url_for_product_rev.sh $EE3_ID $RCS_DUS3_REV)
fi

export TAIPAN_ID=CXP102172_2
export TAIPAN_REV=$($TOPDIR/tools/bin/get_latest_cxp.sh $TAIPAN_ID 3)
export TAIPAN_URL=$($TOPDIR/tools/bin/get_url_for_product_rev.sh $TAIPAN_ID $TAIPAN_REV)

export KATLA_ID=CXP102185_1
export KATLA_REV=$($TOPDIR/tools/bin/get_latest_cxp.sh $KATLA_ID 3)
export KATLA_URL=$($TOPDIR/tools/bin/get_url_for_product_rev.sh $KATLA_ID $KATLA_REV)

export COBRA_ID=CXP102171_1
export COBRA_REV=$($TOPDIR/tools/bin/get_latest_cxp.sh $COBRA_ID 3)
export COBRA_URL=$($TOPDIR/tools/bin/get_url_for_product_rev.sh $COBRA_ID $COBRA_REV)

export TIGER_ID=CXP102194_1
export TIGER_REV=$($TOPDIR/tools/bin/get_latest_cxp.sh $TIGER_ID 3)
export TIGER_URL=$($TOPDIR/tools/bin/get_url_for_product_rev.sh $TIGER_ID $TIGER_REV)

export MTIGER_ID=CXP102201_1
export MTIGER_REV=$($TOPDIR/tools/bin/get_latest_cxp.sh $MTIGER_ID 3)
export MTIGER_URL=$($TOPDIR/tools/bin/get_url_for_product_rev.sh $MTIGER_ID $MTIGER_REV)
