VARIANT=${1:-vrcs}

cd $TOPDIR
ERLC_FLAGS_EXTRA=+debug_info source env-$VARIANT.sh ${VARIANT}_erldebug
waf
dialyzer --plt $OTP/dialyzer_plt $(find $TOPDIR/$BLDDIR -name \*.beam -not -name \*_eunit.beam) --output dialyzer.log
rm -rf $BLDDIR
