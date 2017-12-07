%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_db.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_db).
-vsn('/main/R1A/R4A/1').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2015 All rights reserved.
%%% 
%%% The information in this document is the property of Ericsson.
%%% 
%%% Except as specifically authorized in writing by Ericsson, the 
%%% receiver of this document shall keep the information contained 
%%% herein confidential and shall protect the same in whole or in 
%%% part from disclosure and dissemination to third parties.
%%% 
%%% Disclosure and disseminations to the receivers employees shall 
%%% only be made on a strict need to know basis.
%%% %CCaseCopyrightEnd%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%% ----    ---------- -------  ------------------------------------------------
%% R4A/1   2015-07-07 etxberb  Changed mnesia:create_table to
%%                             clhI:mnesia_create_table.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([
	 init_tables/1, init_data/0
	]).
-export([
	 get_fingerprint/0,
	 set_fingerprint/1
	]
       ).

-export([create_option_feature/1,
	 update_option_feature/1,
	 delete_option_feature/1,
	 get_feature_state/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("Licensing.hrl").
-include("lih_lihi.hrl").

-define(licensing_LicensingId_default, {"1", "1", "1"}).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-spec init_tables(DbNodes::[node()]) -> 'ok'.
init_tables(DbNodes) ->
    Tables = [{licensing, ?licensing_types},
	      {capacityFeature, ?capacityFeature_types},
	      {optionalFeature, ?optionalFeature_types}],
    [create_table(Name, DbNodes, Types) || {Name, Types} <- Tables],
    ok.

-spec init_data() -> 'ok'.
init_data() ->
    %% Create System Created objects
    LicObj =
	#licensing{'LicensingId'      = ?licensing_LicensingId_default,
		   userLabel          = ?licensing_userLabel_default,
		   licenseFileUrl     = ?licensing_licenseFileUrl_default,
		   fingerprint        = ?licensing_fingerprint_default,
		   licenseFileUrlIpv6 = ?licensing_licenseFileUrlIpv6_default},
    ok = mnesia:dirty_write(LicObj),

    ok = lih_fm:register().

-spec get_fingerprint() -> FingerPrint::string().
get_fingerprint() ->
    [#licensing{fingerprint = FingerPrint}] =
	mnesia:dirty_read(licensing, ?licensing_LicensingId_default),

    FingerPrint.

-spec set_fingerprint(Fingerprint::string()) ->
			     'ok' | {'error', Reason::string()}.
set_fingerprint(Fingerprint) ->
    case lih_lici_server:is_LKF_installed() of
	true ->
	    {error, "LKF is installed"};
	false ->
	    [Obj] = mnesia:dirty_read(licensing,
				      ?licensing_LicensingId_default),
	    NewObj = Obj#licensing{fingerprint = Fingerprint},
	    ok = mnesia:dirty_write(NewObj)
    end.

-spec create_option_feature({_,_,_,_,_}) -> boolean().
create_option_feature({KeyId, Rdn, FeatureState, LicenseState, ServiceState}) ->
    FeatureId = ?FEATURE_ID(Rdn),

    case mnesia:dirty_read(optionalFeature, FeatureId) of
	[#optionalFeature{keyId = KeyId}] ->
	    true;
	[] ->
	    Obj = #optionalFeature{'FeatureId'  = FeatureId,
				   featureState = convert(feature, FeatureState),
				   licenseState = convert(license, LicenseState),
				   serviceState = convert(service, ServiceState),
				   keyId        = KeyId,
				   featureInstanceRef = ""},
	    ok = mnesia:dirty_write(Obj),
	    true;
	_ ->
	    false
    end.

-spec update_option_feature({_,_,_,_,_}) -> 'ok'.
update_option_feature({KeyId, Rdn, FeatureState, LicenseState, ServiceState}) ->
    FeatureId = ?FEATURE_ID(Rdn),
    Pattern = #optionalFeature{'FeatureId' = FeatureId,
			       keyId       = KeyId,
			       _           = '_'},
    case mnesia:dirty_match_object(Pattern) of
	[] ->
	    ok;
	[Obj] ->
	    NewObj =
		Obj#optionalFeature{featureState = convert(feature, FeatureState),
				    licenseState = convert(license, LicenseState),
				    serviceState = convert(service, ServiceState)},
	    ok = mnesia:dirty_write(NewObj)
    end.

-spec delete_option_feature(_) -> 'ok'.
delete_option_feature(Rdn) ->
    FeatureId = ?FEATURE_ID(Rdn),
    ok = mnesia:dirty_delete(optionalFeature, FeatureId).

-spec get_feature_state({_,_}) -> 'activated' | 'deactivated'.
get_feature_state({KeyId, Rdn}) ->
    FeatureId = ?FEATURE_ID(Rdn),
    Pattern = #optionalFeature{'FeatureId' = FeatureId,
			       keyId       = KeyId,
			       _           = '_'},
    case mnesia:dirty_match_object(Pattern) of
	[]    -> deactivated;
	[Obj] -> convert(feature, Obj#optionalFeature.featureState)
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

create_table(Name, DbNodes, Types) ->
    Fields = [Field || {Field, _} <- Types],
    {atomic, ok} =
	clhI:mnesia_create_table(Name, [{type, set},
					{disc_copies, DbNodes},
					{attributes, Fields} |
					add_clh_option(Name)]),
    ok.

%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].


convert(feature, activated)                           ->
    ?FeatureActivationState_ACTIVATED;
convert(feature, deactivated)                         ->
    ?FeatureActivationState_DEACTIVATED;
convert(feature, ?FeatureActivationState_ACTIVATED)   -> activated;
convert(feature, ?FeatureActivationState_DEACTIVATED) -> deactivated;

convert(license, enabled)  -> ?LicenseState_ENABLED;
convert(license, disabled) -> ?LicenseState_DISABLED;

convert(service, ?CELLO_LIHI_SERVICE_OPERABLE)   -> ?ServiceState_OPERABLE;
convert(service, ?CELLO_LIHI_SERVICE_INOPERABLE) -> ?ServiceState_INOPERABLE.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
