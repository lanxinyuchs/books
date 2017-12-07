%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ootComSysM.erl %
%%% Author:	eolaand
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
%%% @private
-module(ootComSysM).
-vsn('/main/R4A/R5A/R8A/R9A/R10A/R11A/R12A/3').
-date('2017-11-01').
-author('eolaand').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	ootComSysM.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R4A/1      2015-08-25 eolaand     Created
%%% R4A/2      2015-08-28 eolaand     Rename some fcn stubs
%%% R4A/4      2015-09-02 eolaand     Add check of port ranges.
%%% R4A/8      2015-09-10 eolaand     Check busy ports and store changed ECIM 
%%%                                   config in IMM
%%% R4A/9      2015-09-11 eolaand     Temporarily remove store config.
%%% R4A/10     2015-09-18 eolaand     Add store config again.
%%% R4A/11     2015-10-16 eolaand     Add mirror functions to be called from
%%%                                   COMSA, TR HU25873.
%%% R4A/12     2015-10-22 eolaand     Remove old  mirroring functions
%%% R4A/13     2015-10-23 eolaand     Fix mirroring to undefined accessPoint 
%%% R5A/1      2016-01-27 etxjotj     Model name switch
%%% R10A/2     2017-05-23 eolaand     Remove OamAccessPoint for R-VNFM
%%% R10A/3     2017-06-16 eolaand     Notify server about config change
%%%                                   Only in R-VNFM case for now
%%% R11A/1     2017-08-07 eolaand     Remove mirroring of deprecated attributes
%%% R11A/2     2017-08-15 eolaand     Restore mirroring of deprecated attributes
%%% R11A/3     2017-08-17 eolaand     Change error log to warning when setting
%%%                                   depreacated attributes
%%% R12A/1     2017-10-30 eolaand     Allow netconf port 830 for all env except
%%%                                   old rcssim on host.
%%% R12A/2     2017-10-31 eolaand     Remove mirroring of deprecated attributes
%%% R12A/3     2017-11-01 eolaand     Restore mirroring of deprecated attributes
%%%                                   again.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%%----------------------------------------------------------------------
%% API functions
%%----------------------------------------------------------------------
-export([
	 set_updated_config/1,
	 set_oap_attrs/2
	]).

-export([prepareTransaction/2, 
	 finish/2]).

%% -export([store_changed_conf_in_imm/1]).

-export([get_oap_mo_attrs/1,
	 get_oap_mo_attrs/2]).

-export([set_mo_attrs/1]).

-export([get_mirror_set_attributes/1]).

-export([get_ecim_port_conf/0,
	 get_ecim_port_conf/1,
	 get_ecim_dscp/0,
	 get_ecim_config/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%% -export([internal_function1/2]).

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------
-include("RcsSysM.hrl").
-include("oot.hrl").

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(APP, "OOT").

-define(NC_SSH, "NetconfSsh").
-define(CLI_SSH, "CliSsh").
-define(OAM_TR_CLASS, "OamTrafficClass").

%%ERROR: Invalid value '66000' for attribute 'port'. 
%%Valid values are in range: [0,65535]
-define(ERR_INVALID_PORT(N), "Invalid value '" ++ integer_to_list(N) ++
	    "' for attribute 'port'. Valid values are in range: [1024,65535]").
-define(ERR_INVALID_NC_TGT_PORT(N), "Invalid value '" ++ integer_to_list(N) ++
	    "' for attribute 'port'. "
	"Valid values are 830 or in range: [1024,65535]").
-define(ERR_BUSY_PORT(N), "Invalid value '" ++ integer_to_list(N) ++
	    "' for attribute 'port'. Port is busy.").


%%% ----------------------------------------------------------
%%% Records
%%% ----------------------------------------------------------


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%--------------------------------------------------------------------
%% Function: set_updated_config/1
%% Description:
%%--------------------------------------------------------------------
-spec set_updated_config(Config::list()) -> ok.
set_updated_config(Config) ->	    
    ChangedConfig = get_changed_config(Config, get_ecim_config()),
    ModAttrs = ootLib:config_to_ecim_set_mo_attrs(ChangedConfig),
    set_mo_attrs(ModAttrs).
    
%%--------------------------------------------------------------------
%% Function: set_oap_attrs/1
%% Description:
%%--------------------------------------------------------------------
-spec set_oap_attrs(DN::string(), IMMAttrVals::list()) -> ok.
set_oap_attrs(DN, IMMAttrVals) ->	    
    ECIMAttrVals = ootLib:imm_to_ecim_oap_attr_vals(IMMAttrVals),
    set_mo_attrs([{DN, ECIMAttrVals}]).
    
%%--------------------------------------------------------------------
%% Function: get_ecim_dscp/0
%% Description:
%%--------------------------------------------------------------------
-spec get_ecim_dscp() -> DSCP::integer() | undefined.
get_ecim_dscp() ->
    OTC = comsaI:get_oam_traffic_class_config(),
    case proplists:get_value(?CNF_DSCP, OTC) of
	DSCP when is_integer(DSCP) ->
	    DSCP;
	_Error ->
	    undefined
    end.

%%--------------------------------------------------------------------
%% Function: get_ecim_port_conf/0
%% Description:
%%--------------------------------------------------------------------
-spec get_ecim_port_conf() -> PortConfig::list().
get_ecim_port_conf() ->
    [{Type, get_ecim_port_conf(Type)} || Type <- ?SYS_PORTS]. 

%%--------------------------------------------------------------------
%% Function: get_ecim_port_conf/0
%% Description:
%%--------------------------------------------------------------------
-spec get_ecim_port_conf(Type::atom()) -> PortNumber::integer().
get_ecim_port_conf(?SYS_CLI_PORT = Type) ->
    Conf = comsaI:get_cli_ssh_config(),
    get_port_from_conf(Type, Conf);

get_ecim_port_conf(?SYS_NETCONF_PORT = Type) ->
    Conf = comsaI:get_netconf_ssh_config(),
    get_port_from_conf(Type, Conf).

%%--------------------------------------------------------------------
%% Function: get_ecim_config/0
%% Description:
%%--------------------------------------------------------------------
-spec get_ecim_config() -> Config::list().
get_ecim_config() ->
    [{?CNF_DSCP, get_ecim_dscp()} | 
     [{ootLib:port_type_to_conf_type(Type), Port} || 
	 {Type, Port} <- get_ecim_port_conf()]].
    
%%====================================================================
%% Callback functions from COMSA
%%====================================================================
%%--------------------------------------------------------------------
%% Function: prepareTransaction/2
%% Description:
%%--------------------------------------------------------------------
-spec prepareTransaction(Objs::list(), TX::integer()) -> 
				ok | {abort, Reason::string()}.
prepareTransaction(Objs, _Tx) ->
    case validate_objects(Objs) of
	{ok, _ValidConfig} ->
	    ?LOG_INFO("Validation of tx successful, Objects = ~p", [Objs]),
	    ok;
	{error, Reason} = E ->
	    ?LOG_WARNING("Validation of tx failed: ~p~nObjects = ~p", 
			 [E, Objs]),
	    {abort, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: finish/2
%% Description:
%%--------------------------------------------------------------------
-spec finish(Class::string(), Tx::integer()) -> ok.
finish(Class, _Tx) when Class =:= ?CLI_SSH;
			Class =:= ?NC_SSH;
			Class =:= ?OAM_TR_CLASS ->
    %% Temp fix to avoid duplicated updates until deprecated attrs are removed
    case ootLib:is_rvnfm() of
    	false ->
    	    ok;
    	_True ->
	    ?LOG_INFO("Finish ~p tx, notify ootServer about config change", 
		      [Class]),
    	    ootServer:config_update(),
    	    ok
    end;
    %% ?LOG_INFO("Finish ~p tx, notify ootServer about config change", [Class]),
    %% ootServer:config_update();


finish(_Class, _Tx) ->
    ok.


%%--------------------------------------------------------------------
%% Function: get_mirror_set_attributes/1
%% Description:
%%--------------------------------------------------------------------
-spec get_mirror_set_attributes(SetMoAttrs::tuple()) -> list().
get_mirror_set_attributes(SetMoAttrs) ->
    case ootLib:is_rvnfm() of
    	false ->
	    ?LOG_INFO("SetMoAttrs = ~p", [SetMoAttrs]),
    	    MirrorAttrs = do_get_mirror_set_attributes(SetMoAttrs),
	    ?LOG_INFO("MirrorAttrs = ~p", [MirrorAttrs]),
	    MirrorAttrs;
    	_True ->
    	    []
    end.
    %% [].


do_get_mirror_set_attributes({Cmd, TransId, ?ECIM_OAM_TR_CLASS_DN, AttrVals}) ->
    get_mirror_set_oap_attrs(?ECIM_DSCP, ?OAP_DSCP, Cmd, TransId, AttrVals);

do_get_mirror_set_attributes({Cmd, TransId, ?ECIM_NETCONF_SSH_DN, AttrVals}) ->
    get_mirror_set_oap_attrs(?ECIM_PORT, ?OAP_NETCONF_PORT, Cmd, TransId, 
			     AttrVals);

do_get_mirror_set_attributes({Cmd, TransId, ?ECIM_CLI_SSH_DN, AttrVals}) ->
    get_mirror_set_oap_attrs(?ECIM_PORT, ?OAP_SSH_PORT, Cmd, TransId, 
			     AttrVals);

do_get_mirror_set_attributes({Cmd, TransId, DN, AttrVals}) 
  when DN =:= ?ECIM_DN_OAP; DN =:= ?ECIM_DN_ALT_OAP ->
    lists:flatmap(fun({?OAP_DSCP, Val}) when DN =:= ?ECIM_DN_OAP ->
			  log_use_of_obsolete_attr(?OAP_DSCP, Val),
			  AttrVal = [{?ECIM_DSCP, Val}],
			  [{Cmd, TransId, ?ECIM_OAM_TR_CLASS_DN, AttrVal}];
		     ({?OAP_SSH_PORT, Val}) when DN =:= ?ECIM_DN_OAP ->
			  log_use_of_obsolete_attr(?OAP_SSH_PORT, Val),
			  AttrVal = [{?ECIM_PORT, Val}],
			  [{Cmd, TransId, ?ECIM_CLI_SSH_DN, AttrVal}];
		     ({?OAP_NETCONF_PORT, Val}) when DN =:= ?ECIM_DN_OAP ->
			  log_use_of_obsolete_attr(?OAP_NETCONF_PORT, Val),
			  AttrVal = [{?ECIM_PORT, Val}],
			  [{Cmd, TransId, ?ECIM_NETCONF_SSH_DN, AttrVal}];
		     ({?OAP_IPV4_ADDR, Val}) ->
			  log_use_of_obsolete_attr(?OAP_IPV4_ADDR, Val),
			  get_mirror_set_ipv4_addr(Cmd, TransId, DN, Val);
		     ({?OAP_ACC_POINT, Val}) ->
			  get_mirror_set_acc_point(Cmd, TransId, DN, Val);
		     (_) ->
			  []
		  end, AttrVals);

do_get_mirror_set_attributes(_) ->
    [].


log_use_of_obsolete_attr(Attr, {_, Val}) ->
    do_log_use_of_obsolete_attr(Attr, Val);

log_use_of_obsolete_attr(Attr, Val) ->
    do_log_use_of_obsolete_attr(Attr, Val).


do_log_use_of_obsolete_attr(Attr, Val) ->
    ?LOG_ERROR("Executing Tx including obsolete OamAccessPoint "
	       "attribute ~p~nVal = ~p", [Attr, Val]),
    sysInitI:warning_msg("~p: Executing Tx including obsolete "
			 "OamAccessPoint attribute ~p~nVal = ~p~n",
			 [?MODULE, Attr, Val]).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           validate_objects(Objs) -> ok | {error, Reason::string()}
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
validate_objects(Objs) ->
    case validate_objects(Objs, []) of
	{ok, OAPObjs} ->
	    Config = objects_to_config(OAPObjs),
	    validate_port_objects(Config);
	Other ->
	    Other
    end.


validate_objects([{_DN, Obj} | Objs], Acc) ->
    case validate_object(Obj) of
	ok ->   
	    validate_objects(Objs, Acc);
	{ok, OAPObj} ->   
	    validate_objects(Objs, [OAPObj | Acc]);
	Error ->
	    Error
    end;

validate_objects([], Acc) ->
    {ok, Acc}.
	    
 
validate_object(#netconfSsh{port = Port} = Obj) ->
    Tgt = not ootLib:is_host_sim(),
    if
	Port =:= ?DEFAULT_NETCONF_PORT_VAL, Tgt ->
	    {ok, Obj};
	Port >= ?MIN_PORT_NO andalso Port =< ?MAX_PORT_NO ->
	    {ok, Obj};
	Tgt ->
	    {error, ?ERR_INVALID_NC_TGT_PORT(Port)};
	true ->
	    {error, ?ERR_INVALID_PORT(Port)}
    end;

validate_object(#cliSsh{port = Port} = Obj) ->
    if
	Port >= ?MIN_PORT_NO andalso Port =< ?MAX_PORT_NO ->
	    {ok, Obj};
	true ->
	    {error, ?ERR_INVALID_PORT(Port)}
    end;

validate_object(#oamTrafficClass{name = _Name, dscp = _Dscp} = Obj) ->
    {ok, Obj};

validate_object(_Obj) ->
    ok.


%%% ----------------------------------------------------------
%%% #           validate_port_objects(Config) -> 
%%%                 {ok, Config} | {error, Reason::string()}
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
validate_port_objects(Config) ->
    case ootLib:verify_port_config_change(Config) of
	ok ->
	    {ok, Config};
	{error, Reason} ->
	    {error, to_error_string(Reason)}
    end.


%%% ----------------------------------------------------------
%%% #           objects_to_config(Objs) -> Config::list()
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
objects_to_config(Objs) ->
    objects_to_config(Objs, []).


objects_to_config([#netconfSsh{port = Port} | Objs], Acc) ->
    objects_to_config(Objs, [{?CNF_NETCONF_PORT, Port} | Acc]);

objects_to_config([#cliSsh{port = Port} | Objs], Acc) ->
    objects_to_config(Objs, [{?CNF_CLI_PORT, Port} | Acc]);

objects_to_config([#oamTrafficClass{dscp = Dscp} | Objs], Acc) ->
    objects_to_config(Objs, [{?CNF_DSCP, Dscp} | Acc]);

objects_to_config([_ | Objs], Acc) ->
    objects_to_config(Objs, Acc);

objects_to_config([], Acc) ->
    Acc.


%%% ----------------------------------------------------------
%%% #           to_error_string(Reason) -> ErrorString::string().
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
to_error_string({port_busy, Port}) ->
    ?ERR_BUSY_PORT(Port);

to_error_string({duplicate_val, Val}) ->
    %% Should probably come up with something better here.
    ?ERR_BUSY_PORT(Val).


%%% ----------------------------------------------------------
%%% #           store_changed_conf_in_imm(Config) -> ok.
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Store in IMM via COI instead of SAF in order to avoid 
%%%              admin owner conflict with GMF. 
%%% ----------------------------------------------------------
%% store_changed_conf_in_imm(Config) ->
%%     case get_changed_config(Config) of
%% 	[] ->
%% 	    ok;
%% 	NewConfig ->
%% 	    sysInitI:info_msg("~p: Configuration updated.~n~p~nMirror values "
%% 			      "to corresponding deprecated IMM attributes.~n", 
%% 			      [?MODULE, NewConfig]),
%% 	    AttrVals = ootLib:config_to_ecim_oap_attr_vals(NewConfig),
%% 	    set_mo_attrs([{?ECIM_DN_OAP, AttrVals}])
%%     end.


%% get_changed_config(NewConfig) ->
%%     OldConfig = ootServer:get_cached_config(),
%%     get_changed_config(NewConfig, OldConfig).


get_changed_config(NewConfig, OldConfig) ->
    [Item || Item <- NewConfig, not lists:member(Item, OldConfig)].
	     

%%% ----------------------------------------------------------
%%% #           get_oap_mo_attrs(DN, Attrs) -> ok.
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
get_oap_mo_attrs(DN, Attrs) when is_list(Attrs) ->
    {ok, Tx} = coi:join_new(?APP, ?MODULE),
    Res = comsaTransactionServer:getMoAttributes(Tx, DN, Attrs),
    %% Res = coi:getMoAttributes(Tx, DN, Attrs),
    coi:finish(Tx),
    Res.


get_oap_mo_attrs(DN) ->
    get_oap_mo_attrs(DN, ?OAP_ATTRS).

%%% ----------------------------------------------------------
%%% #           set_mo_attrs(AttrMods) -> ok.
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
set_mo_attrs(AttrMods) when is_list(AttrMods), AttrMods =/= [] ->
    {ok, Tx} = coi:join_new(?APP, ?MODULE),
    lists:foreach(fun({DN, AttrVals}) ->
			  ok = coi:setMoAttributes(Tx, DN, AttrVals)
		  end, AttrMods),
    case coi:prepare(Tx) of
	ok ->
	    ok = coi:commit(Tx), % Not used by COMSA apparently, maybe remove.
	    ok = coi:finish(Tx);
	Error ->
	    coi:finish(Tx),
	    Error
    end;

set_mo_attrs([]) ->
    ok.
    
%%% ----------------------------------------------------------
%%% #           get_port_from_conf(Type, Conf) -> integer() | undefined.
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
get_port_from_conf(Type, Conf) ->
    case proplists:get_value(port, Conf) of
	Port when is_integer(Port) ->
	    Port;
	_Error ->
	    sysEnv:get_initial_port_conf(Type)
    end.

%%% ----------------------------------------------------------
%%% #           get_mirror_set_oap_attrs() -> list().
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
get_mirror_set_oap_attrs(Attr, OapAttr, Cmd, TransId, AttrVals) ->
    case lists:keyfind(Attr, 1, AttrVals) of
	false ->
	    [];
	{_Attr, Val} ->
	    [{Cmd, TransId, ?ECIM_DN_OAP, [{OapAttr, Val}]}]
    end.
    

%%% ----------------------------------------------------------
%%% #           get_mirror_set_ipv4_addr() -> list().
%%% Input: 
%%% Output: 
%%% Exceptions:
%%% Description: 
%%% ----------------------------------------------------------
get_mirror_set_ipv4_addr(Cmd, TransId, OapDN, Val)
  when Val =/= undefined, Val =/= [] ->
    AttrVal = [{?OAP_ACC_POINT, Val}],
    [{Cmd, TransId, OapDN, AttrVal}];

get_mirror_set_ipv4_addr(Cmd, TransId, OapDN, _Val) ->
    ImmOapDN = ootLib:ecim_dn_to_imm_dn_oap(OapDN),
    case ootImm:get_acc_point_dn(ImmOapDN) of
	undefined ->
	    [];
	AccPtDN ->
	    get_mirror_set_ipv4_addr_del(AccPtDN, Cmd, TransId, OapDN)
    end.


get_mirror_set_ipv4_addr_del(AccPtDN, Cmd, TransId, OapDN) ->
    case re:run(AccPtDN, ?ADDR_IPV4_ID) of
	{match, _} ->
	    [{Cmd, TransId, OapDN, [{?OAP_ACC_POINT, undefined}]}];
	_NoMatch ->
	    %% Don't mirror IPv6
	    []
    end.


%%% ----------------------------------------------------------
%%% #           get_mirror_set_acc_point() -> list().
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
get_mirror_set_acc_point(Cmd, TransId, DN, {_Type, Val} = TV) ->
    case re:run(Val, ?ADDR_IPV4) of
	{match, _} ->
	    [{Cmd, TransId, DN, [{?OAP_IPV4_ADDR, TV}]}];
	_NoMatch ->
	    %% Don't mirror IPv6
	    [{Cmd, TransId, DN, [{?OAP_IPV4_ADDR, undefined}]}]
    end;

get_mirror_set_acc_point(Cmd, TransId, DN, Undef) ->
    [{Cmd, TransId, DN, [{?OAP_IPV4_ADDR, Undef}]}].


%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
