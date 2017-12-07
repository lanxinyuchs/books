%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	appmI.erl %
%%% Author:	etxbjca
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(appmI).
-author(etxjotj).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R7A/R8A/R9A/R12A/2').
-date('2017-11-08').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R2A/2      130311     etxarnu     Fix in call_up to pass dialyzer
%%% R2A/4      130515     etxarnu     Implemented call_upi, first version.
%%% R2A/5-6    130520     etxarnu     Added check for param in call_upi
%%% R2A/7      130520     etxarnu     Added environment variables to upi script
%%% R2A/8      130520     etxarnu     Bug fix
%%% R2A/9      130522     etxarnu     Bug fix
%%% R2A/10     130524     etxarnu     Added extra arg to verifyUpgrade
%%% R2A/11     130524     etxarnu     Added continue_after_upgrade/0
%%% R2A/12     130910     etxpeno     Added restart_piu/1
%%% R2A/15     140205     etxberb     Added restart_node/2
%%% R3A/1      140924     etxarnu     Added appdata_complete/0
%%% R3A/2      141219     etxjotj     Added appdata_upgrade &c.
%%% R3A/3      150224     etxberb     Added restart functionality for cluster.
%%% R3A/4      150304     etxarnu     Corrected call_upi if LM removed 
%%% R3A/5      150305     etxarnu     Corrected bug in call_upi 
%%% R4A/1      150710     eolaand     Add fcn get_rcs_env
%%% R4A/2      150818     etxarnu     Added register_warm_cb/1
%%% R4A/4      151120     etxarnu     Added inhibit_escalation/0,1 
%%%                                   and cancel_inhibit/0
%%% R5A/1  160125  etxarnu  Fix for HU50598
%%% R5A/2  160225  etxarnu  HU60044: Added confirm/0 called at upgrade
%%% R8A/1  161221  etxarnu  Changed appmAppData:get_lm_start_data
%%%                                to appmAppData:get_lm_paths
%%% R12A/1     171023     etxarnu     Added register_pgroup_cb/1
%%% R12A/2     171106     etxarnu     Added register_cold_cb/1
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([appdata/3, appdata_upgrade/3]).
-export([appdata_complete/0, appdata_upgrade_complete/0]).
-export([call_upi/1]).
-export([continue_after_upgrade/0]).
-export([confirm/0]).
-export([restart_node/2,
	 restart_node/3]).
-export([restart_own_piu/3,
	 restart_own_piu/4,
	 restart_piu/3,
	 restart_piu/4,
	 restart_piu/5]).
-export([restart_piu_warm/1]).
-export([restart_piu_cold/1]).
-export([inhibit_escalation/0]).
-export([inhibit_escalation/1]).
-export([cancel_inhibit/0]).
-export([get_rcs_env/1, get_rcs_env/2]).
-export([register_warm_cb/1]).
-export([register_pgroup_cb/1]).
-export([register_cold_cb/1]).

-define(AVLI_CAUSE_ManualCOLI, "ManualCOLI").

-include("appm.hrl").

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% @doc Register appdata for appm, called from SWM
%%% ===Arguments===
%%% ProdNo - Product number CXP
%%% ProdRev - Revision of CXP
%%% Data - the xml data as records defined by xmerl


-spec appdata(ProdNo :: string(), ProdRev :: string(), Data :: tuple()) -> ok.

appdata(ProdNo, ProdRev, Data) ->
    appmAppData:appdata(ProdNo, ProdRev, Data).

-spec appdata_upgrade(ProdNo :: string(), ProdRev :: string(), Data :: tuple()) -> ok.

appdata_upgrade(ProdNo, ProdRev, Data) ->
    appmAppData:appdata_upgrade(ProdNo, ProdRev, Data).


%%% @doc Called after all appdata calls, called from SWM
%%% ===Arguments===

-spec appdata_complete() -> ok.

appdata_complete() ->
    appmAppData:appdata_complete().

-spec appdata_upgrade_complete() -> ok.

appdata_upgrade_complete() ->
    appmAppData:appdata_upgrade_complete().

%%% @doc Upgrade callbacks to applications, called from SWM
%%% ===Arguments===
%%% Phase - The phase of the upgrade


-spec call_upi(Phase :: string()) -> {ok,[{{Name::string(),LmId::string()},Result::string()}]} | {error,Reason::any()}.

call_upi(Phase) when is_list(Phase)->
    call_all_upi(Phase,appmAppData:get_upi_apps(), []);
call_upi(Phase)->
    sysInitI:error_msg("appmI:call_upi(~p) failed with reason ~p ~n",
			   [Phase,arg_not_a_list]),
    {error,arg_not_a_list}.

call_all_upi(_Phase,[], Resp) ->
    {ok,Resp};
call_all_upi(Phase,[H={_Name,_Id}|T], Resp) ->
    {ok, CxpPath, RelPath} = appmAppData:get_lm_paths(H),
    Args = case Phase of
	       "verifyUpgrade" ->
		   case catch swmI:get_new_cxp_path(H) of 
		       {'EXIT',{unknown_lm_id,_}} ->
			   unknown_lm_id;
		       NewCxp ->
			   NewCxp
		   end;
	       _ -> ""
	   end,
    Path = filename:join([CxpPath, RelPath]),
    Tpath = swmI:find_file(Path), %use patched version if exist

    case Args of
	unknown_lm_id ->
	    call_all_upi(Phase, T, [{H,"unknown_lm_id"}|Resp]);
	Args ->
	    case filelib:wildcard(Tpath) of
		[Prog] ->
		    %%could be optimized to just read once except for CXP_PATH
		    Env = appmServer:get_rcs_env(CxpPath),
		    Str = ["export "++K++"='"++V++"';" ||{K,V} <- Env],
		    info_msg(Prog ++ " " ++ Phase ++" " ++ Args ++"~n", []),
		    R = os:cmd(Str ++ Prog ++ " " ++ Phase ++" " ++ Args),
		    call_all_upi(Phase, T, [{H,R}|Resp]);
		Other ->
		    sysInitI:error_msg("appmI:call_upi(~p) failed for ~p with reason ~p ~n",
					   [Phase,Tpath,Other]),
		    call_all_upi(Phase, T, [{H,"ERROR:UPI-file_not_found"}|Resp])
	    end
    end.

			   


-spec continue_after_upgrade() -> ok.
continue_after_upgrade()->
    appmServer:continue_after_upgrade().

%%====================================================================
%% @doc confirm/0 is called when the upgrade is confirmed
%% 
%%====================================================================
-spec confirm() -> ok.
confirm()->
    appmServer:confirm().



%%====================================================================
%% @doc Register a callback to be called before and after applications 
%% in a program group are taken down at program group restart
%% 
%%====================================================================
-spec register_pgroup_cb(Module :: atom()) -> ok.
register_pgroup_cb(Module) ->
    appmAppData:register_pgroup_cb(Module).

%%====================================================================
%% @doc Register a callback to be called before and after applications 
%% are taken down at warm restart
%% 
%%====================================================================
-spec register_warm_cb(Module :: atom()) -> ok.
register_warm_cb(Module) ->
    appmAppData:register_warm_cb(Module).

%%====================================================================
%% @doc Register a callback to be called before a cold restart
%% 
%%====================================================================
-spec register_cold_cb(Module :: atom()) -> ok.
register_cold_cb(Module) ->
    appmAppData:register_cold_cb(Module).

%%====================================================================
%% 
%%====================================================================
-spec restart_node(RestartRank :: atom(),
		   RestartCause :: string()) -> ok.
restart_node(RestartRank, RestartCause) ->
    %% TODO: Use definition of RestartType from CLH.
    %% TODO: Default value of RestartType? soft?
    restart_node(hard, RestartRank, RestartCause).

%%====================================================================
-spec restart_node(RestartType :: term(),   % TODO: According to CLH
		   RestartRank :: atom(),
		   RestartCause :: string()) -> ok.
restart_node(RestartType, RestartRank, RestartCause) ->
    appmServer:restart_node(RestartType, RestartRank, RestartCause).

%%====================================================================
%% 
%%====================================================================
-spec restart_own_piu(RestartRank :: atom(),
		      RestartEscalation :: boolean(),
		      RestartCause :: string()) -> ok.
restart_own_piu(RestartRank, RestartEscalation, RestartCause) ->
    %% TODO: Use definition of RestartType from CLH.
    %% TODO: Default value of RestartType? soft?
    restart_own_piu(hard, RestartRank, RestartEscalation, RestartCause).

%%====================================================================
-spec restart_own_piu(RestartType :: term(),   % TODO: According to CLH
		      RestartRank :: atom(),
		      RestartEscalation :: boolean(),
		      RestartCause :: string()) -> ok.
restart_own_piu(RestartType, RestartRank, RestartEscalation, RestartCause) ->
    appmServer:restart_piu(clhI:own_mp_id(),
			   RestartType,
			   RestartRank,
			   RestartEscalation,
			   RestartCause).

%%====================================================================
%% 
%%====================================================================
-spec restart_piu(RestartRank :: atom(),
		  RestartEscalation :: boolean(),
		  RestartCause :: string()) -> ok.
restart_piu(RestartRank, RestartEscalation, RestartCause) ->
    restart_own_piu(RestartRank, RestartEscalation, RestartCause).

%%====================================================================
%% 
%%====================================================================
-spec restart_piu(MpId :: integer(),
		  RestartRank :: atom(),
		  RestartEscalation :: boolean(),
		  RestartCause :: string()) -> ok.
restart_piu(MpId, RestartRank, RestartEscalation, RestartCause) ->
    %% TODO: Use definition of RestartType from CLH.
    %% TODO: Default value of RestartType? soft?
    restart_piu(MpId, hard, RestartRank, RestartEscalation, RestartCause).

%%====================================================================
-spec restart_piu(MpId :: integer(),
		  RestartType :: term(),   % TODO: According to CLH
		  RestartRank :: atom(),
		  RestartEscalation :: boolean(),
		  RestartCause :: string()) -> ok.
restart_piu(MpId, RestartType, RestartRank, RestartEscalation, RestartCause) ->
    appmServer:restart_piu(MpId,
			   RestartType,
			   RestartRank,
			   RestartEscalation,
			   RestartCause).

%%====================================================================
%% 
%%====================================================================
restart_piu_warm(_) ->
    restart_own_piu(warm, false, ?AVLI_CAUSE_ManualCOLI).
restart_piu_cold(_) ->
    restart_own_piu(cold, false, ?AVLI_CAUSE_ManualCOLI).


%%====================================================================
%% 
%%====================================================================
-spec get_rcs_env(CxpPath::list()) -> Env::list(). 
get_rcs_env(CxpPath) when is_list(CxpPath) ->
    appmServer:get_rcs_env(CxpPath).


-spec get_rcs_env(EnvVar::list(), CxpPath::list()) -> Val::term() | undefined. 
get_rcs_env(EnvVar, CxpPath) when is_list(EnvVar), is_list(CxpPath) ->
    proplists:get_value(EnvVar, appmServer:get_rcs_env(CxpPath)).

%%====================================================================
%% 
%%====================================================================
%%% @doc 
%%%  inhibit_escalation
%%%  Inhibits escalations due to crashes to let a restore
%%%  backup run without disturbance. Called from SWM.
%%% ===Arguments===
%%% Time - max time to wait for reboot, else a reboot is performed
%%%       If no argument is given, 30 minutes is assumed

-spec inhibit_escalation() -> ok. 
inhibit_escalation() ->
    inhibit_escalation(30).
-spec inhibit_escalation(Time::integer()) -> ok. 
inhibit_escalation(Time) when is_integer(Time) ->
    appmServer:inhibit_escalation(Time).

%%% @doc 
%%%  cancel_inhibit
%%%  Cancels inhibit. Called from SWM. If any crashes has occured during
%%%                   inhibit period, a reboot is done.
-spec cancel_inhibit() -> ok. 
cancel_inhibit() ->
    appmServer:cancel_inhibit().


info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).
