%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 1.    BASIC INFORMATION
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 1.1   MODULE INFORMATION
%% ###-----------------------------------------------------------------------###
%% %CCaseFile:	swmv3.erl %
%% @author etxpejn
%% @copyright Ericsson AB 2017
%% @version /main/R11A/7

%% @doc == REST API for SWM, version 3 ==
%% Interface module for REST API, specified in swmWeb.xml.
%%
%% @end

%% ###=======================================================================###
%% # 1.2   MODULE DEFINITION
%% ###-----------------------------------------------------------------------###
-module(swmv3).
-vsn('/main/R11A/7').
-date('2017-10-17').
-author(etxberb).

%% ###=======================================================================###
%% # 1.3   LEGAL RIGHTS
%% ###-----------------------------------------------------------------------###
%% %CCaseTemplateFile:	module.erl %
%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%
%% %CCaseCopyrightBegin%
%% Copyright (c) Ericsson AB 2017 All rights reserved.
%% 
%% The information in this document is the property of Ericsson.
%% 
%% Except as specifically authorized in writing by Ericsson, the 
%% receiver of this document shall keep the information contained 
%% herein confidential and shall protect the same in whole or in 
%% part from disclosure and dissemination to third parties.
%% 
%% Disclosure and disseminations to the receivers employees shall 
%% only be made on a strict need to know basis.
%% %CCaseCopyrightEnd%

%% ###=======================================================================###
%% # 1.4   REVISION LOG
%% ###-----------------------------------------------------------------------###
%% Rev     Date       Name     What
%% ----    ---------- -------  -------------------------------------------------
%% R11A/1  2017-09-07 etxpejn  Created.
%% R11A/2  2017-09-12 etxpejn  Added ensure_bin
%% R11A/3  2017-09-13 etxjotj  Removed ensure_bin pending a better solution
%% R11A/4  2017-09-20 etxpejn  Added call to upgrade applications
%% R11A/5  2017-09-27 etxpejn  Added list to binary info 
%% R11A/7  2017-10-17 etxpejn  Moved send_cs_trigger and send_appl_trigger to swmREST
%% ------  ---------- -------  ---END-OF-LOG------------------------------------

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 2.    INTERNAL DEFINITIONS
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 2.1.1 Interface functions
%% ###-----------------------------------------------------------------------###
%% Basic

%% REST Query
-export([activate_start/3,
	 is_supported/3,
	 verify_preconditions/3]).

%% ###-----------------------------------------------------------------------###
%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.3   IMPORT OF DEFINITIONS
%% ###-----------------------------------------------------------------------###
-include("SwmREST.hrl").
-include_lib("kernel/include/file.hrl").

%% ###=======================================================================###
%% # 2.4   LOCAL DEFINITION OF MACROS
%% ###-----------------------------------------------------------------------###


%% ###=======================================================================###
%% # 2.5   LOCAL DEFINITION OF RECORDS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.6   LOCAL DEFINITION OF TYPES
%% ###-----------------------------------------------------------------------###

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 3.    CODE
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% @doc REST query for creating a backup.
%%
%% @end
%% ###=======================================================================###
-spec activate_start(SessionID :: any(),
		     Env       :: list({Property :: any(),
					Value :: any()}),
		     Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
activate_start(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(activate_start).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
activate_start("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),

    %% Add internal calls to erlang app and call to APPM 
    CbModules = swmLib:get_upg_callbacks(),
    {CSAnswer, CSInfo} = swmREST:send_cs_trigger(CbModules, activate_start, "", ok),
    case {{CSAnswer, CSInfo}, swmREST:send_appl_trigger("activateStart")} of
	{{ok, ""}, {ok, ""}} ->
	    swmREST:deliver(SessionID,Env,?JsonResult_Props([{result,<<"SUCCESS">>}]));
	{{ok, Msg1}, {ok, Msg2}} ->
	    swmREST:deliver(SessionID,Env,?JsonResult_Props([{result,<<"SUCCESS">>},
							     {info, Msg1 ++ Msg2}]));
	{{_Ans1, Msg1}, {_Ans2, Msg2}} ->
	    %% One or both of the the answeres are error
	    swmREST:deliver(SessionID,Env,?JsonResult_Props([{result,<<"FAILURE">>},
							     {info, Msg1 ++ Msg2}]))
    end;
activate_start(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% #############################################################################
%% @doc REST query to determine if the REST API Version is supported.
%%
%% @end
%% ###=======================================================================###
-spec is_supported(SessionID :: any(),
		   Env       :: list({Property :: any(), Value :: any()}),
		   Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is_supported(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(is_supported).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is_supported("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    swmREST:deliver(SessionID, Env, ?JsonResult_Props([]));
is_supported(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% #############################################################################
%% @doc REST query to send out verify preconfigure trigger to all applications
%%
%% @end
%% ###=======================================================================###
-spec verify_preconditions(SessionID :: any(),
			   Env       :: list({Property :: any(), Value :: any()}),
			   Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
verify_preconditions(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(verify_preconditions).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
verify_preconditions("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    
    CbModules = swmLib:get_upg_callbacks(),
    {CSAnswer, CSInfo} = swmREST:send_cs_trigger(CbModules, verify_precondition, "", ok),
    case {{CSAnswer, CSInfo}, swmREST:send_appl_trigger("verifyPreconditions")} of
	{{ok, ""}, {ok, ""}} ->
	    swmREST:deliver(SessionID,Env,?JsonResult_Props([{result,<<"SUCCESS">>}]));
	{{ok, Msg1}, {ok, Msg2}} ->
	    swmREST:deliver(SessionID,Env,?JsonResult_Props([{result,<<"SUCCESS">>},
							     {info, Msg1 ++ Msg2}]));
	{{_Ans1, Msg1}, {_Ans2, Msg2}} ->
	    %% One or both of the the answeres are error
	    swmREST:deliver(SessionID,Env,?JsonResult_Props([{result,<<"FAILURE">>},
							     {info, Msg1 ++ Msg2}]))
    end;
verify_preconditions(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).


%% ###=======================================================================###
%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% init_action
%%
%% ###=======================================================================###

%% ###=======================================================================###
%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 3.3.1 Action handling
%% ###-----------------------------------------------------------------------###

%% ###-----------------------------------------------------------------------###
%% # 3.3.2 Help Functions
%% ###-----------------------------------------------------------------------###

%%% This will cause a dialyzer fault if the only known cases is already has
%%% binary input
%% ensure_bin(Term) when is_binary(Term) ->
%%     Term;
%% ensure_bin(Term) ->
%%     list_to_binary(ensure_str(Term)).

%% ensure_str(Term) ->
%%     catch sysUtil:term_to_string(Term).




	    

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 4     CODE FOR TEMPORARY CORRECTIONS
%% ###-----------------------------------------------------------------------###
