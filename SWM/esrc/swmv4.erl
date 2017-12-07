%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 1.    BASIC INFORMATION
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 1.1   MODULE INFORMATION
%% ###-----------------------------------------------------------------------###
%% %CCaseFile:	swmv4.erl %
%% @author etxberb
%% @copyright Ericsson AB 2017
%% @version /main/R11A/R12A/3

%% @doc == REST API for SWM, version 4 ==
%% Interface module for REST API, specified in swmWeb.xml.
%%
%% @end

%% ###=======================================================================###
%% # 1.2   MODULE DEFINITION
%% ###-----------------------------------------------------------------------###
-module(swmv4).
-vsn('/main/R11A/R12A/3').
-date('2017-11-20').
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
%% R11A/1  2017-10-19 etxberb  Created.
%% ----    ---------- -------  -------------------------------------------------
%% R12A/1  2017-10-27 etxberb  Additions for "SP277: backup/restore of vSD/vPP"
%% R12A/2  2017-11-17 etxberb  Additions for "SP277: backup/restore of vSD/vPP"
%% R12A/3  2017-11-20 etxberb  Continuation of SP277.
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
-export([is_supported/3,
	 post_restore_check/3,
	 restore_aborted/3,
	 restore_confirmed/3]).

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
%% @doc REST query for doing post-restore checks.
%%
%% @end
%% ###=======================================================================###
-spec post_restore_check(SessionID :: any(),
			 Env       :: list({Property :: any(),
					    Value :: any()}),
			 Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
post_restore_check(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(post_restore_check).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
post_restore_check("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    swmvBuRestore:post_restore_check(),
    swmREST:deliver(SessionID, Env, ?JsonResult_Props([]));
post_restore_check(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% #############################################################################
%% @doc REST query to inform that the restore action has been aborted.
%%
%% @end
%% ###=======================================================================###
-spec restore_aborted(SessionID :: any(),
		      Env       :: list({Property :: any(),
					 Value :: any()}),
		      Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
restore_aborted(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(restore_aborted).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
restore_aborted("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    Info = swmREST:maps_get_optional(?JsonAttrName_Info, Attrs),
    ResInfo = swmREST:maps_get_optional(?JsonAttrName_ResultInfo, Attrs),
    swmvBuRestore:restore_aborted(#{info => Info,
				    resultInfo => ResInfo}),
    swmREST:deliver(SessionID, Env, ?JsonResult_Props([]));
restore_aborted(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% #############################################################################
%% @doc REST query to inform that the restore action has been confirmed.
%%
%% @end
%% ###=======================================================================###
-spec restore_confirmed(SessionID :: any(),
			Env       :: list({Property :: any(),
					   Value :: any()}),
			Body      :: string()) ->
    any().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
restore_confirmed(SessionID, Env, Body) ->
    ?TRY_REST_QUERY(restore_confirmed).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
restore_confirmed("POST", SessionID, Env, Body) ->
    Attrs = swmREST:decode(Body),
    ?LOG_RestQuery([Attrs]),
    Info = swmREST:maps_get_optional(?JsonAttrName_Info, Attrs),
    swmvBuRestore:restore_confirmed(#{info => Info}),
    swmREST:deliver(SessionID, Env, ?JsonResult_Props([]));
restore_confirmed(_, SessionID, Env, Body) ->
    ?LOG_Unexpected(?JsonDescr_ReqMethNotSupported),
    swmREST:deliver(SessionID,
		    Env,
		    ?JsonResult_Err(501, ?JsonDescr_ReqMethNotSupported)).

%% ###=======================================================================###
%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 3.3.1 Xxx handling
%% ###-----------------------------------------------------------------------###

%% ###-----------------------------------------------------------------------###
%% # 3.3.2 Help Functions
%% ###-----------------------------------------------------------------------###

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 4     CODE FOR TEMPORARY CORRECTIONS
%% ###-----------------------------------------------------------------------###
