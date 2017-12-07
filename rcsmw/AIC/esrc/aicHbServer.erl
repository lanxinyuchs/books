%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	aicHbServer.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/4

%%% @doc ==The auto integration heartbeat server==
%%% Handles the AIC state. The state is updated from aicServer,
%%% and read by VNFC when it gets the heartbeat request from VNFM.
%%% @end
%%% ----------------------------------------------------------
-module(aicHbServer).
-behaviour(gen_server).
-vsn('/main/R11A/4').
-date('2017-10-17').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% R11A/1     17-08-09   uabesvi     Created
%%% R11A/2     17-09-27   uabesvi     Revomed 'detail' from the reply
%%% R11A/3     17-10-10   uabesvi     Added get_heartbeat_status, 
%%%                                   new state attribute heartbeat
%%% R11A/4     17-10-17   uabesvi     Added info_msg/logs
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0]).
-export([heartbeat/0]).
-export([get_heartbeat_status/0]).
-export([status_update/3]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("aic.hrl").

%% aicHbServer internal server state
-record(state,
        {
	  heartbeat = false,
          status    = ?HB_STARTING,
	  detail    = undefined,
	  error     = undefined
        }).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

heartbeat() ->
    gen_server:call(?MODULE, heartbeat).

get_heartbeat_status() ->
    gen_server:call(?MODULE, get_heartbeat_status).

status_update(Status, Detail, Error) ->
    gen_server:cast(?MODULE, {status_update, {Status, Detail, Error}}).


%%% ----------------------------------------------------------
%%% @doc Starts the aicHbServer process
%%% @end
%%% ----------------------------------------------------------
start() ->
    ServerName = {local, ?MODULE},
    Module     = ?MODULE,
    Args       = [],
    Options    = [],
    gen_server:start_link(ServerName, Module, Args, Options).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% ----------------------------------------------------------
%%% gen_server callbacks
%%% ----------------------------------------------------------
init(_Args) ->
    {ok, #state{}}.


handle_call(heartbeat,
	    _From, 
	    #state{status = Status,
		   detail = _Detail,
		   error  = Error} = State) ->
    %% It was decided not not to send detail information
    %% because it may confuse the user; if the AI fails
    %% and is rerun the information in detail may have 
    %% indicated that the AI is almost done, and then 
    %% suddenly shows that it is in the beginning of the
    %% sequence again.
    {reply, {Status, "", Error}, State#state{heartbeat = true}};

handle_call(get_heartbeat_status,
	    _From, 
	    #state{heartbeat = HB} = State) ->
    {reply, HB, State};

handle_call(_Request, _From, State) ->
    {reply, undefined, State}.



handle_cast({status_update, {Status, Detail, Error} = Update},
	    #state{status = OldStatus,
		   detail = OldDetail,
		   error  = OldError} = State) ->
    sysInitI:info_msg("~p: status update  ~p~n", [?MODULE, Update]),
    {noreply, 
     State#state{status = choose(Status == ?HB_NO_CHANGE, OldStatus, Status),
		 detail = choose(Detail == ?HB_NO_CHANGE, OldDetail, Detail),
		 error  = choose(Error  == ?HB_NO_CHANGE, OldError,  Error)
		}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


choose(true,  T, _) -> T;
choose(false, _, F) -> F.
