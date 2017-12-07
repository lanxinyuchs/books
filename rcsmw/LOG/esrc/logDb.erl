%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logDb.erl %
%%% @private
%%% Author:	 uabesvi
%%% Description: Common LOG DB API.
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(logDb).
-vsn('/main/R9A/R10A/1').
-date('2017-04-12').
-author('uabesvi').
-shaid('a2bfbe43458cccf1a6b269e42a473d97228a796a').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
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
%%% Rev     Date       Name     What
%%% ----    ---------- -------  ------------------------------
%%% R9A/1   2017-03-27 uabesvi  First version
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% API
-export([create_table/2]).

-export([log_data_get/1]).
-export([log_data_set/1]).
-export([log_data_get_dirty/1]).
-export([log_data_set_dirty/1]).
-export([log_data_write/1]).

-export([log_get_all/0]).
-export([log_get/1]).
-export([log_set/1]).
-export([log_get_dirty/1]).
-export([log_set_dirty/1]).

-include("log.hrl").


%%%===================================================================
%%% Common functions
%%%===================================================================
create_table(Name, Attributes) ->
    clhI:mnesia_create_table(Name, Attributes ++ add_clh_option(Name)).

%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].



%%%===================================================================
%%% logData API
%%%===================================================================
log_data_get(Key) ->
    trans(fun() -> mnesia:read(logData, Key) end, log_data_get).

log_data_set(Rec) ->
    trans(fun() -> mnesia:write(Rec) end, log_data_set).

log_data_get_dirty(Key) ->
    mnesia:dirty_read(logData, Key).

log_data_set_dirty(Rec) ->
    mnesia:dirty_write(Rec).

log_data_write(Rec) ->
    mnesia:write(Rec).


%%%===================================================================
%%% log API
%%%===================================================================
log_get_all() ->
    ets:tab2list(log).   

log_get(Key) ->
    trans(fun() -> mnesia:read(log, Key) end, log_get).

log_set(Rec) ->
    trans(fun() -> mnesia:write(Rec) end, log_set).

log_get_dirty(Key) ->
    mnesia:dirty_read(log, Key).

log_set_dirty(Rec) ->
    mnesia:dirty_write(Rec).



%%%===================================================================
%%% mnesia transaction
%%%===================================================================
trans(Fun, Fnc) ->
    case mnesia:transaction(Fun) of
	{atomic, ok}  -> 
	    ok;
	{atomic, Res} -> 
	    {ok, Res};
	Error -> 
	    sysInitI:error_report([{mfa, {?MODULE, trans, [Fnc]}},
				   {error, Error}]),
	    {error, Error}
    end.
   
	    

		       
		       
