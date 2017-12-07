%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	fruacc_lib.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R4A/R6A/1
-module(fruacc_lib).
-id('Updated by CCase').
-vsn('/main/R4A/R6A/1').
-date('2016-05-18').
-author('uabesvi').
%%% ----------------------------------------------------------
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
%%% -----      ---------  --------    ------------------------
%%% R4A/1      2015-07-22 uabesvi     Created
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([start/1]).
-export([stop/1]).

-export([hunt/1]).
-export([hunt/2]).
-export([register_frus/2]).
-export([add_fru/3]).
-export([delete_fru/3]).
-export([replies/2]).

-export([rpc/3]).
-export([rpc/4]).

-define(TESTNODE, rpc).

%%--------------------------------------------------------------------
%% @doc 
%% Start ITC.<br/><br/>
%% @end
%%--------------------------------------------------------------------
start(Name) ->
    rpc(ecoli_debug, itc_start, [Name]).

stop(Name) ->
    rpc(ecoli_debug, itc_stop, [Name]).


hunt(Name) ->
    hunt(Name, "ecoli_coli").
hunt(Name, Hunt) ->
    rpc(ecoli_debug, itc_hunt, [Name, Hunt]).

register_frus(Name, Frus) ->
    rpc(ecoli_debug, itc_register, [Name, Frus]).

add_fru(Name, Type, Ldn) ->
    rpc(ecoli_debug, itc_add_fru, [Name, Type, Ldn]).

delete_fru(Name, Type, Ldn) ->
    rpc(ecoli_debug, itc_del_fru, [Name, Type, Ldn]).

replies(Name, Replies) ->
    rpc(ecoli_debug, itc_reply, [Name, Replies]).




rpc(M, F, A) ->
    rct_rpc:call(?TESTNODE, M, F, A, 20000, noprint).

rpc(M, F, A, P) ->
    rct_rpc:call(?TESTNODE, M, F, A, 20000, P).

