%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfDb.erl %
%%% Author:
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(gmfDb).
-vsn('/main/R1A/R2A/R5A/3').
-author('etxpeno').
-shaid('98adc426134c2b03428467eb02551de7c7794dbd').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	xxxDataInit.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R1A/1      2012-06-14 uabesvi     Created
%% ----    ---------- -------  ------------------------------------------------
%% R5A/1   2016-01-07 etxberb  Added transaction check in write/1.
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% ets functions
-export([lookup_value/3]).
-export([lookup_value/4]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("gmf.hrl").

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%===========================================================================
%% @spec lookup_value(Tab, Key, DefaultVal)
%%
%%       -> Val | DefaultVal
%%
%% where
%%
%%   Tab        = atom()
%%   Key        = term()
%%   DefaultVal = term()
%%   Val        = term()
%%
%% @doc
%%   Create new ets table.
%% @end
%%===========================================================================
lookup_value(Tab, Key, DefaultVal) ->
    lookup_value(Tab, Key, DefaultVal, 2).

lv([Rec|_], _, Pos)  -> element(Pos, Rec);
lv(_, DefaultVal, _) -> DefaultVal.


%%===========================================================================
%% @spec lookup_value(Tab, Key, DefaultVal, Pos)
%%
%%       -> Val | DefaultVal
%%
%% where
%%
%%   Tab        = atom()
%%   Key        = term()
%%   DefaultVal = term()
%%   Val        = term()
%%   Pos        = integer()
%%
%% @doc
%%   Create new ets table.
%% @end
%%===========================================================================
lookup_value(Tab, Key, DefaultVal, Pos) ->
    lv(trycatch(Tab, ets, lookup, [Tab, Key]), DefaultVal, Pos).


trycatch(Tab, M, F, A) ->
    try
	undefined /= ets:info(Tab) orelse throw({?MODULE, {error, {no_table, Tab}}}),
	apply(M, F, A)
    catch throw:{?MODULE, Err} ->
	    Err
    end.




%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%internal_function1(One, Two)->
%   nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%even_more_internal_function1(One, Two)->
%   nnn.

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------



%%% # Code stolen from erlib_db.erl
