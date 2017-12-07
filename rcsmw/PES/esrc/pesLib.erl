%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pesLib.erl %
%%% Author:	
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
%%% @private
-module(pesLib).
-vsn('/main/R3A/3').
-date('2014-12-04').
-author('eolaand').
-shaid('dc1d7b77eb5a114cfe9be1c485810a6de7091fac').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014 All rights reserved.
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
%%% Rev        Date          Name        What
%%% -----      ----------    --------    ------------------------
%%% R1A/1      2013-02-01    uabesvi     Created
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([get_time/0]). 
-export([epoch_time/0]). 
-export([get_interval/1]). 
-export([get_me_data/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsPMEventM.hrl").

-define(ROP_MULTI_VAL_FORMAT, "ROP_MULTI_VAL_FORMAT"). 
-define('3GPP', "3gpp").
-define(ECIM, "ecim").

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%===========================================================================
%% epoch_time() -> integer()
%% 
%% Calculates the current time in seconds since 1 jan 1970.
%%===========================================================================
epoch_time() ->
    {MeSec, Sec, MiSec} = os:timestamp(),
    erlang:round(MeSec * 1000000 + Sec + MiSec/1000000).
 

%%========================================================================
%% get_interval(GP) -> Interval
%%
%% Transform the GP ENUM to seconds.
%% 
%%========================================================================
get_interval(?TimePeriod_TEN_SECONDS)    -> 10;
get_interval(?TimePeriod_THIRTY_SECONDS) -> 30;
get_interval(?TimePeriod_ONE_MIN)        -> 60;
get_interval(?TimePeriod_FIVE_MIN)       -> 60*5;
get_interval(?TimePeriod_FIFTEEN_MIN)    -> 60*15;
get_interval(?TimePeriod_THIRTY_MIN)     -> 60*30;
get_interval(?TimePeriod_ONE_HOUR)       -> 60*60;
get_interval(?TimePeriod_TWELVE_HOUR)    -> 60*60*12;
get_interval(?TimePeriod_ONE_DAY)        -> 60*60*24.

%%===========================================================================
%% get_time(GP) -> string()
%% 
%% get a printable time
%%===========================================================================
get_time() ->
    Now = {_, _, MS} = os:timestamp(),
    get_time(calendar:now_to_local_time(Now), MS).
get_time({{Y, M, D}, {H, Mi, S}}, MS) ->
    lists:append([integer_to_list(Y), "-",
		  gt_zero(M),
		  integer_to_list(M), "-",
		  gt_zero(D),
		  integer_to_list(D), "  ",
		  gt_zero(H),
		  integer_to_list(H), ":",
		  gt_zero(Mi),
		  integer_to_list(Mi), ":",
		  gt_zero(S),
		  integer_to_list(S), ".",
		  gt_zero_ms(MS),
		  integer_to_list(MS)]).
 
gt_zero(X) when X < 10 -> "0";
gt_zero(_)             -> "".
    
gt_zero_ms(X) when X < 10     -> "00000";
gt_zero_ms(X) when X < 100    -> "0000";
gt_zero_ms(X) when X < 1000   -> "000";
gt_zero_ms(X) when X < 10000  -> "00";
gt_zero_ms(X) when X < 100000 -> "0";
gt_zero_ms(_)                 -> "".
    
%%===========================================================================
%% get_me_data() -> {UserLabel, NetworkManagedElementId}
%% 
%% Get userLabel and networkManagedElementId from COMSA.
%%===========================================================================
get_me_data() ->
    MEData = comsaI:get_managed_element_data(),
    UserLabel = proplists:get_value(userLabel, MEData),
    NEMEId = proplists:get_value(networkManagedElementId, MEData),
    {UserLabel, NEMEId}.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%===========================================================================
%% Misc functions
%%===========================================================================
%%log(_, _) -> ok.

%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


