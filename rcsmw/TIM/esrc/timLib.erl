%%% ----------------------------------------------------------
%%% %CCaseFile:	timLib.erl %
%%% Author:	erarafo
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(timLib).
-id('Updated by CCase').
-vsn('/main/R4A/6').
-date('2015-12-01').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R4A/1      2015-10-16 erarafo     First version
%%% R4A/2      2015-10-18 erarafo     Cleanup
%%% R4A/3      2015-10-27 erarafo     Adapted to OTP18 time API
%%% R4A/4      2015-11-03 erarafo     Line length not exceeding 80
%%% R4A/5      2015-11-18 erarafo     Trivial function removed
%%% R4A/6      2015-11-30 erarafo     Typo in EDoc fixed
%%% ----------------------------------------------------------

-export(
   [dstStatusCode/1,
    timestepCallback/0,
    universal_time/1,
    nowItime/2,
    nowItime/4,
    timeString/1,
    foldTree/3,
    reportTimeProperties/0]).


-include("tim.hrl").
-include("cello_tzii.hrl").
-include("cello_tzii_sig.hrl").


%%% ----------------------------------------------------------
%%% @doc Maps the given DST status to the code to be used
%%% in signals.
%%% @end
%%% ----------------------------------------------------------
-spec dstStatusCode(dstStatus()) -> 0|1.

dstStatusCode(undefined) ->
    ?FALSE;
dstStatusCode(on) ->
    ?TRUE;
dstStatusCode(off) ->
    ?FALSE.


%%% ----------------------------------------------------------
%%% @doc COMSA calls this function when a timestep has been
%%% detected.
%%% @end
%%% ----------------------------------------------------------
-spec timestepCallback() -> ok.

timestepCallback() ->
    gen_server:cast(?SERVER, timestep).


%%% ----------------------------------------------------------
%%% @doc Wrapper for calendar:universal_time/0 that adds in
%%% a simulated future value in milliseconds.
%%% @end
%%% ----------------------------------------------------------
-spec universal_time(integer()) -> calendar:datetime().

universal_time(SimFuture) ->
    DT = calendar:universal_time(),
    GS = calendar:datetime_to_gregorian_seconds(DT) + SimFuture div 1000,
    calendar:gregorian_seconds_to_datetime(GS).


%%% ----------------------------------------------------------
%%% @doc Returns current time expressed as itime().
%%% @end
%%% ----------------------------------------------------------
-spec nowItime(millis(), integer()) ->  itime().

nowItime(RefTs, SimFuture) ->
    os:system_time(milli_seconds) + SimFuture - RefTs.


%%% ----------------------------------------------------------
%%% @doc Returns current time expressed as itime(), with
%%% trace as an option.
%%% @end
%%% ----------------------------------------------------------
-spec nowItime(millis(), integer(), integer(), string()) ->  itime().

nowItime(RefTs, SimFuture, _Planned, _Label) ->
    os:system_time(milli_seconds) + SimFuture - RefTs.


%%% ----------------------------------------------------------
%%% @doc Convert a millisecond value to a human-readable
%%% string.
%%%
%%% TODO, remove when no more needed
%%% @end
%%% ----------------------------------------------------------

timeString(Millis) ->
    Days = Millis div (24*3600*1000),
    R1 = Millis rem (24*3600*1000),
    Hours = R1 div (3600*1000),
    R2 = R1 rem (3600*1000),
    Mins = R2 div (60*1000),
    R3 = R2 rem (60*1000),
    Secs = R3 div 1000,
    R4 = R3 rem 1000,
    lists:flatten(
      if
	  Days =:= 0 ->
	      io_lib:format(
		"~2..0w:~2..0w:~2..0w.~3..0w",
		[Hours, Mins, Secs, R4]);
	  true ->
	      io_lib:format(
		"~w d, ~2..0w:~2..0w:~2..0w.~3..0w",
		[Days, Hours, Mins, Secs, R4])
      end).


%%% ----------------------------------------------------------
%%% @doc Apply a function to all entries of a tree and
%%% collect an accumulated result.
%%% @end
%%% ----------------------------------------------------------
-spec foldTree(fun((any(), any(), any()) -> any()), any(), gb_trees:tree()) ->
    any().

foldTree(Fun, Acc0, Tree) ->
    foldTreeHelper(Fun, Acc0, gb_trees:iterator(Tree)).


foldTreeHelper(Fun, Acc, Iter) ->
    case gb_trees:next(Iter) of
	none ->
	    Acc;
	{K, V, NewIter} ->
	    NewAcc = apply(Fun, [K, V, Acc]),
	    foldTreeHelper(Fun, NewAcc, NewIter)
    end.


%%% ----------------------------------------------------------
%%% @doc Report facts about time API properties.
%%% @end
%%% ----------------------------------------------------------
reportTimeProperties() ->
    sysInitI:info_report(
      [{X, erlang:system_info(X)}
       || X <- [time_warp_mode,
		time_correction,
		os_monotonic_time_source]]).



%% doCoverage(Module, Line) ->
%%     {ok, Stream} = file:open("/tmp/coverage-tim.txt", [append]),
%%     io:format(Stream, "|covered| ~w:~w~n", [Module, Line]),
%%     file:close(Stream).
