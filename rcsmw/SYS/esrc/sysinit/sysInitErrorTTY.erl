%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysInitErrorTTY.erl %
%%% @author etomist
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R4A/R9A/1

%%% @doc ==Specialized Error logger TTY event handler==
%%% A handler that can be connected to the error_logger
%%% event handler.
%%% Writes all events formatted to stdout.
%%%   Handles events tagged error, emulator and info.
%%%
%%% It differs from standard event handler because it ends
%%% and "end" tag to all printouts, and have more detailed
%%% timestamp info

-module(sysInitErrorTTY).
-behaviour(gen_event).
-vsn('/main/R2A/R4A/R9A/1').
-date('2017-03-24').
-author('etomist').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2013-11-21 etxjotj     Created
%%% R2A/2      2013-11-21 etxjotj     Fixed dialyzer faults
%%% ----------------------------------------------------------
%%% R4A/3      2015-09-07 etxtory     sysInitLogDisk calls
%%% ----------------------------------------------------------
%%% R9A/1      2017-03-24 etomist     HV73396
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([write_event/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%% This one is used when we takeover from the simple error_logger.
init({[], {error_logger, Buf}}) ->
    User = set_group_leader(),
    write_events(Buf,io),
    {ok, {User, error_logger}};
%% This one is used if someone took over from us, and now wants to
%% go back.
init({[], {error_logger_tty_h, PrevHandler}}) ->
    User = set_group_leader(),
    {ok, {User, PrevHandler}};
%% This one is used when we are started directly.
init([]) ->
    User = set_group_leader(),
    {ok, {User, []}}.

handle_event({_Type, GL, _Msg}, State) when node(GL) =/= node() ->
    {ok, State};
handle_event(Event, State) ->
    write_event(tag_event(Event),io),
    {ok, State}.

handle_info({'EXIT', User, _Reason}, {User, PrevHandler}) ->
    case PrevHandler of
	[] ->
	    remove_handler;
	_ -> 
	    {swap_handler, install_prev, {User, PrevHandler}, 
	     PrevHandler, go_back}
    end;
handle_info({emulator, GL, Chars}, State) when node(GL) == node() ->
    write_event(tag_event({emulator, GL, Chars}),io),
    {ok, State};
handle_info({emulator, noproc, Chars}, State) ->
    write_event(tag_event({emulator, noproc, Chars}),io),
    {ok, State};
handle_info(_, State) ->
    {ok, State}.

handle_call(_Query, State) -> {ok, {error, bad_query}, State}.

% unfortunately, we can't unlink from User - links are not counted!
%    if pid(User) -> unlink(User); true -> ok end,
terminate(install_prev, _State) ->
    [];
terminate(_Reason, {_User, PrevHandler}) ->
    {error_logger_tty_h, PrevHandler}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% **********************************************************************
%%% Special handling for sysInitI
write_event({Time, {error, _GL, {Pid, "~w~n"++Format, 
				 ['$sysErrorLogger'|Args]}}},
	    IOMod) ->
    T = write_time(maybe_utc(Time)),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    sysInitLogDisk:write_event(T ++ S ++"=END ===\n"),
	    format(IOMod, T ++ S ++"=END ===\n");
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    sysInitLogDisk:write_event(T ++ F++"=END ===\n", [Format,Args]),
	    format(IOMod, T ++ F++"=END ===\n", [Format,Args])
    end;
%% write_event({Time, {emulator, _GL, Chars}},IOMod) ->
%%     T = write_time(maybe_utc(Time)),
%%     case catch io_lib:format(Chars, []) of
%% 	S when is_list(S) ->
%% 	    format(IOMod, T ++ S ++"=END ===\n");
%% 	_ ->
%% 	    format(IOMod, T ++ "ERROR: ~p ~n=END ===~n", [Chars])
%%     end;
%% write_event({Time, {info, _GL, {Pid, Info, _}}},IOMod) ->
%%     T = write_time(maybe_utc(Time)),
%%     format(IOMod, T ++ add_node("~p~n",Pid) ++"=END ===\n",[Info]);
write_event({Time, {error_report, _GL,{Pid,std_error,['$sysErrorLogger'|Rep]}}},
	    IOMod) ->
    T = write_time(maybe_utc(Time)),
    S = format_report(Rep),
    sysInitLogDisk:write_event(T ++ S ++ add_node("", Pid) ++"=END ===\n"),
    format(IOMod, T ++ S ++ add_node("", Pid)++"=END ===\n");
write_event({Time, {info_report, _GL, {Pid,std_info,['$sysErrorLogger'|Rep]}}},
	    IOMod) ->
    T = write_time(maybe_utc(Time), "INFO REPORT"),
    S = format_report(Rep),
    format(IOMod, T ++ S ++ add_node("", Pid) ++"=END ===\n");
write_event({Time, {info_msg, _GL, 
		    {Pid, "~w~n"++Format, ['$sysErrorLogger'|Args]}}},
	    IOMod) ->
    T = write_time(maybe_utc(Time), "INFO REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    format(IOMod, T ++ S ++"=END ===\n");
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    format(IOMod, T ++ F ++"=END ===\n", [Format,Args])
    end;
write_event({Time, {warning_report, _GL, 
		    {Pid, std_warning, ['$sysErrorLogger'|Rep]}}},
	    IOMod) ->
    T = write_time(maybe_utc(Time), "WARNING REPORT"),
    S = format_report(Rep),
    format(IOMod, T ++ S ++ add_node("", Pid) ++"=END ===\n");
write_event({Time, {warning_msg, _GL, {Pid, "~w~n"++Format, 
				       ['$sysErrorLogger'|Args]}}},IOMod) ->
    T = write_time(maybe_utc(Time), "WARNING REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    format(IOMod, T ++ S ++"=END ===\n");
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    format(IOMod, T ++ F ++"=END ===\n", [Format,Args])
    end;

%%% **********************************************************************

write_event({Time, {error, _GL, {Pid, Format, Args}}},IOMod) ->
    T = write_time(maybe_utc(Time)),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    sysInitLogDisk:write_event(T ++ S ++"\n=END ===\n"),
	    format(IOMod, T ++ S ++"~nUse sysInitI where possible~n=END ===\n");
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    sysInitLogDisk:write_event(T ++ F++"\n=END ===\n", [Format,Args]),
	    format(IOMod, T ++ F++"~nUse sysInitI where possible~n=END ===\n", [Format,Args])
    end;
write_event({Time, {emulator, _GL, Chars}},IOMod) ->
    T = write_time(maybe_utc(Time)),
    case catch io_lib:format(Chars, []) of
	S when is_list(S) ->
	    format(IOMod, T ++ S ++"~nUse sysInitI where possible~n=END ===\n");
	_ ->
	    format(IOMod, T ++ "ERROR: ~p ~nUse sysInitI where possible~n=END ===~n", [Chars])
    end;
write_event({Time, {info, _GL, {Pid, Info, _}}},IOMod) ->
    T = write_time(maybe_utc(Time)),
    format(IOMod, T ++ add_node("~p~n",Pid) ++"~nUse sysInitI where possible~n=END ===\n",[Info]);
write_event({Time, {error_report, _GL, {Pid, std_error, Rep}}},IOMod) ->
    T = write_time(maybe_utc(Time)),
    S = format_report(Rep),
    sysInitLogDisk:write_event(T ++ S ++ add_node("", Pid)++"\n=END ===\n"),
    format(IOMod, T ++ S ++ add_node("", Pid)++"~nUse sysInitI where possible~n=END ===\n");
write_event({Time, {info_report, _GL, {Pid, std_info, Rep}}},IOMod) ->
    T = write_time(maybe_utc(Time), "INFO REPORT"),
    S = format_report(Rep),
    format(IOMod, T ++ S ++ add_node("", Pid) ++"~nUse sysInitI where possible~n=END ===\n");
write_event({Time, {info_msg, _GL, {Pid, Format, Args}}},IOMod) ->
    T = write_time(maybe_utc(Time), "INFO REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    format(IOMod, T ++ S ++"~nUse sysInitI where possible~n=END ===\n");
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    format(IOMod, T ++ F ++"~nUse sysInitI where possible~n=END ===\n", [Format,Args])
    end;
write_event({Time, {warning_report, _GL, {Pid, std_warning, Rep}}},IOMod) ->
    T = write_time(maybe_utc(Time), "WARNING REPORT"),
    S = format_report(Rep),
    format(IOMod, T ++ S ++ add_node("", Pid) ++"~nUse sysInitI where possible~n=END ===\n");
write_event({Time, {warning_msg, _GL, {Pid, Format, Args}}},IOMod) ->
    T = write_time(maybe_utc(Time), "WARNING REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    format(IOMod, T ++ S ++"~nUse sysInitI where possible~n=END ===\n");
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    format(IOMod, T ++ F ++"~nUse sysInitI where possible~n=END ===\n", [Format,Args])
    end;
write_event({_Time, _Error},_IOMod) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ------------------------------------------------------
%%% Misc. functions.
%%% ------------------------------------------------------
set_group_leader() ->
    case whereis(user) of
	User when is_pid(User) -> link(User), group_leader(User,self()), User;
	_                      -> false
    end.

tag_event(Event) ->    
    {os:timestamp(), Event}.

write_events(Events,IOMod) -> write_events1(lists:reverse(Events),IOMod).

write_events1([Event|Es],IOMod) ->
    write_event(Event,IOMod),
    write_events1(Es,IOMod);
write_events1([],_IOMod) ->
    ok.

maybe_utc(Time) ->
    UTC = case application:get_env(sasl, utc_log) of
              {ok, Val} -> Val;
              undefined ->
                  %% Backwards compatible:
                  case application:get_env(stdlib, utc_log) of
                      {ok, Val} -> Val;
                      undefined -> false
                  end
          end,
    maybe_utc(Time, UTC).

maybe_utc(Time, true) -> {utc, Time};
maybe_utc(Time, _) -> {abs, Time}.
%% maybe_utc(Time, _) -> {local, calendar:universal_time_to_local_time(Time)}.

format(IOMod, String)       -> format(IOMod, String, []).
format(io_lib, String, Args) -> io_lib:format(String, Args);
format(io, String, Args) -> io:format(user, String, Args).

format_report(Rep) ->
    Depth = sysInitI:get_logger_depth(),
    format_report(Rep, Depth).

format_report(Rep, Depth) when is_list(Rep) ->
    case string_p(Rep) of
        true ->
            io_lib:format("~s~n",[Rep]);
        _ when Depth == unlimited ->
            format_rep(Rep);
        _ ->
            format_rep(Rep, Depth)
    end;

format_report(Rep, unlimited) ->
    io_lib:format("~p~n",[Rep]);

format_report(Rep, Depth) ->
    io_lib:format("~P~n",[Rep, Depth]).

format_rep([{Tag,Data}|Rep]) ->
    io_lib:format("    ~p: ~p~n",[Tag,Data]) ++ format_rep(Rep);
format_rep([Other|Rep]) ->
    io_lib:format("    ~p~n",[Other]) ++ format_rep(Rep);
format_rep(_) ->
    [].

format_rep([{Tag,Data}|Rep], Depth) ->
    io_lib:format("    ~P: ~P~n",[Tag, Depth, Data, Depth]) ++ format_rep(Rep, Depth);
format_rep([Other|Rep], Depth)  ->
    io_lib:format("    ~P~n",[Other, Depth]) ++ format_rep(Rep, Depth);
format_rep(_, _Depth) ->
    [].

add_node(X, Pid) when is_atom(X) ->
    add_node(atom_to_list(X), Pid);
add_node(X, Pid) when node(Pid) =/= node() ->
    lists:concat([X,"** at node ",node(Pid)," **~n"]);
add_node(X, _) ->
    X.

string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
    case string_p1(H) of
	true -> string_p1(T);
	_    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.

write_time(Time) -> write_time(Time, "ERROR REPORT").
write_time({utc,Now},Type) ->
    {{Y,Mo,D},{H,Mi,S}} = calendar:now_to_datetime(Now),
    Frac = element(3, Now),
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s.~6..0w UTC ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S),Frac]);
write_time({abs, Now},Type) ->
    {{Y,Mo,D},{H,Mi,S}}=calendar:now_to_local_time(Now),
    Diff = time_offset(Now),
    Frac = element(3, Now),
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s.~6..0w ~s ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S),Frac,Diff]).
%% write_time({local, Now},Type) ->
%%     {{Y,Mo,D},{H,Mi,S}} = calendar:now_to_local_time(Now),
%%     Frac = element(3, Now),
%%     io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s.~6..0w LT ===~n",
%% 		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S), Frac]).

%% Borrowed from comsaLib
time_offset(Now) ->
    DT = calendar:now_to_local_time(Now),
    UTC = calendar:now_to_universal_time(Now),
    DiffSecs = calendar:datetime_to_gregorian_seconds(DT) -
        calendar:datetime_to_gregorian_seconds(UTC),
    [Sign, DH, DM] = diff(DiffSecs),
    lists:append([[Sign], padzero(DH), ":", padzero(DM)]).

diff(Secs) ->
    case calendar:seconds_to_daystime(Secs) of
        {0, {H, M,_}} ->
                [$+, H, M];
        {-1, _} ->
                {0, {H, M, _}} = calendar:seconds_to_daystime(-Secs),
                [$-, H, M]
    end.
padzero(N) ->
    if N<10 -> [$0, N+$0];
       true -> integer_to_list(N)
    end.


t(X) when is_integer(X) ->
    t1(integer_to_list(X)).
%% t(_) ->
%%     "".
t1([X]) -> [$0,X];
t1(X)   -> X.

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".
