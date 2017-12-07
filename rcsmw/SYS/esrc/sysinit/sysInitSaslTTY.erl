%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysInitSaslTTY.erl %
%%% @author etomist
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R4A/R9A/R10A/R11A/3

%%% @doc ==Header==
%%%
%%% A handler that can be connected to the error_logger
%%% event handler.
%%% Writes all sasl_* events formatted to stdout.
%%%
%%% @end

-module(sysInitSaslTTY).
-vsn('/main/R2A/R4A/R9A/R10A/R11A/3').
-date('2017-09-07').
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
%%% R2A/1      2013-11-22 etxjotj     Created
%%% R4A/1      2015-09-07 etxtory     sysInitLogDisk calls
%%% R9A/1      2017-03-24 etomist     HV73396
%%% R11A/1     2017-09-05 etomist     HV82742
%%% R11A/3     2017-09-07 etomist     HV82742 - fix after SDC
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 code_change/3, terminate/2]).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCFTIONS
%%% #---------------------------------------------------------
init(Type) ->
% should link to user (or group_leader???)
    {ok, Type}.
    
handle_event({_Type, GL, _Msg}, Type) when node(GL) /= node() ->
    {ok, Type};
handle_event({MsgType, GL, {Pid, Format0, Args0}} = Event, Type) ->
    case Format0 of
        X when is_atom(X) ->
            write_report(standard_io, Type, tag_event(Event));
        _ ->
            Depth = sysInitI:get_logger_depth(),
            {Format1, Args1} =
            case {Format0, Args0} of
                {"~w~n" ++ _, ['$sysErrorLogger' | _]} -> {Format0, Args0};
                {_, ['$sysErrorLogger' | L]} -> {Format0, L};
                _ -> {Format0, Args0}
            end,
            Format2 = io_lib:scan_format(Format1, Args1),
            Format3 = limit_format(Format2, Depth),
            {Format, Args} = io_lib:unscan_format(Format3),
            FormattedEvent = {MsgType, GL, {Pid, Format, Args}},
            write_report(standard_io, Type, tag_event(FormattedEvent))
        end,
    {ok, Type};
handle_event(_E, T) ->
    {ok, T}.

handle_info(_, Type) -> {ok, Type}.

handle_call(_Query, _Type) -> {error, bad_query}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_Reason, _Type) ->
    [].

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
tag_event(Event) ->    
    {os:timestamp(), Event}.

%%% Here begins sasl_report
write_report(Fd, What, Report) ->
    io_report(io, Fd, What, Report).

io_report(IO, Fd, What, {Time, {error_report, _GL, {Pid, Type, Report}}}) ->
    case is_my_error_report(What, Type) of
	true ->
	    Head = write_head(Type, Time, Pid),
	    write_report2(IO, Fd, Head, Type, Report);
	_ -> true
    end;
io_report(IO, Fd, What, {Time, {info_report, _GL, {Pid, Type, Report}}}) ->
    case is_my_info_report(What, Type) of
	true ->
	    Head = write_head(Type, Time, Pid),
	    write_report2(IO, Fd, Head, Type, Report);
	_ -> true
    end;
io_report(_IO, _Fd, _, _) ->
    false.

%% is_my_error_report(all, Type)   ->  is_my_error_report(Type);
%% is_my_error_report(error, Type) ->  is_my_error_report(Type);
%% is_my_error_report(_, _Type)    ->  false.
%% is_my_error_report(supervisor_report)   -> true;
%% is_my_error_report(crash_report)        -> true;
%% is_my_error_report(_)                   -> false.

%% is_my_info_report(all, Type)      -> is_my_info_report(Type);
%% is_my_info_report(progress, Type) -> is_my_info_report(Type);
%% is_my_info_report(_, _Type)       -> false.
%% is_my_info_report(progress)  -> true;
%% is_my_info_report(_)                    -> false.

is_my_error_report(_, supervisor_report) ->
    true;
is_my_error_report(_, crash_report) ->
    true;
is_my_error_report(_, progress) ->
    true;
is_my_error_report(_, _) ->
    false.

is_my_info_report(_, supervisor_report) ->
    true;
is_my_info_report(_, crash_report) ->
    true;
is_my_info_report(_, progress) ->
    true;
is_my_info_report(_, _) ->
    false.

write_report2(IO, Fd, Head, supervisor_report, Report) ->
    Depth = sysInitI:get_logger_depth(),
    Name = sup_get(supervisor, Report),
    Context = sup_get(errorContext, Report),
    Reason = sup_get(reason, Report),
    Offender = sup_get(offender, Report),
    {FmtString, WriteParams} =
    case Depth of
    unlimited ->
        {"     Supervisor: ~p~n     Context:    ~p~n     Reason:     "
         "~80.18p~n     Offender:   ~80.18p~n~n=END ===~n",
         [Name,Context,Reason,Offender]};
    _ ->
        {"     Supervisor: ~P~n     Context:    ~P~n     Reason:     "
         "~80.18P~n     Offender:   ~80.18P~n~n=END ===~n",
         [Name,Depth,Context,Depth,Reason,Depth,Offender,Depth]}
    end,
    sysInitLogDisk:write_event(Head ++ FmtString, WriteParams),
    write_report_action(IO, Fd, Head ++ FmtString, WriteParams);

write_report2(_IO, _Fd, _Head, progress, [{supervisor, _}|_]) ->
    %% We don't want supervisor progress reports because they take up too
    %% much space
    %% Pid = proplists:get_value(pid, Data),
    %% Name = proplists:get_value(name, Data),
    %% Report = [{supervisor, Sup}, {pid, Pid}, {name, Name}],
    %% Format = format_key_val(Report),
    %% write_report_action(IO, Fd, Head ++ "~s=END ===~n", [Format]);
    ok;    
write_report2(IO, Fd, Head, progress, Report) ->
    Format = format_key_val(Report),
    write_report_action(IO, Fd, Head ++ "~s=END ===~n", [Format]);
write_report2(IO, Fd, Head, crash_report, Report) ->
    Format = proc_lib:format(Report),
    sysInitLogDisk:write_event(Head ++ "~s=END ===~n", [Format]),
    write_report_action(IO, Fd, Head ++ "~s=END ===~n", [Format]).

write_report_action(io, Fd, String, Args) ->
    Depth = sysInitI:get_logger_depth(),
    case Depth of
        unlimited ->
            io:format(Fd, String, Args);
        _ ->
            Format = io_lib:scan_format(String, Args),
            Format1 = limit_format(Format, Depth),
            {F, A} = io_lib:unscan_format(Format1),
            io:format(Fd, F, A)
    end.

limit_format([#{control_char:=C0}=M0|T], Depth) when C0 =:= $p;
             C0 =:= $w ->
    C = C0 - ($a - $A), %To uppercase.
    #{args:=Args} = M0,
    M = M0#{control_char:=C,args:=Args++[Depth]},
    [M|limit_format(T, Depth)];
limit_format([H|T], Depth) ->
    [H|limit_format(T, Depth)];
limit_format([], _) ->
    [].

format_key_val(Report) ->
    Depth = sysInitI:get_logger_depth(),
    case Depth of
    unlimited ->
        format_key_val2(Report);
    _ ->
        format_key_val2(Report, Depth)
    end.

format_key_val2([{Tag,Data}|Rep]) ->
    io_lib:format("    ~16w: ~p~n",[Tag,Data]) ++ format_key_val2(Rep);
format_key_val2(_) ->
    [].
format_key_val2([{Tag,Data}|Rep], Depth) ->
    io_lib:format("    ~16W: ~P~n",[Tag,Depth,Data,Depth]) ++ format_key_val2(Rep, Depth);
format_key_val2(_, _Depth) ->
    [].

sup_get(Tag, Report) ->
    case lists:keysearch(Tag, 1, Report) of
	{value, {_, Value}} ->
	    Value;
	_ ->
	    ""
    end.

maybe_utc(Time) ->
    case application:get_env(sasl,utc_log) of
	{ok,true} ->
	    {utc, Time};
	_ ->
	    {abs, Time}
    end.

    %% 	    case calendar:local_time_to_universal_time_dst(Time) of
    %% 		[UTC] ->
    %% 		    {utc,UTC};
    %% 		[UTC1,_UTC2] ->
    %% 		    {utc,UTC1};
    %% 		[] -> % should not happen
    %% 		    Time
    %% 	    end;
    %% 	_ ->
    %% 	    Time
    %% end.

write_head(supervisor_report, Time, Pid) ->
    write_head1("SUPERVISOR REPORT", maybe_utc(Time), Pid);
write_head(crash_report, Time, Pid) ->
    write_head1("CRASH REPORT", maybe_utc(Time), Pid);
write_head(progress, Time, Pid) ->
    write_head1("PROGRESS REPORT", maybe_utc(Time), Pid).

write_head1(Type, {utc,Now}, Pid) when node(Pid) /= node() ->
    {{Y,Mo,D},{H,Mi,S}} = calendar:now_to_datetime(Now),
    Frac = element(3, Now),
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s.~6..0w UTC (~p) ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S),Frac,node(Pid)]);
write_head1(Type, {utc,Now}, _) ->
    {{Y,Mo,D},{H,Mi,S}} = calendar:now_to_datetime(Now),
    Frac = element(3, Now),
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s.~6..0w UTC ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S),Frac]);
write_head1(Type, {abs, Now}, Pid) when node(Pid) /= node() ->
    {{Y,Mo,D},{H,Mi,S}}=calendar:now_to_local_time(Now),
    Diff = time_offset(Now),
    Frac = element(3, Now),
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s.~6..0w ~s (~p) ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S),Frac,Diff,node(Pid)]);
write_head1(Type, {abs, Now}, _) ->
    {{Y,Mo,D},{H,Mi,S}}=calendar:now_to_local_time(Now),
    Diff = time_offset(Now),
    Frac = element(3, Now),
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s.~6..0w ~s ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S),Frac,Diff]).

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
