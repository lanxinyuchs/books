%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaLib.erl %
%%% @author eivomat
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R11A/R12A/2
%%%
%%% @doc == COMSA LIBRARY ==
%%% Contains a number of library functions for COMSA

-module(comsaLib).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R11A/R12A/2').
-date('2017-12-06').
-author(etxjotj).
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/1      2012-01-10 etxbjca     Created
%%% -----      ---------  --------    ------------------------
%%% R2A/11     2013-05-14 etxarnu     Changed to
%%%                                   comte:register_transaction_server_callback
%%%                                   Removed register_callback_old
%%% R2A/12     2013-05-29 etxarnu  Call comte:stop_com/0 in stop_com
%%% R2A/14     2013-06-13 etxarnu  Removed calls to comsaServer:set_adm_state
%%% R2A/15     2013-06-17 etxpeno  Handle timeOfLastStatusUpdate in
%%%                                update_progress/2
%%% R2A/16     2013-09-24 erarafo  Support for CLI extension
%%% R2A/17     2013-10-02 etxpeno  Correction in register_callback/2
%%% R2A/19     2013-11-12 etxarnu  Added timeout to stop_com
%%% R2A/20     2013-11-21 etxpejn  Added support for alarm buffering
%%% R2A/22     2014-04-25 etxjotj  Removed ECIM_CommonLibrary reference
%%% R2A/23     2014-04-29 etxtory  Added update of CLI welcome text
%%% R2A/24     2014-06-25 etxpejn  Added coli_cli_welcome_text/1
%%% -----      ---------  --------    ------------------------
%%% R3A/1      2014-10-21 etxarnu  Retry comte:stop_com if fail
%%% R3A/2      2015-05-12 etxjotj  ESI
%%% -----      ---------  --------  ------------------------
%%% R4A/1      2015-07-22 etxjotj  Clean disk
%%% -----      ---------  --------  ------------------------
%%% R6A/1      2016-08-17 etxpejn  Added iso_time for datetime format
%%% -----      ---------  --------  ------------------------
%%% R7A/1      2016-09-23 etxpejn  Added server, trap & ntpd info at esi collection
%%% R7A/2      2016-09-26 etxpejn  Only fetch esi at target
%%% ----------------------------------------------------------
%%% R11A/1     2017-10-03 etxpejn  Only fetch esi at target, vrcs and core
%%% ----------------------------------------------------------
%%% R12/1      2017-11-30 eivomat  HW48644
%%% R12/2      2017-12-04 eivomat  HW48860
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% COMSA API
-export([register_callback/2]).
%%-export([register_callback_old/2]).
-export([iso_time/2, time_offset/1]).
-export([register_mims_and_cli_exts/2, update_progress/2]).
-export([start_com/1, stop_com/0]).

%% COMSA INTERNAL
-export([register_role_callback/1, get_callback/1,
	 register_transaction_server/1,
	 register_imm_global_callback/1]).
-export([comte_log_dir/0]).
-export([generate_esi/0, esi_dir/0]).
-export([clean_disk/1]).

%% RCS-COLI
-export([coli_cli_welcome_text/1]).
-export([coli_change_cli_welcome_text/1]).
-export([coli_print_cli_welcome_text/1]).

-export([get_variable/1, set_variable/2, erase_variable/1]).

%% COM VERSION DISK PRINTOUT
-export([write_com_version/1, write_com_version/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RcsSysM.hrl").

-define(COM_VERSION_LOG, "com_version_log").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Register a data callback module
%%%
%%% Used by comsaI
%%% Input: ClassPath: Path from root to the registered class
%%%        CbModule:  Callback module
%%% @end
%%% ----------------------------------------------------------
register_callback([], _CbModule) ->
    sysInitI:error_msg("~p:register_callback()~n"
			   "An empty object path cannot be registered~n",
			  [?MODULE]);
register_callback(ObjectPath, CbModule) when is_binary(hd(ObjectPath)) ->
    mnesia:dirty_write({comsaCallbacks, lists:reverse(ObjectPath), CbModule});

register_callback(ObjectPath, CbModule) ->
    Bin = bin(ObjectPath),
    register_callback(Bin, CbModule).

bin(ObjectPath) ->
    [list_to_binary(X)||X<-ObjectPath].

%%% ----------------------------------------------------------
%%% @doc Retrieve a data callback module for a specific DN
%%%
%%% Used by comsaTransactionServer
%%% Input:  Dn - A Distinguished name as a list of binaries
%%% Output: A callback module
%%% Exception: error:no_cb_registered
%%% @end
%%% ----------------------------------------------------------

get_callback(Dn) ->
    Mo = lists:reverse(strip_indexes(lists:reverse(Dn))),
    case case mnesia:is_transaction() of
	     true ->
		 do_get_callback(Mo);
	     false ->
		 mnesia:async_dirty(fun() -> do_get_callback(Mo) end)
	 end of
	{ok, Cb} ->
	    Cb;
	{error, not_found} ->
	    erlang:error(no_cb_registered, [Dn])
    end.

do_get_callback([]) ->
    {error, not_found};
do_get_callback(Mo) ->
    case mnesia:read({comsaCallbacks, Mo}) of
	[{comsaCallbacks, Mo, Cb}] ->
	    {ok, Cb};
	[] ->
	    do_get_callback(tl(Mo))
    end.

strip_indexes([Class, _|Dn]) ->
    [Class|strip_indexes(Dn)];
strip_indexes([Class]) ->
    [Class];
strip_indexes([]) ->
    [].

%%% ----------------------------------------------------------
%%% @doc Convert an os:timestamp() tuple to an ISO 8601 string
%%%
%%% Input: Now  - An os:timestamp() tuple or a datetime tuple
%%%        Type - basic|extended|extended_zonefree
%%% Output: string()
%%% @end
%%% ----------------------------------------------------------
iso_time({{_Y,_M,_D}, {_H,_Mi,_S}} = DateTime, Type) ->
    %% DateTime to now
    DiffSecs = calendar:datetime_to_gregorian_seconds(DateTime) -
	calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    Now = {DiffSecs div 1000000, DiffSecs rem 1000000, 0},
    iso_time(Now, Type);
iso_time(Now, Type) ->
    fn_date(Now, Type)++"T"++fn_time(Now, Type).

time_offset(Now) ->
    DT = calendar:now_to_local_time(Now),
    UTC = calendar:now_to_universal_time(Now),
    DiffSecs = calendar:datetime_to_gregorian_seconds(DT) -
        calendar:datetime_to_gregorian_seconds(UTC),
    [Sign, DH, DM] = diff(DiffSecs),
    lists:append([[Sign], padzero(DH), ":", padzero(DM)]).


fn_date(Now, Type) ->
    {{Y,M,D}, _} = calendar:now_to_local_time(Now),
    case Type of
        basic ->
            lists:append([integer_to_list(Y),
                          padzero(M),
                          padzero(D)]);
        Extended when Extended==extended; Extended==extended_zonefree ->
            lists:append([integer_to_list(Y), "-",
                          padzero(M), "-", padzero(D)])
    end.

fn_time(Now, Type) ->
    DT={_, {H, M, S}} = calendar:now_to_local_time(Now),
    UTC = calendar:now_to_universal_time(Now),
    DiffSecs = calendar:datetime_to_gregorian_seconds(DT) -
        calendar:datetime_to_gregorian_seconds(UTC),
    [Sign, DH, DM] = diff(DiffSecs),
    case Type of
        basic ->
            lists:append([padzero(H),
                          padzero(M),
                          padzero(S),
                          [Sign],
                          padzero(DH),
                          padzero(DM)]);
        extended ->
            lists:append([padzero(H), ":", padzero(M), ":", padzero(S),
                          [Sign], padzero(DH), ":", padzero(DM)]);
        extended_zonefree ->
            lists:append([padzero(H), ":", padzero(M), ":", padzero(S)])
    end.

padzero(N) ->
    if N<10 -> [$0, N+$0];
       true -> integer_to_list(N)
    end.


diff(Secs) ->
    case calendar:seconds_to_daystime(Secs) of
        {0, {H, M,_}} ->
                [$+, H, M];
        {-1, _} ->
                {0, {H, M, _}} = calendar:seconds_to_daystime(-Secs),
                [$-, H, M]
    end.

%%% ----------------------------------------------------------
%%% @doc Register MIMs in COM
%%%
%%% Input: Mims - [{CxpName, [MimPath]}]
%%%        CxpName :: string()
%%%        MimPath :: string() - MIM path relative to CxpName
%%%        CliSharedLibs :: [string()] - paths to *.so files
%%% @end
%%% ----------------------------------------------------------

-spec register_mims_and_cli_exts([{string(), [string()]}], [string()]) -> ok.

register_mims_and_cli_exts(Mims, CliSharedLibs) ->
    {RelMims, AbsMims} = lists:partition(fun(Mim) -> is_tuple(Mim) end, Mims),
    MimPaths = reg_mims(RelMims, []),
    comsaDataInit:install_comte(MimPaths++AbsMims, CliSharedLibs),
    ok.

reg_mims([], Acc) -> Acc;
reg_mims([{CxpName, MimPathList} | T], Acc) ->
    Mims = [filename:join(CxpName, MimPath) || MimPath <- MimPathList],
    reg_mims(T, Acc ++ Mims).

%%% ----------------------------------------------------------
%%% @doc Update the ECIM Common library AsyncActionProgress struct
%%%
%%% See comsaI for further information
%%% NOTE! I newer versions of DX ET the library types are included in each
%%% MOM, which makes it necessary to COPY this function rather than to use
%%% the interface
%%% This function supports the AsyncActionProgress struct from CommonLibrary 1.5
%%% @end
%%% ----------------------------------------------------------
-record('AsyncActionProgress', {actionName,
                                additionalInfo,
                                progressInfo,
                                progressPercentage,
                                result,
                                resultInfo,
                                state,
                                actionId,
                                timeActionStarted,
                                timeActionCompleted,
                                timeOfLastStatusUpdate}).

update_progress(Data, undefined) ->
    ProgressReport = #'AsyncActionProgress'{},
    update_progress(Data, ProgressReport);
update_progress([{actionName, Name}|Data], Progress) ->
    update_progress(Data, Progress#'AsyncActionProgress'{actionName = Name});
update_progress([{additionalInfo, Info}|Data], Progress) ->
    info_msg("~s~n~n",[Info]),
    NewAI = case Progress#'AsyncActionProgress'.additionalInfo of
		AI when is_list(AI) -> AI++[Info];
		_ -> [Info]
	    end,
    P2 = update_progress([{progressInfo, Info}], Progress),
    update_progress(Data, P2#'AsyncActionProgress'{additionalInfo = NewAI});
update_progress([{additionalInfoClear, Info}|Data], Progress) ->
    info_msg("~s~n~n",[Info]),
    P2 = update_progress([{progressInfo, Info}], Progress),
    update_progress(Data, P2#'AsyncActionProgress'{additionalInfo = [Info]});
update_progress([{progressInfo, Info}|Data], P) ->
    update_progress(Data, P#'AsyncActionProgress'{progressInfo = Info});
update_progress([{progressPercentage, Percent}|Data], P) ->
    update_progress(Data, P#'AsyncActionProgress'{progressPercentage = Percent});
update_progress([{result, Result}|Data], P) ->
    update_progress(Data, P#'AsyncActionProgress'{result = Result});
update_progress([{resultInfo, Info}|Data], P) ->
    update_progress(Data, P#'AsyncActionProgress'{resultInfo = Info});
update_progress([{state, State}|Data], P) ->
    update_progress(Data, P#'AsyncActionProgress'{state = State});
update_progress([{actionId, ActionId}|Data], P) ->
    update_progress(Data, P#'AsyncActionProgress'{actionId = ActionId});
update_progress([{timeActionStarted, Date}|Data], P) ->
    update_progress(Data, P#'AsyncActionProgress'{timeActionStarted = Date});
update_progress([{timeActionCompleted, Date}|Data], P) ->
    update_progress(Data, P#'AsyncActionProgress'{timeActionCompleted = Date});
update_progress([{timeOfLastStatusUpdate, Date}|Data], P) ->
    %% Is this correct?? //etxpeno 2013-06-17
    warning_msg("Progress header: {timeOfLastStatusUpdate, ~p}~n", [Date]),
    update_progress(Data,
		    P#'AsyncActionProgress'{timeOfLastStatusUpdate = Date});
update_progress([X|Data], P) ->
    error_msg("Unknown progress header: ~p~n",[X]),
    update_progress(Data, P);
update_progress([], P) ->
    Time = iso_time(os:timestamp(), extended),
    P#'AsyncActionProgress'{timeOfLastStatusUpdate = Time}.

%%% ----------------------------------------------------------
%%% @doc Register a callback model for role enquiries
%%%
%%% COMSA is the only actor in this. Failure causes a printout
%%% in the erlang shell.
%%% This function provides cover for errors in COMTE
%%%
%%% Input: CbModule:atom() -
%%%
%%% @end
%%% ----------------------------------------------------------

-spec register_role_callback(CbModule::atom()) -> ok.
register_role_callback(CbModule) ->
    try comte:register_role_callback(CbModule) of
	ok ->
	    ok
    catch
	Type:Error ->
	    sysInitI:error_report(
	      [{Type, Error}, erlang:get_stacktrace()])
    end.

%%% ----------------------------------------------------------
%%% @doc Register a transaction server callback module
%%%
%%% This allows the system to install another transactionserver, than
%%% the COMTE default transaction server.
%%% Failures cause a printout in the erlang shell
%%% This function provides cover for errors in COMTE
%%%
%%% Input: CbModule:atom()
%%% ----------------------------------------------------------

register_transaction_server(CbModule) ->
    try comte:register_transaction_server_callback(CbModule) of
	ok ->
	    ok
    catch
	Type:Error ->
	    sysInitI:error_report(
	      [{Type, Error}, erlang:get_stacktrace()])
    end.

%%% ----------------------------------------------------------
%%% @doc Register an imm global callback
%%% This function is used together with the comsaTransactionServer.
%%% In comsaTransactionServer is registered as the comte transaction
%%% server, this callback will provide special handling of IMM related
%%% functionality during a transaction.
%%% ----------------------------------------------------------

register_imm_global_callback(CbModule) ->
    set_variable(imm_global_callback, CbModule).

%%% ----------------------------------------------------------
%%% @doc Retrive a comsa global value from comsaVariables
%%% @end
%%% ----------------------------------------------------------

get_variable(Name) ->
    case mnesia:transaction(fun() ->
				    mnesia:read({comsaVariables, Name})
			    end) of
	{atomic, []} -> undefined;
	{atomic, [{_,_,Value}]} -> Value;
	{aborted, _} -> undefined
    end.

%%% ----------------------------------------------------------
%%% @doc Set a comsa global variable in comsaVariables
%%% @end
%%% ----------------------------------------------------------

set_variable(Name, Value)  ->
    case mnesia:transaction(fun() ->
				    mnesia:write({comsaVariables, Name, Value})
			    end) of
	{atomic, ok} -> ok;
	{aborted, R} -> {aborted, R}
    end.

%%% ----------------------------------------------------------
%%% @doc Erase a comsa global variable in comsaVariables
%%% @end
%%% ----------------------------------------------------------

erase_variable(Name) ->
    case mnesia:transaction(fun() ->
				    mnesia:delete({comsaVariables, Name})
			    end) of
	{atomic, ok} -> ok;
	{aborted, R} -> {aborted, R}
    end.

%%% ----------------------------------------------------------
%%% Start COM
%%% ----------------------------------------------------------
start_com(Opts) ->
    comte:start_com(Opts),
    comsaServer:com_started().

%%% ----------------------------------------------------------
%%% Stop COM
%%% ----------------------------------------------------------
stop_com() ->
    comsaServer:com_stopped(),
    wait_for_com_stopped(),
    ok.

wait_for_com_stopped() ->
    wait_for_com_stopped(3).

wait_for_com_stopped(0) ->
    sysInitI:error_msg("Stop_com FAILED  ~n",[]);
    
wait_for_com_stopped(N) ->

    case comte:stop_com(15000) of
	{error,Reason} ->
	    sysInitI:info_msg("Stop_com rejected , Reason ~p~n"
				  "Retrying ~n",[Reason]),
	    timer:sleep(1000),
	    wait_for_com_stopped(N-1);
	Res ->
	    sysInitI:info_msg("Stop_com succeeded, Result ~p~n",[Res]),
	    ok
    end.
	    

%%% ----------------------------------------------------------
%%% COLI CLI welcome message.
%%% ----------------------------------------------------------
coli_cli_welcome_text(["-p"]) ->
    coli_print_cli_welcome_text(["-p"]);
coli_cli_welcome_text(["-s"| WelcomeText]) ->
    coli_change_cli_welcome_text(WelcomeText);
coli_cli_welcome_text(_Args) ->
    coli_print_cli_welcome_text(["-p"]).

%%% ----------------------------------------------------------
%%% Changes the CLI welcome message.
%%% ----------------------------------------------------------
coli_change_cli_welcome_text(Message) ->
    UpMessage = filter_welcome_text(Message),
    set_variable(welcome_message, UpMessage),
    try change_com_config() of
	ok ->
	    %% Need to restart COM to activate the CLI text
	    info_msg("Welcome message changed - restarting COM~n", []),
	    stop_com(),
	    start_com([{start_com, true}]),
	    exit(ok)
    catch Type:Error -> 
	    error_msg("Cannot change Welcome message~n"
		      "Type ~p, Error ~p~n", [Type, Error]),
	    io:format("Internal error, cannot change Welcome message~n"),
	    exit(error)
    end.

%%% ----------------------------------------------------------
%%% Prints the CLI welcome text.
%%% ----------------------------------------------------------
coli_print_cli_welcome_text(_Arg) ->
    Message = comsaI:get_cli_welcome_message(),
    io:format("~s", [Message]),
    exit(ok).

%%% ----------------------------------------------------------
%%% Generate ESI
%%% ----------------------------------------------------------

esi_dir() ->
    filename:join(sysEnv:vnf_dir(), "comsa").


generate_esi() ->
    Path = filename:join(esi_dir(), "mominfo.txt"),
    filelib:ensure_dir(Path),
    {ok, Fd} = file:open(Path, [write]),
    
    io:format(Fd, "~-37s ~-37s~n", ["Identifier","Base model"]),
    io:format(Fd, "~76c~n",[$=]),
    [begin
	 io:format(Fd, "~-37s ~-37s",
		   [X#sysMSchema.identifier++" ("++
		    X#sysMSchema.version++")",
		    case X#sysMSchema.baseModelIdentifier of
			"" -> "";
			BaseModel ->
			    BaseModel++" ("++
				X#sysMSchema.baseModelVersion++")"
		    end]),
	 [io:format(Fd, "~s", [Opt])||Opt<-X#sysMSchema.selectedModelOptions],
	 io:format(Fd, "~n",[])
     end||X<-lists:keysort(#sysMSchema.identifier, ets:tab2list(sysMSchema))],
    file:close(Fd),
    
    case sysEnv:rcs_mode_2() of
	simulated -> 
	    do_nada;
	_TargetOrVrcs ->
	    case sysEnv:role() of
		active ->
		    ServerStatePath = filename:join(esi_dir(), "serverstate.txt"),
		    filelib:ensure_dir(ServerStatePath),
		    {ok, FdSS} = file:open(ServerStatePath, [write]),
		    try_print_ntp_server_state(FdSS, 2),
		    file:close(FdSS),
		    
		    NtpdPath = filename:join(esi_dir(), "ntpdinternal.txt"),
		    filelib:ensure_dir(NtpdPath),
		    {ok, FdN} = file:open(NtpdPath, [write]),
		    comsaNtpServer:print_ntpd_state(FdN),
		    file:close(FdN),
		    
		    NtpTrapPath = filename:join(esi_dir(), "ntptraps.txt"),
		    filelib:ensure_dir(NtpTrapPath),
		    {ok, FdTrap} = file:open(NtpTrapPath, [write]),
		    comsaNtpServer:print_traps(2, FdTrap),
		    file:close(FdTrap);
		_ ->
		    %% Not core DU or MW, no NTP daemon started
		    do_nada
	    end
    end.

comte_log_dir() ->
    filename:join(sysEnv:vnf_dir(), "comte").

clean_disk(major) ->
    %% Check with fuser and remove com log files that are not used
    clean_disk(minor);
clean_disk(minor) ->
    os:cmd(["rm -rf ", esi_dir()]),
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

try_print_ntp_server_state(FdSS, N) when N =< 0 ->
     io:format(FdSS, "~nCould not collect NTP server state.~n~n", []);
try_print_ntp_server_state(FdSS, N) ->
    try comsaNtpServer:print_server_state(FdSS) of
        Result -> Result
    catch
        _:_ ->
            io:format(FdSS, "~nCould not collect NTP server state due to "
                     "timeout, will try again ~p more times.~n~n", [N-1]),
            try_print_ntp_server_state(FdSS, N-1)
    end.

%%% ----------------------------------------------------------
%%% Change the Welcome message in COM configuration.
%%% ----------------------------------------------------------
change_com_config() ->
    ReleasesVsnDir = sysEnv:releases_vsn_dir(),
    ComteDir = filename:join(ReleasesVsnDir, "comte"),
    CliAgentFile = filename:join(ComteDir, "libcom_cli_agent.cfg"),
    CliAgentFileBackup = CliAgentFile ++ "_backup",
    {ok, _} = file:read_file_info(CliAgentFile),
    {ok, Xml, _} = xmerl_sax_parser:file(CliAgentFile, [{event_fun, fun sax_event/3}]),
    file:copy(CliAgentFile, CliAgentFileBackup),
    ok = file:write_file(CliAgentFile, Xml),
    %% Check the new libcom_cli_agent.cfg
    %% If it fails (should not), restore libcom_cli_agent.cfg_latest
    case catch xmerl_sax_parser:file(CliAgentFile, 
				     [{event_fun, fun sax_event/3}]) of
	{ok, _, _} ->
	    ok;
	_ -> 
	    file:copy(CliAgentFileBackup, CliAgentFile),
	    exit("Illegal chars in file")
    end.

sax_event({characters, Chars}, _, State) ->
    update_state(State, {chars, Chars});
sax_event(Event, _Location, State) -> 
    update_state(State, Event).

update_state(_, startDocument) ->
    {false, "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"};
update_state({_, Xml}, endDocument) ->
    Xml;
update_state({_, Xml}, {startElement, _, "IntroductoryMessage", _, _}) ->
    {true, Xml ++ "<" ++ "IntroductoryMessage" ++ ">"};
update_state({_, Xml}, {startElement, _, Element, _, _}) ->
    {false, Xml ++ "<" ++ Element ++ ">"};
update_state({_, Xml}, {endElement, _, Element, _}) ->
    {false, Xml ++ "</" ++ Element ++ ">"};
update_state({_, Xml}, {ignorableWhitespace, WS}) ->
    {false, Xml ++ WS};
update_state({true, Xml}, {chars, _Chars}) ->
    %% Replace existing _Chars with new Welcome message
    Message = get_variable(welcome_message),
    {false, Xml ++ Message};
update_state({false, Xml}, {chars, Chars}) ->
    {false, Xml ++ Chars};
update_state(State, Unknown) ->
    warning_msg("Welcome message - unknown element ~p~n",
		[Unknown]),
    State.

filter_welcome_text(Message) ->
    Str = args_to_string(Message, ""),
    StrNL = re:replace(Str, "\\\\n", "\n", [global, {return,list}]),
    Tokens = string:tokens(StrNL, "<&"),
    lists:flatten(Tokens).

args_to_string([], Acc) -> Acc;
args_to_string([Arg | T], "") -> args_to_string(T, Arg);
args_to_string([Arg | T], Acc) -> args_to_string(T, Acc ++ " " ++ Arg).

%%% ----------------------------------------------------------
%%% Info, warning and error printouts
%%% ----------------------------------------------------------
%info_msg(Format) ->
%    info_msg(Format, []).
info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%warning_msg(Format) ->
%    warning_msg(Format, []).
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%error_msg(Format) ->
%    error_msg(Format, []).
error_msg(Format, Args) ->
   sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% COM version printout in a separate disk file
%%% ----------------------------------------------------------
write_com_version(String, Args) ->
        write_com_version(io_lib:format(String, Args)).

write_com_version(String) ->
    Time = [iso_time(os:timestamp(),extended)],
    BinaryPrintout =
        list_to_binary(io_lib:format("~s" ++ String ++ "~n", Time)),
    case disk_log:blog(?COM_VERSION_LOG, BinaryPrintout) of
    ok ->
        ok;
    {error, no_such_log} ->
        create_com_version_log(),
        disk_log:blog(?COM_VERSION_LOG, BinaryPrintout),
        ok;
    {error, Reason} ->
        warning_msg("~p: Failed writing to ~p: ~p~n~n",
                  [?MODULE, ?COM_VERSION_LOG, Time ++ Reason]),
        ok
    end.

create_com_version_log() ->
    LogDir = filename:join([sysEnv:rcs_dir(), "erlang_disk"]),
    filelib:ensure_dir(LogDir),
    LogFn = filename:join([LogDir, ?COM_VERSION_LOG]),
    MaxSize = 20000,
    MaxNoFiles = 1,
    disk_log:open([{name, ?COM_VERSION_LOG},
                   {file, LogFn},
                   {format, external},
                   {mode, read_write},
                   {type, wrap},
                   {size, {MaxSize, MaxNoFiles}}]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
