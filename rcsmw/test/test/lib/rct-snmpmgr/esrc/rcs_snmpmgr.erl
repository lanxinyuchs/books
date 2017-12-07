%% -----------------------------------------------------------------------------
%% #0.    BASIC INFORMATION
%% -----------------------------------------------------------------------------
%% %CCaseFile:	rcs_snmpmgr.erl %
%% @author etxkols
%% @copyright Ericsson AB 2013-2017
%% @version /main/R2A/R3A/R4A/R5A/R6A/R9A/R10A/R11A/2
%% @doc == Module that starts a SNMP manager for receiving and matching traps. ==
%%
%%
%%   This module starts a SNMP manager. Used by rct_snmpmgr.erl and rcs_snmpmgr_gui.erl<br/>
%% @end
-module(rcs_snmpmgr).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R9A/R10A/R11A/2').
-date('2017-10-23').
-author('etxkols').
-export([start/0,
	 start/1,
	 stop/0,
	 init/7,
	 alarmlist/0,
	 clear_alarmlist/0,
	 alerts/1,
	 alarms/1,
	 format_output/1,
	 start_logging/1,
	 stop_logging/0,
	 wait_for_traps/3,
	 check_traps/0]).

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2015 All rights reserved.
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
%%% R2A/1      2013-05-17 etxkols     Created
%%% R2A/2      2013-05-30 etxkols     Bug fix
%%% R2A/3      2013-05-31 etxkols     Small fixes
%%% R2A/4-5    2013-06-10 etxkols     Fixes
%%% R2A/6      2013-06-10 etxkols     Changed ct:pal printout
%%% R2A/6      2013-09-26 etxkols     Handle eriAlarmAlarmListRebuilt
%%% R2A/7      2013-09-27 etxkols     Fix
%%% R2A/8      2013-11-13 etxkols     undefinded instead of undefined
%%% R2A/10     2013-11-19 erarafo     Added matching of alerts
%%% R2A/11     2013-11-21 etxkols     lastEventTime changed to EriAlarmActiveEventTime
%%% R2A/12     2013-11-21 etxkols     Fix
%%% R2A/13     2014-03-04 etxkols     Fix
%%% R2A/14     2014-08-21 etxkols     Now coldstart trap appeared on wr6
%%% R2A/16     2014-08-28 erarafo     Reading of boolean option from list
%%% R2A/17     2014-09-03 erarafo     Guarding against crashes in main loop
%%% R2A/18     2014-10-10 erarafo     Corrected mapping of alerts to compact format
%%% R3A/1      2014-10-10 erarafo     Merge of R2A/18 to R3
%%% R3A/2      2015-06-12 etxarnu     Corrected handle_msg to accept any ActiveRow
%%% R4A/1      2015-08-11 etxkols     Slightly changed format in eriAlarmAlarmListRebuilt
%%% R5A/1      2016-02-03 etxkols     ipv6
%%% R5A/2      2016-02-09 etxkols     ipv6 fixes
%%% R5A/3      2016-03_03 etxkols     ipv6 again
%%% R5A/4      2016-03_03 etxkols     A better ipv6 solution
%%% R5A/5      2016-04_05 etxkols     Yocto changed handle_inform format for ipv6
%%% R9A/1      2017-02_27 etxkols     New eriChangeIPAddressEvent 
%%% R10A/1     2017-05_09 etxkols     New return_received_traps option to wait_for_traps/3 
%%% R11A/1     2017-08_28 etxkols     New ERICSSON-ALARM-MIB.mib & ERICSSON-ALARM-TC-MIB.mib
%%% R11A/2     2017-10_23 etxkols     Support eriAlarmAppendInfo
%%% ----------------------------------------------------------
-define(DEFAULT_MIB_DIR,"/home/etxkols/RCS/snmp/mibs"). % Directory with mibs
-define(DEFAULT_SNMP_DIR,"/home/etxkols/RCS/snmp/mgr"). % mkdir /home/test/snmp (Dir where OTP snmp application will store its files)
-define(DEFAULT_MGR_PORT, 27162).
-define(MANAGER_CONF,[{engine_id,"RCS manager engine"},
                      {max_message_size,8192}]).

-define(eriAlarmNotifsPrefix,1,3,6,1,4,enterprises,ericsson,ericssonModules,ericssonAlarmMIB,eriAlarmNotifications,eriAlarmNotifsPrefix).
-define(eriAlarmActiveAlarms,1,3,6,1,4,enterprises,ericsson,ericssonModules,ericssonAlarmMIB,eriAlarmObjects,eriAlarmActiveAlarms).
-define(eriAlarmNotifObjects,1,3,6,1,4,enterprises,ericsson,ericssonModules,ericssonAlarmMIB,eriAlarmObjects,eriAlarmNotifObjects).
-define(eriAlarmAlerts,1,3,6,1,4,enterprises,ericsson,ericssonModules,ericssonAlarmMIB,eriAlarmObjects,eriAlarmAlerts).
-define(eriChangeIPAddressEvent,1,3,6,1,4,enterprises,ericsson,ericssonModules,ericssonChangeIPAddressMIB,eriChangeIPAddressNotifications,eriChangeIPAddressNotifsPrefix).
-define(eriChangeIPAddressObjects,1,3,6,1,4,enterprises,ericsson,ericssonModules,ericssonChangeIPAddressMIB,eriChangeIPAddressObjects).

%%===========================================================================
%% @spec start() -> {ok, xmgr_started} | {ok, already_started} | {error,Reason}
%% @doc Same as rcs_snmpmgr:start([]).
%% @end
%%===========================================================================
start() ->
    start([]).

%%===========================================================================
%% @spec start(Opts) -> {ok, xmgr_started} | {ok, already_started} | {error,Reason}
%% Opts      = [Opt]
%% Opt       = {addresses, Addresses} | {port, Port} | {logfile, no_logfile | string()} | {snmpdir, string()} | {mibdir, string()} | {format_output, true | false}
%% Addresses = [Address]
%% Address   = {integer(),integer(),integer(),integer()}
%% Port      = integer()
%% @doc Start SNMP manager.<br/>
%% Opts:
%% ```
%% Addresses     Manager will listen for traps from Addresses
%% Port          Manager will receive traps on Port
%% logfile       All received traps can be logged to logfile
%% snmpdir       OTP SNMP application needs to store its files in a directory
%% mibdir        Directory where MIB files are stored
%% format_output Received traps can be printed in a oneline(default) or [{Key, Value}] format.'''
%% @end
%%===========================================================================
start(Opts) ->
    case whereis(?MODULE) of
        undefined ->
	    IPs          = proplists:get_value(addresses,Opts,[]),
	    Port         = proplists:get_value(port,Opts,?DEFAULT_MGR_PORT),
	    LogFile      = proplists:get_value(logfile,Opts,no_logfile),
	    SNMPDir      = proplists:get_value(snmpdir,Opts,?DEFAULT_SNMP_DIR),
	    MIBDir       = proplists:get_value(mibdir,Opts,?DEFAULT_MIB_DIR),
	    FormatOutput = proplists:get_value(format_output,Opts,true),
	    Transports   = case proplists:get_value(transports,Opts) of
			       {IPv4,[]} ->
				   {transports,[{transportDomainUdpIpv4,{IPv4,Port}}]};
			       {IPv4,IPv6} ->
				   {transports,[{transportDomainUdpIpv4,{IPv4,Port}},
						{transportDomainUdpIpv6,{IPv6,Port}}]};
			       _ ->
				   transports(Port)
			   end,					
	    io:format("MIBDir ~p",[MIBDir]),
	    case xmgr:compile_mibs(MIBDir) of
		ok ->
		    register(?MODULE,Pid = spawn(?MODULE,init,[self(), 
							       IPs, 
							       [{port,Port},
								Transports
								|?MANAGER_CONF], SNMPDir, MIBDir, LogFile, FormatOutput])),
		    receive
			{Pid, xmgr_started} ->
			    {ok, xmgr_started}
		    after 30000 ->
			    {error, timeout_starting_xmgr}
		    end;
		nok ->
		    {error, could_not_compile_mibs}
	    end;
	_ -> {ok, already_started}
    end.

transports(Port) ->
    {ok,IFs} = inet:getifaddrs(),
    [Items] = [Items||{"eth0",Items}<-IFs],
    [{_,_,_,Last}=IPv4] = [IPv4||{addr,IPv4={_,_,_,_}}<-Items],
    Hex = list_to_integer(integer_to_list(Last),16),
    case [IPv6||{addr,IPv6={IP1,_,_,_,_,_,_,IP8}}<-Items,IP1 == 16#2001, IP8 == Hex] of
	[IPv6] ->
	    {transports,[{transportDomainUdpIpv4,{IPv4,Port}},
			 {transportDomainUdpIpv6,{IPv6,Port}}]};
	[] ->
	    {transports,[{transportDomainUdpIpv4,{IPv4,Port}}]}
    end.
    %% case [IPv6||{addr,IPv6={IP1,_,_,_,_,_,_,_}}<-Items,IP1 == 16#2001] of
    %% 	[_,IPv6] ->
    %% 	    {transports,[{transportDomainUdpIpv4,{IPv4,Port}},
    %% 			 {transportDomainUdpIpv6,{IPv6,Port}}]};
    %% 	[IPv6] ->
    %% 	    {transports,[{transportDomainUdpIpv4,{IPv4,Port}},
    %% 			 {transportDomainUdpIpv6,{IPv6,Port}}]};
    %% 	[] ->
    %% 	    {transports,[{transportDomainUdpIpv4,{IPv4,Port}}]}
    %% end.

%%===========================================================================
%% @spec stop() -> ok
%% @doc Stops SNMP manager.
%% @end
%%===========================================================================
stop() ->
    case whereis(alarmlist_gui) of
	AlarmGUI when is_pid(AlarmGUI) ->
	    AlarmGUI ! stop;
	_ ->
	    ok
    end,
    catch(?MODULE ! stop).

%%===========================================================================
%% @spec alarmlist() -> {ok,[[{Key, Value}]]} | {error, mgr_not_started}
%% @doc Returns current alarmlist.
%% @end
%%===========================================================================
alarmlist() ->
    send({self(),alarmlist}).

%%===========================================================================
%% @spec clear_alarmlist() -> ok | {error, mgr_not_started}
%% @doc Clears alarmlist.
%% @end
%%===========================================================================
clear_alarmlist() ->
    send({self(),clear_alarmlist}).

%%===========================================================================
%% @spec alerts(yes | no) -> ok | {error, mgr_not_started}
%% @doc Enables or disables printing of alerts in shell.
%% @end
%%===========================================================================
alerts(YesNo) ->
    send({self(),{alerts,YesNo}}).

%%===========================================================================
%% @spec alarms(yes | no) -> ok | {error, mgr_not_started}
%% @doc Enables or disables printing of alarms in shell.
%% @end
%%===========================================================================
alarms(YesNo) ->
    send({self(),{alarms,YesNo}}).

%%===========================================================================
%% @spec format_output(true | false) -> ok | {error, mgr_not_started}
%% @doc Specifies format to print trap in shell or logfile.<br/>
%% If true, oneline format is printed. <br/>
%% If false [{Key, Value}] format is printed
%% @end
%%===========================================================================
format_output(TrueFalse) ->
    send({self(),{format_output,TrueFalse}}).

%%===========================================================================
%% @spec start_logging(FD) -> ok | {error, mgr_not_started}
%% FD = fd()
%% @doc Start logging traps to open filedescriptor FD
%% @end
%%===========================================================================
start_logging(FD) ->
    send({self(),{start_logging,FD}}).

%%===========================================================================
%% @spec stop_logging() -> ok | {error, mgr_not_started}
%% @doc Stop logging to file
%% @end
%%===========================================================================
stop_logging() ->
    send({self(),stop_logging}).

send(Msg) ->
    case whereis(?MODULE) of
	Pid when is_pid(Pid) ->
	    ?MODULE ! Msg,
	    receive
		{Pid,Reply} ->
		    Reply
	    end;
	undefined ->
	    {error, mgr_not_started}
    end.

wait_for_traps(Traps, Opts, Time) ->
    case whereis(?MODULE) of
	undefined -> {nok,snmp_manager_not_started};
	Pid when is_pid(Pid) ->
	    ?MODULE ! {self(), wait_for_traps, Traps, Opts},
	    case proplists:get_bool(wait, Opts) of
		true ->
		    check_traps(Pid, Opts, s_now() + Time);
		false ->
		    put(wait_for_traps, true),
		    put(opts, Opts),
		    put(times, s_now() + Time),
		    ok
	    end
    end.

check_traps() ->
    case whereis(?MODULE) of
	undefined -> {nok,snmp_manager_not_started};
	Pid when is_pid(Pid) ->
	    case get(wait_for_traps) of
		true ->
		    Opts = get(opts),
		    Time = get(times),
		    check_traps(Pid, Opts, Time);
		_ ->
		    {error, wait_for_traps_not_active}
	    end
    end.

check_traps(Pid, Opts, Time) ->
    timer:sleep(1000),
    Pid ! {self(), get_trap_result},
    receive
	{Pid, [], AllRecTrapData} -> % All traps received
	    put(wait_for_traps, undefined),
	    ?MODULE ! remove_traps,
	    case proplists:get_bool(return_received_traps, Opts) of
		false -> ok;
		true  -> {ok, AllRecTrapData}
	    end;
	{Pid, {error, Reason}} ->
	    put(wait_for_traps, undefined),
	    ?MODULE ! remove_traps,
	    {error, Reason};
	{Pid, TrapRemain, _AllRecTrapData} ->
	    case s_now() > Time of
		true ->
		    ?MODULE ! remove_traps,
		    print_remaining_traps(TrapRemain, Opts),
		    put(wait_for_traps, undefined),
		    {error,timeout_waiting_for_traps};
		false ->
		    check_traps(Pid, Opts, Time)
	    end
    end.

init(From, IPs, ManagerConf, SNMPDir, MIBDir, LogFile, FormatOutput) ->
    process_flag(trap_exit,true),
    ConfDir = filename:join(os:getenv("HOME"),SNMPDir),
    put(alarms,yes),
    put(alerts,yes),
    put(format_output,FormatOutput),
    ets:new(current_alarmlist,[duplicate_bag,public,named_table]),
    ets:new(extra_cleardalarms,[duplicate_bag,public,named_table]),
    ets:new(handle_resent_informs,[ordered_set,public,named_table]),
    case xmgr:start(?MODULE,ConfDir,ManagerConf,{rcs_user,[{inet:ntoa(IP),IP}||IP<-IPs]}, [{translate,yes},{load_mibs,MIBDir}]) of
        {ok,started} ->
	    From ! {self(), xmgr_started},
            loop_snmpm([], [], LogFile, []);
%            loop_snmpm(no_traps, no_opts, no_result, LogFile);
        Other ->
            Other
    end.

%% make_string([]) ->
%%     [];
%% make_string(T) when is_tuple(T) ->
%%     make_string(tuple_to_list(T));
%% make_string(L) ->
%%     string:strip(lists:flatten([integer_to_list(X)++"."||X<-L]),right,$.).


loop_snmpm(WaitTraps, Opts, LogFile, AllRecTrapData) ->
    remove_old_informs(),
    receive
        stop ->
            catch(xmgr ! stop);
	{Pid,alarmlist} ->
	    Pid ! {self(), {ok,ets:tab2list(current_alarmlist)}},
	    loop_snmpm(WaitTraps, Opts, LogFile, AllRecTrapData);
	{Pid,clear_alarmlist} ->
	    ets:delete_all_objects(current_alarmlist),
	    Pid ! {self(), ok},
	    check_gui(),
	    loop_snmpm(WaitTraps, Opts, LogFile, AllRecTrapData);
	{Pid,{alerts,YesNo}} ->
	    Pid ! {self(), ok},
	    put(alerts,YesNo),
	    loop_snmpm(WaitTraps, Opts, LogFile, AllRecTrapData);
	{Pid,{alarms,YesNo}} ->
	    Pid ! {self(), ok},
	    put(alarms,YesNo),
	    loop_snmpm(WaitTraps, Opts, LogFile, AllRecTrapData);
	{Pid,{format_output,TrueFalse}} ->
	    Pid ! {self(), ok},
	    put(format_output,TrueFalse),
	    loop_snmpm(WaitTraps, Opts, LogFile, AllRecTrapData);
	{Pid,{start_logging,FD}} ->
	    Pid ! {self(), ok},
	    loop_snmpm(WaitTraps, Opts, FD, AllRecTrapData);
	{Pid,stop_logging} ->
	    Pid ! {self(), ok},
	    loop_snmpm(WaitTraps, Opts, no_logfile, AllRecTrapData);
	{_Pid, wait_for_traps, WaitTraps2, Opts2} ->
	    loop_snmpm(WaitTraps2, Opts2, LogFile, []);
	remove_traps ->
	    loop_snmpm([], Opts, LogFile, []);
	{Pid, get_trap_result} ->
	    Pid ! {self(), WaitTraps, AllRecTrapData},
	    loop_snmpm(WaitTraps, Opts, LogFile, AllRecTrapData);
	{handle_inform,AgentAddr,
	 {noError,0,[{varbind,[1,3,6,1,2,'mib-2',system,sysUpTime,0],'TimeTicks',_,1},
		     {varbind,[1,3,6,1,6,snmpModules,snmpMIB,snmpMIBObjects,snmpTrap,snmpTrapOID,0],
		      'OBJECT IDENTIFIER',[1,3,6,1,6,snmpModules,snmpMIB,snmpMIBObjects,snmpTraps,coldStart],2}]},[]} ->
 	    ets:delete_all_objects(current_alarmlist),
	    io:format("~s TRAP: ~-15s COLDSTART Clearing alarmlist~n~n",[make_time_string(),AgentAddr]),
	    check_gui(),
	    loop_snmpm(WaitTraps, Opts, LogFile, AllRecTrapData ++ []);
	{handle_inform,AgentAddr,RecTrap,_} ->
	    {WaitTraps2, RecTrapData} =
		try
		    handle_inform(AgentAddr,RecTrap,WaitTraps, Opts, LogFile)
		catch
		    ExType:ExData ->
			S = [{stack, Item} || Item <- erlang:get_stacktrace()],
			error_logger:error_report([{handle_inform, {ExType, ExData}}|S]),
			{WaitTraps, []}
		end,
	    loop_snmpm(WaitTraps2, Opts, LogFile, AllRecTrapData ++ RecTrapData);
	{handle_trap,AgentAddr,RecTrap,_} ->
	    {WaitTraps2, RecTrapData} =
		try
		    handle_msg(AgentAddr,RecTrap,WaitTraps, Opts, LogFile, false)
		catch
		    ExType:ExData ->
			S = [{stack, Item} || Item <- erlang:get_stacktrace()],
			error_logger:error_report([{handle_trap, {ExType, ExData}}|S]),
			{WaitTraps, []}
		end,
	    loop_snmpm(WaitTraps2, Opts, LogFile, AllRecTrapData ++ RecTrapData);
        {'EXIT', Pid, Reason} ->
            error_logger:error_report(
	      lists:flatten(
		io_lib:format(
		  "~p Process xmgr? died, please restart untag program untag:start().",
		  [{'EXIT', Pid, Reason}])));
        X ->
            io:format("######## ~s " ++ atom_to_list(?MODULE) ++ " received ########~n~p~n",[make_time_string(),X]),
            loop_snmpm(WaitTraps, Opts, LogFile, AllRecTrapData ++ [])
    end.

handle_inform(AgentAddr, RecTrap, WaitTraps, Opts, LogFile) ->
    remove_old_informs(), % Old INFORMs are removed after a certain time
    case is_inform_resend(RecTrap) of
	true -> % This INFORM was resent
%	    WaitTraps;
	    handle_msg(AgentAddr, RecTrap, WaitTraps, Opts, LogFile, true);
	false ->
	    handle_msg(AgentAddr, RecTrap, WaitTraps, Opts, LogFile, false)
    end.

handle_msg(AgentAddr, RecTrap, WaitTraps, Opts, LogFile, Resend) ->
    case RecTrap of
	{noError,0,[{varbind,[1,3,6,1,mgmt,'mib-2',system,sysUpTime,0],'TimeTicks',_,1},
		    {varbind,[1,3,6,1,6,snmpModules,1,1,4,1,0],'OBJECT IDENTIFIER',[?eriAlarmNotifsPrefix,Msg],2},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveManagedObject,ActiveRow],'OCTET STRING',EriAlarmActiveManagedObject,3},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveMajorType,ActiveRow],'Unsigned32',EriAlarmActiveMajorType,4},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveMinorType,ActiveRow],'Unsigned32',EriAlarmActiveMinorType,5},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveSpecificProblem,ActiveRow],'OCTET STRING',EriAlarmActiveSpecificProblem,6},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveLastSequenceNo,0],'Unsigned32',EriAlarmActiveLastSequenceNo,7},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveEventType,ActiveRow],'INTEGER',EriAlarmActiveEventType,8},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveEventTime,ActiveRow],'OCTET STRING',EriAlarmActiveEventTime,9},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveProbableCause,ActiveRow],'INTEGER',EriAlarmActiveProbableCause,10},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjAdditionalText,0],'OCTET STRING',EriAlarmNObjAdditionalText,11},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjMoreAdditionalText,0],'INTEGER',EriAlarmNObjMoreAdditionalText,12},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjResourceId,0],'INTEGER',EriAlarmNObjResourceId,13}]} ->
	    % Alarm
	    RecTrap2 = [{type,Msg},
			{agentAddr,AgentAddr},
			{eriAlarmActiveManagedObject,   EriAlarmActiveManagedObject},
			{eriAlarmActiveEventTime,       conv_time(EriAlarmActiveEventTime)},
			{eriAlarmActiveLastSequenceNo,  EriAlarmActiveLastSequenceNo},
			{eriAlarmActiveMajorType,       EriAlarmActiveMajorType},
			{eriAlarmActiveMinorType,       EriAlarmActiveMinorType},
			{eriAlarmActiveSpecificProblem, EriAlarmActiveSpecificProblem},
			{eriAlarmActiveEventType,       EriAlarmActiveEventType},
			{eriAlarmActiveProbableCause,   EriAlarmActiveProbableCause},
			{eriAlarmNObjAdditionalText,    EriAlarmNObjAdditionalText},
			{eriAlarmNObjMoreAdditionalText,EriAlarmNObjMoreAdditionalText},
			{eriAlarmNObjResourceId,        EriAlarmNObjResourceId}],
	    {Format, Alarm} = format_trap(RecTrap2),
	    if Alarm =/= alarm -> ct:pal("ill-formed message: ~p", [RecTrap]); true -> ok end,
	    log_trap(Format, RecTrap2, LogFile, Alarm, get(alarms), get(alerts)),
	    case Resend of
		true->  % An INFORM has been resent, Do not check wanted traps
		    {WaitTraps, []};
		false ->
		    Traps2 = check_trap(RecTrap2, Format, Alarm, WaitTraps, Opts),
		    case Alarm of
			alarm ->
			    check_alarmlist(RecTrap2),
			    check_gui();
			alert ->
			    ok
		    end,
		    {Traps2, [RecTrap2]}
	    end;
	%%%%% New raise alarm ERICSSON-ALARM-MIB.mib & ERICSSON-ALARM-TC-MIB.mib 
	{noError,0,[{varbind,[1,3,6,1,mgmt,'mib-2',system,sysUpTime,0],'TimeTicks',_,1},
		    {varbind,[1,3,6,1,6,snmpModules,1,1,4,1,0],'OBJECT IDENTIFIER',[?eriAlarmNotifsPrefix,Msg],2},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveManagedObject,ActiveRow],'OCTET STRING',EriAlarmActiveManagedObject,3},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveMajorType,ActiveRow],'Unsigned32',EriAlarmActiveMajorType,4},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveMinorType,ActiveRow],'Unsigned32',EriAlarmActiveMinorType,5},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveSpecificProblem,ActiveRow],'OCTET STRING',EriAlarmActiveSpecificProblem,6},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveLastSequenceNo,0],'Unsigned32',EriAlarmActiveLastSequenceNo,7},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveEventType,ActiveRow],'INTEGER',EriAlarmActiveEventType,8},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveEventTime,ActiveRow],'OCTET STRING',EriAlarmActiveEventTime,9},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveProbableCause,ActiveRow],'INTEGER',EriAlarmActiveProbableCause,10},

		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjAdditionalText,0],'OCTET STRING',EriAlarmNObjAdditionalText,11},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjMoreAdditionalText,0],'INTEGER',EriAlarmNObjMoreAdditionalText,12},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjResourceId,0],'INTEGER',EriAlarmNObjResourceId,13},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjAdditionalInfo,0],'OCTET STRING',EriAlarmNObjAdditionalInfo,14},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjMoreAdditionalInfo,0],'INTEGER',EriAlarmNObjMoreAdditionalInfo,15},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjRecordType,0],'INTEGER',EriAlarmNObjRecordType,16}]} ->
	    RecTrap2 = [{type,Msg},
			{agentAddr,AgentAddr},
			{eriAlarmActiveManagedObject,   EriAlarmActiveManagedObject},
			{eriAlarmActiveEventTime,       conv_time(EriAlarmActiveEventTime)},
			{eriAlarmActiveLastSequenceNo,  EriAlarmActiveLastSequenceNo},
			{eriAlarmActiveMajorType,       EriAlarmActiveMajorType},
			{eriAlarmActiveMinorType,       EriAlarmActiveMinorType},
			{eriAlarmActiveSpecificProblem, EriAlarmActiveSpecificProblem},
			{eriAlarmActiveEventType,       EriAlarmActiveEventType},
			{eriAlarmActiveProbableCause,   EriAlarmActiveProbableCause},
			{eriAlarmNObjAdditionalText,    EriAlarmNObjAdditionalText},
			{eriAlarmNObjMoreAdditionalText,EriAlarmNObjMoreAdditionalText},
			{eriAlarmNObjResourceId,        EriAlarmNObjResourceId},
			{eriAlarmNObjAdditionalInfo,    EriAlarmNObjAdditionalInfo},
			{eriAlarmNObjMoreAdditionalInfo,EriAlarmNObjMoreAdditionalInfo},
			{eriAlarmNObjRecordType,        EriAlarmNObjRecordType}],
	    {Format, Alarm} = format_trap(RecTrap2),
	    if Alarm =/= alarm -> ct:pal("ill-formed message: ~p", [RecTrap]); true -> ok end,
	    log_trap(Format, RecTrap2, LogFile, Alarm, get(alarms), get(alerts)),
	    case Resend of
		true->  % An INFORM has been resent, Do not check wanted traps
		    {WaitTraps, []};
		false ->
		    Traps2 = check_trap(RecTrap2, Format, Alarm, WaitTraps, Opts),
		    case Alarm of
			alarm ->
			    check_alarmlist(RecTrap2),
			    check_gui();
			alert ->
			    ok
		    end,
		    {Traps2, [RecTrap2]}
	    end;
	%%%%% New clear alarm ERICSSON-ALARM-MIB.mib & ERICSSON-ALARM-TC-MIB.mib 
	{noError,0,[{varbind,[1,3,6,1,mgmt,'mib-2',system,sysUpTime,0],'TimeTicks',_,1},
		    {varbind,[1,3,6,1,6,snmpModules,1,1,4,1,0],'OBJECT IDENTIFIER',[?eriAlarmNotifsPrefix,Msg],2},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveManagedObject,ActiveRow],'OCTET STRING',EriAlarmActiveManagedObject,3},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveMajorType,ActiveRow],'Unsigned32',EriAlarmActiveMajorType,4},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveMinorType,ActiveRow],'Unsigned32',EriAlarmActiveMinorType,5},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveSpecificProblem,ActiveRow],'OCTET STRING',EriAlarmActiveSpecificProblem,6},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveLastSequenceNo,0],'Unsigned32',EriAlarmActiveLastSequenceNo,7},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveEventType,ActiveRow],'INTEGER',EriAlarmActiveEventType,8},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveEventTime,ActiveRow],'OCTET STRING',EriAlarmActiveEventTime,9},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveProbableCause,ActiveRow],'INTEGER',EriAlarmActiveProbableCause,10},

		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjAdditionalText,0],'OCTET STRING',EriAlarmNObjAdditionalText,11},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjMoreAdditionalText,0],'INTEGER',EriAlarmNObjMoreAdditionalText,12},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjResourceId,0],'INTEGER',EriAlarmNObjResourceId,13},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjAdditionalInfo,0],'OCTET STRING',EriAlarmNObjAdditionalInfo,14},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjMoreAdditionalInfo,0],'INTEGER',EriAlarmNObjMoreAdditionalInfo,15}]} ->
	    RecTrap2 = [{type,Msg},
			{agentAddr,AgentAddr},
			{eriAlarmActiveManagedObject,   EriAlarmActiveManagedObject},
			{eriAlarmActiveEventTime,       conv_time(EriAlarmActiveEventTime)},
			{eriAlarmActiveLastSequenceNo,  EriAlarmActiveLastSequenceNo},
			{eriAlarmActiveMajorType,       EriAlarmActiveMajorType},
			{eriAlarmActiveMinorType,       EriAlarmActiveMinorType},
			{eriAlarmActiveSpecificProblem, EriAlarmActiveSpecificProblem},
			{eriAlarmActiveEventType,       EriAlarmActiveEventType},
			{eriAlarmActiveProbableCause,   EriAlarmActiveProbableCause},
			{eriAlarmNObjAdditionalText,    EriAlarmNObjAdditionalText},
			{eriAlarmNObjMoreAdditionalText,EriAlarmNObjMoreAdditionalText},
			{eriAlarmNObjResourceId,        EriAlarmNObjResourceId},
			{eriAlarmNObjAdditionalInfo,    EriAlarmNObjAdditionalInfo},
			{eriAlarmNObjMoreAdditionalInfo,EriAlarmNObjMoreAdditionalInfo}],
	    {Format, Alarm} = format_trap(RecTrap2),
	    if Alarm =/= alarm -> ct:pal("ill-formed message: ~p", [RecTrap]); true -> ok end,
	    log_trap(Format, RecTrap2, LogFile, Alarm, get(alarms), get(alerts)),
	    case Resend of
		true->  % An INFORM has been resent, Do not check wanted traps
		    {WaitTraps, []};
		false ->
		    Traps2 = check_trap(RecTrap2, Format, Alarm, WaitTraps, Opts),
		    case Alarm of
			alarm ->
			    check_alarmlist(RecTrap2),
			    check_gui();
			alert ->
			    ok
		    end,
		    {Traps2, [RecTrap2]}
	    end;
	{noError,0,
	 [{varbind,[1,3,6,1,mgmt,'mib-2',system,sysUpTime,0],'TimeTicks',_,1},
	  {varbind,[1,3,6,1,6,snmpModules,1,1,4,1,0],'OBJECT IDENTIFIER',[?eriAlarmNotifsPrefix,Msg],2},
	  {varbind,[?eriAlarmAlerts,eriAlarmAlertTable,eriAlarmAlertEntry,eriAlarmAlertManagedObject,0],'OCTET STRING',EriAlarmActiveManagedObject,3},
	  {varbind,[?eriAlarmAlerts,eriAlarmAlertTable,eriAlarmAlertEntry,eriAlarmAlertMajorType,0],'Unsigned32',EriAlarmActiveMajorType,4},
	  {varbind,[?eriAlarmAlerts,eriAlarmAlertTable,eriAlarmAlertEntry,eriAlarmAlertMinorType,0],'Unsigned32',EriAlarmActiveMinorType,5},
	  {varbind,[?eriAlarmAlerts,eriAlarmAlertTable,eriAlarmAlertEntry,eriAlarmAlertSpecificProblem,0],'OCTET STRING',EriAlarmActiveSpecificProblem,6},
	  {varbind,[?eriAlarmAlerts,eriAlarmAlertLastSequenceNo,0],'Unsigned32',EriAlarmActiveLastSequenceNo,7},
	  {varbind,[?eriAlarmAlerts,eriAlarmAlertTable,eriAlarmAlertEntry,eriAlarmAlertEventType,0],'INTEGER',EriAlarmActiveEventType,8},
	  {varbind,[?eriAlarmAlerts,eriAlarmAlertTable,eriAlarmAlertEntry,eriAlarmAlertEventTime,0],'OCTET STRING',EriAlarmActiveEventTime,9},
	  {varbind,[?eriAlarmAlerts,eriAlarmAlertTable,eriAlarmAlertEntry,eriAlarmAlertProbableCause,0],'INTEGER',EriAlarmActiveProbableCause,10},
	  {varbind,[?eriAlarmNotifObjects,eriAlarmNObjAdditionalText,0],'OCTET STRING',EriAlarmNObjAdditionalText,11},
	  {varbind,[?eriAlarmNotifObjects,eriAlarmNObjMoreAdditionalText,0],'INTEGER',EriAlarmNObjMoreAdditionalText,12},
	  {varbind,[?eriAlarmNotifObjects,eriAlarmNObjResourceId,0],'INTEGER',EriAlarmNObjResourceId,13}]} ->
	    % Alert
	    RecTrap2 = [{type,Msg},
			{agentAddr,AgentAddr},
			{eriAlarmAlertManagedObject,    EriAlarmActiveManagedObject},
			{eriAlarmAlertEventTime,        conv_time(EriAlarmActiveEventTime)},
			{eriAlarmAlertLastSequenceNo,   EriAlarmActiveLastSequenceNo},
			{eriAlarmAlertMajorType,        EriAlarmActiveMajorType},
			{eriAlarmAlertMinorType,        EriAlarmActiveMinorType},
			{eriAlarmAlertSpecificProblem,  EriAlarmActiveSpecificProblem},
			{eriAlarmAlertEventType,        EriAlarmActiveEventType},
			{eriAlarmAlertProbableCause,    EriAlarmActiveProbableCause},
			{eriAlarmNObjAdditionalText,    EriAlarmNObjAdditionalText},
			{eriAlarmNObjMoreAdditionalText,EriAlarmNObjMoreAdditionalText},
			{eriAlarmNObjResourceId,        EriAlarmNObjResourceId}],
	    {Format, Alert} = format_trap(RecTrap2),
	    if Alert =/= alert -> ct:pal("ill-formed message: ~p", [RecTrap]); true -> ok end,
	    log_trap(Format, RecTrap2, LogFile, Alert, get(alarms), get(alerts)),
	    case Resend of
		true ->
		    % An INFORM has been resent, do not check wanted traps
		    % (is this case really possible?)
		    {WaitTraps, []};
		false ->
		    {check_trap(RecTrap2, Format, Alert, WaitTraps, Opts), [RecTrap2]}
	    end;
	%%% eriAlarmAppendInfo
	{noError,0,[{varbind,[1,3,6,1,mgmt,'mib-2',system,sysUpTime,0],'TimeTicks',_,1},
		    {varbind,[1,3,6,1,6,snmpModules,1,1,4,1,0],'OBJECT IDENTIFIER',[?eriAlarmNotifsPrefix,Msg],2},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveManagedObject,ActiveRow],'OCTET STRING',EriAlarmActiveManagedObject,3},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveMajorType,ActiveRow],'Unsigned32',EriAlarmActiveMajorType,4},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveMinorType,ActiveRow],'Unsigned32',EriAlarmActiveMinorType,5},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjAdditionalText,0],'OCTET STRING',EriAlarmNObjAdditionalText,6},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveAlarmTable,eriAlarmActiveAlarmEntry,eriAlarmActiveResourceId,0],'OBJECT IDENTIFIER',EriAlarmActiveResourceId,7},
		    {varbind,[?eriAlarmNotifObjects,eriAlarmNObjAppendedAdditionalInfo,0],'OCTET STRING',EriAlarmNObjAppendedAdditionalInfo,8}]} ->
	    RecTrap2 = [{type,Msg},
			{agentAddr,AgentAddr},
			{eriAlarmAlertManagedObject,          EriAlarmActiveManagedObject},
			{eriAlarmAlertMajorType,              EriAlarmActiveMajorType},
			{eriAlarmAlertMinorType,              EriAlarmActiveMinorType},
			{eriAlarmNObjAdditionalText,          EriAlarmNObjAdditionalText},
			{eriAlarmActiveResourceId,            EriAlarmActiveResourceId},
			{eriAlarmNObjAppendedAdditionalInfo,  EriAlarmNObjAppendedAdditionalInfo}],
	    Format = format_trap(RecTrap2),
	    log_trap(Format, RecTrap2, LogFile, alarm, get(alarms), get(alerts)),
	    case Resend of
		true ->
		    % An INFORM has been resent, do not check wanted traps
		    % (is this case really possible?)
		    {WaitTraps, []};
		false ->
		    {check_trap(RecTrap2, Format, alarm, WaitTraps, Opts), [RecTrap2]}
	    end;
	{noError,0,[{varbind,[1,3,6,1,mgmt,'mib-2',system,sysUpTime,0],'TimeTicks',_,1},
		    {varbind,[1,3,6,1,6,snmpModules,1,1,4,1,0],'OBJECT IDENTIFIER',[?eriAlarmNotifsPrefix,eriAlarmHeartBeatNotif],2},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveLastSequenceNo,0],'Unsigned32',EriAlarmActiveLastSequenceNo,3},
		    {varbind,[?eriAlarmAlerts,eriAlarmAlertLastSequenceNo,0],'Unsigned32',EriAlarmAlertLastSequenceNo,4},
		    {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveLastChanged,0],'OCTET STRING',EriAlarmActiveLastChanged,5},
		    {varbind,[?eriAlarmAlerts,eriAlarmAlertLastChanged,0],'OCTET STRING',EriAlarmAlertLastChanged,6}]} ->
	    % Hearbeat, has a MIX of eriAlarmActive* and eriAlarmAlert* fields!
	    RecTrap2 = [{type,eriAlarmHeartBeatNotif},
			{agentAddr,AgentAddr},
			{eriAlarmActiveLastSequenceNo, EriAlarmActiveLastSequenceNo},
			{eriAlarmActiveLastChanged,    conv_time(EriAlarmActiveLastChanged)},
			{eriAlarmAlertLastSequenceNo,  EriAlarmAlertLastSequenceNo},
			{eriAlarmAlertLastChanged,     conv_time(EriAlarmAlertLastChanged)}],
	    {Format, _HBNotify} = format_trap(RecTrap2),
	    log_trap(Format, RecTrap2, LogFile, alert, get(alarms), get(alerts)),
	    case Resend of
		true ->
		    {WaitTraps, []};
		false ->
		    {check_trap(RecTrap2, Format, alert, WaitTraps, Opts), [RecTrap2]}
	    end;
	%%%%% New "coldstart" trap appeared on wr6
	{noError,0,[{varbind,[1,3,6,1,mgmt,'mib-2',system,sysUpTime,0],'TimeTicks',_,1},
		    {varbind,[1,3,6,1,6,snmpModules,1,1,4,1,0],'OBJECT IDENTIFIER',[1,3,6,1,4,enterprises,8072,4,0,2],2},
		    {varbind,[1,3,6,1,6,snmpModules,1,1,4,3,0],'OBJECT IDENTIFIER',[1,3,6,1,4,enterprises,8072,4],3}]} ->
	    RecTrap2 = [{type,nsNotifyShutdown},
			{agentAddr,AgentAddr}],
	    Format = format_trap(RecTrap2),
            log_trap(Format, RecTrap2, LogFile, coldstart, get(alarms), get(alerts)),
	    case Resend of
		true ->
		    {WaitTraps, []};
		false ->
		    {check_trap(RecTrap2, Format, alert, WaitTraps, Opts), [RecTrap2]}
	    end;
	%%%%% OBS below corresponds to coldstart trap, must implement clear of alarmlist
	{noError,0,[{varbind,[1,3,6,1,mgmt,'mib-2',system,sysUpTime,0],'TimeTicks',_,1},
			   {varbind,[1,3,6,1,6,snmpModules,1,1,4,1,0],'OBJECT IDENTIFIER',[?eriAlarmNotifsPrefix,eriAlarmAlarmListRebuilt],2},
			   {varbind,[?eriAlarmActiveAlarms,eriAlarmActiveTableURL|_],'OCTET STRING',EriAlarmActiveTableURL,3}]} ->
	    RecTrap2 = [{type,eriAlarmAlarmListRebuilt},
			{agentAddr,AgentAddr},
			{eriAlarmActiveTableURL,EriAlarmActiveTableURL}],
	    Format = format_trap(RecTrap2),
            log_trap(Format, RecTrap2, LogFile, coldstart, get(alarms), get(alerts)),
	    case Resend of
		true ->
		    {WaitTraps, []};
		false ->
		    {check_trap(RecTrap2, Format, alert, WaitTraps, Opts), [RecTrap2]}
	    end;
	{noError,0,[{varbind,[1,3,6,1,mgmt,'mib-2',system,sysUpTime,0],'TimeTicks',_,1},
		    {varbind,[1,3,6,1,6,snmpModules,1,1,4,1,0],'OBJECT IDENTIFIER',[?eriChangeIPAddressEvent,eriChangeIPAddressEvent],2},
		    {varbind,[?eriChangeIPAddressObjects,eriChangeIPAddressNodeIdValue],'OCTET STRING',EriChangeIPAddressNodeIdValue,3},
		    {varbind,[?eriChangeIPAddressObjects,eriChangeIPAddressNewNodeOamIpAddressType],'INTEGER',EriChangeIPAddressNewNodeOamIpAddressType,4},
		    {varbind,[?eriChangeIPAddressObjects,eriChangeIPAddressNewNodeOamIpAddress],'OCTET STRING',EriChangeIPAddressNewNodeOamIpAddress,5},
		    {varbind,[?eriChangeIPAddressObjects,eriChangeIPAddressAckFdn],'OCTET STRING',EriChangeIPAddressAckFdn,6},
		    {varbind,[?eriChangeIPAddressObjects,eriChangeIPAddressAckAttributeName],'OCTET STRING',EriChangeIPAddressAckAttributeName,7},
		    {varbind,[?eriChangeIPAddressObjects,eriChangeIPAddressAckAttributeValue],'INTEGER',EriChangeIPAddressAckAttributeValue,8},
		    {varbind,[?eriChangeIPAddressObjects,eriChangeIPAddressRemainingRetries],'INTEGER',EriChangeIPAddressRemainingRetries,9}]} ->
	    RecTrap2 = [{type,eriChangeIPAddressEvent},
			{agentAddr,AgentAddr},
			{eriChangeIPAddressNodeIdValue,             EriChangeIPAddressNodeIdValue},
			{eriChangeIPAddressNewNodeOamIpAddressType, EriChangeIPAddressNewNodeOamIpAddressType},
			{eriChangeIPAddressNewNodeOamIpAddress,     EriChangeIPAddressNewNodeOamIpAddress},
			{eriChangeIPAddressAckFdn,                  EriChangeIPAddressAckFdn},
			{eriChangeIPAddressAckAttributeName,        EriChangeIPAddressAckAttributeName},
			{eriChangeIPAddressAckAttributeValue,       EriChangeIPAddressAckAttributeValue},
			{eriChangeIPAddressRemainingRetries,        EriChangeIPAddressRemainingRetries}],
	    Format = format_trap(RecTrap2),
            log_trap(Format, RecTrap2, LogFile, coldstart, get(alarms), get(alerts)),
	    case Resend of
		true ->
		    {WaitTraps, []};
		false ->
		    {check_trap(RecTrap2, Format, alert, WaitTraps, Opts), [RecTrap2]}
	    end;	
        X ->
            ct:pal("######## ~s " ++ atom_to_list(?MODULE) ++ " received ########~n~p~n",[make_time_string(),X]),
	    {WaitTraps, []}
    end.

    

remove_old_informs() ->
    remove_old_informs(ets:tab2list(handle_resent_informs), s_now()).

remove_old_informs([], _Now) ->
    ok;
remove_old_informs([{Trap, TimeStamp}|T], Now) when Now > TimeStamp ->
    ets:delete(handle_resent_informs, Trap),
    remove_old_informs(T, Now);
remove_old_informs([_|T], Now) ->
    remove_old_informs(T, Now).

is_inform_resend(RecTrap) ->
    case ets:lookup(handle_resent_informs, RecTrap) of
	[{RecTrap, _TimeStamp}] ->
%	    ct:pal("Resent Trap ~p",[RecTrap]),
	    ct:pal("Resent Trap"),
	    true;
	[] ->
	    ets:insert(handle_resent_informs,{RecTrap,s_now() + 60}),
	    false
    end.

%%===========================================================
%% check_trap(RecTrap, Format, AlarmOrAlert, WaitTraps, Opts) ->
%% WaitTraps | {error, Reason}
%% Checking if received trap (RecTrap) is one we are waiting for (WaitTraps).
%% If the trap is in WaitTraps, the trap is removed from returning WaitTraps.
%% Also checks if Traps must come in a certain order, or if other alarms
%% or traps are allowed
%%===========================================================
check_trap(_RecTrap, _Format, _AlarmOrAlert, [], _Opts) ->
    [];
check_trap(_RecTrap, _Format, _AlarmOrAlert, Error, _Opts) when is_tuple(Error) ->
    Error;
check_trap(RecTrap, Format, AlarmOrAlert, WaitTraps, Opts) ->
    case proplists:get_bool(any_order, Opts) of
	false -> % checking exact order of traps
	    case is_trap_in_list(RecTrap, WaitTraps) of
		{ok, 1, TrapList2} -> % Trap must match first trap in traplist
		    print_ok_trap(Format, RecTrap),
		    TrapList2;
		{ok, _N, _TrapList2} -> % Trap must match first trap in traplist
		    print_error_trap(Format, RecTrap),
		    {error, unexpected_trap};
		trap_not_in_list ->
		    are_other_traps_allowed(Format, RecTrap, WaitTraps, AlarmOrAlert, Opts)
	    end;
	true -> % checking any order of traps
	    case is_trap_in_list(RecTrap, WaitTraps) of
		{ok, _N, TrapList2} -> % Trap must match any trap in traplist
		    print_ok_trap(Format, RecTrap),
		    TrapList2;
		trap_not_in_list ->
		    are_other_traps_allowed(Format, RecTrap, WaitTraps, AlarmOrAlert, Opts)
	    end
    end.









%%===========================================================
%% is_trap_in_list(RecTrap, WaitTraps) ->
%% {ok, N, WaitTraps} | trap_not_in_list
%% Searches for received trap (RecTrap) in WaitTraps
%% If found, position in WaitTraps is retured together with WaitTraps without RecTrap
%% It is trusted that a trap is a list of 2-tuples
%%===========================================================

is_trap_in_list(RecTrap, Traps) ->
    is_trap_in_list(ordsets:from_list(RecTrap), Traps, [], 1).


is_trap_in_list(_RecTrapO, [], _Acc, _N) ->
    trap_not_in_list;
is_trap_in_list(RecTrapO, [Trap|TrapTail], Acc, N) ->
    case ordsets:is_subset(ordsets:from_list(Trap), RecTrapO) of
	true ->
	    {ok, N, Acc ++ TrapTail};
	false ->
	    is_trap_in_list(RecTrapO, TrapTail, Acc ++ [Trap], N + 1)
    end.


%%===========================================================
%% are_other_traps_allowed(Format, RecTrap, WaitTraps, AlarmOrAlert, Opts) ->
%% WaitTraps | {error, Reason}
%% Checks if other traps than the ones in WaitTraps are allowed
%%===========================================================
are_other_traps_allowed(Format, RecTrap, WaitTraps, AlarmOrAlert, Opts) ->
    case proplists:get_value(allowed_traps, Opts) of
	undefined ->
	    case {proplists:get_bool(other_alarms_allowed, Opts),
		  proplists:get_bool(other_alerts_allowed, Opts),
		  AlarmOrAlert} of
		{Alarm, Alert, Type} when ((Type == alarm) and (Alarm == true));
					  ((Type == alert) and (Alert == true)) ->
		    WaitTraps;
		_ ->
		    print_error_trap(Format, RecTrap),
		    {error, unexpected_trap}
	    end;
	AllowedTrapList ->
	    case is_trap_in_list(RecTrap, AllowedTrapList) of
		{ok, _N, _TrapList2} ->
		    WaitTraps;
		_ ->
		    print_error_trap(Format, RecTrap),
		    {error, unexpected_trap}
	    end
    end.

%%===========================================================
%% Logs received trap to file if Common Test, otherwise to shell
%% "Oneline" or list format can be printed
%%===========================================================
log_trap(_Format, _RecTrap, _LogFile, alarm, no, _Alerts) ->
    ok;
log_trap(_Format, _RecTrap, _LogFile, alert, _Alarms, no) ->
    ok;
log_trap(Format, RecTrap, LogFile, _AlarmOrAlert, _Alarms, _Alerts) ->
    case LogFile of
	no_logfile ->
	    case get(format_output) of
		true  -> io:format("~s", [Format]);
		false -> io:format("~s~n~p~n~n", [make_time_string(),RecTrap])
	    end;
	_ ->
	    case get(format_output) of
		true  -> io:format(LogFile, "~s", [Format]);
		false -> io:format(LogFile, "~s~n~p~n~n", [make_time_string(),RecTrap])
	    end
    end.

%%===========================================================
%% Logs wanted trap to htmllog if Common Test, otherwise to shell
%% "Oneline" or list format can be printed
%%===========================================================
print_ok_trap(Format, RecTrap) ->
    case whereis(test_server_ctrl) of
	undefined ->
	    case get(format_output) of
		true  -> io:format("Received expected trap:~n~s", [Format]);
		false -> io:format("Received expected trap:~n~p~n", [RecTrap])
	    end;
	_ ->
	    case get(format_output) of
		true  -> ct:log(lightgreen,"Received expected trap:~n~s", [Format]);
		false -> ct:log(lightgreen,"Received expected trap:~n~p~n", [RecTrap])
	    end
    end.

%%===========================================================
%% Logs unexpected trap to htmllog if Common Test, otherwise to shell
%% "Oneline" or list format can be printed
%%===========================================================
print_error_trap(Format, RecTrap) ->
    case whereis(test_server_ctrl) of
	undefined ->
	    case get(format_output) of
		true  -> io:format("Received UNEXPECTED trap:~n~s", [Format]);
		false -> io:format("Received UNEXPECTED trap:~n~p~n", [RecTrap])
	    end;
	_ ->
	    case get(format_output) of
		true  -> ct:log(lightred, "Received UNEXPECTED trap:~n~s", [Format]);
		false -> ct:log(lightred, "Received UNEXPECTED trap:~n~p~n", [RecTrap])
	    end
    end.

%%===========================================================
%% Logs timeout to htmllog if Common Test, otherwise to shell
%% "Oneline" or list format can be printed
%%===========================================================
print_remaining_traps(Traps, Opts) ->
    case whereis(test_server_ctrl) of
	undefined ->
	    io:format("TIMEOUT, NOT RECEIVED TRAPS: ~n~p~nOPTS: ~p~n", [Traps, Opts]);
	_ ->
	    ct:log(lightred, "TIMEOUT, NOT RECEIVED TRAPS: ~n~p~nOPTS: ~p~n", [Traps, Opts])
    end.

%%===========================================================
%% If a tuple is returned the notification is classified as
%% one of alarm, alert or hbnotify. In either case a
%% human-readable iolist is returned.
%%===========================================================

-spec format_trap([{atom(), any()}]) ->
	  {list(), alarm|alert|hbnotify} | list().

format_trap([{type,nsNotifyShutdown},
	     {agentAddr,AgentAddr}]) ->
    {IPLen,_TabLen} = ip_addr_len(AgentAddr),
    Format = io_lib:format("~s ~-15s ~-" ++ IPLen ++ "s ~-38s~n",
			   [make_time_string(),
  			    "NETSNMP",
			    AgentAddr,
			    atom_to_list(nsNotifyShutdown)]),
    Format;

format_trap([{type,eriAlarmAlarmListRebuilt},
	     {agentAddr,AgentAddr},
	     {eriAlarmActiveTableURL,EriAlarmActiveTableURL}]) ->
    {IPLen,_TabLen} = ip_addr_len(AgentAddr),
    Format = io_lib:format("~s ~-15s ~-" ++ IPLen ++ "s ~-38s~n",
			   [make_time_string(),
  			    "ALARM REBUILT",
			    AgentAddr,
			    EriAlarmActiveTableURL]),
    Format;

format_trap([{type,eriChangeIPAddressEvent},
	     {agentAddr,AgentAddr},
	     {eriChangeIPAddressNodeIdValue,             EriChangeIPAddressNodeIdValue},
	     {eriChangeIPAddressNewNodeOamIpAddressType, EriChangeIPAddressNewNodeOamIpAddressType},
	     {eriChangeIPAddressNewNodeOamIpAddress,     EriChangeIPAddressNewNodeOamIpAddress},
	     {eriChangeIPAddressAckFdn,                  EriChangeIPAddressAckFdn},
	     {eriChangeIPAddressAckAttributeName,        EriChangeIPAddressAckAttributeName},
	     {eriChangeIPAddressAckAttributeValue,       EriChangeIPAddressAckAttributeValue},
	     {eriChangeIPAddressRemainingRetries,        EriChangeIPAddressRemainingRetries}]) ->
    {IPLen,_TabLen} = ip_addr_len(AgentAddr),
    Format = io_lib:format("~s ~-15s ~-" ++ IPLen ++ "s ~-52s   ~p ~-16s ~-48s ~-22s ~p ~p~n",
			   [make_time_string(),
  			    "CHANGE IP ADDR",
			    AgentAddr,
			    EriChangeIPAddressNodeIdValue,
			    EriChangeIPAddressNewNodeOamIpAddressType,
			    EriChangeIPAddressNewNodeOamIpAddress,
			    EriChangeIPAddressAckFdn,
			    EriChangeIPAddressAckAttributeName,
			    EriChangeIPAddressAckAttributeValue,
			    EriChangeIPAddressRemainingRetries]),
    Format;

                   %% ["~s ~-15s ~-15s ~-38s ~-8p ~-38s ~-38s ~-38s ~-4p ~-4p~n",
                   %%  ["12:00:12","CHANGE IP ADDR","10.67.226.155",
                   %%   "networkManagedElementId not set in siteBasic file",ipv4,
                   %%   "10.67.226.155",
                   %%   "ManagedElement=1,NodeSupport=1,OamIpSupport=1",
                   %%   "ipAddressChangeStatus",1,100]],

format_trap([{type,eriAlarmHeartBeatNotif},
	     {agentAddr,AgentAddr},
	     {eriAlarmActiveLastSequenceNo, EriAlarmActiveLastSequenceNo},
	     {eriAlarmActiveLastChanged,    EriAlarmActiveLastChanged},
	     {eriAlarmAlertLastSequenceNo,  EriAlarmAlertLastSequenceNo},
	     {eriAlarmAlertLastChanged,     EriAlarmAlertLastChanged}]) ->
    {IPLen,TabLen} = ip_addr_len(AgentAddr),
    Format = io_lib:format("~s ~-15s ~-" ++ IPLen ++ "s ~-38s ~4p~n~" ++ TabLen ++ "s ~-38s ~4p~n",
			   [make_time_string(),
			    "RBS HEARTBNTFY",
			    AgentAddr,
			    "ActiveLastChanged: " ++ EriAlarmActiveLastChanged,
			    EriAlarmActiveLastSequenceNo,
			    "",
			    "AlertLastChanged:  " ++ EriAlarmAlertLastChanged,
			    EriAlarmAlertLastSequenceNo]),
    {Format, hbnotify};

format_trap([{type,eriAlarmAppendInfo},
	     {agentAddr,AgentAddr},
	     {eriAlarmAlertManagedObject,          EriAlarmActiveManagedObject},
	     {eriAlarmAlertMajorType,              EriAlarmActiveMajorType},
	     {eriAlarmAlertMinorType,              EriAlarmActiveMinorType},
	     {eriAlarmNObjAdditionalText,          EriAlarmNObjAdditionalText},
	     {eriAlarmActiveResourceId,            EriAlarmActiveResourceId},
	     {eriAlarmNObjAppendedAdditionalInfo,  EriAlarmNObjAppendedAdditionalInfo}]) ->
    {IPLen,_TabLen} = ip_addr_len(AgentAddr),
    Format = io_lib:format("~s ~-15s ~-" ++ IPLen ++ "s (~p,~p) \"~s\" ~s ~p \"~s\"~n",
			   [make_time_string(),
			    "RBS APPENDINFO",
			    AgentAddr,
			    EriAlarmActiveMajorType,
			    EriAlarmActiveMinorType,
			    EriAlarmActiveManagedObject,
			    EriAlarmNObjAdditionalText,
			    EriAlarmActiveResourceId,
			    EriAlarmNObjAppendedAdditionalInfo]),
    Format;

format_trap([{type,Type},
	     {agentAddr,AgentAddr},
	     {eriAlarmActiveManagedObject,   EriAlarmActiveManagedObject},
	     {eriAlarmActiveEventTime,       EriAlarmActiveEventTime},
	     {eriAlarmActiveLastSequenceNo,  EriAlarmActiveLastSequenceNo},
	     {eriAlarmActiveMajorType,       EriAlarmActiveMajorType},
	     {eriAlarmActiveMinorType,       EriAlarmActiveMinorType},
	     {eriAlarmActiveSpecificProblem, EriAlarmActiveSpecificProblem},
	     {eriAlarmActiveEventType,       EriAlarmActiveEventType},
	     {eriAlarmActiveProbableCause,   EriAlarmActiveProbableCause},
	     {eriAlarmNObjAdditionalText,    EriAlarmNObjAdditionalText},
	     {eriAlarmNObjMoreAdditionalText,EriAlarmNObjMoreAdditionalText},
	     {eriAlarmNObjResourceId,        EriAlarmNObjResourceId}]) ->
    {AlarmOrAlert, TypeString} = classify(Type),
    Format = format(
	       TypeString,
	       AgentAddr,
	       EriAlarmActiveEventTime,
	       EriAlarmActiveLastSequenceNo,
	       EriAlarmActiveSpecificProblem,
	       EriAlarmActiveMajorType,
	       EriAlarmActiveMinorType,
	       EriAlarmActiveEventType,
	       EriAlarmActiveProbableCause,
	       EriAlarmActiveManagedObject,
	       EriAlarmNObjAdditionalText,
	       EriAlarmNObjMoreAdditionalText,
	       EriAlarmNObjResourceId),
    {Format, AlarmOrAlert};

format_trap([{type,Type},
	     {agentAddr,AgentAddr},
	     {eriAlarmActiveManagedObject,   EriAlarmActiveManagedObject},
	     {eriAlarmActiveEventTime,       EriAlarmActiveEventTime},
	     {eriAlarmActiveLastSequenceNo,  EriAlarmActiveLastSequenceNo},
	     {eriAlarmActiveMajorType,       EriAlarmActiveMajorType},
	     {eriAlarmActiveMinorType,       EriAlarmActiveMinorType},
	     {eriAlarmActiveSpecificProblem, EriAlarmActiveSpecificProblem},
	     {eriAlarmActiveEventType,       EriAlarmActiveEventType},
	     {eriAlarmActiveProbableCause,   EriAlarmActiveProbableCause},
	     {eriAlarmNObjAdditionalText,    EriAlarmNObjAdditionalText},
	     {eriAlarmNObjMoreAdditionalText,EriAlarmNObjMoreAdditionalText},
	     {eriAlarmNObjResourceId,        EriAlarmNObjResourceId},
	     {eriAlarmNObjAdditionalInfo,    EriAlarmNObjAdditionalInfo},
	     {eriAlarmNObjMoreAdditionalInfo,EriAlarmNObjMoreAdditionalInfo},
	     {eriAlarmNObjRecordType,        EriAlarmNObjRecordType}]) ->
    {AlarmOrAlert, TypeString} = classify(Type),
    Format = format_raise_alarm_new_mib(
	       TypeString,
	       AgentAddr,
	       EriAlarmActiveEventTime,
	       EriAlarmActiveLastSequenceNo,
	       EriAlarmActiveSpecificProblem,
	       EriAlarmActiveMajorType,
	       EriAlarmActiveMinorType,
	       EriAlarmActiveEventType,
	       EriAlarmActiveProbableCause,
	       EriAlarmActiveManagedObject,
	       EriAlarmNObjAdditionalText,
	       EriAlarmNObjMoreAdditionalText,
	       EriAlarmNObjResourceId,
	       EriAlarmNObjAdditionalInfo,
	       EriAlarmNObjMoreAdditionalInfo,
	       EriAlarmNObjRecordType),
    {Format, AlarmOrAlert};

format_trap([{type,Type},
	     {agentAddr,AgentAddr},
	     {eriAlarmActiveManagedObject,   EriAlarmActiveManagedObject},
	     {eriAlarmActiveEventTime,       EriAlarmActiveEventTime},
	     {eriAlarmActiveLastSequenceNo,  EriAlarmActiveLastSequenceNo},
	     {eriAlarmActiveMajorType,       EriAlarmActiveMajorType},
	     {eriAlarmActiveMinorType,       EriAlarmActiveMinorType},
	     {eriAlarmActiveSpecificProblem, EriAlarmActiveSpecificProblem},
	     {eriAlarmActiveEventType,       EriAlarmActiveEventType},
	     {eriAlarmActiveProbableCause,   EriAlarmActiveProbableCause},
	     {eriAlarmNObjAdditionalText,    EriAlarmNObjAdditionalText},
	     {eriAlarmNObjMoreAdditionalText,EriAlarmNObjMoreAdditionalText},
	     {eriAlarmNObjResourceId,        EriAlarmNObjResourceId},
	     {eriAlarmNObjAdditionalInfo,    EriAlarmNObjAdditionalInfo},
	     {eriAlarmNObjMoreAdditionalInfo,EriAlarmNObjMoreAdditionalInfo}]) ->
    {AlarmOrAlert, TypeString} = classify(Type),
    Format = format_clear_alarm_new_mib(
	       TypeString,
	       AgentAddr,
	       EriAlarmActiveEventTime,
	       EriAlarmActiveLastSequenceNo,
	       EriAlarmActiveSpecificProblem,
	       EriAlarmActiveMajorType,
	       EriAlarmActiveMinorType,
	       EriAlarmActiveEventType,
	       EriAlarmActiveProbableCause,
	       EriAlarmActiveManagedObject,
	       EriAlarmNObjAdditionalText,
	       EriAlarmNObjMoreAdditionalText,
	       EriAlarmNObjResourceId,
	       EriAlarmNObjAdditionalInfo,
	       EriAlarmNObjMoreAdditionalInfo),
    {Format, AlarmOrAlert};

format_trap([{type,Type},
	     {agentAddr,AgentAddr},
	     {eriAlarmAlertManagedObject,    EriAlarmAlertManagedObject},
	     {eriAlarmAlertEventTime,        EriAlarmAlertEventTime},
	     {eriAlarmAlertLastSequenceNo,   EriAlarmAlertLastSequenceNo},
	     {eriAlarmAlertMajorType,        EriAlarmAlertMajorType},
	     {eriAlarmAlertMinorType,        EriAlarmAlertMinorType},
	     {eriAlarmAlertSpecificProblem,  EriAlarmAlertSpecificProblem},
	     {eriAlarmAlertEventType,        EriAlarmAlertEventType},
	     {eriAlarmAlertProbableCause,    EriAlarmAlertProbableCause},
	     {eriAlarmNObjAdditionalText,    EriAlarmNObjAdditionalText},
	     {eriAlarmNObjMoreAdditionalText,EriAlarmNObjMoreAdditionalText},
	     {eriAlarmNObjResourceId,        EriAlarmNObjResourceId}]) ->
    {AlarmOrAlert, TypeString} = classify(Type),
    Format = format(
	       TypeString,
	       AgentAddr,
	       EriAlarmAlertEventTime,
	       EriAlarmAlertLastSequenceNo,
	       EriAlarmAlertSpecificProblem,
	       EriAlarmAlertMajorType,
	       EriAlarmAlertMinorType,
	       EriAlarmAlertEventType,
	       EriAlarmAlertProbableCause,
	       EriAlarmAlertManagedObject,
	       EriAlarmNObjAdditionalText,
	       EriAlarmNObjMoreAdditionalText,
	       EriAlarmNObjResourceId),
    {Format, AlarmOrAlert}.

classify(eriAlarmIndeterminate) ->   {alarm, "RBS INDETERM"};
classify(eriAlarmWarning) ->         {alarm, "RBS WARNING"};
classify(eriAlarmMinor) ->           {alarm, "RBS MINOR"};
classify(eriAlarmMajor) ->           {alarm, "RBS MAJOR"};
classify(eriAlarmCritical) ->        {alarm, "RBS CRITICAL"};
classify(eriAlarmCleared) ->         {alarm, "RBS ALARM CLEAR"};
classify(eriAlarmIndAlert) ->        {alert, "RBS ALERT"};
classify(eriAlarmWarnAlert) ->       {alert, "RBS WARN ALERT"};
classify(eriAlarmMinorAlert) ->      {alert, "RBS MINOR ALERT"};
classify(eriAlarmMajorAlert) ->      {alert, "RBS MAJOR ALERT"};
classify(eriAlarmCriticalAlert) ->   {alert, "RBS CRIT ALERT"}.

format(TypeS, AgentAddr,
       EvTime, LastSeqNo, SpecProbl,
       Major, Minor,
       EvType, ProbCause, MO,
       AddText, MoreAddText, ResId) ->
    {IPLen,_TabLen} = ip_addr_len(AgentAddr),
    io_lib:format("~s ~-15s ~-" ++ IPLen ++ "s ~-20s ~4s ~s(~p,~p) ~p ~p \"~s\" \"~s\" ~p ~p~n",
		  [make_time_string(),
		   TypeS, AgentAddr,
		   EvTime, integer_to_list(LastSeqNo), SpecProbl,
		   Major, Minor,
		   EvType, ProbCause, MO,
		   AddText, MoreAddText, ResId]).

format_raise_alarm_new_mib(TypeS, AgentAddr,
	       EvTime, LastSeqNo, SpecProbl,
	       Major, Minor,
	       EvType, ProbCause, MO,
	       AddText, MoreAddText, ResId,
	       AddInfo, MoreAddInfo, RecType) ->
    {IPLen,_TabLen} = ip_addr_len(AgentAddr),
    io_lib:format("~s ~-15s ~-" ++ IPLen ++ "s ~-20s ~4s ~s(~p,~p) ~p ~p \"~s\" \"~s\" ~p ~p \"~s\" ~p ~p~n",
		  [make_time_string(),
		   TypeS, AgentAddr,
		   EvTime, integer_to_list(LastSeqNo), SpecProbl,
		   Major, Minor,
		   EvType, ProbCause, MO,
		   AddText, MoreAddText, ResId,
		   AddInfo, MoreAddInfo, RecType]).

format_clear_alarm_new_mib(TypeS, AgentAddr,
	       EvTime, LastSeqNo, SpecProbl,
	       Major, Minor,
	       EvType, ProbCause, MO,
	       AddText, MoreAddText, ResId,
	       AddInfo, MoreAddInfo) ->
    {IPLen,_TabLen} = ip_addr_len(AgentAddr),
    io_lib:format("~s ~-15s ~-" ++ IPLen ++ "s ~-20s ~4s ~s(~p,~p) ~p ~p \"~s\" \"~s\" ~p ~p \"~s\" ~p~n",
		  [make_time_string(),
		   TypeS, AgentAddr,
		   EvTime, integer_to_list(LastSeqNo), SpecProbl,
		   Major, Minor,
		   EvType, ProbCause, MO,
		   AddText, MoreAddText, ResId,
		   AddInfo, MoreAddInfo]).

ip_addr_len(AgentAddr) ->
    case (Len = length(AgentAddr)) =< 15 of
	true ->  {"15","40"};
	false -> {integer_to_list(Len+1),integer_to_list(Len+26)}
    end.

%inet:ntoa(list_to_tuple([list_to_integer(X)||X<-string:tokens("8193.7024.25217.61696.0.0.0.135",".")])).

check_gui() ->
    case whereis(alarmlist_gui) of
	AlarmGUI when is_pid(AlarmGUI) ->
	    AlarmGUI ! {updated_alarmlist,ets:tab2list(current_alarmlist)};
	_ ->
	    ok
    end.

check_alarmlist(Formated) when hd(Formated) =:= {type,eriAlarmIndeterminate};
			       hd(Formated) =:= {type,eriAlarmWarning};
			       hd(Formated) =:= {type,eriAlarmMinor};
			       hd(Formated) =:= {type,eriAlarmMajor};
			       hd(Formated) =:= {type,eriAlarmCritical} ->
    insert_alarm(Formated);
check_alarmlist(Formated) when hd(Formated) =:= {type,eriAlarmCleared} ->
    clear_alarm(Formated);
check_alarmlist(Formated) ->
    io:format("!!!!!!!!!!!!!!!!!!! Unexpected alarm: ~p~n",[Formated]).

insert_alarm([Type,AgentAddr,ManagedObject,EriAlarmActiveEventTime,SequenceNumber,
	      MajorType,MinorType,SpecificProblem,EventType,ProbableCause,
	      AdditionalText,MoreadditionalText,ResourceId]) ->
    case ets:match(current_alarmlist,{Type,AgentAddr,ManagedObject,'$1',SequenceNumber,MajorType,MinorType,SpecificProblem,EventType,ProbableCause,AdditionalText,MoreadditionalText,ResourceId}) of
	[[EriAlarmActiveEventTime]] -> % Same alarm received twice (ex Inform where acc was lost).
	    ok;
	[[{eriAlarmActiveEventTime,EriAlarmActiveEventTime2}]] -> % Same alarm but at another point in time, can be from another trap receiver. Replace old alarm with new
%	    io:format("!!!!!!!!!!!!! same alarm~n",[]),
	    ets:delete_object(current_alarmlist,{Type,AgentAddr,ManagedObject,{eriAlarmActiveEventTime,EriAlarmActiveEventTime2},SequenceNumber,MajorType,MinorType,SpecificProblem,EventType,ProbableCause,AdditionalText,MoreadditionalText,ResourceId}),
	    ets:insert(current_alarmlist,{Type,AgentAddr,ManagedObject,EriAlarmActiveEventTime,SequenceNumber,MajorType,MinorType,SpecificProblem,EventType,ProbableCause,AdditionalText,MoreadditionalText,ResourceId});
	[] -> % Alarm does not exist, insert new alarm
%	    io:format("Insert new alarm ~n",[]),
	    ets:insert(current_alarmlist,{Type,AgentAddr,ManagedObject,EriAlarmActiveEventTime,SequenceNumber,MajorType,MinorType,SpecificProblem,EventType,ProbableCause,AdditionalText,MoreadditionalText,ResourceId})
    end;
%%%%% New alarm ERICSSON-ALARM-MIB.mib & ERICSSON-ALARM-TC-MIB.mib 
insert_alarm([Type,AgentAddr,ManagedObject,EriAlarmActiveEventTime,SequenceNumber,
	      MajorType,MinorType,SpecificProblem,EventType,ProbableCause,
	      AdditionalText,MoreadditionalText,ResourceId,
	      AdditionalInfo,
	      MoreadditionalInfo,
	      _RecordType]) ->
    case ets:match(current_alarmlist,{Type,AgentAddr,ManagedObject,'$1',SequenceNumber,MajorType,MinorType,SpecificProblem,EventType,ProbableCause,AdditionalText,MoreadditionalText,ResourceId,AdditionalInfo,MoreadditionalInfo}) of
	[[EriAlarmActiveEventTime]] -> % Same alarm received twice (ex Inform where acc was lost).
	    ok;
	[[{eriAlarmActiveEventTime,EriAlarmActiveEventTime2}]] -> % Same alarm but at another point in time, can be from another trap receiver. Replace old alarm with new
%	    io:format("!!!!!!!!!!!!! same alarm~n",[]),
	    ets:delete_object(current_alarmlist,{Type,AgentAddr,ManagedObject,{eriAlarmActiveEventTime,EriAlarmActiveEventTime2},SequenceNumber,MajorType,MinorType,SpecificProblem,EventType,ProbableCause,AdditionalText,MoreadditionalText,ResourceId,AdditionalInfo,MoreadditionalInfo}),
	    ets:insert(current_alarmlist,{Type,AgentAddr,ManagedObject,EriAlarmActiveEventTime,SequenceNumber,MajorType,MinorType,SpecificProblem,EventType,ProbableCause,AdditionalText,MoreadditionalText,ResourceId,AdditionalInfo,MoreadditionalInfo});
	[] -> % Alarm does not exist, insert new alarm
%	    io:format("Insert new alarm ~n",[]),
	    ets:insert(current_alarmlist,{Type,AgentAddr,ManagedObject,EriAlarmActiveEventTime,SequenceNumber,MajorType,MinorType,SpecificProblem,EventType,ProbableCause,AdditionalText,MoreadditionalText,ResourceId,AdditionalInfo,MoreadditionalInfo})
    end.

clear_alarm([Type,AgentAddr,ManagedObject,EriAlarmActiveEventTime,SequenceNumber,MajorType,MinorType,SpecificProblem,EventType,ProbableCause,AdditionalText,MoreadditionalText,ResourceId]) ->
    case ets:match_object(current_alarmlist,{'_',AgentAddr,ManagedObject,'_','_',MajorType,MinorType,'_','_','_','_','_','_'}) of
	[Alarm] -> % Alarm exists, clear it
	    ets:delete_object(current_alarmlist,Alarm);
	[] -> % Alarm does not exist, insert extra cleared alarm
	    ets:insert(extra_cleardalarms,{Type,AgentAddr,ManagedObject,EriAlarmActiveEventTime,SequenceNumber,MajorType,MinorType,SpecificProblem,EventType,ProbableCause,AdditionalText,MoreadditionalText,ResourceId});
	Other ->
	    io:format("Oooooooooops clear_alarm ets:match(current_alarmlist ~p~n",[Other])
    end;
clear_alarm([Type,AgentAddr,ManagedObject,EriAlarmActiveEventTime,SequenceNumber,MajorType,MinorType,SpecificProblem,EventType,ProbableCause,AdditionalText,MoreadditionalText,ResourceId,
	      AdditionalInfo,
	      MoreadditionalInfo]) ->
    case ets:match_object(current_alarmlist,{'_',AgentAddr,ManagedObject,'_','_',MajorType,MinorType,'_','_','_','_','_','_','_','_'}) of
	[Alarm] -> % Alarm exists, clear it
	    ets:delete_object(current_alarmlist,Alarm);
	[] -> % Alarm does not exist, insert extra cleared alarm
	    ets:insert(extra_cleardalarms,{Type,AgentAddr,ManagedObject,EriAlarmActiveEventTime,SequenceNumber,MajorType,MinorType,SpecificProblem,EventType,ProbableCause,AdditionalText,MoreadditionalText,ResourceId,
					   AdditionalInfo,
					   MoreadditionalInfo});
	Other ->
	    io:format("Oooooooooops clear_alarm ets:match(current_alarmlist ~p~n",[Other])
    end.

conv_time(Time) -> % [7,218,8,12,7,7,0,0,43,0,0]
    <<Y:16,Mo,D,H,Mi,S,_/binary>> = list_to_binary(Time),
    lists:flatten(io_lib:format("~s-~s-~s ~s:~s:~s",[integer_to_list(X)||X<-[Y,Mo,D,H,Mi,S]])).

make_time_string()->
    {H,M,S}=time(),
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",[H,M,S])).

s_now() ->
    {Me,Se,_} = os:timestamp(),
    Me * 1000000 + Se.
