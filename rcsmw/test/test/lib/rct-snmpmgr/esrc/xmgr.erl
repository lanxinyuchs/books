%% -----------------------------------------------------------------------------
%% #0.    BASIC INFORMATION
%% -----------------------------------------------------------------------------
%% %CCaseFile:	xmgr.erl %
%% @author etxkols
%% @copyright Ericsson AB 2013-2017
%% @version /main/R2A/R3A/R5A/R9A/1
%% @doc == Module that starts a SNMP manager. ==
%%
%% 
%%   This module starts a SNMP manager. Used by rcs_snmpmgr.erl<br/>
%% @end
-module(xmgr).

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2014 All rights reserved.
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
%%% R2A/2      2014-02-03 etxkols     Added ignore in handle_agent since OTP R16B03
%%% R2A/3      2014-08-28 erarafo     Dialyzer fault fixed
%%% R2A/4      2014-08-29 erarafo     Guarding the main loop against crashing
%%% R2A/5      2014-09-03 erarafo     Some more trace output if VERBOSE
%%% R2A/6      2014-09-09 etxkols     New handle_agent format in OTP snmp 5.0
%%% R3A/1      2014-10-23 erarafo     Fixed bug introduced by R2A/8 of rct_snmpmgr
%%% R5A/1      2016-02-03 etxkols     ipv6
%%% R9A/1      2017-02-27 etxkols     New "ERICSSON-DISCOVERY-MIB.mib" and "ERICSSON-CHANGEIPADDRESS-MIB.mib"
%%% ----------------------------------------------------------

%% Functional definition of true/false that the compiler
%% cannot evaluate statically and complain about
-define(VERBOSE, os:timestamp() =:= {0, 0, 0}).  % false
%-define(VERBOSE, os:timestamp() =/= {0, 0, 0}).  % true

-define(IF_VERBOSE(FORMAT, DATA), 
	case ?VERBOSE of
	    true ->
		error_logger:info_msg("~w:"++FORMAT++"~n", [?MODULE|DATA]);
	    _ ->
		ok
	end
       ).

-define(ERROR_MSG(FORMAT, DATA),
	error_logger:error_msg("~w:"++FORMAT++"~n", [?MODULE|DATA])
       ).


%% The snmpm_user behaviour is defined in OTP. It requires certain
%% callbacks to be implemented (see the export clause below).
-behaviour(snmpm_user).

%% Manager callback API:
-export([handle_error/3,
         handle_agent/5,
         handle_pdu/4,
         handle_trap/3,
         handle_inform/3,
         handle_report/3]).

-export([help/0,
         compile_mibs/1,
         start/1,start/5,
         stop/0,stop/1,
         load_mibs/1,
         start_snmpm/4,
         translate/1,
         translate_msg/1,
         oid_to_names/1,
         resolve_msg/1,
         names_to_oid/1,
         enum_to_integer/1,enum_to_integer/2,
	 integer_to_enum/2]).


-define(REGISTER_AGENT_DATA,{du,[{"10.86.148.133",  [10,86,148,133]}]}). %{Targetname, Address}
%-define(REGISTER_AGENT_DATA,{du,[{"147.214.91.75",  [147,214,91,75]}]}). %{Targetname, Address}

-define(MANAGER_CONF,[{port,27162},
                      {engine_id,"SCX manager engine"},
                      {max_message_size,8192}]).
               

-define(MIBS,["RFC1213-MIB",
	      "SNMP-FRAMEWORK-MIB",
	      "INET-ADDRESS-MIB",
	      "SNMP-TARGET-MIB",
	      "SNMP-NOTIFICATION-MIB",
	      "ITU-ALARM-TC-MIB",
	      "RFC1271-MIB",
	      "TOKEN-RING-RMON-MIB",
	      "RMON-MIB",
	      "RMON2-MIB",
	      "ALARM-MIB",
	      "IANA-ITU-ALARM-TC-MIB",
	      "ERICSSON-TOP-MIB",
	      "ERICSSON-TC-MIB",
	      "ERICSSON-ALARM-PC-MIB",
	      "ERICSSON-ALARM-TC-MIB",
	      "ERICSSON-ALARM-MIB",
	      "ERICSSON-DISCOVERY-MIB",
	      "ERICSSON-CHANGEIPADDRESS-MIB"
              ]).

%xmgr:install("/home/etxkols/RCS/snmp").
%xmgr:start("/home/etxkols/RCS/snmp").
%xmgr:compile_mibs("/home/etxkols/RCS/snmp/mibs").

help() ->
    io:format("Syntax:~n"
              "----------------------------------------------------------------------------------------~n"
              "xmgr:compile_mibs(help).~n"
              "xmgr:start(help).~n"
              "xmgr:stop(help).~n"
              "xmgr:load_mibs(help).~n"
              "xmgr:translate(help).~n"
              "xmgr:translate_msg(help).~n"
              "xmgr:oid_to_names(help).~n"
              "xmgr:resolve_msg(help).~n"
              "xmgr:names_to_oid(help).~n"
              "xmgr:enum_to_integer(help).~n"
              "example: ~n"
              "Tool must be run as root~n"
              "erl~n"
              "xmgr:install(\"/home/etxkols/snmp\").          % Only needs to be done once~n"
              "xmgr:start(\"/home/etxkols/snmp\").~n"
              "We will now receive traps~n"
              "xmgr:compile_mibs(\"/home/etxkols/DMX_mibs2\"). % Only needs to be done when new mibs~n"
              "xmgr:load_mibs(\"/home/etxkols/DMX_mibs2\").~n"
              "xmgr:translate(yes).~n"
              "OIDs and intergers in traps will now be translated~n"
              "snmpm:sync_get(scx_user, \"192.168.0.25\", [[1,3,6,1,4,1,193,177,2,2,1,2,1,7,0]]).~n"
              "xmgr:translate_msg(snmpm:sync_get(scx_user, \"192.168.0.25\", [[1,3,6,1,4,1,193,177,2,2,1,2,1,7,0]])).~n"
              "snmpm:sync_get(scx_user, \"192.168.0.25\", [xmgr:names_to_oid([1,3,6,1,4,enterprises,ericsson,177,2,2,genShelfMIB,shelf,shelfObjects,shelfMgrAutonomousMode,0])]).~n"
              "snmpm:sync_set(scx_user, \"192.168.0.25\", [{[1,3,6,1,4,1,193,177,2,2,1,2,1,7,0],i,1}]).~n"
              "snmpm:sync_set(scx_user, \"192.168.0.25\",  xmgr:resolve_msg([{[1,3,6,1,4,enterprises,ericsson,177,2,2,genShelfMIB,shelf,shelfObjects,shelfMgrAutonomousMode,0],i,autonomous}])).~n"
              "snmpm:async_set(scx_user, \"192.168.0.25\", [{[1,3,6,1,4,1,193,177,2,2,1,2,1,7,0],i,1}]).~n"
              "xmgr:stop().~n"
              "----------------------------------------------------------------------------------------~n").



compile_mibs(help) ->
    io:format("~n----------------------------------------------------------------------------------------~n"
	      "compile_mibs(Dir) -> Result~n"
	      "Input:          Dir              string      Path to mibs~n"
	      "Output:         Result           ok | nok~n"
	      "                Other            undefined   ~n"
	      "Exceptions:  ~n"
	      "Description: Compiles MIB's in Dir for snmp application ~n"
	      "exampel:     xmgr:compile_mibs(\"/home/test34/testmibs\").~n"
	      "             ok~n"
              "----------------------------------------------------------------------------------------~n");
compile_mibs(Dir) ->
    {ok,Cwd} = file:get_cwd(),
    file:set_cwd(Dir),
    Mibs=?MIBS,
    Comp = [{snmpc:compile(Mib),Mib}||Mib<-Mibs],
    file:set_cwd(Cwd),
    case [error||{{error,_},_} <- Comp] of
        [] -> ok;
         _ -> io:format("~p~n",[Comp]),
	      nok
    end.

start(help) ->
    io:format("~n----------------------------------------------------------------------------------------~n"
	      "start()                                                         -> Result~n"
	      "start(ConfDir)                                                  -> Result~n"
	      "start(User,ConfDir,ManagerConf,RegisterData)                    -> Result~n"
	      "Input:       User            pid | no_user     Pid or registered process where traps  ~n"
              "                                               will be sent to. If omitted, traps are printed in shell~n"             
	      "             ConfDir         string            Path to manager configuration, defaults to~n"
	      "                                               'file:get_cwd ++ \"/snmp/manager/conf/\"'~n"
	      "             ManagerConf                       ~n"
	      "             RegisterData    {UserID,Agents,Community}   ~n"
              "             UserID          atom              Alias for snmp manager~n"
              "             Agents          [{TargetName, IPAddress} ~n"
              "             TargetName      string            Alias for an agent~n"
              "             IPAddress       [integer,integer,integer,integer]~n"
              "             Community       string            Default is \"NETMAN\"~n"
	      "Output:      Result          {ok,started} | ~n"
              "                             {nok,{register_agents,Reason}} |~n"
              "                             {nok,{register_user,Reason}} |~n"
              "                             {nok,{application_start,Reason}} |~n"
              "                             {nok,Other} ~n"
	      "             Reason          undefined   ~n"
	      "             Other           undefined   ~n"
	      "Exceptions:  ~n"
	      "Description: Starts snmp manager ~n"
	      "exampel:     call xmgr:start(untag,\"/home/test34/snmp\",[{port,162},~n"
	      "                                                         {engine_id,\"SCX manager engine\"},~n"
	      "                                                         {max_message_size,8192}],~n"
	      "                                                        {scx_user,[{\"192.168.0.2\",[192,168,0,2]},~n"
 	      "                                                                   {\"192.168.48.2\",[192,168,48,2]},~n"
 	      "                                                                   {\"192.168.48.25\",[192,168,48,25]},~n"
 	      "                                                                   {\"192.168.0.25\",[192,168,0,25]}]})~n"
              "----------------------------------------------------------------------------------------~n");
start(ConfDir) ->
    start(no_user,ConfDir,?MANAGER_CONF,?REGISTER_AGENT_DATA,[]).

start(User,ConfDir,ManagerConf,RegisterData,Opts) ->
    case filelib:ensure_dir(ConfDir ++ "/") of
	ok ->
	    case snmpm_conf:write_manager_config(ConfDir,ManagerConf) of
		ok ->    
		    case whereis(?MODULE) of
			undefined -> 
			    case application:get_application(snmp) of
				{ok,snmp} -> application:stop(snmp);
				undefined -> ok
			    end,
			    application:set_env(snmp,manager,[{config,[{dir,ConfDir},
								       {db_dir,ConfDir}]},
							      {def_user_mod,xmgr},
							      {inform_request_behaviour,auto}]),
			    register(?MODULE,spawn_link(?MODULE,start_snmpm,[self(),User,RegisterData,Opts])),
			    receive
				A -> A
			    end;
			_ -> {ok, already_started}
		    end;
		Other ->
		    io:format("Could not open ~s/manager.conf Reason: ~p~n",[ConfDir,Other]),
		    {nok,Other}
	    end;
	{error, Reason} ->
	    io:format("Could not create ~p Reason: ~p~n",[ConfDir,Reason]),
	    {error, Reason}	
    end.

start_snmpm(Pid,User,{UserID,Agents},Opts) ->
    start_snmpm(Pid,User,{UserID,Agents,"public"},Opts);
start_snmpm(Pid,User,{UserID,Agents,Community},Opts) ->
    Translate = proplists:get_value(translate,Opts, no),
    LoadMibs = proplists:get_value(load_mibs,Opts, no),
    case application:start(snmp) of
        ok ->
            case snmpm:register_user(UserID, ?MODULE,[]) of
                ok ->
                    case register_agents(UserID, Agents, Community) of
                        ok -> 
			    case LoadMibs of
				no ->
				    Pid ! {ok,started},
				    handler(Translate,User);
				Dir ->
				    case load_mibs(Dir) of
					ok ->
					    Pid ! {ok,started},
					    handler(Translate,User);
					{nok,Reason} ->
					    Pid ! {nok,{register_agents,Reason}},
					    application:stop(snmp)
				    end
			    end;
                        {nok,Reason} ->
                            Pid ! {nok,{register_agents,Reason}},
                            application:stop(snmp)
                   end;
                {error, Reason} ->
                    Pid ! {nok,{register_user,Reason}},
                    application:stop(snmp)
            end;                   
        {error, Reason} ->
            Pid ! {nok,{application_start,Reason}}
    end.

%%% ----------------------------------------------------------
%%% @doc Process loop of this module, using try/catch everywhere
%%% to keep alive at all times.
%%% @end
%%% ----------------------------------------------------------

handler(Translate, User) ->
    ?IF_VERBOSE("handler/2, process: ~p, translate: ~p, user: ~p",
		[self(), Translate, User]),
    receive
	stop -> 
	    ?IF_VERBOSE("handler/2 received: stop", []),
	    application:stop(snmp);
	{translate, YesOrNo}=Message ->
	    ?IF_VERBOSE("handler/2 received: ~p", [Message]),
	    handler(YesOrNo, User);
	X ->
	    case Translate of
		no ->
		    case User of
			no_user ->
			    ?IF_VERBOSE(
			      "handler/2 received: ~p~n"
			      "translation: no",
			      [X]);
			_ ->
			    try
				?IF_VERBOSE(
				  "handler/2 received: ~p~n"
				  "translation: no~n"
				  "sending to user: ~p",
				  [X, User]),
				User ! X
			    catch
				Ex:Data ->
				    ?ERROR_MSG(
				      "handler/2, sending to ~p failed, ~w:~p",
				      [User, Ex, Data])
			    end
		    end;
		yes ->
		    try translate_msg(X) of
			T ->
			    ?IF_VERBOSE(
			      "handler/2 received: ~p~n"
			      "translation: ~p", [X, T]),
			    if 
				User =/= no_user ->
				    try
					?IF_VERBOSE(
					  "handler/2 sending translation to user: ~p",
					  [User]),
					User ! T
				    catch
					Ex:Data ->
					    ?ERROR_MSG(
					      "handler/2, sending to ~p failed, ~w:~p",
					      [User, Ex, Data])
				    end;
				true ->
				    ok
			    end
		    catch
			Ex:Data ->
			    ?ERROR_MSG(
			      "handler/2 received: ~p~n"
			      "translation failed, ~w:~p", 
			      [X, Ex, Data])
		    end;
		_ ->
		    ?ERROR_MSG(
		      "handler/2, unexpected value for 'translate': ~p",
		      [Translate])
	    end,
	    handler(Translate, User)
    end.

register_agents(_,[],_) ->
    ok;
register_agents(UserID, [{TargetName, IPAddress}|T], Community) ->
    Tdomain = case size(IPAddress) of
		  8 -> transportDomainUdpIpv6;
		  4 -> transportDomainUdpIpv4
	      end,
    case snmpm:register_agent(UserID, TargetName, [{engine_id,"left_local"}, {address,IPAddress}, {community, Community},{tdomain,Tdomain}]) of
        ok -> 
            register_agents(UserID, T, Community);
        {error,Reason} ->
            {nok,Reason}
    end.
    
stop(help) ->
    io:format("~n----------------------------------------------------------------------------------------~n"
	      "stop() -> Result~n"
	      "Input:          -~n"
	      "Output:         true ~n"
	      "Exceptions:  ~n"
	      "Description: Stops the snmp manager ~n"
	      "exampel:     xmgr:stop().~n"
	      "             true~n"
              "----------------------------------------------------------------------------------------~n").
stop()->
    catch(?MODULE ! stop).

load_mibs(help) ->
    io:format("~n----------------------------------------------------------------------------------------~n"
	      "load_mibs(Dir) -> Result~n"
	      "Input:       Dir        string      Path to mibs~n"
	      "Output:      Result     ok | {nok,R}~n"
	      "             R          list        List of load results~n"
	      "Exceptions:  ~n"
	      "Description: Loads mibs and creates xmgr_enums ets table for enum conversion ~n"
	      "exampel:     xmgr:load_mibs(\"/home/test34/testmibs\").~n"
	      "             ok~n"
              "----------------------------------------------------------------------------------------~n");
load_mibs(Dir) ->
    R = [{X,snmpm:load_mib(Dir++"/"++X)}||X<-?MIBS],
    case lists:keysearch(nok,2,R) of
        false ->
             fix_enums(Dir);
        {value,_} ->
            {nok,R}
    end.

fix_enums(Dir) ->
    catch(ets:delete(xmgr_enums)),
    ets:new(xmgr_enums,[public,named_table,ordered_set]),
    Raw = os:cmd("ls " ++ Dir ++ "/*.bin"),
    Files = string:tokens(Raw,"\n"),
    read_mibs(Files).

read_mibs([]) ->   
    ok;
read_mibs([File|T]) ->    
    {ok,{mib,_,_,_,_,Data,_,_,_,_,_}} = snmp:read_mib(File),
    Data2=[{OID,Name,Enums}||{me,OID,_,Name,{_,_,_,_,[{enums,Enums}],_,_,_,_},_,_,_,_,_,_}<-Data],
    insert_enums(Data2),
    read_mibs(T).

insert_enums([]) ->
    ok;
insert_enums([{OID,Name,Enums}|T]) ->
    [ets:insert(xmgr_enums,{{OID,Int},Enum,Name})||{Enum,Int}<-Enums],
    insert_enums(T).
   
  
   

translate(help) ->
    io:format("~n----------------------------------------------------------------------------------------~n"
	      "translate(YesOrNo) -> Result~n"
	      "Input:       YesOrNo    yes | no~n"
	      "Output:      Result     true~n"
	      "Exceptions:  ~n"
	      "Description: Translates OIDs and integers to readable format when a trap is received~n"
	      "exampel:     xmgr:translate(yes).~n"
	      "             true~n"
              "----------------------------------------------------------------------------------------~n");
translate(yes) ->
    ?MODULE ! {translate,yes};
translate(no) ->
    ?MODULE ! {translate,no}.

translate_msg(help) ->
    io:format("~n----------------------------------------------------------------------------------------~n"
	      "translate_msg(Msg) -> Result~n"
	      "Input:       Msg        Message format according to OTP snmp manager application~n"
	      "Output:      Result     Translated message~n"
	      "Exceptions:  ~n"
	      "Description: Translates OIDs and integers to readable format when a trap is received~n"
	      "exampel:     xmgr:translate_msg({handle_trap,\"192.168.0.25\",~n"
	      "                                             {noError,0,~n"
	      "                                                      [{varbind,[1,3,6,1,2,1,1,3,0],'TimeTicks',335580,1},~n"
	      "                                                      {varbind,[1,3,6,1,6,3,1,1,4,1,0],~n"
	      "                                                               'OBJECT IDENTIFIER',~n"
	      "                                                               [1,3,6,1,4,1,193,177,2,2,5,0,3],~n"
	      "                                                               2},~n"
	      "                                                      {varbind,[1,3,6,1,6,3,1,1,4,3,0],~n"
	      "                                                               'OBJECT IDENTIFIER',~n"
 	      "                                                              [1,3,6,1,4,1,193,177,2,2,5],~n"
	      "                                                               3}]},~n"
	      "                                            undefined}).~n"
	      "              {handle_trap,\"192.168.0.25\",~n"
	      "                           {noError,0,~n"
	      "                                    [{varbind,[1,3,6,1,2,'mib-2',system,sysUpTime,0],~n"
	      "                                              'TimeTicks',335580,1},~n"
	      "                                     {varbind,[1,3,6,1,6,snmpModules,snmpMIB,snmpMIBObjects,~n"
	      "                                               snmpTrap,snmpTrapOID,0],~n"
	      "                                              'OBJECT IDENTIFIER',~n"
	      "                                              [1,3,6,1,4,enterprises,ericsson,177,2,2,genSysMgr,~n"
	      "                                               sysMgrNotifications,rootLoggedIn],~n"
 	      "                                             2},~n"
	      "                                     {varbind,[1,3,6,1,6,snmpModules,snmpMIB,snmpMIBObjects,~n"
	      "                                               snmpTrap,snmpTrapEnterprise,0],~n"
	      "                                              'OBJECT IDENTIFIER',~n"
	      "                                              [1,3,6,1,4,enterprises,ericsson,177,2,2,genSysMgr],~n"
	      "                                              3}]},~n"
	      "                           undefined}~n"
              "----------------------------------------------------------------------------------------~n");
translate_msg({handle_inform, TargetName, SnmpTrap, UserData}) ->
    {handle_inform, TargetName, translate_msg(SnmpTrap), UserData};
translate_msg({handle_trap, TargetName, SnmpTrap, UserData}) ->
    {handle_trap, TargetName, translate_msg(SnmpTrap), UserData};
translate_msg({handle_pdu, TargetName, ReqId, SnmpResponse, UserData}) ->
    {handle_pdu, TargetName, ReqId, translate_msg(SnmpResponse), UserData};
translate_msg({ErrorStatus, ErrorIndex, Varbinds}) when is_list(Varbinds) ->
    {ErrorStatus,ErrorIndex,varbind_to_names_enums(Varbinds)};
translate_msg({Res, {ErrorStatus, ErrorIndex, Varbinds}, Remaining}) when is_list(Varbinds) -> 
    {Res, {ErrorStatus, ErrorIndex, varbind_to_names_enums(Varbinds), Remaining}};
translate_msg(Other) ->
    Other.


varbind_to_names_enums([]) ->
    [];
varbind_to_names_enums([{varbind,OID, 'OBJECT IDENTIFIER', Value, N}|T]) ->
    [{varbind,oid_to_names(OID), 'OBJECT IDENTIFIER', oid_to_names(Value), N}|varbind_to_names_enums(T)];
varbind_to_names_enums([{varbind,OID, 'INTEGER', Value, N}|T]) ->
    Translated = oid_to_names(OID),
    [{varbind,Translated, 'INTEGER', integer_to_enum(OID, Value), N}|varbind_to_names_enums(T)];
varbind_to_names_enums([{varbind,OID, Type, Value, N}|T]) ->
    [{varbind,oid_to_names(OID), Type, Value, N}|varbind_to_names_enums(T)].

oid_to_names(help) ->
    io:format("~n----------------------------------------------------------------------------------------~n"
	      "oid_to_names(OID) -> Result~n"
	      "Input:       Dir              [integer,...]~n"
	      "Output:      Result           [atom | integer,...]~n"
	      "Exceptions:  ~n"
	      "Description: Converts OID to names~n"
	      "exampel:     xmgr:oid_to_names([1,3,6,1,4,1,193,177,2,2,5,0,3]).~n"
	      "             [1,3,6,1,4,enterprises,ericsson,177,2,2,genSysMgr,~n"
	      "              sysMgrNotifications,rootLoggedIn]~n"
              "----------------------------------------------------------------------------------------~n");
oid_to_names(OID) ->
    oid_to_names([],OID,[]).

oid_to_names(OID,[],R) ->
    R++oid_to_name(OID);
oid_to_names([],[H|T],R) ->
    oid_to_names([H],T,R);
oid_to_names(OID,[H|T],R) ->
    oid_to_names(OID++[H],T,R++oid_to_name(OID)).

oid_to_name(OID) ->
    case snmpm:oid_to_name(OID) of
        {ok,R} -> 
            [R];
        {error,not_found} -> 
            [lists:last(OID)]
    end.


integer_to_enum(Names,Enum) ->
    lookup_enum(lists:reverse(names_to_oid(Names)),Enum).

lookup_enum(RevOID,Int) ->
    case ets:lookup(xmgr_enums,{lists:reverse(RevOID),Int}) of
        [{_,Enum,_}] ->
            Enum;
        [] ->                    	    
	    [_|RevOID2] = RevOID, %Try one level down
	    case ets:lookup(xmgr_enums,{lists:reverse(RevOID2),Int}) of
		[{_,Enum,_}] ->
		    Enum;
		[] -> 
		    [_|RevOID3] = RevOID2, %Try two levels down
		    case ets:lookup(xmgr_enums,{lists:reverse(RevOID3),Int}) of
			[{_,Enum,_}] ->
			    Enum;
			[] -> 
			    Int
		    end
	    end
    end.

resolve_msg(help) ->
    io:format("~n----------------------------------------------------------------------------------------~n"
	      "resolve_msg(Msg) -> Result~n"
	      "Input:       Msg           [{[atom |integer,...],Type,Value}]~n"
              "             Type          See OTP snmp. If i, Value will be translated from enum to integer~n"
              "             Value         See OTP snmp.~n"
	      "Output:      Result        [{[integer,...},Type,Value}]~n"
	      "Exceptions:  ~n"
	      "Description: Converts a names and enums to integers before sending on snmp~n"
	      "exampel:     xmgr:resolve_msg([{[1,3,6,1,4,enterprises,ericsson,177,2,2,genShelfMIB,shelf,~n"
	      "                                 shelfObjects,shelfMgrAutonomousMode,0],i,autonomous}]).~n"
	      "[{[1,3,6,1,4,1,193,177,2,2,1,2,1,7,0],i,1}]~n"
              "----------------------------------------------------------------------------------------~n");
resolve_msg([]) ->
    [];
resolve_msg([{Names, i, Enum}|T]) ->
    OID = names_to_oid(Names),
    [{OID,i,enum_to_integer(OID,Enum)}|resolve_msg(T)];
resolve_msg([Msg|T]) ->
    [Msg|resolve_msg(T)].


names_to_oid(help) ->
    io:format("~n----------------------------------------------------------------------------------------~n"
	      "names_to_oid(Msg) -> Result~n"
	      "Input:       Msg           [atom |integer,...]~n"
	      "Output:      Result         [integer,...]~n"
	      "Exceptions:  ~n"
	      "Description: Converts a names to integers~n"
	      "exampel:     xmgr:names_to_oid([1,3,6,1,4,enterprises,ericsson,177,2,2,genShelfMIB,shelf,~n"
	      "                                shelfObjects,shelfMgrAutonomousMode,0]).~n"
	      "             [1,3,6,1,4,1,193,177,2,2,1,2,1,7,0]~n"
              "----------------------------------------------------------------------------------------~n");
names_to_oid(Names) ->
    name_to_oid(lists:reverse(Names),[]).

name_to_oid([], R) ->
    lists:reverse(R);
name_to_oid([H|T], R) when is_integer(H) ->
    name_to_oid(T, R ++ [H]);
name_to_oid([H|T], R) when is_atom(H) ->
    case snmpm:name_to_oid(H) of
        {ok,[Res]} -> 
            Res ++ lists:reverse(R);
        {error,not_found} -> 
            [H|T] ++ lists:reverse(R)
    end.


enum_to_integer(help) ->
    io:format("~n----------------------------------------------------------------------------------------~n"
	      "enum_to_integer(List,Enum) -> Result~n"
	      "Input:       List           Names | OID ~n"
              "             Names          [atom |integer,...]~n"
              "             OID            [integer,...]~n"
	      "             Enum           atom~n"
	      "Output:      Result         integer~n"
	      "Exceptions:  ~n"
	      "Description: Converts a enum to integer~n"
	      "exampel:     xmgr:enum_to_integer([1,3,6,1,4,enterprises,ericsson,177,2,2,genShelfMIB,shelf,~n"
	      "                                   shelfObjects,shelfMgrAutonomousMode,0],autonomous).~n"
	      "             1~n"
              "----------------------------------------------------------------------------------------~n").
enum_to_integer(Names,Enum) ->
    lookup_integer(lists:reverse(names_to_oid(Names)),Enum).

lookup_integer(RevOID,Enum) ->
    [_|RevOID2] = RevOID, %Try one level down
    case ets:match(xmgr_enums,{{lists:reverse(RevOID2),'$1'},Enum,'_'}) of
        [[Integer]] ->            
            Integer;
        _ -> 
	    [_|RevOID3] = RevOID2, %Try two levels down
	    case ets:match(xmgr_enums,{{lists:reverse(RevOID3),'$1'},Enum,'_'}) of
		[[Integer]] ->            
		    Integer;
		_ -> 
%		    io:format("Could not convert ~p ~p~n",[lists:reverse(RevOID), Enum]),
		    Enum
	    end
    end.

          
%% ========================================================================
%% SNMPM user callback functions
%% ========================================================================

handle_error(ReqId, Reason, UserData) ->
    ?MODULE ! {handle_error,ReqId, Reason, UserData},
    ignore.

handle_agent(_snmpUDPDomain, {Addr, Port}, Type, SnmpInfo, UserData) -> % New format in OTP snmp 5.0
    handle_agent(Addr, Port, Type, SnmpInfo, UserData);

handle_agent(Addr, _Port, Type, SnmpInfo, UserData) when Type==inform;
							 Type==trap ->
    case snmpm:which_agents() of
	[] -> % If no agents specified -> accept all traps
%	    ?MODULE:(callback_type(Type))(make_ip_string(Addr), SnmpInfo, UserData),
	    ?MODULE:(callback_type(Type))(inet:ntoa(Addr), SnmpInfo, UserData),
	    ignore;
	List -> 
	    case is_agent(Addr, List) of
		ok -> % If agents are specified -> only accept traps from those agents
%		    ?MODULE:(callback_type(Type))(make_ip_string(Addr), SnmpInfo, UserData),
		    ?MODULE:(callback_type(Type))(inet:ntoa(Addr), SnmpInfo, UserData),
	    ignore;
		nok ->
		    ignore
	    end
    end;
handle_agent(Addr, Port, Type, SnmpInfo, UserData) ->
    ?MODULE ! {handle_agent,Addr, Port, Type, SnmpInfo, UserData},
    ignore.

-spec is_agent({integer(), integer(), integer(), integer()}, [string()]) -> nok | ok.

is_agent(_Addr, []) -> 
    nok;
is_agent(Addr, [Agent | T]) ->
    case agent_address_as_tuple(Agent) of
	undefined ->
	    is_agent(Addr, T);
	Addr ->
	    ok;
	_OtherAddr ->
	    is_agent(Addr, T)
    end.


-spec agent_address_as_tuple(string()) -> 
	  {integer(), integer(), integer(), integer()} | 
	      undefined.
	  
agent_address_as_tuple(DottedAddress) ->
    case snmpm:agent_info(DottedAddress, address) of
	{ok, Tuple} ->
	    Tuple;
	{error, _Reason} ->
	    case snmpm:agent_info(DottedAddress, taddress) of
		{ok, {Tuple, _Port}} -> 
		    Tuple;
		_ ->
		    undefined
	    end
    end.


callback_type(trap) -> handle_trap;
callback_type(inform) -> handle_inform.


handle_pdu(TargetName, ReqId, SnmpResponse, UserData) ->
    ?MODULE ! {handle_pdu,TargetName, ReqId, SnmpResponse, UserData},
    ignore.

handle_trap(TargetName, SnmpTrap, UserData) ->
%    io:format("############## TRAP~n"),
    ?MODULE ! {handle_trap,TargetName, SnmpTrap, UserData},
    ignore.

handle_inform(TargetName, SnmpInform, UserData) ->
%    io:format("############## INFORM~n"),
    case whereis(?MODULE) of
	undefined ->
	    error_logger:error_report(
	      [{no_registered_process, ?MODULE},
	       {mfa, {?MODULE, handle_inform, [TargetName, SnmpInform, UserData]}}
	      ]),
	    erlang:error(no_registered_process, [TargetName, SnmpInform, UserData]);
	_ ->
	    ?MODULE ! {handle_inform,TargetName, SnmpInform, UserData},
	    no_reply
    end.

handle_report(TargetName, SnmpReport, UserData) ->
    ?MODULE ! {handle_report,TargetName, SnmpReport, UserData},
    ignore.

%% make_ip_string(T) when is_tuple(T) ->
%%     make_ip_string(tuple_to_list(T));
%% make_ip_string(L) ->
%%     string:strip(lists:flatten([integer_to_list(X)++"."||X<-L]),right,$.).

