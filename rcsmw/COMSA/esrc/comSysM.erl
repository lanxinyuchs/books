%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comSysM.erl %
%%% @author eivmiha
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/R11A/R12A/2

%%% @doc ==Agent implementation for System management==
%%% This is the agent implementation for System management, NTP and Schema

-module(comSysM).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/R11A/R12A/2').
-date('2017-11-22').
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
%%% -----      -------    --------    ------------------------
%%% R1A/1      2012-01-10 etxbjca     Created
%%% R2A/1      2012-12-01 etxjotj     Macro from Common library used
%%% R2A/6      2013-04-04 erarafo     Support for upgrade
%%% R2A/7      2013-05-02 etxjotj     Regenerate ntp conf at upgrade
%%% R2A/16     2014-02-18 etxlg       Don't regenerate ntp conf at upgrade
%%% R2A/18     2014-02-27 etxarnu     SysM 3.1 : netconfSsh,Tls
%%% R2A/19     2014-02-28 etxlg       Populate netconfTls=1,netconfSsh=1
%%%				      Add api for netconf/ssh
%%% R2A/20     2014-05-20 etxjotj     Support for ECIM TimeM
%%% R2A/25     2014-06-16 etxlg       Subscription for tls_config
%%% R2A/26     2014-06-24 etxlg       Subscription for ssh_config
%%% R2A/27     2014-07-23 etxjotj     Fix of HS79545, DateAndTime MO
%%% R2A/28     2014-09-04 etxlg       Fix of HS73150, DSCP
%%% R2A/29     2014-09-18 etxberb     Added validate/3.
%%% R2A/30     2014-09-23 etxpejn     Added call to sysSftp TR HS89846
%%% R2A/31     2014-10-13 etxlg       check netconfTLS attributes TR HT12910
%%% R2A/32     2014-10-16 etxlg       dialyzer-reported error fixed
%%% R3A/1      2014-11-28 etxberb     Added values/1.
%%% R3A/2      2014-12-10 etxarnu     Added values clause for {9,BinVal}
%%% R3A/3      2015-01-09 etxjotj     Improved sftp handling
%%% R3A/4      2015-03-17 etxtory     getMoAttributes fix
%%% R3A/5      2015-04-23 etxlg       TR HT65672 try to ensure TLS or SSH works
%%% R3A/6      2015-04-27 etxarnu     Adaptation to new ComSysM (CliSsh/TlsSsh)
%%% R4A/6      2015-07-08 etxjotj     Upgrade to SysM 3.3
%%% R4A/7      2015-07-08 etxjotj     Bugfix
%%% R4A/8      2015-07-08 etxjotj     Correct handling of port default values
%%% R4A/9      2015-08-24 eolaand     Create OamTrafficClass=1
%%% R4A/10     2015-08-24 eolaand     Add get fcn for OamTrafficClass
%%% R4A/11     2015-08-24 eolaand     Subscription for OamTrafficClass
%%% R4A/13     2015-09-02 eolaand     Call OOT in prepareTransaction and finish
%%% R4A/14     2015-09-25 eolaand     Use default value 0 for dscp
%%% R4A/15     2015-10-16 eolaand     Prepare for HU25873
%%% R4A/16     2015-10-22 eolaand     Add mirroring of deprecated OAP
%%%                                   attributes for HU25873.
%%% R4A/17     2015-10-22 eolaand     Correct DN for CliSsh
%%% R4A/18     2015-12-03 etxjotj     HU42180: Update schema locations
%%% R6A/1      2016-07-15 etxpeno     HV10814: Create mnesia entries at upgrade
%%% R8A/1      2017-01-17 estjako     ssh_sftp module calls changed to ftpI

%%% R9A/1      2017-02-06 eivmiha     http_uri:parse swiitched for ftpI:parse, added 
%%%                                   additional support for FTPES
%%% R12A/1     2017-11-15 emarnek     HW33598
%%% R12A/2     2017-11-22 eivmiha     Added FtpServer
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([finish/3]).
-export([prepare/3,commit/3,validate/3]). %These are NOT used (bleh)
-export([action/3]).
-export([existsMo/2,
         countMoChildren/3,
         getMoAttributes/3,
         setMoAttributes/3,
         action/4,
         createMo/5]).
-export([init_data/1]).
-export([prepareTransaction/2]).

-export([get_netconf_tls_config/0]).
-export([get_netconf_ssh_config/0]).
-export([get_cli_tls_config/0]).
-export([get_cli_ssh_config/0]).
-export([get_oam_traffic_class_config/0]).
-export([get_object_config/1, get_object_config/2]).
-export([get_mirror_set_attributes/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("comTypes.hrl").
-include("RcsSysM.hrl").
-include("RcsFileTPM.hrl").
-include("RcsTimeM.hrl").
-include("ComsaTypes.hrl").

-define(OAP_DN,
	<<"ManagedElement=1,SystemFunctions=1,SysM=1,OamAccessPoint=1">>).
-define(ALT_OAP_DN,
	<<"ManagedElement=1,SystemFunctions=1,SysM=1,"
	  "OamAccessPoint=Alternative">>).
-define(OTC_DN,
	<<"ManagedElement=1,SystemFunctions=1,SysM=1,OamTrafficClass=1">>).
-define(NC_SSH_DN,
	<<"ManagedElement=1,SystemFunctions=1,SysM=1,NetconfSsh=1">>).
-define(CLI_SSH_DN,
	<<"ManagedElement=1,SystemFunctions=1,SysM=1,CliSsh=1">>).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%% @doc Returns true if the specified instance exists.
existsMo(DnRev, _) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).

%%% @doc Returns the number of MO instances of given class
%%% directly below the specified parent.
countMoChildren(DnRev, Class, _) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(Class))).

%%% @doc Gets MO attribute values.
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.
getMoAttributes(AttrNames, DnRev, TxHandle) ->
    case comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))) of
	true  ->
	    [getMoAttribute([AN | DnRev], TxHandle) || AN <- AttrNames];
	false ->
	    []
    end.

%getMoAttribute([Attribute|DnRev], TxHandle) ->
%% HS79545 fix
getMoAttribute([<<"localDateTime">>, _, <<"DateAndTime">>|_], _) ->
    TimeStr = comsaLib:iso_time(os:timestamp(), extended_zonefree),
    ?STRING(list_to_binary(TimeStr));
getMoAttribute([<<"dateTimeOffset">>, _, <<"DateAndTime">>|_], _) ->
    DiffStr = comsaLib:time_offset(os:timestamp()),
    ?STRING(list_to_binary(DiffStr));
%%% HS79545 ends here
getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

%nextMo(Dn, Key, TxHandle) ->
nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).

setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).

createMo([ClassName | ParentDnRev],
	 _KeyAttrName,
	 KeyValue,
	 InitAttrs,
	 _TransId) ->
    Table = table(binary_to_list(ClassName)),
    comsaGeneric:create(Table,
			ParentDnRev,
			KeyValue,
			values(InitAttrs),
			types(Table)).

createMo([Class|ParentDnRev], _, IxValueB, _) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table, ParentDnRev, IxValueB).

deleteMo(DnRev=[_,<<"NtpServer">>|_], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table);
deleteMo(_, _) ->
    ok.

prepareTransaction(Objs, Tx) ->
    case ootComSysM:prepareTransaction(Objs, Tx) of
	ok ->
	    doPrepareTransaction(Objs);
	Abort ->
	    Abort
    end.

get_netconf_tls_config() ->
    get_object_config(netconfTls).

get_cli_tls_config() ->
    get_object_config(cliTls).

get_netconf_ssh_config() ->
    get_object_config(netconfSsh).

get_cli_ssh_config() ->
    get_object_config(cliSsh).

get_oam_traffic_class_config() ->
    get_object_config(oamTrafficClass).

get_object_config(Object) ->
    get_object_config(Object, {"1","1","1","1"}).

get_object_config(Object, Id) ->
  Fun = fun() -> mnesia:read({Object, Id}) end,
    case mnesia:transaction(Fun) of
        {atomic, [Obj]} ->
            Fields = attributes(Object),
            [enum_value_to_atom({Key, Value}) ||
                {Key, Value} <- lists:zip(Fields, tl(tuple_to_list(Obj))),
                Value /= undefined];
        {atomic, []} ->
            [];
        {aborted, Reason} ->
            sysInitI:error_report(
              [{mfa, {mnesia, read, {Object, Id}}},
               {aborted, Reason}]),
            []
    end.

table("SysM") -> sysM;
table("NtpServer") -> ntpServer;
table("Schema") -> sysMSchema;
table("FileTPM") -> fileTPM;
table("FtpTls") -> ftpTls;
table("FtpTlsServer") -> ftpTlsServer;
table("NetconfTls") -> netconfTls;
table("NetconfSsh") -> netconfSsh;
table("CliTls") -> cliTls;
table("CliSsh") -> cliSsh;
table("OamTrafficClass") -> oamTrafficClass;
table("Sftp") -> sftp;
table("SftpServer") -> sftpServer;
table("FtpServer") -> ftpServer;
table("TimeM") -> timeM;
table("DateAndTime") -> dateAndTime.


types(sysM) -> ?sysM_types;
types(ntpServer) -> ?ntpServer_types;
types(sysMSchema) -> ?sysMSchema_types;
types(fileTPM) -> ?fileTPM_types;
types(ftpTls) -> ?ftpTls_types;
types(ftpTlsServer) -> ?ftpTlsServer_types;
types(netconfTls) -> ?netconfTls_types;
types(netconfSsh) -> ?netconfSsh_types;
types(cliTls) -> ?cliTls_types;
types(cliSsh) -> ?cliSsh_types;
types(oamTrafficClass) -> ?oamTrafficClass_types;
types(sftp) -> ?sftp_types;
types(sftpServer) -> ?sftpServer_types;
types(ftpServer) -> ?ftpServer_types;
types(timeM) -> ?timeM_types;
types(dateAndTime) -> ?dateAndTime_types.

attributes(sysM) -> record_info(fields, sysM);
attributes(ntpServer) -> record_info(fields, ntpServer);
attributes(sysMSchema) -> record_info(fields, sysMSchema);
attributes(fileTPM) -> record_info(fields, fileTPM);
attributes(ftpTls) -> record_info(fields, ftpTls);
attributes(ftpTlsServer) -> record_info(fields, ftpTlsServer);
attributes(netconfTls) -> record_info(fields, netconfTls);
attributes(netconfSsh) -> record_info(fields, netconfSsh);
attributes(cliTls) -> record_info(fields, cliTls);
attributes(cliSsh) -> record_info(fields, cliSsh);
attributes(oamTrafficClass) -> record_info(fields, oamTrafficClass);
attributes(sftp) -> record_info(fields, sftp);
attributes(sftpServer) -> record_info(fields, sftpServer);
attributes(ftpServer) -> record_info(fields, ftpServer);
attributes(timeM) -> record_info(fields, timeM);
attributes(dateAndTime) -> record_info(fields, dateAndTime).

values([{Name, {9, Value}} | Tail]) ->
    [{Name, binary_to_list(Value)} | values(Tail)];
values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

%This is NOT used
validate(_DN, User, _Tx) ->
    {ok,User}.
%This is NOT used
prepare(_DN, User, _Tx) ->
    {ok,User}.
%This is NOT used
commit(_DN, User, _Tx) ->
    {ok,User}.

finish(DN, _User, Tx) ->
    Class = comsaGeneric:class(DN),
    ootComSysM:finish(Class, Tx),
    case Class of
	"NetconfTls" ->
	    omc_api:netconf_tls_notify(),
	    ok;
	"NetconfSsh" ->
	    omc_api:netconf_ssh_notify(),
	    ok;
        "CliTls" ->
	    omc_api:cli_tls_notify(),
	    ok;
	"CliSsh" ->
	    omc_api:cli_ssh_notify(),
	    ok;
	"OamTrafficClass" ->
	    omc_api:oam_traffic_class_notify(),
	    ok;
	_ ->
	    ok
   end.

action(Name, ReversedDn, NamedParams, TransId) ->
    Parameters =
        case Name of
            <<"export">> ->
                [getTypeValue(<<"uri">>, NamedParams),
                 getTypeValue(<<"password">>, NamedParams)]
        end,
    action([Name|ReversedDn], Parameters, TransId).

action([<<"export">>, ModelBin|_DnRev], Parameters, _) ->
    [{9, UriBin},{9, PasswdBin}] = Parameters,
    export_model(binary_to_list(ModelBin), binary_to_list(UriBin),
		 binary_to_list(PasswdBin)).

getTypeValue(Name, NamedParams) ->
    case lists:keyfind(Name, 1, NamedParams) of
        false ->
            erlang:error(no_such_parameter, [Name, NamedParams]);
        {_, TypeValue} when is_tuple(TypeValue) ->
            TypeValue;
        {_, [TypeValue]} ->
            TypeValue;
        {_, [_|_]=TypeValues} ->
            erlang:error(multiple_parameter_values, [Name, TypeValues])
    end.

init_data(upgrade) ->
    upg_tab(sysM),
    upg_tab(sftp),
    upg_tab(ftpServer),
    upg_tab(fileTPM),
    upg_tab(sysMSchema),
    upg_tab(ntpServer),
    upg_tab(netconfTls),
    upg_tab(netconfSsh),
    %% FIXME CLI If Netconf configured and Cli didn't exist parameters should
    %% be cloned from Netconf to Cli?
    upg_tab(cliTls),
    upg_tab(cliSsh),
    upg_tab(oamTrafficClass),
    upg_tab(timeM),
    upg_tab(dateAndTime);
    %comsaServer:generate_ntp_conf();
init_data(fromScratch) ->
    [mnesia:dirty_write(X)||
	X<-
	    [#sysM{sysMId={"1","1","1"}},
	     #fileTPM{fileTPMId={"1","1","1","1"}},
	     #sftp{sftpId={"1","1","1","1","1"}},
         #ftpServer{ftpServerId = {"1","1", "1", "1", "1"},
                    idleTimer = ?ftpServer_idleTimer_default},
	     #netconfTls{netconfTlsId={"1","1","1","1"},
			 administrativeState=?BasicAdmState_LOCKED,
			 port = sysEnv:get_port_conf(netconf_tls)},
	     #netconfSsh{netconfSshId={"1","1","1","1"},
			 administrativeState=?BasicAdmState_UNLOCKED,
			 port = sysEnv:get_port_conf(netconf)},
	     #cliTls{cliTlsId={"1","1","1","1"},
		     administrativeState=?BasicAdmState_LOCKED,
		     port = sysEnv:get_port_conf(cli_tls)},
             #cliSsh{cliSshId={"1","1","1","1"},
		     administrativeState=?BasicAdmState_UNLOCKED,
		     port = sysEnv:get_port_conf(cli)},
             #oamTrafficClass{oamTrafficClassId={"1","1","1","1"},
			      name = "Low",
			      dscp = 0},
	     #timeM{timeMId={"1","1","1","1"}},
	     #dateAndTime{dateAndTimeId={"1","1","1","1","1"},
			  tzRevision="No timezone database is installed"}
	]].


get_mirror_set_attributes({setMoAttributes, _TransId, DN, _AttrVals} = SA)
  when DN =:= ?OAP_DN;
       DN =:= ?ALT_OAP_DN;
       DN =:= ?OTC_DN;
       DN =:= ?NC_SSH_DN;
       DN =:= ?CLI_SSH_DN ->
    ootComSysM:get_mirror_set_attributes(SA);

get_mirror_set_attributes({setMoAttribute, TransId, DN, AttrName, AttrValue}) ->
    AttrVals = [{AttrName, AttrValue}],
    get_mirror_set_attributes({setMoAttributes, TransId, DN, AttrVals});

get_mirror_set_attributes(_SA) ->
    [].


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

upg_tab(cliTls) ->
    try
	case ets:lookup(olddb, cliTls) of
	    [] -> %% The cliTls didn't exist before copy from netconfTls
		AllObjects = swmI:all_objects(netconfTls),
		lists:foreach(
		  fun({netconfTls, Index, Nc, Tcat, Admin}) ->
			  mnesia:dirty_write(
			    #cliTls{
			       cliTlsId            = Index,
			       nodeCredential      = Nc,
			       trustCategory       = Tcat,
			       administrativeState = Admin});
		     (Obj) ->
			  mnesia:dirty_write(Obj)
		  end, AllObjects),
		ok;
	    [{cliTls, _Ets}] ->
		case swmI:is_attributes_in_old_record(cliTls, [port]) of
		    true ->
			swmI:copy_old_table(cliTls);
		    false ->
			Added = [{port, sysEnv:get_port_conf(cli_tls)}],
			[begin
			     Record = swmI:transform_obj(Obj, Added),
			     mnesia:dirty_write(Record)
			 end||Obj<-swmI:all_objects(cliTls)]
		end
	end
    catch
        ErrClass : ErrReason ->
            Err = {ErrClass, [ErrReason | erlang:get_stacktrace()]},
            sysInitI:error_report([{?MODULE, upg_tab, [cliTls]},
                    Err]),
            exit(ErrReason)
    end;
upg_tab(cliSsh) ->
    try
	case ets:lookup(olddb, cliSsh) of %% FIXME, swmI:is_table ??
	    [] -> %% The cliSsh didn't exist before copy from netconfSsh
		AllObjects = swmI:all_objects(netconfSsh),
		lists:foreach(
		  fun({netconfSsh, Index, Admin}) ->
			  mnesia:dirty_write(
			    #cliSsh{
			       cliSshId            = Index,
			       administrativeState = Admin});
		     (Obj) ->
			  mnesia:dirty_write(Obj)
		  end, AllObjects),
		ok;
	    [{cliSsh, _Ets}] ->
		case swmI:is_attributes_in_old_record(cliSsh, [port]) of
		    true ->
			swmI:copy_old_table(cliSsh);
		    false ->
			Added = [{port, sysEnv:get_port_conf(cli)}],
			[begin
			     Record = swmI:transform_obj(Obj, Added),
			     mnesia:dirty_write(Record)
			 end||Obj<-swmI:all_objects(cliSsh)]
		end
	end
    catch
        ErrClass : ErrReason ->
            Err = {ErrClass, [ErrReason | erlang:get_stacktrace()]},
            sysInitI:error_report([{?MODULE, upg_tab, [cliSsh]},
                    Err]),
            exit(ErrReason)
    end;
upg_tab(netconfTls) ->
    case swmI:is_attributes_in_old_record(netconfTls, [port]) of
	true ->
	    swmI:copy_old_table(netconfTls);
	false ->
	    Added = [{port, sysEnv:get_port_conf(netconf_tls)}],
	    [begin
		 Record = swmI:transform_obj(Obj, Added),
		 mnesia:dirty_write(Record)
	     end||Obj<-swmI:all_objects(netconfTls)]
    end;
upg_tab(netconfSsh) ->
    case swmI:is_attributes_in_old_record(netconfSsh, [port]) of
	true ->
	    swmI:copy_old_table(netconfSsh);
	false ->
	    Added = [{port, sysEnv:get_port_conf(netconf)}],
	    [begin
		 Record = swmI:transform_obj(Obj, Added),
		 mnesia:dirty_write(Record)
	     end||Obj<-swmI:all_objects(netconfSsh)]
    end;
upg_tab(sftp) ->
    %% Always recreate this system created MO (TR HV10814)
    mnesia:dirty_write(#sftp{sftpId={"1","1","1","1","1"}});
upg_tab(ftpServer) ->
    case ets:lookup(olddb, ftpServer) of
        [{ftpServer, Ets}] -> 
            EtsList= ets:tab2list(Ets),
            lists:foreach(
                    fun(#ftpServer{} = FtpServer) ->
                            mnesia:dirty_write(FtpServer)
                    end, EtsList);
        _ ->
            mnesia:dirty_write(#ftpServer{ftpServerId = {"1","1", "1", "1", "1"},
                                         idleTimer = ?ftpServer_idleTimer_default})
    end;
upg_tab(fileTPM) ->
    %% Always recreate this system created MO (TR HV10814)
    mnesia:dirty_write(#fileTPM{fileTPMId={"1","1","1","1"}});
upg_tab(Tab) ->
    swmI:copy_old_table(Tab).


export_model(Model, Uri, Passwd) ->
    try do_export_model(Model, Uri, Passwd) of
	_ ->
	    undefined
    catch throw:{write_file, R} ->
	    Str = io_lib:format("Write failed: ~p~n",[R]),
	    close_channel(Uri),
	    {error, list_to_binary(Str)};
	  throw:{read_file, R} ->
	    Str = io_lib:format("Read failed: ~s~n",
				[file:format_error(R)]),
	    close_channel(Uri),
	    {error, list_to_binary(Str)};
	  throw:{start_channel, Str} ->
	    close_channel(Uri),
	    {error, list_to_binary(Str)};
	%%HW33598 Cleaner output
	  error:{badmatch,{error,{no_default_port,Protocol,_Path}}} ->
	    sysInitI:error_report("No default port for protocol ~p~n",[Protocol]),
	    Str = io_lib:format("No default port for protocol ~p~n",[Protocol]),
	    {error, list_to_binary(Str)};
	  error:{badmatch,{error,{malformed_url,_Protocol,_Path}}} ->
		sysInitI:error_report("Wrong URI format"),
	    {error, list_to_binary("Wrong URI format")};
	  T:E ->
	    sysInitI:error_report(
	      [{T,E},{?MODULE, export_model, [Model,Uri]}]),
	    Str = io_lib:format("~p~n",[E]),
	    close_channel(Uri),
	    {error, list_to_binary(Str)}
    end.

close_channel(Uri) ->
    Pid = get(channelPid),
    CRef = get(connectionRef),
    case ftpI:parse_uri(Uri) of
        {ok, {Proto, _User, _Host, _Port, _RemoteDir, _Query}} -> ftpI:stop_channel(Proto, Pid, CRef);
        _Error -> ok
    end.

do_export_model(Model, Uri, Passwd) ->
    {ok, {Proto, User, Host, Port, RemoteDir, _Query}} = ftpI:parse_uri(Uri),
    {ok, Pid, CRef} = start_channel(Host, Port, User, Passwd, Proto),
    put(channelPid, Pid),
    put(connectionRef, CRef),
    {atomic, [LocationObj]} =
	mnesia:transaction(fun() ->
				   mnesia:read({comsaSchemaLocations, Model})
			   end),
    LocalPath = LocationObj#comsaSchemaLocations.path,
    {ok, Binary} = case file:read_file(LocalPath) of
		       {ok, B} -> {ok, B};
		       {error, E1} -> throw({read_file, E1})
		   end,
    RemotePath = case filename:extension(RemoteDir) of
		     ".xml" -> RemoteDir;
		     _ ->
			 filename:join(RemoteDir, filename:basename(LocalPath))
		 end,
    case ftpI:write_file(Proto, Pid, RemotePath, Binary) of
	ok -> ok;
	{error, no_such_file} -> throw("No such file or directory");
	{error, Reason} -> throw({write_file, Reason})
    end,
    ftpI:stop_channel(Proto, Pid, CRef),
    garbage_collect().

doPrepareTransaction([]) ->
    ok;
doPrepareTransaction([{Dn, Entry} | T]) ->
    try {comsaGeneric:class(Dn), Entry} of
	{"NetconfSsh", #netconfSsh{administrativeState = AdmState}}
        when AdmState =:= ?BasicAdmState_LOCKED ->
	    case is_netconf_tls_operational() of
		true ->
		    doPrepareTransaction(T);
		false ->
		    {abort, "Locking refused - TLS service is not configured/"
			    "locked."}
	    end;
	{"NetconfTls", #netconfTls{administrativeState = AdmState}}
        when AdmState =:= undefined ->
	    %possibly initial state
	    doPrepareTransaction(T);
	{"NetconfTls", #netconfTls{administrativeState = AdmState}}
        when AdmState =:= ?BasicAdmState_LOCKED ->
	    case is_netconf_ssh_operational() of
		true ->
		    doPrepareTransaction(T);
		false ->
		    {abort, "Locking refused - SSH service is locked."}
	    end;
	{"NetconfTls", #netconfTls{nodeCredential = undefined}} ->
            case is_netconf_ssh_operational() of
		true ->
		    doPrepareTransaction(T);
		false ->
		    {abort, "The SSH service is locked, refusing to remove "
			    "the nodeCredential as this would lock you out of "
			    "the Managed Element."}
	    end;
	{"NetconfTls", #netconfTls{trustCategory = undefined}} ->
	    case is_netconf_ssh_operational() of
		true ->
		    doPrepareTransaction(T);
		false ->
		    {abort, "The SSH service is locked, refusing to remove "
			    "the trustCategory as this would lock you out of "
			    "the Managed Element."}
	    end;
	{"NetconfTls", #netconfTls{nodeCredential = Nc, trustCategory = Tc}} ->
	    case doPTNetconfTls(mnesia:is_transaction(), Nc, Tc) of
		ok ->
		    doPrepareTransaction(T);
		{error, What} ->
		    {abort, What}
	    end;
        {"CliSsh", #cliSsh{administrativeState = AdmState}}
        when AdmState =:= ?BasicAdmState_LOCKED ->
	    case is_cli_tls_operational() of
		true ->
		    doPrepareTransaction(T);
		false ->
		    {abort, "Locking refused - TLS service is not configured/"
			    "locked."}
	    end;
	{"CliTls", #cliTls{administrativeState = AdmState}}
        when AdmState =:= undefined ->
            %possibly initial state
	    doPrepareTransaction(T);
	{"CliTls", #cliTls{administrativeState = AdmState}}
        when AdmState =:= ?BasicAdmState_LOCKED ->
	    case is_cli_ssh_operational() of
		true ->
		    doPrepareTransaction(T);
		false ->
		    {abort, "Locking refused - SSH service is locked."}
	    end;
	{"CliTls", #cliTls{nodeCredential = undefined}} ->
	    case is_cli_ssh_operational() of
		true ->
		    doPrepareTransaction(T);
		false ->
		    {abort, "The SSH service is locked, refusing to remove "
			    "the nodeCredential as this would lock you out of "
			    "the Managed Element."}
	    end;
	{"CliTls", #cliTls{trustCategory = undefined}} ->
	    case is_cli_ssh_operational() of
		true ->
		    doPrepareTransaction(T);
		false ->
		    {abort, "The SSH service is locked, refusing to remove "
			    "the trustCategory as this would lock you out of "
			    "the Managed Element."}
	    end;
	{"CliTls", #cliTls{nodeCredential = Nc, trustCategory = Tc}} ->
	    case doPTCliTls(mnesia:is_transaction(), Nc, Tc) of
		ok ->
		    doPrepareTransaction(T);
		{error, What} ->
		    {abort, What}
	    end;
	_ ->
	    doPrepareTransaction(T)
    catch
	_:_ ->
	    doPrepareTransaction(T)
    end;
doPrepareTransaction([_ | T]) ->
    %because I do not fully understand the API here.../lg
    doPrepareTransaction(T).

doPTNetconfTls(true, Nc, Tc) ->
    case {doPTNetconfTls(nodeCredential, Nc), doPTNetconfTls(trustCategory, Tc)} of
	{ok, ok} -> ok;
	{_, {error, What}} -> {error, What};
	{{error, What}, _} -> {error, What}
    end;
doPTNetconfTls(false, _, _) ->
    ok.

doPTNetconfTls(_, undefined) -> ok;
doPTNetconfTls(Table, Key) ->
    try mnesia:read(Table, comsaGeneric:mo_to_key(Key)) of
	[] ->
	     {error, <<"The MO you try to set in ",
			(list_to_binary(atom_to_list(Table)))/binary,
			" does not exist.">>};
	_ -> ok
    catch
	_:_ ->
	     {error, <<"The MO you try to set in ",
			(list_to_binary(atom_to_list(Table)))/binary,
			" has illegal format or does not exist.">>}
    end.

doPTCliTls(true, Nc, Tc) ->
    case {doPTCliTls(nodeCredential, Nc), doPTCliTls(trustCategory, Tc)} of
	{ok, ok} -> ok;
	{_, {error, What}} -> {error, What};
	{{error, What}, _} -> {error, What}
    end;
doPTCliTls(false, _, _) ->
    ok.

doPTCliTls(_, undefined) -> ok;
doPTCliTls(Table, Key) ->
    try mnesia:read(Table, comsaGeneric:mo_to_key(Key)) of
	[] ->
	     {error, <<"The MO you try to set in ",
			(list_to_binary(atom_to_list(Table)))/binary,
			" does not exist.">>};
	_ -> ok
    catch
	_:_ ->
	     {error, <<"The MO you try to set in ",
			(list_to_binary(atom_to_list(Table)))/binary,
			" has illegal format or does not exist.">>}
    end.

is_netconf_tls_operational() ->
    Wild = mnesia:table_info(netconfTls, wild_pattern),
    case mnesia:match_object(Wild) of
	[#netconfTls{administrativeState = Adm,
		     trustCategory = Tc,
		     nodeCredential = Nc}] when
	    Adm =:= ?BasicAdmState_UNLOCKED,
	    Tc =/= undefined, Nc =/= undefined ->
	    true;
	_  ->
	    false
    end.

is_cli_tls_operational() ->
    Wild = mnesia:table_info(cliTls, wild_pattern),
    case mnesia:match_object(Wild) of
	[#cliTls{administrativeState = Adm,
		     trustCategory = Tc,
		     nodeCredential = Nc}] when
	    Adm =:= ?BasicAdmState_UNLOCKED,
	    Tc =/= undefined, Nc =/= undefined ->
	    true;
	_  ->
	    false
    end.

is_netconf_ssh_operational() ->
    Wild = mnesia:table_info(netconfSsh, wild_pattern),
    case mnesia:match_object(Wild) of
	[#netconfSsh{administrativeState = ?BasicAdmState_UNLOCKED}] ->
	    true;
	_  ->
	    false
    end.

is_cli_ssh_operational() ->
    Wild = mnesia:table_info(cliSsh, wild_pattern),
    case mnesia:match_object(Wild) of
	[#cliSsh{administrativeState = ?BasicAdmState_UNLOCKED}] ->
	    true;
	_  ->
	    false
    end.


start_channel(Host, Port, User, Passwd, Proto) ->
    Options = [{connect_timeout, 30000}],
    case ftpI:start_channel(Proto, Host, Port, User, Passwd, Options) of
	{ok, SP, C} ->
	    {ok, SP, C};
	{error, E1} when is_atom(E1) ->
	    Info1 =
		case E1 of
		    etimedout ->
			"Cannot establish a connection to remote server";
		    timeout ->
			"Cannot establish a connection to remove server";
		    _ ->
			lists:flatten(io_lib:format("~p",[E1]))
		end,
	    throw({start_channel, Info1});
	{error, E1} ->
	    throw({start_channel, E1})
    end.

enum_value_to_atom({administrativeState, ?BasicAdmState_LOCKED}) ->
    {administrativeState, 'LOCKED'};
enum_value_to_atom({administrativeState, ?BasicAdmState_UNLOCKED}) ->
    {administrativeState, 'UNLOCKED'};
enum_value_to_atom(Any) -> Any.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
