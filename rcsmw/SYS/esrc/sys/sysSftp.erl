%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysSftp.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R9A/R11A/1

%%% @doc ==Secure file transfer service== 
%%% This module implements lean put/get operations using the ssh_sftp
%%% library.  However in reality there needs to be progress reporting
%%% for most such operations, which makes these functions unsuitable.

-module(sysSftp).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R9A/R11A/1').
-date('2017-10-17').
-author('etxberb').
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
%%% R1A/1      2012-02-22 etxjotj     Created
%%% R2A/1      2014-09-22 etxpejn     Created start_channel and stop_channel
%%% R2A/4      2014-09-25 etxpejn     Added check for host address in start_channel
%%% R2A/5      2014-10-06 etxpejn     Added check for pids in stop_channel
%%% R2A/6      2014-10-14 etxpejn     Skip IPv6 adr in get_info.
%%% R3A/1      2014-11-04 etxjotj     Disabled channel buffering
%%% R3A/2      2014-12-16 etxlg       Network namespacing, OaM part only
%%% R3A/3      2014-12-18 etxpejn     Added sftp vsn 3
%%% R3A/4      2015-02-11 etxlg       Network namespacing, also LMT
%%% R3A/5      2015-02-11 etxlg       filter out undefined from _old_ API
%%% R3A/6      2015-03-31 etxlg       No OamAP -> SFTP on LMT
%%% R4A/1      2015-09-10 etxlg       Alternate OaM
%%% R4A/2      2015-09-16 etxlg       Dedicated alternate SFTP
%%% R4A/3      2015-10-21 etxlg       Alternate OaM - revisit
%%% R4A/4      2015-11-10 etxlg       IPv6
%%% R6A/1      2016-08-30 etxarnu     TR: HV18754, use ootI:getaddr(Host)
%%% R6A/1      2016-09-01 etxarnu     TR: HV18754, Added {inet,inet6} to options
%%% R6A/3      2016-09-02 etxarnu     added catch to ootI:getaddr call
%%% R6A/4      2016-09-05 uabhgma     CSUC - Added ssh preferred algorithms.
%%% R7A/1      2016-09-08 etxarnu     Editorial change
%%% R7A/2      2016-09-09 etxarnu     More error cases handled in put_file
%%% R7A/3      2016-09-09 etxarnu     Fixed -spec for put_file
%%% R9A/1      2017-02-14 eolaand     Use ootI:getaddr to resolve if DNS 
%%%                                   instead of inet:parse_address 
%%% R11A/1     2017-10-17 etxberb    Adaptions to OTP20; Removed export_all.
%%% ----------------------------------------------------------
%%% 
%%%-compile([export_all]).
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([get_file/3, put_file/3]). 
-export([start_channel_only_alt/4]).
-export([start_channel_only_alt/5]).
-export([start_channel_with_alt/4]).
-export([start_channel_with_alt/5]).
-export([start_channel/4]).
-export([start_channel/5]).
-export([stop_channel/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% -type put_file(Pid, LocalPath, RemotePath)              %#
%%%     ok | error().                                       %#
%%% Input: Pid - A ssh_sftp channel pid
%%%        LocalPath - Path to the local file being transferred
%%%        RemotePath - Destination path on the remote server
%%% Output: ok
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

-spec put_file(Pid::pid(), LocalPath::string(), RemotePath::string()) ->
		      ok |
		      {error, Module::any(), Reason::any()}.

put_file(Pid, LocalPath, RemotePath) ->
    case file:open(LocalPath, [read, raw]) of
	{ok, Fd} ->
	    case ssh_sftp:open(Pid, RemotePath, [write]) of
		{ok, Handle} ->
		    case catch lean_write(Fd, Pid, Handle, file:read(Fd, 65536)) of
			ok ->
			    ssh_sftp:close(Pid, Handle),
			    file:close(Fd),
			    ok;
			{error, Module, Reason} ->
			    ssh_sftp:close(Pid, Handle),
			    file:close(Fd),
			    {error, Module, Reason};
			{'EXIT', Reason} ->
			    erlang:exit(Reason, [Pid, LocalPath, RemotePath])
		    end;
		{error,Reason} ->
		    {error, ssh_sftp, Reason}
	    end;
	{error,Reason} ->
	    {error,file, Reason}
    end.

lean_write(Fd, Pid, Handle, {ok, Data}) ->
    garbage_collect(),
    case ssh_sftp:write(Pid, Handle, Data, 10000) of
	ok -> 
	    lean_write(Fd, Pid, Handle, file:read(Fd, 65536));
	{error, Reason} ->
	    {error, ssh_sftp, Reason}
    end;    
lean_write(_, _, _, eof) ->
    ok;
lean_write(_, _, _, {error, Reason}) ->
    {error, file, Reason}.


%%% ----------------------------------------------------------
%%% -type get_file(Pid, RemotePath, LocalPath)              %#
%%%     ok | error().                                       %#
%%% Input: Pid - A ssh_sftp channel pid
%%%        RemotePath - Path on the file being fetched on the remote server
%%%        LocalPath - Local destination
%%% Output: ok
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

-spec get_file(Pid::pid(), RemotePath::string(), LocalPath::string()) -> ok.

get_file(Pid, RemotePath, LocalPath) ->
    {ok, Fd} = file:open(LocalPath, [write, raw]),
    {ok, Handle} = ssh_sftp:open(Pid, RemotePath, [read]),
    case catch lean_read(Fd, Pid, Handle, ssh_sftp:read(Pid, Handle, 65536)) of 
	ok ->
	    ssh_sftp:close(Pid, Handle),
	    file:close(Fd);
	{error, Module, Reason} ->
	    {error, Module, Reason};
	{'EXIT', Reason} ->
	    ssh_sftp:close(Pid, Handle),
	    file:close(Fd),
	    erlang:exit(Reason, [Pid, RemotePath, LocalPath])
    end.

lean_read(Fd, Pid, Handle, {ok, Data}) ->
    garbage_collect(),
    case file:write(Fd, Data) of
	ok -> 
	    lean_read(Fd, Pid, Handle, ssh_sftp:read(Pid, Handle, 65536));
	{error, Reason} ->
	    {error, file, Reason}
    end;
lean_read(_, _, _, eof) ->
    ok;
lean_read(_, _, _, {error, Reason}) ->
    {error, ssh_sftp, Reason}.

%%% ----------------------------------------------------------
%%% -type start_channel_only_alt(Host, Port, User, Password, AdditionalOpt)    %#
%%%     {ok, Pid, ConnectionRef} | {error, Reason}.                   %#
%%% Input: Host - string
%%%        Port - integer
%%%        User -  string
%%%        Password - string
%%%        AdditionalOpt - list with additional options to start_channel 
%%% Output: {ok, Pid, ConnectionRef}
%%% Exceptions: 
%%% Description: Caller determined that the alternate OamAccessPoint should be
%%%		 used. Othervise same as start_channel/4,5
%%% ----------------------------------------------------------
start_channel_only_alt(Host, Port, User, Password) ->
    start_channel_only_alt(Host, Port, User, Password, []).
start_channel_only_alt(Host, Port, User, Password, AdditionalOpt) ->
    {ok, AltTuple} = inet:parse_address(ootI:get_oap_ip_addr_alt()),
    {ok, AltNs} = ootI:get_oap_alt_namespace(),
    OptsOpts = get_connect_opts(User, Password) ++ AdditionalOpt ++
		omc_api:ns_to_opt_list(AltNs) ++
		ootI:get_dscp_opt_list() ++
		[comsaI:get_ssh_preferred_algorithms()] ++
		[{ip, AltTuple}],
    sftp_start_channel(Host, Port, OptsOpts).

%%% ----------------------------------------------------------
%%% -type start_channel_with_alt(Host, Port, User, Password, AdditionalOpt)    %#
%%%     {ok, Pid, ConnectionRef} | {error, Reason}.                   %#
%%% Input: Host - string
%%%        Port - integer
%%%        User -  string
%%%        Password - string
%%%        AdditionalOpt - list with additional options to start_channel 
%%% Output: {ok, Pid, ConnectionRef}
%%% Exceptions: 
%%% Description: First do the same as start_channel/4,5, if this fails and the
%%%              connection isn't using LMT, try using the alternate
%%%		 OamAccessPoint.
%%% ----------------------------------------------------------
start_channel_with_alt(Host, Port, User, Password) ->
    start_channel_with_alt(Host, Port, User, Password, []).
start_channel_with_alt(Host, Port, User, Password, AdditionalOpt) ->
    case start_channel(Host, Port, User, Password, AdditionalOpt) of
	{error, _Reason} = Current_error ->
	    case get_oot_opts(ootI:getaddr(Host),
			      ootI:get_oap_ip_addr()) of
		{false, _} ->  %destination matched LMT - alternate not relevant
		    Current_error;
		{true, Opts} -> 
		    OptsOpts = get_connect_opts(User, Password) ++
				AdditionalOpt ++ Opts,
		    start_alt(Host, Port, OptsOpts, Current_error)
	     end;
	Good ->
	    Good
    end.
%%% ----------------------------------------------------------
%%% -type start_channel(Host, Port, User, Password, AdditionalOpt)    %#
%%%     {ok, Pid, ConnectionRef} | {error, Reason}.                   %#
%%% Input: Host - string
%%%        Port - integer
%%%        User -  string
%%%        Password - string
%%%        AdditionalOpt - list with additional options to start_channel 
%%% Output: {ok, Pid, ConnectionRef}
%%% Exceptions: 
%%% Description: Starts a SFTP client
%%% ----------------------------------------------------------
-spec start_channel(Host::string(), Port::integer(), User::string(), Password::string()) -> 
			   {ok, Pid::pid(), ConnectionRef::pid()} | 
			   {error, Reason::any()}. 
		
start_channel(Host, Port, User, Password) ->
    start_channel(Host, Port, User, Password, []).


-spec start_channel(Host::string(), Port::integer(), User::string(), Password::string(),
		    AdditionalOpt::list()) -> 
			   {ok, Pid::pid(), ConnectionRef::pid()} | 
			   {error, Reason::any()}.
		
%% Updated to use network namespace on OaM, also prepared to work with a
%% fixed namespace on LMT (when/if that becomes active)
start_channel(Host, Port, User, Password, AdditionalOpt) ->
    {_, Ns_ip_dscp_opts} =
	get_oot_opts(ootI:getaddr(Host), ootI:get_oap_ip_addr()),
    Options = get_connect_opts(User, Password) ++
		AdditionalOpt ++
		[comsaI:get_ssh_preferred_algorithms()] ++
		Ns_ip_dscp_opts,
    sftp_start_channel(Host, Port, Options).
 
 
%%% ----------------------------------------------------------
%%% -type stop_channel(Pid, ConnectionRef)               %#
%%%       ok | error().                                  %#
%%% Input: Pid -
%%%        ConnectionRef -  
%%% Output: ok
%%% Exceptions: 
%%% Description: Stop the SFTP client
%%% ----------------------------------------------------------

-spec stop_channel(Pid::pid(), ConnectionRef::pid()) -> ok.

stop_channel(Pid, ConnectionRef) when is_pid(Pid) and is_pid(ConnectionRef) ->
    ssh_sftp:stop_channel(Pid),
    ssh:close(ConnectionRef);
stop_channel(Pid, _ConnectionRef) when is_pid(Pid) ->
    ssh_sftp:stop_channel(Pid);
stop_channel(_Pid, ConnectionRef) when is_pid(ConnectionRef) ->
    ssh:close(ConnectionRef);
stop_channel(_Pid, _ConnectionRef) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

get_connect_opts(User, Password) ->
    [{silently_accept_hosts, true}, 
     {nodelay, true},
     {user_interaction, false},
     {user, User}, 
     {password, Password},
     {sftp_vsn, 3}].

start_alt(Host, Port, OptsOpts, Current_error) ->
    case {inet:parse_address(ootI:get_oap_ip_addr_alt()),
	  ootI:get_oap_alt_namespace()} of
	{{ok, Ip_tuple}, {ok, AltNs}} ->
	    sysInitI:warning_msg("~w: SFTP client returned: ~w - "
				 "trying alternate connection~n",
				 [?MODULE, Current_error]),
	    NsOpt = lists:keyfind(netns, 1, omc_api:ns_to_opt_list(AltNs)),
	    Alt_opts =
		lists:keyreplace(
			netns, 1, 
			lists:keyreplace(ip, 1, OptsOpts, {ip, Ip_tuple}),
			NsOpt),
	    Final_opts = Alt_opts ++ [comsaI:get_ssh_preferred_algorithms()],		
	    sftp_start_channel(Host, Port, Final_opts);
	_ -> 
	    Current_error
    end.


% Let OOT  resolve the Host before sending to ssh_sftp

sftp_start_channel(Host, Port, Options) ->
    case catch ootI:getaddr(Host) of
    	{ok, IpTuple} ->
	    Opts = get_opts(IpTuple),
    	    ssh_sftp:start_channel(inet:ntoa(IpTuple), Port, Options++Opts );
    	{'EXIT', Reason} ->
    	    {error, Reason};
    	{error, Reason} ->
    	    {error, Reason}
     end.

get_opts(IpTuple) when tuple_size(IpTuple) == 8 ->
    [{inet,inet6}];
get_opts(_) -> [].

% Return tuple: {Using_oot, Options}
% Using_oot: true | false, used to simply when deciding on alt-oam connection
%If the OamAccessPoint isn't set - default to options for LMT, else check
%the address we are trying to connect to.
%Figure out if the IP we are trying to connect is on the network(s)
%directly accessible on the LMT port, if so return suitable options,
%else return options compatable to a connection using OamAccessPoint.
%or just return {false, []} and sit back to see if that works.
get_oot_opts( _, []) ->
    %this clause matches if the OamAccessPoint is unset
    %in this case (i.e. lab case); assume LMT
    {false, ootI:get_lmt_ns_opt_list()};
get_oot_opts({error, _}, _) ->
    {false, []};
get_oot_opts({ok, Host}, _) when size(Host) =:= 8 ->
    %IPv6 can never be on the LMT
    {true, ootI:get_oam_ns_opt_list() ++ 
     ootI:get_oap_ip_opt_list() ++
     ootI:get_dscp_opt_list() ++
     [{inet, inet6}]};
get_oot_opts({ok, Host}, _) ->
    NetList = get_list_of_lmt_networks(),
    case lists:any(fun(Net) -> is_host_on_net(Host, Net) end, NetList) of
	true ->
	    {false, ootI:get_lmt_ns_opt_list()};
	false ->
	    {true, ootI:get_oam_ns_opt_list() ++ 
	    ootI:get_oap_ip_opt_list() ++
	    ootI:get_dscp_opt_list()}
    end.



is_host_on_net({I1, I2, I3, I4}, {{N1, N2, N3, N4}, Mask_length}) ->
    <<Mask:32>> = <<16#ffffffff:Mask_length, 0:(32 - Mask_length)>>,
    <<Ip:32>> = <<I1:8, I2:8, I3:8, I4:8>>,
    <<Net:32>> = <<N1:8, N2:8, N3:8, N4:8>>,
    Ip band Mask =:= Net band Mask.

get_list_of_lmt_networks() ->
    try ootI:get_lmt_ipv4_with_mask() of
	{{_,_,_,_},_} = Ip_and_mask_dhcp ->
	    [Ip_and_mask_dhcp];
	_ ->
	    []
    catch
	error:undef -> %workaround while OOT is going into baseline
	    case ootI:get_lmt_ipv4() of
		{_,_,_,_} = Oot_ip ->
		    [{Oot_ip, 24}];
		_ ->
		    []
	    end
    end ++
    try ootI:get_lmt_ll_ipv4_with_mask() of
	{{_,_,_,_},_} = Ip_and_mask_ll ->
	    [Ip_and_mask_ll]
	% dialyzer knows best _ -> []
    catch
	error:undef -> %workaround while OOT is going into baseline
	    [{{169, 254, 2, 2}, 16}]
    end.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

