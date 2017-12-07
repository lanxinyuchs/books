%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_cli.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_cli).
-vsn('/main/R1A/R2A/1').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2013 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R1A/4      2012-03-01 etxpeno     Dialyzer fixes
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([register/0, coli/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("lih_lici.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-spec register() -> 'ok'.
register() ->
    %% Name = "license",
    %% Usage = "license <cmd>",
    %% Description = "License server command (license <cmd>)",
    %% MF = {?MODULE, coli},
    %% sysSshCli:register_coli(Name, Usage, Description, MF),

    ok.

-spec coli([string()]) -> 'ok'.
coli(["server"])           -> server();
coli(["server"|_])         -> io:format("Usage: license server~n");
coli(["client"])           -> client();
coli(["client"|_])         -> io:format("Usage: license client~n");
coli(["key"])              -> key();
coli(["key"|_])            -> io:format("Usage: license key~n");
coli(["lkf"])              -> lkf();
coli(["lkf"|_])            -> io:format("Usage: license lkf~n");
coli(["sub", "-a"])        -> sub(all);
coli(["sub", Arg])         ->
    case catch list_to_pid(Arg) of
	Pid when is_pid(Pid) ->
	    sub(Pid);
	_ ->
	    io:format("Usage: license sub <client|-a>~n")
    end;
coli(["sub"|_])            -> io:format("Usage: license sub <client|-a>~n");
coli(["iu", "activate"])   -> iu(activate);
coli(["iu", "status"])     -> iu(status);
coli(["iu"|_])             -> io:format("Usage: license iu activate~n"
					"       license iu status~n");
coli(["pu", "activate"])   -> pu(activate);
coli(["pu", "deactivate"]) -> pu(deactivate);
coli(["pu", "status"])     -> pu(status);
coli(["pu"|_])             -> io:format("Usage: license pu activate~n"
					"       license pu deactivate~n"
					"       license pu status~n");
coli(["fingerprint", Arg]) -> fingerprint(Arg);

coli(_)                    ->
    io:format("Available commands:~n"
	      "license server    : License server status~n"
	      "license client    : License server clients~n"
	      "license sub       : License server client subscriptions~n"
	      "license lkf       : License key file status~n"
	      "license key       : Installed license keys~n"
	      "license iu        : Integration unlock~n").

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

-spec server() -> 'ok'.
server() ->
    Info = lih_lici_server:get_info(),

    {_, LkfInstalled} = lists:keyfind(lkf_installed, 1, Info),
    {_, SeqNo} = lists:keyfind(seq_no, 1, Info),
    {_, FingerPrint} = lists:keyfind(finger_print, 1, Info),
    {_, Emergency} = lists:keyfind(emergency, 1, Info),

    case LkfInstalled of
	false      ->
	    io:format("License Key File     : Not Installed~n");
	{true, TS} ->
	    io:format("License Key File     : Installed, ~s~n", [ctime(TS)])
    end,
    io:format("Sequence Number      : ~p~n", [SeqNo]),
    io:format("Fingerprint          : ~s~n", [FingerPrint]),
    io:format("Emergency Status     : ~s~n",
	      [convert({emergency_status, Emergency})]),

    case Emergency of
	never_used ->
	    io:format("Emergency State      : never_used~n"
		      "Emergency State Time : ~s~n",
		      [ctime({0,0,0})]);
	{ES, EST} ->
	    io:format("Emergency State      : ~s~n"
		      "Emergency State Time : ~s~n",
		      [atom_to_list(ES), ctime(EST)])
    end.

-spec client() -> 'ok'.
client() ->
    Info = lih_lici_server:get_info(),

    {_, Subscriptions} = lists:keyfind(subscriptions, 1, Info),
    StatusSubscriptions =
	case lists:keyfind(status, #lici_subscription.id, Subscriptions) of
	    false ->
		[];
	    #lici_subscription{pid_list = PidList} ->
		PidList
	end,

    {_, Providers} = lists:keyfind(providers, 1, Info),

    [print_client(Provider, StatusSubscriptions) || Provider <- Providers],

    io:format("~nClients               : ~p~n", [length(Providers)]).

-spec lkf() -> 'ok'.
lkf() ->
    LKFInfo = lih_lici_server:get_lkf_info(),

    {_, Status} = lists:keyfind(status, 1, LKFInfo),

    if
	Status ->
	    {_, FormatVersion} = lists:keyfind(format_version, 1, LKFInfo),
	    {_, SeqNo} = lists:keyfind(seq_no, 1, LKFInfo),
	    {_, FingerPrint} = lists:keyfind(finger_print, 1, LKFInfo),
	    {_, SignatureType} = lists:keyfind(signature_type, 1, LKFInfo),
	    {_, SWLT} = lists:keyfind(swlt_id, 1, LKFInfo),
	    {_, ProductType} = lists:keyfind(product_type, 1, LKFInfo),
	    {_, CustomerId} = lists:keyfind(customer_id, 1, LKFInfo),

	    Info = lih_lici_server:get_info(),
	    {_, LicenseKeys} = lists:keyfind(license_keys, 1, Info),

	    io:format("Format Version          : ~s~n"
		      "Sequence Number         : ~p~n"
		      "Finger Print            : ~s~n"
		      "Signature Type          : ~p~n"
		      "SWLT                    : ~s~n"
		      "Product Type            : ~s~n"
		      "Customer Id             : ~s~n"
		      "Licenses                : ~p~n",
		      [FormatVersion, SeqNo, FingerPrint, SignatureType, SWLT,
		       ProductType, CustomerId, length(LicenseKeys)]);
	true ->
	    io:format("License Key File Status : Not Valid~n")
    end.

-spec key() -> 'ok'.
key() ->
    Info = lih_lici_server:get_info(),

    {_, LicenseKeys} = lists:keyfind(license_keys, 1, Info),

    lists:foreach(fun print_key/1, LicenseKeys).

-spec sub(pid() | 'all') ->  'ok'.
sub(Pid) ->
    Info = lih_lici_server:get_info(),

    {_, Subscriptions} = lists:keyfind(subscriptions, 1, Info),

    lists:foreach(
      fun(#lici_subscription{pid_list = []}) ->
	      ok;
	 (#lici_subscription{id = status}) ->
	      ok;
	 (Sub) when Pid =:= all ->
	      print_sub(Sub);
	 (#lici_subscription{pid_list = PidList} = Sub) ->
	      case lists:member(Pid, PidList) of
		  false -> ok;
		  true  -> print_sub(Sub#lici_subscription{pid_list = [Pid]})
	      end
      end, Subscriptions),

    io:nl().

-spec iu('activate' | 'status') -> 'ok'.
iu(activate) ->
    case lih_lici_server:integration_unlock(activate) of
	ok ->
	    io:format("Licenses unlocked.~n");
	{error, Reason} ->
	    io:format("Licenses NOT unlocked. Reason: ~s~n", [Reason])
    end;
iu(status)   ->
    {status, Active, Used, TimeLeft} =
	lih_lici_server:integration_unlock(status),

    StatusStr = get_status(Active),
    UsedStr = get_used(Used),

    io:format("Status                      : ~s~n"
	      "Integration unlock used     : ~s~n", [StatusStr, UsedStr]),

    case Active of
	true ->
	    {Days, Hours, Minutes} = get_days_hours_minutes(TimeLeft),
	    io:format("Time left in unlocked mode  : ~p Days ~.2.0w:~.2.0w~n",
		      [Days, Hours, Minutes]);
	false ->
	    ok
    end.

-spec pu('activate' | 'deactivate' | 'status') -> 'ok'.
pu(activate)   ->
    case lih_lici_server:production_unlock(activate) of
	ok ->
	    io:format("Production unlock activated.~n");
	{error, Reason} ->
	    io:format("Production unlock not activated. Reason: ~s~n", [Reason])
    end;
pu(deactivate) ->
    lih_lici_server:production_unlock(deactivate),

    io:format("Production unlock deactivated.~n");
pu(status)     ->
    {status, Active, Used, TimeLeft} =
	lih_lici_server:production_unlock(status),

    StatusStr = get_status(Active),
    UsedStr = get_used(Used),

    io:format("Status                         : ~s~n"
	      "Production unlock used         : ~s~n", [StatusStr, UsedStr]),

    case Active of
	true ->
	    {Days, Hours, Minutes} = get_days_hours_minutes(TimeLeft),
	    io:format("Time left in production unlock : ~p Days ~.2.0w:~.2.0w~n",
		      [Days, Hours, Minutes]);
	false ->
	    ok
    end.

fingerprint(Arg) ->
    case lih_db:set_fingerprint(Arg) of
	ok ->
	    io:format("License fingerprint is set to ~s~n", [Arg]);
	{error, Reason} ->
	    io:format("License fingerprint could not be set, Reason: ~s~n",
		      [Reason])
    end.

ctime(TS) ->
    {{Year, Month, Day}, {Hour, Min, Secs}} = calendar:now_to_local_time(TS),
    Dow = calendar:day_of_the_week(Year, Month, Day),
    io_lib:format("~s ~s ~.2.0w ~w ~.2.0w:~.2.0w:~.2.0w",
		  [dow(Dow), month(Month), Day, Year, Hour, Min, Secs]).

month(1)  -> "Jan";
month(2)  -> "Feb";
month(3)  -> "Mar";
month(4)  -> "Apr";
month(5)  -> "May";
month(6)  -> "Jun";
month(7)  -> "Jul";
month(8)  -> "Aug";
month(9)  -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

dow(1) -> "Mon";
dow(2) -> "Tue";
dow(3) -> "Wed";
dow(4) -> "Thu";
dow(5) -> "Fri";
dow(6) -> "Sat";
dow(7) -> "Sun".

print_client(#lici_provider{provider_pid = Pid}, StatusSubscriptions) ->
    ClientInfo = lih_lici_worker:get_info(Pid),

    {_, SelectedPv} = lists:keyfind(selected_pv, 1, ClientInfo),
    {_, SignalRevision} =
	lists:keyfind(signal_revision, 1, ClientInfo),

    HasStatusSub =
	case lists:member(Pid, StatusSubscriptions) of
	    true -> "True";
	    false -> "False"
	end,

    io:format("Client pid            : ~p~n"
	      "Protocol Version      : ~p~n"
	      "Signal Revision       : ~p~n"
	      "Status Subscription   : ~s~n",
	      [Pid, SelectedPv, SignalRevision, HasStatusSub]).

print_key(LicenseKey) ->
    case LicenseKey#lici_license_key.id of
	emergencyReset ->
	    io:format("Emergency Reset Key :~n");
	{featureKey, Id} ->
	    io:format("Feature Id          : ~s~n", [Id]);
	{capacityKey, Id} ->
	    io:format("Capacity Id         : ~s~n", [Id])
    end,

    {StartY, StartM, StartD} = LicenseKey#lici_license_key.start,
    io:format("Start Date          : ~w-~.2.0w-~.2.0w~n",
	      [StartY, StartM, StartD]),

    case LicenseKey#lici_license_key.stop of
	infinity ->
	    io:format("Stop Date           : No Limit~n");
	{StopY, StopM, StopD} ->
	    io:format("Stop Date           : ~w-~.2.0w-~.2.0w~n",
		      [StopY, StopM, StopD])
    end,

    case LicenseKey#lici_license_key.id of
	{capacityKey, _} ->
	    io:format("Capacity Level      : ~s~n"
		      "Hard Limit          : ~s~n",
		      [convert({level, LicenseKey#lici_license_key.capacity}),
		       convert({limit, LicenseKey#lici_license_key.hard_limit})]);
	_ ->
	    ok
    end,
    io:nl().

print_sub(Sub) ->
    io:format("Client                : ~p~n", [Sub#lici_subscription.pid_list]),

    case Sub#lici_subscription.id of
	{featureKey, Id} ->
	    io:format("Feature Id            : ~s~n"
		      "Feature Status        : ~s~n",
		      [Id,
		       convert({feature_status, Sub#lici_subscription.feature_status})]);
	{capacityKey, Id} ->
	    io:format("Capacity Id           : ~s~n"
		      "Capacity Level        : ~s~n"
		      "Hard Limit            : ~s~n",
		      [Id,
		       convert({level, Sub#lici_subscription.licensed_level}),
		       convert({level, Sub#lici_subscription.hard_limit})])
    end,

    io:format("Change Reason         : ~s~n~n",
	      [convert({change_reason, Sub#lici_subscription.change_reason})]).

convert({emergency_status, never_used})        -> "Deactivated";
convert({emergency_status, {active, _}})       -> "Activated";
convert({emergency_status, {degraded, _}})     -> "Deactivated";
convert({emergency_status, {active_again, _}}) -> "Activated";
convert({emergency_status, {disabled, _}})     -> "Deactivated";

convert({feature_status, ?CELLO_LICI_FEATURE_ENABLED})  -> "Enabled";
convert({feature_status, ?CELLO_LICI_FEATURE_DISABLED}) -> "Disabled";

convert({level, ?LICI_NO_LIMIT})     -> "No Level";
convert({level, ?LICI_LIMIT(Value)}) -> integer_to_list(Value);

convert({limit, ?LICI_NO_LIMIT})     -> "No Limit";
convert({limit, ?LICI_LIMIT(Value)}) -> integer_to_list(Value);

convert({change_reason, ?CELLO_LICI_LICENSED_VALUE})   -> "Licensed";
convert({change_reason, ?CELLO_LICI_NOT_ACTIVATED})    -> "Not activated";
convert({change_reason, ?CELLO_LICI_EMERGENCY_REASON}) -> "Emergency".

get_days_hours_minutes(Seconds) ->
    Days = Seconds div 86400,
    RestSeconds = Seconds rem 86400,
    Hours = RestSeconds div 3600,
    Minutes = (RestSeconds rem 3600) div 60,

    {Days, Hours, Minutes}.

get_status(true)  -> "Active";
get_status(false) -> "Not active".

get_used(true)  -> "Yes";
get_used(false) -> "No".

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
