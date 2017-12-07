%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logI.erl %
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R7A/R8A/R9A/R10A/1
%%% @doc ==Interface for the Log service==
%%% This module contains the interface for the log service.
%%% The log service handles the official customer logs.
%%%
%%% Applications can use this interface for creating and deleting logs,
%%% and for writing log entries to any log
%%%
%%% The log service furthermore collects and transfers Ericsson
%%% support information. The applications can use this interfaces to
%%% add more directories with information to be picked up in the ESI
%%% package, as well as call back modules for generating information
%%% on request. However, applications should endeavour to keep as much
%%% data available in advance, because it might be necessary to
%%% reterive it without the erlang environment running.

-module(logI).
-vsn('/main/R1A/R2A/R3A/R4A/R7A/R8A/R9A/R10A/1').
-date('2017-06-14').
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
%%% R1A/1      2012-02-06 etxjotj     Created
%%% R3A/1      2014-11-24 etxtory     Added get_reg_esi_dirs
%%% R3A/2      2015-01-29 etxjotj     Added generate_esi
%%% R4A/1      2015-07-21 etxjotj     Added is_esi_ongoing
%%% R4A/2      2015-09-25 etxpejn     Added short_fru_id for SecurityLog, AiLog,
%%%                                   AuditTrailLog and SwmLog
%%% ----------------------------------------------------------
%%% R7A/1      2016-10-03 uabesvi  HV26316 timeout in ESI cb
%%% R7A/3      2016-10-17 etxarnu  WP6081: Added generate_rollback_esi/0
%%% R9A/1-4    2017-03-27 uabesvi  Added code for compress
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([create_log/2, delete_log/1]).
-export([update_log/2]).
-export([write_log/7, write_log/6, write_log/5, write_log/4]).
-export([awrite_log/7, awrite_log/5, awrite_log/4]).
-export([register_esi_cb/1, register_esi_cb/2, register_esi_dir/1, get_reg_esi_dirs/0]).
-export([unregister_esi_cb/1, unregister_esi_dir/1]).
-export([get_version/0]).
-export([is_esi_ongoing/0]).
-export([generate_esi/0]).
-export([generate_rollback_esi/0]).
-export([clean_disk/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Creates a disk log with the given parameters.
%%%
%%% This function does not return errors, but failures can occur
%%% anyway; those are displayed in the erlang log.  
%%%
%%% ===Arguments===
%%% <ul>
%%% <li>
%%% Name - The name of the log<br/>
%%% </li>
%%% <li>
%%% Options - A list of options<br/>
%%% </li>
%%% <ul>
%%% <li>
%%%  maxSize          - The maximum size of each log file in megabytes (deprecated). 
%%%                     Default = 2<br/>
%%% </li>
%%% <li>
%%%  maxSizeKb        - The maximum size of each log file in kilobytes. 
%%%                     Default = 2048<br/>
%%% </li>
%%% <li>
%%%  rotatingSegments - The number of rotated files.
%%%                     If not set the LOG is handled as halt log. <br/>
%%% </li>
%%% <li>
%%%  public           - Specifies if the LOG is associated with an MO. 
%%%                     Default = false<br/>
%%% </li>
%%% <li>
%%%  local            - Specifies if the LOG is local to a VM or commof for the node. 
%%%                     Default = false<br/>
%%% </li>
%%% <li>
%%%  encrypted        - Specifies if the LOG is encrypted. 
%%%                     Default = false<br/>
%%% </li>
%%% <li>
%%%  compressed       - Specifies if the LOG files are compressed. 
%%%                     Default = false<br/>
%%% </li>
%%% </ul>
%%% </ul>
%%% 
%%% Note: It is not possible to compress encrypted LOGs
%%% @end
%%% ----------------------------------------------------------

-type maxSize() :: {maxSize, integer()}.
-type maxSizeKb() :: {maxSizeKb, integer()}.
-type rotatingSegments() :: {rotatingSegments, integer()}.
-type public() :: {public, boolean()}.
-type local() :: {local, boolean()}.
-type encrypted() :: {encrypted, boolean()}.
-type compressed() :: {compressed, boolean()}.
-type milliSec() :: {milliSec, boolean()}.
-type opt() :: maxSize() | maxSizeKb() | rotatingSegments() | 
               public() | local() | encrypted() | compressed() | milliSec().
-type opt_list() :: [opt()].

-spec create_log(Name::string(), Options::opt_list()) -> ok.

create_log(Name, Options) ->
    logServer:create_log(Name, Options).


%%% ----------------------------------------------------------
%%% @doc Deletes a system created log.
%%% ===Arguments===
%%% Name - Name of the log<br/>
%%% @end
%%% ----------------------------------------------------------

-spec delete_log(Name::string()) -> ok.

delete_log(Name)
  when Name /= "AlarmLog";
       Name /= "NotificationLog";
       Name /= "SystemLog" ->
    logServer:delete_log(Name).


%%% ----------------------------------------------------------
%%% @doc Update options of a log.
%%% 
%%% ===Arguments===
%%% <ul>
%%% <li>
%%% Name    - Name of the log<br/>
%%% </li>
%%% <li>
%%% Options - A list of options<br/>
%%% </li>
%%% <ul>
%%% <li>
%%%  compressed - Specifies if the log files are compressed. 
%%%               Default = false<br/>
%%% </li>
%%% <li>
%%%  milliSec - Specifies if the timestamp should contain milli seconds. 
%%%             Default = false<br/>
%%% </li>
%%% <li>
%%%  public - Specifies if the LOG is associated with an MO. 
%%%           Default = false<br/>
%%% </li>
%%% </ul>
%%% </ul>
%%% Note: This function may only be used when initializing a log
%%%       at upgrade. It is not supposed to be used on a fully started node, 
%%% @end
%%% ----------------------------------------------------------

-type update_opt() :: compressed() | milliSec() | public().

-type update_opt_list() :: [update_opt()].

-spec update_log(Name::string(), Options::update_opt_list()) -> ok.

update_log(Name, Options) ->
    logServer:update_log(Name, Options).


%%% ----------------------------------------------------------
%%% @doc This function writes a message to a specific log. 
%%% 
%%% This version of write_log is used when not having source 
%%% IP adress and when no facility exist.<br/>
%%% 
%%% @see write_log/5 
%%% @end
%%% ----------------------------------------------------------
-spec write_log(Name::string(), User::string(), Severity::severityLevel(),
		Msg::string()) -> writeRetType().
write_log(Name, User, Severity, Msg) when Name == "SecurityLog";
					  Name == "AiLog";
					  Name == "AuditTrailLog";
					  Name == "SwmLog" ->
    ErlangNode = get_core_node(),
    NewMsg = case clhI:short_fru_id() of
		 "" ->
		     Msg;
		 FruId ->
		     FruId ++ ": " ++ Msg
	     end,
    rpc:call(ErlangNode, logEntry, write_log,
	     [Name, User, Severity, os:timestamp(), NewMsg]);
write_log(Name, User, Severity, Msg) ->
    logEntry:write_log(Name, User, Severity, os:timestamp(), Msg).

%%% ----------------------------------------------------------
%%% @doc  This function writes a message to a specific log.
%%%
%%% This version of write_log is used when not having source 
%%% IP adress and when no facility exist.<br/>
%%% 
%%% @see write_log/7
%%% @end
%%% ----------------------------------------------------------

-type severityLevel() :: emergency | alert | critical | error | warning | notice | info.
-type writeRetType() :: ok | tryAgain | invalidParam | noResources.
-type nowTs() :: {integer(), integer(), integer()} | integer() .
-spec write_log(Name::string(), User::string(), Severity::severityLevel(),
		TimeStamp::nowTs(), Msg::string()) -> writeRetType().
write_log(Name, User, Severity, TimeStamp, Msg) when Name == "SecurityLog";
						     Name == "AiLog";
						     Name == "AuditTrailLog";
						     Name == "SwmLog" ->
    ErlangNode = get_core_node(),
    NewMsg = case clhI:short_fru_id() of
		 "" ->
		     Msg;
		 FruId ->
		     FruId ++ ": " ++ Msg
	     end,
    rpc:call(ErlangNode, logEntry, write_log,
	     [Name, User, Severity, TimeStamp, NewMsg]);
write_log(Name, User, Severity, TimeStamp, Msg) ->
    logEntry:write_log(Name, User, Severity, TimeStamp, Msg).

%%% ----------------------------------------------------------
%%% @doc This function writes a message to a specific log.
%%% 
%%% This version of write_log is used when facility exist
%%% but source IP adress doesn't.<br/>
%%% 
%%% @see write_log/7
%%% @end
%%% ----------------------------------------------------------
-spec write_log(Name::string(), User::string(), Facility::integer(),
    Severity::severityLevel(), TimeStamp::nowTs(), Msg::string()) -> writeRetType().
write_log(Name, User, Facility, Severity, TimeStamp, Msg) ->
    write_log(Name, "-", User, Facility, Severity, TimeStamp, Msg).

%%% ----------------------------------------------------------
%%% @doc  This function writes a message to a specific log.
%%%
%%% The log entry is written according to the specified log entry format.  
%%% There is also an asynchronous version of this function
%%% for improved performance. <br/><br/>
%%%
%%% This version of write_log is used when having both source IP adress 
%%% and facility,<br/>
%%% for example Security Log will use this flavor. <br/><br/>
%%%
%%% Log entries are not guaranteed to be
%%% written in the arrived order. The timestamp should make it
%%% possible for the user to determine the order.
%%%
%%% ===ABOUT TIMESTAMPS===
%%%
%%% The timestamp is formatted according to the log format
%%% specification. Input is the number of seconds since Jan 1, 1970,
%%% but for convenience erlang applications can use the result of the
%%% now() BIF.  There is also a version of this function without timestamp 
%%% argument which means the log service will provide a timestap.
%%%
%%% ===Arguments===
%%% 
%%% <ul>
%%% <li>
%%% Name:string() - The name of the log<br/>
%%% </li>
%%% <li>
%%% SrcIp:string() - Source IP address
%%% </li>
%%% <li>
%%% User:string() - Distinguished name to the associated managed object<br/>
%%% </li>
%%% <li>
%%% Facility:integer() - See rfc5424
%%% </li>
%%% <li>
%%% Severity:atom() - A severity level for this entry. This is used for filtering.<br/>
%%% </li>
%%% <li>
%%% TimeStamp:nowTs() - A now timestamp, can be omitted<br/>
%%% </li>
%%% <li>
%%% Msg:string() - The actual log message<br/>
%%% </li>
%%% </ul>
%%% @end
%%% This function writes a message to a specific log.
%%% This version of write_log is used when having source IP adress and facility,<br/>
%%% for example used for the Security Log.
%%% @see awrite_log/7
%%% @end
%%% ----------------------------------------------------------
-spec write_log(Name::string(), SrcIp::string(), User::string(), Facility::integer(),
    Severity::severityLevel(), TimeStamp::nowTs(), Msg::string()) -> writeRetType().
write_log(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg)
  when Name == "SecurityLog";
       Name == "AiLog";
       Name == "AuditTrailLog";
       Name == "SwmLog" ->
    ErlangNode = get_core_node(),
    NewMsg = case clhI:short_fru_id() of
		 "" ->
		     "src_ip(" ++ SrcIp ++ ") " ++ Msg;
		 FruId ->
		     FruId ++ ": " ++ "src_ip(" ++ SrcIp ++ ") " ++ Msg
	     end,
    rpc:call(ErlangNode, logEntry, write_log,
	     [Name, SrcIp, User, Facility, Severity, TimeStamp, NewMsg]);
write_log(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg) ->
    NewMsg = "src_ip(" ++ SrcIp ++ ") " ++ Msg,
    logEntry:write_log(Name, SrcIp, User, Facility, Severity, TimeStamp, NewMsg).

%%% ----------------------------------------------------------
%%% @doc Asynchronous version of write_log/7
%%% This version of awrite_log is used when having source IP adress and facility,<br/>
%%% for example used for the Security Log.
%%% @see write_log/7
%%% @end
%%% ----------------------------------------------------------
-spec awrite_log(Name::string(), 
		 SrcIp::string(), 
		 User::string(),
		 Facility::integer(),
		 Severity::severityLevel(), 
		 TimeStamp::nowTs(),
		 Msg::string()) -> ok.
awrite_log(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg)
  when Name == "SecurityLog";
       Name == "AiLog";
       Name == "AuditTrailLog";
       Name == "SwmLog" ->
    ErlangNode = get_core_node(),
    NewMsg = case clhI:short_fru_id() of
		 "" ->
		     Msg;
		 FruId ->
		     FruId ++ ": " ++ Msg
	     end,
    rpc:call(ErlangNode, logEntry, awrite_log,
	     [Name, SrcIp, User, Facility, Severity, TimeStamp, NewMsg]);
awrite_log(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg) ->
    logEntry:awrite_log(Name, SrcIp, User, Facility, Severity, TimeStamp, Msg).

%%% ----------------------------------------------------------
%%% @doc Asynchronous version of write_log/5
%%% @see write_log/5
%%% @end
%%% ----------------------------------------------------------
-spec awrite_log(Name::string(), User::string(), Severity::severityLevel(),
		 TimeStamp::nowTs(), Msg::string()) -> ok.
awrite_log(Name, User, Severity, TimeStamp, Msg) when Name == "SecurityLog";
						      Name == "AiLog";
						      Name == "AuditTrailLog";
						      Name == "SwmLog" ->
    ErlangNode = get_core_node(),
    NewMsg = case clhI:short_fru_id() of
		 "" ->
		     Msg;
		 FruId ->
		     FruId ++ ": " ++ Msg
	     end,
    rpc:call(ErlangNode, logEntry, awrite_log,
	     [Name, User, Severity, TimeStamp, NewMsg]);
awrite_log(Name, User, Severity, TimeStamp, Msg) ->
    logEntry:awrite_log(Name, User, Severity, TimeStamp, Msg).

%%% ----------------------------------------------------------
%%% @doc Asynchronious version of write_log/4
%%% @see write_log/4
%%% @end
%%% ----------------------------------------------------------
-spec awrite_log(Name::string(), User::string(), Severity::severityLevel(),
		 Msg::string()) -> ok.
awrite_log(Name, User, Severity, Msg) when Name == "SecurityLog";
					   Name == "AiLog";
					   Name == "AuditTrailLog";
					   Name == "SwmLog" ->
    ErlangNode = get_core_node(),
    NewMsg = case clhI:short_fru_id() of
		 "" ->
		     Msg;
		 FruId ->
		     FruId ++ ": " ++ Msg
	     end,
    rpc:call(ErlangNode, logEntry, awrite_log,
	     [Name, User, Severity, os:timestamp(), NewMsg]);
awrite_log(Name, User, Severity, Msg) ->
    logEntry:awrite_log(Name, User, Severity, os:timestamp(), Msg).

%%% ----------------------------------------------------------
%%% @doc Version of the SAF specification used to implement this log service.
%%% The result is a three element long list of ReleaseCode, MajorVersion and
%%% MinorVersion
%%% @end
%%% ----------------------------------------------------------
-spec get_version() -> [string()].

get_version() ->
    ["A", "02", "01"].

%%% ----------------------------------------------------------
%%% @doc Register an participating directory in the ESI file
%%%  ESI, Ericsson support information, shall contain all the necessary
%%%  data to carry out fault searching.
%%%
%%%  The ESI function collects the files stored in a registered directory.
%%%  This function makes it possible to register a directory as an ESI
%%%  participant. It is preferred that the applications process as much as
%%%  possible for storage directly into files, as too much post processing
%%%  can make the ESI action to heavy.
%%%
%%%  Logs operated by LOG will be handled by LOG automatically, even if
%%%  they have been created by another application.
%%%
%%%  A corresponding unregister function can be used to remove the
%%%  participant
%%%
%%% === Arguments===
%%% Dir - path to a directory with support information files
%%% @end
%%% ----------------------------------------------------------
-spec register_esi_dir(Dir::string()) -> ok.

register_esi_dir(Dir) ->
    logEsi:register_esi_dir(Dir).

%%% ----------------------------------------------------------
%%% @doc Get the registered ESI directories.
%%% @end
%%% ----------------------------------------------------------
-spec get_reg_esi_dirs() -> list().

get_reg_esi_dirs() ->
    logEsi:get_reg_esi_dirs().

%%% ----------------------------------------------------------
%%% @doc Unregister a directory
%%%
%%% ===Argument===
%%% Dir - path to a directory with support information files
%%% @end
%%% ----------------------------------------------------------
-spec unregister_esi_dir(Dir::string()) -> ok.

unregister_esi_dir(Dir) ->
    logEsi:unregister_esi_dir(Dir).

%%% ----------------------------------------------------------
%%% @doc Generate an esi
%%% @end
%%% ----------------------------------------------------------
-spec generate_esi() -> string().

generate_esi() ->
    logEsi:generate_esi().

%%% ----------------------------------------------------------
%%% @doc Generate an rollback esi
%%% @end
%%% ----------------------------------------------------------
-spec generate_rollback_esi() -> string().

generate_rollback_esi() ->
    logEsi:generate_rollback_esi().

%%% ----------------------------------------------------------
%%% @doc Register a module callback ESI participant
%%%  Registers a callback module allowing an application to prepare
%%%  material to be stored in the support information package. This
%%%  function is offered as a complement as the preferred way is to
%%%  store all data in run time on files since it may be necessary to
%%%  retrieve data without the node running.
%%%
%%%  The callback is Module:generate_esi() -> term(). The answer is
%%%  recorded in the ESI log. Callbacks are made before directories
%%%  are collected. The order of the callbacks is not specified.
%%%
%%%  A corresponding unregister function can be used to remove the
%%%  participant
%%%
%%% ===Argument===
%%% Module - Module name implementing the callback
%%% Time   - Max time how long the callback may execute
%%%
%%% ===Callback===
%%% A participating module shall implement the generate_esi/0
%%% function, which shall store the data in a registered directory.
%%% It may optionally implement a generate_esi_post/0 function
%%% which will be called after the esi is generated.
%%% @end
%%% ----------------------------------------------------------

-spec register_esi_cb(Module::atom()) -> ok.

register_esi_cb(Module) ->
    logEsi:register_esi_cb(Module).

-spec register_esi_cb(Module::atom(), Time::integer()) -> ok.

register_esi_cb(Module, Time) ->
    logEsi:register_esi_cb(Module, Time).

%%% ----------------------------------------------------------
%%% @doc Removes the module from the participant list
%%% @end
%%% ----------------------------------------------------------

-spec unregister_esi_cb(Module::atom()) -> ok.

unregister_esi_cb(Module) ->
    logEsi:unregister_esi_cb(Module).

%%% ----------------------------------------------------------
%%% @doc Clean disk
%%% @end
%%% ----------------------------------------------------------

-spec clean_disk(Severity::minor|major) -> any().

clean_disk(Severity) ->
    logServer:clean_disk(Severity).

%%% ----------------------------------------------------------
%%% @doc Check if esi is ongoing
%%% @end
%%% ----------------------------------------------------------

-spec is_esi_ongoing() -> boolean().

is_esi_ongoing() ->
    logEsi:is_esi_ongoing().

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
get_core_node() ->
    case  clhI:erlang_nodes(active) of
    	[] ->
    	    clhI:erlang_node();
    	[Node]->
    	   Node
    end.

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

