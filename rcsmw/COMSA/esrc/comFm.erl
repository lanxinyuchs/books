%%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comFm.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R8A/R9A/R11A/R12A/2

%%% @doc ==Agent implementation for ComFm==
%%% This is the agent implementation for ComFm.
%%%
%%% COM uses this implementation to read alarm types, but it doesn't
%%% use it to store alarms, although that is part of the model. This
%%% information is instead routed to the replicated list service.
%%%
%%% This model also holds the implementation of the internal API to
%%% register alarms.

-module(comFm).
-author(etxjotj).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R8A/R9A/R11A/R12A/2').
-date('2017-11-21').
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
%%% -----      -------    --------    ------------------------
%%% R2A/3      2013-01-24 etxjotj     Specific problem is not always the name
%%% R2A/4      2013-03-26 etxarnu     Changed appdata/1 to appdata/3
%%% R2A/5      2013-04-04 erarafo     Support for upgrade
%%% R2A/7      2013-09-18 etxarnu     Corrected misspelled COMMUNICATIONSALARM
%%% R2A/8      2013-11-12 erarafo     Support for additional text in 'clear'
%%% R2A/12     2014-08-26 erarafo     Comments only
%%% R2A/13     2014-09-10 erarafo     Logging of alarms; catching bad data
%%% -----      -------    --------    ------------------------
%%% R3A/1      2014-09-15 etxpejn     Added warning if comte fails to send alarm to COM
%%% R3A/2      2014-09-18 etxberb     Added validate/3.
%%% R3A/3      2014-09-30 erarafo     Support for Additional Info
%%% R3A/5      2015-01-30 erarafo     Provisional solution for Status LED
%%% R3A/6      2015-01-31 erarafo     Comment added
%%% R3A/7      2015-02-13 erarafo     Removed provisional Status LED solution
%%% R3A/8      2015-03-17 etxtory     getMoAttributes fix
%%% R3A/9      2015-03-20 etxjotj     Warning when parsing appdata
%%% R3A/10     2015-03-24 erarafo     WP2979 groundwork
%%% R3A/11     2015-03-24 erarafo     more WP2979 groundwork
%%% R3A/12     2015-03-25 erarafo     Correction
%%% R3A/13     2015-03-26 erarafo     WP2979
%%% R3A/14     2015-03-31 erarafo     More emphasis on alarmtype clashes
%%% R3A/15     2015-04-16 etxjotj     Clarified alarm printout
%%% R3A/16     2015-04-16 etxjotj     Get active alarm.
%%% R3A/17     2015-06-01 etxjotj     Wrong in alarm table handling in upgrade
%%% R3A/18     2015-06-04 etxjotj     Preserve configured severity
%%% -----      -------    --------    ------------------------
%%% R4A/2      2015-09-15 erarafo     Support for warm restart
%%% R4A/3      2015-10-13 etxjotj     OTP18 erlang now replaced
%%% -----      -------    --------    ------------------------
%%% R5A/1      2016-01-26 erarafo     Improved fault logging
%%% R5A/2      2016-02-09 erarafo     Merge of moClasses attribute values
%%% -----      -------    --------    ------------------------
%%% R8A/1      2016-10-25 etxberb     Added element registerOnNodes and mnesia
%%%                                   table fmAlarmType_notReg.
%% ----    ---------- -------  ------------------------------------------------
%% R9A/1   2017-03-06 etxberb  Added call to swmI:is_node_type_valid
%% R9A/2   2017-03-23 etxjotj  Removed warning for deprecated nodes
%% ----    ---------- -------  ------------------------------------------------
%% R11A/1  2017-09-04 etxjotj  Changed swmLib to swmI
%% R11A/2  2017-10-04 etxpeno  add get_app_alarms/0, get_app_alarms/1
%% R11A/3  2017-10-19 etxpeno  (MR36328) handle program groups
%% ----    ---------- -------  ------------------------------------------------
%% R12A/1  2017-10-25 etxpeno  dialyzer fix
%% R12A/2  2017-11-20 etxpeno  handling alerts correctly in send_alarm_comte
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([send_alarm/7, clear_alarm/5]).
-export([clear_alarm_by_instance/3]).
-export([get_alarms/1]).
-export([register_alarm/9]).
-export([get_app_alarms/0, get_app_alarms/1]).

-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).
-export([validate/3]).

-export([existsMo/2,
         countMoChildren/3,
         getMoAttributes/3,
         setMoAttributes/3,
         createMo/5]).

-export([init_data/1]).

-export([appdata/3]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("ComFm.hrl").
-include("comFaultTypes.hrl").
-include("ComsaTypes.hrl").
-include("ComsaAlarm.hrl").
-include_lib("xmerl/include/xmerl.hrl").


%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	sysInitI:error_report(?RepInfo(__ReportInfo))).
-define(LOG_INFO(__ReportInfo),
	sysInitI:info_report(?RepInfo(__ReportInfo))).
-define(LOG_WARN(__ReportInfo),
	sysInitI:warning_report(?RepInfo(__ReportInfo))).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(MODULE_STR, atom_to_list(?MODULE)).


% Range of values defined in comFaultTypes.h (COMSA CAX)
-type fmSeverity()        :: 1|2|3|4|5.

% Range of the SeverityLevel "enum" defined in ComFm.hrl (COM_CXA1105460)
-type comSeverityLevel()  :: 3|4|5|6.



%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Sends an alarm. The only module using this function
%%% is comsaServer.
%%%
%%% Alarms that result from NTF notifications have the
%%% alarm specified as a {VendorId, MajorId, MinorId} tuple.
%%% @end
%%% ----------------------------------------------------------
-spec send_alarm(atom()|{integer(), integer(), integer()},
		 comsaI:severity_type(),
		 [binary()],
		 string(),
		 list(),
		 boolean(),
		 any()) -> any().

send_alarm(Alarm, Severity, DN, Text, Info, AppAlarm, PrgGrp) ->
    try
	send_alarm_helper(Alarm, Severity, DN, Text, Info, AppAlarm, PrgGrp)
    catch
	throw : registerOnNodes_false ->
	    ?LOG_WARN(["Alarm specified in xml file NOT to be registered",
		       "This function should thus NOT be called",
		       {alarm, Alarm},
		       {dN, DN},
		       "--- Stacktrace ---"
		       | erlang:get_stacktrace()]);
	ErrClass : ErrReason ->
	    ?LOG_ERR([{alarm, Alarm},
		      {severity, Severity},
		      {dN, DN},
		      {text, Text},
		      {info, Info},
		      {ErrClass, ErrReason},
		      "--- Stacktrace ---"
		      | erlang:get_stacktrace()])
    end.


send_alarm_helper(AlarmName, Severity, DN, Text, Info, AppAlarm,
		  PrgGrp) when is_atom(AlarmName) ->
    FmAlarmTypeObj = get_alarm_type(AlarmName),
    send_alarm_comte(
      DN,
      FmAlarmTypeObj#fmAlarmType.majorType,
      FmAlarmTypeObj#fmAlarmType.minorType,
      severity(Severity, FmAlarmTypeObj#fmAlarmType.defaultSeverity),
      binarize(Text),
      Info,
      AppAlarm,
      PrgGrp);


send_alarm_helper({VendorId, MajorId, MinorId}, Severity, DN, Text, Info,
		  AppAlarm, PrgGrp) ->
    Minor = (MajorId bsl 16) + MinorId,

    DefaultSeverity =
	if Severity =:= indeterminate ->
	       getDefaultSeverity(VendorId, Minor);
	   true ->
	       undefined
	end,

    send_alarm_comte(
      DN,
      VendorId,
      Minor,
      severity(Severity, DefaultSeverity),
      binarize(Text),
      Info,
      AppAlarm,
      PrgGrp).

get_alarms(AlarmName) when is_atom(AlarmName) ->
    Key = atom_to_list(AlarmName),
    [FmAlarmType] = mnesia:dirty_read({fmAlarmType, {"1","1","1","1",Key}}),
    Fun = fun() ->
		  mnesia:match_object(
		    #fmAlarm{majorType=FmAlarmType#fmAlarmType.majorType,
			     minorType=FmAlarmType#fmAlarmType.minorType,
			     _ = '_'})
	  end,
    {atomic, Alarms} = mnesia:transaction(Fun),
    transform_alarms(Alarms);

get_alarms({VendorId, MajorId, MinorId}) ->
    MajorType = VendorId,
    MinorType = (MajorId bsl 16) + MinorId,
    Fun = fun() ->
		  mnesia:match_object(#fmAlarm{majorType=MajorType,
					       minorType=MinorType,
					       _ = '_'})
	  end,
    {atomic, Alarms} = mnesia:transaction(Fun),
    transform_alarms(Alarms).

transform_alarms(Alarms) ->
    [transform_alarm(Alarm)||Alarm<-Alarms].

transform_alarm(Alarm) ->
    Fields = record_info(fields, fmAlarm),
    lists:zip(Fields, tl(tuple_to_list(Alarm))).


%%% ----------------------------------------------------------
%%% @doc Gets the default severity of an alarm when the
%%% 'major' and 'minor' attributes are known. It is trusted
%%% that the match operation cannot produce more than one
%%% #fmAlarmType{} record at most (this is ensured by the
%%% appdata parse routine).
%%% @end
%%% ----------------------------------------------------------
getDefaultSeverity(Major, Minor) ->
    case mnesia:dirty_match_object(
	   fmAlarmType,
	   #fmAlarmType{majorType=Major, minorType=Minor, _='_'}) of
	[#fmAlarmType{defaultSeverity=Severity}] ->
	    Severity;
	[] ->
	    undefined
    end.


%%% ----------------------------------------------------------
%%% @doc Returns the #fmAlarmType{} record that matches the
%%% given alarm, specified as an atom whose name equals the
%%% RDN value of the FmAlarmType instance.
%%% @end
%%% ----------------------------------------------------------
-spec get_alarm_type(atom()) -> #fmAlarmType{}.

get_alarm_type(AlarmName) ->
    E = "1",
    AlarmKey = {E, E, E, E, atom_to_list(AlarmName)},
    Fun = fun() -> mnesia:read({fmAlarmType, AlarmKey}) end,
    case mnesia:transaction(Fun) of
	{atomic, [Obj]} ->
	    Obj;
	{atomic, []} ->
	    case mnesia:dirty_read(fmAlarmType_notReg, AlarmKey) of
		[_] ->
		    throw(registerOnNodes_false);
		[] ->
		    erlang:error({no_such_alarm, AlarmName})
	    end;
	{aborted, Reason} ->
	    throw({aborted, {Reason, AlarmName}})
    end.


binarize(Text) when is_binary(Text) ->
    Text;
binarize(Text) when is_list(Text) ->
    list_to_binary(Text);
binarize(Other) ->
    throw({bad_info, Other}).



%%% ----------------------------------------------------------
%%% @doc Maps NTF severity to COMTE severity.
%%% @end
%%% ----------------------------------------------------------

-spec severity(comsaI:severity_type(), comSeverityLevel()|undefined) -> fmSeverity().

severity(indeterminate, ?SeverityLevel_CRITICAL) -> ?FmSeverityCritical;
severity(indeterminate, ?SeverityLevel_MAJOR) -> ?FmSeverityMajor;
severity(indeterminate, ?SeverityLevel_MINOR) -> ?FmSeverityMinor;
severity(indeterminate, ?SeverityLevel_WARNING) -> ?FmSeverityWarning;

severity(indeterminate, undefined) -> ?FmSeverityWarning;

severity(NtfSeverity, _DefaultSeverity) ->
    severity(NtfSeverity).


-spec severity(comsaI:severity_type()) -> fmSeverity().

severity(indeterminate) -> ?FmSeverityIndeterminate;
severity(warning) -> ?FmSeverityWarning;
severity(minor) -> ?FmSeverityMinor;
severity(major) -> ?FmSeverityMajor;
severity(critical) -> ?FmSeverityCritical;
severity(Other) -> throw({bad_severity, Other}).


%%% ----------------------------------------------------------
%%% @doc Clears an alarm.
%%%
%%% comsaServer is the only module that calls this function.
%%% @end
%%% ----------------------------------------------------------
clear_alarm(Alarm, DN, Text, Info, AppAlarm) ->
    try
	clear_alarm_helper(Alarm, DN, Text, Info, AppAlarm)
    catch
	throw : registerOnNodes_false ->
	    ?LOG_WARN(["Alarm specified in xml file NOT to be registered",
		       "This function should thus NOT be called",
		       {alarm, Alarm},
		       {dN, DN},
		       "--- Stacktrace ---"
		       | erlang:get_stacktrace()]);
	ErrClass : ErrReason ->
	    ?LOG_ERR([{alarm, Alarm},
		      {dN, DN},
		      {text, Text},
		      {info, Info},
		      {ErrClass, ErrReason},
		      "--- Stacktrace ---"
		      | erlang:get_stacktrace()])
    end.


%%% ----------------------------------------------------------
%%% @doc Clears an alarm specified as alarm instance.
%%% @end
%%% ----------------------------------------------------------
-spec clear_alarm_by_instance(
	non_neg_integer(),
	non_neg_integer(),
	binary()) -> any().

clear_alarm_by_instance(MajorType, MinorType, Dn) ->
    AppAlarm = true,
    PrgGrp = undefined,
    send_alarm_comte(Dn, MajorType, MinorType, ?FmSeverityCleared, <<"">>, [],
		     AppAlarm, PrgGrp).


clear_alarm_helper(AlarmName, DN, Text, Info, AppAlarm) when is_atom(AlarmName) ->
    FmAlarmTypeObj = get_alarm_type(AlarmName),
    PrgGrp = undefined,
    send_alarm_comte(DN,
		     FmAlarmTypeObj#fmAlarmType.majorType,
		     FmAlarmTypeObj#fmAlarmType.minorType,
		     ?FmSeverityCleared,
		     binarize(Text),
		     Info,
		     AppAlarm,
		     PrgGrp);

clear_alarm_helper({VendorId, MajorId, MinorId}, DN, Text, Info, AppAlarm) ->
    send_alarm_comte(DN,
		     VendorId,
		     (MajorId bsl 16) + MinorId,
		     ?FmSeverityCleared,
		     binarize(Text),
		     Info,
		     AppAlarm,
		     undefined).


-spec send_alarm_comte(binary(),
		       non_neg_integer(),
		       non_neg_integer(),
		       any(),
		       binary(),
		       list(),
		       boolean(),
		       any()) -> any().

send_alarm_comte(Dn, MajorType, MinorType, Severity, Text, Info, AppAlarm,
		 PrgGrp) ->
    % NOTE: The specification of the 1st argument of
    % comte:send_alarm/5 is binary() (in /main/R1A/R2A/18)
    % but the implementation accepts [binary()] as well.
    % (This has been pointed out to Magnus Liden; please
    % remove this note once the typing in COMTE has been
    % adjusted.)
    Additional =
	lists:append(
	  if Text =/= <<"">> -> [{add_text, Text}]; true -> [] end,
	  if Info =/= [] -> [{add_info, Info}]; true -> [] end),

    [FmAlarmType] = mnesia:dirty_match_object(#fmAlarmType{majorType=MajorType,
							   minorType=MinorType,
							   _ = '_'}),

    IsStateful = FmAlarmType#fmAlarmType.isStateful,
    IsAppAlarm = AppAlarm andalso IsStateful,
    update_app_alarm(IsAppAlarm, Dn, MajorType, MinorType, Severity, PrgGrp),

    Res =  comte:send_alarm(Dn,
			    MajorType,
			    MinorType,
			    Severity,
			    Additional),

    F = case Res of
	    ok -> info_report;
	    {error, _} -> warning_report
	end,

    sysInitI:F([{send_alarm, Res},
		{specificProblem, FmAlarmType#fmAlarmType.specificProblem},
		{dn, Dn},
		{major, MajorType},
		{minor, MinorType},
		{severity, reverse_severity(Severity)},
		{text, Text},
		{info, Info}]).


reverse_severity(?FmSeverityCleared) -> cleared;
reverse_severity(?FmSeverityIndeterminate) -> indeterminate;
reverse_severity(?FmSeverityWarning) ->  warning;
reverse_severity(?FmSeverityMinor) -> minor;
reverse_severity(?FmSeverityMajor) -> major;
reverse_severity(?FmSeverityCritical) -> critical.

update_app_alarm(true, Dn, MajorType, MinorType, ?FmSeverityCleared, _PrgGrp) ->
    Key = {MajorType, MinorType, Dn},
    ok = mnesia:dirty_delete({comsaAppAlarm, Key});
update_app_alarm(true, Dn, MajorType, MinorType, _Severity, PrgGrp) ->
    Key = {MajorType, MinorType, Dn},
    Rec = #comsaAppAlarm{key = Key, program_group = PrgGrp},
    ok = mnesia:dirty_write(Rec);
update_app_alarm(false, _Dn, _MajorType, _MinorType, _Severity, _PrgGrp) ->
    ok.

-spec get_app_alarms() -> [{MajorType :: non_neg_integer(),
			    MinorType :: non_neg_integer(),
			    DN        :: binary()}].
get_app_alarms() ->
    Pattern = #comsaAppAlarm{_ = '_'},
    L = mnesia:dirty_match_object(Pattern),
    lists:map(fun(#comsaAppAlarm{key = Key}) -> Key end, L).

get_app_alarms(ProgramGroup) ->
    Pattern = #comsaAppAlarm{program_group = ProgramGroup, _ = '_'},
    L = mnesia:dirty_match_object(Pattern),
    lists:map(fun(#comsaAppAlarm{key = Key}) -> Key end, L).

%%%%%% ALARM REGISTRATION %%%%%%

register_alarm(Name, MajorType, MinorType, MoClasses, SpecificProblem,
	       EventType, ProbableCause, AlarmType, AdditionalText) ->
    IsStateful = case AlarmType of
		     alarm -> true;
		     alert -> false
		 end,
    case mnesia:transaction(
	   fun() ->
		   Key = {"1","1","1","1", atom_to_list(Name)},
		   mnesia:write(
		     #fmAlarmType{fmAlarmTypeId = Key,
				  majorType = MajorType,
				  minorType = MinorType,
				  moClasses = MoClasses,
				  specificProblem = SpecificProblem,
				  eventType = eventType(EventType),
				  probableCause = ProbableCause,
				  isStateful = IsStateful,
				  additionalText = AdditionalText})
	   end) of
	{atomic, ok} -> ok;
	{aborted, Reason} ->
	    erlang:error({aborted, Reason},
			 [Name, MajorType, MinorType, MoClasses,
			  SpecificProblem, EventType, ProbableCause,
			  IsStateful, AdditionalText])
    end.

eventType(other) -> ?EventType_OTHER;
eventType(communications) -> ?EventType_COMMUNICATIONSALARM;
eventType(qos) -> ?EventType_QUALITYOFSERVICEALARM;
eventType(processing) -> ?EventType_PROCESSINGERRORALARM;
eventType(equipment) -> ?EventType_EQUIPMENTALARM;
eventType(environmental) -> ?EventType_ENVIRONMENTALALARM;
eventType(integrityViolation) -> ?EventType_INTEGRITYVIOLATION;
eventType(operationalViolation) -> ?EventType_OPERATIONALVIOLATION;
eventType(physicalViolation) -> ?EventType_PHYSICALVIOLATION;
eventType(securityViolation) -> ?EventType_SECURITYSERVICEORMECHANISMVIOLATION;
eventType(timeDomainViolation) -> ?EventType_TIMEDOMAINVIOLATION;

eventType("OTHER") -> ?EventType_OTHER;
eventType("COMMUNICATIONSALARM") -> ?EventType_COMMUNICATIONSALARM;
eventType("QUALITYOFSERVICEALARM") -> ?EventType_QUALITYOFSERVICEALARM;
eventType("PROCESSINGERRORALARM") -> ?EventType_PROCESSINGERRORALARM;
eventType("EQUIPMENTALARM") -> ?EventType_EQUIPMENTALARM;
eventType("ENVIRONMENTALALARM") -> ?EventType_ENVIRONMENTALALARM;
eventType("INTEGRITYVIOLATION") -> ?EventType_INTEGRITYVIOLATION;
eventType("OPERATIONALVIOLATION") -> ?EventType_OPERATIONALVIOLATION;
eventType("PHYSICALVIOLATION") -> ?EventType_PHYSICALVIOLATION;
eventType("SECURITYSERVICEORMECHANISMVIOLATION") ->
    ?EventType_SECURITYSERVICEORMECHANISMVIOLATION;
eventType("TIMEDOMAINVIOLATION") -> ?EventType_TIMEDOMAINVIOLATION.

%%%%%% INSTRUMENTATION %%%%%%

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

getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).

setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).

%%% @doc Creates a new instance. The given class name is trusted to be
%%% one of the names supported by this module.

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

deleteMo(_, _) ->
    ok.

table("Fm") -> fm;
table("FmAlarmModel") -> fmAlarmModel;
table("FmAlarmType") -> fmAlarmType;
table("FmAlarm") -> fmAlarm.

types(fm) -> ?fm_types;
types(fmAlarmModel) -> ?fmAlarmModel_types;
types(fmAlarmType) -> ?fmAlarmType_types;
types(fmAlarm) -> ?fmAlarm_types.

values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

validate(_DN, User, _Tx) ->
    {ok,User}.
prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, _Tx) ->
    ok.

init_data(upgrade) ->
    swmI:copy_old_table(fm),
    swmI:copy_old_table(fmAlarmModel),

    %% For FmAlarmTypes we need to get the configured severity stored
    %% for any legacy alarms, while at the same time reading the other
    %% alarm info from the appdata files
    [OldSchema] = swmI:read(schema, fmAlarmType),
    OldAttrs = proplists:get_value(attributes, element(3, OldSchema)),

    OldObjects = swmI:all_objects(fmAlarmType),
    Data = [lists:zip(OldAttrs, tl(tuple_to_list(OldObj)))||OldObj<-OldObjects],
    Severities =
	[{proplists:get_value(fmAlarmTypeId, Props),
	  proplists:get_value(configuredSeverity, Props)}||
	    Props<-Data],
    comsaLib:set_variable(fmAlarmTypeConfigSeverity, Severities);

    %% swmI:copy_old_table(fmAlarmType),
    %% swmI:copy_old_table(fmAlarm);

init_data(fromScratch) ->

    TimeStr = comsaLib:iso_time(os:timestamp(), extended_zonefree),
    mnesia:dirty_write(#fm{fmId = {"1","1","1"},
			   lastSequenceNo = 0,
			   sumCritical = 0,
			   sumMajor = 0,
			   sumMinor = 0,
			   sumWarning = 0,
			   totalActive = 0,
			   lastChanged = TimeStr,
			   heartbeatInterval = ?fm_heartbeatInterval_default}),

    mnesia:dirty_write(
      #fmAlarmModel{fmAlarmModelId = {"1","1","1","1"}}).


-spec appdata(string(), string(), #xmlElement{}) -> ok.

appdata(ProductId, _Version, AppdataE) ->
    Severities = case comsaLib:get_variable(fmAlarmTypeConfigSeverity) of
		     undefined -> [];
		     S -> S
		 end,
    parse_alarm(AppdataE#xmlElement.content, Severities, ProductId).

parse_alarm([AlarmTypeE|Content], Severities, ProductId)
  when AlarmTypeE#xmlElement.name == alarmtype ->
    try do_parse_alarm(AlarmTypeE, Severities, ProductId)
    catch T:E ->
	    sysInitI:error_report(
	      [{mfa, {?MODULE, do_parse_alarm, [AlarmTypeE]}},
	       {T,E}]),
	    erlang:T(E)
    end,
    parse_alarm(Content, Severities, ProductId);
parse_alarm([_|Content], Severities, ProductId) ->
    parse_alarm(Content, Severities, ProductId);
parse_alarm([], _, _) ->
    ok.


-spec do_parse_alarm(#xmlElement{}, [{tuple(), integer()}], string()) -> any().

do_parse_alarm(AlarmTypeE, Severities, ProductId) ->
    do_parse_alarm(is_registerOnNode(AlarmTypeE),
		   AlarmTypeE,
		   Severities,
		   ProductId).

do_parse_alarm(true, AlarmTypeE, Severities, ProductId) ->
    MajorTypeStr = find_attribute(majorType, AlarmTypeE),
    MinorTypeStr = find_attribute(minorType, AlarmTypeE),
    MoClasses = find_attribute(moClasses, AlarmTypeE),
    SpecificProblem = find_attribute(specificProblem, AlarmTypeE),
    EventTypeStr = find_attribute(eventType, AlarmTypeE),
    ProbableCauseStr = find_attribute(probableCause, AlarmTypeE),
    IsStatefulStr = find_attribute(isStateful, AlarmTypeE),
    AdditionalText = find_attribute(additionalText, AlarmTypeE),
    DefaultSeverity =
	try find_attribute(defaultSeverity, AlarmTypeE) of
	    DS ->
		try
		    severity_type(DS)
		catch
		    throw:badarg ->
			sysInitI:error_msg(
			  "invalid severity type: ~p, "
			  "product: ~p,~n"
			  "alarm type: ~p~n",
			  [DS, ProductId, AlarmTypeE]),
			erlang:error({bad_severity_type, DS})
		end
	catch _:_ ->
		undefined
	end,

    Name = find_name(AlarmTypeE),

    MajorType = list_to_integer(MajorTypeStr),
    MinorType = list_to_integer(MinorTypeStr),
    EventType = eventType(EventTypeStr),
    ProbableCause = list_to_integer(ProbableCauseStr),
    IsStateful = if IsStatefulStr=="true" -> true; true -> false end,
    Key = {"1","1","1","1", Name},

    ConfiguredSeverity = proplists:get_value(Key, Severities, undefined),

    MoClassesOrdered = orderCommaSepList(MoClasses),

    Obj = #fmAlarmType{fmAlarmTypeId = Key,
		       majorType = MajorType,
		       minorType = MinorType,
		       moClasses = MoClassesOrdered,
		       specificProblem = SpecificProblem,
		       eventType = EventType,
		       probableCause = ProbableCause,
		       isStateful = IsStateful,
		       additionalText = AdditionalText,
		       configuredSeverity = ConfiguredSeverity,
		       defaultSeverity = DefaultSeverity},
    Fun =
	fun() ->
		case mnesia:match_object(
		       #fmAlarmType{majorType=MajorType,
				    minorType=MinorType, _='_'}) of
		    [] ->
			% no collision, store the alarm type now
			mnesia:write(Obj);
		    [#fmAlarmType{moClasses=MCOld,
				  specificProblem=SPOld,
				  eventType=ETOld,
				  probableCause=PCOld,
				  isStateful=ISOld,
				  additionalText=ATOld,
				  configuredSeverity=CSOld,
				  defaultSeverity=DSOld}=AlarmTypeOld] when
		      SPOld =:= SpecificProblem,
		      ETOld =:= EventType,
		      PCOld =:= ProbableCause,
		      ISOld =:= IsStateful,
		      ATOld =:= AdditionalText,
		      CSOld =:= ConfiguredSeverity ->
			% matching alarm found, merge MO classes attribute
			MoClassesMerged =
			    uniteCommaSepLists(MCOld, MoClassesOrdered),
			mnesia:write(
			  AlarmTypeOld#fmAlarmType{moClasses=MoClassesMerged}),
			if
			    DSOld =/= DefaultSeverity ->
				% NOTE: Tolerate ill-defined default severity
				% for now.
				% TODO, treat as error later on when all
				% applications define defaultSeverity
				% consistently; this can be checked offline
				% with the rcs_upinspect script.
				sysInitI:warning_report(
				  ["ignored attempt to "
				   "redefine default severity",
				   {majorType, MajorType},
				   {minorType, MinorType},
				   {specificProblem, SpecificProblem},
				   {defaultSeverity, {{old, DSOld},
						      {new, DefaultSeverity}}},
				   {"occurs in CXP", ProductId}]);
			    true ->
				ok
			end;
		    _ ->
			mnesia:abort({alarm_type_already_defined,
				      MajorType,
				      MinorType})
		end
	end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    ok;
	{aborted, {alarm_type_already_defined, MajorType, MinorType}} ->
	    case ProductId of
		"CXP9025546_3"++_ ->
		    sysInitI:error_report(
		      ["Failed to register RCS alarm type, "
		       "because of existing registration",
		       {majorType, MajorType},
		       {minorType, MinorType},
		       {specificProblem, SpecificProblem},
		       {"occurs in CXP", ProductId}]),
		    erlang:error({alarm_registration_failed, SpecificProblem});
		_ ->
		    sysInitI:error_report(
		      ["attempt to redefine alarm type, *** IGNORED ***",
		       {majorType, MajorType},
		       {minorType, MinorType},
		       {specificProblem, SpecificProblem},
		       {"occurs in CXP", ProductId}])
	    end;
	{aborted, Reason} ->
	    sysInitI:error_report(
	      [{?MODULE, do_parse_alarm, [AlarmTypeE]},
	       {mfa, {mnesia, write, [Obj]}},
	       {aborted, Reason}])
    end;
do_parse_alarm(false, AlarmTypeE, _, _) ->
    AlrmName = find_name(AlarmTypeE),
    Key = {"1", "1", "1", "1", AlrmName},
    RegisterOnNodes = find_registerOnNodes(AlarmTypeE),
    NodeType = swmI:node_type(),
    mnesia:dirty_write(#fmAlarmType_notReg{fmAlarmTypeId = Key,
					   node_type = NodeType,
					   registerOnNodes = RegisterOnNodes}),
    ?LOG_INFO([{"Alarmtype not registered",
		"Node Type not specified in the registerOnNodes list."},
	       {alarmtype_name, AlrmName},
	       {node_type, NodeType},
	       {registerOnNodes, RegisterOnNodes}]);
do_parse_alarm(error, _, _, _) ->
    ok.

%%% ----------------------------------------------------------
find_name(AlarmTypeE) ->
    try
	find_attribute(name, AlarmTypeE)
    catch
	_ : _ ->
	    strip_space(find_attribute(specificProblem, AlarmTypeE))
    end.

%%% ----------------------------------------------------------
find_registerOnNodes(AlarmTypeE) ->
    RegisterOnNodesE = find_optional_element(registerOnNodes, AlarmTypeE),
    [find_attribute(type, NodeE)
	 || NodeE <- find_elements(node, RegisterOnNodesE)].

%%% ----------------------------------------------------------
is_registerOnNode(AlarmTypeE) ->
    RegisterOnNodes = find_registerOnNodes(AlarmTypeE),
    NodeType = swmI:node_type(),
    try
	ok = validate_registerOnNodes(RegisterOnNodes),
	lists:member(NodeType, RegisterOnNodes)
    catch
	throw : registerOnAllNodes ->
	    true;
	throw : {warnings, _Warnings} ->
	    %% MoClasses = (catch find_attribute(moClasses, AlarmTypeE)),
	    %% ?LOG_WARN([{alarmtype_name, find_name(AlarmTypeE)},
	    %% 	       {moClasses, MoClasses},
	    %% 	       "--- Anomalies in xml file ---"
	    %% 	       | _Warnings] ++
	    %% 	      ["--- Stacktrace ---"
	    %% 	       | erlang:get_stacktrace()]),
	    lists:member(NodeType, RegisterOnNodes);
	ErrClass : ErrReason ->
            Stack = erlang:get_stacktrace(),
	    MoClasses = (catch find_attribute(moClasses, AlarmTypeE)),
	    ?LOG_ERR([{"Error in xml file", "Not able to register alarmtype"},
		      {alarmtype_name, find_name(AlarmTypeE)},
		      {moClasses, MoClasses},
		      {node_type, NodeType},
		      {ErrClass, ErrReason},
		      "--- Stacktrace ---"
		      | Stack]),
	    error
    end.

%%% ----------------------------------------------------------
validate_registerOnNodes([]) ->   % Default, legacy, = "ALL"
    throw(registerOnAllNodes);
validate_registerOnNodes(["ALL"]) ->
    throw(registerOnAllNodes);
validate_registerOnNodes(NodeTypes) ->
    case lists:member("ALL", NodeTypes) of
	false ->
	    validate_registerOnNodes_list(NodeTypes);
	true ->
	    erlang:error({illegal_combination, registerOnNodes, NodeTypes})
    end.

validate_registerOnNodes_list([NodeType | Tail]) ->
    case swmI:is_node_type_valid(NodeType) of
	true ->
	    validate_registerOnNodes_list(Tail);
	deprecated ->
	    add_proc_dict(registerOnNodes,
			  [{"Deprecated Node Type", NodeType}]),
	    validate_registerOnNodes_list(Tail);
	false ->
	    erlang:error({illegal_node_type, registerOnNodes, NodeType})
    end;
validate_registerOnNodes_list([]) ->
    case erase(registerOnNodes) of
	undefined ->
	    ok;
	Values ->
	    throw({warnings, Values})
    end.

%%% ----------------------------------------------------------
add_proc_dict(Tag, Values) ->
    case get(Tag) of
	undefined ->
	    put(Tag, Values);
	OldValues ->
	    put(Tag, OldValues ++ Values)
    end.

%%% ----------------------------------------------------------
%%% @doc Maps severity from string (as used in the
%%% defaultSeverity attribute of the alarmtype element) to
%%% an enum defined in COM.
%%% @end
%%% ----------------------------------------------------------
-spec severity_type(string()) -> comSeverityLevel().

severity_type("CRITICAL") -> ?SeverityLevel_CRITICAL;
severity_type("MAJOR") -> ?SeverityLevel_MAJOR;
severity_type("MINOR") -> ?SeverityLevel_MINOR;
severity_type("WARNING") -> ?SeverityLevel_WARNING;
severity_type(_) -> throw(badarg).

%%% ----------------------------------------------------------
%%% @doc Removes characters we expect are not liked in a key:
%%% spaces, equals signs, commas.
%%% @end
%%% ----------------------------------------------------------
-spec strip_space(string()) -> string().

strip_space(String) ->
    [X||X<-String,
	X /= $\s,
	X /= $=,
	X /= $,].


%%% ----------------------------------------------------------
%%% @doc Normalizes a comma-separated list of words by
%%% lexicographic ordering and removal of duplicates.
%%% @end
%%% ----------------------------------------------------------

orderCommaSepList(String) ->
    uniteCommaSepLists(String, "").


%%% ----------------------------------------------------------
%%% @doc Unites two comma-separated lists of words.
%%% @end
%%% ----------------------------------------------------------

uniteCommaSepLists(A, B) ->
    ASet = ordsets:from_list(string:tokens(A, ",")),
    BSet = ordsets:from_list(string:tokens(B, ",")),
    string:join(ordsets:to_list(ordsets:union(ASet, BSet)), ",").


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions:
%%% Description: Finds a sub element to an xml element, or in a list
%%%              of element contents. Assumes there is only one element
%%%              with the same name
%%% ----------------------------------------------------------

%% find_element(ElementName, Element) when is_record(Element, xmlElement) ->
%%     find_element(ElementName, Element#xmlElement.content);
%% find_element(ElementName, ContentList) ->
%%     case lists:keysearch(ElementName, #xmlElement.name, ContentList) of
%% 	{value, Element} ->
%% 	    Element;
%% 	false ->
%% 	    erlang:error({badmatch, false}, [ElementName, ContentList])
%%     end.

%%% ----------------------------------------------------------
find_optional_element(ElementName, Element)
  when is_record(Element, xmlElement) ->
    ContentList = Element#xmlElement.content,
    case lists:keyfind(ElementName, #xmlElement.name, ContentList) of
	false ->
	    false;
	Value  ->
	    Value
    end.

%%% ----------------------------------------------------------
find_elements(_, false) ->
    [];
find_elements(ElementName, Element) when is_record(Element, xmlElement) ->
    find_elements(ElementName, Element#xmlElement.content);
find_elements(ElementName, ContentList) ->
    case sysUtil:keyfind_all(ElementName, #xmlElement.name, ContentList) of
	false ->
	    [];
	Elems ->
	    Elems
    end.

%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions:
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ----------------------------------------------------------

find_attribute(AttributeName, Element) when is_record(Element, xmlElement) ->
    find_attribute(AttributeName, Element#xmlElement.attributes);
find_attribute(AttributeName, AttributeList) ->
    case lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList) of
        {value, Attribute} ->
            Attribute#xmlAttribute.value;
        false ->
            erlang:error({badmatch, false}, [AttributeName, AttributeList])
    end.

%% find_text(Element) when is_record(Element, xmlElement) ->
%%     [Text] = Element#xmlElement.content,
%%     Text#xmlText.value.

%% info_msg(Format) ->
%%     info_msg(Format, []).

%% info_msg(Format, Args) ->
%%     sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%error_msg(Format) ->
%    error_msg(Format, []).
%% error_msg(Format, Args) ->
%%     sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
