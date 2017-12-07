%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	coiMib.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R4A/R11A/1

%%% @doc == COI - Control system Oam Interface ==
%%% This module contains functions for reading the Management Information
%%% Base (MIB) which supports the coi.erl interface functionality.
%%%
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(coiMib).
-vsn('/main/R4A/R11A/1').
-date('2017-10-17').
-author(etxberb).

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%%% Rev    Date       Name     What
%% ----    ---------- -------  ------------------------------------------------
%% R4A/1   2015-10-21 etxberb  Created.
%% R11A/1  2017-10-17 etxpeno  OTP 20 fixes
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 2.1.1 Interface functions
%%% ###---------------------------------------------------------------------###
-export([init/0]).
-export([init_tables/1]).
-export([post_init/0]).

-export([getMibFiles/0,
	 mimVal_to_mibStr/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ###---------------------------------------------------------------------###
%%% # Extended Interface
%%% ###---------------------------------------------------------------------###

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###
-include("coi.hrl").
-include_lib("snmp/include/snmp_types.hrl").

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
-define(ASN1TYPE_EventType, #asn1_type{assocList = [?ENUMs_EventType],
				       aliasname = 'Macro_EventType'}).

%% Copied from /software/RCS-../COM../com-../priv/tgt_../opt/com/etc/mibs/
%% IANA-ITU-ALARM-TC-MIB.mib:
-define(ENUMs_EventType, {enums, [{other, 1},
				  {communicationsAlarm, 2},
				  {qualityOfServiceAlarm, 3},
				  {processingErrorAlarm, 4},
				  {equipmentAlarm, 5},
				  {environmentalAlarm, 6},
				  {integrityViolation, 7},
				  {operationalViolation, 8},
				  {physicalViolation, 9},
				  {securityServiceOrMechanismViolation, 10},
				  {timeDomainViolation, 11}]}).

-define(TblName, ?MODULE).

%% General
-define(ELSE, true).

-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###
-record(enum, {attrDTName :: atom()    | undefined,
	       value      :: integer() | undefined}).

-record(?TblName, {key   :: #enum{} | undefined,
 		   value :: atom()  | undefined}).

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Create tables.
%%%
%%% @end
%%% ###=====================================================================###
init_tables(DbNodes) ->
    {atomic, ok} =
	clhI:mnesia_create_table(?TblName,
				 [{type, ordered_set},
				  {disc_copies, DbNodes},
				  {attributes, record_info(fields, ?TblName)}]).

%%% ###########################################################################
%%% @doc The 'init' start phase.
%%%
%%% @end
%%% ###=====================================================================###
init() ->
    ok.

%%% ###########################################################################
%%% @doc Read MIB files and populate the MIB table.
%%%
%%% @end
%%% ###=====================================================================###
post_init() ->
    MibFiles = getMibFiles(),
    read_mibs(MibFiles),
    populate_tbl([?ASN1TYPE_EventType]),
    NoOfMibFiles = sysUtil:term_to_string(length(MibFiles)),
    Info =
	[{?MODULE, ?FUNCTION},
	 "####### " ++ NoOfMibFiles ++ " MIB files read #######",
	 [lists:last(string:tokens(MibFile, "/")) || MibFile <- MibFiles],
	 {table_info, {?TblName, [Item ||
				     {Tag, _} = Item
					 <- mnesia:table_info(?TblName, all),
				     Tag == size orelse Tag == memory]}}],
    error_logger:info_report(Info),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @doc Get a list of file names of all MIB files in the system.
%%
%% @end
%%% ###=====================================================================###
getMibFiles() ->
    Ls = os:cmd("ls " ++ sysEnv:com_mibs() ++ "/*MIB.bin"),
    case string:str(Ls, "No such file or directory") of
	0 ->
	    string:tokens(Ls, "\n");
	_ ->
	    LsLa = string:tokens(os:cmd("ls -la " ++ sysEnv:com_mibs()), "\n"),
	    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				     "No MIB files found.",
				     Ls,
				     LsLa]),
	    []
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @doc Translate from raw integer value in the MIM to the corresponding string
%%   defined in the MIB.
%%
%% @end
%%% ###=====================================================================###
mimVal_to_mibStr(AttrDTName, AttrValue) when is_list(AttrDTName) ->
    PossibleMibNames = ["Eri" ++ AttrDTName,
			AttrDTName,
			"Macro_" ++ AttrDTName],   % Defined above.
    mimVal_to_str(PossibleMibNames, AttrValue, AttrDTName);
mimVal_to_mibStr(ADTN, AVal) ->
    Stack = element(2, process_info(self(), current_stacktrace)),
    sysInitI:error_report([{?MODULE, ?FUNCTION},
			   "AttributeDataTypeName is not a string.",
			   {'AttributeDataTypeName', ADTN},
			   {'AttributeValue', AVal},
			   {stacktrace, Stack}]),
    sysUtil:term_to_string(AVal).

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% mimVal_EventType_to_str
%%%
%%% ###=====================================================================###
mimVal_to_str([AttrDTName | Tail], Value, OrigADTN) ->
    try mnesia:dirty_read(?TblName,
			  #enum{attrDTName = list_to_atom(AttrDTName),
				value = Value})
	of
	[#?TblName{value = Enum}] ->
	    sysUtil:term_to_string(Enum);
	[] ->
	    mimVal_to_str(Tail, Value, OrigADTN)
    catch
	ErrClass : ErrReason ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {'AttributeDataTypeName', OrigADTN},
				   {'AttributeValue', Value},
				   {ErrClass, ErrReason},
				   {stacktrace, erlang:get_stacktrace()}]),
	    sysUtil:term_to_string(Value)
    end;
mimVal_to_str([], Value, OrigADTN) ->
    Stack = element(2, process_info(self(), current_stacktrace)),
    sysInitI:error_report([{?MODULE, ?FUNCTION},
			   "Value not found in MIB.",
			   {'AttributeDataTypeName', OrigADTN},
			   {'AttributeValue', Value},
			   {stacktrace, Stack}]),
    sysUtil:term_to_string(Value).

%%% ###########################################################################
%%% populate_tbl
%%%
%%% ###=====================================================================###
populate_tbl([#asn1_type{assocList = AList,
			 aliasname = AName} | Tail])
  when AName /= 'INTEGER' andalso
       AName /= 'OCTET STRING' andalso
       AName /= 'BIT STRING' andalso
       AName /= 'OBJECT IDENTIFIER' andalso
       AName /= 'BITS' ->
    populate_tbl_enums(lists:keyfind(enums, 1, AList), AName),
    populate_tbl(Tail);
populate_tbl([_ | Tail]) ->
    populate_tbl(Tail);
populate_tbl([]) ->
    ok.

%%% ###=====================================================================###
populate_tbl_enums([{Enum, Value} | Tail], AName) ->
    mnesia:dirty_write(?TblName,
		       #?TblName{key = #enum{attrDTName = AName,
					     value = Value},
				 value = Enum}),
    populate_tbl_enums(Tail, AName);
populate_tbl_enums({enums, Enums}, AName) ->
    populate_tbl_enums(Enums, AName);
populate_tbl_enums(_, _) ->
    ok.

%%% ###########################################################################
%%% read_mibs
%%%
%%% ###=====================================================================###
read_mibs([MIB | Tail]) ->
    try snmp:read_mib(MIB) of
	{ok, #mib{asn1_types = Asn1Types}} ->
	    populate_tbl(Asn1Types);
	Error ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {'snmp:read_mib', Error},
				   {mib, MIB}])
    catch
	ErrClass : ErrReason ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   "snmp:read_mib",
				   {ErrClass, ErrReason},
				   {stacktrace, erlang:get_stacktrace()}])
    end,
    read_mibs(Tail);
read_mibs([]) ->
    ok.

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###
