%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	coi.hrl %
%%% @author erarafo
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R3A/R4A/R5A/R6A/1

%%% @doc == COI - Control system Oam Interface ==
%%%  Definitions exported to other blocks.
%%%
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-hrl_vsn('/main/R3A/R4A/R5A/R6A/1').
-hrl_date('2016-04-22').
-hrl_author('erarafo').

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%%% Rev    Date       Name     What
%% ------  ---------- -------  ------------------------------------------------
%% R3A/1   2015-01-09 etxberb  First version for delivery.
%% R3A/2   2015-02-13 etxberb  Added coi_notification_0.
%% R4A/1   2015-05-28 etxberb  Added backwards_relation in coi_mim_tree().
%% R4A/4   2015-10-13 etxberb  Added string() to mo_name().
%% R5A/1   2016-01-08 etxpeno  add type sec_ip() (TR HU49263)
%% R5A/2   2016-04-21 erarafo  Type definitions added.
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    DEFINITIONS
%%% ###---------------------------------------------------------------------###

%% COI Basic data types taken from COMTE

%%     CoiOamSpiDatatype_INT8 = 1
-define(INT8, 1).
-define(INT8(Int),{?INT8, Int}).
%%     CoiOamSpiDatatype_INT16 = 2
-define(INT16, 2).
-define(INT16(Int),{?INT16, Int}).
%%     CoiOamSpiDatatype_INT32 = 3
-define(INT32, 3).
-define(INT32(Int),{?INT32, Int}).
%%     CoiOamSpiDatatype_INT64 = 4
-define(INT64, 4).
-define(INT64(Int),{?INT64, Int}).
%%     CoiOamSpiDatatype_UINT8 = 5
-define(UINT8, 5).
-define(UINT8(Int),{?UINT8, Int}).
%%     CoiOamSpiDatatype_UINT16 = 6
-define(UINT16, 6).
-define(UINT16(Int),{?UINT16, Int}).
%%     CoiOamSpiDatatype_UINT32 = 7
-define(UINT32, 7).
-define(UINT32(Int),{?UINT32, Int}).
%%     CoiOamSpiDatatype_UINT64 = 8
-define(UINT64, 8).
-define(UINT64(Int),{?UINT64, Int}).
%%     CoiOamSpiDatatype_STRING = 9
-define(STRING, 9).
-define(STRING(Str),{?STRING, Str}).
%%     CoiOamSpiDatatype_BOOL = 10
-define(BOOL, 10).
-define(BOOL(Bool),{?BOOL, Bool}).
%%     CoiOamSpiDatatype_REFERENCE = 11
-define(REFERENCE, 11).
-define(REFERENCE(Ref), {?REFERENCE, Ref}).
%%     CoiOamSpiDatatype_ENUM = 12
-define(ENUM, 12).
-define(ENUM(Enum),{?ENUM, Enum}).
%%     CoiOamSpiDatatype_STRUCT = 14
-define(STRUCT, 14).
-define(STRUCT(StructMembers),{?STRUCT,StructMembers}).
-define(STRUCT_MEMBER(Name,Value),{Name,Value}).

%%% Not implemented
%%     CoiOamSpiDatatype_DERIVED = 13
%%     CoiOamSpiDatatype_VOID = 15


%% A transaction identifier
-type coi_transaction_id() :: {coi,
			       ApplName :: atom() | string(),
			       User :: atom() | string(),
			       integer()}.

%% An MO name, i.e. `<<"ManagedElement">>' or `<<"Authorization">>'.
-type mo_name()        :: binary() | string().

%% An MO identifier, i.e. `<<"ManagedElementId">>' or `<<"AuthorizationId">>'.
-type mo_id_name()        :: binary().

%% An MO child (instance), i.e. `<<"1">>' or `<<"InstanceName1">>'.
-type mo_child()        :: binary().

%% An MO id value, i.e. {?STRING, <<"1">>} or {?STRING, <<"InstanceName1">>}.
-type mo_value()        :: {?STRING, mo_child()}.   % Subset of coi_value()

%% The name of an MO attribute, i.e <<"siteLocation">>
-type mo_attribute_name() :: binary().

%% The name of an MO action, i.e <<"exportAvailabilityLog">>
-type mo_action_name() :: binary().

%% An Ericsson Common Information Model distingushed name represented
%% as a binary.
%% Example:
%% The ManagedElement=1,SystemFunctions=1,SecM=1,Authorization=1,Local=1 DN
%% should be translated to:
%%   <<"ManagedElement=1,SystemFunctions=1,SecM=1,Authorization=1,Local=1">>.
-type ecim_dn() :: binary().

%% The ECIM (Ericsson Common Information Model) Path is a unique path
%% specifying a MO or an attribute within an MO. This is a reversed list
%% of string binaries representing the MO distinguished name in 3GPP format.
%%
%% Example: ManagedElement=1,TestMO=jimmy,test_string
%% becomes
%% `[<<"test_string">>,<<"jimmy">>,<<"TestMO">>,<<"1">>,<<"ManagedElement">>]'.
-type ecim_path() :: list(binary()).

%% The class path is a unique path specifying a class in the MOM
%% For example:
%% <<"/ManagedElement/SystemFunctions/SecM/UserManagement/LocalAuthorizationMethod/CustomRule">>
-type ecim_class_path() :: binary().

%% A user defined object for a root ECIM path
-type user_object() :: term().

%% The failure reason for the operation which failed.
-type error_reason() :: binary().



%% A member of a COI struct.
%%
%% Example struct: If you want to return a struct looking like this:
%%   `{ id => "1", val => 8}',
%% you should construct the following type:
%%   `?STRUCT([?STRUCT_MEMBER(<<"id">>,?STRING(<<"1">>)),?STRUCT_MEMBER(<<"val">>,?UINT8(8))])'.
-type coi_struct_member() :: {Name :: mo_attribute_name(), coi_value() | list(coi_value())}.

%% A COI value which is tagged using the macros defined in
%% comte/include/comte_types.hrl. The value in a coi_value() term can be
%% extracted using the same macros in comte/include/comte_types.hrl.
%% Note that the ?STRING datatype is always represented as an binary string.
%% Use binary_to_list to convert it to a normal string.
%%
%% The available types are: UINT8, UINT16, UINT32, UINT64, INT8, INT16, INT32,
%%                          INT64, STRING, ENUM, BOOL, STRUCT, REFERENCE
%%
%% Example: If you want to return 19 as an uint16 from getMoAttribute you have
%% to tag the integer using ?UINT16 like this: ?UINT16(19). If you then later
%% want to extract the value you can use ?UINT16 to match it out, like this:
%% ?UINT16(Integer) = ?UINT16(19).
-type coi_value_prim() :: {?UINT8, 0..255} |
		     {?UINT16, 0..65535} |
		     {?UINT32, 0..4294967295} |
		     {?UINT64, 0..18446744073709551615} |
		     {?INT8, -128..127} |
		     {?INT16, -32768..32767} |
		     {?INT32, -2148483648..2147483647} |
		     {?INT64, -9223372036854775808..9223372036854775807} |
		     {?STRING, binary()} |
		     {?ENUM, -32768..32767} |
		     {?BOOL, boolean()} |
		     {?REFERENCE, binary()}.

-type coi_value() :: coi_value_prim() |
		     {?STRUCT, list(coi_struct_member())}.


%% Value that may occur as a MO attribute value according to ECIM
%% guidelines. The worst case complexity is restricted to sequence
%% of struct with primitive members, or struct with members that are
%% sequence of primitive values.

-type coi_value_restr() :: []|coi_value_prim()|coi_value_seq()|coi_value_struct().

-type coi_value_seq() :: nonempty_list(coi_value_prim()|coi_value_struct_restr()).

-type coi_value_struct_restr() :: {?STRUCT, nonempty_list(coi_struct_member_prim())}.

-type coi_struct_member_prim() :: {mo_attribute_name(), coi_value_prim()|[]}.

-type coi_value_struct() :: {?STRUCT, nonempty_list(coi_struct_member_prim()|coi_struct_member_seq_prim())}.

-type coi_struct_member_seq_prim() :: {mo_attribute_name(), nonempty_list(coi_value_prim())|[]}.


%% Named attribute which is used when sending events using comte:notify
-type coi_named_attribute() :: {mo_attribute_name(),
				coi_value() | [coi_value()] | undefined}.

%% Named parameter in actions
-type coi_named_parameter() :: {binary(), coi_value()}.

%% Model information
%% A mim class string, i.e. `"ManagedElement"'.
-type coi_mim_class_name() :: list().   % string()

%% A file name including absolute path, e.g.
%% `"/home/sirpa/software/RCS-SIM_CXP9021221_2_R3A185/LOG_CXC1733858/log-R3A07/priv/model/RcsLogM.xml"'.
-type coi_mim_file() :: list().   % string()

%% Model info values. The info list corresponds to the hiearchical structure
%% in the MIM files. 'Tag' is directly translated from string to atom and
%% 'Value' is exactly the same as in the MIM file.
-type coi_mim_info() :: list(coi_mim_element()).

-type coi_mim_element() :: {Tag :: atom(), coi_mim_info()} |
			   {Tag :: atom(), Value :: string()} |
			   {Tag :: atom(), NoValue :: []}.

-type coi_mim_tree() :: {coi_mim_class_name(),
			 list(coi_mim_tree()) | backwards_relation}.

-type coi_mo_tree() :: {{coi_mim_class_name(),
			 mo_value() | undefined | {error, error_reason()}},
			list(coi_mo_tree())}.

%% COI Notifications:
%% --------------------------------------------------------------------------
%% This type,indicates the source of the operation that led to the generation
%% of this notification. The values are aligned with 3GPP TS 32.662
%% It can have one of the following values:
%%
%% ResourceOperation:   The notification was generated in response to an
%%                      internal operation of the resource;
%%
%% ManagementOperation: The notification was generated in response to a
%%                      management operation applied across the managed
%%                      object boundary external to the managed object;
%%
%% SonOperation:        The notification was generated as result of a SON
%%                      (Self Organising Network, used in Radio networks)
%%                      process like self-configuration, self-optimization,
%%                      self-healing etc.
%%                          A system (MW) that has no support
%%                      for SON will not use this value
%%
%% Unknown:             It is not possible to determine the source
%%                      of the operation.
%% --------------------------------------------------------------------------
-define(ResourceOperation,   1).
-define(ManagementOperation, 2).
-define(SonOperation,        3).
-define(Unknown,             4).

-type coi_source_indicator() :: ?ResourceOperation
                              | ?ManagementOperation
                              | ?SonOperation
                              | ?Unknown.

%% --------------------------------------------------------------------------
%% This type, indicates an event type. It can have one of the following values:
%%
%% MoCreated:            An MO is created. All MOs created should report
%%                       create and delete notifications. There can be
%%                       zero or more named attributes in the event attribute
%%                       list.
%%
%% MoDeleted:            An MO is deleted. The event attribute list must
%%                       be empty.
%%
%% AttributeValueChange: One or more attributes have been updated
%%                       in an existing MO.
%%                       Note that attributes that are marked with
%%                       'noNotification' in the MOM are not reported.
%%
%% Overflow:             One or more notifications have been lost
%%                       due to a flooding situation. The event
%%                       attribute list must be empty.
%%
%% --------------------------------------------------------------------------
-define(MoCreated,            1).
-define(MoDeleted,            2).
-define(AttributeValueChange, 3).
-define(Overflow,             4).

-type coi_event_1_type() :: ?MoCreated
                          | ?MoDeleted
                          | ?AttributeValueChange
                          | ?Overflow.

-type coi_notification_1() :: {coi_notification_1,
			       list(coi_notification_1_element())}.

-type coi_notif_transaction_id() :: integer().
-type coi_notification_1_element() :: {trans_id, coi_notif_transaction_id() } |
                                      {source, coi_source_indicator()} |
                                      {events, list(coi_event_1())}.

-type coi_event_1() :: {coi_event_1, list(coi_event_1_element())}.
-type coi_event_1_element() :: {dn, list(ecim_dn())} |
                               {event_type, coi_event_1_type()} |
                               {attributes, list(coi_named_attribute())}.

%% --------------------------------------------------------------------------
%% This type is similar to the coi_notification_1() above. Main difference is
%% that it contains all attributes that has changed value regardless of how the
%% attributes are specified in the MOM.
%%
%% For now, only the FmAlarm class is covered by this functinality.

-type coi_notification_0() :: {coi_notification_0, list(coi_event_0())}.

-define(COI_MoCreated,       moCreated).
-define(COI_MoDeleted,       moDeleted).
-define(COI_AttrValueChange, attrValueChange).

-type coi_event_0_type() :: ?COI_MoCreated |
                            ?COI_MoDeleted |
                            ?COI_AttrValueChange.

-type coi_event_0() :: {coi_event_0, list(coi_event_0_prop())}.

-type coi_event_0_prop() :: {dn, list(ecim_dn())} |
                            {event_type, coi_event_0_type()} |
                            {attributes, list(mo_attribute_name())}.
%% * For '{event_type, ?COI_MoCreated}' -> {attributes, Attrs}:
%%   Attrs contains all attribute names for that class.
%% * For '{event_type, ?COI_MoDeleted}' -> {attributes, Attrs}:
%%   Attrs contains an empty list.
%% * For '{event_type, ?COI_AttrValueChange}' -> {attributes, Attrs}:
%%   Attrs contains names of those attributes that has changed.


%% --------------------------------------------------------------------------
%% Availability log (AVLI) types
%% --------------------------------------------------------------------------
-type avli_return() :: ok | {error, illegal_param}.
-type now_ts() :: {MegSec::integer(), Sec::integer(), MicSec::integer()} |
		  integer() .
%%% Result of os:timestamp() or seconds since Jan 1 1970 00:00:00.
-type string_60() :: string().
%%% The maximum allowed length is 60 characters.
-type service_status() :: 'InService' | 'OutOfService' |
			  'PartiallyOutOfService' | undefined.
-type reason() :: 'ShutdownCommand' | 'UnOperational' | 'Starting' |
		  'Operational' | undefined.
-type event_id() :: integer().
-type cause() :: string().
-type appLog() :: {'Cause', cause()} |
                  {'InfoText', string()} |
                  'RankCold' |
                  'RankWarm' |
                  'RankColdWTest' |
                  'RestartCompleted' |
                  {'PiuInfo', string()} |
                  {'NodeInfo', string()}.
-type add_info() :: string() |
                    {'AppLog', appLog() | list(appLog())}.
%%% The maximum allowed length is 1500 characters.
-type piu_type() :: 'Bp' | 'Mp' | undefined.
-type piu_hw_addr() :: {SwitchModNo::integer(), SwitchPortNo::integer()} |
                       undefined.
-type prod_no() :: string().
%%% The maximum allowed length is 25 characters.
-type prod_rev() :: string().
%%% The maximum allowed length is 8 characters.
-type prod_name() :: string().
%%% The maximum allowed length is 13 characters.
-type hw_pid() :: {ProdNo::prod_no(), ProdRev::prod_rev(),
		   ProdName::prod_name()}.
-type sw_pid() :: {ProdNo::prod_no(), ProdRev::prod_rev()}.
-type hw_type() :: string_60().
-type hw_address() :: string_60().
-type service_type() :: string_60().
-type service_inst() :: string_60().
-type nodeIdentity() :: {'SiteLocation', string()} |
			{'NetworkManagedElementId', string()} |
			{'ProdNo', string()} |
			{'ProdRev', string()} |
			{'ProdName', string()}.
-type avail_info() :: string() |
                      {'AppLog', appLog() | list(appLog())}.
%%% The maximum allowed length is 15000 characters.


%% --------------------------------------------------------------------------
%% Security log types
%% --------------------------------------------------------------------------
-type sec_facility() :: integer().   % according to rfc5424
-type sec_severity() :: emergency |
                        alert |
		        critical |
		        error |
		        warning |
		        notice |
		        info.
-type sec_msg() :: string().
-type sec_ip() :: string().
-type sec_return() :: ok | tryAgain | invalidParam | noResources.


%% --------------------------------------------------------------------------
%% Miscellaneous
%% --------------------------------------------------------------------------
-type attribute_is() :: (AttributeIs :: boolean() |
			 {AttributeIs :: boolean(),
			  {Sublevel :: enum | struct,
			   sublevelMembers() | (SublevelIs :: boolean())}}).
-type sublevelMembers() :: list({MemberName :: string(),
				 MemberIs :: attribute_is()}).
