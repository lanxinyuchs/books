%%--------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2012. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%--------------------------------------------------------------------
%%
%% Revision History
%%
%% Date: 20150508
%% Name: uabhten, erasipe
%% Description: OTP-11851. 
%% From Erlang 18, predefined type tuple/N, N>0,  no longer exists
%% instead the userdefined types com_named_parameter & com_named_attribute 
%% must be defined as normal tuples {}.

%% In the module comte_types.erl there are type predicates
%% for all the types in this header file.  The implementation
%% and the specification will have to be consistent.  Beware!

%% COM Basic data types taken from ComOamSpiModelRepository_1.h

%%     ComOamSpiDatatype_INT8 = 1
-define(INT8, 1).
-define(INT8(Int),{?INT8, Int}).
%%     ComOamSpiDatatype_INT16 = 2
-define(INT16, 2).
-define(INT16(Int),{?INT16, Int}).
%%     ComOamSpiDatatype_INT32 = 3
-define(INT32, 3).
-define(INT32(Int),{?INT32, Int}).
%%     ComOamSpiDatatype_INT64 = 4
-define(INT64, 4).
-define(INT64(Int),{?INT64, Int}).
%%     ComOamSpiDatatype_UINT8 = 5
-define(UINT8, 5).
-define(UINT8(Int),{?UINT8, Int}).
%%     ComOamSpiDatatype_UINT16 = 6
-define(UINT16, 6).
-define(UINT16(Int),{?UINT16, Int}).
%%     ComOamSpiDatatype_UINT32 = 7
-define(UINT32, 7).
-define(UINT32(Int),{?UINT32, Int}).
%%     ComOamSpiDatatype_UINT64 = 8
-define(UINT64, 8).
-define(UINT64(Int),{?UINT64, Int}).
%%     ComOamSpiDatatype_STRING = 9
-define(STRING, 9).
-define(STRING(Str),{?STRING, Str}).
%%     ComOamSpiDatatype_BOOL = 10
-define(BOOL, 10).
-define(BOOL(Bool),{?BOOL, Bool}).
%%     ComOamSpiDatatype_REFERENCE = 11
-define(REFERENCE, 11).
-define(REFERENCE(Ref), {?REFERENCE, Ref}).
%%     ComOamSpiDatatype_ENUM = 12
-define(ENUM, 12).
-define(ENUM(Enum),{?ENUM, Enum}).
%%     ComOamSpiDatatype_STRUCT = 14
-define(STRUCT, 14).
-define(STRUCT(StructMembers),{?STRUCT,StructMembers}).
-define(STRUCT_MEMBER(Name,Value),{Name,Value}).

%%% Not implemented
%%     ComOamSpiDatatype_DERIVED = 13
%%     ComOamSpiDatatype_VOID = 15


%% A transaction identifier
-type transaction_id() :: uint64().

%% A MO name, i.e. `<<"ManagedElement">>' or `<<"Authorization">>'.
-type mo_name() :: binary().

%% The name of a MO attribute, i.e <<"siteLocation">>
-type mo_attribute_name() :: binary().

-type mo_instance() :: ?STRING(InstanceId :: binary()).

%% An Ericsson Common Information Model distingushed name represented
%% as a binary.
%% Example: The ManagedElement=1,SystemFunctions=1,SecM=1,Authorization=1,Local=1 DN
%% should be translated to:
%%   <<"ManagedElement=1,SystemFunctions=1,SecM=1,Authorization=1,Local=1">>.
-type ecim_dn() :: binary().

%% The ECIM (Ericsson Common Information Model) Path is a unique path
%% specifying a MO or an attribute within an MO. This is a reversed list
%% of string binaries representing the MO distinguished name in 3GPP format.
%%
%% Example: ManagedElement=1,TestMO=jimmy,test_string
%% becomes `[<<"test_string">>,<<"jimmy">>,<<"TestMO">>,<<"1">>,<<"ManagedElement">>]'.
-type ecim_path() :: [binary()].

%% The class path is a unique path specifying a class in the MOM
%% For example:
%% <<"/ManagedElement/SystemFunctions/SecM/UserManagement/LocalAuthorizationMethod/CustomRule">>
-type ecim_class_path() :: binary().

%% A user defined object for a root ECIM path
-type user_object() :: term().


-type com_error() ::
	{error, error_code()} |
	{error, error_reason()} |
	{error, error_code(), error_reason()} |
	{error, error_code(), [error_reason()]}.

-type error_code() ::
	ok |
	try_again |
	not_active |
	failure |
	not_exist |
	already_exist |
	aborted |
	object_locked |
	prepare_failed |
	commit_failed |
	invalid_argument |
	validation_failed |
	no_resources |
	timeout.

%% The failure reason for the operation which failed.
-type error_reason() :: binary().

%% A member of a COM struct.
%%
%% Example struct: If you want to return a struct looking like this:
%%   `{ id => "1", val => 8}',
%% you should construct the following type:
%%   `?STRUCT([?STRUCT_MEMBER(<<"id">>,?STRING(<<"1">>)),?STRUCT_MEMBER(<<"val">>,?UINT8(8))])'.
-type com_struct_member() ::
	{Name :: mo_attribute_name(), Value :: com_data()}.

-type uint8() :: 0..255.
-type uint16() :: 0..65535.
-type uint32() :: 0..4294967295.
-type uint64() :: 0..18446744073709551615.
-type int8() :: -128..127.
-type int16() :: -32768..32767.
-type int32() :: -2148483648..2147483647.
-type int64() :: -9223372036854775808..9223372036854775807.
-type com_string() :: binary().
-type com_enum() :: int16().
-type com_struct() :: nonempty_list(com_struct_member()).
-type com_reference() :: binary().

%% A COM value which is tagged using the macros defined in
%% comte/include/comte_types.hrl. The value in a com_value() term can be
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
-type com_value_uint8() :: ?UINT8(uint8()).
-type com_value_uint16() :: ?UINT16(uint16()).
-type com_value_uint32() :: ?UINT32(uint32()).
-type com_value_uint64() :: ?UINT64(uint64()).
-type com_value_int8() :: ?INT8(int8()).
-type com_value_int16() :: ?INT16(int16()).
-type com_value_int32() :: ?INT32(int32()).
-type com_value_int64() :: ?INT64(int64()).
-type com_value_string() :: ?STRING(com_string()).
-type com_value_enum() :: ?ENUM(com_enum()).
-type com_value_bool() :: ?BOOL(boolean()).
-type com_value_struct() :: ?STRUCT(com_struct()).
-type com_value_reference() :: ?REFERENCE(com_reference()).
-type com_value() ::
	com_value_uint8() |
	com_value_uint16() |
	com_value_uint32() |
	com_value_uint64() |
	com_value_int8() |
	com_value_int16() |
	com_value_int32() |
	com_value_int64() |
	com_value_string() |
	com_value_enum() |
	com_value_bool() |
	com_value_struct() |
	com_value_reference().

%%
%% Note that in most contexts 'undefined' means empty sequence and sometimes
%% libComtE translates that to an empty sequence of ?INT8 which seldom is
%% the right type but that has not mattered historically, however, type
%% checks in COM are gradually implemented so this will not last.  And in
%% other contexts (especially action()) 'undefined' means no value.
%%
%% The value [] means the other thing i.e mostly no value but for action()
%% an empty sequence which there becomes a NULL container hence typeless...
%%
%% So defining both 'undefined' and [] as part of this type is wrong for
%% one of them, but for which depends...
-type com_sequence_uint8() ::
	undefined | [com_value_uint8()] |
	nonempty_improper_list(?UINT8, [uint8()]).
-type com_sequence_uint16() ::
	undefined | [com_value_uint16()] |
	nonempty_improper_list(?UINT16, [uint16()]).
-type com_sequence_uint32() ::
	undefined | [com_value_uint32()] |
	nonempty_improper_list(?UINT32, [uint32()]).
-type com_sequence_uint64() ::
	undefined | [com_value_uint64()] |
	nonempty_improper_list(?UINT64, [uint64()]).
-type com_sequence_int8() ::
	undefined | [com_value_int8()] |
	nonempty_improper_list(?INT8, [int8()]).
-type com_sequence_int16() ::
	undefined | [com_value_int16()] |
	nonempty_improper_list(?INT16, [int16()]).
-type com_sequence_int32() ::
	undefined | [com_value_int32()] |
	nonempty_improper_list(?INT32, [int32()]).
-type com_sequence_int64() ::
	undefined | [com_value_int64()] |
	nonempty_improper_list(?INT64, [int64()]).
-type com_sequence_string() ::
	undefined | [com_value_string()] |
	nonempty_improper_list(?STRING, [com_string()]).
-type com_sequence_enum() ::
	undefined | [com_value_enum()] |
	nonempty_improper_list(?ENUM, [com_enum()]).
-type com_sequence_bool() ::
	undefined | [com_value_bool()]  |
	nonempty_improper_list(?BOOL, [boolean()]).
-type com_sequence_struct() ::
	undefined | [com_value_struct()] |
	nonempty_improper_list(?STRUCT, [com_struct()]).
-type com_sequence_reference() ::
	undefined | [com_value_reference()] |
	nonempty_improper_list(?REFERENCE, [com_reference()]).
-type com_sequence() ::
	com_sequence_uint8() |
	com_sequence_uint16() |
	com_sequence_uint32() |
	com_sequence_uint64() |
	com_sequence_int8() |
	com_sequence_int16() |
	com_sequence_int32() |
	com_sequence_string() |
	com_sequence_enum() |
	com_sequence_bool() |
	com_sequence_struct() |
	com_sequence_reference().

%% Very common combination
-type com_data() :: com_value() | com_sequence().

%% Named attribute which is used when sending events using comte:notify
-type com_named_attribute() :: { mo_attribute_name(), com_data() }.

%% Named parameter in actions
-type com_named_parameter() :: { binary(), com_data() }.


%% Replicated list types
-type listinstancename() :: binary().
-type listitemname() :: binary().
-type listdata() :: binary().


%% Available callback keys
-type comte_cb_key() :: access_mgmt |
                        replicated_list |
                        transaction_server |
                        crypto |
                        log_write |
                        cm_event_producer |
                        pm_event_producer |
                        alarm_producer |
                        oi_register |
                        pm |
                        pm_gp.

%% Use comte_types:comte_cb_key/1 in runtime instead to avoid
%% including this header file
-define(is_callback_key(Key),
        Key =:= access_mgmt ;
            Key =:= replicated_list ;
            Key =:= transaction_server ;
            Key =:= crypto ;
            Key =:= log_write ;
            Key =:= cm_event_producer ;
	        Key =:= pm_event_producer ;
            Key =:= alarm_producer ;
            Key =:= oi_register ;
            Key =:= pm;
	        Key =:= pm_gp).
