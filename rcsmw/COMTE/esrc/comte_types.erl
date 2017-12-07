%%%-------------------------------------------------------------------
%%% %CopyrightBegin%
%%%
%%% Copyright Ericsson AB %TemplateYear%. All Rights Reserved.
%%%
%%% The program may be used and/or copied only with the written
%%% permission from Ericsson AB, or in accordance with the terms
%%% and conditions stipulated in the agreement/contract under which
%%% the program has been supplied.
%%%
%%% %CopyrightEnd%
%%%-------------------------------------------------------------------
%%% @author Raimo Niskanen <raimo.niskanen@ericsson.com>
%%% @doc
%%%   Common type specifications
%%%
%%%   These types are used by for example implementation 
%%%   callback function.
%%%
%%%   Note that the type specifications for `com_sequence_*'
%%%   allows for any number of type tag integers at the head
%%%   of the sequence list, but that is just something lacking
%%%   in the type specification language.  In reality there
%%%   shall be exactly one type tag integer at the head of
%%%   the sequence list.
%%%
%%%   Such `[?TYPE|Vals]' sequence list allows for returning
%%%   a typed sequence of any length to COM, unlike the older
%%%   `[{?TYPE,Val}, ...]' sequence format that can not specify
%%%   a type for the empty sequence.
%%% @end

-module(comte_types).
-compile(export_all). % All functions are type predicates

%% This module implements predicates for type validation
%% of the types defined in the following header file, and this
%% module also exports the types so type users do not have to
%% include the header file.
-include("comte_types.hrl").
%% The type predicate' implementations have to be consistent
%% type declarations so be careful!

-export_type([transaction_id/0]).
-export_type([mo_name/0]).
-export_type([mo_attribute_name/0]).
-export_type([mo_instance/0]).

-export_type([ecim_dn/0]).
-export_type([ecim_path/0]).
-export_type([ecim_class_path/0]).
-export_type([user_object/0]).

-export_type([com_error/0]).
-export_type([error_code/0]).
-export_type([error_reason/0]).

-export_type([com_struct_member/0]).
-export_type([uint8/0, uint16/0, uint32/0, uint64/0]).
-export_type([int8/0, int16/0, int32/0, int64/0]).
-export_type([com_string/0, com_enum/0, com_struct/0, com_reference/0]).
-export_type([com_value_uint8/0, com_value_uint16/0]).
-export_type([com_value_uint32/0, com_value_uint64/0]).
-export_type([com_value_int8/0, com_value_int16/0]).
-export_type([com_value_int32/0, com_value_int64/0]).
-export_type([com_value_string/0, com_value_enum/0]).
-export_type([com_value_bool/0, com_value_struct/0]).
-export_type([com_value_reference/0]).
-export_type([com_value/0]).
-export_type([com_sequence_uint8/0, com_sequence_uint16/0]).
-export_type([com_sequence_uint32/0, com_sequence_uint64/0]).
-export_type([com_sequence_int8/0, com_sequence_int16/0]).
-export_type([com_sequence_int32/0, com_sequence_int64/0]).
-export_type([com_sequence_string/0, com_sequence_enum/0]).
-export_type([com_sequence_bool/0, com_sequence_struct/0]).
-export_type([com_sequence_reference/0]).
-export_type([com_sequence/0]).
-export_type([com_data/0]).

-export_type([com_named_attribute/0]).
-export_type([com_named_parameter/0]).
-export_type([listinstancename/0, listitemname/0, listdata/0]).
-export_type([comte_cb_key/0]).


%% General list predicate for a possibly empty homogenous list.
list(F, []) when is_function(F, 1) -> true;
list(F, [X|Xs]) when is_function(F, 1) ->
    F(X) andalso list(F, Xs);
list(F, _) when is_function(F, 1) -> false.

%% General list predicate for a nonempty homogenous list
nonempty_list(F, [X]) when is_function(F, 1) ->
    F(X);
nonempty_list(F, [X|Xs]) when is_function(F, 1) ->
    F(X) andalso nonempty_list(F, Xs);
nonempty_list(F, _) when is_function(F, 1) -> false.



%% Type exports of the types in comte_types.hrl
%% and their type predicates.

transaction_id(X) -> uint64(X).

mo_name(X) -> is_binary(X).

mo_attribute_name(X) -> is_binary(X).

mo_instance(?STRING(B)) -> is_binary(B);
mo_instance(_) -> false.

ecim_dn(X) -> is_binary(X).

ecim_path(X) -> list(fun erlang:is_binary/1, X).

ecim_class_path(X) -> is_binary(X).

user_object(_) -> true.


com_error({error, Error}) ->
    error_code(Error) orelse error_reason(Error);
com_error({error, Code, Reason}) ->
    error_code(Code) andalso
	(error_reason(Reason) orelse list(fun error_reason/1, Reason));
com_error(_) -> false.

error_code(Code) ->
    case Code of
	ok -> true;
	try_again -> true;
	not_active -> true;
	failure -> true;
	not_exist -> true;
	already_exist -> true;
	aborted -> true;
	object_locked -> true;
	prepare_failed -> true;
	commit_failed -> true;
	invalid_argument -> true;
	validation_failed -> true;
	no_resources -> true;
	timeout -> true;
	_ ->
	    false
    end.

error_reason(X) -> is_binary(X).

com_struct_member({Name, Val}) ->
    mo_attribute_name(Name) andalso com_data(Val).

uint8(X) ->
    is_integer(X) andalso 0 =< X andalso X =< 255.
uint16(X) ->
    is_integer(X) andalso 0 =< X andalso X =< 65535.
uint32(X) ->
    is_integer(X) andalso 0 =< X andalso X =< 4294967295.
uint64(X) ->
    is_integer(X) andalso 0 =< X andalso X =< 18446744073709551615.
int8(X) ->
    is_integer(X) andalso -128 =< X andalso X =< 127.
int16(X) ->
    is_integer(X) andalso -32768 =< X andalso X =< 32767.
int32(X) ->
    is_integer(X) andalso -2148483648 =< X andalso X =< 2147483647.
int64(X) ->
    is_integer(X) andalso
	-9223372036854775808 =< X andalso X =< 9223372036854775807.
com_string(X) -> is_binary(X).
com_enum(X) -> int16(X).
com_struct(X) -> nonempty_list(fun com_struct_member/1, X).
com_reference(X) -> is_binary(X).

com_value_uint8(?UINT8(X)) -> uint8(X);
com_value_uint8(_) -> false.
%%
com_value_uint16(?UINT16(X)) -> uint16(X);
com_value_uint16(_) -> false.
%%
com_value_uint32(?UINT32(X)) -> uint32(X);
com_value_uint32(_) -> false.
%%
com_value_uint64(?UINT64(X)) -> uint64(X);
com_value_uint64(_) -> false.
%%
com_value_int8(?INT8(X)) -> int8(X);
com_value_int8(_) -> false.
%%
com_value_int16(?INT16(X)) -> int16(X);
com_value_int16(_) -> false.
%%
com_value_int32(?INT32(X)) -> int32(X);
com_value_int32(_) -> false.
%%
com_value_int64(?INT64(X)) -> int64(X);
com_value_int64(_) -> false.
%%
com_value_string(?STRING(X)) -> com_string(X);
com_value_string(_) -> false.
%%
com_value_enum(?ENUM(X)) -> com_enum(X);
com_value_enum(_) -> false.
%%
com_value_bool(?BOOL(X)) -> is_boolean(X);
com_value_bool(_) -> false.
%%
com_value_struct(?STRUCT(X)) -> com_struct(X);
com_value_struct(_) -> false.
%%
com_value_reference(?REFERENCE(X)) -> com_reference(X);
com_value_reference(_) -> false.

com_value({Type, Val}) ->
    case Type of
	?UINT8 -> uint8(Val);
	?UINT16 -> uint16(Val);
	?UINT32 -> uint32(Val);
	?UINT64 -> uint64(Val);
	?INT8 -> int8(Val);
	?INT16 -> int16(Val);
	?INT32 -> int32(Val);
	?INT64 -> int64(Val);
	?STRING -> com_string(Val);
	?ENUM -> com_enum(Val);
	?BOOL -> is_boolean(Val);
	?STRUCT -> com_struct(Val);
	?REFERENCE -> com_reference(Val);
	_ -> false
    end;
com_value(_) -> false.

com_sequence_uint8(undefined) -> true;
com_sequence_uint8([?UINT8|Xs]) -> list(fun uint8/1, Xs);
com_sequence_uint8(X) -> list(fun com_value_uint8/1, X).
%%
com_sequence_uint16(undefined) -> true;
com_sequence_uint16([?UINT16|Xs]) -> list(fun uint16/1, Xs);
com_sequence_uint16(X) -> list(fun com_value_uint16/1, X).
%%
com_sequence_uint32(undefined) -> true;
com_sequence_uint32([?UINT32|Xs]) -> list(fun uint32/1, Xs);
com_sequence_uint32(X) -> list(fun com_value_uint32/1, X).
%%
com_sequence_uint64(undefined) -> true;
com_sequence_uint64([?UINT64|Xs]) -> list(fun uint64/1, Xs);
com_sequence_uint64(X) -> list(fun com_value_uint64/1, X).
%%
com_sequence_int8(undefined) -> true;
com_sequence_int8([?INT8|Xs]) -> list(fun int8/1, Xs);
com_sequence_int8(X) -> list(fun com_value_int8/1, X).
%%
com_sequence_int16(undefined) -> true;
com_sequence_int16([?INT16|Xs]) -> list(fun int16/1, Xs);
com_sequence_int16(X) -> list(fun com_value_int16/1, X).
%%
com_sequence_int32(undefined) -> true;
com_sequence_int32([?INT32|Xs]) -> list(fun int32/1, Xs);
com_sequence_int32(X) -> list(fun com_value_int32/1, X).
%%
com_sequence_int64(undefined) -> true;
com_sequence_int64([?INT64|Xs]) -> list(fun int64/1, Xs);
com_sequence_int64(X) -> list(fun com_value_int64/1, X).
%%
com_sequence_string(undefined) -> true;
com_sequence_string([?STRING|Xs]) -> list(fun com_string/1, Xs);
com_sequence_string(X) -> list(fun com_value_string/1, X).
%%
com_sequence_enum(undefined) -> true;
com_sequence_enum([?ENUM|Xs]) -> list(fun com_enum/1, Xs);
com_sequence_enum(X) -> list(fun com_value_enum/1, X).
%%
com_sequence_bool(undefined) -> true;
com_sequence_bool([?BOOL|Xs]) -> list(fun erlang:is_boolean/1, Xs);
com_sequence_bool(X) -> list(fun com_value_bool/1, X).
%%
com_sequence_struct(undefined) -> true;
com_sequence_struct([?STRUCT|Xs]) -> list(fun com_struct/1, Xs);
com_sequence_struct(X) -> list(fun com_value_struct/1, X).
%%
com_sequence_reference(undefined) -> true;
com_sequence_reference([?REFERENCE|Xs]) -> list(fun com_reference/1, Xs);
com_sequence_reference(X) -> list(fun com_value_reference/1, X).

%% This could be just an orelse over all sequences, but the following code
%% optimizes a bit by checking the tag fewer times.
com_sequence(undefined) -> true;
com_sequence([]) -> true;
com_sequence([{Type, Val} | Xs]) ->
    Fun =
	case Type of
	    ?UINT8 -> fun uint8/1;
	    ?UINT16 -> fun uint16/1;
	    ?UINT32 -> fun uint32/1;
	    ?UINT64 -> fun uint64/1;
	    ?INT8 -> fun int8/1;
	    ?INT16 -> fun int16/1;
	    ?INT32 -> fun int32/1;
	    ?INT64 -> fun int64/1;
	    ?STRING -> fun com_string/1;
	    ?ENUM -> fun com_enum/1;
	    ?BOOL -> fun erlang:is_boolean/1;
	    ?STRUCT -> fun com_struct/1;
	    ?REFERENCE -> fun com_reference/1;
	    _ -> undefined
	end,
    Fun =/= undefined andalso Fun(Val) andalso
	list(
	  fun ({T, V}) when T =:= Type -> Fun(V);
	      (_) -> false
	  end, Xs);
com_sequence([Type | Vals]) ->
    case Type of
	?UINT8 -> list(fun uint8/1, Vals);
	?UINT16 -> list(fun uint16/1, Vals);
	?UINT32 -> list(fun uint32/1, Vals);
	?UINT64 -> list(fun uint64/1, Vals);
	?INT8 -> list(fun int8/1, Vals);
	?INT16 -> list(fun int16/1, Vals);
	?INT32 -> list(fun int32/1, Vals);
	?INT64 -> list(fun int64/1, Vals);
	?STRING -> list(fun com_string/1, Vals);
	?ENUM -> list(fun com_enum/1, Vals);
	?BOOL -> list(fun erlang:is_boolean/1, Vals);
	?STRUCT -> list(fun com_struct/1, Vals);
	?REFERENCE -> list(fun com_reference/1, Vals);
	_ -> false
    end;
com_sequence(_) -> false.

com_data(X) ->
    com_value(X) orelse com_sequence(X).

com_named_attribute({Name, Val}) ->
    mo_attribute_name(Name) andalso com_data(Val);
com_named_attribute(_) -> false.

com_named_parameter({Name, Val}) ->
    is_binary(Name) andalso com_data(Val);
com_named_parameter(_) -> false.


listinstancename(X) -> is_binary(X).
listitemname(X) -> is_binary(X).
listdata(X) -> is_binary(X).


comte_cb_key(X) ->
    case X of
	access_mgmt -> true;
	replicated_list -> true;
	transaction_server -> true;
	crypto -> true;
	log_write -> true;
	cm_event_producer -> true;
	pm_event_producer -> true;
	alarm_producer -> true;
	oi_register -> true;
	pm -> true;
    pm_gp -> true;
	_ -> false
    end.
