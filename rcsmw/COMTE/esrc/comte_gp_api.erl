%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2014. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% @author Magnus Liden <magnus.liden@ericsson.com>
%% @doc
%%   Callback specification for a Performance Management
%%   measurement handler. Its only applicable when ComtE is built
%%   with COM 4.0 and above. For an example,
%%   see {@link comte_default_pm_handler}.
%% @end

%%
%% Revision History
%%
%% Date: 20150508
%% Name: uabhten, erasipe
%% Description: OTP-11851. 
%% From Erlang 18, predefined type tuple/N, N>0,  no longer exists
%% instead the userdefined types pm_meas_opt, pm_meas &  pm_meas_def  
%% must be defined as normal tuples {}.

-module(comte_gp_api).

%% Return value validation exports
-export([get_gp/1]).

-export_type([pm_gp_value/0,
              pm_gp_value_type/0,
              pm_gp_aggregated_value/0,
              pm_gp_instance/0,
              pm_gp_meas_obj_class/0,
              pm_gp_data/0
              ]).

-type pm_gp_value() :: number() | [number()] | [].

%% TODO: specify codes for value types from ComOamSpiPm_2.h 
-type pm_gp_value_type() :: 0..4.

-type pm_gp_aggregated_value() ::
          {MeasType :: binary(),
           Value :: pm_gp_value(),
           ValueType :: pm_gp_value_type(),
           IsSuspect :: boolean()}.

-type pm_gp_instance() ::
          {MeasObjLdn :: binary(),
           Values :: [pm_gp_aggregated_value()]}.

-type pm_gp_meas_obj_class() ::
          {MeasObjClass :: binary(),
           Instances :: [pm_gp_instance()]}.

-type pm_gp_data() ::
          {MeasObjClasses :: [pm_gp_meas_obj_class()],
           IsSuspect :: boolean()}.

%% Return value validation

get_gp(X) ->
    pm_gp_data(X) orelse comte_types:com_error(X).

pm_gp_data({MeasObjClasses, IsSuspect}) ->
    pm_gp_meas_obj_class_list(MeasObjClasses) andalso
    is_boolean(IsSuspect);
pm_gp_data(_) -> false.

valid_str(X) -> is_binary(X) orelse is_atom(X).

pm_gp_meas_obj_class({MeasObjClass, Instances}) ->
    valid_str(MeasObjClass) andalso
    pm_gp_instance_list(Instances);

pm_gp_meas_obj_class(_) -> false.

pm_gp_meas_obj_class_list(L) ->
    comte_types:list(fun pm_gp_meas_obj_class/1, L).

pm_gp_instance({MeasObjLdn, Values}) ->
    valid_str(MeasObjLdn) andalso
    pm_gp_aggregated_value_list(Values);

pm_gp_instance(_) -> false.

pm_gp_instance_list(L) ->
    comte_types:list(fun pm_gp_instance/1, L).

pm_gp_aggregated_value({MeasType, Value, ValueType, IsSuspect}) ->
    valid_str(MeasType) andalso
    pm_gp_value(Value) andalso
    pm_gp_value_type(ValueType) andalso
    is_boolean(IsSuspect).

pm_gp_aggregated_value_list(L) ->
    comte_types:list(fun pm_gp_aggregated_value/1, L).

pm_gp_value([]) -> true;

pm_gp_value(X) when erlang:is_list(X) ->
    comte_types:list(fun is_number/1, X);

pm_gp_value(X) when erlang:is_number(X) ->
    true;

pm_gp_value(_) -> false.

pm_gp_value_type(X) ->
    is_number(X) andalso
    X >= 0 andalso
    X =< 4.
