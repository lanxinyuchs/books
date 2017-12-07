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

-module(comte_pm_api).

%% Return value validation exports
-export([get_measurements/1, get_measurement_names/1, get_job_ids/1]).

-export_type([pm_meas_value/0,
              pm_meas_name/0,
	      pm_get_meas_opt/0,
              pm_meas/0,
              pm_meas_def/0,
	      pm_job_id/0
             ]).



-type pm_meas_value() :: comte_types:com_value_int64().
%% The measurement value tuple.

-type pm_meas_value_spec() ::
	pm_meas_value() |
	{pm_meas_value(), IsSuspect :: boolean()}.
%% A measurement value with suspicion flag.

-type pm_meas_name() :: binary().
%% The measurement name.

-type pm_meas_opt() ::
	{info, binary()} |
	{job_id, pm_job_id()} |
	{error_information, binary()} |
	{gp_in_seconds, comte_types:uint32()}.
%% The list of optionals for a measurement, such as
%% it's job id.

%% '{<<"MyMeasurement">>, [{?INT64, 234432}, {?INT64, -6565433}]}'
%% '{<<"MyMeasurement">>, [{?INT64, 234432}], [{info, <<"MeasurementDescription">>}]}'
-type pm_meas() ::
	{PmMeasName :: pm_meas_name(),
	 PmMeasValues :: [pm_meas_value_spec()]} |
	{PmMeasName :: pm_meas_name(),
	 PmMeasValues :: [pm_meas_value_spec()],
	 PmMeasOptionals :: [pm_meas_opt()]}.
%% The measurement return type.

-type pm_get_meas_opt() ::
	{verbose, boolean()} |
	{job_id, pm_job_id()}.

%% Options for get measurements.

-type pm_meas_def() ::
	pm_meas_name() |
	{pm_meas_name(), [{info, binary()}]}.

%% A measurement definition. It's just a measurement name
%% or a tuple combining the name and a list of optionals.

-type pm_job_id() :: binary().
%% The measurement job id


%% Will be called during startup of comte
-callback start(Args :: list()) ->
    ok.


%% This callback should return PM Measurement names
%% and values related to the Measured Object.
%% The list of measurement names is the set that
%% that is expected to be returned. The empty list
%% denotes the request for all measurements.
%% I.e placeholder for future improvements
%%
%% Return value:
%% A list of pm_meas().
%% The empty list denotes that there are no
%% measurements for this Measured Object.
%% An error tuple like:
%%    {error, not_exist},
%%    {error, not_exist, <<"My error description">>} or just
%%    {error, <<"Detailed error info">>} for failure.
%% denotes that the Measured Object does not exist.
%%
-callback get_measurements(
	    DN :: comte_types:ecim_dn(),
	    MeasNames :: [pm_meas_name()],
	    Opts :: [pm_get_meas_opt()]) ->
    [pm_meas()] | comte_types:com_error().


%% This callback should return a PM measurement definition
%% related to a Measured Object or an error tuple.
%% The definition is either just the measurement name
%% or a tuple with the name and a list of optionals.
%% An optional tuple is a measurement description.
%% Ex:
%% [ {<<"MyCounter">>, [{info, <<"This counter is something else">>}]}, ...]
%% [ <<"MyOtherCounter">>,  ...]
-callback get_measurement_names(DN :: comte_types:ecim_dn()) ->
    [pm_meas_def()] | comte_types:com_error().

%% This callback should return PM job ids
%% related to a Measured Object or an error tuple.
%% The definition is the job id name
%% Ex:
%% [ <<"MyJobId">>,  ...]
-callback get_job_ids(DN :: comte_types:ecim_dn()) ->
    [pm_job_id()] | comte_types:com_error().



%%% Return value validation

get_measurements(X) ->
    pm_meas_list(X) orelse comte_types:com_error(X).

pm_meas({Name, Values}) ->
    is_binary(Name) andalso pm_meas_value_spec_list(Values);
pm_meas({Name, Values, Opts}) ->
    is_binary(Name) andalso
	pm_meas_value_spec_list(Values) andalso
	pm_meas_opt_list(Opts);
pm_meas(_) -> false.
%%
pm_meas_list(L) -> comte_types:list(fun pm_meas/1, L).

pm_meas_value_spec(X) ->
    pm_meas_value(X) orelse
	case X of
	    {Val,Suspect} ->
		pm_meas_value(Val) andalso is_boolean(Suspect);
	    _ -> false
	end.
%%
pm_meas_value_spec_list(L) ->
    comte_types:list(fun pm_meas_value_spec/1, L).

pm_meas_value(X) -> comte_types:com_value_int64(X).

pm_meas_opt({info,Info}) -> is_binary(Info);
pm_meas_opt({job_id,JobId}) -> pm_job_id(JobId);
pm_meas_opt({error_information,Info}) -> is_binary(Info);
pm_meas_opt({gp_in_seconds,GP}) -> comte_types:uint32(GP);
pm_meas_opt(_) -> false.
%%
pm_meas_opt_list(L) -> comte_types:list(fun pm_meas_opt/1, L).



get_measurement_names(X) ->
    pm_meas_def_list(X) orelse comte_types:com_error(X).

pm_meas_def({Name, Opts}) ->
    is_binary(Name) andalso pm_meas_def_opt_list(Opts);
pm_meas_def(Name) -> is_binary(Name).
%%
pm_meas_def_list(L) -> comte_types:list(fun pm_meas_def/1, L).

pm_meas_def_opt({info,Info}) -> is_binary(Info);
pm_meas_def_opt(_) -> false.
%%
pm_meas_def_opt_list(L) -> comte_types:list(fun pm_meas_def_opt/1, L).



get_job_ids(X) ->
    pm_job_id_list(X) orelse comte_types:com_error(X).


pm_job_id(X) -> is_binary(X).
%%
pm_job_id_list(L) -> comte_types:list(fun pm_job_id/1, L).
