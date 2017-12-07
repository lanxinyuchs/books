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
%% @author Magnus Lidén <magnus.liden@ericsson.com>
%% Created : 24 Apr 2014 by Magnus Lidén
-module(comte_default_pm_handler).

%% Behaviours
-behaviour(comte_pm_api).


%%% Initialization
-export([start/1]).

%%% PM counter exports
-export([get_measurements/2,get_measurements/3]).
-export([get_measurement_names/1]).
-export([get_job_ids/1]).
-export([get_gp/1]).

-export([add_meas/2]).
-export([delete_measurement/1]).
-export([pm_version/0]).


%% @doc The callback is invoked during the
%%      startup of ComtE, if registered.
%%      The arguments are the ones optionally
%%      registered in the application environment.
%% @end
start(_Args) ->
    create_tables(),
    ok.

%% @private
get_measurements(DN, ListOfMeasNames) -> % For COM_PM_MEAS=1
    lookup_meas(DN, ListOfMeasNames, []).

%% @doc The callback is invoked by ComtE if an
%%      operator invokes 'show-counters' in the CLI.
%%      It should return the measurements of the DN
%%      given in the first parameter.
%%      The second parameter is a list of measurement
%%      names to return. An empty list is
%%      a request for all measurements. The return types
%%      are defined in {@link comte_pm_api}.
%% @end
-spec get_measurements(
	DN :: comte_types:ecim_dn(),
	MeasNames :: list(comte_pm_api:pm_meas_name()),
	Opts :: list(comte_pm_api:pm_get_meas_opt())) ->
			      list(comte_pm_api:pm_meas()) |
                              comte_types:com_error().
get_measurements(DN, ListOfMeasNames, Opts) ->
    lookup_meas(DN, ListOfMeasNames, Opts).


%% @doc The callback is invoked by ComtE if a
%%      COM operator invokes 'show-counters TAB'
%%      in the CLI. This is utilized for auto-completion
%%      and user help. It should return a list
%%      of measurement definitions according to
%%      {@link comte_pm_api}.
%% @end
-spec get_measurement_names(
	DN :: comte_types:ecim_dn()) ->
                                   list(comte_pm_api:pm_meas_def()) |
                                   comte_types:com_error().
get_measurement_names(DN) ->
    lookup_meas_names(DN).

%% @doc The callback is invoked by ComtE if a
%%      COM operator invokes 'show-counters -j TAB'
%%      in the CLI. This is utilized for auto-completion
%%      and user help. It should return a list
%%      of measurement job ids according to
%%      {@link comte_pm_api}.
%% @end
-spec get_job_ids(
	DN :: comte_types:ecim_dn()) ->
			 list(comte_pm_api:pm_job_id()) |
			 comte_types:com_error().
get_job_ids(DN) ->
    lookup_job_ids(DN).

get_gp(_PmGpId) -> {[], false}. %% empty interface only, implement default behavour here

%% ===================================
%% Internal
%% ===================================

pm_table() ->
    pm_meas.

create_tables() ->
    Tables = [pm_table()],
    Opts = [set, public, named_table],
    ok = comte_lib:tab_create(Tables, Opts).


%% @private
add_meas(DN, {_Name, _ValList, _Opts}=Meas)
    when is_binary(DN) ->
    add_meas(DN, [Meas]);
add_meas(DN, Meas)
    when is_binary(DN),
         is_list(Meas) ->
    comte_lib:tab_insert(pm_table(), DN, Meas).

lookup_meas(DN) when is_binary(DN) ->
    case comte_lib:tab_lookup(pm_table(), DN) of
        {error, _Reason} ->
            {error, not_exist,
             <<"There are no measurements for ",DN/binary>>};
        Meas ->
            Meas
    end.

lookup_meas(DN, Names, Opts) ->
    JobId = lists:keyfind(job_id, 1, Opts),
    case lookup_meas(DN) of
	{DN, Meas} ->
	    lists:filter(
	      fun (M) ->
		      (Names =:= []
		       orelse lists:member(element(1, M), Names))
			  andalso
			  (JobId =:= false
			   orelse lists:member(JobId, element(3, M)))
	      end, Meas);
        Other ->
            Other
    end.

lookup_meas_names(DN) ->
    case lookup_meas(DN) of
        {DN, Meas} ->
            extract_def(Meas, []);
        _ ->
            {error, not_exist}
    end.

lookup_job_ids(DN) ->
    case lookup_meas(DN) of
	{DN, Meas} ->
	    [JobId ||
		{job_id,JobId} <-
		    [lists:keyfind(job_id, 1, Opts) ||
			{_Name, _Vals, Opts} <- Meas]];
	_ ->
	    {error, not_exist}
    end.


extract_def([], Acc) ->
    lists:reverse(Acc);

extract_def([{Name, _Vals} | Rest], Acc)  ->
    extract_def(Rest, [Name | Acc]);
extract_def([{Name, _Vals, Opts} |Rest], Acc) ->
     case lists:keyfind(info, 1, Opts) of
         false ->
             extract_def(Rest, [Name|Acc]);
         InfoTuple ->
             extract_def(Rest, [{Name, [InfoTuple]} | Acc])
     end.

%% @private
delete_measurement(DN) when is_binary(DN) ->
    comte_lib:tab_delete_entry(pm_table(), DN).

%% @private
-ifdef(COM_PM_MEAS_H).
pm_version() ->
    case begin ?COM_PM_MEAS_H end of
	true -> 1;
    	V when is_integer(V) -> V
    end.
-else.
pm_version() ->
    0.
-endif.
