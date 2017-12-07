%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfEsi.erl %
%%% Author:	etxpejn
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(gmfEsi).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R10A/R11A/1').
-date('2017-10-02').
-author('etxpeno').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R2A/1      2014-05-14 etxpejn     Created
%%% R2A/3      2014-05-22 etxpejn     Correction in write_attr
%%% R2A/4      2014-06-10 etxpejn     Added dump of mnesia at ESI
%%% -----      ---------  --------    ------------------------
%%% R3A/1      2014-10-10 etxberb     Changed integer_to_list to
%%%                                   sysUtil:term_to_string.
%%% R3A/2      2015-01-28 etxpeno     Add init_board/1
%%% R3A/3      2015-02-13 etxpejn     Add role check in generate_esi
%%% R3A/4      2015-03-05 etxjotj     HT54172 Only register root dir for esi
%%% -----      ---------  --------    ------------------------
%%% R4A/1      2015-07-22 etxjotj     Clean disk
%%% R4A/2      2015-08-05 etxtory     clean_disk makes generate_esi to crash
%%% R4A/3      2015-08-13 etxtory     dialyze fix
%%% -----      ---------  --------    ------------------------
%%% R6A/1      2016-06-17 etxpeno     struct as attribute
%%% -----      ---------  --------    ------------------------
%%% R7A/1      2016-10-11 etxpeno     struct as attribute fix
%%% ----------------------------------------------------------
%%% R8A/1      2016-12-13 erarafo     adapting ESI for cloud
%%% ----------------------------------------------------------
%%% R11A/1     2017-10-02 etxpeno     improve the dump of the database to disk
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([generate_esi/0, generate_esi_post/0]).
-export([init_data/0, init_board/1]).
-export([clean_disk/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-export([dump_imm/0, dump_mnesia/0]).

-define(IMM_DB_DIR, filename:join([db_dir(), "imm"])).
-define(IMM_LOG, filename:join([?IMM_DB_DIR, "imm_esi.log"])).
-define(MNESIA_DB_DIR, filename:join([db_dir(), "mnesia"])).
-define(MNESIA_LOG, filename:join([?MNESIA_DB_DIR, "mnesia_esi.log"])).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% @doc ESI callbacks to applications, called from LOG
%%% ===Arguments===
%%% -
generate_esi() ->
    CoreState = clhI:core_state(),
    generate_esi(CoreState).

generate_esi(active) ->
    Funs = [{?MODULE, F, []} || F <- [dump_imm, dump_mnesia]],
    {ok, [ok, ok]} = sysUtil:parallel_call(Funs, []),
    ok;
generate_esi(_) ->
    ok.

%%% @doc ESI callbacks to applications, called from LOG;
%%% this function is called after the tarfile creation.
%%% ===Arguments===
%%% -
generate_esi_post() ->
    Vrcs = sysEnv:vrcs(),
    generate_esi_post(Vrcs).

generate_esi_post(true) ->
    %% error_logger:info_msg("delete file tree: ~s~n", [db_dir()]),
    os:cmd("rm -r " ++ db_dir());
generate_esi_post(false) ->
    ok.

init_data() ->
    %% Register for callbacks
    ok = logI:register_esi_cb(?MODULE),

    %% Register an participating directory in the ESI file
    ok = logI:register_esi_dir(db_dir()),
    sysServer:register_file_owner("db", gmfEsi).

init_board(active) ->
    ok = filelib:ensure_dir(?IMM_LOG),
    ok = filelib:ensure_dir(?MNESIA_LOG);
init_board(_CoreState) ->
    ok.

%%% @doc Clean away database copy
%%% @end

-spec clean_disk(Severity::minor|major) -> any().

clean_disk(_) ->
    rpc:multicall(os, cmd, [["rm -rf ", db_dir(), "/*"]]).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

dump_mnesia() ->
    ok = filelib:ensure_dir(?MNESIA_LOG),
    ok = mnesia:dump_to_textfile(?MNESIA_LOG).

dump_imm() ->
    ok = filelib:ensure_dir(?IMM_LOG),
    {ok, IoDevice} = file:open(?IMM_LOG, [write, delayed_write, raw]),

    {ok, OmHandle} = gmfImmOmI:initialize(),
    {ok, Handle} = gmfImmOmI:search_initialize_2(OmHandle, undefined),

    write_imm_objs(Handle, IoDevice),

    gmfImmOmI:finalize(OmHandle),

    close_imm_dump(IoDevice),

    ok.

close_imm_dump(IoDevice) ->
    case file:close(IoDevice) of
	ok ->
	    ok;
	{error, _Reason} ->
	    %% Try to close the Io device again
	    file:close(IoDevice)
    end.

write_imm_objs(Handle, IoDevice) ->
    %% the value 4 seems to give the fastest time when executing dump_imm()
    case gmfImmOmI:search_next_n_s2(Handle, 4) of
	{ok, Objs} ->
	    write_objs(IoDevice, Objs),
	    write_imm_objs(Handle, IoDevice);
	{error, Reason} ->
	    Reason
    end.

write_objs(IoDevice, Objs) ->
    lists:foreach(
      fun({ObjDn, Attrs}) ->
	      ok = file:write(IoDevice, ["ImmDn : ", ObjDn, "\n"]),
	      write_attr(IoDevice, lists:reverse(Attrs))
      end, Objs).

write_attr(IoDevice, []) ->
    ok = file:write(IoDevice, "\n");
write_attr(IoDevice, [{Attr, _Type, []} | Rest]) ->
    ok = file:write(IoDevice, [Attr, " : []\n"]),
    write_attr(IoDevice, Rest);
write_attr(IoDevice, [{Attr, _Type, [Value]} | Rest]) when is_binary(Value) ->
    ok = file:write(IoDevice, [Attr, " : ", Value, "\n"]),
    write_attr(IoDevice, Rest);
write_attr(IoDevice, [{Attr, _Type, [Value]} | Rest]) when is_integer(Value) ->
    ok = file:write(IoDevice, [Attr, " : ",
			       sysUtil:term_to_string(Value), "\n"]),
    write_attr(IoDevice, Rest);
write_attr(IoDevice, [{Attr, sa_imm_attr_csstructt, [Value]} | Rest])  ->
    StructValues = gmfSALib:struct_values(Value),
    ok = file:write(IoDevice, [Attr, " :\n"]),
    write_struct_members(IoDevice, StructValues),
    write_attr(IoDevice, Rest);


write_attr(IoDevice,
	   [{Attr, Type, [Value | RestValue]} | Rest]) when is_binary(Value) ->
    ok = file:write(IoDevice, [Attr, " : ", Value, "\n"]),
    write_attr(IoDevice, [{Attr, Type, RestValue}] ++ Rest);
write_attr(IoDevice,
	   [{Attr, sa_imm_attr_csstructt, [Value | RestValue]} | Rest])  ->
    StructValues = gmfSALib:struct_values(Value),
    ok = file:write(IoDevice, [Attr, " :\n"]),
    write_struct_members(IoDevice, StructValues),
    write_attr(IoDevice, [{Attr, sa_imm_attr_csstructt, RestValue}] ++ Rest);
write_attr(IoDevice, [{Attr, _Type, Values} | Rest])  ->
    ok = file:write(IoDevice, [Attr, " : ", lists:concat(Values), "\n"]),
    write_attr(IoDevice, Rest).

write_struct_members(IoDevice, StructValues) ->
    [write_struct_member(IoDevice, S) || S <- StructValues].

write_struct_member(IoDevice, {Attr, _Type, [Value]}) when is_binary(Value) ->
    ok = file:write(IoDevice, ["\t", Attr, " : ", Value, "\n"]);
write_struct_member(IoDevice, {Attr, _Type, [Value]}) when is_integer(Value) ->
    ok = file:write(IoDevice,
		    ["\t", Attr, " : ", sysUtil:term_to_string(Value), "\n"]);
write_struct_member(IoDevice, {Attr, _Type, Values}) ->
    ok = file:write(IoDevice, ["\t", Attr, " : ", lists:concat(Values), "\n"]).

db_dir() ->
    case sysEnv:vrcs() of
	true ->
	    %% note: the name "esi_tmp_db" is also hardcoded in logEsi
	    filename:join([sysEnv:rcs_root(), "tmp", "esi_tmp_db"]);
	false ->
	    filename:join([sysEnv:rcs_dir(), "db"])
    end.

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
