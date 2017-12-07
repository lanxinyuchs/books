%%% ----------------------------------------------------------
%% coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	saf_log_SUITE.erl %
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R5A/R6A/R8A/R11A/1

%%% @doc 
%%% == Basic test of CS SAF LOG ==
%%%  
%%% @end
%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R2A/1      2013-07-04 eolaand     Created
%%% R2A/3      2013-07-25 uabesvi     added more test cases
%%% R2A/4      2013-07-25 uabesvi     added esi
%%% R2A/11     2013-09-10 etxivri     Updates due to renamed saf-rpc hooks.
%%% R3A/3      2015-02-28 etxkols     Preparation for 2 labs
%%% R3A/4      2015-07-13 etxjovp     Add group definitions used by CS CI
%%% R3A/4      2015-07-13 etxjovp     Add group definitions used by CS CI
%%% R6A/2      2016-08-16 etxkols     GIT
%%% R11A/1     2017-10-12 eolaand     Replace export_all with export list
%%% ----------------------------------------------------------
%%% 
-module(saf_log_SUITE).

%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% -export([initialize_curr_vsn/1,
%% 	 awrite_app/1]).
%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([all/0, 
	 suite/0,
	 groups/0,
	 init_per_suite/1, 
	 end_per_suite/1, 
         init_per_testcase/2, 
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2]).

-export([initialize_curr_vsn/1,
	 initialize_no_cb/1,
	 initialize_cb/1,
	 finalize_invalid_handle/1,
	 open_async/1,
	 open_app/1,
	 open_system/1,
	 open_ntf/1,
	 open_alarm/1,
	 open_app_invalid_open_flags/1, 
	 open_app_invalid_fmt/1,
	 open_app_invalid_path/1,
	 open_app_invalid_handle/1,
	 open_app_open_flags_false/1,
	 open_sys_invalid_open_flags/1,
	 open_2_apps_2_stream/1,
	 open_1_app_2_stream/1,
	 open_max/1,   
	 open_max_2_apps/1,
	 sync_write_app/1,
	 awrite_app/1,
	 awrite_sys/1,
	 awrite_app_cb_no_ack/1,
	 awrite_app_rotate/1,
	 awrite_invalid_inv/1,
	 awrite_ala_invalid_head/1,
	 awrite_severity/1,
	 close_invalid/1,
	 no_close_seq_first/1,
	 no_close_seq_middle/1,
	 no_close_no_seq/1,
	 no_close_changed_fmt/1,
	 limit_get/1,
	 awrite_app_esi/1,
	 initialize_inc_rc/1,
	 initialize_inc_maj_vsn/1,
	 initialize_dec_maj_vsn/1,
	 initialize_inc_min_vsn/1,
	 initialize_invalid_cb/1,
	 cb_initialize/1,
	 change_filter_app/1,
	 change_filter_ala/1,
	 change_filter_app_no_change/1,
	 finalize_handle_not_found/1,
	 open_invalid_stream_name/1,
	 open_app_no_open_flags/1,
	 open_sys_ca_defined/1,
	 open_app_2nd_no_flag/1,
	 open_5_apps_1_stream/1,
	 open_5_apps_format/1,
	 open_5_apps_5_stream/1,
	 awrite_app_invalid_head/1,
	 awrite_ntf/1,
	 awrite_ala/1,
	 awrite_app_no_cb_undef/1,
	 awrite_app_cb_false/1,
	 awrite_app_cb_filter/1,
	 get_root/1,
	 trace/1,
	 print_tables/1,
	 loop_data/1,
	 unknown_msg/1]).

-export([awrite_x/1,
	 severity/0,
	 remote_trace/0,
	 local_trace/0]).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("safe/include/safe.hrl").
-include_lib("safe/include/safe_log.hrl").
-include_lib("saf_log_test.hrl").


%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(HELP_MOD, saf_log_test).

-define(INVALID_HANDLE, 123457).

-define(INV1, 123).
-define(INV2, 456).
-define(INV3, 789).


-define(OPEN_FLAGS_TRUE,  true).
-define(OPEN_FLAGS_FALSE, false).

-define(ACK_FLAGS_TRUE,  true).
-define(ACK_FLAGS_FALSE, false).

-define(APP_STREAM(Name), "safLgStr=" ++ Name ++ ",safApp=safLogService").

-define(NO_CREATE_ATTRS, undefined).
-define(NO_FORMAT,       undefined).
-define(NO_OPEN_FLAGS,   undefined).
-define(NO_ACK_FLAGS,    undefined).

-define(TCS_NON_CLOUD, [awrite_app_esi, 
			open_app_invalid_path]).

-define(VNF_LOG_BASE_DIRS, ["vnf", "rcs"]).
-define(LOG_BASE_DIRS, ["rcs"]).

%%=========================================================================
%% definitions stolen from log_esi_SUITE
%% to be able to fetch the ESI files
%%=========================================================================
%% -define(SFTP_HOST, "10.68.200.11").
%% -define(USER, "mauve").
%% -define(PSWD, "dilbert").
%% -define(SFTP_URL, "sftp://"++?USER++"@"++?SFTP_HOST).


%%--------------------------------------------------------------------
%% suite() -> Info
%% Info = [tuple()]
%% 
%%--------------------------------------------------------------------
suite() ->
    suite(?HELP_MOD:host()).

suite(?TESTNODE) ->
    [
     {timetrap,{seconds,120}},
     {ct_hooks, [{cth_conn_log,[]},
		 {rct_safe_log_rpc, [{safe_debug_level, 2}]},
		 {rct_rpc,     ?TESTNODE},
		 {rct_netconf, {nc1, html}},
		 {rct_logging, {oi_testapp, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}}
		]}
    ];
suite(_) ->
    [
     {timetrap,{seconds,30}}
    ].

%%--------------------------------------------------------------------
%% init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% 
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    init_per_suite(?HELP_MOD:host(), Config).

init_per_suite(?TESTNODE, Config) ->
    Config;
init_per_suite(_, Config) ->
    ips_log_root(ct:get_config(log_root)),
    ips_log_cfg(ct:get_config(log_cfg)),
    application:start(safs),
    %% sys:trace(whereis(rct_safe_log_rpc), true),
    Config.


ips_log_root(undefined) ->
    ok;
ips_log_root(Path) when is_list(Path) ->
    application:set_env(safs, log_root, Path).

ips_log_cfg(undefined) ->
    ok;
ips_log_cfg(Path) when is_list(Path) ->
    application:set_env(safs, log_cfg,  Path).


%%--------------------------------------------------------------------
%% end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% 
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% 
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% 
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% 
%%--------------------------------------------------------------------
%% init_per_testcase(TestCase, Config) when TestCase == awrite_app_esi orelse
%% 					 TestCase == open_app_invalid_path ->
%%     case os:getenv("SIM_OR_TARGET") of
%% 	"cloudish" -> {skip, "not working on cloudish"};
%% 	_Sim       -> Config
%%     end;
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% 
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% 
%%--------------------------------------------------------------------
groups() ->
    AllGroup=all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []}
    ].

%%--------------------------------------------------------------------
%% all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% 
%%--------------------------------------------------------------------
all() -> 
    all(host()) ++
    [
     initialize_curr_vsn,
     initialize_no_cb,
     %% initialize_cb,
     finalize_invalid_handle,
     open_async,
     open_app,
     open_system,
     open_ntf,
     open_alarm,
     open_app_invalid_open_flags, %% safe_log does not support bad flags
                                  %% fixed in the error code handling
     open_app_invalid_fmt,
     open_app_invalid_path,
     open_app_invalid_handle,
     open_app_open_flags_false,
     open_sys_invalid_open_flags,
     open_2_apps_2_stream,
     open_1_app_2_stream,
     %% open_max,     %% changed max val from 10 to 1000
     %% open_max_2_apps,
     sync_write_app,
     awrite_app,
     awrite_sys,
     awrite_app_cb_no_ack,
     awrite_app_rotate,
     awrite_invalid_inv,
     awrite_ala_invalid_head,
     awrite_severity,
     close_invalid,
     no_close_seq_first,
     no_close_seq_middle,
     no_close_no_seq,
     no_close_changed_fmt,
     limit_get
    ] -- excl_tcs().
%%       ++ 
%% 	all(host()).

all(?TESTNODE) ->
    %% esi is only running on testnode
    [awrite_app_esi];
%% git environment
all(_) ->
    [
     %% safe_log does not support version verification,
     %% it uses only the default version
     initialize_inc_rc,
     initialize_inc_maj_vsn,
     initialize_dec_maj_vsn,
     initialize_inc_min_vsn,
     %% somewhere the cb-record is changed on the way from SAFE to SAFS
     initialize_invalid_cb,
     %% only valid when testing safs_log Erlang interface
     cb_initialize,
     %% safe_log cannot change filter settings
     change_filter_app,
     change_filter_ala,
     change_filter_app_no_change,
     %% safe_log freaks out on these ones
     finalize_handle_not_found,
     open_invalid_stream_name,
     open_app_no_open_flags,
     open_sys_ca_defined,
%%     open_app_2nd_no_flag,
     open_5_apps_1_stream,
     open_5_apps_format,
     open_5_apps_5_stream,
     awrite_app_invalid_head,
     awrite_ntf,
     awrite_ala,
     awrite_app_no_cb_undef,
     awrite_app_cb_false,
     %% safe_log does not seem to send filter cb
     awrite_app_cb_filter,
     %% funktions only implemented in safs
     get_root,
     trace,
     print_tables,
     loop_data,
     unknown_msg
    ].


excl_tcs() ->
    case os:getenv("SIM_OR_TARGET") of
	"cloudish" -> ?TCS_NON_CLOUD;
	_          -> []
    end.
    


%%--------------------------------------------------------------------
%% TestCase() -> Info
%% Info = [tuple()]
%% 
%%--------------------------------------------------------------------

%%========================================================================
%% initialize_curr_vsn(Config) -> ok.
%% 
%% @doc 
%% initialize using current version
%% @end
%%========================================================================
initialize_curr_vsn(_Config) ->
    {ok, Handle, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    ok   = finalize(Handle),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% initialize_inc_rc(Config) -> ok.
%% 
%% @doc 
%% initialize using incremented release code
%%
%% Note: safe ignores the version and always uses CurrentVsn
%% @end
%%========================================================================
initialize_inc_rc(_Config) ->
    Error   = err_version(),
    Version = version(?CURRENT_VSN),
    {error, Error, Version} = initialize(callbacks(), 
					 version(?RC_PLUS_VSN)),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% initialize_inc_maj_vsn(Config) -> ok.
%% 
%% @doc 
%% initialize using incremented major version
%%
%% Note: safe ignores the version and always uses CurrentVsn
%% @end
%%========================================================================
initialize_inc_maj_vsn(_Config) ->
    Error   = err_version(),
    Version = version(?CURRENT_VSN),
    {error, Error, Version} = initialize(callbacks(), 
					 version(?MAJOR_PLUS_VSN)),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% initialize_dec_maj_vsn(Config) -> ok.
%% 
%% @doc 
%% initialize using decremented major version
%%
%% Note: safe ignores the version and always uses CurrentVsn
%% @end
%%========================================================================
initialize_dec_maj_vsn(_Config) ->
    Error   = err_version(),
    Version = version(?CURRENT_VSN),
    {error, Error, Version} = initialize(callbacks(), 
					 version(?MAJOR_MINUS_VSN)),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% initialize_inc_min_vsn(Config) -> ok.
%% 
%% @doc 
%% initialize using incremented minor version
%%
%% Note: safe ignores the version and always uses CurrentVsn
%% @end
%%========================================================================
initialize_inc_min_vsn(_Config) ->
    Version = version(?CURRENT_VSN),
    {ok, Handle, Version} = initialize(callbacks(), version(?MINOR_PLUS_VSN)),
    ok   = finalize(Handle),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% initialize_no_cb(Config) -> ok.
%% 
%% @doc 
%% initialize using no callbacks
%%
%% Note: safe ignores the version and always uses CurrentVsn
%% @end
%%========================================================================
initialize_no_cb(_Config) ->
    {ok, Handle, _} = initialize(undefined, version(?CURRENT_VSN)),
    ok   = finalize(Handle),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% initialize_cb(Config) -> ok.
%% 
%% @doc 
%% initialize using callback record
%% @end
%%========================================================================
initialize_cb(_Config) ->
    CallbacksF  = callbacks({false, false, false}),
    {ok, HF, _} = initialize(CallbacksF, version(?CURRENT_VSN)),
    ok   = finalize(HF),
    true = noof_app_streams(0),

    CallbacksT  = callbacks({true, true, true}),
    {ok, HT, _} = initialize(CallbacksT, version(?CURRENT_VSN)),
    ok   = finalize(HT),
    true = noof_app_streams(0),

    ok.

%%========================================================================
%% initialize_invalid_cb(Config) -> ok.
%% 
%% @doc 
%% initialize using invalid values in teh callback record
%% @end
%%========================================================================
initialize_invalid_cb(_Config) ->
    Error   = err_invalid_param(),
    Version = version(?CURRENT_VSN),
    CB1   = callbacks({illegal_val, true, true}),
    {error, Error, Version} = initialize(CB1, version(?CURRENT_VSN)),

    CB2 = callbacks({undefined, illegal_val, true}),
    {error, Error, Version} = initialize(CB2, version(?CURRENT_VSN)),

    CB3 = callbacks({{module, fnc}, undefined, illegal_val}),
    {error, Error, Version} = initialize(CB3, version(?CURRENT_VSN)),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% cb_initialize(Config) -> ok.
%% 
%% @doc 
%% Test callbacks_initialize function in safs.
%% Not valid testcase when using safe.
%% @end
%%========================================================================
cb_initialize(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    callbacks_initialize(H1),

    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% finalize_handle_not_found(Config) -> ok.
%% 
%% @doc 
%% Invoke finalize with a non existing handle
%% @end
%%========================================================================
finalize_handle_not_found(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {error, _}  = finalize(handle(H1, 444555666777)),
    ok = finalize(H1),
    ok.

%%========================================================================
%% finalize_invalid_handle(Config) -> ok.
%% 
%% @doc 
%% Invoke finalize with an invalid handle
%% @end
%%========================================================================
finalize_invalid_handle(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {error, _}  = finalize(handle(H1, inv_handle)),
    ok = finalize(H1),
    ok.

%%========================================================================
%% open_async(Config) -> ok.
%% 
%% @doc 
%% Try to invoke async open
%% @end
%%========================================================================
open_async(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    {error, _} = stream_open_async(H1, "gurka", ?OPEN_FLAGS_TRUE, ?INV1),

    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% open_app(Config) -> ok.
%% 
%% @doc 
%% Open application stream
%% @end
%%========================================================================
open_app(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1, "tomat", ?OPEN_FLAGS_TRUE),

    true = noof_app_streams(1),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),

    {ok, H2, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    {ok, _SH1} = stream_open_2(H2, "tomat", ?OPEN_FLAGS_TRUE),

    true = noof_app_streams(1),
    ok   = finalize(H2),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% open_system(Config) -> ok.
%% 
%% @doc 
%% Open system stream
%% @end
%%========================================================================
open_system(_Config) ->
    open_3(?LOG_SYSTEM).

%%========================================================================
%% open_ntf(Config) -> ok.
%% 
%% @doc 
%% Open notification stream
%% @end
%%========================================================================
open_ntf(_Config) ->
    open_3(?LOG_NOTIFY).

%%========================================================================
%% open_alarm(Config) -> ok.
%% 
%% @doc 
%% Open alarm stream
%% 
%% @end
%%========================================================================
open_alarm(_Config) ->
    open_3(?LOG_ALARM).


%%========================================================================
%% open_app_invalid_open_flags(Config) -> ok.
%% 
%% @doc 
%% Open stream with invalid open flags
%%
%% Note: safe_log does not support invalid flags but crashes,
%%       fix in err_bad_flag/0 and in saf_rpc_lib:slave_fun
%% @end
%%========================================================================
open_app_invalid_open_flags(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    Error = err_bad_flag(),
    {error, Error} = stream_open_2(H1, "sallat", inv_flag),

    true = noof_app_streams(0),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% open_app_invalid_fmt(Config) -> ok.
%% 
%% @doc 
%% Open stream using invalid format (alarm format instead of application format)
%% @end
%%========================================================================
open_app_invalid_fmt(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    Error = err_invalid_param(),
    {error, Error} = stream_open_2(H1,
				   "safLgStr=bensin",
				   {"bensin", ?LOG_ALARM_FORMAT},
				   ?OPEN_FLAGS_TRUE),

    true = noof_app_streams(0),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% open_app_invalid_path(Config) -> ok.
%% 
%% @doc 
%% Open stream defining an invalid path
%% @end
%%========================================================================
open_app_invalid_path(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    Error = err_invalid_param(),
    {error, Error} = stream_open_2(H1,
				   "safLgStr=diesel",
				   {"diesel", ?NO_FORMAT, {path, "/sune/eva"}}, 
				   ?OPEN_FLAGS_TRUE),

    true = noof_app_streams(0),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% open_app_open_flags_false(Config) -> ok.
%% 
%% @doc 
%% Open application stream with open flags set to false.
%% @end
%%========================================================================
open_app_open_flags_false(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    Error = err_invalid_param(),
    {error, Error} = stream_open_2(H1, "etanol", ?OPEN_FLAGS_FALSE),

    true = noof_app_streams(0),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% open_app_2nd_no_flag(Config) -> ok.
%% 
%% @doc 
%% Open same application stream but change open flags from set to not defined.
%% @end
%%========================================================================
open_app_2nd_no_flag(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    {ok, _} = stream_open_2(H1, "volvo", ?OPEN_FLAGS_TRUE),

    Error = err_invalid_param(),
    {error, Error} = stream_open_2(H1, "volvo", ?NO_OPEN_FLAGS),

    true = noof_app_streams(1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% open_sys_invalid_open_flags(Config) -> ok.
%% 
%% @doc 
%% Open system stream using open flags.
%% @end
%%========================================================================
open_sys_invalid_open_flags(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    Error = err_invalid_param(),
    {error, Error} = stream_open_2(H1,
				   ?LOG_SYSTEM, 
				   undefined, 
				   ?OPEN_FLAGS_TRUE),

    true = noof_app_streams(0),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% open_app_invalid_handle(Config) -> ok.
%% 
%% @doc 
%% Open application using an invalid handle.
%%
%% Note: safe_log does not support invalid handles but crashes,
%%       fix in err_bad_flag/0 and in saf_rpc_lib:slave_fun
%% @end
%%========================================================================
open_app_invalid_handle(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    Error = err_invalid_handle(),
    {error, Error} = stream_open_2(?INVALID_HANDLE, "trappa", ?OPEN_FLAGS_TRUE),

    true = noof_app_streams(0),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% open_invalid_stream_name(Config) -> ok.
%% 
%% @doc 
%% Open a stream with invalid stream name format
%% 
%% @end
%%========================================================================
open_invalid_stream_name(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    Error = err_invalid_param(),
    {error, Error} = stream_open_2(H1,
				   'safLgStr=hiss',     %% note: atom()
				   "hiss", 
				   ?OPEN_FLAGS_TRUE),
    
    true = noof_app_streams(0),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% (Config) -> ok.
%% 
%% @doc 
%% Open application stream without open flags
%% @end
%%========================================================================
open_app_no_open_flags(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    Error = err_invalid_param(),
    {error, Error} = stream_open_2(H1, "flyg", ?NO_OPEN_FLAGS),

    true = noof_app_streams(0),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% open_sys_ca_defined(Config) -> ok.
%% 
%% @doc 
%% Open system stream with defiend create attributes
%% @end
%%========================================================================
open_sys_ca_defined(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    Error = err_invalid_param(),
    {error, Error} = stream_open_2(H1,
				   ?LOG_SYSTEM,
				   "segelbat", 
				   ?NO_OPEN_FLAGS),

    true = noof_app_streams(0),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% open_5_apps_1_stream(Config) -> ok.
%% 
%% @doc 
%% 5 applications open the same stream using different open options
%% @end
%%========================================================================
open_5_apps_1_stream(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH1}  = stream_open_2(H1,
				"safLgStr=cykel",
				<<"cykel">>,
				?OPEN_FLAGS_TRUE),

    {ok, H2, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH2}  = stream_open_2(H2,
				"safLgStr=cykel",
				?NO_CREATE_ATTRS,
				?NO_OPEN_FLAGS),

    {ok, H3, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH3}  = stream_open_2(H3, "cykel", ?OPEN_FLAGS_TRUE),

    {ok, H4, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    Error = err_invalid_param(),
    {error, Error} = stream_open_2(H4, "cykel", ?NO_OPEN_FLAGS),

    {ok, H5, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {error, Error} = stream_open_2(H5,
				   "safLgStr=cykel",
				   ?NO_CREATE_ATTRS,
				   ?OPEN_FLAGS_TRUE),

    true = noof_app_streams(1),
    ok   = finalize(H1),
    true = noof_app_streams(1),
    ok   = finalize(H2),
    true = noof_app_streams(1),
    ok   = finalize(H3),
    true = noof_app_streams(0), %% H4 and H5 were not successful
    ok   = finalize(H4),
    true = noof_app_streams(0),
    ok   = finalize(H5),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% open_5_apps_format(Config) -> ok.
%% 
%% @doc 
%% Five applications open the same stream. 
%% The stream's format is set to differ to the default format.
%% @end
%%========================================================================
open_5_apps_format(_Config) ->
    Format = "@Ch:@Cn:@Cs @Cm/@Cd/@CY @Sv @Sl \"@Cb\"",
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH1} = stream_open_2(H1,
			       "safLgStr=gurka",
			       {"gurka", Format}, 
			       ?OPEN_FLAGS_TRUE),

    {ok, H2, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH2} = stream_open_2(H2,
			       "safLgStr=gurka",
				?NO_CREATE_ATTRS,
				?NO_OPEN_FLAGS),

    {ok, H3, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH3} = stream_open_2(H3,
			       "safLgStr=gurka",
			       {"gurka", Format}, 
			       ?OPEN_FLAGS_TRUE),

    {ok, H4, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    Error = err_exist(),
    {error, Error} = stream_open_2(H4,
				   "safLgStr=gurka",
				   {"gurka", undefined}, 
				   ?OPEN_FLAGS_TRUE),

    true = noof_app_streams(1),
    ok   = finalize(H1),
    true = noof_app_streams(1),
    ok   = finalize(H2),
    true = noof_app_streams(1),
    ok   = finalize(H3),
    true = noof_app_streams(0),
    ok   = finalize(H4),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% open_2_apps_2_stream(Config) -> ok.
%% 
%% @doc 
%% Two applications open an own unique stream each
%% @end
%%========================================================================
open_2_apps_2_stream(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH1}  = stream_open_2(H1, "sko", ?OPEN_FLAGS_TRUE),

    {ok, H2, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH2} = stream_open_2(H2, "sandal", ?OPEN_FLAGS_TRUE),

    true = noof_app_streams(2),
    ok   = finalize(H1),
    ok   = finalize(H2),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% open_1_app_2_stream(Config) -> ok.
%% 
%% @doc 
%% One application opens two different streams
%% 
%% @end
%%========================================================================
open_1_app_2_stream(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH1}  = stream_open_2(H1, "bmw", ?OPEN_FLAGS_TRUE),
    {ok, _SH2}  = stream_open_2(H1, "dkw", ?OPEN_FLAGS_TRUE),

    true = noof_app_streams(2),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% open_5_apps_5_stream(Config) -> ok.
%% 
%% @doc 
%% Open 5 streams from 5 applications
%% 
%% @end
%%========================================================================
open_5_apps_5_stream(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH1}  = stream_open_2(H1, "citroen", ?OPEN_FLAGS_TRUE),

    {ok, H2, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH2}  = stream_open_2(H2, "peugeot", ?OPEN_FLAGS_TRUE),

    {ok, H3, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH3}  = stream_open_2(H3,
				?LOG_SYSTEM,
				?NO_CREATE_ATTRS,
				?NO_OPEN_FLAGS),

    {ok, H4, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH4}  = stream_open_2(H4,
				?LOG_NOTIFY,
				?NO_CREATE_ATTRS,
				?NO_OPEN_FLAGS),

    {ok, H5, _} = initialize(callbacks(), version(?CURRENT_VSN)),
    {ok, _SH5}  = stream_open_2(H5,
				?LOG_ALARM,
				?NO_CREATE_ATTRS,
				?NO_OPEN_FLAGS),

    true = noof_app_streams(2),
    ok   = finalize(H1),
    true = noof_app_streams(1),
    ok   = finalize(H2),
    true = noof_app_streams(0),
    ok   = finalize(H3),
    true = noof_app_streams(0),
    ok   = finalize(H4),
    true = noof_app_streams(0),
    ok   = finalize(H5),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% open_max(Config) -> ok.
%% 
%% @doc 
%% Open maximum number of application streams from one application.
%% @end
%%========================================================================
open_max(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    Ten = ["max_mango",
	   "max_banan",
	   "max_ananas",
	   "max_apple",
	   "max_citron",
	   "max_apelsin",
	   "max_lime",
	   "max_paron",
	   "max_melon"],

    [{ok, _} = stream_open_2(H1, FileName, ?OPEN_FLAGS_TRUE) || FileName <- Ten],

    Error = err_no_resources(),
    {error, Error} = stream_open_2(H1, "hahahaa", ?OPEN_FLAGS_TRUE),

    ok   = finalize(H1),
    true = noof_app_streams(0),
    timer:sleep(3000),
    ok.

%%========================================================================
%% open_max_2_apps(Config) -> ok.
%% 
%% @doc 
%% Open maximum number of application streams from two applications.
%% @end
%%========================================================================
open_max_2_apps(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    One = ["chips", "notter", "ostbage", "pinnar", "oliver"],
    [{ok, _} = stream_open_2(H1, File, ?OPEN_FLAGS_TRUE) || File <- One],

    {ok, H2, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    Two = ["veteol", "rodvin", "vitvin", "vatten"],
    [{ok, _} = stream_open_2(H2, File, ?OPEN_FLAGS_TRUE) || File <- Two],

    Error = err_no_resources(),
    {error, Error} = stream_open_2(H1,
				   "safLgStr=voinevoine",
				   {"voinevoine", undefined}, 
				   ?OPEN_FLAGS_TRUE),

    ok   = finalize(H1),
    ok   = finalize(H2),
    true = noof_app_streams(0),
    timer:sleep(3000),
    ok.



%%========================================================================
%% sync_write_app(Config) -> ok.
%% 
%% @doc 
%% Invoke sync write.
%% @end
%%========================================================================
sync_write_app(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1, "diskborste", ?OPEN_FLAGS_TRUE),

    {error, _} = write_log(SH1, 5000, get_log_rec_gen("awrite_app_log")),   

    true = noof_app_streams(1),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% awrite_app(Config) -> ok.
%% 
%% @doc 
%% Write asyncroneously to an application stream.
%% @end
%%========================================================================
awrite_app(_Config) ->
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1, "sony", ?OPEN_FLAGS_TRUE),

    ok = write_log_async(SH1,
			 ?INV1,
			 ?ACK_FLAGS_TRUE,
			 get_log_rec_gen("awrite_app_log")),   
    
    ok = expected_cb({write_log, {?INV1, ok()}, undefined}),

    true = noof_app_streams(1),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% awrite_app_esi(Config) -> ok.
%% 
%% @doc 
%% Write asyncroneously to an application stream and
%% check that the entries are found in the files.
%% @end
%%========================================================================
awrite_app_esi(Config) ->

    clear_esi_dir(),
    StreamName = "htc",
    
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1,
			      ?APP_STREAM(StreamName), 
			      {StreamName, ?NO_FORMAT, {rot, 4}}, 
 			      ?OPEN_FLAGS_TRUE),

    Msgs = ["kalle anka",
%% 	    "aku ankka",
%% 	    "anders and",
%% 	    "andres önd",
%% 	    "donald duck",
%% 	    "donald bebek",
%% 	    "paperino",
%% 	    "anec donald",
 	    "pasko patak",
 	    "kaczor donald",
 	    "pato donald",
 	    "vulle vuojasj",
 	    "jaca racman",
	    "el pato donald",
	    "vacvac amca"
	   ],

    ok = aae(Msgs, ?INV1, SH1),

    true = noof_app_streams(1),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),

    rct_rpc:call(?TESTNODE, pmsDebug, stop_clear, [], 5000, noprint),
    ok = check_esi(Config, StreamName, Msgs).

aae([], _Invoke, _) ->
    ok;
aae([Msg | T], Invoke, SH) ->
    ct:log("writing ~p~n", [Msg]),
    ok = write_log_async(SH, Invoke, ?ACK_FLAGS_TRUE, get_log_rec_gen(Msg)),    
    ok = expected_cb({write_log, {Invoke, ok()}, undefined}),
    timer:sleep(1000), %% needed to get different names for the log files
    aae(T, Invoke + 1, SH).
   



%%========================================================================
%% awrite_sys(Config) -> ok.
%% 
%% @doc 
%% Write asyncroneously to a system stream.
%% @end
%%========================================================================
awrite_sys(_Config) ->
    awrite_3(generic, ?LOG_SYSTEM, "awrite_sys_log").

%%========================================================================
%% awrite_ntf(Config) -> ok.
%% 
%% @doc 
%% Write asyncroneously to a notify stream.
%% 
%% 
%% @end
%%========================================================================
awrite_ntf(_Config) ->
    awrite_3(ntf, ?LOG_NOTIFY, "awrite_ntf_log").

%%========================================================================
%% awrite_ala(Config) -> ok.
%% 
%% @doc 
%% Write asyncroneously to an alarm stream.
%% @end
%%========================================================================
awrite_ala(_Config) ->
    awrite_3(ntf, ?LOG_ALARM, "awrite_ala_log").


%%========================================================================
%% awrite_app_invalid_head(Config) -> ok.
%% 
%% @doc 
%% Write asyncroneously to an application stream
%% but using wrong log head.
%% @end
%%========================================================================
awrite_app_invalid_head(_Config) ->
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1, "apple", ?OPEN_FLAGS_TRUE), 
    Error = err_invalid_param(),
    {error, Error} = write_log_async(SH1,
			 ?INV1,
			 ?ACK_FLAGS_TRUE,
			 get_log_rec_ntf("awrite_app_log")),   

    true = noof_app_streams(1),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% (Config) -> ok.
%% 
%% @doc 
%% Write asyncroneously to an application stream
%% but using wrong log head.
%% @end
%%========================================================================
awrite_ala_invalid_head(_Config) ->
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1,
			      ?LOG_ALARM,
			      undefined,
			      ?OPEN_FLAGS_FALSE),
    
    Error = err_invalid_param(),
    {error, Error} = write_log_async(SH1,
				     ?INV1,
				     ?ACK_FLAGS_TRUE,
				     get_log_rec_gen("awrite_ala_log")),   
    
    true = noof_app_streams(0),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% awrite_app_rotate(Config) -> ok.
%% 
%% @doc 
%% Create a rotate stream. Check that it the log is rotated.
%% @end
%%========================================================================
awrite_app_rotate(_Config) ->
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1,
			      ?APP_STREAM("samsung"),
			      {"samsung", undefined, {rot, 3}}, 
			      ?OPEN_FLAGS_TRUE),

    Expected = {write_log, {?INV1, ok()}, undefined},
    rotate(25, SH1, ?INV1, ?ACK_FLAGS_TRUE, "awrite_app_log", Expected),

    ok = write_log_async(SH1,
			 ?INV1, 
			 ?ACK_FLAGS_TRUE,
			 get_log_rec_gen("awrite_app_log")),   

    ok = expected_cb({write_log, {?INV1, ok()}, undefined}),

    true = noof_app_streams(1),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% awrite_app_no_cb_undef(Config) -> ok.
%% 
%% @doc 
%% Callback record with undefined parameters.
%% @end
%%========================================================================
awrite_app_no_cb_undef(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1, "sony", ?OPEN_FLAGS_TRUE),


    ok = write_log_async(SH1,
			 ?INV1,
			 ?ACK_FLAGS_TRUE,
			 get_log_rec_gen("awrite_app_log")),   
    
    ok = expected_cb({write_log, timeout, undefined}),

    true = noof_app_streams(1),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% awrite_app_cb_false(Config) -> ok.
%% 
%% @doc 
%% Callback record with all callbacks defined as false.
%% @end
%%========================================================================
awrite_app_cb_false(_Config) ->
    {ok, H1, _} = initialize(callbacks({false, false, false}), 
			     version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1, "sony", ?OPEN_FLAGS_TRUE),

    ok = write_log_async(SH1,
			 ?INV1,
			 ?ACK_FLAGS_TRUE,
			 get_log_rec_gen("awrite_app_log")),   
    
    ok = expected_cb({write_log, timeout, undefined}),

    true = noof_app_streams(1),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% awrite_app_cb_no_ack(Config) -> ok.
%% 
%% @doc 
%% Callback record with defined callbacks but using no ack flag.
%% @end
%%========================================================================
awrite_app_cb_no_ack(_Config) ->
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1, "sony", ?OPEN_FLAGS_TRUE),


    ok = write_log_async(SH1,
			 ?INV1,
			 ?ACK_FLAGS_FALSE,
			 get_log_rec_gen("awrite_app_log")),   
    
    ok = expected_cb({write_log, timeout, undefined}),

    true = noof_app_streams(1),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% awrite_app_cb_filter(Config) -> ok.
%% 
%% @doc 
%% Check that filter cb is sent.
%% @end
%%========================================================================
awrite_app_cb_filter(_Config) ->
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1, "gurka", ?OPEN_FLAGS_TRUE),

    ok = write_log_async(SH1,
			 ?INV1,
			 ?ACK_FLAGS_TRUE,
			 get_log_rec_gen("awrite_app_log")), 

    ok = expected_cb({filter_set,
		      {H1, {safsLogSeverityFlags,
			    true,true,true,true,true,true,true}},
		      undefined}),

    true = noof_app_streams(1),
    timer:sleep(2000),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% awrite_invalid_inv(Config) -> ok.
%% 
%% @doc 
%% Write to application stream but using invalid invocation.
%% @end
%%========================================================================
awrite_invalid_inv(_Config) ->
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1, "gurka", ?OPEN_FLAGS_TRUE),

    {error, _} = write_log_async(SH1,
				 kalle,
				 ?ACK_FLAGS_TRUE,
				 get_log_rec_gen("awrite_app_log")),   
    
    true = noof_app_streams(1),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% awrite_severity(Config) -> ok.
%% 
%% @doc 
%% Write using all severities.
%% @end
%%========================================================================
awrite_severity(_Config) ->
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),
    
    {ok, SH1} = stream_open_2(H1, 
			      ?LOG_SYSTEM, 
			      ?NO_CREATE_ATTRS,
			      ?OPEN_FLAGS_FALSE),
    
    Sev = ?HELP_MOD:all_severities(),
    
    [write_log_async(SH1,
		     ?INV1,
		     ?ACK_FLAGS_TRUE,
		     get_log_rec_gen("awrite_sys_log", S)) || S <- Sev], 

    true = noof_app_streams(0),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

%%========================================================================
%% change_filter_app(Config) -> ok.
%% 
%% @doc 
%% Change the filter settings for an application stream.
%% Note: not valid for safe_log
%% @end
%%========================================================================
change_filter_app(_Config) ->
    NewFilters = filters(true, true, true, false, false, false, true),
    
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),
    
    {ok, SH1} = stream_open_2(H1, "gurka", ?OPEN_FLAGS_TRUE),
    
    ok = change_filter("safLgStr=gurka", NewFilters),

    ok = write_log_async(SH1,
			 ?INV1,
			 ?ACK_FLAGS_TRUE,
			 get_log_rec_gen("awrite_app_log ok")), 


    ok = expected_cb({write_log, {?INV1, ?SA_OK}, undefined}),

    ok = write_log_async(SH1,
			 ?INV1 + 1,
			 ?ACK_FLAGS_TRUE,
			 get_log_rec_gen("awrite_app_log filtered",
				       ?LOG_WARNING)), 
    
    ok = expected_cb({write_log, {?INV1 + 1, ?SA_OK}, undefined}),

    true = noof_app_streams(1),
    timer:sleep(2000),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% change_filter_ala(Config) -> ok.
%% 
%% @doc 
%% Change filter settings for an alarm stream.
%% Note: not valid for safe_log
%% @end
%%========================================================================
change_filter_ala(_Config) ->
    NewFilters = filters(true, true, true, false, false, false, true),

    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1,
			      ?LOG_ALARM,
			      ?NO_CREATE_ATTRS,
			      ?OPEN_FLAGS_FALSE),

    {error, ?SA_ERR_NOT_SUPPORTED} = change_filter(?LOG_ALARM, NewFilters),

    ok = write_log_async(SH1,
			 ?INV1,
			 ?ACK_FLAGS_TRUE,
			 get_log_rec_ntf("awrite_app_log ok")), 
    

    ok = expected_cb({write_log, {?INV1, ?SA_OK}, undefined}),
    
    ok = write_log_async(SH1,
			 ?INV1 + 1,
			 ?ACK_FLAGS_TRUE,
			 get_log_rec_ntf("awrite_app_log not filtered")), 
    
    ok = expected_cb({write_log, {?INV1 + 1, ?SA_OK}, undefined}),

    true = noof_app_streams(0),
    timer:sleep(2000),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% change_filter_app_no_change(Config) -> ok.
%% 
%% @doc 
%% Reset the same filter settings for an application stream.
%% Note: not valid for safe_log
%% @end
%%========================================================================
change_filter_app_no_change(_Config) ->
    NewFilters = filters(true, true, true, false, false, false, true),

    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1, "gurka", ?OPEN_FLAGS_TRUE),
    
    ok = change_filter("safLgStr=gurka", NewFilters),
    {error, ?SA_ERR_NO_OP} = change_filter("safLgStr=gurka", NewFilters),

    true = noof_app_streams(1),
    timer:sleep(2000),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% close_invalid(Config) -> ok.
%% 
%% @doc 
%% Invoke close with invalid handle.
%% 
%% @end
%%========================================================================
close_invalid(_Config) ->
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1, "gurka", ?OPEN_FLAGS_TRUE),

    ok = write_log_async(SH1,
			 ?INV1,
			 ?ACK_FLAGS_TRUE,
			 get_log_rec_gen("awrite_app_log")),   

    true = noof_app_streams(1),
    {error, _} = stream_close(kalle),
    ok = finalize(H1),
    true = noof_app_streams(0),
    ok.

     
%%      no_close_seq_middle,
%%      no_close_no_seq,

%%========================================================================
%% no_close_seq_first(Config) -> ok.
%% 
%% @doc 
%% Close a file without explicitely closing it, send only finalize.
%% Check that the loop data contains the correct sequence number
%% when reopening.
%% @end
%%========================================================================
no_close_seq_first(_Config) ->
    {ok, H, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),
    {ok, _SH}  = stream_open_2(H, "sweden", ?OPEN_FLAGS_TRUE),

    ok = no_close("finland", undefined, {0, 0, 1, 2, 3, 4, 5, 6}),
    ok = finalize(H).


%%========================================================================
%% no_close_seq_middle(Config) -> ok.
%% 
%% @doc 
%% Close a file without explicitely closing it, send only finalize.
%% Check that the loop data contains the correct sequence number
%% when reopening.
%% @end
%%========================================================================
no_close_seq_middle(_Config) ->
    {ok, H, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),
    {ok, _SH}  = stream_open_2(H, "norway", ?OPEN_FLAGS_TRUE),

    MiddleSeq = "@Ch:@Cn:@Cs @Cm/@Cd/@CY @Sv @Sl @Cr \"@Cb\"",
    ok = no_close("iceland", MiddleSeq, {0, 0, 1, 2, 3, 4, 5, 6}),
    ok = finalize(H).

%%========================================================================
%% no_close_no_seq(Config) -> ok.
%% 
%% @doc 
%% Close a file without explicitely closing it, send only finalize.
%% Check that the loop data contains the correct sequence number
%% when reopening.
%% @end
%%========================================================================
no_close_no_seq(_Config) ->
    {ok, H, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),
    {ok, _SH}  = stream_open_2(H, "denmark", ?OPEN_FLAGS_TRUE),

    NoSeq = "@Ch:@Cn:@Cs @Cm/@Cd/@CY @Sv @Sl \"@Cb\"",
    ok = no_close("greenland", NoSeq, {0, 0, 0, 0, 1, 2, 3, 4}),
    ok = finalize(H).
    



%%========================================================================
%% no_close_changed_fmt(Config) -> ok.
%% 
%% @doc 
%% Close a file without explicitely closing it, send only finalize.
%% Check that the loop data contains the correct sequence number
%% when reopening using different format description
%% @end
%%========================================================================
no_close_changed_fmt(_Config) ->
    {ok, H, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),
    {ok, _SH}  = stream_open_2(H, "england", ?OPEN_FLAGS_TRUE),

    Stream = "ireland",

    DefaultFmt = undefined,
    ChangedFmt = "@Ch:@Cn:@Cs @Cm/@Cd/@CY @Sv @Sl @Cr \"@Cb\"",

    {ok, H0, SH0} = no_close_open(Stream, DefaultFmt),
    no_close_write(SH0, Stream ++ ": this is not poetry 1"),
    no_close_write(SH0, Stream ++ ": neither is this a love story 2"),
    no_close_write(SH0, Stream ++ ": even if it may look like one 3"),
    ok = finalize(H0),


    ok = no_close("ireland", ChangedFmt, {0, 0, 1, 2, 3, 4, 5, 6}),
    ok = no_close("ireland", DefaultFmt, {0, 0, 1, 2, 3, 4, 5, 6}),
    ok = finalize(H).





no_close(Stream, Fmt, {_E1, E2, E3, E4, E5, E6, E7, E8}) ->
    {ok, H0, _SH} = no_close_open(Stream, Fmt),
    Seq = get_seq_no(Stream),
    ok  = finalize(H0),

    {ok, H1, SH1} = no_close_open(Stream, Fmt),
    Seq = get_seq_no(Stream) - E2,
    no_close_write(SH1, Stream ++ ": this is not poetry 1"),
    ok = finalize(H1),

    {ok, H2, SH2} = no_close_open(Stream, Fmt),
    Seq = get_seq_no(Stream) - E3,
    no_close_write(SH2, Stream ++ ": neither is this a love story 2"),
    ok = finalize(H2),

    {ok, H34, SH34} = no_close_open(Stream, Fmt),
    Seq = get_seq_no(Stream) - E4,
    no_close_write(SH34, Stream ++ ": even if it may look like one 3"),
    Seq = get_seq_no(Stream) - E5,
    no_close_write(SH34, Stream ++ ": this is not a technical description 4"),
    Seq = get_seq_no(Stream) - E6,
    no_close_write(SH34, Stream ++ ": also it is not a memoir 5"),
    Seq = get_seq_no(Stream) - E7,
    no_close_write(SH34, Stream ++ ": this is pure science fiction 6"),
    Seq = get_seq_no(Stream) - E8,
    ok = finalize(H34).
    
    

no_close_open(Stream, Fmt) ->
    {ok, H, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),
    {ok, SH}   = stream_open_2(H,
			       ?APP_STREAM(Stream), 
			       {Stream, Fmt, {rot, 3}}, 
			       ?OPEN_FLAGS_TRUE),
    {ok, H, SH}.

no_close_write(SH, String) ->
    timer:sleep(1300),
    ok = write_log_async(SH,
			 ?INV1,
			 ?ACK_FLAGS_TRUE,
			 get_log_rec_gen(String)).

    


%%========================================================================
%% limit_get(Config) -> ok.
%% 
%% @doc 
%% Invoke limit_get.
%% @end
%%========================================================================
limit_get(_Config) ->
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1, "gurka", ?OPEN_FLAGS_TRUE),
    
    {error, _} = limit_get_1(SH1, 1),

    true = noof_app_streams(1),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.


%%========================================================================
%% get_root(Config) -> ok.
%% 
%% @doc 
%% Only valid for safs.
%% @end
%%========================================================================
get_root(_Config) ->
    safs_log:get_root(),
    ok.

%%========================================================================
%% trace(Config) -> ok.
%% 
%% @doc 
%% Only valid for safs.
%% @end
%%========================================================================
trace(_Config) ->
    safs_log:trace_groups(),
    safs_log:trace_points_list(),
    ok.

%%========================================================================
%% print_tables(Config) -> ok.
%% 
%% @doc 
%% Only valid for safs.
%% @end
%%========================================================================
print_tables(_Config) ->
    safs_log_srv:tables(),
    safs_log_srv:tables(user),
    safs_log_srv:tables(kalle),
    ok.

%%========================================================================
%% loop_data(Config) -> ok.
%% 
%% @doc 
%% Only valid for safs.
%% @end
%%========================================================================
loop_data(_Config) ->
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    {ok, _} = stream_open_2(H1,
			    ?APP_STREAM("gurka"), 
			    {"gurka", undefined}, 
			    ?OPEN_FLAGS_TRUE),
    safs_log_srv:loop_data(srv),
    safs_log_srv:loop_data(all),
    safs_log_srv:stream_pids(),
    ok = safs_log:finalize(H1),
    ok.

%%========================================================================
%% unknown_msg(Config) -> ok.
%% 
%% @doc 
%% Only valid for safs.
%% @end
%%========================================================================
unknown_msg(_Config) ->
    safs_log_srv ! kalle,
    gen_server:cast(safs_log_srv, kalle),
    gen_server:call(safs_log_srv, kalle),
    ok.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------



%%========================================================================
%% initialize(CallBacks, Version)
%% 
%% Invoke initialize/2 in either cc (using rct_safe_log_rpc) or 
%% git (using safs_log) environment.
%%========================================================================
initialize(CB, Vsn) ->
    initialize(module(), CB, Vsn).

initialize(Module, CB, Vsn) ->
    Module:initialize(CB, Vsn).
    

callbacks_initialize(Handle) ->
    callbacks_initialize(module(), Handle).

callbacks_initialize(Module, Handle) ->
    Module:callbacks_initialize(Handle).
    

%%========================================================================
%% finalize(Handle)
%% 
%% Invoke finalize/1 in either cc (using rct_safe_log_rpc) or 
%% git (using safs_log) environment.
%%========================================================================
finalize(Handle) ->
    finalize(module(), Handle).

finalize(Module, Handle) ->
    Module:finalize(Handle).



%%========================================================================
%% stream_open_2(Handle, StreamName, OpenFlag)
%% stream_open_2(Handle, StreamName, Params, OpenFlag)
%% 
%% Invoke stream-open_2 in either cc (using rct_safe_log_rpc) or 
%% git (using safs_log) environment.
%%========================================================================
stream_open_2(Handle, StreamName, OpenFlag) ->
    stream_open_2(module(), 
		  Handle, 
		  ?APP_STREAM(StreamName), 
		  {StreamName, undefined}, 
		  OpenFlag).

stream_open_2(Handle, StreamName, Params, OpenFlag) ->
    stream_open_2(module(), Handle, StreamName, Params, OpenFlag).



stream_open_2(rct_safe_log_rpc = M, 
	      Handle, 
	      StreamName, 
	      Params, 
	      OpenFlag) ->
    M:stream_open_2(Handle,
		    StreamName,
		    open_flags(OpenFlag), 
		    5000,
		    log_safe(Params));
stream_open_2(safs_log = M, 
	      Handle, 
	      StreamName,
	      Params, 
	      OpenFlag) ->
    M:stream_open_2(Handle,
		    StreamName,
		    log_safs(Params), 
		    open_flags(OpenFlag), 
		    5000).



%%========================================================================
%% stream_open_async(Handle, StreamName, OpenFlag, Invocation)
%% stream_open_async(Handle, StreamName, Params, OpenFlag, Invocation)
%% 
%% Invoke stream-open_2 in either cc (using rct_safe_log_rpc) or 
%% git (using safs_log) environment.
%%========================================================================
stream_open_async(Handle, StreamName, OpenFlag, Invocation) ->
    stream_open_async(module(), Handle, StreamName, OpenFlag, Invocation).


stream_open_async(rct_safe_log_rpc = M, 
		  Handle, 
		  StreamName, 
		  OpenFlag,
		  Invocation) ->
    M:stream_open_async_2(Handle,
			  ?APP_STREAM(StreamName),
			  open_flags(OpenFlag), 
			  Invocation,
			  log_safe({StreamName, undefined, {path, ?APP_LOG_DIR}}));
stream_open_async(safs_log = M, 
		  Handle, 
		  StreamName, 
		  OpenFlag, 
		  Invocation) ->
    M:stream_open_async_2(Handle,
			  ?APP_STREAM(StreamName),
			  log_safs({StreamName, undefined, {path, ?APP_LOG_DIR}}), 
			  open_flags(OpenFlag), 
			  Invocation).


log_safe(undefined) ->
    undefined;
log_safe(Name) when is_list(Name); 
		    is_binary(Name) ->
    log_safe(Name, undefined, ?APP_LOG_DIR, 0);
log_safe({Name, Fmt}) ->
    log_safe(Name, Fmt, ?APP_LOG_DIR, 0);
log_safe({Name, Fmt, {path, Path}}) ->
    log_safe(Name, Fmt, Path, 0);
log_safe({Name, Fmt, {rot, Rot}}) ->
    log_safe(Name, Fmt, ?APP_LOG_DIR, Rot).

log_safe(Name, Fmt, Path, Rot) ->
    Action = case Rot of
		 0 -> ?SAFE_LOG_FILE_FULL_ACTION_HALT;
		 _ -> ?SAFE_LOG_FILE_FULL_ACTION_ROTATE
	     end,
    #safe_log_file_create_attributes_2{log_file_name        = Name,
				       log_file_path_name   = Path,
				       max_log_file_size    = 500,
				       max_log_record_size  = 100,
				       ha_property          = false,
				       log_file_full_action = Action,
				       max_files_rotated    = Rot,
				       log_file_fmt         = Fmt}.


log_safs(undefined) ->
    undefined;
log_safs(Name) when is_list(Name);
		    is_binary(Name) ->
    log_safs(Name, undefined, ?APP_LOG_DIR, undefined);
log_safs({Name, Fmt}) ->
    log_safs(Name, Fmt, ?APP_LOG_DIR, undefined);
log_safs({Name, Fmt, {path, Path}}) ->
    log_safs(Name, Fmt, Path, undefined);
log_safs({Name, Fmt, {rot, Rot}}) ->
    log_safs(Name, Fmt, ?APP_LOG_DIR, Rot).

log_safs(Name, Fmt, Path, Rot) ->
    Action = case Rot of
		 undefined -> ?LOG_HALT;
		 _         -> ?LOG_ROTATE
	     end,
    #safsLogFileCreateAttributes_2{logFileName        = Name,
				   logFilePathName    = Path,
				   maxLogFileSize     = 500,
				   maxLogRecordSize   = 100,
				   haProperty         = false,
				   logFileFullAction  = Action,
				   maxFilesRotated    = Rot,
				   logFileFmt         = Fmt}.


%%========================================================================
%% write_log(StreamHandle, Invocation, LogRec)
%% 
%% Invoke write_log/3 in either cc (using rct_safe_log_rpc) or 
%% git (using safs_log) environment.
%%========================================================================
write_log(SH, Invokation, LogRec) ->
    write_log(module(), SH, Invokation, LogRec).

write_log(Module, SH, Invokation, LogRec) ->
    Module:write_log(SH, Invokation, LogRec).


%%========================================================================
%% write_log_async(StreamHandle, Invocation, AckFlag, LogRec)
%% 
%% Invoke write_log_async/4 in either cc (using rct_safe_log_rpc) or 
%% git (using safs_log) environment.
%%========================================================================
write_log_async(SH, Invokation, AckFlag, LogRec) ->
    write_log_async(module(), SH, Invokation, AckFlag, LogRec).

write_log_async(Module, SH, Invokation, AckFlag, LogRec) ->
    Module:write_log_async(SH, Invokation, ack_flags(AckFlag), LogRec).



%%========================================================================
%% stream_close(StreamHandle)
%% 
%% Invoke stream_close/1 in either cc (using rct_safe_log_rpc) or 
%% git (using safs_log) environment.
%%========================================================================
stream_close(SH) ->
    stream_close(module(), SH).

stream_close(Module, SH) ->
    Module:stream_close(SH).



%%========================================================================
%% get_seq_no(Stream)
%% 
%% Invoke safs_log_srv:get_seq_no/1 in either cc (using rct_safe_log_rpc) or 
%% git (using safs_log) environment.
%%========================================================================
get_seq_no(Stream) ->
    timer:sleep(1000),
    R = get_seq_no(host(), Stream),
    ct:pal("Got seq no: ~p~n", [R]),
    R.

get_seq_no(?TESTNODE = Node, Stream) ->
    rct_rpc:call(Node, safs_log_srv, get_seq_no, [Stream], 20000, noprint);
get_seq_no(_Node, Stream) ->
    safs_log_srv:get_seq_no(Stream).



%%========================================================================
%% 
%% 
%% 
%% 
%%========================================================================
get_log_rec_gen(Msg) ->
    ?HELP_MOD:get_log_rec_gen(Msg).

get_log_rec_gen(Msg, Severity) ->
    ?HELP_MOD:get_log_rec_gen(Msg, Severity).


get_log_rec_ntf(Msg) ->
    ?HELP_MOD:get_log_rec_ntf(Msg).




%%========================================================================
%% open_3(StreamName) -> ok
%% 
%% Open one of the 3 well-known streams
%%========================================================================
open_3(Name) ->
    %%======================================================
    %% close and finalize
    %%======================================================
    {ok, H1, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1, Name, ?NO_CREATE_ATTRS, ?OPEN_FLAGS_FALSE),

    true = noof_app_streams(0),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),

    %%======================================================
    %% finalize (close is skipped)
    %%======================================================
    {ok, H2, _} = initialize(callbacks(), version(?CURRENT_VSN)),

    {ok, _SH2} = stream_open_2(H2, Name, ?NO_CREATE_ATTRS, ?OPEN_FLAGS_FALSE),

    true = noof_app_streams(0),
    ok   = finalize(H2),
    true = noof_app_streams(0),

    ok.


%%========================================================================
%% 
%% 
%% 
%% 
%%========================================================================
awrite_3(Head, StreamName, Msg) ->
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1,
			      StreamName,
			      ?NO_CREATE_ATTRS, 
			      ?OPEN_FLAGS_FALSE),

    Log = case Head of
	      generic -> get_log_rec_gen(Msg);
	      _       -> get_log_rec_ntf(Msg)
	  end,

    ok = write_log_async(SH1,
			 ?INV1,
			 ?ACK_FLAGS_TRUE,
			 Log),   
    
    ok = expected_cb({write_log, {?INV1, ok()}, undefined}),

    true = noof_app_streams(0),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.

awrite_x(_Config) ->
    {ok, H1, _} = initialize(get_cb_fun(), version(?CURRENT_VSN)),

    {ok, SH1} = stream_open_2(H1,
			      ?LOG_ALARM,
			      ?NO_CREATE_ATTRS, 
			      ?OPEN_FLAGS_FALSE),


    ok = write_log_async(SH1,
			 ?INV1,
			 ?ACK_FLAGS_TRUE,
			 get_log_rec_ntf("awrite_x_log")),   
    
    ok = expected_cb({write_log, {?INV1, ok()}, undefined}),

    true = noof_app_streams(1),
    ok   = stream_close(SH1),
    ok   = finalize(H1),
    true = noof_app_streams(0),
    ok.



%%========================================================================
%% miscellaneous get functions
%%========================================================================

%%-----------------------------------------------------
%% get host name
%%-----------------------------------------------------
host()       -> ?HELP_MOD:host().
module()     -> ?HELP_MOD:module().
severity()   -> ?HELP_MOD:severity().
version(Vsn) -> ?HELP_MOD:version(Vsn).


%%-----------------------------------------------------
%% get filters
%%-----------------------------------------------------
filters(Eme, Ale, Cri, Err, War, Not, Info) ->
    #safsLogSeverityFlags{saLogSevFlagEmergency = Eme,
			  saLogSevFlagAlert     = Ale,    
			  saLogSevFlagCritical  = Cri, 
			  saLogSevFlagError     = Err,    
			  saLogSevFlagWarning   = War,  
			  saLogSevFlagNotice    = Not,   
			  saLogSevFlagInfo      = Info
			 }.


%%-----------------------------------------------------
%% get an invalid handle
%%-----------------------------------------------------
handle(Handle, Int) -> handle(host(), Handle, Int).

handle(?TESTNODE, {safe_log_handle, Port, _}, Int) -> 
    {safe_log_handle, Port, Int};
handle(_, _, Int) -> 
    Int.


%%========================================================================
%% 
%% 
%% 
%% 
%%========================================================================
rotate(N, _SH, _Inv, _AckFlags, _Msg, _Expected) when N < 1 ->
    ok;
rotate(N, SH, Inv, AckFlags, Msg, {ExpMsg, {_, ExpRes}, ExpPid} = Exp) ->
    Msg2 = Msg ++ "_" ++ integer_to_list(Inv),
    ok = write_log_async(SH,
			 Inv,
			 AckFlags,
			 get_log_rec_gen(Msg2)),   
    ok = expected_cb({ExpMsg, {Inv, ExpRes}, ExpPid}),
    rotate(N-1, SH, Inv+1, AckFlags, Msg, Exp).




change_filter(StreamName, Filters) -> cf(host(), StreamName, Filters).

cf(?TESTNODE, _, _) -> 
    ok;
cf(_, StreamName, Filters) -> 
    M = module(),
    M:change_filter(StreamName, Filters).



limit_get_1(SH, LimitId) -> limit_get_1(module(), SH, LimitId).

limit_get_1(Module, SH, LimitId) -> 
    Module:limit_get(SH, LimitId).



%%========================================================================
%% callbacks()                            -> undefined | #safsLogCallbacks{}
%% callbacks({FilterCb, OpenCb, WriteCb}) -> undefined | #safsLogCallbacks{}
%% 
%% Get callback record
%%
%% Note: rct_safe_log_rpc does not support callback records therfore
%%       all TESTNODE cases return undefined
%%========================================================================
callbacks() -> 
    callbacks_0(host()).

callbacks_0(?TESTNODE) -> 
    #safe_log_callbacks{}; 
callbacks_0(_) -> 
    #safsLogCallbacks{}.


callbacks(Values) -> 
    callbacks_1(host(), Values).

callbacks_1(?TESTNODE, {F, O, W}) -> 
    #safe_log_callbacks{filter_set_callback  = F,
			stream_open_callback = O,
			write_log_callback   = W
		       };
callbacks_1(_, {F, O, W}) -> 
    #safsLogCallbacks{saLogFilterSetCallback  = F,
		      saLogStreamOpenCallback = O,
		      saLogWriteLogCallback   = W
		     }.


%%========================================================================
%% get_cb_fun() -> function()
%% 
%% Get a function used as callback fun in initialize/2
%%========================================================================
get_cb_fun() ->
    Self = self(),
    fun(Data) -> 
	    Self ! {log_cb, Data}, 
	    ok
    end.




%%========================================================================
%% get open and ack flags
%%========================================================================
%%-----------------------------------------------------
%% open flags
%%-----------------------------------------------------
open_flags(false) -> open_flags_f(host());
open_flags(true)  -> open_flags_t(host());
open_flags(Error) -> Error.

open_flags_f(?TESTNODE) -> 0;
open_flags_f(_)         -> undefined.

open_flags_t(?TESTNODE) -> ?SAFE_LOG_STREAM_CREATE;
open_flags_t(_)         -> ?LOG_OPEN_FLAGS.

%%-----------------------------------------------------
%% ack flags
%%-----------------------------------------------------
ack_flags(false) -> ack_flags_f(host());
ack_flags(true)  -> ack_flags_t(host());
ack_flags(Error) -> Error.

ack_flags_f(?TESTNODE) -> 0;
ack_flags_f(_)         -> undefined.

ack_flags_t(?TESTNODE) -> ?SAFE_LOG_RECORD_WRITE_ACK;
ack_flags_t(_)         -> ?LOG_ACK_FLAGS.


%%========================================================================
%% check number of streams
%%========================================================================
noof_app_streams(X) -> ?HELP_MOD:noof_app_streams(X).

%%========================================================================
%% ok and error results
%%========================================================================
ok() -> ?HELP_MOD:ok().

err_bad_flag()       -> ?HELP_MOD:err_bad_flag().
err_exist()          -> ?HELP_MOD:err_exist().
err_invalid_handle() -> ?HELP_MOD:err_invalid_handle().
err_invalid_param()  -> ?HELP_MOD:err_invalid_param().
err_no_resources()   -> ?HELP_MOD:err_no_resources().
err_version()        -> ?HELP_MOD:err_version().



%%========================================================================
%% check the expected callbacks are received
%%========================================================================
expected_cb(Exp) ->
    case host() of
	?TESTNODE -> expected_cb_cc(Exp);
	_         -> expected_cb_git(Exp)
    end.


expected_cb_cc({Msg, timeout, Pid}) ->
    expected_cb_cc({Msg, {0, timeout}, Pid});
expected_cb_cc({Msg, {Inv, Res}, _Pid} = Exp) ->
    receive 
	{log_cb, 
	 {safe_log_write_log_callback, Inv, Res}} when Msg == write_log ->
	    ok;
	_Other ->
	    ct:pal("Got unexpected msg: ~p", [_Other]),
	    expected_cb_cc(Exp)
    after 3000 ->
	    case Res of
		timeout ->
		    ok;
		_ ->
		    ct:fail("### CB cc Timeout ~p~n", [Exp])
	    end
    end.

expected_cb_git({Msg, Res, Pid}) ->
    receive 
	{log_cb, {Msg, Res, Pid}} ->
	    ok;
	_Other ->
	    ct:pal("Got unexpected msg: ~p", [_Other]),
	    expected_cb_git({Msg, Res, Pid})
    after 3000 ->
	    case Res of
		timeout ->
		    ok;
		_ ->
		    ct:fail("### CB git Timeout ~p~n", [{Msg, Res, Pid}])
	    end
    end.





%%========================================================================
%% check_esi(TestCaseConfig, StreamName, Msgs) -> ok | {error, Reason}
%% 
%% Use esi to check that the logs contain the expected entries.
%%========================================================================
check_esi(Config, StreamName, Msgs) when StreamName == ?LOG_SYSTEM;
					 StreamName == ?LOG_ALARM;
					 StreamName == ?LOG_NOTIFY ->
    check_esi(Config, StreamName, Msgs, ?SAF_LOG_DIR);
check_esi(Config, StreamName, Msgs) ->
    check_esi(Config, StreamName, Msgs, ?SAF_LOG_DIR ++ "/" ++ ?APP_LOG_DIR).

check_esi(Config, StreamName, Msgs, LogDir) ->
    PrivDir = ?config(priv_dir, Config),
    check_esi(host(), PrivDir, StreamName, Msgs, LogDir).

%%======================================================
%% only valid on TESTNODE 
%%======================================================
check_esi(Node, _, _, _, _) when Node /= ?TESTNODE ->
    ok;
check_esi(?TESTNODE, PrivDir, StreamName, Msgs, LogDir) ->
    {EsiLogPath, EsiLogName} = export_esi(PrivDir),

    os:cmd("cd " ++ EsiLogPath ++ " ; tar zxvf " ++ EsiLogName),
    LogBaseDirs = log_base_dirs(),
    check_fetched_esi(LogBaseDirs, StreamName, Msgs, EsiLogPath, LogDir).


check_fetched_esi([Dir | Dirs], StreamName, Msgs, EsiLogPath, LogDir) ->
    FileDir  = filename:join([EsiLogPath, Dir, LogDir]),
    FilesStr = os:cmd("cd " ++ FileDir ++ " ; ls"),
    Files    = string:tokens(FilesStr, "\n"),
    LogFiles = [LF || LF <- Files, lists:suffix(".log", LF)],
    FilesRes = ce_files([LF || LF <- LogFiles, lists:prefix(StreamName, LF)],
			FileDir),
    case ce_msgs(FilesRes, Msgs, FileDir) of
	ok ->
	    ok;
	_Error when Dirs =/= [] ->
	    ct:log("Failed to fetch logs from ~s~n~p~n", [FileDir, _Error]),
	    check_fetched_esi(Dirs, StreamName, Msgs, EsiLogPath, LogDir);
	Error ->
	    ct:log("Failed to fetch logs from ~s~n~p~n", [FileDir, Error]),
	    Error
    end.


log_base_dirs() ->
    case os:getenv("SIM_OR_TARGET") of
	"cloudish" ->
	    ?VNF_LOG_BASE_DIRS;
	_Other ->
	    ?LOG_BASE_DIRS
    end.

%%------------------------------------------------------
%% Check if any files found
%%------------------------------------------------------
ce_files([], _) ->
    {error, no_files_found};
ce_files([File | FT], FileDir) ->
    {ok, file:read_file(filename:join([FileDir, File])), FT}.


%%------------------------------------------------------
%% Check that the messages are written to the files
%%------------------------------------------------------
ce_msgs({ok, {ok, Bin}, Files}, Msgs, FileDir) ->
    cem_rc(cem(string:tokens(binary_to_list(Bin), "\n"), Msgs),
	   Files,
	   FileDir);
ce_msgs({ok, Error, _}, _Msgs, _FileDir) ->
    {error, {file_read, Error}};
ce_msgs({error, _} = Error, _, _) ->
    Error;
ce_msgs({Error, _, _}, _, _) ->
    Error.

%% check if we have to continue to read the file
cem_rc(ok, _, _) ->
    ok;
cem_rc({cont, MsgsT}, [File | FT], FileDir) ->
    ce_msgs({ok, file:read_file(filename:join([FileDir, File])), FT},
	    MsgsT,
	    FileDir);
cem_rc(Error, _, _) ->
    Error.


%% Find all messages in this file
cem([], []) ->
    ok;
cem([], Msgs) ->
    {cont, Msgs};
cem([L | Lines], [M | Msgs]) ->
    case string:str(L, M) of
	0 -> {error, {msg_not_found, M}};
	_ -> cem(Lines, Msgs)
    end.


%%=========================================================================
%% clear the directory where the esi files are stored, i.e. the root dir
%%=========================================================================
clear_esi_dir() ->
    Dir = rct_rpc:call(?TESTNODE, safs_log, get_root, [], 5000, noprint),
    AppDir = filename:join([Dir, ?APP_LOG_DIR]),
    rct_rpc:call(?TESTNODE, os, cmd, ["rm -rf " ++ AppDir], 5000, noprint).


%%=========================================================================
%% functions stolen from log_esi_SUITE
%% to be able to fetch the ESI files
%%=========================================================================

%%--------------------------------------------------------------------
%% @doc
%% Using netconf to trig a transfer ESI log from SUT to a sftp server<br/>
%% This TC transfer ESI log from SUT to a sftp server using netconf.<br/>
%% The log file will be stored .../log_private/ , that will be created when TC starts.<br/>
%% TC will check progressreport using netconf and also check that correct file is transfered correct.
%%
%% @spec export_esi(Config) -> ok
%% @end
%%--------------------------------------------------------------------
export_esi(EsiLogPath)->
    %%EsiLogPath = ?config(priv_dir, Config),
    ct:pal("#### EsiLogPath export_esi ~p~n", [EsiLogPath]),
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    SFTP_URL = "sftp://"++Username++"@"++SftpHost,
    os:cmd("chmod 777 " ++ EsiLogPath), % else permission.
    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'LogM',
		  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
		  [{logMId,[],["1"]},
		   {'exportEsi',[],
		    [{uri, [], [SFTP_URL++EsiLogPath]},
		     {password, [], [Password]}]}
		    %% [{uri, [], [?SFTP_URL++EsiLogPath]},
		    %%  {password, [], [?PSWD]}]}
		  ]}]}]},
    ProgressFilter = {'ManagedElement',          
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],["1"]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'LogM',
			  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
			  [{logMId,[],["1"]},
			   {progressReport,[],[]}
			  ]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    %% trig the action export_esi using netconf.
    case ct_netconfc:action(nc1, Action) of
	{ok, A} ->
	    Return = extract_element(returnValue, A),
	    ct:pal("export_esi ~p~n",[Return]);
	Error ->
	    ct:pal("~p~n",[Error]),
	    ct:fail("action error")
    end,
    ct_netconfc:close_session(nc1),
    timer:sleep(500),
    te(wait_for_progress(progressReport, ProgressFilter), EsiLogPath).

te({["SUCCESS"], [EsiLogName]}, EsiLogPath) ->
    ct:pal("export_esi: SUCCESS~n",[]),
    check_esi_log_transfered(EsiLogName, EsiLogPath);
te(Result, _) ->
    ct:pal("export_esi: ~p~n",[Result]),
    ct:fail(Result).




%% @hidden
%% Will check that esi log is tranfered to expected path and also the size of the file.
%% If the size is to small, then maybe it does not consist on anything!
check_esi_log_transfered(EsiLogName, EsiLogPath) ->
    ct:pal("#### EsiLogPath check_esi_log_transfered ~p~n", [EsiLogPath]),
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    {ok,ChPid,_ChRef} = 
	ssh_sftp:start_channel(SftpHost, 
			       [{user, Username},
				{password, Password},
				{silently_accept_hosts, true},
				{timeout, 10000}]),
	%% ssh_sftp:start_channel(?SFTP_HOST, 
	%% 		       [{user, ?USER},
	%% 			{password, ?PSWD},
	%% 			{silently_accept_hosts, true},
	%% 			{timeout, 10000}]),

    {ok, DirData} = ssh_sftp:list_dir(ChPid, EsiLogPath, 2000),
    % DirData = lists of strings from the directory.
    Pred= fun(Str) ->
		  if Str == EsiLogName ->
			  true;
		     true ->
			  false
		  end
	  end,

    case lists:any(Pred,DirData) of
	true -> 
	    ct:pal("### Esi log file: ~p, exist in \n path : ~p \n",
		   [EsiLogName, EsiLogPath]);
	false -> 
	    ct:fail(" Could not find the ESI log file, on sftp server.")
    end,
	     
    {ok, FileInfo} = 
	ssh_sftp:read_file_info(ChPid, EsiLogPath++EsiLogName, 2000),
    ct:pal("### Recieved FileInfo: ~p", [FileInfo]),

    Size = lists:nth(2, tuple_to_list(FileInfo)),
    %{file_info, Size, _, _, _, _, _, _, _, _, _, _, _, _} = FileInfo,

    if Size > 10000 ->
	    ct:pal("### Recieved Size: ~p", [Size]),
	    true;
       true  ->
	    ct:pal("### Size of the esi tar file is: ~p. ~n "
		   "It is smaller than expected. ~n "
		   "Unpack the file and ckeck that it look OK. \n",[Size]),
	    ct:fail("Size of the esi log file is to small! check if it "
		    "looks ok after unpack!.")
    end,  

    X = ssh_sftp:stop_channel(ChPid),
    ct:pal("### stop sftp channel ~p", [X]),
 
    {EsiLogPath, EsiLogName}.


%%%--------------------------------------------------------------------
%%% Description: Loop until the progress information says FINISHED
%%%              Requires a name and a netconf filter extracting the
%%%              progress report attribute
%%%--------------------------------------------------------------------
wait_for_progress(Attribute, ProgressFilter) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    {ok, A} = ct_netconfc:get(nc1, ProgressFilter),
    ct_netconfc:close_session(nc1),
    {ok, Report} = extract_element(Attribute, A),
    {ok, State} = extract_element(state, [Report]),
    timer:sleep(1000),
    case State of
	{state, _, ["FINISHED"]} ->
	    ct:log("~p~n",[Report]),
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, {resultInfo, _, ResultInfo}} = 
		extract_element(resultInfo, [Report]),
	    {Result, ResultInfo};
	{state, _, ["CANCELLED"]} ->
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, {resultInfo, _, ResultInfo}} = 
		extract_element(resultInfo, [Report]),
	    {Result, ResultInfo};
	{state, _, [Current]} ->
	    ct:log("State: ~s~n",[Current]),
	    wait_for_progress(Attribute, ProgressFilter)
    end.

%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.



remote_trace() ->
    Exp = [{'_',[],[{message,{exception_trace}}]}],
    {ok, TN} = rct_safe_log_rpc:get_testnode(),
%%     ct:pal("### dbg rt ~p~n", [rpc:call(TN, dbg, tracer, [])]),
%%     ct:pal("### dbg rt ~p~n", [rpc:call(TN, dbg, p, [all, [c]])]),
%%     ct:pal("### dbg rt ~p~n", [rpc:call(TN, dbg, tpl, [safe_log_lib, '_', '_', Exp])]),
%%     ct:pal("### dbg rt ~p~n", [rpc:call(TN, dbg, tpl, [safe_lib, '_', '_', Exp])]),
    ct:pal("### dbg rt ~p~n", [rpc:call(TN, dbg, tp, [safe_log, '_', '_', Exp])]).


local_trace() ->
    dbg:tracer(),
    dbg:p(all, [c]),
%%     dbg:tp(gen_server,call,'_',[{'_',[],[{message,{exception_trace}}]}]),
    dbg:tpl(rct_safe_log_rpc,'_','_',[{'_',[],[{message,{exception_trace}}]}]),
%%     dbg:tpl(rct_safe_rpc_lib,'_','_',[{'_',[],[{message,{exception_trace}}]}]),
    ok.


