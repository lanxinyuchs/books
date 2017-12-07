%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	timDataInit.erl %
%%% Author:	etxbjca
%%% Description: Data initialization functions used at system installation and
%%%              software upgrade.
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(timDataInit).
-vsn('/main/R4A/R5A/1').
-date('2016-01-07').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	xxxDataInit.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% ----    ---------- -------  ------------------------------------------------
%%% R4A/1   2015-06-22 etxbjca  Created
%%% R4A/2   2015-07-07 etxjotj  First version
%%% R4A/3   2015-07-07 etxberb  Changed mnesia:create_table to
%%%                             clhI:mnesia_create_table.
%%% R4A/5   2015-09-01 erarafo  timServer declared as child
%%% R4A/6   2015-09-28 erarafo  Support for interactive testing
%%% R4A/7   2015-09-30 erarafo  Improved support for interactive testing
%%% R4A/8   2015-10-08 erarafo  More support for interactive testing
%%% R4A/9   2015-10-09 erarafo  Interactive test support adjusted
%%% R4A/10  2015-10-17 erarafo  Restructuring and cleanup
%%% R4A/11  2015-10-18 erarafo  Server made 'permanent'
%%% R4A/12  2015-11-03 erarafo  Line length not exceeding 80
%%% R4A/13  2015-11-04 erarafo  Non-default restart strategy
%% ----    ---------- -------  ------------------------------------------------
%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-define(CHILD_MODE, permanent).
%-define(CHILD_MODE, transient).    % useful for interactive testing

%% -define(SERVER_MODULE_ABSPATH,
%% 	"/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/"
%%      "TIM/TIM_CNX9013408/TIM_CAX1033921/out/timServer").
%% -define(MODEL_MODULE_ABSPATH,
%% 	"/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/"
%%      "TIM/TIM_CNX9013408/TIM_CAX1033921/out/timModel").

-define(SERVER_MODULE_ABSPATH,
	"/home/sirpa/dev_patches/timServer").
-define(MODEL_MODULE_ABSPATH,
	"/home/sirpa/dev_patches/timModel").
-define(LIB_MODULE_ABSPATH,
	"/home/sirpa/dev_patches/timLib").

-define(SERVER_MODULE, timServer).

-export([instPhParallel_init/1,
	 instPhParallel_init_data/0,
	 instPhParallel_post_init/0]).
-export([children/0,
	 activate/0, 
	 restart_strategy/0]).


-export([testControl/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RmeTimeSettings.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init(DbNodes) ->
    TimeSettings = [{timeSettings, ?timeSettings_types}],

    [[create_table(TableDef, DbNodes)||TableDef<-Tables]||
	Tables<-[TimeSettings]],
    
%%     {atomic, ok} =
%% 	clhI:mnesia_create_table(
%% 	  tziClients, 
%% 	  [{type, set},   
%% 	   {disc_copies, DbNodes}]),  % TODO, ram_copies is good enough?

    ok.

children() ->
    {ok, [{?SERVER_MODULE,
	   {?SERVER_MODULE, start_link, []},
	   ?CHILD_MODE,
	   1000,
	   worker,
	   [?SERVER_MODULE]}]}.

restart_strategy() ->
    {ok, {one_for_one, 0, 1}}.


create_table({Name, Types}, DbNodes) ->
    Fields = [Field||{Field, _}<-Types],
    {atomic, ok} =
	clhI:mnesia_create_table(Name, [{type, set},
					{disc_copies, DbNodes},
					{attributes, Fields} |
					add_clh_option(Name)]).%% ;
%% create_table({Name, Types, ram}, DbNodes) ->
%%     Fields = [Field||{Field, _}<-Types],
%%     {atomic, ok} =
%%	clhI:mnesia_create_table(Name, [{type, set},
%%					{ram_copies, DbNodes},
%%					{attributes, Fields} |
%%					add_clh_option(Name)]).

%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_data() ->
    case swmI:is_upgrade_ongoing() of
	true ->
	    case swmI:read(schema, timeSettings) of
		[] ->
		    mnesia:dirty_write(
		      #timeSettings{timeSettingsId={"1","1","1"}});
		_ ->
		    ok = swmI:copy_old_table(timeSettings)
	    end;
	false ->
	    mnesia:dirty_write(#timeSettings{timeSettingsId={"1","1","1"}})
    end,
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_post_init() ->
    Root = ["ManagedElement", "NodeSupport", "TimeSettings"],
    comsaLib:register_callback(Root, timModel),

    comsaI:register_subscriptions(
      "RmeTimeSettings", [{"TimeSettings", timeSettings}]).


activate()->
    timServer:activate(),
    ok.


%%% ----------------------------------------------------------
%%% @doc Restart the timServer module; useful for interactive
%%% testing. Before using this function define CHILD_MODE as
%%% transient.
%%%
%%% To restart the server process type
%%%
%%%     testControl(restartServer, []).
%%%
%%% @end
%%% ----------------------------------------------------------
testControl(restartServer, []) ->
    timServer:testControl(stop, []),

    IftApp = "ift_app",
    case lists:keyfind(IftApp, 1, appmServer:get_apps()) of
	false ->
	    ok;
	_ ->
	    appmServer:stop_lm(IftApp, 0)
    end,
    appmServer:start_lm(IftApp, 0),

    try
	lists:foreach(
	  fun(SleepMillis) ->
		  case whereis(timServer) of
		      undefined ->
			  throw(ok);
		      _ ->
			  timer:sleep(SleepMillis)
		  end
	  end,
	  [100, 200, 500, 1000, 1000, 2000, 2000]) of
	ok ->
	    false
    catch
	throw:ok ->
	    testControl(reload, []),
	    timServer:testControl(start, []),
	    ok
    end;

testControl(reload, []) ->
    lists:map(
      fun({Module, Path}) ->
	      code:purge(Module),
	      code:load_abs(Path)
      end,
      [{timServer, ?SERVER_MODULE_ABSPATH},
       {timLib, ?LIB_MODULE_ABSPATH},
       {timModel, ?MODEL_MODULE_ABSPATH}]).


%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%%some_method(Parameter)->
%%   nn.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%%internal_function1(One, Two)->
%%   nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%% info_msg(Format) ->
%%     info_msg(Format, []).

%% info_msg(Format, Args) ->
%%     error_logger:info_msg("~w: "++Format, [?MODULE|Args]).

%error_msg(Format) ->
%    error_msg(Format, []).
%% error_msg(Format, Args) ->
%%     error_logger:error_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     error_logger:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%even_more_internal_function1(One, Two)->
%   nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


