%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfTrService.erl %
%%% @author qostjoa
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R10A/R11A/R12A/1

%%% @doc ==Distinguished name translation service==
%%% This module implements the distinguished name translation service interface
%%% DNTI to provide a translation mechanisms between MO references and
%%% IMM references
%%% end

-module(gmfTrService).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R10A/R11A/R12A/1').
-date('2017-11-24').
-author('qostjoa').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% Rev        Date         Name        What
%%% -----      -------      --------    ------------------------
%%% R1A/1      2012-10-31   uabesvi     Created
%%% R2A/6      2013-03-04   etxjotj     Removed the trap_exit since it messed
%%%                                     up the shutdown sequence, and there was
%%%                                     no code for receiving exit traps anyway
%%% R2A/8      2013-03-18   uabesvi     fixed parents for imm_to_mim
%%% R2A/15     2014-03-31   etxpeno     Support for "ManagedElement=1,SystemFunctions=1,SysM=1"
%%% R2A/21     2014-07-30   uabesvi     added nodelay to inet:setopts, and
%%%                                     increased cec_setup timeout
%%% R3A/9      2014-12-12   etxpeno     Handle multiple parents in i2mu/2
%%% R3A/11     2015-01-15   etxpeno     Support for regular role
%%% R3A/18     2015-03-09   etxpeno     Startup timing
%%%                                     Correction in get_imm_class2/4
%%% R3A/19     2015-03-10   erarafo     Some EDoc and -spec; groundwork for HT52686
%%% R3A/24     2015-04-14   etxberb     Added sysUtil:timediff_XXX.
%% ----    ---------- -------  ------------------------------------------------
%% R4A/1   2015-06-15 erarafo  Support for HwInventory under SystemFunctions
%% R4A/4   2015-07-07 etxberb  Changed mnesia:create_table to
%%                             clhI:mnesia_create_table.
%%% R4A/9  2015-10-09 etxpeno  Added cec_takeover
%%% R4A/12 2016-02-05 etxpeno  (TR HU55442) correction in m2i_get_rdn/4
%%% R5A/1  2015-11-17 etxpeno  remove dead code
%%% ----------------------------------------------------------
%%% R6A/2  2016-06-13 uabesvi  HU99496
%%% R6A/4  2016-09-15 etxpeno  MIB sync improvements
%%% R6A/5  2016-09-22 etxpeno  HV26141 Add subscription of OAP data
%%% ----------------------------------------------------------
%%% R7A/1  2016-11-02 etxpeno  Make the handling of OAP related data more robust
%%% ----------------------------------------------------------
%%% R8A/1  2016-12-30 etxpeno  remove imm_to_mim_details/1, moref_from_ecim/1,
%%%                             moref_to_ecim/1
%%% ----------------------------------------------------------
%%% R10A/1 2017-05-08 etxpeno  (TR HV86130)
%%% R10A/2 2017-07-11 emarnek  HV87092
%%% ----------------------------------------------------------
%%% R11A/1 2017-09-07 etxpeno  TR HW26710
%%% ----------------------------------------------------------
%%% R12A/1 2017-11-21 qostjoa  Add support for RmeSds split
%%% ----------------------------------------------------------


-export([start/0,
	 start/1,
	 start_link/0,
	 start_link/1,
	 stop/0]).


-export([activate/0]).
-export([oot_notify/1]).

-export([cec_setup/1, cec_takeover/1]).
-export([mim_to_imm/1]).
-export([imm_to_mim/1]).
-export([imm_to_mim_details/1]).
-export([is_moclass_struct/1]).
-export([is_attr_ref_to_struct/2]).
-export([get_struct_ref_for_attr/2]).

-export([init_tables/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).


-export([check_integer/1]).
-export([check_string/1]).

-export([get_integer/1]).
-export([get_string/1]).

-export([get_adm_oper_id/2, get_action_name/2]).

-export([get_oap_data/0, get_oap_data2/0]).
-export([get_imm_class/1]).

-export([get_object_id/1, get_ecim_dn/1]).

%% test
-export([get_bidir_assoc/1]).

-define(SERVER, ?MODULE).

-define(GS_TO, 10000).

-define(PRI_SIGNAL_REVISION, 0).

-define(INIT_SERVICE,          0).
-define(TERM_SERVICE,          1).
-define(MIM_TO_IMM,            2).
-define(IMM_TO_MIM,            3).
-define(IS_MOCLASS_STRUCT,     4).
-define(IS_ATTR_REF_TO_STRUCT, 5).
-define(GET_STRUCTREFFORATTR,  6).
-define(GET_INTEGER,           7).
-define(GET_STRING,            8).
-define(GET_INTEGERS,          9).
-define(GET_STRINGS,          10).
-define(GET_MEID,             11).
-define(GET_ADMOPID,          12).
-define(GET_ACTIONNAME,       13).
-define(GET_OAPDATA,          14).
-define(GET_ME_USER_LABEL,    15).
-define(GET_IMM_CLASS,        16).
-define(GET_BIDIR_ASSOC,      17).
-define(GET_OAPDATA2,         18).
-define(GET_DN_PREFIX,        19).
-define(SUBSCR_OAP_DATA,      20).

%% Refer to csrc/cmsi.h
-define(CMSI_OK,                      0).
-define(CMSI_OBJECT_CLASS_NOT_FOUND,  5).
-define(CMSI_CLASS_NOT_FOUND,         6).
-define(CMSI_ATTRIBUTE_NOT_FOUND,     7).
-define(CMSI_ATTRIBUTE_NO_STRUCT_REF, 8).
-define(CMSI_LDN_NOT_FOUND,           9).
-define(CMSI_ACTION_NOT_FOUND,        10).

-define(CMSI_RESERVED_OBJECTIDS_START, 16677216).
-define(CMSI_RESERVED_OBJECTIDS_END,   16777215).

-define(OBJECTIDS_START, 1).
-define(OBJECTIDS_END, 16#ffffffff).

-record(state, {sockets        :: list(),
		oapNs = <<>>   :: iodata(),
		oapIA = 0      :: integer(),
		oapDscp = 0    :: integer(),
		oapIPv4 = true :: boolean(),
		oapSubscribers = []}).
-record(gmfTrData, {key, value}).

-include("gmf.hrl").
-include("cmsi.hrl").

-define(GMFLOG, gmfLog:comte).

%%===========================================================================
%% gen_server start functions
%%===========================================================================
start() ->
    start([]).
start(Opts) ->
    %%process_flag(trap_exit, true),
    ok = cec:register(<<"DNTI">>, gmfTrService),
    ?GMFLOG({?MODULE, cec_register, "DNTI"}),
    gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).

start_link() ->
    start_link([]).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

stop() ->
    ok = cec:unregister(<<"DNTI">>),
    ?GMFLOG({?MODULE, cec_unregister, "DNTI"}),
    gen_server:cast(?SERVER, stop).

init(_Opts) ->
    %%    erlang:process_flag(trap_exit, true),
    sysInitI:log_startup_time(gmfImmTrService_started),
    {ok, #state{sockets = []}}.

activate() ->
    gen_server:cast(?SERVER, activate).

cec_setup(Socket) ->
    try
	gen_server:call(?SERVER, {cec_setup, Socket}, 30000)
    catch exit:{timeout, _} = Reason ->
            sysInitI:error_report([{mfa, {?MODULE, ?FUNCTION_NAME, []}},
				   {caught, {exit, Reason}},
				   {socket, Socket}
				  ]),
	    throw(Reason)
    end.

cec_takeover(Socket) ->
    gen_server:cast(?SERVER, {cec_takeover, Socket}).

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

terminate(_Reason, S) ->
    [gen_tcp:close(Socket) || Socket <- S#state.sockets],
    ok.

init_tables(DbNodes) ->
    {atomic, ok} =
	clhI:mnesia_create_table(gmfTrData,
				 [{disc_copies, DbNodes},
				  {index, [value]},
				  {attributes, record_info(fields,gmfTrData)}]),
    ok.

%%% ----------------------------------------------------------
%%% @doc Called through fun registered in the ootServer
%%% informs about dscp, net-name-space, ip-addresses (lmt and tn)
%%% @end
%%% ----------------------------------------------------------
oot_notify(PropList) ->
    gen_server:cast(?SERVER, {oot_notify, PropList}).

%%===========================================================================
%% Interface functions for transforming integers to strings, and vice versa.
%% The integers can be seen as aliases for the strings.
%%===========================================================================

check_integer(Integer) ->
    case gen_server:call(?SERVER, {check_integer, Integer}, ?GS_TO) of
	{?CMSI_OK, String}          -> {ok, binary_to_list(String)};
	{?CMSI_LDN_NOT_FOUND, <<>>} -> {error, not_found}
    end.

check_string(String) ->
    gen_server:call(?SERVER, {check_string, list_to_binary(String)}, ?GS_TO).


get_integer(String) ->
    {?CMSI_OK, Int} = gen_server:call(?SERVER,
				      {get_integer, list_to_binary(String)},
				      ?GS_TO),
    {ok, Int}.

get_string(Integer) ->
    case gen_server:call(?SERVER, {get_string, Integer}, ?GS_TO) of
	{?CMSI_OK, String}          -> {ok, binary_to_list(String)};
	{?CMSI_LDN_NOT_FOUND, <<>>} -> {error, not_found}
    end.

get_oap_data() ->
    gen_server:call(?SERVER, get_oap_data, ?GS_TO).

get_oap_data2() ->
    gen_server:call(?SERVER, get_oap_data2, ?GS_TO).

%%===========================================================================
%% gen_server message functions
%%===========================================================================
%%---------------------------------------------------------------------
%% call
%%---------------------------------------------------------------------
handle_call({cec_setup, Socket}, _From, #state{sockets = Sockets} = S) ->
    ?GMFLOG({?MODULE,
	     cec_setup,
	     [{socket,  Socket},
	      {sockets, Sockets}]}),
    {reply, self(), S#state{sockets = [Socket | Sockets]}};
handle_call({check_integer, Integer}, _From, S) ->
    {reply, handle_get_string(Integer), S};
handle_call({check_string, String}, _From, S) ->
    {reply, handle_check_string(String), S};
handle_call({get_integer, String}, _From, S) ->
    {reply, handle_get_integer(String), S};
handle_call({get_string, Integer}, _From, S) ->
    {reply, handle_get_string(Integer), S};
handle_call(get_oap_data, _From, S) ->
    {reply, handle_get_oap_data(S), S};
handle_call(get_oap_data2, _From, S) ->
    {reply, handle_get_oap_data2(S), S};
handle_call(Command, _From, S) ->
    {reply, Command, S}.

%%---------------------------------------------------------------------
%% cast
%%---------------------------------------------------------------------
handle_cast({oot_notify, PropList}, State) ->
    NewState =
	lists:foldl(
	  fun({oap_namespace, B}, S) when is_binary(B) ->
		  S#state{oapNs = get_oap_namespace(B)};
	     ({dscp, Dscp}, S) when is_integer(Dscp) ->
		  S#state{oapDscp = Dscp};
	     ({access_point_address, I}, S) when is_list(I) ->
		  {OapIA, OapIPv4} = get_oap_ip_addr(I),
		  S#state{oapIA   = OapIA,
			  oapIPv4 = OapIPv4};
	     (_, S) ->
		  S
	  end, State, PropList),

    send_oap_data(NewState, State),

    {noreply, NewState};
handle_cast(stop, S) ->
    {stop, normal, S};
handle_cast(activate, S) ->
    ok = ootI:register_cfg_upd_cb(fun ?MODULE:oot_notify/1),
    OapNS = get_oap_namespace(),
    {OapIA, OapIPv4} = get_oap_ip_addr(),
    OapDscp = get_oap_dscp(),
    NewState = S#state{oapNs   = OapNS,
		       oapIA   = OapIA,
		       oapDscp = OapDscp,
		       oapIPv4 = OapIPv4},
    send_oap_data(NewState, S),
    {noreply, NewState};
handle_cast({cec_takeover, Socket}, S) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, S};
handle_cast(_Msg, S) ->
    {noreply, S}.

%%---------------------------------------------------------------------
%% info
%%---------------------------------------------------------------------
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?INIT_SERVICE:4/native-unsigned-integer-unit:8>>},
	    S) ->
    ?GMFLOG({?MODULE,
	     init_service,
	     [{socket, Socket}]}),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?TERM_SERVICE:4/native-unsigned-integer-unit:8>>},
            #state{sockets = Sockets} = S) ->
    ?GMFLOG({?MODULE,
	     term_service,
	     [{socket, Socket}]}),
    gen_tcp:close(Socket),
    {noreply, S#state{sockets = lists:delete(Socket, Sockets)}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?MIM_TO_IMM:4/native-unsigned-integer-unit:8,
	       _Size:4/native-unsigned-integer-unit:8, Mim/binary>>},
            S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('MIM_TO_IMM'),
    translate(fun mim_to_imm/1, Mim, Socket),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?IMM_TO_MIM:4/native-unsigned-integer-unit:8,
	       _Size:4/native-unsigned-integer-unit:8, Imm/binary>>},
            S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('IMM_TO_MIM'),
    translate(fun imm_to_mim/1, Imm, Socket),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?IS_MOCLASS_STRUCT:4/native-unsigned-integer-unit:8,
	       ImmClass/binary>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('IS_MOCLASS_STRUCT'),
    reply_is_moclass_struct(ImmClass, Socket),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?IS_ATTR_REF_TO_STRUCT:4/native-unsigned-integer-unit:8,
	       ImmClassSize:4/native-unsigned-integer-unit:8,
	       ImmClass:ImmClassSize/binary,
	       AttrName/binary>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('IS_ATTR_REF_TO_STRUCT'),
    reply_is_attr_ref_to_struct(ImmClass, AttrName, Socket),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_STRUCTREFFORATTR:4/native-unsigned-integer-unit:8,
	       ImmClassSize:4/native-unsigned-integer-unit:8,
	       ImmClass:ImmClassSize/binary,
	       AttrName/binary>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_STRUCTREFFORATTR'),
    reply_get_struct_ref_for_attr(ImmClass, AttrName, Socket),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_INTEGER:4/native-unsigned-integer-unit:8,
	       String/binary>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_INTEGER'),
    reply_get_integer(String, Socket),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_STRING:4/native-unsigned-integer-unit:8,
	       Integer:4/native-unsigned-integer-unit:8>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_STRING'),
    reply_get_string(Integer, Socket),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_INTEGERS:4/native-unsigned-integer-unit:8,
	       Data/binary>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_INTEGERS'),
    reply_get_integers(Data, Socket),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_STRINGS:4/native-unsigned-integer-unit:8,
	       Data/binary>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_STRINGS'),
    reply_get_strings(Data, Socket),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_MEID:4/native-unsigned-integer-unit:8>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_MEID'),
    reply_get_network_managed_element_id(Socket),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_ADMOPID:4/native-unsigned-integer-unit:8,
	       ImmClassSize:4/native-unsigned-integer-unit:8,
	       ImmClass:ImmClassSize/binary,
	       ActionName/binary>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_ADMOPID'),
    reply_get_adm_op_id(Socket, ImmClass, ActionName),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_ACTIONNAME:4/native-unsigned-integer-unit:8,
	       AdmOpId:8/native-unsigned-integer-unit:8,
	       ImmClass/binary>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_ACTIONNAME'),
    reply_get_action_name(Socket, ImmClass, AdmOpId),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_OAPDATA:4/native-unsigned-integer-unit:8>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_OAPDATA'),
    reply_get_oap_data(Socket, S),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_ME_USER_LABEL:4/native-unsigned-integer-unit:8>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_ME_USER_LABEL'),
    reply_get_me_user_label(Socket),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_IMM_CLASS:4/native-unsigned-integer-unit:8,
	       Imm/binary>>},
            S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_IMM_CLASS'),
    reply_get_imm_class(Socket, Imm),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_BIDIR_ASSOC:4/native-unsigned-integer-unit:8,
	       ImmClass/binary>>},
            S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_BIDIR_ASSOC'),
    reply_get_bidir_assoc(Socket, ImmClass),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_OAPDATA2:4/native-unsigned-integer-unit:8>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_OAPDATA2'),
    reply_get_oap_data2(Socket, S),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?SUBSCR_OAP_DATA:4/native-unsigned-integer-unit:8>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('SUBSCR_OAP_DATA'),
    reply_get_oap_data2(Socket, S),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    sysUtil:timediff_after_and_previous(Before),
    NewOapSubscribers = [Socket|S#state.oapSubscribers],
    {noreply, S#state{oapSubscribers = NewOapSubscribers}};
handle_info({tcp, Socket,
	     <<_ClientPid:4/native-unsigned-integer-unit:8,
	       ?GET_DN_PREFIX:4/native-unsigned-integer-unit:8>>}, S) ->
    timediff_start(?MODULE),
    Before = sysUtil:timediff_before('GET_ME_USER_LABEL'),
    reply_get_dn_prefix(Socket),
    sysUtil:timediff_after_and_previous(Before),
    {noreply, S};

handle_info({tcp_closed, Socket},
	    #state{sockets        = Sockets,
		   oapSubscribers = OapSubscribers} = S) ->
    ?GMFLOG({?MODULE,
	     tcp_closed,
	     [{socket, Socket}]}),
    NewSockets = lists:delete(Socket, Sockets),
    NewOapSubscribers = lists:delete(Socket, OapSubscribers),
    {noreply, S#state{sockets        = NewSockets,
		      oapSubscribers = NewOapSubscribers}};
handle_info(_Info, S) ->
    {noreply, S}.

%%====================================================================
%% Internal functions
%%====================================================================
timediff_start(Tag) ->
    case get(sysUtilTimediff) of
	undefined ->
	    case sysUtil:timediff_start(Tag) of
		disabled ->
		    ok;
		_ ->
		    put(sysUtilTimediff, started)
	    end;
	_ ->
	    ok
    end.

%%===========================================================================
%% translate functions
%%===========================================================================
translate(Fun, String, Socket) ->
    Res = case Fun(String) of
	      {ok, Transf}   ->
		  [<<?CMSI_OK:1/native-integer-unit:8>>, Transf, 0];
	      {error, Error} ->
		  [<<?CMSI_OBJECT_CLASS_NOT_FOUND:1/native-integer-unit:8>>,
		   Error, 0]
	  end,
    gen_tcp:send(Socket, Res),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]).


%%===============================================================
%% MIM to IMM
%%===============================================================

%%% ----------------------------------------------------------
%%% @doc Convert the given ECIM DN to an IMM DN.
%%% @end
%%% ----------------------------------------------------------
-spec mim_to_imm(binary()) -> {ok, string()}|{error, string()}.

mim_to_imm(Mim) ->
    MimL  = binary_to_list(Mim),
    MimLS = string:strip(MimL, right, 0),
    MimS = list_to_binary(MimLS),
    ?GMFLOG({?MODULE, mim_to_imm, MimS}),
    Res =
	case gmfSALib:mim_to_imm(MimS) of
	    {error, Reason} ->
		{error, Reason};
	    {ok, ImmDn} ->
		{ok, binary_to_list(ImmDn)}
	end,
    ?GMFLOG({?MODULE, mim_to_imm_res, Res}),
    Res.


%% %%% ----------------------------------------------------------
%% %%% @doc Convert the given ECIM DN string to an IMM DN.
%% %%% @end
%% %%% ----------------------------------------------------------
%% -spec mim_to_imm2(string()) -> {ok, string()}|{error, binary()}.

%% mim_to_imm2("ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1"++_ = Mim) ->
%%     %% special case: do NOT perform ECIM -> IMM conversion
%%     {ok, Mim};

%% mim_to_imm2(Mim) ->
%%     m2i(split_dn(Mim), Mim).


%% %%% ----------------------------------------------------------
%% %%% @doc Preprocesses the ECIM DN to be passed to m2i_get_rdn.
%% %%%
%% %%% The first argument is an ECIM DN represented as a top-down
%% %%% list of binary fragments.
%% %%%
%% %%% The second argument is ignored except if a match error
%% %%% occurs.
%% %%% @end
%% %%% ----------------------------------------------------------
%% -spec m2i([binary()], string()) -> {ok, string()}|{error, string()}.


%% m2i([<<"ManagedElement">>, _, <<"SystemFunctions">>, _|[<<"HwInventory">>, _|_]=DnT], _) ->
%%     %% The DN is ManagedElement=1,SystemFUnctions=1,HwInventory=1 and possibly deeper
%%     m2i_get_rdn(undefined, DnT, "SystemFunctions", []);

%% m2i([<<"ManagedElement">>, _, <<"SystemFunctions">>, _, <<"SysM">>, _|DnT], _) ->
%%     %% DNs into OamAccessPoint=1 and below
%%     m2i_get_rdn(undefined, DnT, "SysM", []);

%% m2i([<<"ManagedElement">>, _, <<"SystemFunctions">>, _|DnT], _) ->
%%     m2i_get_rdn(undefined, DnT, "SystemFunctions", []);

%% m2i([<<"ManagedElement">>, _|DnT], _) ->
%%     m2i_get_rdn(undefined, DnT, "ManagedElement", []);

%% m2i(Dn, _) when length(Dn) >= 2 ->
%%     m2i_get_rdn(undefined, Dn, "", []);

%% m2i(_, Mim) ->
%%     {error, Mim}.


%% %%% ----------------------------------------------------------
%% %%% @doc Attempts to return a {ok, ImmDnString} tuple.
%% %%%
%% %%% The first argument is a MOM name or 'undefined'.
%% %%%
%% %%% The second argument is a top-down ECIM DN represented
%% %%% as a list of binary fragments (classnames and RDN values).
%% %%%
%% %%% The third argument is a classname which is the parent of
%% %%% the leading class in the ECIM DN list.
%% %%%
%% %%% The fourth argument is an accumulator holding the IMM
%% %%% DN under buildup, represented as a list of RdnName=RdnValue
%% %%% strings.
%% %%% @end
%% %%% ----------------------------------------------------------
%% -spec m2i_get_rdn(string()|undefined, [binary()], string(), [string()]) ->
%% 			 {ok, string()}|{error, string()}.

%% m2i_get_rdn(_MimName, [], _, ObjDn) ->
%%     {ok, string:join(ObjDn, ",")};
%% m2i_get_rdn(_MimName, [Class], _Parent, _ObjDn) ->
%%     %% Handles the error case when an incorrect DN contains of an odd number of
%%     %% classes and values e.g. ManagedElement=1,CarrierAggregationFunction
%%     %% (TR HU55442)

%%     %% In some cases Class might look like <<"E1T1Port.e1t1PortId">>
%%     %% In this case the classname is "E1T1Port"
%%     %% Remove the period and the Id after the period
%%     ClassL = re:replace(Class, <<"\\..*$">>, <<"">>, [{return,list}]),
%%     {error, ClassL};
%% m2i_get_rdn(MimName, [Class, RdnValue | Rest], Parent, ObjDn) ->
%%     %% In some cases Class might look like <<"E1T1Port.e1t1PortId">>
%%     %% In this case the classname is "E1T1Port"
%%     %% Remove the period and the Id after the period
%%     ClassL = re:replace(Class, <<"\\..*$">>, <<"">>, [{return,list}]),

%%     case read_mim_class(MimName, ClassL) of
%% 	[#gmfMimClass{key={MimName1, _},
%% 		      imm_rdn=ImmRdnName,
%% 		      parent=ParentsList}] ->
%% 	    IsParentDefined = proplists:is_defined(Parent, ParentsList),
%% 	    if
%% 		not(IsParentDefined) ->
%% 		    {error, Parent ++ " not valid parent for " ++ ClassL};

%% 		Parent =:= "SystemFunctions" andalso
%% 		ClassL =:= "HwInventory" ->
%% 		    ObjDn2 =
%% 			[string:join([ImmRdnName, binary_to_list(RdnValue)], "="),
%% 			 "systemFunctionsId=1"],
%% 		    m2i_get_rdn(MimName1, Rest, ClassL, ObjDn2);

%% 		Parent =:= "SysM" andalso ObjDn =:= [] ->
%% 		    ObjDn2 =
%% 			[string:join([ImmRdnName, binary_to_list(RdnValue)], "="),
%% 			 "sysMId=1",
%% 			 "systemFunctionsId=1"],
%% 		    m2i_get_rdn(MimName1, Rest, ClassL, ObjDn2);

%% 		true ->
%% 		    ObjDn2 = [string:join([ImmRdnName, binary_to_list(RdnValue)], "=")|ObjDn],
%% 		    m2i_get_rdn(MimName1, Rest, ClassL, ObjDn2)
%% 	    end;
%% 	_ ->
%% 	    %% no match or multiple match
%% 	    {error, ClassL}
%%     end.

%% read_mim_class(MimName, ClassName) ->
%%     case mnesia:match_object(#gmfMimClass{key = {'_', ClassName},
%%     					  _   = '_'}) of
%%     	[] ->
%%     	    [];
%%     	[_] = Rec ->
%%     	    Rec;
%%     	[_|_] = Recs ->
%%     	    lists:filter(fun(#gmfMimClass{key = {MimName1, _}}) ->
%%     				 MimName1 == MimName
%%     			 end, Recs)
%%     end.


%%===============================================================
%% IMM to MIM
%%===============================================================

%%% ----------------------------------------------------------
%%% @doc Converts a DN from IMM to MIM style.
%%% @end
%%% ----------------------------------------------------------
-spec imm_to_mim(binary()) -> {ok, string()} | {error, any()}.

imm_to_mim(Imm) ->
    ImmL  = binary_to_list(Imm),
    ImmLS = string:strip(ImmL, right, 0),
    ?GMFLOG({?MODULE, imm_to_mim, ImmLS}),
    Res = mnesia:ets(fun() -> imm_to_mim2(ImmLS) end),
    ?GMFLOG({?MODULE, imm_to_mim_res, Res}),
    Res.


%%% ----------------------------------------------------------
%%% @doc Perform the imm_to_mim work in the context of a
%%% mnesia transaction.
%%%
%%% TODO: Can the first clause ever occur? The expected argument
%%% is in MIM style, but the seconf and general clause expects
%%% a string in bottom-up IMM-style order.
%%% @end
%%% ----------------------------------------------------------
-spec imm_to_mim2(string()) -> {ok, string()} | {error, any()}.

imm_to_mim2("ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1"++_ = Imm) ->
    {ok, Imm};
imm_to_mim2("ManagedElement=1,NodeSupport=1,ServiceDiscovery=1" = Imm) ->
    {ok, Imm};
imm_to_mim2("ManagedElement=1,NodeSupport=1,ServiceDiscoveryServer=1" = Imm) ->
    {ok, Imm};

imm_to_mim2(Imm) ->
    try
	Dn = lists:reverse(split_dn(Imm)),
	%% the DN will be top-down and look like this e g:
	%% [<<"1">>, <<"NOBLEheliumId">>, <<"1">>, <<"neonId">>, <<"1">>, <<"argonId">>]
        %% or
	%% [<<"1">>, <<"systemFunctionsId">>,
        %%  <<"1">>, <<"RcsHwIMhwInventoryId">>,
        %%  <<"101">>, <<"lampId">>, ]

	{Ins, ClassId, DnT} = i2m(Dn),
	%% ClassId could be: <<"NOBLEheliumId">>
	%% Ins could be: <<"1">>

	{Up, Key, MimName}  = i2m_up(ClassId),
	%% {["ManagedElement=1"],"Helium","NOBLE"}

	Down = i2m_down(DnT, Key, MimName, []),
	Top  = Key ++ "=" ++ binary_to_list(Ins),
	{ok, string:join(lists:append([Up, [Top], Down]), ",")}
	%% Up is the above-application prefix, like ["ManagedElement=1"]
	%% [Top] is the topmost instance (root instance in MOM): ["Helium=1"]
	%% Down is the rest of the wanted DN.

    catch throw:{?MODULE, Error} ->
	    Error;
	  error:_ ->
	    {error, Imm}
    end.


-spec i2m([binary()]) -> {binary(), binary(), [binary()]}.

i2m([_, <<"systemFunctionsId">>, _, <<"sysMId">>, Ins, ClassId | DnT]) ->
    %% DNs like oamAccessPointId=1,sysMId=1,systemFunctionsId=1
    %% need this clause
    {Ins, ClassId, DnT};

i2m([_, <<"systemFunctionsId">>, Ins, ClassId | DnT]) ->
    %% DNs like RcsHwIMhwInventoryId=1,systemFunctionsId=1 need this clause
    {Ins, ClassId, DnT};

i2m([Ins, ClassId | DnT]) ->
    {Ins, ClassId, DnT}.


%%% ----------------------------------------------------------
%%% @doc Expects an IMM RDN name. A matching #gmfMimClass{}
%%% record is looked up. The result of this function is the
%%% result of passing the record to the i2mu/2 function.
%%% @end
%%% ----------------------------------------------------------
-spec i2m_up(binary()) -> {[string()], string(), string()}.

i2m_up(ClassId) ->
    ClassIdL = binary_to_list(ClassId),
    %% it is trusted that the lookup produces exactly ONE hit;
    %% this is reasonable since the RDN name is a concatenated
    %% one, e g "NOBLEheliumId"
    i2mu(mnesia:index_read(gmfMimClass, ClassIdL, #gmfMimClass.imm_rdn), ClassIdL).

%%% ----------------------------------------------------------
%%% @doc Expects a list with precisely ONE #gmfImmClass{}
%%% record describing a root class in a MIM. The class is
%%% expected to have a parent which is ManagedElement=1 or
%%% some other RBS CS class that can have MOMs underneath it.
%%%
%%% A tuple is returned, containing the MIM DN of the
%%% parent class, and the class name and MIM name of the
%%% class itself.
%%% @end
%%% ----------------------------------------------------------
-spec i2mu([#gmfMimClass{}], string()) ->
		  {[string()], string()|'_', string()|'_'}.

i2mu([#gmfMimClass{parent   = [{"ManagedElement", _}],
		   key      = {Name, Key}}],
     _) ->
    {["ManagedElement=1"], Key, Name};

i2mu([#gmfMimClass{parent   = [{"SystemFunctions", _}],
		   key      = {Name, Key}}],
     _)  ->
    {["ManagedElement=1", "SystemFunctions=1"],
     Key,
     Name};

i2mu([#gmfMimClass{parent   = [{"Equipment", _}],
		   key      = {Name, Key}}],
     _) ->
    {["ManagedElement=1", "Equipment=1"],
     Key,
     Name};

i2mu([#gmfMimClass{parent   = [{"SysM", _}],
		   key      = {Name, Key}}],
     _) ->
    {["ManagedElement=1", "SystemFunctions=1", "SysM=1"],
     Key,
     Name};

i2mu([#gmfMimClass{parent   = ParentList,
		   key      = {Name, Key}}],
     ClassId) ->
    %% Check if ManagedElement is a parent if several parents exist
    case lists:keyfind("ManagedElement", 1, ParentList) of
	false ->
	    throw({?MODULE, {error, ClassId}});
	_ ->
	    {["ManagedElement=1"], Key, Name}
    end;

i2mu(_, ClassId) ->
    %% zero or multiple #gmfMimClass{} records were passed
    throw({?MODULE, {error, ClassId}}).


%%% ----------------------------------------------------------
%%% @doc Converts the given list of IMM DN fragments to a list
%%% of RDNs in MIM style:
%%% ```
%%% [<<"17">>, <<"fooId">>, <<"33">>, <<"barId">>] -> ["Foo=17", "Bar=33"]
%%% '''
%%% Both lists are in top-down order, but the input list has
%%% RDN names and values flipped.
%%%
%%% The translation relies on #gmfMimClass records for translating
%%% the IMM RDN names, which depend on the position in the MOM (a MOM
%%% name prefix may be present on the root instance of the MOM).
%%% @end
%%% ----------------------------------------------------------
-spec i2m_down([binary()], % IMM DN in top-down flipped order
	       string(),   % Parent class of the leading instance
	       string(),   % MIM of parent class
	       [string()]  % Result accumulator in reverse order
	      ) -> [string()].
i2m_down([Ins, ClassId | T], Parent, MimName, Acc) ->
    ClassIdL = binary_to_list(ClassId),
    Objs = mnesia:index_read(gmfMimClass, ClassIdL, #gmfMimClass.imm_rdn),
    {Key, NewMimName} = i2md(Objs, {Parent, MimName}, ClassIdL),
    New = Key ++ "=" ++ binary_to_list(Ins),
    i2m_down(T, Key, NewMimName, [New | Acc]);

i2m_down([], _, _, Acc) ->
    lists:reverse(Acc).


%%% ----------------------------------------------------------
%%% @doc Selects the first #gmfMimClass{} record for which the
%%% specified {Parent, MimName} is one of the defined parents
%%% of the #gmfMimClass{} record. A {Class, MimName} tuple taken
%%% from this record is returned.
%%%
%%% It is considered a failure if no such record exists.
%%%
%%% The third argument is used as information only (it is the
%%% RDN name of the offending instance).
%%% @end
%%% ----------------------------------------------------------
-spec i2md([#gmfMimClass{}],
	   {string(), string()},
	   string()) -> {string(), string()}.
i2md([#gmfMimClass{key      = {Name, Key},
		   parent   = Parents} | T],
     {Parent, MimName},
     ClassId) ->
    case lists:member({Parent, MimName}, Parents) of
	true ->
	    {Key, Name};
	false ->
	    i2md(T, {Parent, MimName}, ClassId)
    end;

i2md([], _, ClassId) ->
    throw({?MODULE, {error, ClassId}}).

%%% ----------------------------------------------------------
%%% @doc Retrieves MIM class name and MIM name for the
%%% leaf instance defined by the given IMM-style DN. The
%%% search pattern was copied from imm_to_mim/1.
%%% @end
%%% ----------------------------------------------------------
-spec imm_to_mim_details(binary()) ->
				{ok, {string(), string()}} | {error, any()}.

imm_to_mim_details(DnImmB) ->
    DnImmRawS  = binary_to_list(DnImmB),
    DnImmS = string:strip(DnImmRawS, right, 0),
    Res = mnesia:ets(fun() -> imm_to_mim_details2(DnImmS) end),
    Res.

imm_to_mim_details2(DnImmS) ->
    try
	TokensB = lists:reverse(split_dn(DnImmS)),
	%% [<<"1">>,<<"NOBLEheliumId">>, <<"1">>,<<"neonId">>, <<"1">>,<<"argonId">>]

	{_Ins, TopClass, DnT} = i2m(TokensB),
	%% {<<"1">>, <<"NOBLEheliumId">>, [<<"1">>,<<"neonId">>, <<"1">>,<<"argonId">>]}

	{_UpperPart, TopClassS, MimName} = i2m_up(TopClass),
	if
	    DnT =:= [] ->
		{ok, {TopClassS, MimName}};
	    true ->
		{ok, i2m_down_details(DnT, TopClassS, MimName, [])}
	end
    catch
	throw:{?MODULE, Error} ->
	    %% trust that anything caught here is {error, _}.
	    Error;
	error:_ ->
	    {error, DnImmS}
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% TODO, refactor, use fewer arguments
%%% @end
%%% ----------------------------------------------------------
i2m_down_details([_Ins, ClassId | T], Parent, MimName, _) ->
    ClassIdL = binary_to_list(ClassId),
    Objs = mnesia:index_read(gmfMimClass, ClassIdL, #gmfMimClass.imm_rdn),
    {Key, NewMimName} = i2md_details(Objs, {Parent, MimName}, ClassIdL),
    i2m_down_details(T, Key, NewMimName, {Key, NewMimName});

i2m_down_details([], _, _, R) ->
    R.


-spec i2md_details([#gmfMimClass{}],
		   {string(), string()},
		   string()) -> {string(), string()}.

i2md_details([#gmfMimClass{key={Name, Key}, parent=Parents}|T],
	     {Parent, MimName},
	     ClassId) ->
    case lists:member({Parent, MimName}, Parents) of
	true ->
	    {Key, Name};
	false ->
	    i2md_details(T, {Parent, MimName}, ClassId)
    end;
i2md_details([], _, ClassId) ->
    throw({?MODULE, {error, ClassId}}).

%%===============================================================
%% Misc functions
%%===============================================================

%%% ----------------------------------------------------------
%%% @doc Converts the given DN-like string to a flat list of
%%% binarized tokens delimited by ',' and '='. Examples:
%%% ```
%%%     "a=1,b=2"  -> [<<"a">>, <<"1">>, <<"b">>, <<"2">>]
%%%     "a=1"      -> [<<"a">>, <<"1">>]
%%%     ""         -> []
%%% '''
%%% @end
%%% ----------------------------------------------------------
-spec split_dn(string()) -> [binary()].

split_dn(Dn) ->
    L = lists:append([string:tokens(S, "=") || S <- string:tokens(Dn, ",")]),
    [list_to_binary(T) || T <- L].

reply_is_moclass_struct(ImmClass, Socket) ->
    {Result, IsMoClassStruct} = is_moclass_struct(ImmClass),
    Reply = <<Result:4/native-unsigned-integer-unit:8,
	      IsMoClassStruct:4/native-integer-unit:8>>,
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

is_moclass_struct(ImmClass) ->
    ClassL = binary_to_list(ImmClass),
    F = fun() ->
		case mnesia:read(gmfImmClass, ClassL) of
		    [] ->
			{?CMSI_CLASS_NOT_FOUND, 0};
		    [#gmfImmClass{gmfMimClass = undefined}] ->
			{?CMSI_OK, 1};
		    [#gmfImmClass{}] ->
			{?CMSI_OK, 0};
		    _ ->
			{?CMSI_CLASS_NOT_FOUND, 0}
		end
	end,
    ?GMFLOG({?MODULE, is_moclass_struct, ClassL}),
    Res = mnesia:ets(F),
    ?GMFLOG({?MODULE, is_moclass_struct_res, Res}),
    Res.

reply_is_attr_ref_to_struct(ImmClass, AttrName, Socket) ->
    {Result, IsAttrRef2Struct} = is_attr_ref_to_struct(ImmClass, AttrName),
    Reply = <<Result:4/native-unsigned-integer-unit:8,
	      IsAttrRef2Struct:4/native-integer-unit:8>>,
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

is_attr_ref_to_struct(ImmClass, AttrName) ->
    ImmClassL = binary_to_list(ImmClass),
    F = fun() ->
		case mnesia:read(gmfImmClass, ImmClassL) of
		    [#gmfImmClass{attributes = Attr,
				  gmfMimClass = MimClass}] ->
			AttrNameL = binary_to_list(AttrName),
			case lists:keyfind(AttrNameL, 1, Attr) of
			    false ->
				{?CMSI_ATTRIBUTE_NOT_FOUND, 0};
			    _ ->
				is_attr_ref_to_struct2(MimClass, AttrNameL)
			end;
		    _ ->
			{?CMSI_CLASS_NOT_FOUND, 0}
		end
	end,

    ?GMFLOG({?MODULE, is_attr_ref_to_struct, {ImmClass, AttrName}}),
    Res = mnesia:ets(F),
    ?GMFLOG({?MODULE, is_attr_ref_to_struct_res, Res}),
    Res.

is_attr_ref_to_struct2(MimClass, AttrName) ->
    case mnesia:read(gmfMimClass, MimClass) of
	[] ->
	    {?CMSI_OK, 0};
	[Mim] ->
	    Props = proplists:get_value(AttrName, Mim#gmfMimClass.attributes),

	    case gmfSALib:get_com_basic_type(Props) of
		{{structRef, _, _}, _} ->
		    {?CMSI_OK, 1};
		_ ->
		    {?CMSI_OK, 0}
	    end
    end.

reply_get_struct_ref_for_attr(ImmClass, AttrName, Socket) ->
    {Result, StructRef} = get_struct_ref_for_attr(ImmClass, AttrName),
    Reply = [<<Result:4/native-unsigned-integer-unit:8>>,
	     StructRef,0],
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

get_struct_ref_for_attr(ImmClass, AttrName) ->
    ImmClassL = binary_to_list(ImmClass),
    F = fun() ->
		case mnesia:read(gmfImmClass, ImmClassL) of
		    [] ->
			{?CMSI_CLASS_NOT_FOUND, ""};
		    [#gmfImmClass{attributes = Attr,
				  gmfMimClass = MimClass}] ->
			AttrNameL = binary_to_list(AttrName),
			case lists:keyfind(AttrNameL, 1, Attr) of
			    false ->
				{?CMSI_ATTRIBUTE_NOT_FOUND, ""};
			    _ ->
				get_struct_ref_for_attr2(MimClass, AttrNameL)
			end;
		    _ ->
			{?CMSI_CLASS_NOT_FOUND, ""}
		end
	end,

    ?GMFLOG({?MODULE, get_struct_ref_for_attr, {ImmClass, AttrName}}),
    Res = mnesia:ets(F),
    ?GMFLOG({?MODULE, get_struct_ref_for_attr_res, Res}),
    Res.

get_struct_ref_for_attr2(MimClass, AttrNameL) ->
    case get_imm_struct_name(mnesia:read(gmfMimClass, MimClass), AttrNameL) of
	{ok, ImmStructName} ->
	    {?CMSI_OK, ImmStructName};
	error ->
	    {?CMSI_ATTRIBUTE_NO_STRUCT_REF, ""}
    end.

get_imm_struct_name([Obj], AttrNameL) ->
    Props = proplists:get_value(AttrNameL, Obj#gmfMimClass.attributes),

    case gmfSALib:get_com_basic_type(Props) of
	{{structRef, StructName, MomName}, _} ->
	    StructKey = {MomName, StructName},
	    [#gmfMimStruct{imm_ns = ImmNs}] =
		mnesia:read(gmfMimStruct, StructKey),
	    %% Dirty operation. OK OK OK for the timebeing
	    ImmStructName = ImmNs ++ StructName,
	    {ok, ImmStructName};
	_ ->
	    error
    end;
get_imm_struct_name(_, _AttrNameL) ->
    error.

reply_get_integer(String, Socket) ->
    {Result, Integer} = handle_get_integer(String),
    Reply = <<Result:4/native-unsigned-integer-unit:8,
	      Integer:4/native-integer-unit:8>>,
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

handle_get_integer(String) ->
    F = fun() -> {?CMSI_OK, handle_get_integer2(String)} end,
    ?GMFLOG({?MODULE, get_integer, {String}}),
    %% Must be a transaction
    {atomic, Res} = mnesia:transaction(F),
    ?GMFLOG({?MODULE, get_integer, Res}),
    Res.

handle_get_integer2(String) ->
    handle_get_integer3(mnesia:index_read(gmfTrData, String, #gmfTrData.value),
			String).

handle_get_integer3([#gmfTrData{key = K}], _) when is_integer(K) ->
    K;
handle_get_integer3([], String) ->
    K = update_counter(),
    ok = mnesia:write(#gmfTrData{key = K, value = String}),
    K.

update_counter() ->
    TmpCnt = mnesia:dirty_update_counter(gmfTrData, counter, 1),
    update_counter(TmpCnt).

update_counter(Cnt) when Cnt >= ?CMSI_RESERVED_OBJECTIDS_START,
			 Cnt =< ?CMSI_RESERVED_OBJECTIDS_END ->
    %% Numbers between ?CMSI_RESERVED_OBJECTIDS_START and
    %% ?CMSI_RESERVED_OBJECTIDS_END are reserved
    NewCnt = ?CMSI_RESERVED_OBJECTIDS_END+1,
    ok = mnesia:write(#gmfTrData{key = counter, value = NewCnt}),
    NewCnt;
update_counter(Cnt) when Cnt > ?OBJECTIDS_END ->
    %% Larger than ?OBJECTIDS_END. Start over with ?OBJECTIDS_START
    %% This will probable never occur
    NewCnt = ?OBJECTIDS_START,
    ok = mnesia:write(#gmfTrData{key = counter, value = NewCnt}),
    NewCnt;
update_counter(Cnt) ->
    Cnt.

reply_get_integers(InData, Socket) ->
    {Result, OutData} = handle_get_integers(InData),
    Reply = [<<Result:4/native-unsigned-integer-unit:8>>, OutData],
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

handle_get_integers(InData) ->
    F = fun() -> {?CMSI_OK, handle_get_integers2(InData)} end,
    ?GMFLOG({?MODULE, get_integers, {InData}}),
    {atomic, Res} = mnesia:transaction(F),
    ?GMFLOG({?MODULE, get_integers, Res}),
    Res.

handle_get_integers2(<<Length:4/native-integer-unit:8,
		       String:Length/binary,
		       Rest/binary>>) ->
    Integer = handle_get_integer2(String),
    [<<Integer:4/native-integer-unit:8>> | handle_get_integers2(Rest)];
handle_get_integers2(<<>>) ->
    [].

handle_check_string(Bin) ->

    F = fun() ->
		case mnesia:match_object(#gmfTrData{value = Bin, _ = '_'}) of
		    [#gmfTrData{key = Key}] -> {ok, Key};
		    _                       -> {error, not_found}
		end
	end,
    ?GMFLOG({?MODULE, check_string, {Bin}}),
    {atomic, Res} = mnesia:transaction(F),
    ?GMFLOG({?MODULE, check_string, Res}),
    Res.

reply_get_string(Integer, Socket) ->
    {Result, String} = handle_get_string(Integer),
    Reply = [<<Result:4/native-unsigned-integer-unit:8>>, String, 0],
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

handle_get_string(Integer) ->
    ?GMFLOG({?MODULE, get_string, {Integer}}),
    hgs(mnesia:dirty_read(gmfTrData, Integer)).

hgs([]) ->
    Res = {?CMSI_LDN_NOT_FOUND, <<>>},
    ?GMFLOG({?MODULE, get_string, Res}),
    Res;
hgs([#gmfTrData{value = String}]) ->
    Res = {?CMSI_OK, String},
    ?GMFLOG({?MODULE, get_string, Res}),
    Res.



%% handle_get_string(Integer) ->
%%     F = fun() ->
%% 		case handle_get_string2(Integer) of
%% 		    <<>> ->
%% 			{?CMSI_LDN_NOT_FOUND, <<>>};
%% 		    String ->
%% 			{?CMSI_OK, String}
%% 		end
%% 	end,
%%     ?GMFLOG({?MODULE, get_string, {Integer}}),
%%     {atomic, Res} = mnesia:transaction(F),
%%     ?GMFLOG({?MODULE, get_string, Res}),
%%     Res.

handle_get_string2(Integer) ->
    case mnesia:read(gmfTrData, Integer) of
	[] ->
	    <<>>;
	[#gmfTrData{value = V}] when is_binary(V) ->
	    V
    end.

reply_get_strings(InData, Socket) ->
    {Result, OutData} = handle_get_strings(InData),
    Reply = [<<Result:4/native-unsigned-integer-unit:8>>, OutData],
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

handle_get_strings(InData) ->
    F = fun() -> {?CMSI_OK, handle_get_strings2(InData)} end,
    ?GMFLOG({?MODULE, get_strings, {InData}}),
    {atomic, Res} = mnesia:transaction(F),
    ?GMFLOG({?MODULE, get_strings, Res}),
    Res.

handle_get_strings2(<<Integer:4/native-integer-unit:8,
		      Rest/binary>>) ->
    String = handle_get_string2(Integer),

    case byte_size(String) of
	0 ->
	    [<<0:4/native-integer-unit:8>> | handle_get_strings2(Rest)];
	Length ->
	    [<<(Length+1):4/native-integer-unit:8>>, String, 0 |
	     handle_get_strings2(Rest)]
    end;
handle_get_strings2(<<>>) ->
    [].

reply_get_network_managed_element_id(Socket) ->
    {Result, OutData} = handle_get_network_managed_element_id(),
    Reply = [<<Result:4/native-unsigned-integer-unit:8>>, OutData],
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

handle_get_network_managed_element_id() ->
    Key = networkManagedElementId,
    Default = "1",
    NMEId = get_comTop_data(Key, Default),

    {?CMSI_OK, [NMEId, 0]}.

reply_get_me_user_label(Socket) ->
    {Result, OutData} = handle_get_me_user_label(),
    Reply = [<<Result:4/native-unsigned-integer-unit:8>>, OutData],
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

handle_get_me_user_label() ->
    Key = userLabel,
    Default = "",
    UserLabel = get_comTop_data(Key, Default),

    {?CMSI_OK, [UserLabel, 0]}.

reply_get_dn_prefix(Socket) ->
    {Result, OutData} = handle_get_dn_prefix(),
    Reply = [<<Result:4/native-unsigned-integer-unit:8>>, OutData],
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.
handle_get_dn_prefix() ->
    Key = dnPrefix,
    Default = "",
    DnPrefix = get_comTop_data(Key, Default),

    {?CMSI_OK, [DnPrefix, 0]}.

reply_get_adm_op_id(Socket, ImmClass, ActionName) ->
    {Result, AdmOpId} = get_adm_oper_id(ImmClass, ActionName),
    Reply = <<Result:4/native-unsigned-integer-unit:8,
	      AdmOpId:8/native-integer-unit:8>>,
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

get_adm_oper_id(ImmClass, ActionName) ->
    ImmClassL = binary_to_list(ImmClass),
    F = fun() ->
		get_adm_oper_id2(mnesia:read(gmfImmClass, ImmClassL),
				 ActionName)
	end,

    ?GMFLOG({?MODULE, get_adm_oper_id, {ImmClass, ActionName}}),
    Res = mnesia:ets(F),
    ?GMFLOG({?MODULE, get_adm_oper_id, Res}),
    Res.

get_adm_oper_id2([#gmfImmClass{gmfMimClass = MimClass}], ActionName) ->
    Actions = read_actions(MimClass),
    ActionData = proplists:get_value(binary_to_list(ActionName), Actions),
    get_adm_oper_id3(ActionData);
get_adm_oper_id2(_, _) ->
    {?CMSI_CLASS_NOT_FOUND, 0}.

get_adm_oper_id3(undefined) ->
    {?CMSI_ACTION_NOT_FOUND, 0};
get_adm_oper_id3(ActionData) ->
    AdmOpIdStr = proplists:get_value("admOpId", ActionData),
    {?CMSI_OK, list_to_integer(AdmOpIdStr)}.

reply_get_action_name(Socket, ImmClass, AdmOpId) ->
    {Result, ActionName} = get_action_name(ImmClass, AdmOpId),
    Reply = [<<Result:4/native-unsigned-integer-unit:8>>, ActionName, 0],
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

get_action_name(ImmClass, AdmOpId) ->
    ImmClassL = binary_to_list(ImmClass),
    F = fun() ->
		get_action_name2(mnesia:read(gmfImmClass, ImmClassL), AdmOpId)
	end,

    ?GMFLOG({?MODULE, get_action_name, {ImmClass, AdmOpId}}),
    Res = mnesia:ets(F),
    ?GMFLOG({?MODULE, get_action_name, Res}),
    Res.

get_action_name2([#gmfImmClass{gmfMimClass = MimClass}], AdmOpId) ->
    Actions = read_actions(MimClass),
    AdmOpIdStr = integer_to_list(AdmOpId),
    get_action_name3(AdmOpIdStr, Actions);
get_action_name2(_, _) ->
    {?CMSI_CLASS_NOT_FOUND, 0}.

get_action_name3(_AdmOpIdStr, []) ->
    {?CMSI_ACTION_NOT_FOUND, ""};
get_action_name3(AdmOpIdStr, [{ActionName, ActionData}|T]) ->
    case proplists:get_value("admOpId", ActionData) of
	AdmOpIdStr ->
	    {?CMSI_OK, list_to_binary(ActionName)};
	_ ->
	    get_action_name3(AdmOpIdStr, T)
    end.

read_actions(MimClass) ->
    case mnesia:read(gmfMimClass, MimClass) of
	[]    -> [];
	[Mim] -> Mim#gmfMimClass.actions
    end.

-spec get_oap_namespace() -> iodata().
get_oap_namespace() ->
    get_oap_namespace(ootI:get_oap_namespace()).

-spec get_oap_namespace(term()) -> iodata().
get_oap_namespace({ok, B})             -> get_oap_namespace(B);
get_oap_namespace(<<>>)                -> <<>>;
get_oap_namespace(B) when is_binary(B) -> [<<"/var/run/netns/">>, B];
get_oap_namespace(Ns)                  ->
    UsedNs = <<>>,
    sysInitI:warning_report([{mfa, {?MODULE, ?FUNCTION_NAME, [Ns]}},
			     {used_ns_value, UsedNs}
			    ]),
    UsedNs.

-spec get_oap_ip_addr() -> {integer(), boolean()}.
get_oap_ip_addr() ->
    get_oap_ip_addr(ootI:get_oap_ip_addr()).

-spec get_oap_ip_addr(list()) -> {integer(), boolean()}.
get_oap_ip_addr("") ->
    {0, true};
get_oap_ip_addr(IpAddress) when is_list(IpAddress) ->
    goia(inet:parse_address(IpAddress)).

goia({ok, {A, B, C, D}}) ->
    %% IPv4 address
    <<Ip:32>> = <<A:8, B:8, C:8, D:8>>,
    {Ip, true};
goia({ok, {A, B, C, D, E, F, G, H}}) ->
    %% IPv6 address
    <<Ip:128>> = <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>,
    {Ip, false}.

-spec get_oap_dscp() -> integer().
get_oap_dscp() ->
    Dscp = ootI:get_oap_dscp(),
    get_oap_dscp(Dscp).

-spec get_oap_dscp(term()) -> integer().
get_oap_dscp(Dscp) when is_integer(Dscp) ->
    Dscp;
get_oap_dscp(Dscp) ->
    UsedDscp = 0,
    sysInitI:warning_report([{mfa, {?MODULE, ?FUNCTION_NAME, [Dscp]}},
			     {used_dscp_value, UsedDscp}
			    ]),
    UsedDscp.

handle_get_oap_data(#state{oapIPv4 = true} = S) ->
    {S#state.oapNs, S#state.oapIA, S#state.oapDscp};
handle_get_oap_data(#state{oapIPv4 = false} = S) ->
    {S#state.oapNs, 0, S#state.oapDscp}.

reply_get_oap_data(Socket, S) ->
    {Ns, IA, Dscp} = handle_get_oap_data(S),
    Result = ?CMSI_OK,
    Reply = [<<Result:4/native-unsigned-integer-unit:8,
	       IA:4/native-integer-unit:8,
	       Dscp:4/native-integer-unit:8>>, Ns, 0],
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

handle_get_oap_data2(S) ->
    {S#state.oapNs, S#state.oapIA, S#state.oapDscp, S#state.oapIPv4}.

reply_get_oap_data2(Socket, S) ->
    {Ns, IA, Dscp, IPv4} = handle_get_oap_data2(S),
    Length = case IPv4 of
		 true  ->  4;
		 false -> 16
	     end,
    Result = ?CMSI_OK,
    Reply = [<<Result:4/native-unsigned-integer-unit:8,
	       Length:4/native-unsigned-integer-unit:8,
	       IA:Length/big-integer-unit:8,
	       Dscp:4/native-integer-unit:8>>, Ns, 0],
    gen_tcp:send(Socket, Reply),
    ok.

reply_get_imm_class(Socket, ImmDn) ->
    {Result, ImmClass} = get_imm_class(ImmDn),
    Reply = [<<Result:4/native-unsigned-integer-unit:8>>,
	     ImmClass, 0],
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

get_imm_class(ImmDn) ->
    F = fun() ->
		try
		    ImmL = binary_to_list(ImmDn),
		    Dn = lists:reverse(split_dn(ImmL)),
		    {_Ins, ClassId, DnT} = i2m(Dn),
		    {_Up, Key, MimName} = i2m_up(ClassId),
		    get_imm_class2(DnT, Key, MimName, ClassId)
		catch throw:{?MODULE, _} ->
			{?CMSI_OBJECT_CLASS_NOT_FOUND, ""};
		      error:_ ->
			{?CMSI_OBJECT_CLASS_NOT_FOUND, ""}
		end
	end,
    mnesia:ets(F).

get_imm_class2([], _Parent, MimName, ClassId ) ->
    ClassIdL = binary_to_list(ClassId),
    Objs = mnesia:index_read(gmfMimClass, ClassIdL, #gmfMimClass.imm_rdn),
    get_imm_class3(Objs, MimName);
get_imm_class2([Value, <<"id">>], _Parent, _MimName, ClassId) ->
    ClassIdL = binary_to_list(ClassId),
    AttrNameL = re:replace(Value, "(_[0-9]+)+$", "", [{return,list}]),
    Objs = mnesia:index_read(gmfMimClass, ClassIdL, #gmfMimClass.imm_rdn),

    case get_imm_struct_name(Objs, AttrNameL) of
	{ok, ImmStructName} ->
	    {?CMSI_OK, ImmStructName};
	error ->
	    {?CMSI_OBJECT_CLASS_NOT_FOUND, ""}
    end;
get_imm_class2([_Ins, ClassId | T], Parent, MimName, _ClassId) ->
    ClassIdL = binary_to_list(ClassId),
    Objs = mnesia:index_read(gmfMimClass, ClassIdL, #gmfMimClass.imm_rdn),
    {NewParent, NewMimName} = i2md(Objs, {Parent, MimName}, ClassIdL),
    get_imm_class2(T, NewParent, NewMimName, ClassId).

get_imm_class3([#gmfMimClass{key    = {MimName, ClassName},
			     imm_ns = ImmNs}|_], MimName) ->
    {?CMSI_OK, ImmNs ++ ClassName};
get_imm_class3([_|T], MimName) ->
    get_imm_class3(T, MimName);
get_imm_class3([], _MimName) ->
    {?CMSI_OBJECT_CLASS_NOT_FOUND, ""}.

get_object_id(EcimDn) ->
    handle_get_integer2(EcimDn).

get_ecim_dn(ImmDn) ->
    imm_to_mim2(ImmDn).

reply_get_bidir_assoc(Socket, ImmClass) ->
    {Result, L} = get_bidir_assoc(ImmClass),
    MsgList =
	case L of
	    [] -> [];
	    [Rec] ->
		[get_protoBiDirectionalAssociation(L2) ||
		    L2 <- Rec#gmfMetaBiDir.bidir_list]
	end,
    Msg = #protoBidirRet{result = Result, biDirectionalAssoc = MsgList},
    Reply = cmsi:encode_msg(Msg),
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    ok.

get_bidir_assoc(ImmClass) ->
    F = fun() -> get_bidir_assoc2(ImmClass) end,
    ?GMFLOG({?MODULE, get_bidir_assoc, {immClass, ImmClass}}),
    BiDirRec = mnesia:ets(F),
    ?GMFLOG({?MODULE, get_bidir_assoc, {biDirList, BiDirRec}}),
    BiDirRec.

get_bidir_assoc2(ImmClass) ->
    ClassL = binary_to_list(ImmClass),
    case mnesia:read(gmfImmClass, ClassL) of
	[#gmfImmClass{}] ->
	    L = mnesia:index_read(gmfMetaBiDir, ClassL,
				  #gmfMetaBiDir.imm_class),
	    {?CMSI_OK, L};
	_ ->
	    {?CMSI_CLASS_NOT_FOUND, []}
    end.

get_protoBiDirectionalAssociation(L) ->
    L1 = proplists:get_value(reservingAssocEnd, L),
    L2 = proplists:get_value(reservedAssocEnd, L),
    #protoBiDirectionalAssociation{reservingAssociationEnd = get_protoAssociationEnd(L1),
				   reservedAssociationEnd  = get_protoAssociationEnd(L2)}.

get_protoAssociationEnd(L) ->
    IsScoped = proplists:get_bool(isScoped, L),
    #protoAssociationEnd{name        = proplists:get_value(name, L),
			 hasClass    = get_protoHasClass(L, IsScoped),
			 cardinality = get_protoCardinality(L),
			 isReserving = proplists:get_bool(isReserving, L)}.

get_protoHasClass(L, IsScoped) ->
    {Name, MimName} = proplists:get_value(hasClass, L),
    #protoHasClass{name     = Name,
		   mimName  = MimName,
		   isScoped = IsScoped}.

get_protoCardinality(L) ->
    case proplists:get_value(cardinality, L) of
	undefined -> undefined;
	L1        -> #protoCardinality{min = proplists:get_value(min, L1),
				       max = proplists:get_value(max, L1)}
    end.

get_comTop_data(Key, Default) ->
    Data = comTop:get_managed_element_data(),
    case proplists:get_value(Key, Data) of
	undefined -> Default;
	V         -> V
    end.

send_oap_data(State, State) ->
    ok;
send_oap_data(NewState, _) ->
    [reply_get_oap_data2(Socket, NewState) ||
	Socket <- NewState#state.oapSubscribers].
