%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	appmPgh.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R8A/R9A/R10A/R11A/R12A/2

%%% @doc ==Encoding/decoding of PGH messages==
%%% This module contains all the functionality for encoding and decoding
%%% messages to or from the program handler.
-module(appmPgh).
-vsn('/main/R2A/R3A/R4A/R5A/R8A/R9A/R10A/R11A/R12A/2').
-date('2017-11-09').
-author('etxarnu').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
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
%%% -----      -------    --------    ------------------------
%%% R2A/1      2013-02-28 etxpeno     Created
%%% R2A/9      2013-09-26 etxarnu     Updated for new PGH interface
%%% R2A/11     2014-01-10 etxarnu     added signalpgm
%%% R3A/3      2015-01-13 etxarnu     added spawnpgmex to handle ns
%%% R3A/4      2015-01-14 etxarnu     Handle  ns == <<>>
%%% R4A/1      2015-06-23 etxarnu     added warmrestart, softrt
%%% R5A/1      2015-11-20 etxarnu     Corrected encode_body(restartbrd,..
%%% R5A/2      2016-02-16 etxarnu     Added flag byte to spawnpgmex
%%% R5A/3      2016-03-09 etxarnu     Added getpgmpid
%%% R8A/1      2016-12-15 erarafo     Fix compiler warning
%%% R9A/1      2017-03-23 etxarnu     Added writellog
%%% R10A/1     2017-05-17 etxarnu     Added Reason to pgmterm
%%% R10A/2     2017-05-31 etxarnu     Changed pgmterm w. Reason to pgmtermex
%%%                                   Added _Rest/binary to all received msgs
%%%                                   to be able to handle future additions to msgs
%%% R11A/1     2017-10-02 etxarnu     Added PGMTYPE field to spawnpgmex
%%% R12A/1    2017-11-09 etxarnu     Removed all group related functions
%%% R12A/2    2017-11-09 etxarnu     Added back encode_rank(pgm_grp)
%%% ----------------------------------------------------------

%%% ----------------------------
%%% EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------
-export([encode_req/3, decode_rsp/1]).
-export([decode_cpuset/1]).

%%% ---------------------------
%%% EXPORTED INTERNAL FUNCTIONS
%%% ---------------------------

-include("appmPgh.hrl").

%%% -------
%%% DEFINES
%%% -------

-define(PGH_IF_ERR_SUCCESS,    0).
-define(PGH_IF_ERR_OTHER,      1).
-define(PGH_IF_ERR_INVALFRM,   2).
-define(PGH_IF_ERR_INVALPARAM, 3).
-define(PGH_IF_ERR_TOOMANY,    4).
-define(PGH_IF_ERR_UNKNOWN,    5).
-define(PGH_IF_ERR_NOMEM,      6).
-define(PGH_IF_ERR_ALREADY,    7).

-define(PGH_IF_CAP_CPUSET, 16#1).
-define(PGH_IF_CAP_MEMORY, 16#2).
-define(PGH_IF_CAP_CPUACC, 16#4).
-define(PGH_IF_CAP_PGMCAP, 16#10).

-define(PGH_IF_MSG_INIT,            0).
-define(PGH_IF_MSG_SETDEFCPUSET,    1).
-define(PGH_IF_MSG_SIGNALPGM,       3).
-define(PGH_IF_MSG_GETPGMRSCUSAGE,  9).

-define(PGH_IF_MSG_SPAWNPGM,        21).
-define(PGH_IF_MSG_DESTROYPGM,      22).
-define(PGH_IF_MSG_SPAWNPGM_EX,     26).
-define(PGH_IF_MSG_GETPGMPID,       27).

-define(PGH_IF_MSG_RESTARTBRD, 50).
-define(PGH_IF_MSG_WARMRESTART, 51).

-define(PGH_IF_MSG_LLOG, 60).

-define(PGH_IF_MSG_PGMCRASH, 100).
-define(PGH_IF_MSG_PGMTERM,  101).
-define(PGH_IF_MSG_PGMTERMEX,  102).

-define(PGH_IF_PGMCAP_NICE, 16#1).
-define(PGH_IF_PGMCAP_NET,  16#2).
-define(PGH_IF_PGMCAP_RT,   16#4).
-define(PGH_IF_PGMCAP_SYS,  16#8).
-define(PGH_IF_PGMCAP_SOFTRT,   16#10).

-define(PGH_IF_RANK_UNSPEC,  0). % unspecified
-define(PGH_IF_RANK_PGM,     1).
-define(PGH_IF_RANK_PGM_GRP, 2).
-define(PGH_IF_RANK_WARM,    3).
-define(PGH_IF_RANK_COLD,    4).
-define(PGH_IF_RANK_COLDWT,  5). % cold with test

-define(PGH_IF_PGMTYPE_DEFAULT,  0). % Default
-define(PGH_IF_PGMTYPE_DP,       1). % Data Processing

%%% -----
%%% TYPES
%%% -----


%%% -------------------------------------
%%% CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% -------------------------------------

%%% @doc Encodes a request to the program handler
-spec encode_req(MsgType :: msg_type(),
		 Seq :: uint32(),
		 EncodeList :: [encode_option()]) -> iolist().
encode_req(MsgType, Seq, EncodeList) ->
    Body = encode_body(MsgType, EncodeList),
    BodyLength = iolist_size(Body),

    Header = encode_header(MsgType, BodyLength, Seq),

    [Header, Body].

%%% @doc Decodes a response from the program handler
-spec decode_rsp(binary()) -> [decode_option()].
decode_rsp(<<Type:4/native-unsigned-integer-unit:8,
	     _:4/native-unsigned-integer-unit:8,
	     Seq:4/native-unsigned-integer-unit:8,
	     _:4/native-unsigned-integer-unit:8,
	     Body/binary>>) ->

    MsgType = decode_msg_type(Type),
    [{msg_type, MsgType}, {seq, Seq}] ++ decode_body(MsgType, Body).

decode_cpuset(CpuSet) ->
    decode_cpus_conf(CpuSet).

%%% ------------------------------------
%%% CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ------------------------------------


%%% ---------------------------
%%% CODE FOR INTERNAL FUNCTIONS
%%% ---------------------------

encode_header(MsgType, Length, Seq) ->
    Reserved = 0,
    [encode_msg_type(MsgType),
     <<(Length+16):4/native-unsigned-integer-unit:8,
       Seq:4/native-unsigned-integer-unit:8,
       Reserved:4/native-unsigned-integer-unit:8>>].

encode_msg_type(init)            ->
    <<?PGH_IF_MSG_INIT:4/native-unsigned-integer-unit:8>>;
encode_msg_type(setdefcpuset)    ->
    <<?PGH_IF_MSG_SETDEFCPUSET:4/native-unsigned-integer-unit:8>>;
encode_msg_type(spawnpgm)        ->
    <<?PGH_IF_MSG_SPAWNPGM:4/native-unsigned-integer-unit:8>>;
encode_msg_type(spawnpgmex)        ->
    <<?PGH_IF_MSG_SPAWNPGM_EX:4/native-unsigned-integer-unit:8>>;
encode_msg_type(destroypgm)      ->
    <<?PGH_IF_MSG_DESTROYPGM:4/native-unsigned-integer-unit:8>>;
encode_msg_type(signalpgm)      ->
    <<?PGH_IF_MSG_SIGNALPGM:4/native-unsigned-integer-unit:8>>;
encode_msg_type(getpgmrscusage)  ->
    <<?PGH_IF_MSG_GETPGMRSCUSAGE:4/native-unsigned-integer-unit:8>>;
encode_msg_type(getpgmpid)  ->
    <<?PGH_IF_MSG_GETPGMPID:4/native-unsigned-integer-unit:8>>;
encode_msg_type(restartbrd) ->
    <<?PGH_IF_MSG_RESTARTBRD:4/native-unsigned-integer-unit:8>>;
encode_msg_type(warmrestart) ->
    <<?PGH_IF_MSG_WARMRESTART:4/native-unsigned-integer-unit:8>>;
encode_msg_type(writellog) ->
    <<?PGH_IF_MSG_LLOG:4/native-unsigned-integer-unit:8>>.

encode_body(init, EncodeList) ->
    Notify = proplists:get_value(notify, EncodeList, false),
    Version = proplists:get_value(version, EncodeList, 0),
    [encode_boolean(Notify),
     <<Version:4/native-unsigned-integer-unit:8>>];
encode_body(setdefcpuset, EncodeList) ->
    {cpus_def, CpusDef} = lists:keyfind(cpus_def, 1, EncodeList),

    encode_cpuSet(CpusDef);
encode_body(spawnpgm, EncodeList) ->
    Args = proplists:get_value(args, EncodeList, []),
    Envs = proplists:get_value(envs, EncodeList, []),
    Uid = proplists:get_value(uid, EncodeList, 0),
    Gid = proplists:get_value(gid, EncodeList, 0),
%    Ns = proplists:get_value(ns, EncodeList, ""),
    Capabilities = proplists:get_value(capabilities, EncodeList, []),
    CpuSet = proplists:get_value(cpu_set, EncodeList, 0),
    MemLimit = proplists:get_value(memory_limit, EncodeList, unlimited),

    Argc = length(Args),
    Envc = length(Envs),
    Argv = [[Arg, 0] || Arg <- Args],
    Envv = [[Env, 0] || Env <- Envs],

    [<<0:4/native-unsigned-integer-unit:8,
       0:4/native-unsigned-integer-unit:8,
       Argc:4/native-unsigned-integer-unit:8,
       Envc:4/native-unsigned-integer-unit:8,
       Uid:4/native-unsigned-integer-unit:8,
       Gid:4/native-unsigned-integer-unit:8>>,
     encode_pgmcap(Capabilities),
     <<CpuSet:4/native-unsigned-integer-unit:8>>,
     encode_memLimit(MemLimit),
     Argv,
     Envv];
encode_body(spawnpgmex, EncodeList) ->
    Args = proplists:get_value(args, EncodeList, []),
    Envs = proplists:get_value(envs, EncodeList, []),
    Uid = proplists:get_value(uid, EncodeList, 0),
    Gid = proplists:get_value(gid, EncodeList, 0),
    Ns = proplists:get_value(ns, EncodeList, ""),
    Flag = proplists:get_value(flag, EncodeList, 0),
    Type = proplists:get_value(pgmtype, EncodeList),
    Capabilities = proplists:get_value(capabilities, EncodeList, []),
    CpuSet = proplists:get_value(cpu_set, EncodeList, 0),
    MemLimit = proplists:get_value(memory_limit, EncodeList, unlimited),

    Argc = length(Args),
    Envc = length(Envs),
    Argv = [[Arg, 0] || Arg <- Args],
    Envv = [[Env, 0] || Env <- Envs],

    [<<0:4/native-unsigned-integer-unit:8,
       0:4/native-unsigned-integer-unit:8,
       Argc:4/native-unsigned-integer-unit:8,
       Envc:4/native-unsigned-integer-unit:8,
       Uid:4/native-unsigned-integer-unit:8,
       Gid:4/native-unsigned-integer-unit:8>>,
     encode_pgmcap(Capabilities),
     <<CpuSet:4/native-unsigned-integer-unit:8>>,
     encode_memLimit(MemLimit),
     encode_ns(Ns,Flag,Type),
     Argv,
     Envv];
encode_body(destroypgm, EncodeList) ->
    {pgm_id, PgmIdList} = lists:keyfind(pgm_id, 1, EncodeList),
    NoOfPgmId = length(PgmIdList),
    CoreDump = proplists:get_value(core_dump, EncodeList, false),
    PgIdL = [<<X:4/native-unsigned-integer-unit:8>> || X<-PgmIdList],
    [encode_boolean(CoreDump),
     <<NoOfPgmId:4/native-unsigned-integer-unit:8>>,
     PgIdL
    ];
encode_body(signalpgm, EncodeList) ->
    {pgm_id, PgmId} = lists:keyfind(pgm_id, 1, EncodeList),
    {sig_no, SigNo} = lists:keyfind(sig_no, 1, EncodeList),
    SigVal=0, %not used yet
    [<<PgmId:4/native-unsigned-integer-unit:8>>,
     <<SigNo:4/native-unsigned-integer-unit:8>>,
     <<SigVal:4/native-unsigned-integer-unit:8>>];
encode_body(getpgmrscusage, EncodeList) ->
    {pgm_id, PgmId} = lists:keyfind(pgm_id, 1, EncodeList),
    <<PgmId:4/native-unsigned-integer-unit:8>>;
encode_body(getpgmpid, EncodeList) ->
    {pgm_id, PgmId} = lists:keyfind(pgm_id, 1, EncodeList),
    <<PgmId:4/native-unsigned-integer-unit:8>>;
encode_body(restartbrd, EncodeList) ->
    HwTest = proplists:get_value(hw_test, EncodeList, false),
    Escalate = proplists:get_value(escalate, EncodeList, false),
    Reason = proplists:get_value(reason, EncodeList, ""),
    RStr = [Reason, 0],
    [encode_boolean(HwTest),
     encode_boolean(Escalate),
     RStr];
encode_body(warmrestart, EncodeList) ->
    Reason = proplists:get_value(reason, EncodeList, ""),
    RStr = [Reason, 0],
    [[<<0:4/native-unsigned-integer-unit:8>> || _<-lists:seq(0,7)],
     RStr];
encode_body(writellog, EncodeList) ->
    Rank = encode_rank(proplists:get_value(rank, EncodeList,unspecified)),
    Reason = proplists:get_value(reason, EncodeList, ""),
    RStr = [Reason, 0],
    Name = proplists:get_value(name, EncodeList, ""),
    NStr = [Name, 0],
    Extra = proplists:get_value(extra, EncodeList, ""),
    EStr = [Extra, 0],
    [ <<Rank:4/native-unsigned-integer-unit:8>>,
      [<<0:4/native-unsigned-integer-unit:8>> || _<-lists:seq(0,7)],
      RStr,
      NStr,
      EStr].


encode_boolean(true)  -> <<1:4/native-unsigned-integer-unit:8>>;
encode_boolean(false) -> <<0:4/native-unsigned-integer-unit:8>>.

encode_pgmcap(Capabilities) ->
    I = lists:foldl(
	  fun(nice, Acc) -> Acc bor ?PGH_IF_PGMCAP_NICE;
	     (rt, Acc)   -> Acc bor ?PGH_IF_PGMCAP_RT;
	     (softrt, Acc)   -> Acc bor ?PGH_IF_PGMCAP_SOFTRT;
	     (net,  Acc) -> Acc bor ?PGH_IF_PGMCAP_NET;
	     (sys,  Acc) -> Acc bor ?PGH_IF_PGMCAP_SYS
	  end, 0, Capabilities),

    <<I:4/native-unsigned-integer-unit:8>>.

encode_cpuSet(CpuSet)  ->
    I = lists:foldl(
	  fun(Cpu, Acc) when Cpu >= 0,
			     Cpu =< 31 ->
		  Acc bor (1 bsl Cpu)
	  end, 0, CpuSet),

    <<I:4/native-unsigned-integer-unit:8>>.

encode_memLimit(unlimited) -> <<0:8/native-unsigned-integer-unit:8>>;
encode_memLimit(MemLimit)  -> <<MemLimit:8/native-unsigned-integer-unit:8>>.

encode_ns(NsName,Flag,Type) when NsName == ""; NsName == <<>> ->
    [<<0:4/native-unsigned-integer-unit:8>> ] ++ encode_rest(Flag,Type);
encode_ns(NsName,Flag,Type) ->
    NsPath = "/run/netns/" ++ binary_to_list(NsName),
    [<<1:4/native-unsigned-integer-unit:8>> ] ++ encode_rest(Flag,Type) ++
    [NsPath, 0].

encode_rest(Flag,Type) ->
    T = encode_type(Type),
    [<<Flag:4/native-unsigned-integer-unit:8>>,<<T:4/native-unsigned-integer-unit:8>> |
     [<<0:4/native-unsigned-integer-unit:8>> || _<-lists:seq(0,5)]].

encode_type(dp) ->
    ?PGH_IF_PGMTYPE_DP;
encode_type(_) ->
    ?PGH_IF_PGMTYPE_DEFAULT.


encode_rank(unspecified)    -> ?PGH_IF_RANK_UNSPEC;
encode_rank(pgm)            -> ?PGH_IF_RANK_PGM;
encode_rank(pgm_grp)        -> ?PGH_IF_RANK_PGM_GRP;
encode_rank(warm)           -> ?PGH_IF_RANK_WARM;
encode_rank(cold)           -> ?PGH_IF_RANK_COLD;
encode_rank(cold_with_test) -> ?PGH_IF_RANK_COLDWT.


decode_msg_type(?PGH_IF_MSG_INIT)            -> init;
decode_msg_type(?PGH_IF_MSG_SETDEFCPUSET)    -> setdefcpuset;
decode_msg_type(?PGH_IF_MSG_SPAWNPGM)        -> spawnpgm;
decode_msg_type(?PGH_IF_MSG_SPAWNPGM_EX)     -> spawnpgmex;
decode_msg_type(?PGH_IF_MSG_DESTROYPGM)      -> destroypgm;
decode_msg_type(?PGH_IF_MSG_SIGNALPGM)       -> signalpgm;
decode_msg_type(?PGH_IF_MSG_GETPGMRSCUSAGE)  -> getpgmrscusage;
decode_msg_type(?PGH_IF_MSG_GETPGMPID)       -> getpgmpid;
decode_msg_type(?PGH_IF_MSG_RESTARTBRD)      -> restartbrd;
decode_msg_type(?PGH_IF_MSG_WARMRESTART)     -> warmrestart;
decode_msg_type(?PGH_IF_MSG_LLOG)            -> writellog;
decode_msg_type(?PGH_IF_MSG_PGMCRASH)        -> pgmcrash;
decode_msg_type(?PGH_IF_MSG_PGMTERM)         -> pgmterm;
decode_msg_type(?PGH_IF_MSG_PGMTERMEX)       -> pgmtermex.

decode_body(init, <<Result:4/native-unsigned-integer-unit:8,
		    Capabilities:4/native-unsigned-integer-unit:8,
		    CpusConf:4/native-unsigned-integer-unit:8,
		    _Rest/binary>>) ->
    [{result, decode_result(Result)},
     {capabilities, decode_capabilities(Capabilities)},
     {cpus_conf, decode_cpus_conf(CpusConf)}];
decode_body(setdefcpuset,
	    <<Result:4/native-unsigned-integer-unit:8,
	      _Rest/binary>>) ->
    [{result, decode_result(Result)}];
decode_body(spawnpgm,
	    <<Result:4/native-unsigned-integer-unit:8,
	      PgmId:4/native-unsigned-integer-unit:8,
	      _Rest/binary>>) ->
    [{result, decode_result(Result)},
     {pgm_id, PgmId}];
decode_body(spawnpgmex,
	    <<Result:4/native-unsigned-integer-unit:8,
	      PgmId:4/native-unsigned-integer-unit:8,
	      _Rest/binary>>) ->
    [{result, decode_result(Result)},
     {pgm_id, PgmId}];
decode_body(destroypgm,
	    <<Result:4/native-unsigned-integer-unit:8,
	      _Rest/binary>>) ->
    [{result, decode_result(Result)}];
decode_body(signalpgm,
	    <<Result:4/native-unsigned-integer-unit:8,
	      _Rest/binary>>) ->
    [{result, decode_result(Result)}];
decode_body(getpgmrscusage,
	    <<MemoryUsage:8/native-unsigned-integer-unit:8,
	      CpuUsage:8/native-unsigned-integer-unit:8,
	      Result:4/native-unsigned-integer-unit:8,
	      _Rest/binary>>) ->
    [{result, decode_result(Result)},
     {memory_usage, MemoryUsage},
     {cpu_usage, CpuUsage}];
decode_body(getpgmpid,
	    <<_PgmId:4/native-unsigned-integer-unit:8,
	      Result:4/native-unsigned-integer-unit:8,
	      NoOfPids:4/native-unsigned-integer-unit:8,
	      Pids/binary>>) ->
    [{result, decode_result(Result)},
     {pids, decode_pids(NoOfPids,Pids)}];
decode_body(restartbrd,
	    <<Result:4/native-unsigned-integer-unit:8,
	      _Rest/binary>>) ->
    [{result, decode_result(Result)}];
decode_body(warmrestart,
	    <<Result:4/native-unsigned-integer-unit:8, _Reserved/binary>>) ->
    [{result, decode_result(Result)}];
decode_body(writellog,
	    <<Result:4/native-unsigned-integer-unit:8, _Reserved/binary>>) ->
    [{result, decode_result(Result)}];
decode_body(pgmcrash, <<PgmId:4/native-unsigned-integer-unit:8,
			Rank:4/native-unsigned-integer-unit:8,
			_Rest/binary>>) ->
    [{pgm_id, PgmId},
     {rank, decode_rank(Rank)}];
decode_body(pgmterm, <<PgmId:4/native-unsigned-integer-unit:8,
	    _Rest/binary>>) ->
    [{pgm_id, PgmId}];
decode_body(pgmtermex,
	    <<PgmId:4/native-unsigned-integer-unit:8,
	      Reason:4/native-unsigned-integer-unit:8,
	    _Rest/binary>>) ->
    [{pgm_id, PgmId},
     {term_reason,Reason}].


decode_result(?PGH_IF_ERR_SUCCESS)    -> success;
decode_result(?PGH_IF_ERR_OTHER)      -> other;
decode_result(?PGH_IF_ERR_INVALFRM)   -> invalid_message_format;
decode_result(?PGH_IF_ERR_INVALPARAM) -> invalid_parameter;
decode_result(?PGH_IF_ERR_TOOMANY)    -> too_many_objects;
decode_result(?PGH_IF_ERR_UNKNOWN)    -> unknown_object;
decode_result(?PGH_IF_ERR_NOMEM)      -> out_of_memory;
decode_result(?PGH_IF_ERR_ALREADY)    -> warm_already_ongoing.

decode_capabilities(Capabilities) ->
    lists:foldl(
      fun(Capability, Acc) when Capabilities band Capability =:= Capability ->
	      [decode_capability(Capability) | Acc];
	 (_Capability, Acc) ->
	      Acc
      end, [], [?PGH_IF_CAP_CPUSET, ?PGH_IF_CAP_MEMORY,
		?PGH_IF_CAP_CPUACC, ?PGH_IF_CAP_PGMCAP]).

decode_capability(?PGH_IF_CAP_CPUSET) -> cpuset;
decode_capability(?PGH_IF_CAP_MEMORY) -> memory;
decode_capability(?PGH_IF_CAP_CPUACC) -> cpuacc;
decode_capability(?PGH_IF_CAP_PGMCAP) -> pgmcap.

decode_cpus_conf(CpusConf) ->
    decode_cpus_conf(CpusConf, []).

decode_cpus_conf(0, Acc) ->
    Acc;
decode_cpus_conf(Integer, Acc) ->
    NewInteger = Integer band (Integer-1),
    Diff = Integer band bnot NewInteger,
    decode_cpus_conf(NewInteger, [bit_position(Diff)|Acc]).


decode_rank(?PGH_IF_RANK_UNSPEC)  -> unspecified;
decode_rank(?PGH_IF_RANK_PGM)     -> pgm;
decode_rank(?PGH_IF_RANK_PGM_GRP) -> pgm_grp;
decode_rank(?PGH_IF_RANK_WARM)    -> warm;
decode_rank(?PGH_IF_RANK_COLD)    -> cold;
decode_rank(?PGH_IF_RANK_COLDWT)  -> cold_with_test.

bit_position(1 bsl 0)  -> 0;
bit_position(1 bsl 1)  -> 1;
bit_position(1 bsl 2)  -> 2;
bit_position(1 bsl 3)  -> 3;
bit_position(1 bsl 4)  -> 4;
bit_position(1 bsl 5)  -> 5;
bit_position(1 bsl 6)  -> 6;
bit_position(1 bsl 7)  -> 7;
bit_position(1 bsl 8)  -> 8;
bit_position(1 bsl 9)  -> 9;
bit_position(1 bsl 10) -> 10;
bit_position(1 bsl 11) -> 11;
bit_position(1 bsl 12) -> 12;
bit_position(1 bsl 13) -> 13;
bit_position(1 bsl 14) -> 14;
bit_position(1 bsl 15) -> 15;
bit_position(1 bsl 16) -> 16;
bit_position(1 bsl 17) -> 17;
bit_position(1 bsl 18) -> 18;
bit_position(1 bsl 19) -> 19;
bit_position(1 bsl 20) -> 20;
bit_position(1 bsl 21) -> 21;
bit_position(1 bsl 22) -> 22;
bit_position(1 bsl 23) -> 23;
bit_position(1 bsl 24) -> 24;
bit_position(1 bsl 25) -> 25;
bit_position(1 bsl 26) -> 26;
bit_position(1 bsl 27) -> 27;
bit_position(1 bsl 28) -> 28;
bit_position(1 bsl 29) -> 29;
bit_position(1 bsl 30) -> 30;
bit_position(1 bsl 31) -> 31.



decode_pids(No,Bin) ->
    decode_pids(No,Bin,[]).

decode_pids(0,_, Acc) ->
    Acc;
decode_pids(No,
	    <<Value:4/native-unsigned-integer-unit:8, Rest/binary>>,
	   Acc) ->
    decode_pids(No-1, Rest, [Value|Acc]).


    
