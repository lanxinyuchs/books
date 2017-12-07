%%% %CCaseFile:	cert_seci_c_SUITE.erl %
%%% @author etxasta
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R3A/R4A/R5A/R6A/R7A/1

%%% @doc ==Tests of XYZ==
%%% This test suite exercises the SECI interface.
%%%
%%% To run the dialyzer on this module:
%%%
%%% dialyzer cmsi_c_SUITE.erl $RCT_TOP/test/lib/rct-proxy/esrc/rct_proxy.erl
%%% @end

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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

-module(cert_seci_c_SUITE).
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev     Date       Name    What
%%% ------- ---------- ------- -------------------------------
%%% R3A/1   2014-11-06 etxasta Created
%%% R3A/10  2015-02-28 etxkols Preparation for cluster
%%% R4A/3   2015-07-10 etxjovp Add group definitions used by CS CI
%%% R4A/4   2015-10-15 etxpejn Add group for cluster sim
%%% R5A/1   2016-04-05 etxasta Added seci_get_nc, seci_get_tcat
%%% R6A/1   2016-06-17 etxasta Added seci_verify_peer
%%% R7A/1   2016-09-27 etxasta Added seci_verify_peer_vc,
%%%                            seci_get_vcert, seci_encode_decode
%%% ----------------------------------------------------------

-export([
	 suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 groups/0,
	 all/0
	]).

-export([
	 seci_get_cert/1,
         seci_write_data/1,
         seci_read_data/1,
         seci_delete_data/1,
         seci_log/1,
         seci_sub_test/1,
	 seci_get_vc/1,
	 seci_get_nc/1,
	 seci_get_tcat/1,
         seci_verify_peer/1,
         seci_verify_peer_vc/1,
         seci_get_vcert/1,
         seci_encode_decode/1
	]).

-include_lib("common_test/include/ct.hrl").
-include("test_seci.hrl").

%%% ----------------------------------------------------------
%%% COMMON TEST CALLBACK FUNCTIONS
%%% For descriptions, see the Common Test manual.
%%% ----------------------------------------------------------

-type config() :: [{atom(), term()}].

%% @hidden
-spec suite() -> config().
suite() ->
    [
     {timetrap, {minutes, 30}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       []}}]}},
		 {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
		 {rct_core,[]},
                 {rct_netconf,{nc1, html}}
		]}
    ].

%% @hidden
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    {ok, client_started} = rct_proxy:start_proxy(node1, seci1, ?SECI),
    Config.

%% @hidden
-spec end_per_suite(config()) -> any().
end_per_suite(_Config) ->
    ok.

%% @hidden
-spec init_per_group(atom(), config()) -> config().
init_per_group(_GroupName, Config) ->
    Config.

%% @hidden
-spec end_per_group(atom(), config()) -> any().
end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
    Config.

%% @hidden
-spec end_per_testcase(atom(), config()) -> any().
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @hidden
-spec groups() -> [{atom(), list(), list()}].
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], []}, 
     {sbc__upgrade_short__all__1__group, [], []}, 
     {sbc__upgrade__all__1__group, [], []},
     {sdc__cover__sim__1__group, [], []},  
     {sdc__nocover__sim__1__group, [], []},
     {sdc__def__all__1__group, [], []},  
     {sdc__qual__all__1__group, [], []},
     %% This suite should only be run on core MP within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]}
 ].

%% @hidden
-spec all() -> list() | {skip, term()}.
all() ->
    [
       seci_get_cert,
       seci_write_data,
       seci_read_data,
       seci_delete_data,
       seci_log,
       seci_sub_test,
       seci_get_vc,
       seci_get_nc       
    ].

%%% ----------------------------------------------------------
%%% TEST CASES
%%% ----------------------------------------------------------
seci_get_cert(_Config) ->
    NC1 = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=2",
    ct:pal("Get NC: ~p", [NC1]),
    get_cert(NC1),
    NC2 = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=1",
    ct:pal("Get NC: ~p", [NC2]),
    get_cert(NC2),
    TCAT1 = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=2",
    ct:pal("Get TCAT: ~p", [TCAT1]),
    get_cert(TCAT1),
    TCAT2 = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=1",
    ct:pal("Get TCAT: ~p", [TCAT2]),
    get_cert(TCAT2),
    ct:pal("Get VC", []),
    get_cert("at_least_something"),
    ok.

get_cert(String) ->
    case send(?SECI_GET_CERT, {String}) of
        {ok, ?SECI_OK, Result} ->
            ct:pal("Result ok: ~p", [Result]),
            ok;
        {ok, ?SECI_NOT_FOUND, Result} -> 
            ct:pal("Result not_found: ~p", [Result]),
            ok;
        Result ->
            ct:pal("Not supported result: ~p", [Result]),
            ct:fail(Result)
    end.

seci_get_vc(_Config) ->
    ct:pal("Run test case seci_get_vc"),
    case send(?SECI_GET_VC, {"at_least_something"}) of
        {ok, ?SECI_OK, Result} ->
            ct:pal("Result ok: ~p", [Result]),
            ok;
        {ok, ?SECI_NOT_FOUND, Result} -> 
            ct:pal("Result not_found: ~p", [Result]),
            ok;
        Result ->
            ct:pal("Not supported result: ~p", [Result]),
            ct:fail(Result)
    end.

seci_get_nc(_Config) ->
    ct:pal("Run test case seci_get_nc"),
    NC1 = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=1",
    ct:pal("Get NC1: ~p", [NC1]),
    get_nc(NC1),
    NC2 = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=2",
    ct:pal("Get NC2: ~p", [NC2]),
    get_nc(NC2),
    ok.

get_nc(String) ->
    case send(?SECI_GET_NC, {String}) of
        {ok, ?SECI_OK, Result} ->
            ct:pal("Result ok: ~p", [Result]),
            ok;
        {ok, ?SECI_NOT_FOUND, Result} -> 
            ct:pal("Result not_found: ~p", [Result]),
            ok;
        Result ->
            ct:pal("Not supported result: ~p", [Result]),
            ct:fail(Result)
    end.

seci_get_tcat(_Config) ->
    ct:pal("Run test case seci_get_tcat"),
    TCAT1 = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=1",
    ct:pal("Get TCAT1: ~p", [TCAT1]),
    get_tcat(TCAT1),
    TCAT2 = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=2",
    ct:pal("Get TCAT2: ~p", [TCAT2]),
    get_tcat(TCAT2),
    ok.

get_tcat(String) ->
    case send(?SECI_GET_TCAT, {String}) of
        {ok, ?SECI_OK, Result} ->
            ct:pal("Result ok: ~p", [Result]),
            ok;
        {ok, ?SECI_NOT_FOUND, Result} -> 
            ct:pal("Result not_found: ~p", [Result]),
            ok;
        Result ->
            ct:pal("Not supported result: ~p", [Result]),
            ct:fail(Result)
    end.

seci_write_data(_Config) ->
    Id    = "test",
    Index = "1",
    Data  = <<"Save this dummy text and see if it can be read again!">>,
    ct:pal("Write secure storage, Id: ~p, Index: ~p, Data: ~p",
        [Id, Index, Data]),
    case send(?SECI_WRITE, {Id, Index, Data}) of
        {ok, ?SECI_OK} ->
            ct:pal("Result ok", []),
            ok;
        Result ->
            ct:pal("Not supported result: ~p", [Result]),
            ct:fail("SECI_ERROR")
    end.

seci_read_data(_Config) ->
    Id    = "test",
    Index = "1",
    ct:pal("Read secure storage, Id: ~p, Index: ~p", [Id, Index]),
    case send(?SECI_READ, {Id, Index}) of
        {ok, ?SECI_OK, Result} ->
            ct:pal("Result ok: ~p", [Result]),
            ok;
        Result ->
            ct:pal("Not supported result: ~p", [Result]),
            ct:fail(Result)
    end.

seci_delete_data(_Config) ->
    Id    = "test",
    Index = "1",
    ct:pal("Delete secure storage, Id: ~p, Index: ~p", [Id, Index]),
    case send(?SECI_DELETE, {Id, Index}) of
        {ok, ?SECI_OK} ->
            ct:pal("Result ok", []),
            ok;
        Result ->
            ct:pal("Not supported result: ~p", [Result]),
            ct:fail(Result)
    end.

seci_log(_Config) ->
    Type    = ?SECI_SECURITY_LOG,
    Facility = 4,
    Severity = ?SECI_SEVERITY_INFO,
    Message  = "Testing testing!!!",
    ct:pal("Log, Type: ~p, Facility: ~p, Severity: ~p Message: ~p",
        [Type, Facility, Severity, Message]),
    case send(?SECI_LOG, {Type, Facility, Severity, Message}) of
        {ok, ?SECI_OK} ->
            ct:pal("Result ok", []),
            ok;
        Result ->
            ct:pal("Not supported result: ~p", [Result]),
            ct:fail(Result)
    end.

seci_sub_test(_Config) ->
    ct:pal("Run subscription test", []),
    Id = "seci_test",
    Dn = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=1",
    case send(?SECI_SUB_INIT, {Id, Dn}) of
        {ok, ?SECI_OK, Result} ->
            ct:pal("Result ok, Result: ~p", [Result]),
            ok;
        Result ->
            ct:pal("Not supported result: ~p", [Result]),
            ct:fail(Result)
    end.

seci_verify_peer(_Config) ->
    ct:pal("Run verify_peer", []),
    Tcat = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=1",
    File = list_to_binary("/home/etxasta/cacert.pem"),
    {ok, PeerCertPemBin} = file:read_file(File),
    ct:pal("Tcat: ~p", [Tcat]),
    case send(?SECI_VERIFY_PEER, {Tcat, PeerCertPemBin}) of
        {ok, ?SECI_VERIFY_PEER_VALID} ->
            ct:pal("Peer certificate is valid", []);
        {ok, ?SECI_VERIFY_PEER_NOT_VALID} -> 
            ct:pal("Peer certificate is not valid", []);
        {ok, ?SECI_VERIFY_PEER_UNKNOWN} -> 
            ct:pal("State of peer certificate is unknown", [])
    end,
    ok.

seci_get_vcert(_Config) ->
    ct:pal("Run seci_get_vcert", []),
    case do_get_vcert() of
        {error, Result} ->
            ct:pal("Not supported result: ~p", [Result]),
            ct:fail(Result);
        Result ->
            ct:pal("Result ok: ~p", [Result])
    end,
    ok.

seci_verify_peer_vc(_Config) ->
    ct:pal("Run seci_verify_peer_vc", []),
    %% Use local VC as peer
    Peer = do_get_vcert(),
    case send(?SECI_VERIFY_PEER_VC, {Peer}) of
        {ok, ?SECI_OK, Result} ->
            ct:pal("Result ok: ~p", [Result]),
            ok;
        Result ->
            ct:pal("Failed, result: ~p", [Result]),
            ct:fail(Result)
    end,
    ok.

seci_encode_decode(_Config) ->
    ct:pal("Run seci_encode_decode", []),
    Peer    = do_get_vcert(),
    Data    = "test123",
    %% Use local VC as peer
    EncodedData =
    case send(?SECI_ENCODE, {Peer, length(Data), Data}) of
        {ok, ?SECI_OK, Res1} ->
            ct:pal("Result ok: ~p", [Res1]),
            Res1;
        Res1 ->
            ct:pal("Failed, result: ~p", [Res1]),
            ct:fail(Res1)
    end,
    case send(?SECI_DECODE, {Peer, byte_size(EncodedData), EncodedData}) of
        {ok, ?SECI_OK, Res2} ->
            ct:pal("Result ok: ~p", [Res2]);
        Res2 ->
            ct:pal("Failed, result: ~p", [Res2]),
            ct:fail(Res2)
    end,
    ok.

do_get_vcert() ->
    case send(?SECI_GET_VCERT, []) of
        {ok, ?SECI_OK, Result} ->
            Result;
        Result ->
            {error, Result}
    end.




%%% ----------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%% @doc Send a request to the IFT application.
send(Function, Data) ->
    rct_proxy:send_proxy(node1, seci1, Function, Data).

