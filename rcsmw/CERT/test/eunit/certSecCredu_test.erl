%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certSecCredu_test.erl %
%%% @author emirbos
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/R12A/1

%%% @doc ==certSecCredu unit test==
%%% Unit test for certSecCredu
%%% @end

-module(certSecCredu_test).
-vsn('/main/R11A/R12A/1').
-date('2017-10-25').
-author('emirbos').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% -------------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% -------------------------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ---------------------------------------
%%% R11A/1     2017-08-28 ekurnik     Created
%%% R11A/2     2017-08-29 ekurnik     New UTs added
%%% R11A/3     2017-08-31 evadumb     Added subscriber functions tests
%%% R11A/4     2017-08-31 enekdav     Added UTs for directory functions
%%% R11A/5     2017-09-04 edartop     Added UTs for Data serilization
%%% R11A/6     2017-09-04 ekurnik     Added UTs for DN manipulation + refactoring
%%% R11A/8     2017-09-07 emirbos     Added UTs for handle_sec_credu, part I
%%% R11A/9     2017-09-08 evadumb     Added handle_sec_credu_request_test, 
%%%                                   handle_sec_credu_response_test, tcat_list_to_binary_test,
%%%                                   version_string_to_tuple_test
%%% R11A/10    2017-09-11 emirbos     Added UTs for handle_sec_credu, part II
%%% R11A/11    2017-09-12 edartop     Added additional Uts for Data serilization
%%% R11A/12    2017-09-15 emirbos     Added UTs for handle_cert_event
%%% R11A/13    2017-10-03 emirbos     handle_sec_credu_trustcategory_subscribe_test updated
%%% R11A/14    2017-10-05 enatdok     Added minor fixes
%%% R11A/15    2017-10-05 emirbos     handle_cast_cert_event_test and
%%%                                   handle_sec_credu_nodecredential_subscribe_test updated
%%% R11A/16    2017-10-17 enekdav     Changed send_socket into send_event
%%% R11A/17    2017-10-17 ekurnik     Cleaned up unused functions
%%% R11A/18    2017-10-18 enekdav     Separated send_socket into send_event and send_response
%%% R11A/19    2017-10-18 evadumb     Added get_subscriber_by_comm_socket_test, finalize_subscriber_test,
%%%                                   unsubscribe_from_all_test
%%% R12A/1     2017-10-24 emirbos     Added tests to increase coverage
%%% -------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").
-include("certSecCredu.hrl").


setup() ->
    _Config = [].

tear_down(_Config) ->
    ok.


crypto_test_() ->
    { foreach,
      fun setup/0,
      fun tear_down/1,
      []
    }.

-define(NODE_CREDENTIAL_DN1, list_to_binary(?NODE_CREDENTIAL_DN("nc1"))).
-define(NODE_CREDENTIAL_DN2, list_to_binary(?NODE_CREDENTIAL_DN("nc2"))).
-define(NODE_CREDENTIAL_DN3, list_to_binary(?NODE_CREDENTIAL_DN("nc3"))).
-define(TRUSTED_CERTIFICATE_DN1, list_to_binary(?TRUSTED_CERTIFICATE_DN("1"))).
-define(TRUSTED_CERTIFICATE_DN2, list_to_binary(?TRUSTED_CERTIFICATE_DN("2"))).
-define(TRUST_CATEGORY_DN1, list_to_binary(?TRUST_CATEGORY_DN("tcat1"))).
-define(TRUST_CATEGORY_DN2, list_to_binary(?TRUST_CATEGORY_DN("tcat2"))).


-define(SUBSCRIBER1, #subscriber{id = 1}).
-define(SUBSCRIBER2, #subscriber{id = 2}).
-define(SUBSCRIBER3, #subscriber{id = 3, communication_socket = undefined}).
-define(SUBSCRIBER4, #subscriber{id = 4, select_object = {socket, sel_obj}, subscriptions = [#subscription{ subscribe_id = 4, mo_ref = ?NODE_CREDENTIAL_DN1}],
                                 communication_socket = 321}).

-define(SUBSCRIBER1_SUBS, #subscriber{id = 1, subscriptions = 
                                     [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1}], select_object = {socket, sel_obj},
                                     communication_socket = 123}).

-define(SUBSCRIBERS_EMPTY, []).
-define(SUBSCRIBERS_NON_EMPTY, [?SUBSCRIBER1, ?SUBSCRIBER3]).
-define(SUBSCRIBERS1_SUBS, [?SUBSCRIBER1_SUBS, ?SUBSCRIBER3]).
-define(SUBSCRIBERS2_SUBS, [?SUBSCRIBER1_SUBS, ?SUBSCRIBER3, ?SUBSCRIBER4]).
-define(SUBSCRIBERS_INVALID, [#subscriber{id = 1}, #subscriber{id = 3}, #subscriber{id = 1}]).

-define(PID, 123).
-define(SOCKET, 321).
-define(MSG, "Msg").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DN manipulation unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mo_ref_to_id_test() ->
    ?assertEqual("nc1", certSecCredu:mo_ref_to_id(?NODE_CREDENTIAL_DN1)),
    ?assertEqual("tcat1", certSecCredu:mo_ref_to_id(?TRUST_CATEGORY_DN1)),
    ?assertEqual("1", certSecCredu:mo_ref_to_id(?TRUSTED_CERTIFICATE_DN1)),
    ?assertError(_, certSecCredu:mo_ref_to_id("nc1")).

node_credential_mo_ref_test() ->
    ?assertEqual(?NODE_CREDENTIAL_DN1, certSecCredu:node_credential_mo_ref("nc1")).

trust_category_mo_ref_test() ->
    ?assertEqual(?TRUST_CATEGORY_DN1, certSecCredu:trust_category_mo_ref("tcat1")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Subscriber unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_client_id_test() ->
    lists:foldl(fun(_, UniqueIDs) -> 
                          UniqueId = certSecCredu:generate_client_id(),
                          ?assertNot(lists:member(UniqueId, UniqueIDs)),
                          [UniqueId | UniqueIDs]
                  end, [] ,lists:seq(1, 10)).

add_subscriber_test() ->
    Subscribers1 = certSecCredu:add_subscriber(2, ?SOCKET, ?SUBSCRIBERS_EMPTY),
    ?assertEqual(1, length(Subscribers1)),
    ?assertEqual(Subscribers1, [#subscriber{id = 2, communication_socket = ?SOCKET}]),
    
    Subscribers2 = certSecCredu:add_subscriber(2, ?SOCKET, ?SUBSCRIBERS_NON_EMPTY),
    ?assertEqual(3, length(Subscribers2)),
    ?assert(lists:member(#subscriber{id = 2, communication_socket = ?SOCKET}, Subscribers2)),
    
    ok.

remove_subscriber_test() ->
    %% No subscribers
    ?assertError(_, certSecCredu:remove_subscriber(1, ?SUBSCRIBERS_EMPTY)),
    
    %% No ID
    ?assertError(_, certSecCredu:remove_subscriber(2, ?SUBSCRIBERS_NON_EMPTY)),
    
    Subscribers1 = certSecCredu:remove_subscriber(1, ?SUBSCRIBERS_NON_EMPTY),
    ?assertEqual(1, length(Subscribers1)),
    ?assertNot(lists:member(#subscriber{id = 1}, Subscribers1)),
    ?assert(lists:member(#subscriber{id = 3}, Subscribers1)),
    
    ok.

update_subscriber_test() ->
    ?assertError(_, certSecCredu:update_subscriber(?SUBSCRIBER1, ?SUBSCRIBERS_EMPTY)), %% empty list
    ?assertError(_, certSecCredu:update_subscriber(?SUBSCRIBER2, ?SUBSCRIBERS1_SUBS)), %% no user
    NewList = certSecCredu:update_subscriber(?SUBSCRIBER1, ?SUBSCRIBERS1_SUBS), %% valid case, removed subscriptions
    ?assertEqual(2, length(NewList)),
    ?assertNot(lists:member(?SUBSCRIBER1_SUBS, NewList)),
    ?assert(lists:member(?SUBSCRIBER1, NewList)),
    
    ok.

get_subscriber_by_id_test() ->

    ?assertEqual({error, id_not_found}, certSecCredu:get_subscriber_by_id(2, ?SUBSCRIBERS_EMPTY)),
    ?assertEqual({error, id_not_found}, certSecCredu:get_subscriber_by_id(2, ?SUBSCRIBERS_NON_EMPTY)),
    
    ?assertEqual({ok, #subscriber{id = 1}}, certSecCredu:get_subscriber_by_id(1, ?SUBSCRIBERS_NON_EMPTY)),
    
    ?assertError(_, certSecCredu:get_subscriber_by_id(1, ?SUBSCRIBERS_INVALID)),
    
    ok.

get_selection_object_test() ->
    
    ?assertEqual({error, sel_obj_not_found}, certSecCredu:get_selection_object(#subscriber{id = 1})),
    ?assertEqual({ok, {socket, sel_obj}}, certSecCredu:get_selection_object(#subscriber{id = 1, select_object = {socket, sel_obj}})),

    ok.

set_selection_object_test() ->
    
    ?assertEqual(#subscriber{id = 1, select_object = {socket, sel_obj}}, 
                 certSecCredu:set_selection_object(#subscriber{id = 1}, {socket, sel_obj})),

    ok.

close_selection_object_test() ->
    meck:new(gen_tcp,[ passthrough, unstick]),
    meck:expect(gen_tcp,close, fun({socket, sel_obj}) -> ok; (_) -> error end),
    ?assertEqual(#subscriber{id = 1, select_object = undefined}, 
                 certSecCredu:close_selection_object(#subscriber{id = 1, select_object = {socket, sel_obj}})),
    meck:unload(gen_tcp),

    ok.

generate_subscription_id_test() ->
    lists:foldl(fun(_, UniqueIDs) -> 
                          UniqueId = certSecCredu:generate_subscription_id(),
                          ?assertNot(lists:member(UniqueId, UniqueIDs)),
                          [UniqueId | UniqueIDs]
                  end, [], lists:seq(1, 10)),
    ok.

get_subscription_id_test() ->
    Subscriber1 = #subscriber{id = 1, subscriptions = []},
    Subscriber2 = #subscriber{id = 2, subscriptions = 
                                  [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1}]},
    Subscriber3 = #subscriber{id = 2, subscriptions = 
                                  [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1},
                                   #subscription{subscribe_id = 3, mo_ref = ?NODE_CREDENTIAL_DN3},
                                   #subscription{subscribe_id = 4, mo_ref = ?TRUST_CATEGORY_DN1}]},
    SubscriberInvalid = #subscriber{id = 2, subscriptions = 
                                  [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1},
                                   #subscription{subscribe_id = 3, mo_ref = ?NODE_CREDENTIAL_DN3},
                                   #subscription{subscribe_id = 4, mo_ref = ?NODE_CREDENTIAL_DN3}]},
    
    ?assertEqual({error, sub_id_not_found}, certSecCredu:get_subscription_id(Subscriber1, ?NODE_CREDENTIAL_DN3)),
    ?assertEqual({error, sub_id_not_found}, certSecCredu:get_subscription_id(Subscriber2, ?NODE_CREDENTIAL_DN3)),
    ?assertEqual({ok, 3}, certSecCredu:get_subscription_id(Subscriber3, ?NODE_CREDENTIAL_DN3)),
    ?assertError(_, certSecCredu:get_subscription_id(SubscriberInvalid, ?NODE_CREDENTIAL_DN3)),
    
    ok.

add_subscripton_test() ->
    Subscriber1 = #subscriber{id = 1, subscriptions = []},
    Subscriber2 = #subscriber{id = 2, subscriptions = 
                                  [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1},
                                   #subscription{subscribe_id = 2, mo_ref = ?NODE_CREDENTIAL_DN2}]},

    #subscriber{subscriptions = Subs1} = certSecCredu:add_subscription(Subscriber1, ?NODE_CREDENTIAL_DN1, 1),
    ?assertEqual(1, length(Subs1)),
    ?assert(lists:member(#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1}, Subs1)),

    #subscriber{subscriptions = Subs2} = certSecCredu:add_subscription(Subscriber2, ?NODE_CREDENTIAL_DN3, 3),
    ?assertEqual(3, length(Subs2)),
    ?assert(lists:member(#subscription{subscribe_id = 3, mo_ref = ?NODE_CREDENTIAL_DN3}, Subs2)),

    ok.

remove_subscription_test() ->
    Subscriber1 = #subscriber{id = 1, subscriptions = []},
    Subscriber2 = #subscriber{id = 1, subscriptions = 
                                  [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1}]},
    Subscriber3 = #subscriber{id = 1, subscriptions = 
                                  [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1},
                                   #subscription{subscribe_id = 2, mo_ref = ?NODE_CREDENTIAL_DN2}]},

    ?assertError(_, certSecCredu:remove_subscription(Subscriber1, 1)),
    ?assertEqual(Subscriber1, certSecCredu:remove_subscription(Subscriber2, 1)),
    ?assertError(_, certSecCredu:remove_subscription(Subscriber2, 2)),
    ?assertEqual(Subscriber2, certSecCredu:remove_subscription(Subscriber3, 2)),
    ok.

get_subscription_by_id_test() ->
    SubscriptionsEmpty = [],
    Subscriptions1 = [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1}],
    Subscriptions2 = [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1},
                      #subscription{subscribe_id = 2, mo_ref = ?NODE_CREDENTIAL_DN2}],
    
    ?assertEqual({error, not_found}, certSecCredu:get_subscription_by_id(SubscriptionsEmpty, 1)),
    ?assertEqual({ok, #subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1}}, certSecCredu:get_subscription_by_id(Subscriptions1, 1)),
    ?assertEqual({error, not_found}, certSecCredu:get_subscription_by_id(Subscriptions2, 3)),
    ok.

get_subscription_by_mo_ref_test() ->
    SubscriptionsEmpty = [],
    Subscriptions1 = [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1}],
    Subscriptions2 = [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1},
                      #subscription{subscribe_id = 2, mo_ref = ?NODE_CREDENTIAL_DN2}],
    
    ?assertEqual({error, not_found}, certSecCredu:get_subscription_by_mo_ref(SubscriptionsEmpty, ?NODE_CREDENTIAL_DN1)),
    ?assertEqual({ok, #subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1}}, 
                 certSecCredu:get_subscription_by_mo_ref(Subscriptions1, ?NODE_CREDENTIAL_DN1)),
    ?assertEqual({error, not_found}, certSecCredu:get_subscription_by_mo_ref(Subscriptions2, ?NODE_CREDENTIAL_DN3)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Directory unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_dir_structure_test() ->
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(file, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    
    SEC_CREDU_DIR = ?SEC_CREDU_DIR,
    NC_CERT_DIR = ?NC_CERT_DIR,
    NC_KEY_DIR = ?NC_KEY_DIR,
    TCAT_ROOT_DIR = ?TCAT_ROOT_DIR,
    meck:expect(file, make_dir, 1, fun(Dir) when Dir =:= SEC_CREDU_DIR orelse
                                                 Dir =:= NC_CERT_DIR orelse
                                                 Dir =:= NC_KEY_DIR orelse
                                                 Dir =:= TCAT_ROOT_DIR -> ok end),
    ?assertEqual(ok, certSecCredu:create_directory_structure()),
    
    meck:expect(file, make_dir, 1, fun(Dir) when Dir =:= TCAT_ROOT_DIR -> {error, eacces};
                                                (_Dir) -> ok end),
    ?assertError({badmatch, {error, eacces}}, certSecCredu:create_directory_structure()),
    
    meck:expect(file, make_dir, 1, fun(Dir) when Dir =:= SEC_CREDU_DIR -> {error, eacces};
                                                (_Dir) -> ok end),
    ?assertError({badmatch, {error, eacces}}, certSecCredu:create_directory_structure()),

    meck:expect(file, make_dir, 1, fun(Dir) when Dir =:= NC_CERT_DIR -> {error, eacces};
                                                (_Dir) -> ok end),
    ?assertError({badmatch, {error, eacces}}, certSecCredu:create_directory_structure()),

    meck:expect(file, make_dir, 1, fun(Dir) when Dir =:= NC_KEY_DIR -> {error, eacces};
                                                (_Dir) -> ok end),
    ?assertError({badmatch, {error, eacces}}, certSecCredu:create_directory_structure()),
    
    meck:unload(file),
    meck:unload(sysEnv).

delete_dir_structure_test() ->
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(file, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),

    SEC_CREDU_DIR = ?SEC_CREDU_DIR,
    NC_CERT_DIR = ?NC_CERT_DIR,
    NC_KEY_DIR = ?NC_KEY_DIR,
    TCAT_ROOT_DIR = ?TCAT_ROOT_DIR,
    meck:expect(file, delete, 1, fun(_File) -> ok end),
    meck:expect(file, del_dir, 1, fun(Dir) when Dir =:= SEC_CREDU_DIR orelse
                                                Dir =:= NC_CERT_DIR orelse
                                                Dir =:= NC_KEY_DIR orelse
                                                Dir =:= TCAT_ROOT_DIR -> ok end),
    meck:expect(file, list_dir_all, fun(_Dir) -> {ok, []} end),
    ?assertEqual(ok, certSecCredu:delete_directory_structure()),
    
    meck:expect(file, del_dir, 1, fun(Dir) when Dir =:= TCAT_ROOT_DIR -> {error, eacces};
                                                (_Dir) -> ok end),
    ?assertError({badmatch, {error, eacces}}, certSecCredu:delete_directory_structure()),
    
    meck:expect(file, del_dir, 1, fun(Dir) when Dir =:= SEC_CREDU_DIR -> {error, eacces};
                                                (_Dir) -> ok end),
    ?assertError({badmatch, {error, eacces}}, certSecCredu:delete_directory_structure()),

    meck:expect(file, del_dir, 1, fun(Dir) when Dir =:= NC_CERT_DIR -> {error, eacces};
                                                (_Dir) -> ok end),
    ?assertError({badmatch, {error, eacces}}, certSecCredu:delete_directory_structure()),

    meck:expect(file, del_dir, 1, fun(Dir) when Dir =:= NC_KEY_DIR -> {error, eacces};
                                                (_Dir) -> ok end),
    ?assertError({badmatch, {error, eacces}}, certSecCredu:delete_directory_structure()),

    meck:unload(file),
    meck:unload(sysEnv).

get_nc_cert_filename_test() ->
    ?assertEqual(?NC_CERT_FILENAME("1"), certSecCredu:get_nc_cert_filename("1")),
    ?assertNotEqual(?NC_CERT_FILENAME("2"), certSecCredu:get_nc_cert_filename("3")).

get_nc_key_filename_test() ->
    ?assertEqual(?NC_KEY_FILENAME("1"), certSecCredu:get_nc_key_filename("1")),
    ?assertNotEqual(?NC_KEY_FILENAME("2"), certSecCredu:get_nc_key_filename("3")).

nc_cert_file_exists_test() ->
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(filelib, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    
    ExistingFile = ?NC_CERT_DIR ++ "/" ++ ?NC_CERT_FILENAME("1"),
    meck:expect(filelib, is_regular, 1, fun(File) when File =:= ExistingFile -> true end),
    ?assert(certSecCredu:nc_cert_file_exists(?NC_CERT_FILENAME("1"))),
    
    NonExistingFile = ?NC_CERT_DIR ++ "/" ++ ?NC_CERT_FILENAME("2"),
    meck:expect(filelib, is_regular, 1, fun(File) when File =:= NonExistingFile -> false end),
    ?assertNot(certSecCredu:nc_cert_file_exists(?NC_CERT_FILENAME("2"))),
    
    meck:unload(filelib),
    meck:unload(sysEnv).

nc_key_file_exists_test() ->
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(filelib, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    
    ExistingFile = ?NC_KEY_DIR ++ "/" ++ ?NC_KEY_FILENAME("1"),
    meck:expect(filelib, is_regular, 1, fun(File) when File =:= ExistingFile -> true end),
    ?assert(certSecCredu:nc_key_file_exists(?NC_KEY_FILENAME("1"))),
    
    NonExistingFile = ?NC_KEY_DIR ++ "/" ++ ?NC_KEY_FILENAME("2"),
    meck:expect(filelib, is_regular, 1, fun(File) when File =:= NonExistingFile -> false end),
    ?assertNot(certSecCredu:nc_key_file_exists(?NC_KEY_FILENAME("2"))),
    
    meck:unload(filelib),
    meck:unload(sysEnv).

write_nc_cert_to_dir_test() ->
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(file, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),

    ExistingFile = ?NC_CERT_DIR ++ "/" ++ ?NC_CERT_FILENAME("1"),
    meck:expect(file, write_file, 2, fun(Path, _Content) when Path =:= ExistingFile -> ok end),
    ?assertEqual(ok, certSecCredu:write_nc_cert_to_dir(?NC_CERT_FILENAME("1"), "test")),
    
    NonExistingFile = ?NC_CERT_DIR ++ "/" ?NC_CERT_FILENAME("2"),
    meck:expect(file, write_file, 2, fun(Path, _Content) when Path =:= NonExistingFile -> {error, enoent} end),
    ?assertError({badmatch, {error, enoent}}, certSecCredu:write_nc_cert_to_dir(?NC_CERT_FILENAME("2"), "test")),

    meck:unload(file),
    meck:unload(sysEnv).

write_nc_key_to_dir_test() ->
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(file, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),

    ExistingFile = ?NC_KEY_DIR ++ "/" ++ ?NC_KEY_FILENAME("1"),
    meck:expect(file, write_file, 2, fun(Path, _Content) when Path =:= ExistingFile -> ok end),
    ?assertEqual(ok, certSecCredu:write_nc_key_to_dir(?NC_KEY_FILENAME("1"), "test")),
    
    NonExistingFile = ?NC_KEY_DIR ++ "/" ++ ?NC_KEY_FILENAME("2"),
    meck:expect(file, write_file, 2, fun(Path, _Content) when Path =:= NonExistingFile -> {error, enoent} end),
    ?assertError({badmatch, {error, enoent}}, certSecCredu:write_nc_key_to_dir(?NC_KEY_FILENAME("2"), "test")),

    meck:unload(file),
    meck:unload(sysEnv).

remove_nc_cert_from_dir_test() ->
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(file, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),

    ExistingFile = ?NC_CERT_DIR ++ "/" ++ ?NC_CERT_FILENAME("1"),
    meck:expect(file, delete, 1, fun(Path) when Path =:= ExistingFile -> ok end),
    ?assertEqual(ok, certSecCredu:remove_nc_cert_from_dir(?NC_CERT_FILENAME("1"))),
    
    NonExistingFile = ?NC_CERT_DIR ++ "/" ++ ?NC_CERT_FILENAME("2"),
    meck:expect(file, delete, 1, fun(Path) when Path =:= NonExistingFile -> {error, enoent} end),
    ?assertError({badmatch, {error, enoent}}, certSecCredu:remove_nc_cert_from_dir(?NC_CERT_FILENAME("2"))),

    meck:unload(file),
    meck:unload(sysEnv).

remove_nc_key_from_dir_test() ->
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(file, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),

    ExistingFile = ?NC_KEY_DIR ++ "/" ++ ?NC_KEY_FILENAME("1"),
    meck:expect(file, delete, 1, fun(Path) when Path =:= ExistingFile -> ok end),
    ?assertEqual(ok, certSecCredu:remove_nc_key_from_dir(?NC_KEY_FILENAME("1"))),
    
    NonExistingFile = ?NC_KEY_DIR ++ "/" ++ ?NC_KEY_FILENAME("2"),
    meck:expect(file, delete, 1, fun(Path) when Path =:= NonExistingFile -> {error, enoent} end),
    ?assertError({badmatch, {error, enoent}}, certSecCredu:remove_nc_key_from_dir(?NC_KEY_FILENAME("2"))),

    meck:unload(file),
    meck:unload(sysEnv).

get_tcat_dirname_test() ->
    ?assertEqual(?TCAT_DIRNAME("1"), certSecCredu:get_tcat_dirname("1")),
    ?assertNotEqual(?TCAT_DIRNAME("2"), certSecCredu:get_tcat_dirname("3")).

tcat_dir_exists_test() ->
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(filelib, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    
    ExistingDir = ?TCAT_ROOT_DIR ++?TCAT_DIRNAME("1"),
    meck:expect(filelib, is_dir, 1, fun(Dir) when Dir =:= ExistingDir -> true end),
    ?assert(certSecCredu:tcat_dir_exists(?TCAT_DIRNAME("1"))),
    
    NonExistingDir = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("2"),
    meck:expect(filelib, is_dir, 1, fun(Dir) when Dir =:= NonExistingDir -> false end),
    ?assertNot(certSecCredu:tcat_dir_exists(?TCAT_DIRNAME("2"))),
    
    meck:unload(filelib),
    meck:unload(sysEnv).

create_tcat_dir_test() ->
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(file, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    
    ExistingDir = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("1"),
    meck:expect(file, make_dir, 1, fun(Dir) when Dir =:= ExistingDir -> ok end),
    ?assertEqual(ok, certSecCredu:create_tcat_dir(?TCAT_DIRNAME("1"))),
    
    NonExistingDir = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("2"),
    meck:expect(file, make_dir, 1, fun(Dir) when Dir =:= NonExistingDir -> {error, eacces} end),
    ?assertError({badmatch, {error, eacces}}, certSecCredu:create_tcat_dir(?TCAT_DIRNAME("2"))),
    
    meck:unload(file),
    meck:unload(sysEnv).

delete_tcat_dir_test() ->
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(file, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    
    ExistingDir = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("1"),
    meck:expect(file, delete, 1, fun(_File) -> ok end),
    meck:expect(file, del_dir, 1, fun(Dir) when Dir =:= ExistingDir -> ok end),
    meck:expect(file, list_dir_all, fun(Dir) when Dir =:= ExistingDir -> {ok, []} end),
    ?assertEqual(ok, certSecCredu:delete_tcat_dir(?TCAT_DIRNAME("1"))),
    
    NonExistingDir = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("2"),
    meck:expect(file, del_dir, 1, fun(Dir) when Dir =:= NonExistingDir -> {error, eacces} end),
    meck:expect(file, list_dir_all, fun(Dir) when Dir =:= NonExistingDir -> {error, eacces} end),
    ?assertError({badmatch, {error, eacces}}, certSecCredu:delete_tcat_dir(?TCAT_DIRNAME("2"))),
    
    meck:unload(file),
    meck:unload(sysEnv).

get_tc_cert_filename_test() ->
    ?assertEqual(?TC_CERT_FILENAME("1"), certSecCredu:get_tc_cert_filename("1")),
    ?assertNotEqual(?TC_CERT_FILENAME("2"), certSecCredu:get_tc_cert_filename("3")).

write_tc_cert_to_dir_test() ->
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(file, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),

    ExistingFile = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("1") ++ "/" ++ ?TC_CERT_FILENAME("1"),
    meck:expect(file, write_file, 2, fun(Path, _Content) when Path =:= ExistingFile -> ok end),
    ?assertEqual(ok, certSecCredu:write_tc_cert_to_dir(?TCAT_DIRNAME("1"), ?TC_CERT_FILENAME("1"), "test")),
    
    NonExistingFile = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("1") ++ "/" ++ ?TC_CERT_FILENAME("2"),
    meck:expect(file, write_file, 2, fun(Path, _Content) when Path =:= NonExistingFile -> {error, enoent} end),
    ?assertError({badmatch, {error, enoent}}, certSecCredu:write_tc_cert_to_dir(?TCAT_DIRNAME("1"), ?TC_CERT_FILENAME("2"), "test")),

    meck:unload(file),
    meck:unload(sysEnv).

create_response_message_test() ->
    StatusAtom0 = ok,
    StatusAtom1 = id_not_found,
    StatusAtom2 = sub_id_not_found,
    StatusAtom3 = mo_ref_not_found,
    StatusAtom4 = nc_not_installed,
    StatusAtom5 = tcat_empty,
    StatusAtomInvalid = dumb,
    StatusAtom6 = unknown_request,
    
    ?assertEqual(<<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_OK:4/native-unsigned-integer-unit:8>>, certSecCredu:create_response_message(StatusAtom0)),
    ?assertEqual(<<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_ERR_ID_NOT_FOUND:4/native-unsigned-integer-unit:8>>, certSecCredu:create_response_message(StatusAtom1)),
    ?assertEqual(<<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_ERR_SUB_ID_NOT_FOUND:4/native-unsigned-integer-unit:8>>, certSecCredu:create_response_message(StatusAtom2)),
    ?assertEqual(<<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_ERR_MO_REF_NOT_FOUND:4/native-unsigned-integer-unit:8>>, certSecCredu:create_response_message(StatusAtom3)),
    ?assertEqual(<<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_ERR_NC_NOT_INSTALLED:4/native-unsigned-integer-unit:8>>, certSecCredu:create_response_message(StatusAtom4)),
    ?assertEqual(<<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_ERR_TCAT_EMPTY:4/native-unsigned-integer-unit:8>>, certSecCredu:create_response_message(StatusAtom5)),
    ?assertEqual(<<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_ERR_UNKNOWN_REQ_TYPE:4/native-unsigned-integer-unit:8>>, certSecCredu:create_response_message(StatusAtom6)),
    ?assertError(_, certSecCredu:create_response_message(StatusAtomInvalid)),
    
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_sec_credu related unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_sec_credu_initialize_test() ->
    SecCreduVersion = "A11",
    meck:new(sysInitI, [passthrough, non_strict]),
    meck:expect(sysInitI, info_msg, 2, fun(_String, _ListOfEls) -> ok end),
    {{Result, Id, SecCreduVersion}, #state{subscribers = [Subscriber]}} = certSecCredu:handle_sec_credu_initialize({SecCreduVersion, ?SOCKET}, #state{subscribers = []}),
    ?assertEqual(ok, Result),
    ?assertEqual(is_integer(Id), true),
    ?assertEqual("A11", SecCreduVersion),
    ?assertEqual(Id, Subscriber#subscriber.id),
    meck:unload(sysInitI),
    ok.

handle_sec_credu_finalize_test() ->
    
    SubscriberNoSelObj = #subscriber{id = 1, select_object = undefined},
    Subscriber = #subscriber{id = 1, select_object = {socket, sel_obj}},
    FinalizeEvent = <<?SEC_CREDU_EVENT_SIG:4/native-unsigned-integer-unit:8, ?FINALIZE_EVENT:4/native-unsigned-integer-unit:8>>,
    
    ?assertEqual({{error,id_not_found},#state{subscribers = [Subscriber]}},
                 certSecCredu:handle_sec_credu_finalize(2, #state{subscribers = [Subscriber]})),
    
    ?assertEqual({ok, #state{subscribers = ?SUBSCRIBERS_EMPTY}},
                 certSecCredu:handle_sec_credu_finalize(1, #state{subscribers = [SubscriberNoSelObj]})),
    
    meck:new(gen_tcp, [passthrough, unstick]),
    meck:new(inet, [passthrough, unstick]),
    meck:expect(gen_tcp, send, 2, fun({socket, sel_obj}, Message) when Message =:= FinalizeEvent -> ok end),
    meck:expect(inet, setopts, 2, fun({socket, sel_obj}, [{active, once}]) -> ok end),
    meck:expect(gen_tcp, close, fun({socket, sel_obj}) -> ok; (_) -> error end),
    
    ?assertEqual({ok, #state{subscribers = ?SUBSCRIBERS_EMPTY}}, 
                 certSecCredu:handle_sec_credu_finalize(1, #state{subscribers = [Subscriber]})),
    
    meck:unload(gen_tcp),
    meck:unload(inet),
    
    ok.

handle_sec_credu_selectionobject_get_test() ->
    
    Subscriber = #subscriber{id = 1, select_object = {socket, sel_obj}},
    SubscriberNoSelObj = #subscriber{id = 1, select_object = undefined},
    SubscriberSelObjOld = #subscriber{id = 1, select_object = {socket, sel_obj_old}},
    
    ?assertEqual({{error,id_not_found},#state{subscribers = ?SUBSCRIBERS_EMPTY}},
                 certSecCredu:handle_sec_credu_selectionobject_get({1, {socket, sel_obj}}, #state{subscribers = ?SUBSCRIBERS_EMPTY})),
    
    ?assertEqual({ok, #state{subscribers = [Subscriber]}},
                 certSecCredu:handle_sec_credu_selectionobject_get({1, {socket, sel_obj}}, #state{subscribers = [SubscriberNoSelObj]})),
    
    meck:new(gen_tcp,[ passthrough, unstick]),
    meck:expect(gen_tcp, close, fun({socket, sel_obj_old}) -> ok; (_) -> error end),
    ?assertEqual({ok, #state{subscribers = [Subscriber]}},
                 certSecCredu:handle_sec_credu_selectionobject_get({1, {socket, sel_obj}}, #state{subscribers = [SubscriberSelObjOld]})),
    meck:unload(gen_tcp),
    
    ok.

handle_sec_credu_nodecredential_subscribe_test() ->
    
    Subscriber = #subscriber{id = 1, select_object = {socket, sel_obj}},
    SubscriberWithSubNc2 = #subscriber{id = 2, select_object = {socket, sel_obj}, subscriptions = [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1}]},
    
%%  subscription unsuccessful, subscriber id is not found
    ?assertEqual({{error,id_not_found},#state{subscribers = ?SUBSCRIBERS_EMPTY}},
                 certSecCredu:handle_sec_credu_nodecredential_subscribe({1, "nc1"}, #state{subscribers = ?SUBSCRIBERS_EMPTY})),
    
    meck:new(certLib, [non_strict, passthrough]),
    meck:new(mnesia,[passthrough, non_strict]),
    
    Nc1 = ?NODE_CREDENTIAL_DN1,
    Nc1Key = {"1","1","1","1","nc1"},
    meck:expect(certLib, decode_moref, 1, fun(MoRef) when MoRef =:= Nc1 -> {nc, Nc1Key}; (_) -> error end),
                                                                           
    meck:expect(mnesia, dirty_read, fun({nodeCredential, Key}) when Key =:= Nc1Key -> []; (_) -> error end),
    
%%  subscription unsuccessful, Node Credential MO does not exist
    ?assertEqual({{error, mo_ref_not_found},#state{subscribers = [Subscriber]}},
                 certSecCredu:handle_sec_credu_nodecredential_subscribe({1, "nc1"}, #state{subscribers = [Subscriber]})),
                 
    meck:new(certI,[passthrough, unstick]),
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(file, [unstick, passthrough]), 
    
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    
    ExistingFile = ?NC_CERT_DIR ++ "/" ++ ?NC_CERT_FILENAME("nc1"),
    ExistingFile1  = ?NC_KEY_DIR ++ "/" ++ ?NC_KEY_FILENAME("nc1"),
    meck:expect(file, write_file, 2, fun(Path, _Content) when Path =:= ExistingFile -> ok; (Path, _Content) when Path =:= ExistingFile1 -> ok; (_, _) -> error end),
                                                                           
    meck:expect(mnesia, dirty_read, fun({nodeCredential, Key}) when Key =:= Nc1Key -> [ok]; (_) -> error end),

    meck:expect(certI, get_cert, fun(NodeCredential, pem) when NodeCredential =:= Nc1 -> {ok, "NodeCert", "PrivKey"}; (_, _) -> error end),
    meck:expect(certI, subscribe, fun(NodeCredential, certSecCredu) when NodeCredential =:= Nc1 -> ok; (_, _) -> error end),
    
%%  subscription successful
    {{Result, SubscriptionId}, #state{subscribers = [#subscriber{subscriptions = [Subscription]} = SubscriberWithSubNc]}} = 
        certSecCredu:handle_sec_credu_nodecredential_subscribe({1, "nc1"}, #state{subscribers = [Subscriber]}),
    
    ?assertEqual(ok, Result),
    ?assertEqual(is_integer(SubscriptionId), true),
    ?assertEqual(1, SubscriberWithSubNc#subscriber.id), 
    ?assertEqual({socket, sel_obj}, SubscriberWithSubNc#subscriber.select_object),  
    ?assertEqual(SubscriptionId, Subscription#subscription.subscribe_id),
    ?assertEqual(?NODE_CREDENTIAL_DN1, Subscription#subscription.mo_ref),
    
%%  subscription successful, we already have a subscriber with subscription to specified NodeCredential
    meck:expect(certI, get_cert, fun(_, _) -> error end),
    meck:expect(certI, subscribe, fun(_, _) -> error end),
    
    {{Result1, SubscriptionId1}, #state{subscribers = [#subscriber{subscriptions = [Subscription1]} = SubscriberWithSubNc1, _Subscriber2]}} = 
        certSecCredu:handle_sec_credu_nodecredential_subscribe({1, "nc1"}, #state{subscribers = [Subscriber, SubscriberWithSubNc2]}),
    
    ?assertEqual(ok, Result1),
    ?assertEqual(is_integer(SubscriptionId1), true),
    ?assertEqual(1, SubscriberWithSubNc1#subscriber.id),    
    ?assertEqual({socket, sel_obj}, SubscriberWithSubNc1#subscriber.select_object), 
    ?assertEqual(SubscriptionId1, Subscription1#subscription.subscribe_id),
    ?assertEqual(?NODE_CREDENTIAL_DN1, Subscription1#subscription.mo_ref),
    
%%  subscription successful, specified NodeCredential is not installed yet
    
    meck:expect(file, write_file, 2, fun(_, _) -> error end),
    Nc2 = ?NODE_CREDENTIAL_DN2,
    meck:expect(certLib, decode_moref, 1, fun(MoRef) when MoRef =:= Nc2 -> {nc, {"1","1","1","1","nc2"}}; (_) -> error end),
    Nc2Key = {"1","1","1","1","nc2"},
    meck:expect(mnesia, dirty_read, fun({nodeCredential, Key}) when Key =:= Nc2Key -> [ok]; (_) -> error end),
    meck:expect(certI, subscribe, fun(NodeCredential, certSecCredu) when NodeCredential =:= Nc2 -> ok; (_, _) -> error end),
    
    {{Result2, SubscriptionId2}, #state{subscribers = [#subscriber{subscriptions = [Subscription2]} = SubscriberWithSubNc3]}} = 
        certSecCredu:handle_sec_credu_nodecredential_subscribe({1, "nc2"}, #state{subscribers = [Subscriber]}),
    
    ?assertEqual(ok, Result2),
    ?assertEqual(is_integer(SubscriptionId2), true),
    ?assertEqual(1, SubscriberWithSubNc3#subscriber.id),    
    ?assertEqual({socket, sel_obj}, SubscriberWithSubNc3#subscriber.select_object), 
    ?assertEqual(SubscriptionId2, Subscription2#subscription.subscribe_id),
    ?assertEqual(?NODE_CREDENTIAL_DN2, Subscription2#subscription.mo_ref),
    
    meck:unload(certI),
    meck:unload(sysEnv),
    meck:unload(file),
    meck:unload(mnesia),
    meck:unload(certLib),
    
    ok.

handle_sec_credu_nodecredential_unsubscribe_test() ->
    
    Subscriber = #subscriber{id = 1, select_object = {socket, sel_obj}, subscriptions = []},
    SubscriberWithSubNc = #subscriber{id = 1, select_object = {socket, sel_obj}, subscriptions = [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1}]},
    SubscriberWithSubNc2 = #subscriber{id = 2, select_object = {socket, sel_obj}, subscriptions = [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1}]},
    
    ?assertEqual({{error,id_not_found},#state{subscribers = ?SUBSCRIBERS_EMPTY}},
                 certSecCredu:handle_sec_credu_nodecredential_unsubscribe({1, 1}, #state{subscribers = ?SUBSCRIBERS_EMPTY})),
    
    ?assertEqual({{error, sub_id_not_found},#state{subscribers = [Subscriber]}},
                 certSecCredu:handle_sec_credu_nodecredential_unsubscribe({1, 1}, #state{subscribers = [Subscriber]})),
    
    meck:new(certI, [passthrough, unstick]),
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(file, [unstick, passthrough]),
    meck:new(filelib, [unstick, passthrough]),
    
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    
    ExistingFile = ?NC_CERT_DIR ++ "/" ++ ?NC_CERT_FILENAME("nc1"),
    ExistingFile1  = ?NC_KEY_DIR ++ "/" ++ ?NC_KEY_FILENAME("nc1"),
    meck:expect(file, delete, 1, fun(Path) when Path =:= ExistingFile -> ok; (Path) when Path =:= ExistingFile1 -> ok; (_) -> error end),
    meck:expect(filelib, is_regular, 1, fun(File) when File =:= ExistingFile -> true end),
    
    Nc1 = ?NODE_CREDENTIAL_DN1,
    meck:expect(certI, unsubscribe, fun(NodeCredential, certSecCredu) when NodeCredential =:= Nc1 -> ok; (_, _) -> error end),
    
    Arg1 = {ok, #state{subscribers = [Subscriber]}},
    Arg2 = certSecCredu:handle_sec_credu_nodecredential_unsubscribe({1, 1}, #state{subscribers = [SubscriberWithSubNc]}),
    ?assertEqual(Arg1, Arg2),

    meck:expect(filelib, is_regular, 1, fun(_) -> false end),
    
    Arg3 = {ok, #state{subscribers = [Subscriber]}},
    Arg4 = certSecCredu:handle_sec_credu_nodecredential_unsubscribe({1, 1}, #state{subscribers = [SubscriberWithSubNc]}),
    ?assertEqual(Arg3, Arg4),
    
    meck:unload(certI),
    meck:unload(sysEnv),
    meck:unload(file),
    meck:unload(filelib),
    
    Arg5 = {ok, #state{subscribers = [Subscriber, SubscriberWithSubNc2]}},
    Arg6 = certSecCredu:handle_sec_credu_nodecredential_unsubscribe({1, 1}, #state{subscribers = [SubscriberWithSubNc, SubscriberWithSubNc2]}),
    ?assertEqual(Arg5, Arg6),
    
    ok.

handle_sec_credu_trustcategory_subscribe_test() ->

    Subscriber = #subscriber{id = 1, select_object = {socket, sel_obj}},
    SubscriberWithSubTc2 = #subscriber{id = 2, select_object = {socket, sel_obj}, subscriptions = [#subscription{subscribe_id = 1, mo_ref = ?TRUST_CATEGORY_DN1}]},

%%  subscription unsuccessful, subscriber id is not found   
    ?assertEqual({{error,id_not_found},#state{subscribers = ?SUBSCRIBERS_EMPTY}},
                 certSecCredu:handle_sec_credu_trustcategory_subscribe({1, "tcat1"}, #state{subscribers = ?SUBSCRIBERS_EMPTY})),

    meck:new(certI,[passthrough, unstick]),
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(file, [unstick, passthrough]),
    
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    
    ExistingDir = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("tcat1"),
    meck:expect(file, make_dir, 1, fun(Dir) when Dir =:= ExistingDir -> ok end),
    
    ExistingFile = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("tcat1") ++ "/" ++ ?TC_CERT_FILENAME("1"),
    ExistingFile2 = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("tcat1") ++ "/" ++ ?TC_CERT_FILENAME("2"),
    meck:expect(file, write_file, 2, fun(Path, _Content) when Path =:= ExistingFile -> ok; (Path, _Content) when Path =:= ExistingFile2 -> ok; (_, _) -> error end),
    
    TCat1 = ?TRUST_CATEGORY_DN1,
    meck:expect(certI, get_tcat_certs_and_MoRefs, fun(TrustCategory) when TrustCategory =:= TCat1 -> 
                                                  {ok, [{?TRUSTED_CERTIFICATE_DN1, <<"testTC1">>}, {?TRUSTED_CERTIFICATE_DN2, <<"testTC2">>}]}; (_) -> undefined end),
   
    meck:expect(certI, subscribe, fun(TrustCategory, certSecCredu) when TrustCategory =:= TCat1 -> ok; (_, _) -> error end),
    
%%  subscription successful 
    {{Result, SubscriptionId}, #state{subscribers = [#subscriber{subscriptions = [Subscription]} = SubscriberWithSubTc]}} = 
                certSecCredu:handle_sec_credu_trustcategory_subscribe({1, "tcat1"}, #state{subscribers = [Subscriber]}),

    ?assertEqual(ok, Result),
    ?assertEqual(is_integer(SubscriptionId), true),
    ?assertEqual(1, SubscriberWithSubTc#subscriber.id), 
    ?assertEqual({socket, sel_obj}, SubscriberWithSubTc#subscriber.select_object),  
    ?assertEqual(SubscriptionId, Subscription#subscription.subscribe_id),
    ?assertEqual(?TRUST_CATEGORY_DN1, Subscription#subscription.mo_ref),
 
%%  subscription successful, we already have a subscriber with subscription to specified Trust Category
    meck:expect(certI, subscribe, fun(_, _) -> error end),
    meck:expect(file, make_dir, 1, fun(_) -> error end),
    meck:expect(file, write_file, 2, fun(_, _) -> error end),
   
    {{Result1, SubscriptionId1}, #state{subscribers = [#subscriber{subscriptions = [Subscription1]} = SubscriberWithSubTc1, _Subscriber2]}} = 
                certSecCredu:handle_sec_credu_trustcategory_subscribe({1, "tcat1"}, #state{subscribers = [Subscriber, SubscriberWithSubTc2]}),

    ?assertEqual(ok, Result1),
    ?assertEqual(is_integer(SubscriptionId1), true),
    ?assertEqual(1, SubscriberWithSubTc1#subscriber.id),    
    ?assertEqual({socket, sel_obj}, SubscriberWithSubTc1#subscriber.select_object), 
    ?assertEqual(SubscriptionId1, Subscription1#subscription.subscribe_id),
    ?assertEqual(?TRUST_CATEGORY_DN1, Subscription1#subscription.mo_ref),

%%  subscription successful, but Trust Category is empty
    meck:expect(certI, get_tcat_certs_and_MoRefs, fun(TrustCategory) when TrustCategory =:= TCat1 -> 
                                                  {ok, []}; (_) -> undefined end),
    meck:expect(certI, subscribe, fun(TrustCategory, certSecCredu) when TrustCategory =:= TCat1 -> ok; (_, _) -> error end),

    {{Result2, SubscriptionId2}, #state{subscribers = [#subscriber{subscriptions = [Subscription2]} = SubscriberWithSubTc3]}} = 
                certSecCredu:handle_sec_credu_trustcategory_subscribe({1, "tcat1"}, #state{subscribers = [Subscriber]}),
    
    ?assertEqual(ok, Result2),
    ?assertEqual(is_integer(SubscriptionId2), true),
    ?assertEqual(1, SubscriberWithSubTc3#subscriber.id),
    ?assertEqual({socket, sel_obj}, SubscriberWithSubTc3#subscriber.select_object), 
    ?assertEqual(SubscriptionId2, Subscription2#subscription.subscribe_id),
    ?assertEqual(?TRUST_CATEGORY_DN1, Subscription2#subscription.mo_ref),
    
%%  subscription successful, Trust Category is not installed
    meck:expect(certI, get_tcat_certs_and_MoRefs, fun(_) -> undefined end),
    meck:expect(certI, subscribe, fun(TrustCategory, certSecCredu) when TrustCategory =:= TCat1 -> ok; (_, _) -> error end),

   ?assertEqual({{error,mo_ref_not_found},#state{subscribers = [Subscriber, SubscriberWithSubTc2]}},
                certSecCredu:handle_sec_credu_trustcategory_subscribe({1, "tcat2"}, #state{subscribers = [Subscriber, SubscriberWithSubTc2]})),
    
    meck:unload(certI),
    meck:unload(sysEnv),
    meck:unload(file),
    
    ok.

handle_sec_credu_trustcategory_unsubscribe_test() ->
    
    Subscriber = #subscriber{id = 1, select_object = {socket, sel_obj}, subscriptions = []},
    SubscriberWithSubTc = #subscriber{id = 1, select_object = {socket, sel_obj}, subscriptions = [#subscription{subscribe_id = 1, mo_ref = ?TRUST_CATEGORY_DN1}]},
    SubscriberWithSubTc2 = #subscriber{id = 2, select_object = {socket, sel_obj}, subscriptions = [#subscription{subscribe_id = 1, mo_ref = ?TRUST_CATEGORY_DN1}]},
    
    ?assertEqual({{error,id_not_found},#state{subscribers = ?SUBSCRIBERS_EMPTY}},
                 certSecCredu:handle_sec_credu_trustcategory_unsubscribe({1, 1}, #state{subscribers = ?SUBSCRIBERS_EMPTY})),
    
    ?assertEqual({{error, sub_id_not_found},#state{subscribers = [Subscriber]}},
                 certSecCredu:handle_sec_credu_trustcategory_unsubscribe({1, 1}, #state{subscribers = [Subscriber]})),
    
    meck:new(certI, [passthrough, unstick]),
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(file, [unstick, passthrough]),
    meck:new(filelib, [unstick, passthrough]),
    
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    
    ExistingDir = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("tcat1"),
    meck:expect(file, delete, 1, fun(_File) -> ok end),
    meck:expect(file, del_dir, 1, fun(Dir) when Dir =:= ExistingDir -> ok end),
    meck:expect(file, list_dir_all, fun(Dir) when Dir =:= ExistingDir -> {ok, []} end),
    meck:expect(filelib, is_dir, 1, fun(Dir) when Dir =:= ExistingDir -> true end),
    
    Tcat1 = ?TRUST_CATEGORY_DN1,
    meck:expect(certI, unsubscribe, fun(NodeCredential, certSecCredu) when NodeCredential =:= Tcat1 -> ok; (_, _) -> error end),
    
    Arg1 = {ok, #state{subscribers = [Subscriber]}},
    Arg2 = certSecCredu:handle_sec_credu_trustcategory_unsubscribe({1, 1}, #state{subscribers = [SubscriberWithSubTc]}),
    ?assertEqual(Arg1, Arg2),
    
    meck:expect(filelib, is_dir, 1, fun(_) -> false end),
    meck:expect(file, delete, 1, fun(_File) -> error end),
    meck:expect(file, del_dir, 1, fun(_) -> error end),
    meck:expect(file, list_dir_all, fun(_) -> error end),
    
    Arg3 = {ok, #state{subscribers = [Subscriber]}},
    Arg4 = certSecCredu:handle_sec_credu_trustcategory_unsubscribe({1, 1}, #state{subscribers = [SubscriberWithSubTc]}),
    ?assertEqual(Arg3, Arg4),
    
    meck:unload(certI),
    meck:unload(sysEnv),
    meck:unload(file),
    meck:unload(filelib),
    
    Arg5 = {ok, #state{subscribers = [Subscriber, SubscriberWithSubTc2]}},
    Arg6 = certSecCredu:handle_sec_credu_trustcategory_unsubscribe({1, 1}, #state{subscribers = [SubscriberWithSubTc, SubscriberWithSubTc2]}),
    ?assertEqual(Arg5, Arg6),
    
    ok. 

handle_sec_credu_nodecredential_get_cert_test() ->
    Subscriber = #subscriber{id = 1, select_object = {socket, sel_obj}, subscriptions = []},
    SubscriberWithSubNc = #subscriber{id = 1, select_object = {socket, sel_obj}, subscriptions = [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1}]},
    
    ?assertEqual({{error,id_not_found},#state{subscribers = ?SUBSCRIBERS_EMPTY}},
                 certSecCredu:handle_sec_credu_nodecredential_get_cert({1, 1, pem}, #state{subscribers = ?SUBSCRIBERS_EMPTY})),

    ?assertEqual({{error,id_not_found},#state{subscribers = ?SUBSCRIBERS_EMPTY}},
                 certSecCredu:handle_sec_credu_nodecredential_get_cert({1, 1, filepath}, #state{subscribers = ?SUBSCRIBERS_EMPTY})),
    
    ?assertEqual({{error, sub_id_not_found},#state{subscribers = [Subscriber]}},
                 certSecCredu:handle_sec_credu_nodecredential_get_cert({1, 1, pem}, #state{subscribers = [Subscriber]})),
    
    ?assertEqual({{error, sub_id_not_found},#state{subscribers = [Subscriber]}},
                 certSecCredu:handle_sec_credu_nodecredential_get_cert({1, 1, filepath}, #state{subscribers = [Subscriber]})),
    
    meck:new(certI, [passthrough, unstick]),
    Nc1 = ?NODE_CREDENTIAL_DN1,
    meck:expect(certI, get_cert, fun(NodeCredential, pem) when NodeCredential =:= Nc1 -> {ok, <<"NodeCert1">>, <<"PrivKey1">>}; (_, _) -> error end),

    ?assertEqual({{ok,<<"NodeCert1">>},#state{subscribers = [SubscriberWithSubNc]}},
                 certSecCredu:handle_sec_credu_nodecredential_get_cert({1, 1, pem}, #state{subscribers = [SubscriberWithSubNc]})),

    meck:expect(certI, get_cert, fun (_, _) -> error end),
    
    ?assertEqual({{error, nc_not_installed},#state{subscribers = [SubscriberWithSubNc]}},
                 certSecCredu:handle_sec_credu_nodecredential_get_cert({1, 1, pem}, #state{subscribers = [SubscriberWithSubNc]})),

    meck:unload(certI),

    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(filelib, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    ExistingFile = ?NC_CERT_DIR ++ "/" ++ ?NC_CERT_FILENAME("nc1"),
    ExistingFileBin = list_to_binary(ExistingFile),
    meck:expect(filelib, is_regular, 1, fun(File) when File =:= ExistingFile -> true end),
    
    ?assertEqual({{ok,ExistingFileBin},#state{subscribers = [SubscriberWithSubNc]}},
                 certSecCredu:handle_sec_credu_nodecredential_get_cert({1, 1, filepath}, #state{subscribers = [SubscriberWithSubNc]})),

    meck:expect(filelib, is_regular, 1, fun(_) -> false end),

    ?assertEqual({{error, nc_not_installed},#state{subscribers = [SubscriberWithSubNc]}},
                 certSecCredu:handle_sec_credu_nodecredential_get_cert({1, 1, filepath}, #state{subscribers = [SubscriberWithSubNc]})),
    
    meck:unload(filelib),
    meck:unload(sysEnv),
    
    ok.

handle_sec_credu_nodecredential_get_key_test() ->
    Subscriber = #subscriber{id = 1, select_object = {socket, sel_obj}, subscriptions = []},
    SubscriberWithSubNc = #subscriber{id = 1, select_object = {socket, sel_obj}, subscriptions = [#subscription{subscribe_id = 1, mo_ref = ?NODE_CREDENTIAL_DN1}]},
    
    ?assertEqual({{error,id_not_found},#state{subscribers = ?SUBSCRIBERS_EMPTY}},
                 certSecCredu:handle_sec_credu_nodecredential_get_key({1, 1, pem}, #state{subscribers = ?SUBSCRIBERS_EMPTY})),

    ?assertEqual({{error,id_not_found},#state{subscribers = ?SUBSCRIBERS_EMPTY}},
                 certSecCredu:handle_sec_credu_nodecredential_get_key({1, 1, filepath}, #state{subscribers = ?SUBSCRIBERS_EMPTY})),
    
    ?assertEqual({{error, sub_id_not_found},#state{subscribers = [Subscriber]}},
                 certSecCredu:handle_sec_credu_nodecredential_get_key({1, 1, pem}, #state{subscribers = [Subscriber]})),
    
    ?assertEqual({{error, sub_id_not_found},#state{subscribers = [Subscriber]}},
                 certSecCredu:handle_sec_credu_nodecredential_get_key({1, 1, filepath}, #state{subscribers = [Subscriber]})),

    meck:new(certI, [passthrough, unstick]),
    Nc1 = ?NODE_CREDENTIAL_DN1,
    meck:expect(certI, get_cert, fun(NodeCredential, pem) when NodeCredential =:= Nc1 -> {ok, <<"NodeCert1">>, <<"PrivKey1">>}; (_, _) -> error end),

    ?assertEqual({{ok,<<"PrivKey1">>},#state{subscribers = [SubscriberWithSubNc]}},
                 certSecCredu:handle_sec_credu_nodecredential_get_key({1, 1, pem}, #state{subscribers = [SubscriberWithSubNc]})),

    meck:expect(certI, get_cert, fun (_, _) -> error end),
    
    ?assertEqual({{error, nc_not_installed},#state{subscribers = [SubscriberWithSubNc]}},
                 certSecCredu:handle_sec_credu_nodecredential_get_key({1, 1, pem}, #state{subscribers = [SubscriberWithSubNc]})),

    meck:unload(certI),

    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(filelib, [unstick, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    ExistingFile = ?NC_KEY_DIR ++ "/" ++ ?NC_KEY_FILENAME("nc1"),
    ExistingFileBin = list_to_binary(ExistingFile),
    meck:expect(filelib, is_regular, 1, fun(File) when File =:= ExistingFile -> true end),
    
    ?assertEqual({{ok,ExistingFileBin},#state{subscribers = [SubscriberWithSubNc]}},
                 certSecCredu:handle_sec_credu_nodecredential_get_key({1, 1, filepath}, #state{subscribers = [SubscriberWithSubNc]})),

    meck:expect(filelib, is_regular, 1, fun(_) -> false end),

    ?assertEqual({{error, nc_not_installed},#state{subscribers = [SubscriberWithSubNc]}},
                 certSecCredu:handle_sec_credu_nodecredential_get_key({1, 1, filepath}, #state{subscribers = [SubscriberWithSubNc]})),
    
    meck:unload(filelib),
    meck:unload(sysEnv),
    
    ok.

handle_sec_credu_trustcategory_get_test() ->   
    Subscriber = #subscriber{id = 1, select_object = {socket, sel_obj}, subscriptions = []},
    SubscriberWithSubTc = #subscriber{id = 1, select_object = {socket, sel_obj}, subscriptions = [#subscription{subscribe_id = 1, mo_ref = ?TRUST_CATEGORY_DN1}]},

    ?assertEqual({{error,id_not_found},#state{subscribers = ?SUBSCRIBERS_EMPTY}},
                 certSecCredu:handle_sec_credu_trustcategory_get({1, 1}, #state{subscribers = ?SUBSCRIBERS_EMPTY})),
    
    ?assertEqual({{error, sub_id_not_found},#state{subscribers = [Subscriber]}},
                 certSecCredu:handle_sec_credu_trustcategory_get({1, 1}, #state{subscribers = [Subscriber]})),
                 
    meck:new(certI, [passthrough, unstick]),
    meck:expect(certI, get_tcat_certs_and_MoRefs, fun(_) -> undefined end),
    
    ?assertEqual({{error, tcat_empty},#state{subscribers = [SubscriberWithSubTc]}},
                 certSecCredu:handle_sec_credu_trustcategory_get({1, 1}, #state{subscribers = [SubscriberWithSubTc]})),
 
    TCat1 = ?TRUST_CATEGORY_DN1,
    meck:expect(certI, get_tcat_certs_and_MoRefs, fun(TrustCategory) when TrustCategory =:= TCat1 -> {ok, []}; (_) -> undefined end),
    
    ?assertEqual({{error, tcat_empty},#state{subscribers = [SubscriberWithSubTc]}},
                 certSecCredu:handle_sec_credu_trustcategory_get({1, 1}, #state{subscribers = [SubscriberWithSubTc]})),
    
    meck:expect(certI, get_tcat_certs_and_MoRefs, fun(TrustCategory) when TrustCategory =:= TCat1 -> 
                                                  {ok, [{?TRUSTED_CERTIFICATE_DN1, <<"testTC1">>}, {?TRUSTED_CERTIFICATE_DN2, <<"testTC2">>}]}; (_) -> undefined end),
   

    meck:new(sysEnv, [non_strict, passthrough]),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    
    ExistingDir = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("tcat1"),
    TrustCertId1 = "1",
    Tc1Filename = ?TC_CERT_FILENAME(TrustCertId1),
    TrustCertId2 = "2",
    Tc2Filename = ?TC_CERT_FILENAME(TrustCertId2),
    ?assertEqual({{ok, 2, ExistingDir, [{TrustCertId1, Tc1Filename, <<"testTC1">>}, {TrustCertId2, Tc2Filename, <<"testTC2">>}]},#state{subscribers = [SubscriberWithSubTc]}},
                 certSecCredu:handle_sec_credu_trustcategory_get({1, 1}, #state{subscribers = [SubscriberWithSubTc]})),

    meck:unload(certI),   
    meck:unload(sysEnv),   
    
    ok.

cert_event_test() ->
    MoRef = ?NODE_CREDENTIAL_DN1,
    meck:new(timer,[ passthrough, unstick]),
    meck:new(sysInitI, [non_strict, passthrough]),
    meck:expect(sysInitI, info_msg, 2, fun(_Format, _Args) -> ok end),
    meck:expect(timer, apply_after, fun(?CERT_EVENT_OFFSET, gen_server, cast, [certSecCredu, {cert_event, MoReference}]) 
                                         when MoReference =:= MoRef-> ok end),
   
    ?assertEqual(ok, certSecCredu:cert_event(MoRef)),
    meck:unload(timer),
    meck:unload(sysInitI),
    ok.

handle_cast_cert_event_test() ->
    MoRef1 = ?NODE_CREDENTIAL_DN1,
    MoRef2 = ?NODE_CREDENTIAL_DN2,
    MoRef3 = ?TRUST_CATEGORY_DN1,
    MoRef4 = ?TRUST_CATEGORY_DN2,
    
    Subscriber = #subscriber{id = 1, 
                             select_object = {socket, sel_obj}, 
                             subscriptions = [#subscription{subscribe_id = 1, mo_ref = MoRef1}, 
                                              #subscription{subscribe_id = 2, mo_ref = MoRef3}]},
    Subscriber1 = #subscriber{id = 1, 
                             select_object = undefined, 
                             subscriptions = [#subscription{subscribe_id = 1, mo_ref = MoRef1}, 
                                              #subscription{subscribe_id = 2, mo_ref = MoRef3}]},
    
    meck:new(sysInitI, [non_strict, passthrough]),
    meck:expect(sysInitI, info_msg, 2, fun(_Format, _Args) -> ok end),
    meck:new(certLib, [non_strict, passthrough]),   
    meck:expect(certLib, decode_moref, 1, fun(MoRef) when MoRef =:= MoRef1 orelse
                                                          MoRef =:= MoRef2 -> {nc, {"1","1","1","1","nc1"}};
                                             (MoRef) when MoRef =:= MoRef3 orelse
                                                          MoRef =:= MoRef4 -> {tcat, irrelevant} end),

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NodeCredential event reception
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
   
%%cert_event received for NC MO that has no subscribed user, directory structure must remain intact

    ?assertEqual({noreply,  #state{subscribers = [Subscriber]}},certSecCredu:handle_cast({cert_event, MoRef2}, #state{subscribers = [Subscriber]})),

%cert_event received for NC MO with subscribed users, directory structure is updated but event is not sent towards user because selection object is undefined

    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(filelib, [passthrough, unstick]),
    meck:new(file, [passthrough, unstick]),
    meck:new(certI, [passthrough, unstick]),
    meck:new(mnesia, [passthrough, non_strict]),
    
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    
    ExistingFile = ?NC_CERT_DIR ++ "/" ++ ?NC_CERT_FILENAME("nc1"),
    ExistingFile1  = ?NC_KEY_DIR ++ "/" ++ ?NC_KEY_FILENAME("nc1"),
    
    meck:expect(filelib, is_regular, 1, fun(File) when File =:= ExistingFile -> true end),
    meck:expect(file, delete, 1, fun(Path) when Path =:= ExistingFile -> ok; (Path) when Path =:= ExistingFile1 -> ok; (_) -> error end),
    meck:expect(certI, get_cert, fun(NodeCredential, pem) when NodeCredential =:= MoRef1 -> {ok, "NodeCertUpdated", "PrivKeyUpdated"}; (_, _) -> error end),
    meck:expect(file, write_file, 2, fun(Path, _Content) when Path =:= ExistingFile -> ok; (Path, _Content) when Path =:= ExistingFile1 -> ok; (_, _) -> error end),
 
    Nc1Key = {"1","1","1","1","nc1"},
    meck:expect(mnesia, dirty_read, fun({nodeCredential, Key}) when Key =:= Nc1Key -> [ok]; (_) -> error end),
    
    ?assertEqual({noreply,  #state{subscribers = [Subscriber1]}},certSecCredu:handle_cast({cert_event, MoRef1}, #state{subscribers = [Subscriber1]})),

    %cert_event received for NC MO with subscribed users, directory structure is updated and NC event is sent towards user
    meck:new(gen_tcp, [passthrough, unstick]),
    meck:new(inet, [passthrough, unstick]),
    
    SubscriptionId1 = 1,
    LengthNC1 = 3,
    BinaryNC1 = <<"nc1">>,
    NodeCredentialEvent = <<?SEC_CREDU_EVENT_SIG:4/native-unsigned-integer-unit:8, ?NC_EVENT:4/native-unsigned-integer-unit:8, 
                            SubscriptionId1:4/native-unsigned-integer-unit:8, LengthNC1:4/native-unsigned-integer-unit:8,
                            BinaryNC1/binary>>,

    meck:expect(gen_tcp, send, 2, fun({socket, sel_obj}, Message) when Message =:= NodeCredentialEvent -> ok end),
    meck:expect(inet, setopts, 2, fun({socket, sel_obj}, [{active, once}]) -> ok end),

    ?assertEqual({noreply,  #state{subscribers = [Subscriber]}},certSecCredu:handle_cast({cert_event, MoRef1}, #state{subscribers = [Subscriber]})),

    meck:unload(gen_tcp),
    meck:unload(inet),    
    meck:unload(sysEnv),
    meck:unload(file),
    meck:unload(filelib),
    meck:unload(certI),
    meck:unload(mnesia),

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TrustCategory event reception
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

%%cert_event received for TCat MO that has no subscribed user, directory structure must remain intact   
   ?assertEqual({noreply,  #state{subscribers = [Subscriber]}},certSecCredu:handle_cast({cert_event, MoRef4}, #state{subscribers = [Subscriber]})),
    

%cert_event received for TCat MO with subscribed users, directory structure is updated but event is not sent towards user because selection object is undefined

    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(filelib, [passthrough, unstick]),
    meck:new(file, [passthrough, unstick]),
    meck:new(certI, [passthrough, unstick]),
    
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    ExistingDir = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("tcat1"),
    meck:expect(filelib, is_dir, 1, fun(Dir) when Dir =:= ExistingDir -> true end),
    meck:expect(file, delete, 1, fun(_File) -> ok end),
    meck:expect(file, del_dir, 1, fun(Dir) when Dir =:= ExistingDir -> ok end),
    meck:expect(file, list_dir_all, fun(Dir) when Dir =:= ExistingDir -> {ok, []} end),
    meck:expect(file, make_dir, 1, fun(Dir) when Dir =:= ExistingDir -> ok end),
    meck:expect(certI, get_tcat_certs_and_MoRefs, fun(TrustCategory) when TrustCategory =:= MoRef3 -> 
                                                  {ok, [{?TRUSTED_CERTIFICATE_DN1, <<"updatedtestTC1">>}, {?TRUSTED_CERTIFICATE_DN2, <<"testTC2">>}]}; (_) -> undefined end),
 
    ExistingFile2 = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("tcat1") ++ "/" ++ ?TC_CERT_FILENAME("1"),
    ExistingFile3 = ?TCAT_ROOT_DIR ++ ?TCAT_DIRNAME("tcat1") ++ "/" ++ ?TC_CERT_FILENAME("2"),
    meck:expect(file, write_file, 2, fun(Path, _Content) when Path =:= ExistingFile2 -> ok; (Path, _Content) when Path =:= ExistingFile3 -> ok; (_, _) -> error end),
    
    ?assertEqual({noreply,  #state{subscribers = [Subscriber1]}},certSecCredu:handle_cast({cert_event, MoRef3}, #state{subscribers = [Subscriber1]})),

%cert_event received for TCat MO with subscribed users, directory structure is updated and TCAT event is sent towards user
    meck:new(gen_tcp, [passthrough, unstick]),
    meck:new(inet, [passthrough, unstick]),
    
    SubscriptionId2 = 2,
    LengthTcat1 = 5,
    BinaryTCat1 = <<"tcat1">>,
    TrustCategoryEvent = <<?SEC_CREDU_EVENT_SIG:4/native-unsigned-integer-unit:8, ?TCAT_EVENT:4/native-unsigned-integer-unit:8, 
                            SubscriptionId2:4/native-unsigned-integer-unit:8, LengthTcat1:4/native-unsigned-integer-unit:8,
                            BinaryTCat1/binary>>,

    meck:expect(gen_tcp, send, 2, fun({socket, sel_obj}, Message) when Message =:= TrustCategoryEvent -> ok end),
    meck:expect(inet, setopts, 2, fun({socket, sel_obj}, [{active, once}]) -> ok end),

    ?assertEqual({noreply,  #state{subscribers = [Subscriber]}},certSecCredu:handle_cast({cert_event, MoRef3}, #state{subscribers = [Subscriber]})),

    meck:unload(gen_tcp),
    meck:unload(inet), 
    meck:unload(sysEnv),
    meck:unload(file),
    meck:unload(filelib),
    meck:unload(certI),
    
    meck:unload(sysInitI),
    meck:unload(certLib),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CEC related unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cec_setup_process_test() ->
    Pid = spawn_link(fun() -> receive
                            _ -> exit(self(), normal)
                        end
                     end),
    register(certSecCredu, Pid),
    
    ?assertEqual(Pid, certSecCredu:cec_setup(?SOCKET)),
    
    Pid ! stop,
    timer:sleep(100),
    
    ?assertThrow(no_certSecCredu, certSecCredu:cec_setup(?SOCKET)),
    
    ok.

cec_takeover_test() ->
    ?assertEqual(ok, certSecCredu:cec_takeover(?SOCKET)),
    ok.

handle_cec_takeover_test() ->
    meck:new(inet, [passthrough, unstick]),
    meck:new(sysInitI, [non_strict, passthrough]),
    meck:expect(sysInitI, info_msg, 2, fun(_Format, _Args) -> ok end),
    meck:expect(inet, setopts, 2, fun(_, [{active, once}]) -> ok end),
    
    Arg1 = {noreply, #state{}},
    
    ?assertEqual(Arg1, certSecCredu:handle_cast({cec_takeover, {socket, sel_obj}}, #state{})),
    
    meck:unload(inet),
    meck:unload(sysInitI),
    ok.

send_event_test() ->
    meck:new(gen_tcp, [passthrough, unstick]),
    meck:new(inet, [passthrough, unstick]),
    
    meck:expect(gen_tcp, send, 2, fun(?SOCKET, ?MSG) -> ok end),
    
    ?assertEqual(ok, certSecCredu:send_event(?SOCKET, ?MSG)),
    
    meck:unload(gen_tcp),
    meck:unload(inet),
    ok.

send_response_test() ->
    meck:new(gen_tcp, [passthrough, unstick]),
    meck:new(inet, [passthrough, unstick]),
    
    meck:expect(gen_tcp, send, 2, fun(?SOCKET, ?MSG) -> ok end),
    meck:expect(inet, setopts, 2, fun(?SOCKET, [{active, once}]) -> ok end),
    
    ?assertEqual(ok, certSecCredu:send_response(?SOCKET, ?MSG)),
    
    meck:unload(gen_tcp),
    meck:unload(inet),
    ok.

serialize_integer_test() ->
    serialize_integer32_test().

serialize_integer32_test() ->
    Integer1 = 10,
    Integer2 = "10",
    Integer3 = <<10>>,
    Message1 = <<"test">>,
    Message2 = 20,
    Message3 = <<>>,
    
    ?assertEqual(<<Message1/binary, Integer1:4/native-unsigned-integer-unit:8>>, certSecCredu:serialize_integer32(Integer1, Message1)),
    ?assertEqual(<<Message3/binary, Integer1:4/native-unsigned-integer-unit:8>>, certSecCredu:serialize_integer32(Integer1, Message3)),
    ?assertError(_, certSecCredu:serialize_integer32(Integer2, Message2)),
    ?assertError(_, certSecCredu:serialize_integer32(Integer3, Message1)),

    ok.

serialize_string_result_test() ->
    Result1 = "10",
    Result2 = 10,
    Result3 = <<10>>,
    Result1Len = string:len(Result1),
    Result1Bin = list_to_binary(Result1),
    Message1 = <<"test">>,
    Message2 = 20,
    Message3 = <<>>,
    
    ?assertEqual(<<Message1/binary, Result1Len:4/native-unsigned-integer-unit:8, Result1Bin/binary>>, certSecCredu:serialize_string_result(Result1, Message1)),
    ?assertEqual(<<Message3/binary, Result1Len:4/native-unsigned-integer-unit:8, Result1Bin/binary>>, certSecCredu:serialize_string_result(Result1, Message3)),
    ?assertError(_, certSecCredu:serialize_integer(Result2, Message2)),
    ?assertError(_, certSecCredu:serialize_integer(Result3, Message3)),
    
    ok.

serialize_binary_result_test() ->
    Result1 = <<"10">>,
    Result2 = <<10>>,
    Result3 = 10,
    Size1 = erlang:byte_size(Result1),
    Size2 = erlang:byte_size(Result2),
    Message1 = <<"test">>,
    Message2 = 20,
    Message3 = <<>>,
    
    ?assertEqual(<<Message1/binary, Size1:4/native-unsigned-integer-unit:8, Result1/binary>>, certSecCredu:serialize_binary_result(Result1, Message1)),
    ?assertEqual(<<Message3/binary, Size2:4/native-unsigned-integer-unit:8, Result2/binary>>, certSecCredu:serialize_binary_result(Result2, Message3)),
    ?assertError(_, certSecCredu:serialize_binary_result(Result2, Message2)),
    ?assertError(_, certSecCredu:serialize_binary_result(Result3, Message3)),
    
    ok.


handle_sec_credu_request_test() ->
    Socket = {socket, sel_obj},
    StateEmpty = #state{},
    Subscriber = #subscriber{id = 1, select_object = {socket, sel_obj}},
    StateNotEmpty = #state{ subscribers = [ Subscriber ]},
    MajorVersion = 1,
    MinorVersion = 1,
    StringA = 65,
    ID = 1,
    NcIdBin = <<65, 66, 67>>,
    NcId = "ABC",
    NcIdLen = string:len(NcId),
    SubId = 2,
    SecCreduFormat = 2,
    SecCreduFormat1 = 1,
    TcatIdBin = <<66,67,68>>,
    TcatId = "BCD",
    TcatIdLen = string:len(TcatId),
    SelectedVersion = "A11",
    NcCert = <<6,5,4>>,
    NcCertFilePath = "/sec_credu_api/certs/nc1.pem",

    NcKey = <<7,6,5>>,
    meck:new(certSecCredu, [passthrough]),
    
    %initialize
    BinaryInitialize = <<StringA:1/native-unsigned-integer-unit:8,     %release code = "A" = [65]
                         MajorVersion:1/native-unsigned-integer-unit:8,
                         MinorVersion:1/native-unsigned-integer-unit:8>>,
    meck:expect(certSecCredu, handle_sec_credu_initialize, 2, fun(_SelectedVersion, _StateEmpty) -> {{ok, ID, SelectedVersion}, StateNotEmpty} end),
    ?assertEqual({{ok, ID, SelectedVersion}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_INITIALIZE, BinaryInitialize, Socket, StateEmpty)),

    %finalize with ok
    BinaryFinalize = <<ID:4/native-unsigned-integer-unit:8>>,
    meck:expect(certSecCredu, handle_sec_credu_finalize, 2, fun(_ID, _StateEmpty) -> {ok, StateNotEmpty} end),
    ?assertEqual({ok, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_FINALIZE, BinaryFinalize, Socket, StateEmpty)),
    %finalize with error
    meck:expect(certSecCredu, handle_sec_credu_finalize, 2, fun(_ID, _StateEmpty) -> {{error, id_not_found}, StateNotEmpty} end),
    ?assertEqual({{error, id_not_found}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_FINALIZE, BinaryFinalize, Socket, StateEmpty)),

    %selection_object_get same answers
    BinarySelectionObjectGet = <<ID:4/native-unsigned-integer-unit:8>>,
    meck:expect(certSecCredu, handle_sec_credu_selectionobject_get, 2, fun({_ID, _Socket}, _StateEmpty) -> {ok, StateNotEmpty} end),
    ?assertEqual({ok, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_SELECTION_OBJECT_GET, BinarySelectionObjectGet, Socket, StateEmpty)),
    meck:expect(certSecCredu, handle_sec_credu_selectionobject_get, 2, fun({_ID, _Socket}, _StateEmpty) -> {{error, id_not_found}, StateNotEmpty} end),
    ?assertEqual({{error, id_not_found}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_SELECTION_OBJECT_GET, BinaryFinalize, Socket, StateEmpty)),

    %nc_subscribe with ok
    BinaryNcSubscribe = <<ID:4/native-unsigned-integer-unit:8,
                          NcIdLen:4/native-unsigned-integer-unit:8,
                          NcIdBin/binary>>,
    meck:expect(certSecCredu, handle_sec_credu_nodecredential_subscribe, 2, fun({_ID, _NcId}, _StateEmpty) -> {{ok, SubId}, StateNotEmpty} end),
    ?assertEqual({{ok, SubId}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_NC_SUBSCRIBE, BinaryNcSubscribe, Socket, StateEmpty)),
    %nc_subscribe with error
    meck:expect(certSecCredu, handle_sec_credu_nodecredential_subscribe, 2, fun({_ID, _NcId}, _StateEmpty) -> {{error, mo_ref_not_found}, StateNotEmpty} end),
    ?assertEqual({{error, mo_ref_not_found}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_NC_SUBSCRIBE, BinaryNcSubscribe, Socket, StateEmpty)),
    
    %nc_unsubscribe with ok
    BinaryNcUnsubscribe = <<ID:4/native-unsigned-integer-unit:8,
                            SubId:4/native-unsigned-integer-unit:8>>,
    meck:expect(certSecCredu, handle_sec_credu_nodecredential_unsubscribe, 2, fun({_ID, _SubId}, _StateEmpty) -> {ok, StateNotEmpty} end),
    ?assertEqual({ok, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_NC_UNSUBSCRIBE, BinaryNcUnsubscribe, Socket, StateEmpty)),
    %nc_unsubscribe with error
    meck:expect(certSecCredu, handle_sec_credu_nodecredential_unsubscribe, 2, fun({_ID, _SubId}, _StateEmpty) -> {{error, id_not_found}, StateNotEmpty} end),
    ?assertEqual({{error, id_not_found}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_NC_UNSUBSCRIBE, BinaryNcUnsubscribe, Socket, StateEmpty)),
    
    %nc_cert_get with ok
    BinaryNcCertGet = <<ID:4/native-unsigned-integer-unit:8,
                        SubId:4/native-unsigned-integer-unit:8,
                        SecCreduFormat:4/native-unsigned-integer-unit:8>>,
    
    meck:expect(certSecCredu, handle_sec_credu_nodecredential_get_cert, 2, fun({_ID, _SubId, _SecCreduFormat}, _StateEmpty) -> {{ok, NcCert}, StateNotEmpty} end),    %<ok, data}
    ?assertEqual({{ok, NcCert}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_NC_CERT_GET, BinaryNcCertGet, Socket, StateEmpty)),

    BinaryNcCertGet1 = <<ID:4/native-unsigned-integer-unit:8,
                        SubId:4/native-unsigned-integer-unit:8,
                        SecCreduFormat1:4/native-unsigned-integer-unit:8>>,

    meck:expect(certSecCredu, handle_sec_credu_nodecredential_get_cert, 2, fun({_ID, _SubId, _SecCreduFormat1}, _StateEmpty) -> {{ok, NcCertFilePath}, StateNotEmpty}end),    %<ok, data}

    ?assertEqual({{ok, NcCertFilePath}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_NC_CERT_GET, BinaryNcCertGet1, Socket, StateEmpty)),
    
    %nc_cert_get with error

    meck:expect(certSecCredu, handle_sec_credu_nodecredential_get_cert, 2, fun({_ID, _SubId, _SecCreduFormat}, _StateEmpty) -> {{error, sub_id_not_found}, StateNotEmpty} end),
    ?assertEqual({{error, sub_id_not_found}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_NC_CERT_GET, BinaryNcCertGet, Socket, StateEmpty)),
    
    %nc_key_get with ok
    BinaryNcKeyGet = <<ID:4/native-unsigned-integer-unit:8,
                       SubId:4/native-unsigned-integer-unit:8,
                       SecCreduFormat:4/native-unsigned-integer-unit:8>>,
    meck:expect(certSecCredu, handle_sec_credu_nodecredential_get_key, 2, fun({_ID, _SubId, _filepath}, _StateEmpty) -> {{ok, NcKey}, StateNotEmpty} end),     %{ok, data}
    ?assertEqual({{ok, NcKey}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_NC_KEY_GET, BinaryNcKeyGet, Socket, StateEmpty)),
    %nc_key_get with error
    meck:expect(certSecCredu, handle_sec_credu_nodecredential_get_key, 2, fun({_ID, _SubId, _SecCreduFormat}, _StateEmpty) -> {{error, sub_id_not_found}, StateNotEmpty} end),
    ?assertEqual({{error, sub_id_not_found}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_NC_KEY_GET, BinaryNcKeyGet, Socket, StateEmpty)),
    
    %tcat_subscribe with ok
    BinaryTcatSubscribe = <<ID:4/native-unsigned-integer-unit:8,
                            TcatIdLen:4/native-unsigned-integer-unit:8,
                            TcatIdBin/binary>>,
    meck:expect(certSecCredu, handle_sec_credu_trustcategory_subscribe, 2, fun({_ID, _TcatId}, _StateEmpty) -> {{ok, SubId}, StateNotEmpty} end),   %{ok, subid}
    ?assertEqual({{ok, SubId}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_TCAT_SUBSCRIBE, BinaryTcatSubscribe, Socket, StateEmpty)),
    %tcat_subscribe with error
    meck:expect(certSecCredu, handle_sec_credu_trustcategory_subscribe, 2, fun({_ID, _TcatId}, _StateEmpty) -> {{error, mo_ref_not_found}, StateNotEmpty} end),
    ?assertEqual({{error, mo_ref_not_found}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_TCAT_SUBSCRIBE, BinaryTcatSubscribe, Socket, StateEmpty)),
    
    %tcat_unsubscribe with ok
    BinaryTcatUnsubscribe = <<ID:4/native-unsigned-integer-unit:8,
                              SubId:4/native-unsigned-integer-unit:8>>,
    meck:expect(certSecCredu, handle_sec_credu_trustcategory_unsubscribe, 2, fun({_ID, _SubId}, _StateEmpty) -> {ok, StateNotEmpty} end),
    ?assertEqual({ok, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_TCAT_UNSUBSCRIBE, BinaryTcatUnsubscribe, Socket, StateEmpty)),
    %tcat_unsubscribe with error
    meck:expect(certSecCredu, handle_sec_credu_trustcategory_unsubscribe, 2, fun({_ID, _SubId}, _StateEmpty) -> {{error, id_not_found}, StateNotEmpty} end),
    ?assertEqual({{error, id_not_found}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_TCAT_UNSUBSCRIBE, BinaryTcatUnsubscribe, Socket, StateEmpty)),
    
    %tcat_get with ok
    BinaryTcatGet = <<ID:4/native-unsigned-integer-unit:8,
                      SubId:4/native-unsigned-integer-unit:8>>,
    BinaryResponseTcatGet = {{ok, 2, "TcatDirName", [{<<"mo_ref1">>, "TcFileName1", <<"pemcert1">>},
                                                     {<<"mo_ref2">>, "TcFileName2", <<"pemcert2">>}]}, StateNotEmpty},
    meck:expect(certSecCredu, handle_sec_credu_trustcategory_get, 2, fun({_ID, _SubId}, _StateEmpty) -> BinaryResponseTcatGet end),
                                                                                          %{ok, Count, TcatDirName, TcatContents :: [{TcId, TcFilename, PemCert }] }

    ?assertEqual(BinaryResponseTcatGet, certSecCredu:handle_sec_credu_request(?REQ_TCAT_GET, BinaryTcatGet, Socket, StateEmpty)),
    %tcat_get with error
    meck:expect(certSecCredu, handle_sec_credu_trustcategory_get, 2, fun({_ID, _SubId}, _StateEmpty) -> {{error, sub_id_not_found}, StateNotEmpty} end),
    ?assertEqual({{error, sub_id_not_found}, StateNotEmpty}, certSecCredu:handle_sec_credu_request(?REQ_TCAT_GET, BinaryTcatGet, Socket, StateEmpty)),

    %unknown request
    ?assertEqual({{error, unknown_request}, StateEmpty}, certSecCredu:handle_sec_credu_request(?REQ_UNKNOWN, BinaryTcatGet, Socket, StateEmpty)),

    meck:unload(certSecCredu),
    ok.

handle_sec_credu_response_test() ->
    StringA = 65,
    ID = 1,
    SubId = 2,
    SelectedVersion = "A11",
    MajorVersion = 1,
    MinorVersion = 1,
    NcCert = <<6,5,4>>,
    NcCertLen = erlang:byte_size(NcCert),
    NcKey = <<7,6,5>>,
    NcKeyLen = erlang:byte_size(NcKey),
    
    %initialize
    BinaryMessageInitialize = <<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_OK:4/native-unsigned-integer-unit:8, ID:4/native-unsigned-integer-unit:8,
                                StringA:1/native-unsigned-integer-unit:8, MajorVersion:1/native-unsigned-integer-unit:8, MinorVersion:1/native-unsigned-integer-unit:8>>,
    ?assertEqual(BinaryMessageInitialize, certSecCredu:handle_sec_credu_response(?REQ_INITIALIZE, {ok, ID, SelectedVersion})),
    
    %finalize, selection_object_get, nc_unsubscribe, tcat_unsubscribe
    BinaryMessageOk = <<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_OK:4/native-unsigned-integer-unit:8>>,
    ?assertEqual(BinaryMessageOk, certSecCredu:handle_sec_credu_response(?REQ_FINALIZE, ok)),
    ?assertEqual(BinaryMessageOk, certSecCredu:handle_sec_credu_response(?REQ_SELECTION_OBJECT_GET, ok)),
    ?assertEqual(BinaryMessageOk, certSecCredu:handle_sec_credu_response(?REQ_NC_UNSUBSCRIBE, ok)),
    ?assertEqual(BinaryMessageOk, certSecCredu:handle_sec_credu_response(?REQ_TCAT_UNSUBSCRIBE, ok)),
    %errors
    BinaryMessageError = <<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_ERR_ID_NOT_FOUND:4/native-unsigned-integer-unit:8>>,
    
    ?assertEqual(BinaryMessageError, certSecCredu:handle_sec_credu_response(?REQ_FINALIZE, {error, id_not_found})),
    ?assertEqual(BinaryMessageError, certSecCredu:handle_sec_credu_response(?REQ_SELECTION_OBJECT_GET, {error, id_not_found})),
    ?assertEqual(BinaryMessageError, certSecCredu:handle_sec_credu_response(?REQ_NC_UNSUBSCRIBE, {error, id_not_found})),
    ?assertEqual(BinaryMessageError, certSecCredu:handle_sec_credu_response(?REQ_TCAT_UNSUBSCRIBE, {error, id_not_found})),
    ?assertEqual(BinaryMessageError, certSecCredu:handle_sec_credu_response(?REQ_NC_SUBSCRIBE, {error, id_not_found})),
    ?assertEqual(BinaryMessageError, certSecCredu:handle_sec_credu_response(?REQ_TCAT_SUBSCRIBE, {error, id_not_found})),
    ?assertEqual(BinaryMessageError, certSecCredu:handle_sec_credu_response(?REQ_NC_CERT_GET, {error, id_not_found})),
    ?assertEqual(BinaryMessageError, certSecCredu:handle_sec_credu_response(?REQ_NC_KEY_GET, {error, id_not_found})),
    ?assertEqual(BinaryMessageError, certSecCredu:handle_sec_credu_response(?REQ_TCAT_GET, {error, id_not_found})),
    
    %nc_subscribe, tcat_subscribe
    BinaryMessageNcSubscribeOk = <<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_OK:4/native-unsigned-integer-unit:8, SubId:4/native-unsigned-integer-unit:8>>,
    %<<resp sign, ok, subid>>
    ?assertEqual(BinaryMessageNcSubscribeOk, certSecCredu:handle_sec_credu_response(?REQ_NC_SUBSCRIBE, {ok, SubId})),
    ?assertEqual(BinaryMessageNcSubscribeOk, certSecCredu:handle_sec_credu_response(?REQ_TCAT_SUBSCRIBE, {ok, SubId})),

    %nc_cert_get, nc_key_get
    BinaryMessageNcCertGetOk = <<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_OK:4/native-unsigned-integer-unit:8, NcCertLen:4/native-unsigned-integer-unit:8,
                                 NcCert:NcCertLen/binary>>,
    BinaryMessageNcKeyGetOk = <<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_OK:4/native-unsigned-integer-unit:8, NcKeyLen:4/native-unsigned-integer-unit:8,
                                 NcKey:NcKeyLen/binary>>,
    %<<resp sign, ok, size, message>>
    ?assertEqual(BinaryMessageNcCertGetOk, certSecCredu:handle_sec_credu_response(?REQ_NC_CERT_GET, {ok, NcCert})),
    ?assertEqual(BinaryMessageNcKeyGetOk, certSecCredu:handle_sec_credu_response(?REQ_NC_KEY_GET, {ok, NcKey})),

    %tcat_get
    BinaryResponseTcatGet = {ok, 2, "TcatDirName", [{"mo_ref1", "TcFileName1", <<"pemcert1">>},
                                                     {"mo_ref2", "TcFileName2", <<"pemcert2">>}]},
    TcatDirNameLen = string:len("TcatDirName"),
    TcatDirNameBin = list_to_binary("TcatDirName"),
    TcFileNameBinary1 = list_to_binary("TcFileName1"),
    TcFileNameLen1 = string:len("TcFileName1"),
    TcIdLen1 = erlang:byte_size(<<"mo_ref1">>),
    TcCertLen1 = erlang:byte_size(<<"pemcert1">>),
    TcFileNameBinary2 = list_to_binary("TcFileName2"),
    TcFileNameLen2 = string:len("TcFileName2"),
    TcIdLen2 = erlang:byte_size(<<"mo_ref2">>),
    TcCertLen2 = erlang:byte_size(<<"pemcert2">>),
    Data1 = <<TcIdLen1:4/native-unsigned-integer-unit:8, <<"mo_ref1">>/binary, TcFileNameLen1:4/native-unsigned-integer-unit:8, TcFileNameBinary1/binary, TcCertLen1:4/native-unsigned-integer-unit:8, <<"pemcert1">>/binary>>,
    Data2 = <<TcIdLen2:4/native-unsigned-integer-unit:8, <<"mo_ref2">>/binary, TcFileNameLen2:4/native-unsigned-integer-unit:8, TcFileNameBinary2/binary, TcCertLen2:4/native-unsigned-integer-unit:8, <<"pemcert2">>/binary>>,
    TcDataLen1 = erlang:byte_size(Data1),
    TcDataLen2 = erlang:byte_size(Data2),

    BinaryMessageTcatGet = <<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_OK:4/native-unsigned-integer-unit:8, 2:4/native-unsigned-integer-unit:8,
                             TcatDirNameLen:4/native-unsigned-integer-unit:8, TcatDirNameBin/binary,
                             TcDataLen1:4/native-unsigned-integer-unit:8, Data1/binary, TcDataLen2:4/native-unsigned-integer-unit:8, Data2/binary>>,
                            % <<resp sign, ok, count, namelen, namebin, list of: datalen, tcidlen, tcid, tcfilenamelen, tcfilenamebin, pemcertlen, pemcert>>
    ?assertEqual(BinaryMessageTcatGet, certSecCredu:handle_sec_credu_response(?REQ_TCAT_GET, BinaryResponseTcatGet)),

    BinaryMessageUnknownReq = <<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_ERR_UNKNOWN_REQ_TYPE:4/native-unsigned-integer-unit:8>>,
    
    ?assertEqual(BinaryMessageUnknownReq, certSecCredu:handle_sec_credu_response(?REQ_UNKNOWN, {error, unknown_request})),
    
    ok.

tcat_list_to_binary_test() ->
    ListOfTcat = [{"tc1", "tc1", <<1>>},
                  {"tc2", "tc2", <<2>>},
                  {"tc3", "tc3", <<3>>}],
    TcIdLen1 = erlang:byte_size(<<"tc1">>),
    TcFileNameLen1 = string:len("tc1"),
    TcFileNameBin1 = list_to_binary("tc1"),
    TcPemCertLen1 = erlang:byte_size(<<1>>),
    Data1 = <<TcIdLen1:4/native-unsigned-integer-unit:8, <<"tc1">>/binary, TcFileNameLen1:4/native-unsigned-integer-unit:8, TcFileNameBin1/binary, TcPemCertLen1:4/native-unsigned-integer-unit:8, <<1>>/binary>>,
    DataLen1 = erlang:byte_size(Data1),
    
    TcIdLen2 = erlang:byte_size(<<"tc2">>),
    TcFileNameLen2 = string:len("tc2"),
    TcFileNameBin2 = list_to_binary("tc2"),
    TcPemCertLen2 = erlang:byte_size(<<2>>),
    Data2 = <<TcIdLen2:4/native-unsigned-integer-unit:8, <<"tc2">>/binary, TcFileNameLen2:4/native-unsigned-integer-unit:8, TcFileNameBin2/binary, TcPemCertLen2:4/native-unsigned-integer-unit:8, <<2>>/binary>>,
    DataLen2 = erlang:byte_size(Data2),
    
    TcIdLen3 = erlang:byte_size(<<"tc3">>),
    TcFileNameLen3 = string:len("tc3"),
    TcFileNameBin3 = list_to_binary("tc3"),
    TcPemCertLen3 = erlang:byte_size(<<3>>), 
    Data3 = <<TcIdLen3:4/native-unsigned-integer-unit:8, <<"tc3">>/binary, TcFileNameLen3:4/native-unsigned-integer-unit:8, TcFileNameBin3/binary, TcPemCertLen3:4/native-unsigned-integer-unit:8, <<3>>/binary>>,
    DataLen3 = erlang:byte_size(Data3),
    
    BinaryMessage = <<DataLen1:4/native-unsigned-integer-unit:8, TcIdLen1:4/native-unsigned-integer-unit:8, <<"tc1">>/binary, TcFileNameLen1:4/native-unsigned-integer-unit:8,
                      TcFileNameBin1/binary, TcPemCertLen1:4/native-unsigned-integer-unit:8, <<1>>/binary,
                      DataLen2:4/native-unsigned-integer-unit:8, TcIdLen2:4/native-unsigned-integer-unit:8, <<"tc2">>/binary, TcFileNameLen2:4/native-unsigned-integer-unit:8,
                      TcFileNameBin2/binary, TcPemCertLen2:4/native-unsigned-integer-unit:8, <<2>>/binary,
                      DataLen3:4/native-unsigned-integer-unit:8, TcIdLen3:4/native-unsigned-integer-unit:8, <<"tc3">>/binary, TcFileNameLen3:4/native-unsigned-integer-unit:8,
                      TcFileNameBin3/binary, TcPemCertLen3:4/native-unsigned-integer-unit:8, <<3>>/binary>>,

    ?assertEqual(BinaryMessage, certSecCredu:tcat_list_to_binary(ListOfTcat, <<>>)),
    
    ok.

version_string_to_tuple_test() ->
    StringExample = "A11",
    ?assertEqual({ok, {65, 1, 1}}, certSecCredu:version_string_to_tuple(StringExample)),
    
    ok.

version_tuple_to_string_test() ->
    TupleExample = {65, 1, 1},
    ?assertEqual({ok, "A11"}, certSecCredu:version_tuple_to_string(TupleExample)),
    
    ok.

get_subscriber_by_comm_socket_test() ->
    ?assertEqual({ok, ?SUBSCRIBER4}, certSecCredu:get_subscriber_by_comm_socket(?SOCKET, ?SUBSCRIBERS2_SUBS)),
    ?assertEqual({error, sub_id_not_found}, certSecCredu:get_subscriber_by_comm_socket(?SOCKET, ?SUBSCRIBERS1_SUBS)),
    ok.

unsubscribe_from_all_test() ->
    Subscriptions = [#subscription{subscribe_id = 11, mo_ref = ?NODE_CREDENTIAL_DN2}, #subscription{subscribe_id = 21, mo_ref = ?NODE_CREDENTIAL_DN3}],
    SubId = 1,
    Subscriber      = #subscriber{id = SubId, select_object = {socket, sel_obj}, subscriptions = Subscriptions, communication_socket = 123},
    EmptySubscriber = #subscriber{id = SubId, select_object = {socket, sel_obj}, subscriptions = [],            communication_socket = 123},
    State    = #state{ subscribers = [Subscriber, ?SUBSCRIBER4]},
    NewState = #state{ subscribers = [EmptySubscriber, ?SUBSCRIBER4]},

    meck:new(sysInitI, [passthrough, non_strict]),
    meck:expect(sysInitI, info_msg, 2, fun(_String, _ListOfEls) -> ok end),

    meck:new(certSecCredu, [passthrough, unstick]),
    meck:expect(certSecCredu, handle_sec_credu_unsubscribe, 4, fun(SubId1, SubsId, State1, nodeCredential) ->
                                                               {ok, Sub1} = certSecCredu:get_subscriber_by_id(SubId1, State1#state.subscribers),
                                                               Subscriptions1 = Sub1#subscriber.subscriptions,
                                                               {ok, Subscription} = certSecCredu:get_subscription_by_id(Subscriptions1, SubsId),
                                                               NewSubs = lists:delete(Subscription, Subscriptions1),
                                                               Sub2 = #subscriber{ id = SubId1, select_object = Sub1#subscriber.select_object, subscriptions = NewSubs, communication_socket = Sub1#subscriber.communication_socket},
                                                               Subs2 = lists:delete(Sub1, State1#state.subscribers),
                                                               Subs3 = [Sub2 | Subs2],
                                                               {ok, #state{subscribers = Subs3}} end),
    ?assertEqual({ok, NewState}, certSecCredu:unsubscribe_from_all(SubId, State)),
    meck:unload(sysInitI),
    meck:unload(certSecCredu),
    ok.



finalize_subscriber_test() ->
    Subscriptions = [#subscription{subscribe_id = 11, mo_ref = ?NODE_CREDENTIAL_DN2}, #subscription{subscribe_id = 21, mo_ref = ?NODE_CREDENTIAL_DN3}],
    SubId = 1,
    Subscriber      = #subscriber{id = SubId, select_object = {socket, sel_obj}, subscriptions = Subscriptions, communication_socket = 123},
    EmptySubscriber = #subscriber{id = SubId, select_object = {socket, sel_obj}, subscriptions = [],            communication_socket = 123},
    State    = #state{ subscribers = [Subscriber, ?SUBSCRIBER4]},
    NewState1 = #state{ subscribers = [EmptySubscriber, ?SUBSCRIBER4]},
    NewState2 = #state{ subscribers = [?SUBSCRIBER4]},

    meck:new(sysInitI, [passthrough, non_strict]),
    meck:expect(sysInitI, info_msg, 2, fun(_String, _ListOfEls) -> ok end),
    
    meck:new(gen_tcp,[ passthrough, unstick]),
    meck:expect(gen_tcp, close, 1, fun(_Sock) -> ok end),
    meck:expect(gen_tcp, send, 2, fun(_Sock, _Msg) -> ok end),

    meck:new(certSecCredu, [passthrough, unstick]),
    meck:expect(certSecCredu, unsubscribe_from_all, 2, fun(_SubId, _State) -> {ok, NewState1} end),

    ?assertEqual({ok, NewState2}, certSecCredu:finalize_subscriber(Subscriber, State)),
    meck:unload(sysInitI),
    meck:unload(certSecCredu),
    meck:unload(gen_tcp),
    ok.

activate_test() ->
    ?assertEqual(ok, certSecCredu:activate()),
    ok.

handle_cec_register_test() ->
    
    meck:new(cec, [passthrough, non_strict]),
    meck:expect(cec, register, 2, fun(<<"SEC_CREDU_API">>, certSecCredu) -> ok end),
    
    Arg1 = {noreply, #state{}},
    
    ?assertEqual(Arg1, certSecCredu:handle_cast({cec_register}, #state{})),
    
    meck:unload(cec),
    
    ok.

handle_cast_unknown_test() ->
    meck:new(sysInitI, [passthrough, non_strict]),
    meck:expect(sysInitI, warning_msg, 2, fun(_String, _Args) -> ok end),
    
    Arg1 = {noreply, #state{}},
    
    ?assertEqual(Arg1, certSecCredu:handle_cast(unknown, #state{})),
    
    meck:unload(sysInitI),
    
    ok.

handle_call_unknown_test() ->
    meck:new(sysInitI, [passthrough, non_strict]),
    meck:expect(sysInitI, info_msg, 2, fun(_String, _Args) -> ok end),
    
    Arg1 = {reply,ok, #state{}},
    
    ?assertEqual(Arg1, certSecCredu:handle_call(unknown, from, #state{})),
    
    meck:unload(sysInitI),
    
    ok.

handle_info_unknown_test() ->
    meck:new(sysInitI, [passthrough, non_strict]),
    meck:expect(sysInitI, warning_msg, 2, fun(_String, _Args) -> ok end),
    
    Arg1 = {noreply, #state{}},
    
    ?assertEqual(Arg1, certSecCredu:handle_info(unknown, #state{})),
    
    meck:unload(sysInitI),
    
    ok.

handle_info_tcp_error_test() ->
    meck:new(sysInitI, [passthrough, non_strict]),
    meck:expect(sysInitI, error_msg, 1, fun(_String) -> ok end),
    
    Arg1 = {noreply, #state{}},
    
    ?assertEqual(Arg1, certSecCredu:handle_info({tcp_error, socket, data}, #state{})),
    
    meck:unload(sysInitI),
    
    ok.

init_test() ->
    meck:new(sysEnv, [non_strict, passthrough]),    
    meck:new(file, [unstick, passthrough]),

    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),
    SEC_CREDU_DIR = ?SEC_CREDU_DIR,
    NC_CERT_DIR = ?NC_CERT_DIR,
    NC_KEY_DIR = ?NC_KEY_DIR,
    TCAT_ROOT_DIR = ?TCAT_ROOT_DIR,
    meck:expect(file, make_dir, 1, fun(Dir) when Dir =:= SEC_CREDU_DIR orelse
                                                 Dir =:= NC_CERT_DIR orelse
                                                 Dir =:= NC_KEY_DIR orelse
                                                 Dir =:= TCAT_ROOT_DIR -> ok end),
    
    Arg1 = {ok, #state{}},
    ?assertEqual(Arg1, certSecCredu:init(argument)),

    meck:unload(file),
    meck:unload(sysEnv),
    
    ok.

terminate_test() ->
    meck:new(cec, [passthrough, non_strict]),
    meck:new(file, [unstick, passthrough]),
    meck:new(sysEnv, [non_strict, passthrough]),
    meck:new(sysInitI, [passthrough, non_strict]),
    meck:expect(sysInitI, info_msg, 2, fun(_String, _Args) -> ok end),
    
    meck:expect(cec, unregister, 1, fun(<<"SEC_CREDU_API">>) -> ok end),
    meck:expect(sysEnv, tmp_dir, 0, fun() -> "/tmp" end),

    SEC_CREDU_DIR = ?SEC_CREDU_DIR,
    NC_CERT_DIR = ?NC_CERT_DIR,
    NC_KEY_DIR = ?NC_KEY_DIR,
    TCAT_ROOT_DIR = ?TCAT_ROOT_DIR,
    meck:expect(file, delete, 1, fun(_File) -> ok end),
    meck:expect(file, del_dir, 1, fun(Dir) when Dir =:= SEC_CREDU_DIR orelse
                                                Dir =:= NC_CERT_DIR orelse
                                                Dir =:= NC_KEY_DIR orelse
                                                Dir =:= TCAT_ROOT_DIR -> ok end),
    meck:expect(file, list_dir_all, fun(_Dir) -> {ok, []} end),

    ?assertEqual(ok, certSecCredu:terminate("Crash!!!", #state{})),
    
    meck:unload(cec),
    meck:unload(file),
    meck:unload(sysEnv),
    meck:unload(sysInitI),

    ok.

code_change_test() ->
    ?assertEqual({ok, #state{}}, certSecCredu:code_change(old_version, #state{}, extra)),
    
    ok.

handle_call_stop_test() ->

    ?assertEqual({stop, normal, stopped, #state{}}, certSecCredu:handle_call(stop, from, #state{})),

    ok. 

handle_call_get_sub_list_test() ->
    
    Subscriber = #subscriber{id = 1, select_object = {socket, sel_obj}},
    Arg = {reply, [Subscriber], #state{subscribers = [Subscriber]}},
    ?assertEqual(Arg, certSecCredu:handle_call(get_subscribers_list, from, #state{subscribers = [Subscriber]})),

    ok.

handle_call_finalize_test() ->
    
    Subscriber = #subscriber{id = 1, select_object = {socket, sel_obj}},
    SubscriberNoSelObj = #subscriber{id = 1, select_object = undefined},

    FinalizeEvent = <<?SEC_CREDU_EVENT_SIG:4/native-unsigned-integer-unit:8, ?FINALIZE_EVENT:4/native-unsigned-integer-unit:8>>,
    
    meck:new(gen_tcp, [passthrough, unstick]),
    meck:new(inet, [passthrough, unstick]),
    meck:expect(gen_tcp, send, 2, fun({socket, sel_obj}, Message) when Message =:= FinalizeEvent -> ok end),
    meck:expect(inet, setopts, 2, fun({socket, sel_obj}, [{active, once}]) -> ok end),
    meck:expect(gen_tcp, close, fun({socket, sel_obj}) -> ok; (_) -> error end),

    ?assertEqual({reply, ok, #state{subscribers = [SubscriberNoSelObj]}}, 
                 certSecCredu:handle_call({finalize, 1}, from, #state{subscribers = [Subscriber]})),
                 
    meck:unload(gen_tcp),
    meck:unload(inet),
    
    ok.

handle_info_tcp_closed_test() ->
    Subscriptions = [#subscription{subscribe_id = 11, mo_ref = ?NODE_CREDENTIAL_DN2}, #subscription{subscribe_id = 21, mo_ref = ?NODE_CREDENTIAL_DN3}],
    SubId = 1,
    Subscriber      = #subscriber{id = SubId, select_object = {socket, sel_obj}, subscriptions = Subscriptions, communication_socket = 123},
    EmptySubscriber = #subscriber{id = SubId, select_object = {socket, sel_obj}, subscriptions = [],            communication_socket = 123},
    State    = #state{ subscribers = [Subscriber, ?SUBSCRIBER4]},
    NewState1 = #state{ subscribers = [EmptySubscriber, ?SUBSCRIBER4]},
    NewState2 = #state{ subscribers = [?SUBSCRIBER4]},

    meck:new(sysInitI, [passthrough, non_strict]),
    meck:expect(sysInitI, info_msg, 2, fun(_String, _ListOfEls) -> ok end),
    
    meck:new(gen_tcp,[ passthrough, unstick]),
    meck:expect(gen_tcp, close, 1, fun(_Sock) -> ok end),
    meck:expect(gen_tcp, send, 2, fun(_Sock, _Msg) -> ok end),

    meck:new(certSecCredu, [passthrough, unstick]),
    meck:expect(certSecCredu, unsubscribe_from_all, 2, fun(_SubId, _State) -> {ok, NewState1} end),

    ?assertEqual({noreply, NewState2}, certSecCredu:handle_info({tcp_closed, 123}, State)),

    %% no matching subscriber, nothing changes
    ?assertEqual({noreply, State}, certSecCredu:handle_info({tcp_closed, 124}, State)),

    meck:unload(sysInitI),
    meck:unload(certSecCredu),
    meck:unload(gen_tcp),
    ok.

handle_info_tcp_receive_request_test() ->
    meck:new(gen_tcp, [passthrough, unstick]),
    meck:new(inet, [passthrough, unstick]),
    
    BinaryMessageUnknownReq = <<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, ?RESP_STAT_ERR_UNKNOWN_REQ_TYPE:4/native-unsigned-integer-unit:8>>,

    meck:expect(gen_tcp, send, 2, fun(?SOCKET, Message) when Message =:= BinaryMessageUnknownReq -> ok end),
    meck:expect(inet, setopts, 2, fun(?SOCKET, [{active, once}]) -> ok end),
    
    ?assertEqual({noreply, #state{}}, certSecCredu: handle_info({tcp, ?SOCKET,
                                                                  <<23:4/native-unsigned-integer-unit:8,
                                                                    ?SEC_CREDU_REQ_SIG:4/native-unsigned-integer-unit:8,
                                                                    ?REQ_UNKNOWN:4/native-unsigned-integer-unit:8,
                                                                    <<12:4/native-unsigned-integer-unit:8>>/binary>>}, #state{})),
    
    meck:unload(gen_tcp),
    meck:unload(inet),
    ok.
