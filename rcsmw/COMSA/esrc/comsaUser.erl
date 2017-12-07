%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaUser.erl %
%%% @author etxlg
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/R4A/5
%%%
%%% @doc ==Agent implementation for RcsUser==
%%% This is the agent implementation for RcsUser.

-module(comsaUser).
-vsn('/main/R2A/R3A/R4A/5').
-date('2015-07-10').
-author('etxlg').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% R2A/1      2014-01-20 etxjotj     Created
%%% R2A/5      2014-06-12 etxlg       Mods for super_OaM_user
%%%                                   Also need support to limit this, these
%%%				      users should only be changed by a user
%%%				      who already have the role EricssonSupport
%%% R2A/6      2014-06-17 etxlg       MaintenanceUserSecurity=1
%%% R3A/1      2014-11-28 etxberb     Added values/1.
%%% R3A/8      2015-03-18 etxlg       Action return {10, true} -> undefined
%%% ----------------------------------------------------------
%%% R4A/2      2015-06-26 etxtory     HT85636 - added prepareTransaction
%%% R4A/4      2015-06-30 etxtory     Corrected previous (AI didn't work)
%%% R4A/5      2015-07-10 etxlg       No duplicates allowed, TR HT90067
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([init_data/1]).
-export([getMoAttribute/2]).
-export([nextMo/3]).
-export([setMoAttribute/4]).
-export([validate/3]).
-export([prepare/3]).
-export([prepareTransaction/2]).
-export([commit/3]).
-export([finish/3]).

-export([existsMo/2,
         countMoChildren/3,
         getMoAttributes/3,
         setMoAttributes/3]).

-export([action/4]).
-export([createMo/4, createMo/5]).
-export([deleteMo/2]).
-export([is_super_oam_user/1]).

%% Shortest length of password
-define(PWD_MIN_LENGTH, 12).
%% Minimum number of lower case alpha characters in the password
-define(PWD_MIN_LC, 3).
%% Minimum number of upper case alpha characters in the password
-define(PWD_MIN_UC, 3).
%% Minimum number of numeric characters in the password
-define(PWD_MIN_NC, 2).
%% Minimum number of speical characters in the password
%% Special characters ascii dec: 33-47, 58-64, 91-96, 123-126
-define(PWD_MIN_SC, 1).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RcsUser.hrl").
-include("comte_types.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%% @doc Returns true if the specified instance exists.
%% @end
%% existsMo(DnRev, TxId)
%%% ----------------------------------------------------------
existsMo([<<"1">>,<<"UserIdentity">>,<<"1">>,<<"UserManagement">>,<<"1">>,
	  <<"SecM">>,<<"1">>,<<"SystemFunctions">>,_, <<"ManagedElement">>],
	 _) ->
    true;
existsMo(DnRev, _TransId) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).

%%% ----------------------------------------------------------
%%% @doc Returns the number of MO instances of given class
%%% directly below the specified parent.
%%% @end
%%% ----------------------------------------------------------
countMoChildren([<<"1">>,<<"UserManagement">>,<<"1">>,<<"SecM">>,<<"1">>,
		 <<"SystemFunctions">>,_, <<"ManagedElement">>],
		<<"UserIdentity">>, _) ->
    1;
countMoChildren([DnRev], ClassName, _TransId) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(ClassName))).

%%% ----------------------------------------------------------
%%% @doc Gets MO attribute values.
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.
%%% ----------------------------------------------------------
getMoAttributes(AttrNames, DnRev, TxHandle) ->
    [getMoAttribute([AttrName|DnRev], TxHandle)||AttrName<-AttrNames].

%%% ----------------------------------------------------------
%%% @doc Get value for a specific attribute
%%% Will not return any value
%%% @end
%%% ----------------------------------------------------------
getMoAttribute([<<"userIdentityId">>|_], _) ->
    ?STRING(<<"1">>);
getMoAttribute([<<"userName">>,_,<<"UserIdentity">>|_], _) ->
    undefined;
getMoAttribute([<<"password">>,_,<<"MaintenanceUser">>|_], _) ->
    undefined;
getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table));
getMoAttribute(_DnRev, _TxHandle) ->
    undefined.

%%% ----------------------------------------------------------
%%% @doc Iterate over instances
%%% UserIdentity is hardcoded.
%%% MaintenanceUser is stored in mnesia.
%%% @end
%%% ----------------------------------------------------------
nextMo([<<"UserIdentity">>|_], undefined, _TxHandle) ->
    {ok, {?STRING(<<"1">>), {"1","1","1","1","1"}}};
nextMo([<<"UserIdentity">>|_], {"1","1","1","1","1"}, _TxHandle) ->
    {ok, undefined};
nextMo(Dn, Key, _TxHandle) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

%%% ----------------------------------------------------------
%%% @doc Set multiple attribute values
%%% Nothing gets set for UserIdentity:userName.
%%% @end
%%% ----------------------------------------------------------
setMoAttributes([{<<"userName">>, _ComValue}], [_,<<"UserIdentity">>|_],
    _TxHandle) ->
    {ok, #userIdentity{}};
setMoAttributes([{<<"password">>, {_,List}}] = Attrs,
    [_,<<"MaintenanceUser">>|_] = DnRev, _TxHandle) ->
    case omc_api:is_only_support_users() of
	true ->
            case catch aicI:is_ai_ongoing() of
                true ->
                    Table = table(comsaGeneric:class(DnRev)),
                    comsaGeneric:set(DnRev, Table, types(Table), Attrs);
                false ->
                    {<<"password">>,{9, EncryptedKey}} =
                    lists:keyfind(<<"password">>, 1, List),
                    Key = comsaI:decrypt_password(binary_to_list(EncryptedKey)),
                    case check_pwd_strength(Key) of
                        ok ->
                            Table = table(comsaGeneric:class(DnRev)),
                            comsaGeneric:set(DnRev, Table, types(Table), Attrs);
                        {too_weak, Reason} ->
                            mnesia:abort(list_to_binary(Reason));
                        error ->
                            Msg = "Not allowed characters!",
                            mnesia:abort(list_to_binary(Msg))
                    end
            end;
	false ->
	    S = "Only maintenance users may be login when altering maintenance user configuration.\n" ++
		"Close all sessions except for maintenance user sessions with action closeSessions().",
	    mnesia:abort(list_to_binary(S))
    end;
setMoAttributes(Attrs, DnRev, _TxHandle) ->
    case omc_api:is_only_support_users() of
	true ->
	    Table = table(comsaGeneric:class(DnRev)),
	    comsaGeneric:set(DnRev, Table, types(Table), Attrs);
	false ->
	    S = "Only maintenance users may be login when altering maintenance user configuration.\n" ++
		"Close all sessions except for maintenance user sessions with action closeSessions().",
	    mnesia:abort(list_to_binary(S))
    end.

check_pwd_strength(Key) when length(Key) < ?PWD_MIN_LENGTH ->
    {too_weak, "Password is too short!"};
check_pwd_strength(Key) ->
    do_strength_check(Key, {0,0,0,0}).

do_strength_check([], {LC, _, _, _}) when LC < ?PWD_MIN_LC ->
    {too_weak, "Password has too few lower case alpha characters!"};
do_strength_check([], {_, UC, _, _}) when UC < ?PWD_MIN_UC ->
    {too_weak, "Password has too few upper case alpha characters!"};
do_strength_check([], {_, _, NC, _}) when NC < ?PWD_MIN_NC ->
    {too_weak, "Password has too few numeric characters!"};
do_strength_check([], {_, _, _, SC}) when SC < ?PWD_MIN_SC ->
    {too_weak, "Password has too few special characters!"};
do_strength_check([], _) ->
    ok;
do_strength_check([H|T], {LC, HC, NC, SC}) when H < 123, H > 96 ->
    do_strength_check(T, {LC + 1, HC, NC, SC});
do_strength_check([H|T], {LC, HC, NC, SC}) when H < 91, H > 64 ->
    do_strength_check(T, {LC, HC + 1, NC, SC});
do_strength_check([H|T], {LC, HC, NC, SC}) when H < 58, H > 47 ->
    do_strength_check(T, {LC, HC, NC + 1, SC});
do_strength_check([H|T], {LC, HC, NC, SC}) when H < 47, H > 32 ->
    do_strength_check(T, {LC, HC, NC, SC + 1});
do_strength_check([H|T], {LC, HC, NC, SC}) when H < 65, H > 57 ->
    do_strength_check(T, {LC, HC, NC, SC + 1});
do_strength_check([H|T], {LC, HC, NC, SC}) when H < 97, H > 90 ->
    do_strength_check(T, {LC, HC, NC, SC + 1});
do_strength_check([H|T], {LC, HC, NC, SC}) when H < 127, H > 122 ->
    do_strength_check(T, {LC, HC, NC, SC + 1});
do_strength_check(_, _)  ->
    error.

%%% ----------------------------------------------------------
%%% @doc Set a single attribute
%%% @end
%%% ----------------------------------------------------------
setMoAttribute([<<"userName">>,_,<<"UserIdentity">>|_], _TypeAndValue, _Internal, _TxHandle) ->
    {ok, #userIdentity{}};
setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).

validate(_DN, User, _Tx) ->
    {ok, User}.

prepare(_DN, User, _Tx) ->
    {ok, User}.

%% ok | {abort, Error}
prepareTransaction(Objects, _Tx) ->
    MUs  = [MU || {_, MU} <- Objects, is_record(MU, maintenanceUser)],
    case check_mus_more(MUs) of
	ok ->
	    check_delete_mus(Objects);
	Error ->
	    Error
    end.

commit(_DN, User, _Tx) ->
    {ok, User}.

finish(_DN, _User, _Tx) ->
    ok.

%% called from comsaDataInit
init_data(upgrade) ->
    transform(maintenanceUser),
    mnesia:dirty_write(
      #maintenanceUserSecurity{
	 maintenanceUserSecurityId = {"1","1","1","1","1","1"}});
init_data(fromScratch) ->
    %% system created
    %% ManagedElement=1,SystemFunctions=1,SecM=1,UserManagement=1,UserIdentity=1,
    %%	MaintenanceUserSecurity=1
    mnesia:dirty_write(
      #maintenanceUserSecurity{
	 maintenanceUserSecurityId = {"1","1","1","1","1","1"}}).

transform(maintenanceUser) ->
    F = fun() ->
		Users = swmI:all_objects(maintenanceUser),
		[begin
		     case User of
			 {maintenanceUser, Id, CN} ->
			     mnesia:write(
			       #maintenanceUser{maintenanceUserId = Id,
						subjectName = CN});
			 Obj ->
			     mnesia:write(Obj)
		     end
		 end||User<-Users],
		ok
	end,
    {atomic, ok} = mnesia:transaction(F).

%%% ----------------------------------------------------------
%%% @doc Action close session
%%% @end
%%% ----------------------------------------------------------
action(<<"closeSessions">>, _DnRev, _NamedParams, _TransId) ->
    ok =  omc_api:logout_non_support_users(),
    %%{error, <<"What's up?">>}.
    %%{10, true}.
    undefined.

%%% ----------------------------------------------------------
%%% @doc Creates a new instance of Maintenance User.
%%% @end
%%% ----------------------------------------------------------
createMo([Class | ParentDnRev], _KeyAttrName, KeyValue, InitAttrs, _TransId) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table,
			ParentDnRev,
			KeyValue,
			values(InitAttrs),
			types(Table)).

createMo([Class|ParentDnRev], _, IxValueB, _) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table, ParentDnRev, IxValueB).

%%% @doc Deletes an instance of Maintenance User.
%%% @end
deleteMo(DnRev, _) ->
    case omc_api:is_only_support_users() of
	true ->
	    Table = table(comsaGeneric:class(DnRev)),
	    case mnesia:table_info(maintenanceUser, size) of
		1 ->
		    mnesia:abort(
		      <<"The last maintenance user cannot be deleted">>);
		_ ->
		    comsaGeneric:delete(DnRev, Table)
	    end;
	false ->
	    S = "Only maintenance users may be login when altering maintenance user configuration.\n" ++
		"Close all sessions except for maintenance user sessions with action closeSessions().",
	    mnesia:abort(list_to_binary(S))
    end.

%%% ----------------------------------------------------------
%%% @doc Check if a user has been added as MaintenanceUser
%%% @end
%%% ----------------------------------------------------------
is_super_oam_user({User, Pwd}) ->
    Table   = table("MaintenanceUser"),
    Pattern = mnesia:table_info(Table ,wild_pattern),
    Match   = Pattern#maintenanceUser{userName = User},
    Objects = mnesia:dirty_match_object(Table, Match),
    check_user_objects(Objects, Pwd);
is_super_oam_user(Subject_name) ->
    Table   = table("MaintenanceUser"),
    Pattern = mnesia:table_info(Table ,wild_pattern),
    Match   = Pattern#maintenanceUser{subjectName = Subject_name},
    case mnesia:dirty_match_object(Table, Match) of
	[] -> false;
	[_] -> true
    end.

check_user_objects([], _) ->
    false;
check_user_objects([Obj|T], Pwd) ->
    case comsaI:decrypt_password(Obj#maintenanceUser.password) of
        Pwd ->
            true;
        _ ->
            check_user_objects(T, Pwd)
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
table("UserIdentity") -> userIdentity;
table("MaintenanceUser") -> maintenanceUser;
table("MaintenanceUserSecurity") -> maintenanceUserSecurity.

types(userIdentity) -> ?userIdentity_types;
types(maintenanceUser) -> ?maintenanceUser_types;
types(maintenanceUserSecurity) -> ?maintenanceUserSecurity_types.

values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

%%% ----------------------------------------------------------
%%% Checks in prepare transaction.
%%% ----------------------------------------------------------
%%% ensure that there are no duplicate userName or duplicate subjectName added
%%% if there should ever be very many maintenance users defined this is not
%%% efficient...
check_mus_more(MUs) ->
    ExistingMUs =
	mnesia:match_object(mnesia:table_info(maintenanceUser, wild_pattern)),
    %usort the MUs because there maybe duplicates in MUs, and it also seems
    %that what is in MUs is also already written to mnesia within this
    %transaction
    AllMUs = 
	lists:usort(
	    fun(#maintenanceUser{maintenanceUserId = A},
		#maintenanceUser{maintenanceUserId = B}) when A =< B ->
		true;
	       (_, _) ->
		false
	    end, MUs ++ ExistingMUs),

    case {get_duplicate(#maintenanceUser.userName, AllMUs),
	  get_duplicate(#maintenanceUser.subjectName, AllMUs)} of
	{{true, Dup}, _} ->
	    {abort, list_to_binary("Duplicate userName: \"" ++ Dup ++
				   "\" - this is not allowed")};
	{_, {true, Dup}} ->
	    {abort, list_to_binary("Duplicate subjectName: \"" ++ Dup ++
				   "\" - this is not allowed")};
	{false, false} ->
	    check_mus(MUs)
    end.

get_duplicate(RecordPos, AllMUs) ->
    get_duplicate(RecordPos, AllMUs, []).
get_duplicate(_, [], _) ->
    false;
get_duplicate(RecordPos, [H | T], FoundItems) ->
    Item = element(RecordPos, H),
    case Item of
	undefined ->
	    get_duplicate(RecordPos, T, FoundItems);
	Item ->
	    case lists:member(Item, FoundItems) of
		true ->
		    {true, Item};
		false ->
		    get_duplicate(RecordPos, T, [Item | FoundItems])
	    end
    end.

check_mus([]) ->
    ok;
check_mus([MU | T]) ->
    case {MU#maintenanceUser.userName, MU#maintenanceUser.password, MU#maintenanceUser.subjectName} of
	{undefined, undefined, undefined} ->
	    %% No userName/password or subjectName; reject
	    {abort, <<"Missing userName/password or subjectName">>};
	{undefined, undefined, _SubjectName} ->
	    %% subjectName only; ok
	    check_mus(T);
	{undefined, _Password, _SubjectName} ->
	    %% No userName; reject
	    {abort, <<"Missing userName">>};
	{_UserName, undefined, _SubjectName} ->
	    %% No password; reject
	    {abort, <<"Missing password">>};
	{_UserName, _Password, _SubjectName} ->
	    %% userName/password and subjectName; ok
	    check_mus(T)
    end.

check_delete_mus(Objects) ->
    NoOfDeletes = get_deletes(Objects, _Acc = 0),
    NoOfMUs = mnesia:table_info(maintenanceUser, size),
    if 
	NoOfDeletes == 0 ->
	    ok;
	NoOfDeletes < NoOfMUs ->
	    ok;
	NoOfDeletes == NoOfMUs ->
	    {abort, <<"The last maintenance user cannot be deleted">>};
	NoOfDeletes > NoOfMUs ->
	    %% Should not happen
	    {abort, <<"The last maintenance user cannot be removed">>}
    end.
	
get_deletes([], Acc) ->
    Acc;
get_deletes([{_, {deleted, _}} | T], Acc) ->
    get_deletes(T, Acc+1);
get_deletes([_ | T], Acc) ->
    get_deletes(T, Acc).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
