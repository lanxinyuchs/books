%%%-------------------------------------------------------------------
%%% @author Lukas Larsson <lukas@erlang-solutions.com>
%%% @doc An example callback module, this is only here for documentation
%%% purposes.
%%% @end
%%% Created : 21 Mar 2011 by Lukas Larsson <lukas@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(comte_example_callback).

-behaviour(comte_data_api).

-include("comte_types.hrl").

%% Comte callbacks
-export([getMoAttribute/2,
         getMoAttributes/2,
         createMo/4,
         setMoAttribute/4,
         setMoAttributes/3,
         nextMo/3,
         deleteMo/2,
         existsMo/2]).
-export([validate/3,
         prepare/3,
         commit/1,
         finish/3,
         action/3]).

%% API
-export([init/0, init/1]).

%% Records
-record(config, { id, attrs }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Get the attribute given by the <i>ECIMPath</i>.
%% If the attribute does not exist the empty list should be returned.
%% If the attribute is unset then undefined should be returned.
%%
%% If a list of com_values are returned all of the values will be shown to the
%% user in the order specified.
%% @end
-spec getMoAttribute(ECIMPath :: ecim_path(), transaction_id()) ->
                            com_data() | {error, error_reason()}.

getMoAttribute([Attr | MO], _Tx) ->
    case mnesia:read(config, MO) of
	[#config{ attrs = Attrs }] ->
	    hd(get_attributes([Attr], Attrs, []));
	[] ->
	    []
    end.

get_attributes([], _Attrs, Acc) ->
    lists:reverse(Acc);
get_attributes([ <<"localDateTime">> | Rest], Attrs, Acc) ->
    get_attributes(Rest, Attrs, [get_local_time() | Acc]);
get_attributes([Attr | Rest], Attrs, Acc) ->
    get_attributes(Rest,
		   Attrs,
		   [proplists:get_value(Attr, Attrs) | Acc]).

%% @doc Get the attributes given by the list of <i>ECIMPath</i>.
%% If the attribute does not exist the empty list should be returned.
%% If the attribute is unset then undefined should be returned.
%%
%% The list of com_values will be shown to the
%% user in the order specified.
%% @end
-spec getMoAttributes(ECIMPaths :: list(ecim_path()), transaction_id()) ->
                             [com_data()] | {error, error_reason()}.
getMoAttributes([ [_Attr|MO] | _Rest]=Attributes, _Tx) ->
    case mnesia:read(config, MO) of
	[#config{ attrs = Attrs }] ->
	    get_attributes([A || [A|_MO] <- Attributes],
			   Attrs, []);
	[] ->
	    []
    end.



get_local_time() ->
    {{Y,M,D},{Hs,Ms,Ss}} = calendar:local_time(),
    Fmt = "~w~s~2..0w~s~2..0w~s~2..0w~s~2..0w~s~2..0w",
    ?STRING(list_to_binary(io_lib:format(Fmt, [Y,"-",M,"-",D, "T", Hs,":",Ms,":",Ss]))).

%% @doc Set the attribute given by the <i>ECIMPath</i> to <i>Value</i>.
%%   If <i>Value</i> is set to undefined, then the attribute is to be
%%   unset. You can choose to either do the validation of the data in this
%%   call or you can do the validation in prepare or commit.
%%
%% If `{ok, NewUserObject}' is returned, that <i>NewUserObject</i> will be
%% used given as the <i>UserObject</i> for subsequent setMoAttribute,
%% commit and finish calls. If `ok' is returned the same
%% UserObject will be used.
%%
%% This command has to be side effect free as it will be called multiple times.
%% @end
-spec setMoAttribute(ECIMPath :: ecim_path(),
		     Value :: com_data(),
		     UserObject :: undefined | user_object(),
		     transaction_id()) ->
			    ok | {ok, NewUserObject :: user_object()} |
			    {error, error_reason()}.
setMoAttribute(ECIMPath, Value, UserObject, Tx) ->
    setMoAttributes([{ECIMPath,Value}], UserObject, Tx).



%% @doc Set the attributes given by the <i>NamedAttrs</i> to
%%   their corresponding value or if it is set to undefined, then the attribute is to be
%%   unset. You can choose to either do the validation of the data in this
%%   call or you can do the validation in prepare or commit.
%%
%% If `{ok, NewUserObject}' is returned, that <i>NewUserObject</i> will be
%% used given as the <i>UserObject</i> for subsequent setMoAttributes,
%% commit and finish calls. If `ok' is returned the same
%% UserObject will be used.
%%
%% This command has to be side effect free as it will be called multiple times.
%% @end
-spec setMoAttributes(
	NamedAttrs :: [{ecim_path(), com_data()}],
	UserObject :: undefined | user_object(),
	transaction_id()) ->
			     ok | {ok, NewUserObject :: user_object()} |
			    {error, error_reason()}.
setMoAttributes([ {[_Attr|MO],_AttrVal} | _Rest]=NamedAttrs, _UserObj, _Tx) ->
    case mnesia:read(config, MO) of
	[#config{ attrs = Attrs } = Config] ->
            StoreList = store_list(NamedAttrs, Attrs),
            mnesia:write(Config#config{ attrs = StoreList })
    end.

%% @doc Create the MO given by the <i>ECIMPath</i> with the given
%%   <i>KeyName</i> and <i>KeyValue</i>.
%%
%% If `{ok, NewUserObject}' is returned, that <i>NewUserObject</i> will be
%% used given as the <i>UserObject</i> for subsequent setMoAttribute,
%% commit and finish calls. If `ok' is returned the same
%% UserObject will be used.
%%
%% This command has to be side effect free as it will be called multiple times.
%% @end
-spec createMo(ecim_path(), binary(), binary(), transaction_id()) ->
		      ok | {ok, NewUserObject :: user_object()} |
		      {error, error_reason()}.

createMo(ECIMPath, KeyName, KeyValue, _Tx) ->
    mnesia:write(#config{ id = [KeyValue|ECIMPath],
			  attrs = [{KeyName, ?STRING(KeyValue)}]}).



%% @doc Delete the MO given by the <i>ECIMPath</i>. The <i>ECIMPath</i> will
%%   always refer to an MO and never to an attribute within an MO.
%%
%% This command has to be side effect free as it will be called multiple times.
%% @end
%% @see setMoAttribute/4. setMoAttribute/4 for details about
%%                        how an attribute is unset
-spec deleteMo(ecim_path(), transaction_id()) ->
		      ok | {error, error_reason()}.
deleteMo(ECIMPath, _Tx) ->
    mnesia:delete({config, ECIMPath}).



existsMo(ECIMPath, _Tx) ->
    case mnesia:read(config, ECIMPath) of
        [#config{}] ->
            true;
        [] ->
            false
    end.

%% @doc Get the next key for the MO given by the <i>ECIMPath</i>. For the
%%   initial call <i>CurrKey</i> will be undefined, in all subsequant call it
%%   will be whatever is returned as the <i>NextKey</i> in the previous call.
%%
%% Right now the <i>ComKey</i> must be of the com_type ?STRING.
%% @end
-spec nextMo(ECIMPath :: ecim_path(),
	     CurrKey :: undefined | term(),
	     transaction_id()) ->
		    {ok, undefined | {ComKey :: com_value(),
				      NextKey :: term()}} |
		    {error, error_reason()}.
nextMo(ECIMPath, undefined, _Tx) ->
    nextMo(ECIMPath,mnesia:all_keys(config),_Tx);
nextMo(ECIMPath, [[Key | ECIMPath] | Rest], _Tx) ->
    {ok, {?STRING(Key),Rest}};
nextMo(ECIMPath, [_ | Rest], _Tx) ->
    nextMo(ECIMPath, Rest, _Tx);
nextMo(_ECIMPath, [], _Tx) ->
    {ok, undefined}.

%% @doc Validate an ECIM MO. Here you should validate the
%% contents of this MO and also it's relation to other MOs. The <i>ECIMPath</i>
%% points to the MO which is beeing modified, eg.
%% `[<<"1">>,<<"TestMO">>,<<"1">>, <<"ManagedElement">>]'.
%% Note that only MOs will be called here,
%% i.e. if ManagedElement=1,TestMO=1.userLabel is to be set, prepare with
%% `[<<"1">>,<<"TestMO">>,<<"1">>, <<"ManagedElement">>]' will be called.
%%
%% If a MO has been deleted, this function will be called with the
%% <i>ECIMPath</i> and that MO and `deleted' as the <i>UserObject</i>.
%%
%% @end
-spec validate(ecim_path(), undefined | deleted | user_object(),
	      transaction_id()) ->
		     ok | {error, error_reason()}.
validate(_ECIMPath, _UserObject, _Tx) ->
    ok.



%% @doc Prepare an ECIM MO for being commited. Here you should validate the
%% contents of this MO and also it's relation to other MOs. The <i>ECIMPath</i>
%% points to the MO which is beeing modified, eg.
%% `[<<"1">>,<<"TestMO">>,<<"1">>, <<"ManagedElement">>]'.
%% Note that only MOs will be called here,
%% i.e. if ManagedElement=1,TestMO=1.userLabel is to be set, prepare with
%% `[<<"1">>,<<"TestMO">>,<<"1">>, <<"ManagedElement">>]' will be called.
%%
%% If a MO has been deleted, this function will be called with the
%% <i>ECIMPath</i> and that MO and `deleted' as the <i>UserObject</i>.
%%
%% No more setMoAttribute, createMo or deleteMo actions will be done on any MO
%% in this transaction after this function is called.
%%
%% If `{ok, NewUserObject}' is returned, that <i>NewUserObject</i> will be
%% used given as the <i>UserObject</i> for subsequent finish calls.
%% If `ok' is returned the same UserObject will be used.
%% @end
%% @see finish/3
-spec prepare(ecim_path(), undefined | deleted | user_object(),
	      transaction_id()) ->
		     ok | {ok, NewUserObject :: user_object()} |
		     {error, error_reason()}.
prepare(_ECIMPath, _UserObject, _Tx) ->
    ok.

%% @doc Commit an transaction. Here you do any actions which need to be
%% done within the transaction but after all create, set and deletes have
%% been done.
%%
%% This function is called once per transaction for each callback module
%% involved in that transaction.
%%
%% @end
%% @see finish/3
-spec commit(transaction_id()) -> ok | {error, error_reason()}.
commit(_Tx) ->
    ok.

%% @doc Called after the transaction is commited. Here you should do actions
%% which are needed after the configuratino data has been commited to the
%% database, i.e. setup network interfacases and the like.
%%
%% If a MO as been deleted, this function will be called with the
%% <i>ECIMPath</i> and that MO and `deleted' as the <i>UserObject</i>.
%%
%% This function is not called within an mnesia transaction! Returning an error
%% here will not rollback the transaction!
%% @end
%% @see prepare/3
-spec finish(ecim_path(), undefined | deleted | user_object(), transaction_id()) ->
		     ok | {error, error_reason()}.
finish(_ECIMPath, _UserObject, _Tx) ->
    ok.

%% @doc Do the action given by the <i>ECIMPath</i>.
%% The parameters of the action is given in a list of com_values.
%% The return value follows the same rules as getMoAttribute
%% except for `undefined' which means that no return should be given.
%%
%% This function is not called within an mnesia transaction!
%%
%% @end
%% @see getMoAttribute/2
-spec action(ecim_path(), list(com_named_parameter()), transaction_id()) ->
		    com_data() | {error, error_reason()}.

action([<<"actionWithError">> | _ECIMPath], _Parameters, _Tx) ->
    {error, <<"Not implemented">>};
action([<<"actionWithNoReturn">>|_], _Parameters, _Tx) ->
    undefined;
action([<<"addNumbers">>|_], Parameters, _Tx) ->
    ?INT32(Int1) = proplists:get_value(<<"num1">>, Parameters),
    ?INT32(Int2) =  proplists:get_value(<<"num2">>, Parameters),
    <<Sum:32/signed>> = <<(Int1 + Int2):32>>,
    ?INT32(Sum);
action([<<"actionReturnSeq">> | _ECIMPath], Parameters, _Tx) ->
    ?INT32(Sz) = proplists:get_value(<<"sz">>, Parameters),
    ?INT32(Start) =  proplists:get_value(<<"start">>, Parameters),
    [?INT32(Start+(S-1)) || S <- lists:seq(1,Sz)];

action(_ECIMPath, _Parameters, _Tx) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

store_list([], Attrs) ->
    Attrs;
store_list([{[AttrName|_MO], TypeVal}|NamedAttrs], Attrs) ->
    store_list(NamedAttrs, lists:keystore(AttrName, 1, Attrs, {AttrName, TypeVal})).

init(Data) ->
    [mnesia:dirty_write(#config{ id = Key, attrs = Attrs})
     || {Key, Attrs} <- Data].

%% @private
init() ->
    ComteTrans = comte_default_transaction_server,
    ComteTrans:register_data_callback([<<"ManagedElement">>], ?MODULE),

    mnesia:create_table(config, [{attributes, record_info(fields, config)}]),
    mnesia:clear_table(config),

    {ok, Vsn} = application:get_key(comte, vsn),
    %% Top element
    mnesia:dirty_write(#config{ id = [<<"1">>,<<"ManagedElement">>],
				attrs = [{<<"managedElementId">>,?STRING(<<"1">>)},
					 {<<"managedElementType">>,?STRING(<<"ComtE">>)},
					 {<<"release">>,?STRING(list_to_binary(Vsn))},
                                         {<<"networkManagedElementId">>,?STRING(<<"1">>)},
					 {<<"dateTimeOffset">>,?STRING(<<"+01:00">>)}]}),

    %% Local Authorization Method
    mnesia:dirty_write(#config{ id = [<<"1">>,<<"LocalAuthorizationMethod">>,
                                      <<"1">>,<<"UserManagement">>,
                                      <<"1">>,<<"SecM">>,
                                      <<"1">>,<<"SystemFunctions">>,
				      <<"1">>,<<"ManagedElement">>],
				attrs = [{<<"administrativeState">>,?ENUM(0)}]}),

    mnesia:dirty_write(#config{ id = [<<"1">>,<<"Role">>,
				      <<"1">>,<<"LocalAuthorizationMethod">>,
				      <<"1">>,<<"UserManagement">>,
				      <<"1">>,<<"SecM">>,
				      <<"1">>,<<"SystemFunctions">>,
				      <<"1">>,<<"ManagedElement">>],
				attrs = [{<<"roleName">>,?STRING(<<"SystemAdministrator">>)}]}),

    mnesia:dirty_write(#config{ id = [<<"1">>,<<"Rule">>,
				      <<"1">>,<<"Role">>,
				      <<"1">>,<<"LocalAuthorizationMethod">>,
				      <<"1">>,<<"UserManagement">>,
				      <<"1">>,<<"SecM">>,
				      <<"1">>,<<"SystemFunctions">>,
				      <<"1">>,<<"ManagedElement">>],
				attrs = [{<<"permission">>,?ENUM(7)},
                                         {<<"ruleData">>,?STRING(<<"ManagedElement,*">>)},
                                         {<<"ruleName">>,?STRING(<<"ManagedElement_1">>)}
                                        ]}),

    %% SecM
    mnesia:dirty_write(#config{ id = [<<"1">>,<<"SecM">>,
                                      <<"1">>,<<"SystemFunctions">>,
				      <<"1">>,<<"ManagedElement">>],
				attrs = [{<<"secMId">>,?STRING(<<"1">>)}]}),
    mnesia:dirty_write(#config{ id = [<<"1">>,<<"UserManagement">>,
                                      <<"1">>,<<"SecM">>,
                                      <<"1">>,<<"SystemFunctions">>,
                                      <<"1">>,<<"ManagedElement">>],
				attrs = [{<<"userManagementId">>,?STRING(<<"1">>)}]}),


    mnesia:dirty_write(#config{ id = [<<"1">>,<<"Fm">>,
				      <<"1">>,<<"SystemFunctions">>,
				      <<"1">>,<<"ManagedElement">>],
				attrs = [{<<"fmId">>,?STRING(<<"1">>)},
					 {<<"sumWarning">>,?UINT32(0)},
					 {<<"sumMinor">>,?UINT32(0)},
					 {<<"sumMajor">>,?UINT32(0)},
					 {<<"sumCritical">>,?UINT32(0)},
					 {<<"heartbeatInterval">>,?UINT32(60)}
					]}),
    mnesia:dirty_write(#config{ id = [<<"1">>,<<"SystemFunctions">>,
				      <<"1">>,<<"ManagedElement">>],
				attrs = [{<<"systemFunctionsId">>,?STRING(<<"1">>)}]}),

    mnesia:dirty_write(#config{ id = [<<"1">>,<<"SysM">>,
				      <<"1">>,<<"SystemFunctions">>,
				      <<"1">>,<<"ManagedElement">>],
				attrs = [{<<"sysMId">>,?STRING(<<"1">>)}]
			      }),
    mnesia:dirty_write(#config{ id = [<<"1">>,<<"Snmp">>,
				      <<"1">>,<<"SysM">>,
				      <<"1">>,<<"SystemFunctions">>,
				      <<"1">>,<<"ManagedElement">>],
				attrs = [{<<"snmpId">>,?STRING(<<"1">>)},
					 {<<"opState">>,?ENUM(0)},
					 {<<"listenerPort">>,?UINT32(161)},
					 {<<"admState">>,?ENUM(0)}]
			      }),
    mnesia:dirty_write(#config{ id = [<<"1">>,<<"FmAlarmModel">>,
				      <<"1">>,<<"Fm">>,
				      <<"1">>,<<"SystemFunctions">>,
				      <<"1">>,<<"ManagedElement">>],
				attrs = [{<<"fmAlarmModelId">>,?STRING(<<"1">>)}]}),

    mnesia:dirty_write(#config{ id = [<<"1">>,<<"FmAlarmType">>,
				      <<"1">>,<<"FmAlarmModel">>,
				      <<"1">>,<<"Fm">>,
				      <<"1">>,<<"SystemFunctions">>,
				      <<"1">>,<<"ManagedElement">>],
				attrs = [{<<"fmAlarmTypeId">>,?STRING(<<"1">>)},
					 {<<"majorType">>,?UINT32(1)},
					 {<<"minorType">>,?UINT32(0)},
					 {<<"moClasses">>,?STRING(<<"">>)},
					 {<<"specificProblem">>,?STRING(<<"Test Error">>)},
					 {<<"eventType">>,?ENUM(1)},
					 {<<"probableCause">>,?UINT32(0)},
					 {<<"isStateful">>,?BOOL(true)},
					 {<<"additionalText">>,?STRING(<<"test">>)}
					]}),

    mnesia:dirty_write(#config{ id = [<<"2">>,<<"FmAlarmType">>,
				      <<"1">>,<<"FmAlarmModel">>,
				      <<"1">>,<<"Fm">>,
				      <<"1">>,<<"SystemFunctions">>,
				      <<"1">>,<<"ManagedElement">>],
				attrs = [{<<"fmAlarmTypeId">>,?STRING(<<"2">>)},
					 {<<"majorType">>,?UINT32(1)},
					 {<<"minorType">>,?UINT32(1)},
					 {<<"moClasses">>,?STRING(<<"">>)},
					 {<<"specificProblem">>,?STRING(<<"Test Error">>)},
					 {<<"eventType">>,?ENUM(1)},
					 {<<"probableCause">>,?UINT32(0)},
					 {<<"isStateful">>,?BOOL(false)},
					 {<<"additionalText">>,?STRING(<<"test">>)}
					]}),

    mnesia:dirty_write(#config{ id = [<<"1">>,<<"SnmpTargetV3">>,
				      <<"1">>,<<"Snmp">>,
				      <<"1">>,<<"SysM">>,
				      <<"1">>,<<"SystemFunctions">>,
				      <<"1">>,<<"ManagedElement">>],
				attrs = [{<<"snmpTargetV3Id">>,?STRING(<<"1">>)},
					 {<<"isMibWritable">>,?BOOL(true)},
					 {<<"snmpSecurityLevel">>,?ENUM(1)},
					 {<<"transportMethod">>,?ENUM(1)},
					 {<<"informTimeout">>,?UINT32(300)},
					 {<<"informRetryCount">>,?UINT32(1)},
					 {<<"privProtocol">>,?ENUM(0)},
					 {<<"authProtocol">>,?ENUM(0)},
					 {<<"authKey">>,?STRING(<<"SuperSecret">>)},
					 {<<"privKey">>,?STRING(<<"SuperSecret">>)},
					 {<<"user">>,?STRING(<<"com_user">>)},
					 {<<"port">>,?UINT32(5000)},
					 {<<"address">>,?STRING(<<"127.0.0.1">>)},
					 {<<"opState">>,?ENUM(0)},
					 {<<"admState">>,?ENUM(1)}]}),

    [{[<<"ManagedElement">>], ?MODULE}].



