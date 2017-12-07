%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_imm_special_applier.erl
%%
%% Description:
%%    Handling the AVC notifictations
%%    http://devel.opensaf.org/ticket/2873
%%--------------------------------------------------------------------
-module(safs_imm_special_applier).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_ntf.hrl").
%%-include("om.hrl").
%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
% Start and stop
-export([
	 start_link/0,
	 start/0,
	 stop/0
        ]).

% API
-export([
	 ccb_object_create_2/5,
	 ccb_object_delete/3,
	 ccb_object_modify_2/4,
	 ccb_completed/2,
	 ccb_apply/2,
	 ccb_abort/2,
	 rt_attr_update/4,
	 admin_operation_2/5,
	 filter_ccb/1
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3,
 	 notification_object_create/3
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(SAFS_SPECIAL_APPLIER_NOTIFYING_OBJECT, <<"safApp=safImmService">>).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
	  special_appliers = []
	 }).

-record(safs_imm_special_applier, {
	  ccbid,
	  filter = false,
	  notifications=[]
	 }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: start/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: start/0
%% Description: Starts the server
%%--------------------------------------------------------------------
stop() ->
    call(stop).

ccb_object_create_2(_OiHandle, 0, ClassName, ParentName, Attr) ->
    %io:format("SpecialApplier(): create RT\n",[]),
    case safs:get_env(imm_notification_category, all) of
	none ->
	    ok;
	config ->
	    ok;
	_ ->
	    case safs_ntf:initialize(undefined) of
		{ok, NtfHandle, _Version} ->
		    send_avc_notifications(0, NtfHandle, [{create, ClassName, ParentName, Attr}]),
		    safs_ntf:finalize(NtfHandle),
		    ok;
		{error, ErrorCode} ->
		    error_logger:format("~p(~p) couldn't initialize NTF, error_code=~p\n",
					[?MODULE, self(), ErrorCode]),
		    ok
	    end
    end;
ccb_object_create_2(_OiHandle, CcbId, ClassName, ParentName, Attr) ->
    %io:format("SpecialApplier(~p): create\n",[CcbId]),
    case safs:get_env(imm_notification_category, all) of
	none ->
	    ok;
	runtime ->
	    ok;
	_ ->
	    update_avc(CcbId, {create, ClassName, ParentName, Attr}),
	    ok
     end.

ccb_object_delete(_OiHandle, 0, ObjectName) ->
    %io:format("SpecialApplier(): delete RT\n",[]),
    case safs:get_env(imm_notification_category, all) of
	none ->
	    ok;
	config ->
	    ok;
	_ ->
	    case safs_ntf:initialize(undefined) of
		{ok, NtfHandle, _Version} ->
		    send_avc_notifications(0, NtfHandle, [{delete, ObjectName, []}]),
		    safs_ntf:finalize(NtfHandle),
		    ok;
		{error, ErrorCode} ->
		    error_logger:format("~p(~p) couldn't initialize NTF, error_code=~p\n",
					[?MODULE, self(), ErrorCode]),
		    ok
	    end
    end;
ccb_object_delete(_OiHandle, CcbId, ObjectName) ->
    %io:format("SpecialApplier(~p): delete\n",[CcbId]),
    case safs:get_env(imm_notification_category, all) of
	none ->
	    ok;
	runtime ->
	    ok;
	_ ->
	     update_avc(CcbId, {delete, ObjectName}),
	     ok
     end.

ccb_object_modify_2(_OiHandle, 0, ObjectName, AttrMods) ->
    %io:format("SpecialApplier(): modify RT\n",[]),
    case safs:get_env(imm_notification_category, all) of
	none ->
	    ok;
	config ->
	    ok;
	_ ->
	    case safs_ntf:initialize(undefined) of
		{ok, NtfHandle, _Version} ->
		    send_avc_notifications(0, NtfHandle, [{modify, ObjectName, AttrMods}]),
		    safs_ntf:finalize(NtfHandle),
		    ok;
		{error, ErrorCode} ->
		    error_logger:format("~p(~p) couldn't initialize NTF, error_code=~p\n",
					[?MODULE, self(), ErrorCode]),
		    ok
	    end
    end;
ccb_object_modify_2(_OiHandle, CcbId, ObjectName, AttrMods) ->
    %io:format("SpecialApplier(~p): modify\n",[CcbId]),
    case safs:get_env(imm_notification_category, all) of
	none ->
	    ok;
	runtime ->
	    ok;
	_ ->
	    update_avc(CcbId, {modify, ObjectName, AttrMods}),
	    ok
    end.

ccb_completed(_OiHandle, _CcbId) ->
    %io:format("SpecialApplier(~p): completed\n",[CcbId]),
    ok.

ccb_apply(_OiHandle, CcbId) ->
    %io:format("SpecialApplier(~p): apply\n",[CcbId]),
    case safs:get_env(imm_notification_category, all) of
	none ->
	    ok;
	runtime ->
	    ok;
	_ ->
	    case lookup_tbl(safs_imm_special_applier, CcbId) of
		undefined ->
		    %io:format("SpecialApplier(~p): avc undefined\n",[CcbId]),
		    ok;
		Object ->
		    case Object#safs_imm_special_applier.filter of
			true ->
			    %io:format("SpecialApplier(~p): filter\n",[CcbId]),
			    ok;
			false ->
			    % io:format("SpecialApplier(~p):\n  Notification List: ~p\n",
			    % 	      [CcbId, Object#safs_imm_special_applier.notifications]),
			    send_avcs_for_ccb(Object)
		    end,
		    delete_tbl(safs_imm_special_applier, CcbId),
		    ok
	    end
    end.

ccb_abort(_OiHandle, CcbId) ->
    %io:format("SpecialApplier(~p): abort\n",[CcbId]),
    case safs:get_env(imm_notification_category, all) of
	none ->
	    ok;
	runtime ->
	    ok;
	_ ->
	    delete_tbl(safs_imm_special_applier, CcbId),
	    ok
    end.

rt_attr_update(_OiHandle, _ImplementerName, _ObjectName, _AttributeNames) ->
    ok.

admin_operation_2(_OiHandle, _Invocation, _ObjectName, _OperationId, _Params) ->
    ok.

filter_ccb(CcbHandle) ->
    try
        CcbId = safs_imm_om:get_ccbid_from_ccb_handle(CcbHandle),
	case lookup_tbl(safs_imm_special_applier, CcbId) of
	    undefined ->
		ok;
	    Object ->
		insert_tbl(safs_imm_special_applier,
			   Object#safs_imm_special_applier{filter = true}),
		ok
	end
    catch
	throw:Error -> Error
    end.

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
%%LATH: Start and Stop must be fixed when multi node, can't be ets ????
init([]) ->
    ets:new(safs_imm_special_applier, [public, named_table,
			   {keypos, #safs_imm_special_applier.ccbid}]),
    Version = #safsVersion{releaseCode = $A,
			   majorVersion = 2,
			   minorVersion = 13},
    {ok, ApplierHandle, Version} = safs_imm_oi:initialize(?MODULE, Version),
    safs_imm_oi:implementer_set(ApplierHandle, <<"@SafsImmReplicatorA">>),
    {ok, #state{special_appliers=[ApplierHandle]}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Msg, From, #state{} = State) ->
    error_logger:format("~p~p got unexpected call from ~p:\n\t~p\n",
                        [?MODULE, self(), From, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, #state{} = State) ->
    error_logger:format("~p~p got unexpected cast:\n\t~p\n",
                        [?MODULE, self(), Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Msg, #state{} = State) ->
    error_logger:format("~p~p got unexpected message:\n\t~p\n",
                        [?MODULE, self(), Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change/3
%% Description: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: send_avcs_for_ccb/1
%% Description:
%%--------------------------------------------------------------------
send_avcs_for_ccb(Object) ->
    case safs_ntf:initialize(undefined) of
	{ok, NtfHandle, _Version} ->
	    send_avc_notifications(Object#safs_imm_special_applier.ccbid, NtfHandle, lists:reverse(Object#safs_imm_special_applier.notifications)),
	    safs_ntf:finalize(NtfHandle),
	    ok;
	{error, ErrorCode} ->
	    error_logger:format("~p(~p) couldn't initialize NTF, error_code=~p\n",
				[?MODULE, self(), ErrorCode]),
	    ok
    end.

%%--------------------------------------------------------------------
%% Sending Notifications
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function: send_avc_notifications/3
%% Description:
%%--------------------------------------------------------------------
send_avc_notifications(_CcbId, _NtfHandle, []) ->
    ok;
send_avc_notifications(CcbId, NtfHandle, [{create, ClassName, ParentName, Attrs} |Notifications]) ->
    NotificationObject = notification_object_create(ParentName, ClassName, Attrs),
    AttrsUpdated = case CcbId of
		       0 ->
			   Attrs;
		       _ ->
			   update_attrs(create, CcbId, last_notification(Notifications), Attrs)
		   end,
    NotificationHeader =
	#safsNtfNotificationHeader{eventType=sa_ntf_object_creation,
				   notificationObject = NotificationObject,
				   notifyingObject = ?SAFS_SPECIAL_APPLIER_NOTIFYING_OBJECT,
				   notificationClassId =
				   #safsNtfClassId{vendorId=?SA_NTF_VENDOR_ID_SAF,
						   majorId=8,  %% SA_SVC_IMMS
						   minorId=0},
				   eventTime = timestamp(),
				   correlatedNotifications=[],
				   additionalInfo = additional_info(AttrsUpdated, [], 0)
				  },

    CreateNotification =
	#safsNtfObjectCreateDeleteNotification{notificationHeader=NotificationHeader,
					       sourceIndicator=source_indicator(CcbId),
					       objectAttributes=convert_attribute_list(AttrsUpdated, [], 0)
					      },
    safs_ntf:notification_send(NtfHandle, CreateNotification),
    send_avc_notifications(CcbId, NtfHandle, Notifications);
send_avc_notifications(CcbId, NtfHandle, [{delete, ObjectName, Attrs} |Notifications]) ->
    AttrsUpdated = case CcbId of
		       0 ->
			   Attrs;
		       _ ->
			   update_attrs(delete, CcbId, last_notification(Notifications), Attrs)
		   end,
    NotificationHeader =
	#safsNtfNotificationHeader{eventType=sa_ntf_object_deletion,
				   notificationObject = ObjectName,
				   notifyingObject = ?SAFS_SPECIAL_APPLIER_NOTIFYING_OBJECT,
				   notificationClassId =
				   #safsNtfClassId{vendorId=?SA_NTF_VENDOR_ID_SAF,
						   majorId=8,  %% SA_SVC_IMMS
						   minorId=0},
				   eventTime = timestamp(),
				   correlatedNotifications=[],
				   additionalInfo = additional_info(AttrsUpdated, [], 0)
				  },
    DeleteNotification =
	#safsNtfObjectCreateDeleteNotification{notificationHeader=NotificationHeader,
					       sourceIndicator=source_indicator(CcbId),
					       objectAttributes=convert_attribute_list(AttrsUpdated, [], 0)
					      },
    safs_ntf:notification_send(NtfHandle, DeleteNotification),
    send_avc_notifications(CcbId, NtfHandle, Notifications);
send_avc_notifications(CcbId, NtfHandle, [{modify, ObjectName, AttrMods} |Notifications]) ->
    AttrModsUpdated = case CcbId of
			  0 ->
			      AttrMods;
			  _ ->
			      update_attrs(modify, CcbId, last_notification(Notifications), AttrMods)
		      end,
    NotificationHeader =
	#safsNtfNotificationHeader{eventType=sa_ntf_attribute_changed,
				   notificationObject = ObjectName,
				   notifyingObject = ?SAFS_SPECIAL_APPLIER_NOTIFYING_OBJECT,
				   notificationClassId =
				   #safsNtfClassId{vendorId=?SA_NTF_VENDOR_ID_SAF,
						   majorId=8,  %% SA_SVC_IMMS
						   minorId=0},
				   eventTime = timestamp(),
				   correlatedNotifications=[],
				   additionalInfo = additional_info(AttrModsUpdated, [], 0)    				   	       		  },
    ModifyNotification =
	#safsNtfAttributeChangeNotification{notificationHeader=NotificationHeader,
					    sourceIndicator=source_indicator(CcbId),
					    changedAttributes=convert_attribute_list(AttrModsUpdated, [], 0)
					   },
    safs_ntf:notification_send(NtfHandle, ModifyNotification),
    send_avc_notifications(CcbId, NtfHandle, Notifications).


notification_object_create(ParentName, ClassName, Attrs) ->
   [{RName, _RType} |_] = safs_imm_db:get_class_types(ta(ClassName)),
    #safsImmAttrValues_2{attrName=Name, attrValueType=Type, attrValues=Values} =
	lists:keyfind(tb(RName), 2, Attrs),
    {ok, [Value]} = safs_imm_lib:get_attr_values(Type, Values),
    case ParentName of
	undefined ->
	    <<Name/binary,"=", Value/binary>>;
	_ ->
	    <<Name/binary,"=", Value/binary, ",", ParentName/binary>>
    end.

timestamp() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    ((MegaSecs*1000000 + Secs)*1000000 + MicroSecs)*1000.

source_indicator(0) ->
    sa_ntf_object_operation;
source_indicator(_) ->
    sa_ntf_management_operation.

convert_attribute_list([], Acc, _Id) ->
        lists:reverse(Acc);
convert_attribute_list([#safsImmAttrModification_2{modAttr=#safsImmAttrValues_2{attrValueType=Type,
										attrValuesNumber=_N,
										attrValues=Values}} |AttrMods],
		Acc,
		Id) ->
   convert_attribute_list(AttrMods,
			   convert_changed_attribute_values(Type, Values, Acc, Id),
			   Id+1);
convert_attribute_list([#safsImmAttrValues_2{attrValueType=Type,
					     attrValuesNumber=_N,
					     attrValues=Values
					    } |Attrs],
		       Acc,
		       Id) ->
    convert_attribute_list(Attrs,
			   convert_attribute_values(Type, Values, Acc, Id),
			   Id+1).

convert_attribute_values(_Type, [], Acc, _Id) ->
    Acc;
convert_attribute_values(Type, [Value |Values], Acc, Id) ->
    convert_attribute_values(Type,
			     Values,
			     [#safsNtfAttribute{attributeId=Id,
						attributeType=convert_immtype_to_ntftype(Type),
						attributeValue=convert_immvalue_to_ntfvalue(Type, Value)
					       } |Acc],
			      Id).

convert_changed_attribute_values(_Type, [], Acc, _Id) ->
    Acc;
convert_changed_attribute_values(Type, [Value |Values], Acc, Id) ->
    convert_changed_attribute_values(Type,
			     Values,
			     [#safsNtfAttributeChange{attributeId=Id,
						      attributeType=convert_immtype_to_ntftype(Type),
						      newAttributeValue=convert_immvalue_to_ntfvalue(Type, Value)
					       } |Acc],
			      Id).

additional_info([], Acc, _Id) ->
    lists:reverse(Acc);
additional_info([#safsImmAttrModification_2{modAttr=#safsImmAttrValues_2{attrName = Name}} |AttrMods],
		Acc,
		Id) ->
    additional_info(AttrMods,
		    [#safsNtfAdditionalInfo{infoId=Id,
					    infoType=sa_ntf_value_string,
					    infoValue=Name} |Acc],
		    Id+1);
additional_info([#safsImmAttrValues_2{attrName = Name} |Attrs],
		Acc,
		Id) ->
    additional_info(Attrs,
		    [#safsNtfAdditionalInfo{infoId=Id,
					    infoType=sa_ntf_value_string,
					    infoValue=Name} |Acc],
		    Id+1).


convert_immtype_to_ntftype(sa_imm_attr_saint32t) ->
    sa_ntf_value_int32;
convert_immtype_to_ntftype(sa_imm_attr_sauint32t) ->
    sa_ntf_value_uint32;
convert_immtype_to_ntftype(sa_imm_attr_saint64t) ->
    sa_ntf_value_int64;
convert_immtype_to_ntftype(sa_imm_attr_sauint64t) ->
    sa_ntf_value_uint64;
convert_immtype_to_ntftype(sa_imm_attr_satimet) ->
    sa_ntf_value_uint64;
convert_immtype_to_ntftype(sa_imm_attr_sanamet) ->
    sa_ntf_value_ldap_name;
convert_immtype_to_ntftype(sa_imm_attr_safloatt) ->
    sa_ntf_value_float;
convert_immtype_to_ntftype(sa_imm_attr_sadoublet) ->
    sa_ntf_value_double;
convert_immtype_to_ntftype(sa_imm_attr_sastringt) ->
    sa_ntf_value_string;
convert_immtype_to_ntftype(sa_imm_attr_saanyt) ->
    sa_ntf_value_binary;
convert_immtype_to_ntftype(sa_imm_attr_csstructt) ->
    sa_ntf_value_csstructt.


convert_immvalue_to_ntfvalue(sa_imm_attr_sauint32t, #safsImmAttrValue{sauint32=Value})->
    #safsNtfValue{uint32Val=Value};
convert_immvalue_to_ntfvalue(sa_imm_attr_saint32t, #safsImmAttrValue{saint32=Value}) ->
    #safsNtfValue{int32Val=Value};
convert_immvalue_to_ntfvalue(sa_imm_attr_safloatt, #safsImmAttrValue{safloat=Value}) ->
    #safsNtfValue{floatVal=Value};
convert_immvalue_to_ntfvalue(sa_imm_attr_sauint64t, #safsImmAttrValue{sauint64=Value}) ->
    #safsNtfValue{uint64Val=Value};
convert_immvalue_to_ntfvalue(sa_imm_attr_saint64t, #safsImmAttrValue{saint64=Value}) ->
    #safsNtfValue{int64Val=Value};
convert_immvalue_to_ntfvalue(sa_imm_attr_satimet, #safsImmAttrValue{satime=Value}) ->
    #safsNtfValue{uint64Val=Value};
convert_immvalue_to_ntfvalue(sa_imm_attr_sanamet, #safsImmAttrValue{saname=Value}) ->
    #safsNtfValue{variable=Value};
convert_immvalue_to_ntfvalue(sa_imm_attr_sadoublet, #safsImmAttrValue{sadouble=Value}) ->
    #safsNtfValue{doubleVal=Value};
convert_immvalue_to_ntfvalue(sa_imm_attr_sastringt, #safsImmAttrValue{sastring=Value}) ->
    #safsNtfValue{variable=Value};
convert_immvalue_to_ntfvalue(sa_imm_attr_saanyt, #safsImmAttrValue{saany=Value}) ->
    #safsNtfValue{variable=Value};
convert_immvalue_to_ntfvalue(sa_imm_attr_csstructt, #safsImmAttrValue{csstruct=Value}) ->
    #safsNtfValue{csstruct=Value}.


update_attrs(create, CcbId, LastNotificationInCcb, [Ao, Class |Attrs]) ->
    {ok, CcbIdValue} = safs_imm_lib:set_attr_values(sa_imm_attr_sauint64t, [CcbId]),
    {ok, CcbLastValue} = safs_imm_lib:set_attr_values(sa_imm_attr_sauint32t, [LastNotificationInCcb]),
    [Ao, Class,
     #safsImmAttrValues_2{attrName = <<"SaImmOiCcbIdT">>,
			  attrValueType=sa_imm_attr_sauint64t,
			  attrValuesNumber=1,
			  attrValues=CcbIdValue},
     #safsImmAttrValues_2{attrName = <<"ccbLast">>,
			  attrValueType=sa_imm_attr_sauint32t,
			  attrValuesNumber=1,
			  attrValues=CcbLastValue}
     | Attrs];
update_attrs(modify, CcbId, LastNotificationInCcb, [Ao, Class |AttrMods]) ->
    {ok, CcbIdValue} = safs_imm_lib:set_attr_values(sa_imm_attr_sauint64t, [CcbId]),
    {ok, CcbLastValue} = safs_imm_lib:set_attr_values(sa_imm_attr_sauint32t, [LastNotificationInCcb]),
    [Ao, Class,
     #safsImmAttrModification_2{modType=sa_imm_attr_values_replace,
				modAttr=#safsImmAttrValues_2{attrName = <<"SaImmOiCcbIdT">>,
							     attrValueType=sa_imm_attr_sauint64t,
							     attrValuesNumber=1,
							     attrValues=CcbIdValue}},
     #safsImmAttrModification_2{modType=sa_imm_attr_values_replace,
				modAttr=#safsImmAttrValues_2{attrName = <<"ccbLast">>,
							     attrValueType=sa_imm_attr_sauint32t,
							     attrValuesNumber=1,
							     attrValues=CcbLastValue}}
     | AttrMods];
update_attrs(delete, CcbId, LastNotificationInCcb, [Ao]) ->
    {ok, CcbIdValue} = safs_imm_lib:set_attr_values(sa_imm_attr_sauint64t, [CcbId]),
    {ok, CcbLastValue} = safs_imm_lib:set_attr_values(sa_imm_attr_sauint32t, [LastNotificationInCcb]),
    [Ao#safsImmAttrModification_2.modAttr,
     #safsImmAttrValues_2{attrName = <<"SaImmOiCcbIdT">>,
			  attrValueType=sa_imm_attr_sauint64t,
			  attrValuesNumber=1,
			  attrValues=CcbIdValue},
     #safsImmAttrValues_2{attrName = <<"ccbLast">>,
			  attrValueType=sa_imm_attr_sauint32t,
			  attrValuesNumber=1,
			  attrValues=CcbLastValue}
     ].

last_notification([]) ->
    1; % SA_TRUE
last_notification(_) ->
    0. % SA_FALSE

%%--------------------------------------------------------------------
%%DB functions
%%--------------------------------------------------------------------
update_avc(CcbId, {delete, ObjectName}) ->
    case lookup_tbl(safs_imm_special_applier, CcbId) of
	undefined ->
	    error_logger:warning_msg("Special Applier: A delete operation should NEVER be first message!!!\n"
				     "                 Missing a modify/create message with Admin Owner.\n"),
	    ok;
	Object ->
	    % io:format("SpecialApplier(~p): record exists\n~p\n", [CcbId, Object]),
	    [{modify, ObjectName, AttrMods} |Notifications] = Object#safs_imm_special_applier.notifications,
	    insert_tbl(safs_imm_special_applier,
		       Object#safs_imm_special_applier{
			 notifications = [{delete, ObjectName, AttrMods} |Notifications]
			})
    end;
update_avc(CcbId, Op) ->
    case lookup_tbl(safs_imm_special_applier, CcbId) of
	undefined ->
	    % io:format("SpecialApplier(~p): new record\n", [CcbId]),
	    insert_tbl(safs_imm_special_applier,
		       #safs_imm_special_applier{
			  ccbid = CcbId,
			  notifications = [Op]
			 });
	Object ->
	    % io:format("SpecialApplier(~p): record exists\n~p\n", [CcbId, Object]),
	    insert_tbl(safs_imm_special_applier,
		       Object#safs_imm_special_applier{
			 notifications = [Op |Object#safs_imm_special_applier.notifications]
			})
    end.

insert_tbl(Table, Obj) ->
    ets:insert(Table, Obj).

lookup_tbl(Table, Key) ->
    case ets:lookup(Table, Key) of
	[] ->
	    undefined;
	[Object] ->
	    Object
    end.

delete_tbl(Table, Handle) ->
    ets:delete(Table, Handle),
    ok.
%%----------------------------------------------------------------------
%% Commonly used functions
%%----------------------------------------------------------------------
ta(B) when is_binary(B) ->
    list_to_atom(unicode:characters_to_list(B, utf8));
ta(L) when is_list(L) ->
    list_to_atom(L);
ta(A) when is_atom(A) ->
    A.

%%--------------------------------------------------------------------
tb(B) when is_binary(B) ->
    B;
tb(L) when is_list(L) ->
    list_to_binary(L);
tb(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A)).

call(Request) ->
    gen_server:call(?SERVER, Request, infinity).

% cast(Request) ->
%     gen_server:cast(?SERVER, Request).
