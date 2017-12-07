%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rcsSecM.erl %
%%% @author etxtory
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/7
%%%
%%% @doc ==Agent implementation for Security management==
%%% This is the agent implementation the security management model

-module(rcsSecM).
-vsn('/main/R1A/R2A/R3A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/7').
-date('2017-11-28').
-author('etxtory').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R1A/1      2012-01-10 etxbjca     Created
%%% R1A/12     2012-08-15 etxjotj     Bugfix in method order
%%% R1A/13     2012-10-04 etxarnu     Changed LDAP server address
%%% R1A/14     2012-10-05 etxarnu     Reverted LDAP address
%%% R2A/9      2013-04-04 erarafo     Support for upgrade
%%% R2A/21     2014-02-19 etxlg       localAuthorization to LOCKED (for now)
%%% R2A/22     2014-02-27 etxlg       Populate ericssonFilter
%%% R2A/23     2014-02-28 etxlg       Filter functions, extend get_ldap_config
%%% R2A/24     2014-03-14 etxlg       handle filterType=[] (deprecated - remove)
%%% R2A/25     2014-04-25 etxjotj     Fix of HS52659 Delete case
%%% R2A/26     2014-06-23 etxarnu     Use sysEnv:com_top() to find COM top
%%% R2A/27     2014-07-11 etxjotj     Removed test printout
%%% R2A/28     2014-08-06 etxjotj     Unlocked ldap authentication
%%% R2A/29     2014-08-18 etxjotj     Unlocked local authorization
%%% R2A/30     2014-08-18 etxjotj     Removed expert role on secure boards
%%% R2A/31     2014-08-19 etxjotj     Switched to sysInitI
%%% R2A/32     2014-09-09 etxjotj     HS94339 Upgrade customRole/Rule config
%%% R2A/33     2014-09-11 etxjotj     HS94877 Default role/rule upgrade
%%% R2A/34     2014-09-11 etxjotj     HS94877 again
%%% R2A/35     2014-09-11 etxjotj     HS94877 again again
%%% R2A/36     2014-09-18 etxberb     Added validate/3.
%%% R3A/1      2014-11-26 etxarnu     Added version to ericssonFilter
%%% R3A/2      2014-11-29 etxarnu     Bug fix in transform
%%% R3A/3      2014-12-02 etxberb     Added values/1.
%%% R3A/4      2014-12-04 etxarnu     Reverted ericssonFilter for now
%%% R3A/5      2014-12-12 etxarnu     New try with ericssonFilter
%%% R3A/6      2014-12-04 etxarnu     Reverted ericssonFilter for now
%%% R3A/8      2015-01-27 etxjotj     LDAP config moved to OMC
%%% R3A/9      2015-03-17 etxtory     getMoAttributes fix
%%% R5A/1      2016-01-26 emariad     Added TLS MO with attributes
%%% R5A/2      2016-04-01 emariad     Commented out all my TLS MO changes, to be delivered in 17A instead
%%% R6A/1      2016-05-04 uabhgma     ComSecM -> RcsSecM, including module name prefix change to 'rcs'
%%% R6A/3      2016-05-30 enenteo     Uplift to  ECIM SecM 2.2
%%% R6A/4      2016-08-15 emariad     Added SSH and TLS implemenation
%%% R6A/5      2016-09-08 emariad     Fixed problem with upgrade to new OTP version for TLS part
%%% R6A/6      2016-09-22 emariad     Don't notify TLS/SSH servers when new cipher config match old config
%%% ----------------------------------------------------------
%%% R7A/1      2016-11-11 emariad     HV40605, added 1s timeout for restart of TLS/SSH servers (cipher config)
%%% ----------------------------------------------------------
%%% R8A/2      2016-12-13 estjako     Added cipher change callback to FTPES
%%% ----------------------------------------------------------
%%% R9A/1      2017-02-14 estjako     Changed callback for FTPES
%%% R9A/2      2017-11-16 etxjotj     HW44638 change in modelparser for string 
%%%                                   type default values
%%% ----------------------------------------------------------
%%% R10A/1     2017-05-16 etxjotj     Expert role handling for vrcs
%%% R10A/2     2017-06-22 etxpeno     Support for dev_patches when validating XML
%%% ----------------------------------------------------------
%%% R11A/1     2017-09-04 etxjotj     Replaced swmLib with swmI
%%% R11A/3     2017-11-16 etxarnu     Removed atom_to_list for legalNotice
%%% R11A/4     2017-11-20 etxjotj     HW44638 change in modelparser for string
%%%                                   type default values
%%% R11A/5     2017-11-21 etxarnu     HW44638 
%%% R11A/6     2017-11-27 etxtory     Introduce diffie-hellman
%%% R11A/7     2017-11-28 etxtory     Reverse order 
%%%
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).
-export([validate/3]).

-export([existsMo/2,
         countMoChildren/3,
         getMoAttributes/3,
         setMoAttributes/3,
         createMo/5]).

-export([init_data/1]).
-export([get_all_security_roles/0]).
-export([get_target_type/0]).
-export([reload_roles/0]).

-export([get_tls_enabled_ciphers/0,
         get_tls_cipher_filter/0]).

-export([get_ssh_preferred_algorithms/0,
	 get_ssh_modify_algorithms/0]).

%% Test interface
-export([verify_selected_ssh_algos_after_upgrade/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RcsLocalAuthorization.hrl").
-include("RcsSecM.hrl").
-include("ComTop.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%% @doc Returns true if the specified instance exists.
existsMo(DnRev, _) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).

%%% @doc Returns the number of MO instances of given class
%%% directly below the specified parent.
countMoChildren(DnRev, Class, _) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(Class))).

%%% @doc Gets MO attribute values.
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.
getMoAttributes(AttrNames, DnRev, TxHandle) ->
    case comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))) of
	true  ->
	    [getMoAttribute([AN | DnRev], TxHandle) || AN <- AttrNames];
	false ->
	    []
    end.

getMoAttribute([<<"authenticationMethodOrder">>|DnRev], _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev),
    case mnesia:read(authenticationOrder, Key) of
	[] ->
	    undefined;
	[Obj] ->
	    MethodOrder = Obj#authenticationOrder.authenticationMethodOrder,
	    comsaGeneric:format_struct(MethodOrder, ?'MethodOrder_types')
    end;
getMoAttribute([<<"authorizationMethodOrder">>|DnRev], _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev),
    case mnesia:read(authorizationOrder, Key) of
	[] ->
	    undefined;
	[Obj] ->
	    MethodOrder = Obj#authorizationOrder.authorizationMethodOrder,
	    comsaGeneric:format_struct(MethodOrder, ?'MethodOrder_types')
    end;
getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

setMoAttributes([], DnRev, _TransId) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), []);

setMoAttributes([{AttrName, TypeAndValue}|Rest], DnRev, TransId) ->
    R = setMoAttribute([AttrName|DnRev], TypeAndValue, undefined, TransId),
    case Rest of
        [] -> R;
        _  -> setMoAttributes(Rest, DnRev, TransId)
    end.

%% returns a converted value when the value is contained in a tuple
get_values({_,Value}) ->
    [binary_to_list(Value)];
%% returns a list of converted values when the values are contained in a list of tuples
get_values(TypeAndValues) ->
    [binary_to_list(Value) || {_,Value}<-TypeAndValues].

setMoAttribute([<<"cipherFilter">>|DnRev], {_,Value}, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    Key = comsaGeneric:dnrev_to_key(DnRev),
    case mnesia:read(Table, Key) of
        [] ->
            mnesia:abort({no_obj, Table, Key});
        [Obj] ->
            Attr = binary_to_list(Value),
            set_tls_enabled_ciphers(Attr, Obj),
            NewObj = Obj#tls{cipherFilter=Attr},
            mnesia:write(NewObj),
            {ok, NewObj}
    end;

setMoAttribute([<<"selectedCiphers">>|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    Key = comsaGeneric:dnrev_to_key(DnRev),
    ValueList = get_values(TypeAndValue),
    case mnesia:read(Table, Key) of
        [] -> mnesia:abort({no_obj, Table, Key});
        [Obj] -> SupportedCiphers = Obj#ssh.supportedCiphers,
                 case Selected = comsaSshConfig:extract_selected_algorithms
                     (SupportedCiphers, ValueList) of
                      [] ->
                         mnesia:abort({'no resulting ciphers', Table, Key});
                     _ ->
                         case Selected =:= Obj#ssh.selectedCiphers of
                             true ->
                                 ok;
                             false ->
                                 put(sshAlgosChanged, yes)
                         end,
                         NewObj = Obj#ssh{selectedCiphers=Selected },
                         mnesia:write(NewObj),
                         {ok, NewObj}
                 end
    end;

setMoAttribute([<<"selectedKeyExchanges">>|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    Key = comsaGeneric:dnrev_to_key(DnRev),

    ValueList = get_values(TypeAndValue),
    case mnesia:read(Table, Key) of
        [] -> mnesia:abort({no_obj, Table, Key});
        [Obj] -> SupportedKeyExchanges = Obj#ssh.supportedKeyExchanges,
                 case Selected = comsaSshConfig:extract_selected_algorithms
                     (SupportedKeyExchanges, ValueList) of
                      [] ->
                         mnesia:abort({'no resulting kex', Table, Key});
                     _ ->
                         case Selected =:= Obj#ssh.selectedKeyExchanges of
                             true ->
                                 ok;
                             false ->
                                 put(sshAlgosChanged, yes)
                         end,
                         NewObj = Obj#ssh{selectedKeyExchanges=Selected },
                         mnesia:write(NewObj),
                         {ok, NewObj}
                 end
    end;

setMoAttribute([<<"selectedMacs">>|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    Key = comsaGeneric:dnrev_to_key(DnRev),

    ValueList = get_values(TypeAndValue),
    case mnesia:read(Table, Key) of
        [] -> mnesia:abort({no_obj, Table, Key});
        [Obj] -> SupportedMacs = Obj#ssh.supportedMacs,
                 case Selected = comsaSshConfig:extract_selected_algorithms
                     (SupportedMacs, ValueList) of
                       [] ->
                         mnesia:abort({'no resulting macs', Table, Key});
                     _ ->
                         case Selected =:= Obj#ssh.selectedMacs of
                             true ->
                                 ok;
                             false ->
                                 put(sshAlgosChanged, yes)
                         end,
                         NewObj = Obj#ssh{selectedMacs=Selected },
                         mnesia:write(NewObj),
                         {ok, NewObj}
                 end
    end;

%%% Unfortunately COMTE cannot differ between X and [X] so we must handle
%%% that sometimes TypeAndValue is not a list
setMoAttribute([<<"authorizationMethodOrder">>|DnRev], TypeAndValue, _, _) ->
    Table=table(comsaGeneric:class(DnRev)),
    Key = comsaGeneric:dnrev_to_key(DnRev),
    Value = case TypeAndValue of
		TypeAndValue when is_tuple(TypeAndValue) ->
		    Struct = comsaGeneric:make_struct(
			       methodOrder, TypeAndValue, ?'MethodOrder_types'),
		    [Struct];
		TypesAndValues when is_list(TypesAndValues) ->
		    [comsaGeneric:make_struct(
		       methodOrder, TAV, ?'MethodOrder_types')||
			TAV<-TypeAndValue]
	    end,
    case mnesia:read(Table, Key) of
	[] ->
	    mnesia:abort({no_obj, Table, Key});
	[Obj] ->
	    NewObj = Obj#authorizationOrder{authorizationMethodOrder=Value},
	    mnesia:write(NewObj),
	    {ok, NewObj}
    end;
setMoAttribute([<<"authenticationMethodOrder">>|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    Key = comsaGeneric:dnrev_to_key(DnRev),
    Value = case TypeAndValue of
		TypeAndValue when is_tuple(TypeAndValue) ->
		    Struct = comsaGeneric:make_struct(
			       methodOrder, TypeAndValue, ?'MethodOrder_types'),
		    [Struct];
		TypesAndValues when is_list(TypesAndValues) ->
		    [comsaGeneric:make_struct(
		       methodOrder, TAV, ?'MethodOrder_types')||
			TAV<-TypeAndValue]
	    end,
    case mnesia:read(Table, Key) of
	[] ->
	    mnesia:abort({no_obj, Table, Key});
	[Obj] ->
	    NewObj = Obj#authenticationOrder{authenticationMethodOrder=Value},
	    mnesia:write(NewObj),
	    {ok, NewObj}
    end;

setMoAttribute([Attribute= <<"rules">>|DnRev], TypesAndValues, _, _)
  when is_list(TypesAndValues) ->
    Table = customRole = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypesAndValues);


setMoAttribute([Attribute= <<"rules">>|DnRev], TypeAndValue, _, _) ->
    Table = customRole = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), [TypeAndValue]);

setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).

createMo([ClassName | ParentDnRev],
	 _KeyAttrName,
	 KeyValue,
	 InitAttrs,
	 _TransId) ->
    Table = table(binary_to_list(ClassName)),
    comsaGeneric:create(Table,
			ParentDnRev,
			KeyValue,
			values(InitAttrs),
			types(Table)).

createMo([Class|ParentDnRev], _, IxValueB, _) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table, ParentDnRev, IxValueB).

%% HS52659 fix

deleteMo([_,Class|_] = DnRev, _) when Class== <<"CustomRole">> ->
    comsaGeneric:delete(DnRev, table(binary_to_list(Class)));
deleteMo([_,Class|_] = DnRev, _) when Class== <<"CustomRule">> ->
    comsaGeneric:delete(DnRev, table(binary_to_list(Class))).

%% Fix ends here

table("SecM") -> secM;
table("AuthenticationOrder") -> authenticationOrder;
table("AuthorizationOrder") -> authorizationOrder;
table("Tls") -> tls;
table("Ssh") -> ssh;
table("UserManagement") -> userManagement;
table("Role") -> role;
table("Rule") -> rule;
table("CustomRole") -> customRole;
table("CustomRule") -> customRule;
table("LocalAuthorizationMethod") -> localAuthorizationMethod.

types(secM) -> ?secM_types;
types(authenticationOrder) -> ?authenticationOrder_types;
types(authorizationOrder) -> ?authorizationOrder_types;
types(userManagement) -> ?userManagement_types;
types(tls) -> ?tls_types;
types(ssh) -> ?ssh_types;
types(role) -> ?role_types;
types(rule) -> ?rule_types;
types(customRole) -> ?customRole_types;
types(customRule) -> ?customRule_types;
types(localAuthorizationMethod) -> ?localAuthorizationMethod_types.

values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

validate(_DN, User, _Tx) ->
    {ok,User}.
prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, _Tx) ->
    case get(tlsChanged) == yes of
        true->
            put(tlsChanged, no),
            cipher_configuration_notify("Tls",
                                        "TLS: Cipher configuration has changed");
        false->
            ok
    end,
    case get(sshAlgosChanged) == yes of
        true ->
            put(sshAlgosChanged, no),
            cipher_configuration_notify("Ssh",
                                        "SSH: Cipher configuration has changed");
        false ->
            ok
    end.

init_data(upgrade) ->
    swmI:copy_old_table(secM),
    swmI:copy_old_table(tls),
    swmI:copy_old_table(ssh),
    upg_tab(userManagement),
    swmI:copy_old_table(authenticationOrder),
    swmI:copy_old_table(authorizationOrder),
    swmI:copy_old_table(localAuthorizationMethod),
    swmI:copy_old_table(customRole), %HS94339
    swmI:copy_old_table(customRule), %HS94339
    
    %% HS94877 Roles and rules shall not be copied
    %% They should be regenerated with each upgrade

    create_roles(),
    %% Ericsson expert

    case sysEnv:rcs_mode_2() of
	Mode when Mode==target; Mode==vrcs ->
	    case sysInitI:is_secure() of
		false ->
		    add_expert_role();
		true ->
		    ok
	    end;
	simulated ->
	    add_expert_role()
    end,
    %% Recover userLabels
    [begin
	 case swmI:read(role, RoleKey) of
	     [OldRole] ->
		 [NewRole] = mnesia:dirty_read(role, RoleKey),
		 mnesia:dirty_write(
		   NewRole#role{userLabel = OldRole#role.userLabel});
	     [] ->
		 ok
	 end
     end||RoleKey<-mnesia:dirty_all_keys(role)],
    [begin
	 case swmI:read(role, RuleKey) of
	     [OldRule] ->
		 [NewRule] = mnesia:dirty_read(role, RuleKey),
		 mnesia:dirty_write(
		   NewRule#rule{userLabel = OldRule#rule.userLabel});
	     [] ->
		 ok
	 end
     end||RuleKey<-mnesia:dirty_all_keys(rule)],

    %%Update TLS enabled ciphers incase supported ciphers has changed
    case mnesia:dirty_read(tls, {"1","1","1","1"}) of
        [] ->
            store_initial_tls_ciphers();
        [TlsObj]->
            verify_tls_after_upgrade(TlsObj)
    end,

    %%Update SSH enabled ciphers incase supported ciphers has changed
    case mnesia:dirty_read(ssh, {"1","1","1","1"}) of
        [] ->
            store_initial_ssh_algos();
        [SshObj]->
            verify_selected_ssh_algos_after_upgrade(SshObj)
    end,
    ok;

init_data(fromScratch) ->

    mnesia:dirty_write(#secM{secMId={"1", "1","1"}}),

    mnesia:dirty_write(#userManagement{userManagementId={"1","1","1","1"},
                legalNotice=?userManagement_legalNotice_default, % HW44638
                loginFailureDelay=?userManagement_loginFailureDelay_default,
                targetType="",
                userLabel=""}),

    store_initial_tls_ciphers(),

    store_initial_ssh_algos(),

    Authentication =
	[#'MethodOrder'{orderNumber=1,
			methodReference="ManagedElement=1,SystemFunctions=1,"
			"SecM=1,UserManagement=1,LdapAuthenticationMethod=1",
			userLabel=""}
	],

    mnesia:dirty_write(
      #authenticationOrder{authenticationOrderId = {"1","1","1","1","1"},
			   authenticationMethodOrder = Authentication}),

    Authorization =
	[#'MethodOrder'{orderNumber=1,
			methodReference="ManagedElement=1,SystemFunctions=1,"
			"SecM=1,UserManagement=1,LocalAuthorizationMethod=1",
			userLabel=""}
	],
    mnesia:dirty_write(
      #authorizationOrder{authorizationOrderId = {"1","1","1","1","1"},
			  authorizationMethodOrder = Authorization}),

%%% Local authorization method

    mnesia:dirty_write(#localAuthorizationMethod{
			  localAuthorizationMethodId = {"1","1","1","1","1"},
			  %% administrativeState = ?BasicAdmState_LOCKED}),
			  administrativeState = ?BasicAdmState_UNLOCKED}),

    create_roles(),
    %% Ericsson expert

    case sysEnv:rcs_mode_2() of
	Mode when Mode==target ; Mode==vrcs ->
	    case sysInitI:is_secure() of
		false ->
		    add_expert_role();
		true ->
		    ok
	    end;
	simulated ->
	    add_expert_role()
    end.


upg_tab(userManagement) ->

    case swmI:is_attributes_in_old_record(userManagement, [legalNotice, loginFailureDelay]) of
	true ->
	    swmI:copy_old_table(userManagement);
	false ->
	    Added = [{legalNotice,?userManagement_legalNotice_default},
                     {loginFailureDelay,?userManagement_loginFailureDelay_default}],
	    [begin
		 Record = swmI:transform_obj(Obj, Added),
		 mnesia:dirty_write(Record)
	     end||Obj<-swmI:all_objects(userManagement)]
    end.


add_expert_role() ->
    %% Not VC enabled, add expert role

    mnesia:dirty_write(#role{roleId = {"1","1","1","1","1","expert"},
			     roleName = "expert",
			     userLabel = "Lab use only"}),

    mnesia:dirty_write(#rule{ruleId = {"1","1","1","1","1","expert","root"},
			     userLabel = "Access to the entire model",
			     permission = ?PermissionType_RWX,
			     ruleData = "ManagedElement,*"}).

%%% ----------------------------------------------------------
%%% Description: Return the target type
%%% ----------------------------------------------------------
get_target_type() ->
    {atomic, Types} =
	mnesia:transaction(
	  fun() ->
		  [Obj] = mnesia:read({userManagement, {"1","1","1","1"}}),
		  case Obj#userManagement.targetType of
		      undefined -> [];
		      T -> T
		  end
	  end),
    Types.

%%% ----------------------------------------------------------
%%% Description: Return all role identities
%%% ----------------------------------------------------------
get_all_security_roles() ->
    Fun = fun() -> [element(6, Key)||Key<-mnesia:all_keys(role)] end,
    {atomic, Roles} = mnesia:transaction(Fun),
    Roles.

%%% ----------------------------------------------------------
%%% Description: Return all Tls enabled ciphers
%%% ----------------------------------------------------------
get_tls_enabled_ciphers()->
    %%In case of TLS table or undefined not existing, return default cipher suites
    case mnesia:dirty_read(tls, {"1","1","1","1"}) of
        []->
            ssl:cipher_suites(erlang);
        [Obj]->
            case Obj#tls.enabledCiphers of
                undefined ->
                    ssl:cipher_suites(erlang);
                _->
                    get_tls_enabled_ciphers(Obj#tls.enabledCiphers, [])
            end
    end.
get_tls_enabled_ciphers([EnabledCipher | Rest], List)->
    Encryption = string:join(string:tokens(EnabledCipher#'Cipher'.encryption,
                                           "-"), "_"),
    case Comma = string:str(EnabledCipher#'Cipher'.mac, "-") of
        0 ->
            Cipher = {EnabledCipher#'Cipher'.keyExchange,
                      EnabledCipher#'Cipher'.authentication,
                      Encryption,
                      EnabledCipher#'Cipher'.mac};
        _ ->
            Mac = string:substr(EnabledCipher#'Cipher'.mac, 1, Comma-1),
            Prf = string:substr(EnabledCipher#'Cipher'.mac, Comma+1),
            Cipher = {EnabledCipher#'Cipher'.keyExchange,
                      EnabledCipher#'Cipher'.authentication,
                      Encryption,
                      Mac,
                      Prf}
    end,
    get_tls_enabled_ciphers(Rest, List ++ [Cipher]);
get_tls_enabled_ciphers([], List) ->
    OtpCipherFormat =
        comsaTlsCipherConfig:convert_ciphers_to_otp_ssl_format(List),
    OtpCipherFormat.

%%% ----------------------------------------------------------
%%% Description: Return the preferred ssh algorithms as configured
%%% Note: Modified algorithms with "prepend" should be removed 
%%%       before returned
%%% ----------------------------------------------------------
get_ssh_preferred_algorithms() ->
    SupportedCiphers = comsaSshConfig:get_supported_algorithms_by_type(cipher),
    PrependKexs = ["diffie-hellman-group1-sha1"],
    SupportedKex = comsaSshConfig:get_supported_algorithms_by_type(kex) -- PrependKexs,
    SupportedMacs = comsaSshConfig:get_supported_algorithms_by_type(mac),
    case mnesia:dirty_read(ssh, {"1","1","1","1"}) of
        [] ->
            %% In case of SSH table not existing or undefined, return default cipher algorithms
            comsaSshConfig:get_ssh_preferred_algorithms(SupportedCiphers,
							SupportedKex,
							SupportedMacs);
        [Obj] ->
            case Obj#ssh.selectedCiphers of
                undefined ->
                    comsaSshConfig:get_ssh_preferred_algorithms(SupportedCiphers,
								SupportedKex,
								SupportedMacs);
                _ ->
                    SelectedCiphers = Obj#ssh.selectedCiphers,
                    SelectedKex = Obj#ssh.selectedKeyExchanges,
                    SelectedMacs = Obj#ssh.selectedMacs,
                    comsaSshConfig:get_ssh_preferred_algorithms(SelectedCiphers,
								SelectedKex,
								SelectedMacs)
            end
    end.

%%% ----------------------------------------------------------
%%% Description: Return the modified algorithms
%%% See OTP ssh documentation for more information
%%% ----------------------------------------------------------
get_ssh_modify_algorithms() ->
    PrependKexs = ["diffie-hellman-group1-sha1"],
    case mnesia:dirty_read(ssh, {"1","1","1","1"}) of
        [] ->
	    get_prepend_kexs(PrependKexs);
        [Obj] ->
            case Obj#ssh.selectedKeyExchanges of
                undefined ->
		    get_prepend_kexs(PrependKexs);
                SelectedKexs ->
		    get_prepend_kexs(PrependKexs, SelectedKexs, _Acc = [])
            end
    end.

get_prepend_kexs([Kex | T], SelectedKexs, Acc) ->
    case lists:member(Kex, SelectedKexs) of
	true ->
	    get_prepend_kexs(T, SelectedKexs, [Kex | Acc]);
	false ->
	    get_prepend_kexs(T, SelectedKexs, Acc)
    end;
get_prepend_kexs([], _, Acc) ->
    get_prepend_kexs(Acc).

get_prepend_kexs([]) ->
    {modify_algorithms,  []};
get_prepend_kexs(PrependKexs) ->
    PKexs = [list_to_atom(Kex) || Kex <- PrependKexs],
    {modify_algorithms,  [{prepend, [{kex, PKexs}]}]}.

%%% ----------------------------------------------------------
%%% Description: Initialize the Tls attributes
%%% ----------------------------------------------------------
store_initial_tls_ciphers()->
    put(tlsChanged, no),
    SupportedCiphers = get_decoded_cipher_suites(ssl:cipher_suites(erlang)),
    mnesia:dirty_write(#tls{
                            tlsId            = {"1","1","1","1"},
                            enabledCiphers   = SupportedCiphers,
                            cipherFilter = ?tls_cipherFilter_default,
                            supportedCiphers = SupportedCiphers}),
    ok.

%%% ----------------------------------------------------------
%%% Description: Initialize the ssh algorithm attributes
%%% ----------------------------------------------------------
store_initial_ssh_algos() ->
    put(sshAlgosChanged, no),
    SupportedCiphers = comsaSshConfig:get_supported_algorithms_by_type(cipher),
    SupportedKeyExchanges = comsaSshConfig:get_supported_algorithms_by_type(kex),
    SupportedMacs = comsaSshConfig:get_supported_algorithms_by_type(mac),
    mnesia:dirty_write(#ssh{
           sshId            = {"1","1","1","1"},
           selectedCiphers   = SupportedCiphers,
           selectedKeyExchanges = SupportedKeyExchanges,
           selectedMacs = SupportedMacs,
           supportedCiphers = SupportedCiphers,
           supportedKeyExchanges = SupportedKeyExchanges,
           supportedMacs = SupportedMacs}),
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Description: Get Tls cipher struct format from OTP SSL cipher format
%%% ----------------------------------------------------------
get_decoded_cipher_suites(Cipher_suite) ->
    get_decoded_cipher_suites(Cipher_suite, []).
get_decoded_cipher_suites([Cipher_suite | Rest ], CipherList) ->
    Cipher = comsaTlsCipherConfig:convert_ciphers_from_otp_ssl_format(Cipher_suite),
    get_decoded_cipher_suites(Rest, CipherList ++ [Cipher]);
get_decoded_cipher_suites([], CipherList) -> CipherList.

%%% ----------------------------------------------------------
%%% Description: Get Tls supported ciphers from db
%%% ----------------------------------------------------------
get_tls_supported_ciphers() ->
    [Obj] = mnesia:dirty_read(tls, {"1","1","1","1"}),
    SupportedCiphers = Obj#tls.supportedCiphers,
    SupportedCiphers.

%%% ----------------------------------------------------------
%%% Description: Get Tls cipher filter from db
%%% ----------------------------------------------------------
get_tls_cipher_filter() ->
    [Obj] = mnesia:dirty_read(tls, {"1","1","1","1"}),
    Obj#tls.cipherFilter.

%%% ----------------------------------------------------------
%%% Description: Write Tls enbled ciphers to db
%%% ----------------------------------------------------------
set_tls_enabled_ciphers(Value, Obj) ->
    put(tlsChanged, no),
    case EnabledCiphers =
        comsaTlsCipherConfig:generate_enabled_ciphers(Value,
                                                get_tls_supported_ciphers()) of
        {error, Reason} ->
            mnesia:abort(Reason++ " Value= '" ++Value++"'");
        _ ->

            case ok of
                _ when Value =:= Obj#tls.cipherFilter,
                       EnabledCiphers =:= Obj#tls.enabledCiphers ->
                true;
                _ ->
                    put(tlsChanged, yes)
            end,
            mnesia:dirty_write(tls, Obj#tls{enabledCiphers = EnabledCiphers})
    end.

%%% ----------------------------------------------------------
%%% Description: Verify that Tls enabled ciphers is operable after upgrade
%%% ----------------------------------------------------------
verify_tls_after_upgrade(Obj)->
    put(tlsChanged, no),
    SupportedCiphers = get_decoded_cipher_suites(ssl:cipher_suites(erlang)),
    case SupportedCiphers =:= Obj#tls.supportedCiphers of
        true->
            ok;
        false->
            verify_tls_enabled_after_upgrade(Obj, SupportedCiphers)
    end.

verify_tls_enabled_after_upgrade(Obj, SupportedCiphers)->
    Filter = Obj#tls.cipherFilter,
    EnabledCiphers =
             comsaTlsCipherConfig:generate_enabled_ciphers(Filter,
                                                           SupportedCiphers),
    Notify = case EnabledCiphers of
        {error, _} ->
            mnesia:dirty_write(Obj#tls{supportedCiphers = SupportedCiphers,
                                       cipherFilter = ?tls_cipherFilter_default,
                                       enabledCiphers = SupportedCiphers}),
             {notify,
    "TLS: Upgrade has changed cipher configuration, cipherFilter reverted to DEFAULT"};
        _ ->

            case Obj#tls.enabledCiphers =:= EnabledCiphers of
                true->
                    mnesia:dirty_write(Obj#tls{supportedCiphers = SupportedCiphers});
                false->
                    mnesia:dirty_write(Obj#tls{supportedCiphers = SupportedCiphers,
                                       enabledCiphers = EnabledCiphers}),
                     {notify, "TLS: Upgrade has changed cipher configuration"}
            end
    end,

      %Notify servers of cipher configuration changes
    case Notify of
        {notify, Msg}->
             cipher_configuration_notify("Tls", Msg);
        _->
            ok
    end.

%%%-----------------------------------------------------------
%%% Description: Verify that the selected ssh algos are operable
%%%              The selected algos from mnesia that are still supported
%%%              are written as 'new' selected. If the selected from mnesia
%%%              are no longer supported, the supported will be set as selected
%%% ----------------------------------------------------------
verify_selected_ssh_algos_after_upgrade(Obj) ->
    put(sshAlgosChanged, no),
    SupportedCiphers = comsaSshConfig:get_supported_algorithms_by_type(cipher),
    SupportedKeyExchanges = comsaSshConfig:get_supported_algorithms_by_type(kex),
    SupportedMacs = comsaSshConfig:get_supported_algorithms_by_type(mac),
    
    mnesia:dirty_write(Obj#ssh{
                            supportedCiphers = SupportedCiphers,
                            supportedKeyExchanges = SupportedKeyExchanges,
                            supportedMacs = SupportedMacs}),

    NewObj = Obj#ssh{supportedCiphers = SupportedCiphers,
                     supportedKeyExchanges = SupportedKeyExchanges,
                     supportedMacs = SupportedMacs},
    
    SshCiphers = handle_ssh_ciphers_after_upgrade(NewObj),
    SshKex = handle_ssh_kex_after_upgrade(NewObj, Obj),
    SshMacs = handle_ssh_macs_after_upgrade(NewObj),

    mnesia:dirty_write(NewObj#ssh{selectedCiphers=SshCiphers,
				  selectedKeyExchanges=SshKex,
				  selectedMacs=SshMacs}).

handle_ssh_ciphers_after_upgrade(Obj) ->
    SupportedCiphers = Obj#ssh.supportedCiphers,
    WantedCiphers = Obj#ssh.selectedCiphers,

    Ciphers = verify_ssh_algos(SupportedCiphers, WantedCiphers),
    Ciphers.

handle_ssh_kex_after_upgrade(Obj, OldObj) ->
    %% New (inside Obj) kexs should be inserted in selectedKeyExchange.
    NewKexs = get_new_kexs(Obj#ssh.supportedKeyExchanges, OldObj#ssh.supportedKeyExchanges),

    SupportedKeyExchanges = Obj#ssh.supportedKeyExchanges,
    KeyExchanges = lists:append(Obj#ssh.selectedKeyExchanges, NewKexs),

    WantedKeyExchanges = remove_dups(KeyExchanges),

    Kex = verify_ssh_algos(SupportedKeyExchanges, WantedKeyExchanges),
    Kex.

get_new_kexs(SupportKexs, OldSupportKexs) ->
    ordsets:subtract(ordsets:from_list(SupportKexs), ordsets:from_list(OldSupportKexs)).

remove_dups([]) -> 
    [];
remove_dups([H|T]) ->
    [H | [X || X <- remove_dups(T), X /= H]].

handle_ssh_macs_after_upgrade(Obj) ->
    SupportedMacs = Obj#ssh.supportedMacs,
    WantedMacs = Obj#ssh.selectedMacs,

    Macs = verify_ssh_algos(SupportedMacs, WantedMacs),
    Macs.

verify_ssh_algos(Supported, Wanted) ->
    case Selected =
             comsaSshConfig:extract_selected_algorithms(Supported, Wanted) of
        [] ->
            Supported;
        _ ->
            case Selected =:= Wanted of
                true ->
                    Wanted;
                false ->
                    cipher_configuration_notify("Ssh",
                    "SSH: Upgrade has changed the selected attributes"),
                    Selected
            end
    end.

create_roles() ->
    %%% Local Roles & Rules
    PrivDir = code:priv_dir(comsa),
    MibsPattern = filename:join([PrivDir, "mib", "*.xml"]),
    Mibs = filelib:wildcard(MibsPattern),
    IncPattern =
	filename:join(
	  [sysEnv:com_top(), "opt", "com", "etc", "model"]),
    IncDirs = filelib:wildcard(IncPattern),

    create_roles(IncDirs, Mibs).

create_roles(_, []) ->
    ok;
create_roles(IncDirs, [Mib|Mibs]) ->
    PatchesDir = sysEnv:dev_patches_dir(),
    Name = filename:basename(Mib),
    PatchPath = filename:join(PatchesDir, Name),
    Path =
	case filelib:is_file(PatchPath) of
	    true -> PatchPath;
	    false -> Mib
	end,

    Options = [{validation, dtd}, {fetch_path, [PatchesDir|IncDirs]}],

    {ModelsE, _ } =
	case xmerl_scan:file(Path, Options) of
	    {error, Reason} -> erlang:error(Reason, [IncDirs, [Mib|Mibs]]);
	    Result -> Result
	end,
    [[scan_mib_object(Object)||Object<-MibE#xmlElement.content,
			       Object#xmlElement.name == object]
     ||MibE<-ModelsE#xmlElement.content,
       MibE#xmlElement.name == mib],
    create_roles(IncDirs, Mibs).

scan_mib_object(ObjectE) ->
    HasClassE = find_element(hasClass, ObjectE),
    Class = find_attribute(name, HasClassE),
    case Class of
	"Role" ->
	    scan_role_object(ObjectE);
	"Rule" ->
	    scan_rule_object(ObjectE)
    end.

scan_role_object(ObjectE) ->
    KeyValues = scan_slots(ObjectE),
    Role = build_record(role, {"1","1","1","1","1",undefined},
			KeyValues, ?role_types),
    mnesia:dirty_write(Role).


scan_rule_object(ObjectE) ->
    "Role="++RoleId = find_attribute(parentDn, ObjectE),
    KeyValues = scan_slots(ObjectE),
    Role = build_record(rule, {"1","1","1","1","1",RoleId,undefined},
			KeyValues, ?rule_types),
    mnesia:dirty_write(Role).


scan_slots(ObjectE) ->
    [scan_slot(SlotE)||SlotE<-ObjectE#xmlElement.content,
		       SlotE#xmlElement.name == slot].

scan_slot(SlotE) ->
    NameStr = find_attribute(name, SlotE),
    ValueE = find_element(value, SlotE),
    Value = find_text(ValueE),
    Name = list_to_atom(NameStr),
    {Name, Value}.


build_record(Class, KeyPrototype, KeyValues, Types) ->
    Attributes =
	[begin
	     Basic = proplists:get_value(Name, KeyValues, undefined),
	     case Basic of
		 undefined -> undefined;
		 Basic when Name == ruleId ; Name == roleId->
		     Size = size(KeyPrototype),
		     setelement(Size, KeyPrototype, Basic);
		 Basic ->
		     case Type of
			 string ->
			     Basic;
			 'RcsLocalAuthorization.PermissionType' ->
			     list_to_integer(Basic);
			 'RcsLocalAuthorization.RuleDataType'->
			     Basic
		     end
	     end
	 end||{Name, Type}<-Types],
    list_to_tuple([Class|Attributes]).

%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions:
%%% Description: Finds a sub element to an xml element, or in a list
%%%              of element contents. Assumes there is only one element
%%%              with the same name
%%% ----------------------------------------------------------
find_element(ElementName, Element) when is_record(Element, xmlElement) ->
    find_element(ElementName, Element#xmlElement.content);
find_element(ElementName, ContentList) ->
    {value, Element} =
	lists:keysearch(ElementName, #xmlElement.name, ContentList),
    Element.

%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions:
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ----------------------------------------------------------

-spec find_attribute(atom(), #xmlElement{} | [#xmlAttribute{}]) ->
	  string().

find_attribute(AttributeName, Element) when is_record(Element, xmlElement) ->
    find_attribute(AttributeName, Element#xmlElement.attributes);
find_attribute(AttributeName, AttributeList) ->
    case lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList) of
	{value, Attribute} ->
	    Attribute#xmlAttribute.value;
	false ->
	    erlang:error({badmatch, false}, [AttributeName, AttributeList])
    end.


%%% ----------------------------------------------------------

find_text(Element) when is_record(Element, xmlElement) ->
    [Text] = Element#xmlElement.content,
    Text#xmlText.value.

sec_log(Msg) ->
    List = comsaI:get_managed_element_data(),
    MeId =
    case lists:keyfind(networkManagedElementId, 1, List) of
        {networkManagedElementId, undefined} ->
            "1";
        {networkManagedElementId, ME} ->
            ME
    end,
    logI:write_log("SecurityLog", MeId, 4, info, os:timestamp(), Msg).

%%% ----------------------------------------------------------
%%% Description: Notify Tls and Ssh servers of cipher configuration change
%%% ----------------------------------------------------------
cipher_configuration_notify(Class, Msg)->
    try
        sysInitI:info_msg("~w: "++Msg++"~n", [?MODULE]),
        sec_log(Msg),
        case Class of
            "Tls"->
                %Wait 1 sec before notify servers to give time to
                %reply client
                timer:apply_after(1000, omc_api, tls_cipher_notify, []),
                timer:apply_after(1000, comSnmpDtlsConfig, dtls_cipher_filter_notify, []),
                timer:apply_after(1000, ftpesI, ftpes_cipher_notify, []);
            "Ssh"->
                %Wait 1 sec before notify servers to give time to
                %reply client
                timer:apply_after(1000, omc_api, ssh_config_notify, []);
            _->
                ok
        end
    catch _:_ ->
              sysInitI:info_msg("~p: Could not notify servers of cipher configuration change for: ~p~n",
              [?MODULE, Class]),
              ok
    end.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

reload_roles() ->
    [mnesia:dirty_delete(role, Role)||Role<-mnesia:dirty_all_keys(role)],
    [mnesia:dirty_delete(rule, Role)||Role<-mnesia:dirty_all_keys(rule)],
    create_roles().
