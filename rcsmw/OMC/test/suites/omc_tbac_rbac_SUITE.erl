%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_tbac_rbac_SUITE.erl %
%%% @author etxjovp
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/3
%%% 
%%% @doc == Test Suite for testing different LDAP profiles, e.g. the profileFilter attribute.
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% Despite the name, this test suite doesn't actually test any TBAC.
%%% <br/><br/>
%%% rct_netconf is used in ct_hooks:
%%% see <a href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/supp/esrc/rct_netconf.erl">rct_netconf.erl</a><br/>
%%% @end


-module(omc_tbac_rbac_SUITE).
%%% Except as specifically authorized in writing by Ericsson, the 
%%% receiver of this document shall keep the information contained 
%%% herein confidential and shall protect the same in whole or in 
%%% part from disclosure and dissemination to third parties.
%%% 
%%% Disclosure and disseminations to the receivers employees shall 
%%% only be made on a strict need to know basis.
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date        Name        What
%%% -----      ----------  --------    -----------------------
%%% R2A/1      2013-10-30  etxasta     Created
%%% Rx         2014-03-24  etxlg       New ldap, moved from RCT_TOP
%%% R2A/2      2014-03-25  etxlg       Fix initial state
%%% R2A/2      2015-03-10  etxlg       Fetch ldap-ip from config
%%% R3A/3      2015-07-10  etxjovp     Add group definitions used by CS CI
%%% ----------------------------------------------------------
%%% 

%%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 profile_posix/1,
         profile_ericsson/1,
         profile_flexible/1,
	 groups/0]).

%these are defaults for use with our LDAP @imf01
%%%-define(LDAP_SERVER, "10.68.200.11").
-define(BASE_DN, "ou=people,dc=mordor,dc=invalid").
-define(BIND_DN, "cn=king,dc=mordor,dc=invalid").
-define(BIND_PW, "1:7TcvZCTcqkKUI6RNL3IKSlMB/kas"). %corresponds to "hello"

%%%% Preconditions: LDAP configuration %%%%

% search expression to read the ldap database -useful when fixing this tc
%%% ldapsearch -LLL -v -x -w hello -H ldap://10.68.200.11
%%%	-D "cn=king,dc=mordor,dc=invalid" -b "dc=mordor,dc=invalid"
%%%	'(&(uid=ldap*)(objectClass=posixAccount))'
%%%ldap_initialize( ldap://10.68.200.11:389/??base )
%%%filter: (&(uid=ldap*)(objectClass=posixAccount))
%%%requesting: All userApplication attributes
%%%
%This use is used for testing POSIX_GROUPS profileFilter. The roles are
%looked up in posixGroup objects with "memberUid" containing this user. The
%matching group can be seen last in these "preconditions".
%%%dn: uid=ldap_test_user_1,ou=people,dc=mordor,dc=invalid
%%%objectClass: inetOrgPerson
%%%objectClass: posixAccount
%%%uid: ldap_test_user_1
%%%uidNumber: 10001
%%%gidNumber: 1001
%%%homeDirectory: /bin/false
%%%cn: Test user for omc_tbac_rbac_SUITE
%%%sn: posix_filter
%%%userPassword:: bGRhcF90ZXN0X3VzZXJfMQ==
%%%
%This user is used for ERICSSON_FILTER profileFilter. TBAC is not on and
%targetType not set, causing the single unqualified role in the attribute
%"ericssonUserAuthorizationScope" to be returned.
%%%dn: uid=ldap_test_user_2,ou=people,dc=mordor,dc=invalid
%%%objectClass: inetOrgPerson
%%%objectClass: posixAccount
%%%objectClass: ericssonUserAuthorization
%%%objectClass: ericssonUserAuthentication
%%%uid: ldap_test_user_2
%%%uidNumber: 10002
%%%gidNumber: 1002
%%%homeDirectory: /bin/false
%%%cn: Test user for omc_tbac_rbac_SUITE
%%%sn: ericsson_filter
%%%userPassword:: bGRhcF90ZXN0X3VzZXJfMg==
%%%ericssonUserAuthorizationScope: expert
%%%
%This user is used for testing FLEXIBLE profileFilter. A filter is set
%to find this user and return the "title" attribute.
%%%dn: uid=ldap_test_user_3,ou=people,dc=mordor,dc=invalid
%%%objectClass: inetOrgPerson
%%%objectClass: posixAccount
%%%uid: ldap_test_user_3
%%%uidNumber: 10003
%%%gidNumber: 1003
%%%homeDirectory: /bin/false
%%%cn: Test user for omc_tbac_rbac_SUITE
%%%title: expert
%%%title: coli
%%%sn: flexible_filter
%%%userPassword:: bGRhcF90ZXN0X3VzZXJfMw==
%%%

%this group is the one that will match when we authenticate using POSIX_GROUPS
%note memberUid : ldap_test_user_1
%%% ldapsearch -LLL -v -x -w hello -H ldap://10.68.200.11 -D
%%%	"cn=king,dc=mordor,dc=invalid" -b "dc=mordor,dc=invalid"
%%%	'(&(objectClass=posixGroup))'
%%%ldap_initialize( ldap://10.68.200.11:389/??base )
%%%filter: (&(objectClass=posixGroup))
%%%requesting: All userApplication attributes
%%%dn: cn=expert,ou=people,dc=mordor,dc=invalid
%%%objectClass: posixGroup
%%%gidNumber: 10
%%%cn: expert
%%%memberUid: expert
%%%memberUid: etxlg
%%%memberUid: ldap_test_user_1



%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [%% {rct_htmllink,[]},
		 {rct_rpc, rpc},
		 {rct_netconf,{nc1, man_auth}},
                 {cth_conn_log,[]},
                 {rct_logging, {all,
			[{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}}
]}].


%% @hidden
init_per_suite(Config) ->
    crypto:start(),
    ssh:start(),
    restore_default_ldap_config(),
    Config.
%% @hidden
end_per_suite(_Config) ->
    restore_default_ldap_config(),
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.
%% @hidden
groups() ->
    AllGroup = all(),
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
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [profile_posix, profile_ericsson, profile_flexible].

%%--------------------------------------------------------------------
%% @doc
%% Using netconf to configure LDAP profile.
%%
%% @spec profile_posix(Config) -> ok
%% @end
%%--------------------------------------------------------------------
profile_posix(_Config) ->
    %% Test to log in using test user "ldap_test_user_1"
    %% filter for posix groups is the default so there is no need to change
    ct:pal("Use LDAP profile POSIX_GROUPS (the default)", []),
    case ct_netconfc:open(nc1,[{user, "ldap_test_user_1"},
				{password, "ldap_test_user_1"}]) of
        {ok,_} ->
            ok = ct_netconfc:edit_config(nc1, running,
					set_user_label("ldap tc posix_groups")),
            ct_netconfc:close_session(nc1),
	    ct:pal("Test passed!!!", []);
         {error, Reason} ->
             ct:pal("error, reason: ~p", [Reason]),
             ct:fail("Test case failed!!!")
     end,
     ct:pal("Test case complete", []).


%%--------------------------------------------------------------------
%% @doc
%% Using netconf to configure LDAP profile.
%%
%% @spec profile_ericsson(Config) -> ok
%% @end
%%--------------------------------------------------------------------
profile_ericsson(_Config) ->
    ct:pal("Use LDAP profile ERICSSON_FILTER", []),
    case ct_netconfc:open(nc1,[{user, "expert"}, {password, "expert"}]) of
        {ok,_} ->
	    %% configure LDAP ERICSSON_ROLES profile using netconf
            ok = ct_netconfc:edit_config(nc1, running, ericsson_ldap_config()),
            ct_netconfc:close_session(nc1),
            %% Negative test with no valid role
            {error,_} = ct_netconfc:open(nc1,[{user, "ldap_test_user_1"},
					{password, "ldap_test_user_1"}]),
            {error,_} = ct_netconfc:open(nc1,[{user, "ldap_test_user_3"},
					{password, "ldap_test_user_3"}]),
            case ct_netconfc:open(nc1,[{user, "ldap_test_user_2"},
					{password, "ldap_test_user_2"}]) of
                {ok,_} ->
                    ok = ct_netconfc:edit_config(nc1, running,
				default_ldap_config()),
                    ct_netconfc:close_session(nc1),
                    ct:pal("Test passed!!!", []);
                {error, Reason} ->
		    restore_default_ldap_config(),
                    ct:pal("error, reason: ~p", [Reason]),
                    ct:fail("Test case failed!!!")
            end;
         {error, Reason} ->
             ct:pal("error, reason: ~p", [Reason]),
             ct:fail("Test case failed!!!")
     end,
    ct:pal("Test case complete",[]).


%%--------------------------------------------------------------------
%% @doc
%% Using netconf to configure LDAP profile.
%%
%% @spec profile_flexible(Config) -> ok
%% @end
%%--------------------------------------------------------------------
profile_flexible(_Config) ->
    ct:pal("Use LDAP profile FLEXIBLE", []),
    case ct_netconfc:open(nc1,[{user, "expert"}, {password, "expert"}]) of
        {ok,_} ->
    %% Test to log in using test user "test_flexible"
            ok = ct_netconfc:edit_config(nc1, running,
					flex_filter_ldap_config()),
            ct_netconfc:close_session(nc1),
            case ct_netconfc:open(nc1,[{user, "ldap_test_user_3"},
				{password, "ldap_test_user_3"}]) of
                {ok,_} ->
                    ok = ct_netconfc:edit_config(nc1, running, 
					default_ldap_config()),
                    ct_netconfc:close_session(nc1),
                    ct:pal("Test passed!!!", []);
                {error, Reason} ->
		    restore_default_ldap_config(),
                    ct:pal("error, reason: ~p", [Reason]),
                    ct:fail("Test case failed!!!")
            end;
         {error, Reason} ->
             ct:pal("error, reason: ~p", [Reason]),
             ct:fail("Test case failed!!!")
     end,
    
     ct:pal("Test case complete",[]).

restore_default_ldap_config() ->
    ct:pal("Restoring LDAP config to default", []),
    {ok, _} = ct_netconfc:open(nc1,[{user, "expert"}, {password, "expert"}]),
    ok = ct_netconfc:edit_config(nc1, running, default_ldap_config()),
    ct_netconfc:close_session(nc1).

ericsson_ldap_config() ->
    ldap_config("ERICSSON_FILTER", undefined, {undefined, "LOCKED"},
		{undefined, undefined}).
flex_filter_ldap_config() ->
    %when using the CLI we get the \ included, with netconf NOT
    %do it like this to force it since internally we look for \"\"
    ldap_config("FLEXIBLE", undefined, {undefined, "LOCKED"},
		{"(&(uid=\\\"\\\")(objectClass=posixAccount))", "title"}).

default_ldap_config() ->
% try to reset most every LDAP related attribute to it's "default",
% this includes stuff that may not have been touched in this testcase
% also UserManagement=1, targetType = []
% but not any of the deprecated attributes
    ldap_config("POSIX_GROUPS", undefined, {undefined, "LOCKED"},
		{undefined, undefined}).

ldap_config(Profile_filter, Target_type, {Role_alias_dn, Tbac},
	    {Flex_filter, Flex_filter_type}) ->
    Ldap_server = ?config(host, ct:get_config(ldap_server)),
    {'ManagedElement',          
     [{'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],

     [{managedElementId,[],["1"]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
        {'SecM',
         [],
         [{secMId,[],["1"]},
          {'UserManagement',[],
           [{userManagementId,[],["1"]},
	    case Target_type of
		undefined ->
		    {targetType, [{'xc:operation', "delete"}],[]};
		Target_type ->
		    {targetType, [],[Target_type]}
	    end,
            {'LdapAuthenticationMethod',
             [],
             [{ldapAuthenticationMethodId,[],["1"]},
              {administrativeState,[],["UNLOCKED"]},
              {'Ldap',[],
               [{ldapId,[],["1"]},
                {ldapIpAddress,[],[Ldap_server]},
                {fallbackLdapIpAddress,[{'xc:operation', "delete"}],[]},
                {nodeCredential,[{'xc:operation', "delete"}],[]},
                {serverPort,[{'xc:operation', "delete"}],[]},
                {trustCategory,[{'xc:operation', "delete"}],[]},
                {tlsMode,[],["STARTTLS"]},
                {useTls,[],["false"]},
                {useReferrals,[],["false"]},
                {bindDn,[],[?BIND_DN]},
                {bindPassword,[{struct,"EcimPassword"}],
		 [{password,[],[?BIND_PW]}]},
		{baseDn, [], [?BASE_DN]},
                {profileFilter,[],[Profile_filter]},
		{userLabel, [], ["Default (created by omc_tbac_rbac_SUITE)"]},
		{'EricssonFilter',[],
		 [{ericssonFilterId,[],["1"]},
		  case Role_alias_dn of
		      undefined ->
			  {roleAliasesBaseDn,
			   [{'xc:operation', "delete"}],[]};
		      Role_alias_dn ->
			  {roleAliasesBaseDn,[],[Role_alias_dn]}
		  end,
		  {targetBasedAccessControl,[],[Tbac]}]},
		{'Filter',[],
		 [{filterId,[],["1"]},
		  case Flex_filter of
		      undefined ->
			  {filter,[{'xc:operation', "delete"}],[]};
		      Flex_filter ->
			  {filter,[],[Flex_filter]}
		  end,
		  case Flex_filter_type of
		      undefined ->
			  {type,[{'xc:operation', "delete"}],[]};
		      Flex_filter_type ->
			  {type,[],[Flex_filter_type]}
		  end,
		  {userLabel,[{'xc:operation', "delete"}],[]}]}
	       ]}]}]}]}]}]}.
                
set_user_label(Label) ->
% make a harmless change to confirm we have access
    {'ManagedElement',          
     [{'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],

     [{managedElementId,[],["1"]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
        {'SecM',
         [],
         [{secMId,[],["1"]},
          {'UserManagement',[],
           [{userManagementId,[],["1"]},
            {'LdapAuthenticationMethod',
             [],
             [{ldapAuthenticationMethodId,[],["1"]},
              {'Ldap',[],
               [{ldapId,[],["1"]},
		{userLabel, [], [Label]}
            ]}]}]}]}]}]}.
