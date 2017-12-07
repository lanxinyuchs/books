%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_datainit.erl %
%%% Author:	etxlg
%%% Description: Data initialization functions used at system installation and
%%%              software upgrade.
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(omc_datainit).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/1').
-date('2017-02-09').
-author('etomist').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	xxxDataInit.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R2A/1      2014-01-17   etxlg     Created from template
%%% R2A/2      2014-01-30   etxlg     Removed forgotten printouts
%%% R2A/3      2014-02-28   etxlg     ldap-genserver process
%%% R2A/4      2014-03-10   etxlg     Ram table for alarms
%%% R2A/5      2014-06-26   etxlg     activate removed (not used)
%%% R3A/1      2014-11-11   etxtory   generate_host_keys in post_int
%%% R3A/2      2015-01-19   etxjotj   RcsLdapAuthentication support
%%% R3A/4      2015-01-28   etxarnu   Fixed transform(ldap)
%%% R3A/5      2015-01-29   etxjotj   Support for RcsHttpM
%%% R3A/6      2015-01-29   etxjotj   Register callback for httpm
%%% R3A/7      2015-02-17   etxjotj   HT48743 Default values for LDAP MO
%%% R3A/9      2015-03-23   etxlg     Ensure EricssonFilterVersion=2
%%% R4A/2      2015-10-19   etxlg     run omc_login_bucket
%% ----    ---------- -------  ------------------------------------------------
%%% R4A/1   2015-07-07 etxberb  Changed mnesia:create_table to
%%                             clhI:mnesia_create_table.
%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% ----------------------------------------------------------
%%% R6A/1      2016-06-28   ehsake    ECIM LdapAuthentication 3.0
%%% R7A/1      2016-10-18   ehsake    WP6081, Generate ESI at rollback
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([instPhParallel_init/1,
	 instPhSeqBeg_init_data/0,
	 instPhParallel_init_data/0,
	 instPhParallel_post_init/0,
	 instPhSeqEnd_post_init/0,
     add_clh_option/1]).

-export([children/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsLdapAuthentication.hrl").
-include("RcsHttpM.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init(DbNodes) ->
    {atomic, ok} =
	clhI:mnesia_create_table(omc_alarms,
				 [{type, set},
				  {attributes, [key, value]},
				  {ram_copies, DbNodes} |
				  add_clh_option(omc_alarms)]),
    LdapA = [{filter, ?filter_types},
	     {ericssonFilter, ?ericssonFilter_types},
	     {ldap, ?ldap_types},
	     {ldapAuthenticationMethod, ?ldapAuthenticationMethod_types}],
    HttpM = [{httpM, ?httpM_types},
	     {https, ?https_types}],
    [[create_table(Name, DbNodes, Types)||{Name, Types}<-Tables]||
	Tables<-[LdapA, HttpM]],
    omc_lib:init(DbNodes),
    ok.

create_table(Name, DbNodes, Types) ->
    Fields = [Field||{Field, _}<-Types],
    {atomic, ok} =
	clhI:mnesia_create_table(Name, [{type, set},
					{disc_copies, DbNodes},
					{attributes, Fields} |
					add_clh_option(Name)]).

%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].

%%% ----------------------------------------------------------
%%% -type init_data()->                                     %#
%%%     ok                                                  %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Initiate Mnesia data tables for XXX.
%%% ----------------------------------------------------------
%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_data()->
    case swmI:is_upgrade_ongoing() of
	false -> 
	    %% Initialization of system created object
	    mnesia:dirty_write(
	      #ldapAuthenticationMethod{
		 ldapAuthenticationMethodId = {"1","1","1","1","1"},
		 administrativeState = ?BasicAdmState_UNLOCKED}),
	    
	    %% It is expected that LDAP parameters are given in 
	    %% autointegration since there is no local authentication
	    %% HT48743 fix
	    mnesia:dirty_write(
	      #ldap{ldapId = {"1","1","1","1","1","1"},
		    baseDn = "o=local", %% Ldap 3.0 requires 3 chars
		    ldapIpAddress = "ldap.local",
		    tlsMode = ?TlsMode_STARTTLS,
		    useReferrals = ?ldap_useReferrals_default,
		    useTls = false
		   }),
	    %% Fix ends here
	    mnesia:dirty_write(
	      #filter{filterId = {"1","1","1","1","1","1","1"}}), 

	    mnesia:dirty_write(
	      #ericssonFilter{
		 ericssonFilterId = {"1","1","1","1","1","1","1"},
		 targetBasedAccessControl = ?BasicAdmState_LOCKED,
		 version = ?EricssonFilterVersion_2}),

	    mnesia:dirty_write(
	      #httpM{httpMId = {"1","1","1","1"}}),
	    mnesia:dirty_write(
	      #https{httpsId = {"1","1","1","1","1"}}),

	    ok;
	true -> 
	    swmI:copy_old_table(ldapAuthenticationMethod),
	    transform(ldap),
	    swmI:copy_old_table(filter),
	    transform(ericssonFilter),
	    
	    swmI:copy_old_table(httpM),
	    swmI:copy_old_table(https)
    end,

    ok.

transform(ldap) ->
    [OldLdap] =  swmI:all_objects(ldap),
    NewLdap = case OldLdap of
                  
	{ldap, LdapId, LdapIpAddress, FallbackLdapIpAddress, ServerPort,
	 BindDn, BindPassword, BaseDn, ProfileFilter, NodeCredential,
	 TrustCategory, _TlsCaCertificate, _TlsClientCertificate, _TlsClientKey,
	 UseTls, _UseTlsFallback, TlsMode, UserLabel, _NodeType, _FilterType,
	 _RoleAliasesBaseDn} ->

	    #ldap{ldapId = LdapId,
			 ldapIpAddress = LdapIpAddress,
			 fallbackLdapIpAddress = FallbackLdapIpAddress,
			 serverPort = ServerPort,
			 bindDn = BindDn,
			 bindPassword = BindPassword,
			 baseDn = BaseDn,
			 useReferrals = undefined,
			 profileFilter = ProfileFilter,
			 nodeCredential = NodeCredential,
			 trustCategory = TrustCategory,
			 useTls = UseTls,
			 tlsMode = TlsMode,
			 userLabel = UserLabel};
    
     {ldap, LdapId, LdapIpAddress, FallbackLdapIpAddress, ServerPort,
      BindDn, BindPassword, BaseDn, UseReferrals, ProfileFilter, NodeCredential,
      TrustCategory, _TlsCaCertificate, _TlsClientCertificate, _TlsClientKey,
      UseTls, _UseTlsFallback, TlsMode, UserLabel} ->
      
         #ldap{ldapId = LdapId,
             ldapIpAddress = LdapIpAddress,
             fallbackLdapIpAddress = FallbackLdapIpAddress,
             serverPort = ServerPort,
             bindDn = BindDn,
             bindPassword = BindPassword,
             baseDn = BaseDn,
             useReferrals = UseReferrals,
             profileFilter = ProfileFilter,
             nodeCredential = NodeCredential,
             trustCategory = TrustCategory,
             useTls = UseTls,
             tlsMode = TlsMode,
             userLabel = UserLabel};
               
	OldLdap ->
	    OldLdap
    end,
    {atomic,_} = mnesia:transaction(fun()->mnesia:write(NewLdap) end);


transform(ericssonFilter) ->
    OldTab = swmI:all_objects(ericssonFilter),
    F = fun({ericssonFilter,Id,Dn,Tbac}) -> %LDAP 2.0 format
		mnesia:write(
		  #ericssonFilter{ericssonFilterId = Id,
				  roleAliasesBaseDn = Dn,
				  targetBasedAccessControl = Tbac,
				  version = ?EricssonFilterVersion_2});
	   ( X = #ericssonFilter{} ) ->
		mnesia:write(X)

	end,

    {atomic,_} = mnesia:transaction(
		   fun() ->
			   lists:map(F,OldTab)
		   end).


%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% -type post_init()->                                     %#
%%%     ok                                                  %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Register things for XXX that require init_data phase
%%%              to have been done.
%%% ----------------------------------------------------------
instPhSeqBeg_init_data()->
    omc_server:begin_generate_host_keys(),
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_post_init()->
    LdapMoPath = ["ManagedElement", "SystemFunctions", "SecM", 
		  "UserManagement", "LdapAuthenticationMethod"],
    comsaLib:register_callback(LdapMoPath, omc_model),
    HttpMoPath = ["ManagedElement", "SystemFunctions", "SysM", "HttpM"],

    comsaLib:register_callback(HttpMoPath, omc_model),
    comsaI:register_subscriptions(
      "RcsLdapAuthentication", [{"Filter", filter},
    				{"Ldap", ldap},
    				{"LdapAuthenticationMethod",
    				 ldapAuthenticationMethod}]),
    logI:register_esi_cb(omc_api),
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhSeqEnd_post_init()->
    omc_server:end_generate_host_keys(),
    ok.

-spec children() -> {ok, list()}.
children() ->
    {ok, [
	  {omc_tls_server, {omc_tls_server, start_link, []}, permanent,
	   1000, worker, [omc_tls_server]},
	  {omc_login_bucket, {omc_login_bucket, start_link, []}, permanent,
	   1000, worker, [omc_login_bucket]},
	  {omc_server, {omc_server, start_link, []}, permanent,
	   1000, worker, [omc_server]},
	  {omc_ldap_server, {omc_ldap_server, start_link, []}, permanent,
	   1000, worker, [omc_ldap_server]},
	  {omc_https, {omc_https, start_link, []}, permanent,
	   1000, worker, [omc_https]}	  
	 ]}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
