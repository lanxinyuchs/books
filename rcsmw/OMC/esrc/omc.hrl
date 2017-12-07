%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc.hrl %
%%% Author:	etxbjca
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R2A/R5A/R8A/R9A/1').
-hrl_date('2017-02-09').
-hrl_author('etomist').
%%% %CCaseTemplateFile:	module.hrl %
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
%%% R2A/1      2014-01-20   etxlg     Created
%%% R2A/2      2014-01-20   etxlg     More defined
%%% R2A/3      2014-01-26   etxlg     More defined
%%% R2A/4      2014-03-05   etxlg     #ldap_params
%%% R2A/5      2014-03-10   etxlg     ldap define
%%% R2A/6      2014-03-14   etxlg     timeout 5s -> 4s to avoid gen_srv timeout
%%%				      probably we should increase this value
%%%				      since there is tcp transport, i.e. we
%%%				      should atleast handle retry at connect
%%%				      but there are several gen_servers, leave
%%%				      this for later improvement.
%%% R2A/7      2014-03-21   etxlg     tracing added to #ldap_params{}
%%% R2A/8      2014-03-24   etxlg     redefined use of trace in #ldap_params{}
%%% R2A/9      2014-04-09   etxlg     arg_list in  #user_scan
%%% R2A/10     2014-06-11   etxlg     Role of superOaMuser
%%% R2A/11     2014-10-02   etxlg     extended ldap_params with extra_inet_opts
%%% R2A/12     2014-10-13   etxlg     MAX_SESSIONS increased to 20
%%% R5A/1      2016-04-05   ehsake    LDAP_TIMEOUT and LDAP_QUERY_TIMEOUT increased, HU72746
%%% R5A/2      2016-08-19   ehsake    Increase bucket to 15,HV18193.

%%% ----------------------------------------------------------
%%% 
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description: 
%%% ----------------------------------------------------------
-define(MAX_SESSIONS, 20).
%-define(MAX_SESSIONS, 3). %During test

-define(BUCKET_TIMEOUT, 5000).
%-define(BUCKET_TIMEOUT, 60 * 1000). %During test
-define(BUCKET_SIZE, 15).

-define(MS_USER_ROLE, "ms_useridentity").
-define(MS_USER_SET_PREFIX, <<"ManagedElement=1,SystemFunctions=1,SecM=1,"
				"UserManagement=1,UserIdentity=1,userName=">>).

-define(MS_USER_CAP_URN, <<"urn:X-ericsson:params:auth:ms_useridentity:">>).

-define(SUPER_ROLE, "EricssonSupport").

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           record_name
%%% Description: 
%%% ----------------------------------------------------------
%used in omc_ssh_channel/omc_tls_instance/omc_ms_user_change
-record(user_scan, {	state,
			acc,
			pattern1,
			last_from,
			user,
			count = 0,
			arg_list = []}).

%%% used in omc_ldap_server/omc_ldap_instance/omc_tbac
%%% wait this many ms for the ldapsearch to complete, 3 seconds is from
%%% 2/155 01-FAE 151 04 Uen Rev. A (Role and Target Based Access Control
%%% with LDAP, Howto) (possibly applicable only to the bind?)
%%% 3 seconds is not enough for OSS-RC due to larger amount of nodes.
-define(LDAP_TIMEOUT, 4000).
-define(LDAP_QUERY_TIMEOUT, 4500). %time allowed from start to end
-define(AUTHENTICATESCOPE, "ericssonUserAuthenticationScope").
-define(AUTHORIZESCOPE,    "ericssonUserAuthorizationScope").

-record(ldap_params, {
                ldap_primary,
                ldap_secondary,
                server_port,
                bind_dn,
                bind_password,
                base_dn,
                ca_certs,
                node_cert,
                node_key,
		verify_fun,
                tls_mode,
		profile_filter,
		flexible_filter,
                use_tbac,
                target_type,
                role_alias_base_dn,
		trace = undefined,
	        server, %%set to secondary/primary for trace printing
		extra_inet_opts = []
                }).

%%% ongoing TLS/SSH sessions
-record(omc_session_info, {
                key,
                type,
                subtype,
                user,
                ip_address
                }).

%%% counts ongoing cli/coli/netconf sessions
-record(omc_session_count, {
                key,
                count
                }).
