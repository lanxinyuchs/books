%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	oot.hrl %
%%% Author:	eolaand
%%% Description:
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R2A/R4A/R5A/R6A/R9A/R10A/R11A/R12A/1').
-hrl_date('2017-10-30').
-hrl_author('eolaand').
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
%%% R2A/1      2014-01-31 eolaand     Created
%%% R10A/1     2017-05-09 ecaiyan     Support initialize3
%%% R11/1      2017-07-27 egabing     Removed cstn version atom macros
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
-define(SAF_VSN, #safsVersion{releaseCode  = $A,
			      majorVersion = 2,
			      minorVersion = 11}).


-define(ECIM_DN_ALT_OAP, 
	<<"ManagedElement=1,SystemFunctions=1,SysM=1,"
	  "OamAccessPoint=Alternative">>).
-define(ECIM_DN_OAP, 
	<<"ManagedElement=1,SystemFunctions=1,SysM=1,OamAccessPoint=1">>).
-define(ECIM_NETCONF_SSH_DN, 
	<<"ManagedElement=1,SystemFunctions=1,SysM=1,NetconfSsh=1">>).
-define(ECIM_CLI_SSH_DN, 
	<<"ManagedElement=1,SystemFunctions=1,SysM=1,CliSsh=1">>).
-define(ECIM_OAM_TR_CLASS_DN, 
	<<"ManagedElement=1,SystemFunctions=1,SysM=1,OamTrafficClass=1">>).
-define(SYS_FUNC, <<"SystemFunctions">>).
-define(SYS_FUNC_ID, <<"systemFunctionsId">>).
-define(SYS_M, <<"SysM">>).
-define(SYS_M_ID, <<"sysMId">>).
-define(SYS_M_PARENT_DN, <<"systemFunctionsId=1">>).
-define(OAP_PARENT_DN, <<"sysMId=1,systemFunctionsId=1">>).
-define(OAP_ID_VAL, "1").
-define(ALT_OAP_ID_VAL, "Alternative").
-define(ALT_OAP_ID_OLD_VAL, "2").
-define(OAP_DN, <<"oamAccessPointId=1,sysMId=1,systemFunctionsId=1">>).
-define(ALT_OAP_DN, 
	<<"oamAccessPointId=Alternative,sysMId=1,systemFunctionsId=1">>).
-define(OAP_SCHEMA, <<"OotRcsOamAccessPoint">>).
-define(ADDR_IPV4, <<"AddressIPv4">>).
-define(ADDR_IPV4_ID, <<"addressIPv4Id">>).
-define(ADDR_IPV4_RESERVED_BY, <<"reservedBy">>).
-define(ADDR_IPV4_MOM, "RtnL3AddressIPv4").
-define(ADDR_IPV6, <<"AddressIPv6">>).
-define(ADDR_IPV6_ID, <<"addressIPv6Id">>).
-define(ADDR_IPV6_RESERVED_BY, <<"reservedBy">>).
-define(ADDR_IPV6_MOM, "RtnL3AddressIPv6").
-define(OAP, <<"OamAccessPoint">>).
-define(OAP_ID, <<"oamAccessPointId">>).
-define(OAP_SSH_PORT, <<"sshPort">>).
-define(OAP_NETCONF_PORT, <<"netconfPort">>).
-define(OAP_DSCP, <<"dscp">>).
-define(OAP_IPV4_ADDR, <<"ipv4address">>).
-define(OAP_ACC_POINT, <<"accessPoint">>).
-define(OAP_ATTRS, [?OAP_SSH_PORT,
		    ?OAP_NETCONF_PORT,
		    ?OAP_DSCP,
		    %% ?OAP_IPV4_ADDR,
		    ?OAP_ACC_POINT]).

-define(OAP_ECIM_DN, <<"RcsImmAttrEcimDn">>).
-define(OAP_OBJ_ID, <<"RcsImmAttrObjId">>).

-define(TN_IPV4_ADDR, <<"address">>).
-define(TN_IP_ADDRESS, <<"address">>).
-define(TN_5G_USED_IP_ADDRESS, <<"usedAddress">>).
-define(REPLACE, sa_imm_attr_values_replace).
-define(ADD, sa_imm_attr_values_add).
-define(DELETE, sa_imm_attr_values_delete).
-define(ERR_BAD_OP, sa_ais_err_bad_operation).
-define(ERR_EXIST, sa_ais_err_exist).
-define(ERR_NOT_EXIST, sa_ais_err_not_exist).
-define(ONE, sa_imm_one).

-define(CNF_IPV4_ADDR, ipv4_address).
-define(CNF_IPV4_ADDR_ALT, ipv4_address_alt).
-define(CNF_ACC_POINT_ADDR, access_point_address).
-define(CNF_ACC_POINT_ADDR_ALT, access_point_address_alt).
-define(CNF_DSCP, dscp).
-define(CNF_CLI_PORT, cli_port).
-define(CNF_NETCONF_PORT, netconf_port).
-define(CNF_PORTS, [?CNF_CLI_PORT, ?CNF_NETCONF_PORT]).
-define(CNF_OAP_NAMESPACE, oap_namespace).
-define(CNF_OAP_ALT_NAMESPACE, oap_alt_namespace).


-define(SYS_CLI_PORT, cli).
-define(SYS_NETCONF_PORT, netconf).
-define(SYS_PORTS, [?SYS_CLI_PORT, ?SYS_NETCONF_PORT]).

-define(ECIM_UINT8, 5).
-define(ECIM_UINT8_VAL(Val), {?ECIM_UINT8, Val}).
-define(ECIM_UINT16, 6).
-define(ECIM_UINT16_VAL(Val), {?ECIM_UINT16, Val}).
-define(ECIM_REF, 11).
-define(ECIM_REF_VAL(Val), {?ECIM_REF, Val}).
    
-define(ECIM_PORT, <<"port">>).
-define(ECIM_DSCP, <<"dscp">>).

%% -define(TARGET_ARM_WR6_NETCONF_PORT_VAL, 830).
-define(DEFAULT_NETCONF_PORT_VAL, 830).
-define(MIN_PORT_NO, 16#400).
-define(MAX_PORT_NO, 16#FFFF).


-define(LOG(__Sev, __Msg, __Arg),
	logI:write_log("OotLog", 
		       atom_to_list(?MODULE) ++ 
		       ":" ++ 
		       integer_to_list(?LINE), 
		       __Sev, 
		       io_lib:format(__Msg, __Arg))).


-define(LOG_INFO(__Msg), 
	?LOG(info, __Msg, [])).
-define(LOG_INFO(__Msg, __Arg), 
	?LOG(info, __Msg, __Arg)).

-define(LOG_WARNING(__Msg), 
	?LOG(warning, __Msg, [])).
-define(LOG_WARNING(__Msg, __Arg), 
	?LOG(warning, __Msg, __Arg)).

-define(LOG_ERROR(__Msg), 
	?LOG(error, __Msg, [])).
-define(LOG_ERROR(__Msg, __Arg), 
	?LOG(error, __Msg, __Arg)).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           record_name
%%% Description:
%%% ----------------------------------------------------------
