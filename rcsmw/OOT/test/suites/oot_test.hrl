%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	oot_test.hrl %
%%% Author:	eolaand
%%% Description: Common macro definitions for OOT tests
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R11A/4').
-hrl_date('2017-09-25').
-hrl_author('eolaand').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R11A/1      2017-08-29 eolaand     Created
%%% ----------------------------------------------------------
%%%
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------

-define(TNODE, rpc).
-define(NETCNF, nc1).
-define(CLI, c1).
-define(SNMP, snmp1).
-define(IFT_NODE, node1).
-define(IFT_NAME, cstn1).
-define(PROXY_SLAVE, proxy_slave_cstn).
-define(COMTOP, {xmlns, "urn:com:ericsson:ecim:ComTop"}).
-define(NC_NAMESPACE, "urn:ietf:params:xml:ns:netconf:base:1.0").
-define(DELETE, [{'xmlns:nc', ?NC_NAMESPACE}, {'nc:operation', "delete"}]).
-define(SLEEP_AFTER_EDIT, timer:seconds(10)).
-define(RPC_CALL_TIMEOUT, 10000).
-define(IFT_NOTIFICATION_TIMEOUT, 10000).
-define(OOT_NOTIFICATION_TIMEOUT, 10000).
-define(CSTN_NOTIFICATION_TIMEOUT, 10000).
-define(OAP_ID, "1").
-define(ALT_OAP_ID, "Alternative").
-define(TN_MASK, "/24").
-define(TN_MASK_ALT, "/24").
-define(TN_IPV6_MASK, "/64").
-define(TN_IPV6_MASK_ALT, "/64").
-define(NS, "fib_2").
-define(ALT_NS, "fib_3").
-define(FH_NS, "fib_4").
-define(LDN_IPV4_1, 
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1").
-define(LDN_IPV4_2, 
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=2,AddressIPv4=1").
-define(LDN_IPV4_3, 
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=3,AddressIPv4=1").
-define(LDN_IPV6_1, 
	"ManagedElement=1,Transport=1,Router=2,InterfaceIPv6=1,AddressIPv6=1").
-define(LDN_IPV6_2, 
	"ManagedElement=1,Transport=1,Router=2,InterfaceIPv6=2,AddressIPv6=1").
-define(LDN_IPV6_3, 
	"ManagedElement=1,Transport=1,Router=2,InterfaceIPv6=3,AddressIPv6=1").

%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description:
%%% ----------------------------------------------------------
%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           record_name
%%% Description:
%%% ----------------------------------------------------------
