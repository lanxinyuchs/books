%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	appm.hrl %
%%% Author: 	etxbjca
%%% Description: Data initialization functions used at system installation and
%%%              software upgrade.
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R3A/R4A/R6A/R7A/R8A/R9A/R10A/R11A/2').
-date('2017-10-04').
-author('etxarnu').
%%% ----------------------------------------------------------
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
%%% R1A/1     20120215   etxarnu      Created
%% ----    ---------- -------  ------------------------------------------------
%%% R2A/1     20130114   etxarnu      Dummy change to get a new release.
%%% R2A/2     20130114   etxarnu      Dummy change to get a new release.
%%% R2A/3     20130302   etxarnu      Added appmPgroupData
%%% R2A/5     20130328   etxarnu      Changed nice to rtCap
%%% R2A/6     20130430   etxpeno      Merge name and no into nameCxc
%%% R2A/7     20130430   etxarnu      Merged BoardType,CxpId and CxpRev into id 
%%%                                   to appmBoardData
%%% R2A/8     20131120   etxarnu      Added MAX_GRP_* defines
%%% R2A/10    2014-01-31 etxberb      Added appmNodeTable.
%%% R2A/11    2014-02-04 etxarnu      Changed MAX_RST_DEF* to 0
%%% R2A/12    2014-04-11 etxpejn      Added heartbeatInterval and cardiacArrest_noOfHb
%% ----    ---------- -------  ------------------------------------------------
%%% R3A/1     2014-09-09 etxarnu      Added cxpNo and cxpRev to appmLmData
%%% R3A/2     2014-09-29 etxarnu      Added sysAdmin
%%% R3A/3     2014-11-17 etxarnu      Added _ug version of tables
%% ----    ---------- -------  ------------------------------------------------
%%% R4A/1     2015-06-20 etxarnu      Added softRt
%%% R4A/2     2015-07-10 eolaand      Added attribute tmpSize in appmPgmData
%% ----    ---------- -------  ------------------------------------------------
%% R6A/1   2016-07-05 etxberb  Added appmHwData & appmHwData_ug.
%% R6A/2   2016-07-08 etxberb  Added DEFAULT* macros.
%% ----    ---------- -------  ------------------------------------------------
%% R7A/1   2016-10-03 uabesvi  HV26316 Timeout in ESI CB
%% R7A/2   2016-10-05 etxarnu  HV26316 added MAX_ESI_CB_TMO_DEF
%% ----    ---------- -------  ------------------------------------------------
%% R8A/1   2016-12-21 etxarnu  added pgroup to appmLmData
%% ----    ---------- -------  ------------------------------------------------
%% R10A/1  2017-05-29 etxarnu  added MP_TAGS
%% ----    ---------- -------  ------------------------------------------------
%% R11A/1  2017-09-29 etxarnu  added dataProcessing to appmPgmData
%% R11A/2  2017-10-03 etxarnu  Added delayedKill
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------


-define(MAX_RST_DEF, "0").
-define(MAX_TMO_DEF, "300").
-define(MAX_ESI_CB_TMO_DEF, "120").
-define(MAX_RST_DEF_INT, 0).
-define(MAX_TMO_DEF_INT, 300).
-define(MAX_GRP_RST_DEF_INT, 3).
-define(MAX_GRP_TMO_DEF_INT, 300).

-define(DEFAULT_BoardRev,   "P1A").
-define(DEFAULT_HwModel,    '_').
-define(WILDCARD_HwDataTbl, "").

-define(MP_TAGS,["local",
		 "central",
		 "dynamic",
		 "upi",
		 "rollback_esi",
		 "esi",
		 "esi_local",
		 "shared_lib",
		 "restart_cold"
		]).


%%%  Mnesia data %%%

%%% @doc appmNodeTable stores general info about nodes.
-record(appmNodeTable, {key, value}).

%%% @doc appmDataTable stores general info about applications.
-record(appmDataTable, {key, value}).

%%% @doc appmLmData keeps information about all registered load modules
-record(appmLmData, {nameCxc, % {Name, CxcId}
		     rev,
		     tag,
		     date,
		     cxpNo,  
		     cxpRev,  
		     cxpPath, % Root path for this load module
		     pgroup, % Which pgroup lm belongs to, if any
		     files = [], % List of file records
		     mpInfo % Extra info for MP  programs
		    }).

%%% @doc appmPgroupData keeps information what LMs belong to a Program Group
-record(appmPgroupData, {name,
			 escalation, % board or subscriber
			 maxRestarts, % before escalation
			 maxTime, % maxRestarts during this time
			 lms = [] % List of #lmProdId records
			}).

%%% @doc appmBoardData keeps information what LMs belong to a board type
-record(appmBoardData, {id, %= {BoardType,CxpId,CxpRev}
			lms = [] % List of #lmProdId records
		       }).

%%% @doc appmHwData keeps information what LMs belong to a hw type
-record(appmHwData, {id, %= {HwType,CxpId,CxpRev}
		     lms = [] % List of #lmProdId records
		    }).

%%% The following two tables are filled when SWM calls appdata at start of upgrade
%%%  The reason is that during the preload phase, the RUs are preoloaded with new SW
%%% @doc appmLmData_ug keeps information about all registered load modules during UG
-record(appmLmData_ug, {nameCxc, % {Name, CxcId}
		     rev,
		     tag,
		     date,
		     cxpNo,  
		     cxpRev,  
		     cxpPath, % Root path for this load module
		     files = [], % List of file records
		     mpInfo % Extra info for MP  programs
		    }).

%%% @doc appmBoardData_ug keeps information what LMs belong to a board type during UG
-record(appmBoardData_ug, {id, %= {BoardType,CxpId,CxpRev}
			lms = [] % List of #lmProdId records
		       }).

%%% @doc appmHwData_ug keeps information what LMs belong to a hw type during UG
-record(appmHwData_ug, {id, %= {HwType,CxpId,CxpRev}
			lms = [] % List of #lmProdId records
		       }).
%%% End of Mnesia data %%%

%%% @doc lmProdId is the Product Id for an LM
-record(lmProdId, {nameCxc, rev}).

%%% @doc appmPgmData keeps information about an MP Program in the appmServer
-record(appmPgmData, {escalation, % pgmGrp, board or subscriber
		      maxMem,
		      maxRestarts, %
		      maxTime,
		      rtCap,
		      softRt,
		      netAdmin,
		      sysAdmin,
		      dataProcessing,
		      delayedKill,
		      env=[],
		      exportedEnv=[],
		      heartbeatInterval,
		      cardiacArrest_noOfHb,
		      tmpSize,
		      maxEsiCbTime
		     }).


%%% @doc file is stores the type and relative path to a file
-record(file, {type,relpath }).


-record(appmExtServer, {monitor, socket, pid}).

%%% Old format, DEPRECATED, still here for backward compatibility
-record(binFile, {arch,relpath }).

-record(libFile, {arch,relpath }).

