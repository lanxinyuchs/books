%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	clhTables.hrl %
%%% Author:	etxbjca
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R3A/R4A/2').
-hrl_date('2015-06-05').
-hrl_author('etxberb').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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
%%% R3A/1      2015-01-19   etxpejn     Created
%%% R4A/1      2015-04-17 etxberb     Updated according to changed interfaces.
%%% R4A/2      2015-06-05 etxberb     Changed FruId_undefined to FruId_default.
%%% ----------------------------------------------------------
%%% 
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------

-define(FruId_default_prefix, "'$default_fru_id' MpId = ").
-define(FruId_default(MpId),  list_to_binary(?FruId_default_prefix ++
					     sysUtil:term_to_string(MpId))).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------

-define(TblVersion_clhMpConf, 1).
-record(clhMpConf, {mpId,          % integer(),
		    fruId,         % binary(string())
		    erlNode,       % atom()
		    mpRole,        % atom(core | regular | undefined)
		    coreRank}).    % atom(primary | secondary | undefined)

-define(TblVersion_clhMpState, 1).
-record(clhMpState, {mpId,         % integer(),
		     opState = disabled,   % atom(disabled | enabled)
		     coreState}).  % atom(active | standby | undefined)
		     
-record(clhTableVersion, {table,
			  version}).
