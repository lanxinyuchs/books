%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_c.hrl %
%%% Author:	erarafo
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R2A/3').
-hrl_date('2014-04-22').
-hrl_author('uabesvi').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014 All rights reserved.
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
%%% R2A/1      2013-02-13   erarafo     Created
%%% R2A/2      2014-04-22   uabesvi     added show counters
%%% ----------------------------------------------------------

-define(DEBUG_LEVEL, list_to_atom("info")).

%% used towards the CEC service
-define(SERVICE_ID, "PMI").

%% must match definition in master.h of ift_app
-define(PMI, 7).

%% must match IFT_APP codes
-define(PMI_INITIALIZE, 1).
-define(PMI_FINALIZE, 2).
-define(PMI_DATA, 3).
-define(PMI_SETUP_CALLBACK, 4).
-define(PMI_EMUL_DEATH, 5).
-define(PMI_INITIALIZE_2, 6).
-define(PMI_DATA_SHOW_COUNTERS, 7).

%% must match codes in the C interface
-define(SUBSCRIBE, 1).
-define(REPORT, 2).

%% represents the single C node where the IFT_APP runs
-define(NODE, node1).

-define(CEC_EBIN_PATH, os:getenv("RCS_TOP")++"/CEC/CEC_CXC1734012_1/cec/ebin").

-define(INFO(Format, Items),
		case ?DEBUG_LEVEL of
			debug -> ct:pal(Format, Items);
			info -> ct:pal(Format, Items);
			_ -> ok
		end).

-define(DEBUG(Format, Items),
		case ?DEBUG_LEVEL of
			debug -> ct:pal(Format, Items);
			_ -> ok
		end).
