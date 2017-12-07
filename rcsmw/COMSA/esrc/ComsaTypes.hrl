%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ComsaTypes.hrl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_id('11/190 55-CNA 113 348 Ux').
-hrl_vsn('/main/R4A/R8A/1').
-hrl_date('2016-10-25').
-hrl_author('etxberb').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% R1A/1      2010-12-20   etxjotj   Created
%%% R8A/1      2016-10-25   etxberb   Added fmAlarmType_notReg
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
%-define(CONSTANT_NAME,               Value).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           record_name
%%% Description: 
%%% ----------------------------------------------------------

-record(comsaSchemaLocations, {model,   % string() - Schema Name
			       path,    % string() - Path to the file
			       file}).  % string() - File basename

%%% ----------------------------------------------------------
%%% #           fmAlarmType_notReg
%%% Description: Alarms that are specified not to be registered for the current
%%%              node type.
%%% ----------------------------------------------------------
-record(fmAlarmType_notReg, {fmAlarmTypeId,   % Same as #fmAlarmType{}
			     node_type,
			     registerOnNodes}).
