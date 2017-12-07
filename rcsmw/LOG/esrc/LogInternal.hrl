%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	LogInternal.hrl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_vsn('/main/R1A/R2A/1').
-hrl_date('2012-11-01').
-hrl_author('etxjotj').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012 All rights reserved.
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
%%% R1A/1      2012-02-15 etxjotj     Created
%%% ----------------------------------------------------------
%%% 
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS%%% #---------------------------------------------------------
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
%%% Description: Record for internal logs (not visible in the LogM model)
%%% ----------------------------------------------------------
-record(logInternal, {name, maxSize, rotatingSegments}).



