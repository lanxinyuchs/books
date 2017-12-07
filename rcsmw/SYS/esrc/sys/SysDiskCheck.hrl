%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	SysDiskCheck.hrl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_vsn('/main/R4A/2').
-hrl_date('2015-09-28').
-hrl_author('etxtory').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
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
%%% R4A/1      2015-07-21 etxjotj     Created
%%% R4A/2      2015-09-27 etxtory     Added disk reserve table
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
%%% #           sysDiskCheckCb
%%% Description: Record for diskSpaceCheck callback owners
%%% ----------------------------------------------------------
-record(sysDiskCheckCb, {file, module}).

%%% ----------------------------------------------------------
%%% #           sysDiskReserve
%%% Description: Record for diskSpace reservation
%%% ----------------------------------------------------------
-record(sysDiskReserve, {dir, module, diskspace}). 
