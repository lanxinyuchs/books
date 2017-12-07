%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sys.hrl %
%%% Author: 	etxbjca
%%% Description: Data initialization functions used at system installation and
%%%              software upgrade.
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-vsn('/main/R3A/R4A/R5A/1').
-date('2016-03-12').
-author('erarafo').
%%% ----------------------------------------------------------
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
%%% R3A/1      2015-01-29 etxarnu     Created
%%% R5A/1      2016-03-12 erarafo     Type vc_state() added
%%% ----------------------------------------------------------



-type vc_state()  :: vc|no_vc|unknown.


%%%  Mnesia data %%%

%%% @doc sysVariables stores general info for SYS.
-record(sysVariables, {key, value}).
-record(sysSftpRegDirs, {dir, root_dir, type, size, mod}).
