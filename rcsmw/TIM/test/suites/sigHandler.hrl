%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sigHandler.hrl %
%%% Author:	erarafo
%%% Description:
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R4A/3').
-hrl_date('2015-11-03').
-hrl_author('erarafo').
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
%%% R4A/1      2015-10-30 erarafo     First version
%%% R4A/2      2015-11-02 erarafo     Switch to OTP 18 time API
%%% R4A/3      2015-11-03 erarafo     Signal handler improvement
%%% ----------------------------------------------------------

-type millis()     :: integer().
-type timestamp()  :: non_neg_integer().

-type sigSpec()    :: non_neg_integer()|[non_neg_integer()]|undefined.

-record(signal, {arrived          :: timestamp(),
                 source           :: non_neg_integer(),
                 no               :: non_neg_integer(),
		 fields           :: list()
		}).

-define(MILLIS(), erlang:monotonic_time(milli_seconds)).
