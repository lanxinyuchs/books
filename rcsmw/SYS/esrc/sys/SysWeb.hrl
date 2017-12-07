%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	SysWeb.hrl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_vsn('/main/R2A/R3A/R4A/1').
-hrl_date('2015-09-14').
-hrl_author('etxasta').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2015 All rights reserved.
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
%%% R3A/1      2014-10-03 etxberb     Added alias, erl_script_alias,
%%%                                   erl_script_nocache & mime.
%%% R4A/1      2015-09-04 etxasta     Added sysLogLogin
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

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           record_name
%%% Description: Record for internal logs (not visible in the LogM model)
%%% ----------------------------------------------------------
-record(alias, {realName,   % string() May be a wildcard pattern
		alias       % string() Relative position under document root
		}).

%%% "dir" is equivalent to "alias". "alias" is following the same
%%% naming convention as OTP / Erlang documentation.
-record(dir, {source, % string() May be a wildcard pattern
	      published % string() Relative position under document root
	     }).

-record(erl_script_alias, {url,
			   module
			  }).

-record(erl_script_nocache, {bool
			    }).

-record(mime, {extension,
	       type
	      }).

-record(sysWeb, {key, % integer()
		 prodId, % string() CXP containing the files
		 prodVersion, %string() Cxp version
		 prop % record(alias | )
		}).
        
-record(sysWebSec, {key, % integer()
		    prodId, % string() CXP containing the files
		    prodVersion, %string() Cxp version
		    prop % record(alias | )
		   }).
