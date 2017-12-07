%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfMeta.hrl %
%%% Author:	erarafo
%%% Description: Header clauses for users of the gmfMetaLlib
%%% functions.
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R2A/R4A/1').
-hrl_date('2015-06-04').
-hrl_author('erarafo').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% R2A/1      2013-09-18 erarafo     First version
%%% R2A/2      2013-10-24 erarafo     3-level schema versioning support
%%% R2A/3      2014-01-12 erarafo     Versioning failure support
%%% R2A/4      2014-02-07 erarafo     Extended record #versionFailure{}
%%% R2A/5      2014-10-14 erarafo     Extended record #immInfo{}
%%% R2A/6      2014-10-15 erarafo     Restored #immInfo{}
%%% R4A/1      2015-06-03 erarafo     Most of the info lifted to gmf.hrl

-record(versionFailure, {
			 schemaName       :: string(),
			 fromVersions     :: [immInfoFromVersion()],
			 oldVersion       :: immInfoVersion(),
			 oldMode="actual" :: string(),
			 toVersion        :: immInfoVersion()
			}).
