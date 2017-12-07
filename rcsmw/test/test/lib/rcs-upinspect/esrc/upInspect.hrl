%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	upInspect.hrl %
%%% Author:	erarafo
%%% Description: Header items for the upInspect program.
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R5A/R6A/R8A/3').
-hrl_date('2016-12-02').
-hrl_author('erarafo').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% R5A/1      2016-02-11 erarafo     First version
%%% R5A/2      2016-02-15 erarafo     Added inspContext record
%%% R5A/3      2016-02-18 erarafo     Adjustments
%%% R5A/4      2016-02-19 erarafo     Restructuring
%%% R5A/5      2016-02-22 erarafo     Minor addition
%%% R5A/6      2016-02-24 erarafo     Refactoring
%%% R5A/7      2016-02-25 erarafo     Minor additions
%%% R5A/9      2016-03-18 erarafo     Additions
%%% R6A/1      2016-04-28 erarafo     immClass{} added
%%% R8A/1      2016-11-29 erarafo     Refactoring
%%% R8A/2      2016-12-01 erarafo     Support for HW/SW compatibility
%%% R8A/3      2016-12-02 erarafo     Cleanup
%%% ----------------------------------------------------------

-define(RCS_MW_ARM, "CXP9025546_3").

-include_lib("xmerl/include/xmerl.hrl").


% TODO, introduce informative types that map to atom(), string(), etc.

-type severity()   :: ok|deprec|warning|error|fatal.

-type globalName() :: atom().

-type relpath()    :: string().

-type pathname()   :: string().

-type basename()   :: string().

-record(osResult,
	{code=0          :: integer(),
         stdout=[]       :: [string()],
	 stderr=[]       :: [string()]}).

-record(boardType,
	{productNumber=""   :: string(),
	 revision=""        :: string()}).

-record(product,
	{name=""         :: string(),
	 id=""           :: string(),
	 ver=""          :: string(),
	 file=""         :: basename()}).

-record(boardList,
	{hwcategory=""   :: string(),
	 hwmodel=""      :: string(),
	 boardTypes=[]   :: [#boardType{}],
	 products=[]     :: [#product{}]}).
	

-record(cxpInfo,
	{name=""                      :: string(),
	 id=""                        :: string(),
	 ver=""                       :: string(), 
	 filename=""                  :: string(),
	 dir=""                       :: string(),
	 topElement=#xmlElement{}     :: #xmlElement{}}).

-record(cxcInfo,
	{name=""                      :: string(),
	 id=""                        :: string(),
	 version=""                   :: string()}).

-record(programFileInfo,
	{type=""     :: string(),
	 relPath=""  :: string()}).

-record(programInfo,
	{tag=""                     :: string(),
	 cxpNameIdVer={}            :: tuple(),
	 cxpDir="" :: string(),
	 cxcName=""                 :: string(),

	 name=""                    :: string(),
	 fileInfos=[]               :: [#programFileInfo{}]}).

-record(lmlistLmRef,
	{cxpNameIdVer={"", "", ""}   :: {string(), string(), string()},
	 cxcId=""                    :: string(),
	 boardType=""                :: string(),
	 resFile=""                  :: relpath()}).

-record(programgroupLmRef,
	{resFile=""   :: string()}).

-record(inspContext,
	{enabled=false    :: boolean(),
	 options=[]       :: [{atom(), any()}],
	 cxpInfo          :: #cxpInfo{}|undefined,
	 cxcInfo          :: #cxcInfo{}|undefined,
	 resolvedFile=""  :: string()}).

-record(xsdResult,
	{libXml2Code=0       :: integer(),        % 0 = ok, 1, 2, 3 ... = nok
         xmerlXsdCode=0      :: integer(),
	 text=""             :: string(),
	 reasons             :: any(),
	 textFromReasons     :: any()}).


-record(targetHandler,
	{schemaPath 	         :: string()|undefined,
	 xsdState                :: tuple()|undefined,
	 handler                 :: function()|undefined,
	 summarizer              :: function()|undefined}).

-record(immClass,
	{name         :: string(),
	 rdnName      :: string()
	}).
 
