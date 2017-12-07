%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	imm.hrl %
%%% Author:	erarafo
%%% Description: Header file needed for using imm.erl.
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R3A/R5A/R6A/4').
-hrl_date('2016-06-19').
-hrl_author('erarafo').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% R3A/1      2015-03-13 erarafo     First version
%%% R3A/2      2015-03-20 erarafo     SaImmScopeT enum added
%%% R5A/1      2016-04-18 erarafo     Type definitions
%%% R6A/1      2016-06-09 erarafo     immHandle() sharpened
%%% R6A/2      2016-06-10 erarafo     Added type: #instDescr{}
%%% R6A/3      2016-06-17 erarafo     Added UNUSED_ARG macro
%%% R6A/4      2016-06-18 erarafo     Added type: #struct{}
%%% ----------------------------------------------------------

% the SaImmScopeT enum
-define(SA_IMM_ONE, 1).
-define(SA_IMM_SUBLEVEL, 2).
-define(SA_IMM_SUBTREE, 3).

-define(SA_IMM_CCB_NO_FLAG, 0).
-define(SA_IMM_CCB_REGISTERED_OI, 1).

-define(SA_IMM_ATTR_VALUES_ADD, 1).
-define(SA_IMM_ATTR_VALUES_DELETE, 2).
-define(SA_IMM_ATTR_VALUES_REPLACE, 3).


% attribute type codes from saImm.h
-define(SA_IMM_ATTR_SAINT32T, 1).
-define(SA_IMM_ATTR_SAUINT32T, 2).
-define(SA_IMM_ATTR_SAINT64T, 3).
-define(SA_IMM_ATTR_SAUINT64T, 4).
-define(SA_IMM_ATTR_SATIMET, 5).
-define(SA_IMM_ATTR_SANAMET, 6).
-define(SA_IMM_ATTR_SAFLOATT, 7).
-define(SA_IMM_ATTR_SADOUBLET, 8).
-define(SA_IMM_ATTR_SASTRINGT, 9).
-define(SA_IMM_ATTR_SAANYT, 10).
-define(SA_IMM_ATTR_CSSTRUCTT, 9999).

-define(SA_MAX_NAME_LENGTH, 256).

-define(UNUSED_ARG, 0).

-type attrTypeImm() :: 1..10.

-type immHandle()  :: 0..18446744073709551615.  % 0..(2^64)-1
-type aoHandle()   :: 0..18446744073709551615.
-type ccbHandle()  :: 0..18446744073709551615.
-type accHandle()  :: 0..18446744073709551615.
-type oiHandle()   :: 0..18446744073709551615.
-type immVersion() :: {string(), integer(), integer()}.
-type oiVersion()  :: {string(), integer(), integer()}.
-type className()  :: string().
-type dn()         :: string().


-record(testContext, 
	{node   :: atom(),
	 child  :: atom()}).

-type testContext()  :: #testContext{}.


-type structPrimitive() :: integer()|string().

-record(memberValues,
	{name          :: string(),
	 type          :: attrType(),
	 values        :: [structPrimitive()]
	}).

-record(struct,
	{name        :: string(),
	 members     :: [#memberValues{}]
	}).


-type attrType()       :: int32|string|name|struct.
-type attrPrimitive()  :: integer()|string()|#struct{}.


-record(attrValues,    % order of fields is significant, do not alter
	{name          :: string(),
	 type          :: attrType(),
	 values        :: [attrPrimitive()]
	}).



-record(instDescr,
	{className     :: string(),
	 parentName    :: string(),
	 attrValues    :: [#attrValues{}]
	 }).

-type modType()    :: ?SA_IMM_ATTR_VALUES_ADD |
	              ?SA_IMM_ATTR_VALUES_DELETE |
	              ?SA_IMM_ATTR_VALUES_REPLACE.

-record(attrMod,
	{modType          :: modType(),
	 modAttr          :: #attrValues{}
	}).
