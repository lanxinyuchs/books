%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comTypes.hrl %
%%% Author:     etxjotj
%%% Description:
%%%
%%% ----------------------------------------------------------
-hrl_vsn('/main/R1A/2').
-hrl_date('2012-01-18').
-hrl_author('etxjotj').
%%% %CCaseTemplateFile: module.hrl %
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
%%% -----      ---------- --------    ------------------------
%%% R1A/1      2012-01-18 etxjotj     Created
%%% ----------------------------------------------------------
%%%
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------

%% COM Basic data types taken from ComOamSpiModelRepository_1.h

%%     ComOamSpiDatatype_INT8 = 1
-define(INT8, 1).
-define(INT8(Int),{?INT8, Int}).
%%     ComOamSpiDatatype_INT16 = 2
-define(INT16, 2).
-define(INT16(Int),{?INT16, Int}).
%%     ComOamSpiDatatype_INT32 = 3
-define(INT32, 3).
-define(INT32(Int),{?INT32, Int}).
%%     ComOamSpiDatatype_INT64 = 4
-define(INT64, 4).
-define(INT64(Int),{?INT64, Int}).
%%     ComOamSpiDatatype_UINT8 = 5
-define(UINT8, 5).
-define(UINT8(Int),{?UINT8, Int}).
%%     ComOamSpiDatatype_UINT16 = 6
-define(UINT16, 6).
-define(UINT16(Int),{?UINT16, Int}).
%%     ComOamSpiDatatype_UINT32 = 7
-define(UINT32, 7).
-define(UINT32(Int),{?UINT32, Int}).
%%     ComOamSpiDatatype_UINT64 = 8
-define(UINT64, 8).
-define(UINT64(Int),{?UINT64, Int}).
%%     ComOamSpiDatatype_STRING = 9
-define(STRING, 9).
-define(STRING(Str),{?STRING, Str}).
%%     ComOamSpiDatatype_BOOL = 10
-define(BOOL, 10).
-define(BOOL(Bool),{?BOOL, Bool}).
%%     ComOamSpiDatatype_REFERENCE = 11
-define(REFERENCE, 11).
-define(REFERENCE(Ref), {?REFERENCE, Ref}).
%%     ComOamSpiDatatype_ENUM = 12
-define(ENUM, 12).
-define(ENUM(Enum),{?ENUM, Enum}).
%%     ComOamSpiDatatype_STRUCT = 14
-define(STRUCT, 14).
-define(STRUCT(StructMembers),{?STRUCT,StructMembers}).
-define(STRUCT_MEMBER(Name,Value),{Name,Value}).

%%% Not implemented
%%     ComOamSpiDatatype_DERIVED = 13
%%     ComOamSpiDatatype_VOID = 15
