%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsa_avc.hrl %
%%% Author:	erarafo
%%% Description: Request identifiers of comsa_avc_SUITE. The
%%% numbering must be duplicate-free but is otherwise not significant.
%%% The IFT makefile generates a C header file from the content of
%%% this file.
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R6A/1').
-hrl_date('2016-06-03').
-hrl_author('erarafo').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R6A/1      2016-06-03 erarafo     First version, copied from com_avc.hrl
%%% ----------------------------------------------------------


-define(AVC_INIT, 1).
-define(AVC_FIN, 2).

-define(AVC_IMPL_SET, 101).
-define(AVC_IMPL_CLEAR, 102).

-define(AVC_SET_UINT32, 111).       %% deprecated, replaced by AVC_SET_32
-define(AVC_CLEAR_UINT32, 112).

-define(AVC_SET_32, 113).

-define(AVC_ADD_STRING, 121).
-define(AVC_UPDATE_STRING, 122).
-define(AVC_REMOVE_STRING, 123).

-define(AVC_ADD_STRINGS, 221).      %% add a duplicate-free collection of strings
-define(AVC_REPLACE_STRINGS, 222).  %% replace existing strings with given strings (one or more)
-define(AVC_CLEAR_STRINGS, 223).    %% replace existing strings with zero strings
-define(AVC_DELETE_STRINGS, 224).   %% delete given strings

-define(AVC_CREATE_INSTANCE_SINGLE_ATTR, 201).
-define(AVC_DELETE_INSTANCE, 202).

-define(AVC_DISP, 901).

