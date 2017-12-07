%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfComteI.erl %
%%% Author:	qthupha
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(gmfComteI).
-vsn('/main/R1A/R2A/R3A/2').
-date('2015-02-03').
-author('etxpeno').
-shaid('043e43a8afc071c07bfdd25bbdcc457ee7500bd8').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2015 All rights reserved.
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
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2012-04-18 qthupha     Created
%%% R2A/5      2014-05-22 erarafo     Removed prepareReplay/4
%%% ----------------------------------------------------------
%%%
%%%% #---------------------------------------------------------
%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([getMoAttribute/5, nextMo/6]).
-export([createMo/7, setMoAttribute/7, deleteMo/5]).
-export([prepare/6, commit/6, finish/6, action/6]).
-export([commit/4]).

%% -export([prepareReplay/4]).

-export([existsMo/5, countMoChildren/6, getMoAttributes/6,
	 setMoAttributes/6, action/7, createMo/8]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%===========================================================================
%% @spec getMoAttribute(ECIMPath, TransId, Ldn, CxpRev, ImmH)
%%
%%       -> ok
%%
%% where
%%
%%   ECIMPath = [binary()]
%%   TransId  = integer()
%%
%% @doc
%%
%% @end
%%===========================================================================
getMoAttribute(ECIMPath, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:getMoAttribute(ECIMPath, TransId, Ldn, CxpRev, ImmH).

%%===========================================================================
%% @spec nextMo(ECIMPath, CurrKey, TransId, Ldn, CxpRev, ImmH)
%%
%%       -> ok
%%
%% where
%%
%%   ECIMPath = [binary()]
%%   CurrKey  = binary()
%%   TransId  = integer()
%%
%% @doc
%%
%% @end
%%===========================================================================
nextMo(ECIMPath, CurrKey, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:nextMo(ECIMPath, CurrKey, TransId, Ldn, CxpRev, ImmH).

%%===========================================================================
%% @spec createMo(ECIMPath, KeyName, KeyValue, TransId, Ldn, CxpRev, ImmH)
%%
%%       -> ok
%%
%% where
%%
%%   ECIMPath = [binary()]
%%   KeyName  = binary()
%%   KeyValue = binary()
%%   TransId  = integer()
%%
%% @doc
%%   Craete an MO
%% @end
%%===========================================================================
createMo(ECIMPath, KeyName, KeyValue, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:createMo(ECIMPath,
			 KeyName,
			 KeyValue,
			 TransId,
			 Ldn,
			 CxpRev,
			 ImmH).

%%===========================================================================
%% @spec setMoAttribute(ECIMPath, Value, UserObject, TransId, Ldn, CxpRev, ImmH)
%%
%%       -> ok
%%
%% where
%%
%%   ECIMPath   = [binary()]
%%   Value      = binary()
%%   UserObject = undefined | [term()]
%%   TransId    = integer()
%%
%% @doc
%%   Set attribute in an MO
%% @end
%%===========================================================================
setMoAttribute(ECIMPath, Value, UserObject, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:setMoAttribute(ECIMPath,
			       Value,
			       UserObject,
			       TransId,
			       Ldn,
			       CxpRev,
			       ImmH).

%%===========================================================================
%% @spec deleteMo(ECIMPath, TransId, Ldn, CxpRev, ImmH)
%%
%%       -> ok
%%
%% where
%%
%%   ECIMPath = [binary()]
%%   TransId  = integer()
%%
%% @doc
%%   Delete an MO
%% @end
%%===========================================================================
deleteMo(ECIMPath, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:deleteMo(ECIMPath, TransId, Ldn, CxpRev, ImmH).

%%===========================================================================
%% @spec prepare(ECIMPath, UserObject, TransId, Ldn, CxpRev, ImmH)
%%
%%       -> ok
%%
%% where
%%
%%   ECIMPath   = [binary()]
%%   UserObject = undefined | [term()]
%%   TransId    = integer()
%%
%% @doc
%%   Prepare a commit
%% @end
%%===========================================================================
prepare(ECIMPath, UserObject, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:prepare(ECIMPath, UserObject, TransId, Ldn, CxpRev, ImmH).

%%===========================================================================
%% @spec commit(ECIMPath, UserObject, TransId, Ldn, CxpRev, ImmH)
%%
%%       -> ok
%%
%% where
%%
%%   ECIMPath   = [binary()]
%%   UserObject = undefined | [term()]
%%   TransId    = integer()
%%
%% @doc
%%   Commit the transaction
%% @end
%%===========================================================================
commit(ECIMPath, UserObject, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:commit(ECIMPath, UserObject, TransId, Ldn, CxpRev, ImmH).

%%===========================================================================
%% @spec finish(ECIMPath, UserObject, TransId, Ldn, CxpRev, ImmH)
%%
%%       -> ok
%%
%% where
%%
%%   ECIMPath   = [binary()]
%%   UserObject = undefined | [term()]
%%   TransId    = integer()
%%
%% @doc
%%   Finish the transaction
%% @end
%%===========================================================================
finish(ECIMPath, UserObject, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:finish(ECIMPath, UserObject, TransId, Ldn, CxpRev, ImmH).

%%===========================================================================
%% @spec action(ECIMPath, Parameters, TransId, Ldn, CxpRev, ImmH)
%%
%%       -> ok
%%
%% where
%%
%%   ECIMPath   = [binary()]
%%   Parameters = [term()]
%%   TransId    = integer()
%%
%% @doc
%%
%% @end
%%===========================================================================
action(ECIMPath, Parameters, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:action(ECIMPath, Parameters, TransId, Ldn, CxpRev, ImmH).

commit(TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:commit(TransId, Ldn, CxpRev, ImmH).

%% prepareReplay(TransId, Ldn, CxpRev, ImmH) ->
%%     gmfComteLib:prepareReplay(TransId, Ldn, CxpRev, ImmH).

existsMo(ECIMPath, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:existsMo(ECIMPath, TransId, Ldn, CxpRev, ImmH).

countMoChildren(ECIMPath, ClassName, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:countMoChildren(ECIMPath, ClassName, TransId, Ldn, CxpRev,
				ImmH).

getMoAttributes(AttrNames, ECIMPath, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:getMoAttributes(AttrNames, ECIMPath, TransId, Ldn, CxpRev,
				ImmH).

setMoAttributes(AttrNames, ECIMPath, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:setMoAttributes(AttrNames, ECIMPath, TransId, Ldn, CxpRev,
				ImmH).

action(Name, ReverseDn, NamedParams, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:action(Name, ReverseDn, NamedParams, TransId, Ldn, CxpRev,
		       ImmH).

createMo(ECIMPath, KeyName, KeyValue, InitAttrs, TransId, Ldn, CxpRev, ImmH) ->
    gmfComteLib:createMo(ECIMPath, KeyName, KeyValue, InitAttrs, TransId, Ldn,
			 CxpRev, ImmH).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------




%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
