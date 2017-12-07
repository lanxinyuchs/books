%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaDispatch.erl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(comsaDispatch).
-vsn('/main/R2A/2').
-date('2014-09-18').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2014 All rights reserved.
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
%%% R2A/1      2012-11-15   etxjotj     Created
%%% R2A/2      2014-09-18 etxberb     Added validate/3.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3, setMoAttribute/4,
	 createMo/4, deleteMo/2, action/3]).
-export([prepare/3,commit/3,finish/3]).
-export([validate/3]).
%%%-export([something_else/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%% -export([internal_function1/2]).
%% %%%-export([internal_function2/3]).

%% -include("template.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ------------------------------------------------------------

get_callback(DnRev) ->
    case comsaLib:get_callback(DnRev) of
	Module when is_atom(Module) -> 
	    Module;
	Other -> 
	    element(1, Other)
    end.

getMoAttribute(DnRev, TxHandle) ->
    Module = get_callback(DnRev),
    try apply(Module, getMoAttribute, [DnRev, TxHandle]) of
	Result -> 
	    Result
    catch T:E ->
	    erlang:T(E, [DnRev, TxHandle])
    end.

nextMo(DnRev, Key, TxHandle) ->
    Module = get_callback(DnRev),
    try apply(Module, nextMo, [DnRev, Key, TxHandle]) of
	Result -> 
	    Result
    catch T:E ->
	    erlang:T(E, [DnRev, Key, TxHandle])
    end.

setMoAttribute(DnRev, TypeAndValue, Internal, TxHandle) ->
    Module = get_callback(DnRev),
    try apply(Module, setMoAttribute, [DnRev, TypeAndValue, Internal, TxHandle]) of
	Result -> 
	    Result
    catch T:E ->
	    erlang:T(E, [DnRev, TypeAndValue, Internal, TxHandle])
    end.
    
createMo(DnRev, KeyAttr, Key, TxHandle) ->
    Module = get_callback(DnRev),
    try apply(Module, createMo, [DnRev, KeyAttr, Key, TxHandle]) of
	Result -> 
	    Result
    catch T:E ->
	    erlang:T(E, [DnRev, KeyAttr, Key, TxHandle])
    end.
    
deleteMo(DnRev, TxHandle) ->
    Module = get_callback(DnRev),
    try apply(Module, deleteMo, [DnRev, TxHandle]) of
	Result -> 
	    Result
    catch T:E ->
	    erlang:T(E, [DnRev, TxHandle])
    end.

action(DnRev, Parameters, TxHandle) ->
    Module = get_callback(DnRev),
    try apply(Module, action, [DnRev, Parameters, TxHandle]) of
	Result -> 
	    Result
    catch T:E ->
	    erlang:T(E, [DnRev, Parameters, TxHandle])
    end.
    



validate(DnRev, User, Tx) ->
    Module = get_callback((DnRev)),
    try apply(Module, validate, [DnRev, User, Tx]) of
	Result ->
	    Result
    catch T:E ->
	    erlang:T(E, [DnRev, User, Tx])
    end.

prepare(DnRev, User, Tx) ->
    Module = get_callback((DnRev)),
    try apply(Module, prepare, [DnRev, User, Tx]) of
	Result -> 
	    Result
    catch T:E ->
	    erlang:T(E, [DnRev, User, Tx])
    end.

commit(DnRev, User, Tx) ->
    Module = get_callback((DnRev)),
    try apply(Module, commit, [DnRev, User, Tx]) of
	Result -> 
	    Result
    catch T:E ->
	    erlang:T(E, [DnRev, User, Tx])
    end.

finish(DnRev, User, Tx) ->
    Module = get_callback((DnRev)),
    try apply(Module, finish, [DnRev, User, Tx]) of
	Result -> 
	    Result
    catch T:E ->
	    erlang:T(E, [DnRev, User, Tx])
    end.

%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% some_method(Parameter)->
%%    nn.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% internal_function1(One, Two)->
%%    nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

