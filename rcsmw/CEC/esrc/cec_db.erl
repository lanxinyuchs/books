%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% -----      -------    --------    ------------------------
%%% R1A/1      2012-10-03   etxbjca     Created
%% R4A/    ---------- -------  ------------------------------------------------
%% R4A/1   2015-07-07 etxberb  Changed mnesia:create_table to
%%                             clhI:mnesia_create_table.
%%% ----------------------------------------------------------
-module(cec_db).

-export([init/1,
	 register/2,
	 unregister/1,
	 get_info/0]).

init(DbNodes) ->
    {atomic, ok} =
	clhI:mnesia_create_table(cecRegistration, [{type, set},
						   {disc_copies, DbNodes}]),
    ok.

register(Signature, Module) ->
    ok = mnesia:dirty_write({cecRegistration, Signature, Module}).

unregister(Signature) ->
    ok = mnesia:dirty_delete({cecRegistration, Signature}).

get_info() ->
    Regs = ets:tab2list(cecRegistration),

    lists:map(fun({cecRegistration, Signature, Module}) ->
		      IsExported = erlang:function_exported(Module,
							    cec_setup, 1),
		      State =
			  case IsExported of
			      true  -> exported;
			      false -> not_exported
			  end,

		      {Signature, Module, State}
	      end, Regs).
