%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysSupervisor.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2015
%%% @version /main/R1A/R2A/R3A/R4A/2

%%% @doc ==Default supervisor==
%%% Standard supervisor start up callback module

-module(sysSupervisor).
-behaviour(supervisor).
-vsn('/main/R1A/R2A/R3A/R4A/2').
-date('2015-08-12').
-author('etxberb').
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
%%% -----      -------    --------    ------------------------
%%% R1A/1      2012-01-25 etxjotj     Created
%%% R4A/1      2015-05-18 etxpejn     Removed COMTE startup code from regular MPs
%%% R4A/2      2015-08-12 etxberb     Using clhI:mp_role/0 to find MP role.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% -type init([Module])->                                  %#
%%%     ok | error().                                       %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: This function will ask the application's init module
%%% for information on how to start the supervisor.  The default
%%% behaviour is that the supervisor tries to restart processes
%%% locally for ever, with no escalation.
%%% ----------------------------------------------------------

init([Module]) ->
    init([Module, []]);
init([Module, Included, StartType]) ->
    
    RestartStrategy = 
	try apply(Module, restart_strategy, []) of
	    {ok, R} -> R
	catch  _:_ -> {one_for_one, 5, 10}
	end,
    
    Children = 
	try apply(Module, children, []) of
	    {ok, C} -> C
	catch _:_ -> []
	end,

    IncludedMod = 
	case clhI:mp_role() of
	    core ->
		Included;
	    regular ->
		%% COMTE should not be started on a regular DU since COM
		%% will only be run on core MPs.
		lists:delete(comte, Included)
	end,
    
    IncludedSups = 
	[begin 
	     Id = list_to_atom(atom_to_list(App)++"Super"),

	     Path = filename:join([code:lib_dir(App), "ebin", 
				   atom_to_list(App)++".app"]),
	     {ok, [{application, App, Data}]} = file:consult(Path),
	     {StartModule, StartArgs} = proplists:get_value(mod, Data),
	     StartFunc = 
		 case StartModule of
		     sysApp ->
			 {StartModule, start, [StartType, [{appname, App}|
							   StartArgs]]};
		     _ ->
			 {StartModule, start, [StartType, StartArgs]}
		 end,
	     Restart = permanent,
	     Shutdown = 15000,
	     Type = supervisor,
	     Modules = [supervisor],
	     {Id, StartFunc, Restart, Shutdown, Type, Modules}
	 end||App<-IncludedMod],
    {ok, {RestartStrategy, Children++IncludedSups}}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------




%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

