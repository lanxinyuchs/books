%%% ----------------------------------------------------------
%%% %CCaseFile:	cec.erl %
%%% Author:	etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(cec).
-vsn('/main/R2A/R4A/1').
-date('2015-11-05').
-author('erarafo').
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
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R1A/1      2012-10-03 etxbjca     Created
%%% R4A/1      2015-11-05 erarafo     EDoc, types
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% MODULE INTERFACE
%%% ----------------------------------------------------------
-export([register/2,
	 unregister/1,
	 lookup_registration/1,
	 get_info/0,
	 get_program_name/1]).

%%% ----------------------------------------------------------
%%% OTHER EXPORTED FUNCTIONS
%%% ----------------------------------------------------------


%%% ----------------------------------------------------------
%%% HEADER FILES AND MACRO DEFINITIONS
%%% ----------------------------------------------------------


%%% ----------------------------------------------------------
%%% TYPES AND RECORDS
%%% ----------------------------------------------------------

-type cec_signature() :: string() | binary().

%%% ----------------------------------------------------------
%%% FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Registers a CEC signature to a callback module.
%%% A callback module must implement cec_setup/1 which
%%% gets a socket as argument and must return the pid of
%%% the process that will handle messages incoming from the
%%% C side.
%%%
%%% The callback module is also highly recommended to
%%% implement cec_takeover/1, which gets the same socket and
%%% is expected to execute
%%% inet:setopts(Socket, [{active, once}]) in the process that
%%% cec_setup/1 returned.
%%%
%%% cec_takeover/1 gets called immediately after cec_setup/1
%%% has returned.
%%%
%%% The receiving Erlang process should execute
%%% inet:setopts(Socket, [{active, once}]) after each received
%%% message. Failing to do so will stop further messages from
%%% being received.
%%%
%%% If no more messages are expected and the next action is to
%%% close the socket then the
%%% inet:setopts(Socket, [{active, once}]) call is optional.
%%% @end
%%% ----------------------------------------------------------

-spec register(cec_signature(), module()) -> ok.

register(Signature, Module) when is_list(Signature);
				 is_binary(Signature),
				 is_atom(Module) ->
    cec_db:register(convert_signature(Signature), Module).

%%% ----------------------------------------------------------
%%% @doc Unregisters a CEC signature.
%%% @end
%%% ----------------------------------------------------------

-spec unregister(cec_signature()) -> ok.

unregister(Signature) when is_list(Signature);
			   is_binary(Signature) ->
    cec_db:unregister(convert_signature(Signature)).

%%% ----------------------------------------------------------
%%% @doc Returns the callback module registered to a CEC signature.
%%% @end
%%% ----------------------------------------------------------

-spec lookup_registration(cec_signature()) -> false | {true, module()}.

lookup_registration(Signature) when is_list(Signature);
				    is_binary(Signature) ->
    cec_service:lookup_registration(convert_signature(Signature)).

%%% ----------------------------------------------------------
%%% @doc Returns CEC related debug information.
%%% @end
%%% ----------------------------------------------------------

-spec get_info() -> [{Signature :: binary(),
		      Module :: module(),
		      exported | not_exported}].

get_info() ->
    cec_db:get_info().

%%% ----------------------------------------------------------
%%% @doc Returns the program name (argv[0]) as a string or the
%%% atom 'undefined' if the program name is not available.
%%% @end
%%% ----------------------------------------------------------

-spec get_program_name(pos_integer()) -> undefined | string().

get_program_name(LinuxPid) when is_integer(LinuxPid) ->
    Filename = ["/proc/", integer_to_list(LinuxPid), "/cmdline"],
    case file:open(Filename, [read, raw]) of
	{error, _} ->
	    undefined;
	{ok, IoDevice} ->
	    Res = case file:read(IoDevice, 1000) of
		      eof ->
			  undefined;
		      {error, _} ->
			  undefined;
		      {ok, Data} ->
			  lists:takewhile(fun(C) -> C/=0 end, Data)
		  end,
	    file:close(IoDevice),
	    Res
    end.

%%% ----------------------------------------------------------
%%% @doc Enforces the binary variant of the cec_signature()
%%% type.
%%% @end
%%% ----------------------------------------------------------
-spec convert_signature(cec_signature()) -> binary().

convert_signature(Signature) when is_binary(Signature) ->
    Signature;
convert_signature(Signature) ->
    list_to_binary(Signature).
