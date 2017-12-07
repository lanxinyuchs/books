%% -----------------------------------------------------------------------------
%% Copyright (c) Ericsson AB 2012 All rights reserved.
%%
%% The information in this document is the property of Ericsson.
%%
%% Except as specifically authorized in writing by Ericsson, the receiver of
%% this document shall keep the information contained herein confidential and
%% shall protect the same in whole or in part from disclosure and dissemination
%% to third parties.
%%
%% Disclosure and disseminations to the receivers employees shall only be made
%% on a strict need to know basis.
%% -----------------------------------------------------------------------------
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2012-01-31  etxkols    Created
%%% R1A/2      2012-03-23  etxkols    Changed timeout to 30000
%%% R1A/3      2012-03-23  etxkols    Removed change in R1A/2
%%% R1A/4      2012-03-24  etxkols    If timeout, retry 5 times 1 sec delay
%%%                                   because of new cons serv behavior
%%% R1A/5      2012-05-23  etxkols    Reintroduced timeout of 30000 ms on ct_tel
%%% R1A/6      2012-05-23  etxkols    Silly bug
%%% R1A/7      2012-06-04  etxkols    Added 500 ms sleep btwn login and sending username
-module(rct_telnet_common).
-export([login/6]).
-define(TIMEOUT, 30000).

login(Name,Loginprompt,Username,Passwdprompt,Password,Userprompt) ->
    login(Name,Loginprompt,Username,Passwdprompt,Password,Userprompt,5).

login(_Name,_Loginprompt,_Username,_Passwdprompt,_Password,_Userprompt,0) ->
    {error, timeout};
login(Name,Loginprompt,Username,Passwdprompt,Password,Userprompt,N) ->
    case ct_telnet:send(Name,"\n") of
	ok ->
	    case ct_telnet:expect(Name,[{loginprompt,Loginprompt},{passwdprompt,Passwdprompt},{userprompt,Userprompt}],[{timeout,?TIMEOUT}]) of
		{ok,{userprompt,_}} ->
		    ok;
		{ok,{passwdprompt,_}} ->
		    case ct_telnet:send(Name,Password) of
			ok ->
			    case ct_telnet:expect(Name,[{userprompt,Userprompt}],[{timeout,?TIMEOUT}]) of
				{ok,{userprompt,_}} ->
				    ok;
				{error,Reason} ->
				    {error,Reason}
			    end;
			{error,Reason} ->
			    {error,Reason}
		    end;
		{ok,{loginprompt,_}} ->
		    timer:sleep(500),
		    case ct_telnet:send(Name,Username) of
			ok ->
			    case ct_telnet:expect(Name,[{passwdprompt,Passwdprompt}],[{timeout,?TIMEOUT}]) of
				{ok,{passwdprompt,_}} ->
				    case ct_telnet:send(Name,Password) of
					ok ->
					    case ct_telnet:expect(Name,[{userprompt,Userprompt}],[{timeout,?TIMEOUT}]) of
						{ok,{userprompt,_}} ->
						    ok;
						{error,Reason} ->
						    {error,Reason}
					    end;
					{error,Reason} ->
					    {error,Reason}
				    end;
				{error,Reason} ->
				    {error,Reason}
			    end;
			{error,Reason} ->
			    {error,Reason}
		    end;
		{error,timeout} ->
		    timer:sleep(1000),
		    login(Name,Loginprompt,Username,Passwdprompt,Password,Userprompt,N-1);
		{error,Reason} ->
		    {error,Reason}
	    end;
	{error,Reason} ->
	    {error,Reason}
    end.
