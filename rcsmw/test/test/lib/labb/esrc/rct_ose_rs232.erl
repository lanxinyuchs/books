%% -----------------------------------------------------------------------------
%% Copyright (c) Ericsson AB 2014 All rights reserved.
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
%% ----------------------------------------------------------
%% #1.    REVISION LOG
%% ----------------------------------------------------------
%% Rev        Date       Name        What
%% -----      ---------  --------    ------------------------
%% R2A/1      2012-12-07  etxkols    Created
%% R2A/4      2014-03-19  etxkols    Adaption to OTP R17B01
%% -----------------------------------------------------------------------------
%% @doc Callback module from rct_rs232.erl to support ose prompts and login.
%% @end
-module(rct_ose_rs232). 
-export([get_prompt_regexp/0]).
-export([get_login_data/1]).
-export([connect/5]).
-export([connect/6]).

-define(loginprompt,".*username: $").
-define(passwdprompt,".*password: $").
-define(rootprompt,".*\\\$ $").
-define(prx,?loginprompt ++ "|" ++ ?passwdprompt ++ "|" ++ ?rootprompt).

%% @hidden
%% Mandatory ct_telnet callback function.
get_prompt_regexp() ->
    ?prx.

%% @spec get_login_data(Name) -> list() | {error, Reason}
%% Name = atom()
%% @doc Supplies rct_rs232.erl with login data for OSE.
get_login_data(Name) ->
    Username = ct:get_config({Name,username}),
    Password = ct:get_config({Name,password}),
    {?loginprompt,Username,?passwdprompt,Password,?rootprompt}.

%% @hidden
%% R17B01 Mandatory ct_telnet callback function.
connect(ConnName,Ip,Port,Timeout,KeepAlive,_Name) ->
    ct_telnet_client:open(Ip,Port,Timeout,KeepAlive,ConnName).

%% @hidden
%% Mandatory ct_telnet callback function.
connect(Ip,Port,Timeout,KeepAlive,_Name) ->
    ct_telnet_client:open(Ip,Port,Timeout,KeepAlive).