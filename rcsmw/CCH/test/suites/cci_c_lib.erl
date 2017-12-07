-module(cci_c_lib).
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% R10A/2   2017-06-13  eivmiha     Created
%%% R10A/4   2017-06-19  evadumb     Added timer:sleep
%%% R10A/5   2017-06-30  ekurnik     Included cloudish env into is_target()
%%% R10A/6   2017-06-29  evadumb     Added configurable timeout
%%% ----------------------------------------------------------


-export ([get_ntp_element/1,
          get_ntp_element/2,
          set_ntp_element/2,
          set_ntp_element/3,
          create_ntp_server_mo/4,
          create_ntp_server_mo/5,
          delete_ntp_server_mo/1,
          delete_ntp_server_mo/2,
          is_target/0]).


-define(COMTOP,  {xmlns, "urn:com:ericsson:ecim:ComTop"}). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MO functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_ntp_element(Element) ->
    MeId = "1",
    Get = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],[MeId]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
                           [{sysMId,[],["1"]},
                    {'NtpServer', [], [{ntpServerId,[],["1"]},
                                       {Element, []}]}]}]}]},
    
    case get_mo(Get) of
        {ok, Result} ->
            {ok, {Element, _, [ElementValue]}} = extract_element(Element, Result),
            ElementValue;
        {error, Error} ->
            ct:fail("Trying to get nonexisting element! ~p", [Error])
    end.

get_ntp_element(Element, ID) ->
    MeId = "1",
    Get = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],[MeId]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
                           [{sysMId,[],["1"]},
                    {'NtpServer', [], [{ntpServerId,[],[ID]},
                                       {Element, []}]}]}]}]},
    
    case get_mo(Get) of
        {ok, Result} ->
            {ok, {Element, _, [ElementValue]}} = extract_element(Element, Result),
            ElementValue;
        {error, Error} ->
            ct:fail("Trying to get nonexisting element! ~p", [Error])
    end.


set_ntp_element(Element, ElementValue) ->
    set_ntp_element(Element, ElementValue, 10000).
set_ntp_element(Element, ElementValue, Timeout) ->
    MeId = "1",
    Edit = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],[MeId]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
                           [{sysMId,[],["1"]},
                    {'NtpServer', [], [{ntpServerId,[],["1"]},
                                       {Element, [],[ElementValue]}]}]}]}]},
    
    case set_mo(Edit) of
        ok ->
            timer:sleep(Timeout),
            ok;
    Error ->
        ct:fail(Error)
    end.


create_ntp_server_mo(AdministrativeState, NtpServerId, ServerAddress, UserLabel) ->
    create_ntp_server_mo(AdministrativeState, NtpServerId, ServerAddress, UserLabel, 10000).
create_ntp_server_mo(AdministrativeState, NtpServerId, ServerAddress, UserLabel, Timeout) ->
    MeId = "1",
    Edit = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],[MeId]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
                           [{sysMId,[],["1"]},
                    {'NtpServer', [{ntpServerId,[NtpServerId]},
                                   {administrativeState, [AdministrativeState]},
                                   {serverAddress, [ServerAddress]},
                                   {userLabel, [UserLabel]}]}]}]}]},

    case set_mo(Edit) of
        ok ->
            timer:sleep(Timeout),
            ok;
    Error ->
        ct:fail(Error)
    end.   


delete_ntp_server_mo(NtpServerId) ->
    delete_ntp_server_mo(NtpServerId, 10000).
delete_ntp_server_mo(NtpServerId, Timeout) ->
    MeId = "1",
    Edit = {'ManagedElement', [?COMTOP, {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                              [{managedElementId,[],[MeId]},
                {'SystemFunctions', [{systemFunctionsId,[],["1"]},
                  {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
                           [{sysMId,[],["1"]},
                    {'NtpServer', [{'xc:operation', "delete"}], [{ntpServerId,[],[NtpServerId]}]}]}]}]},
    
    case set_mo(Edit) of
        ok ->
            timer:sleep(Timeout),
            ok;
    Error ->
        ct:fail(Error)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_target() ->
    case os:getenv("SIM_OR_TARGET") of
    "sim" ->
        false;
    _Target -> %% can be target or cloudish
        true 
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NETCONF get/set/action functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_mo(Get :: tuple()) -> {ok, tuple()} | {error, term()}.
get_mo(Get) ->
    get_mo(nc1, Get).
get_mo(NC, Get) ->
    netconf(NC, get, [NC, Get]).

-spec set_mo(Edit :: tuple()) -> ok | {error, term()}.
set_mo(Edit) ->
    set_mo(nc1, Edit).
set_mo(NC, Edit) ->
        case netconf(NC, edit_config, [NC, running, Edit]) of
            ok ->
                timer:sleep(1000),
                ok;
            {error, Reason} ->
                {error, Reason};
            Other ->
                {error, Other}
        end.

%% netconf/3
%% ====================================================================
%% @doc Wrapper for netconf command

-spec netconf(NC :: atom(), F :: string(), A :: list()) -> string().
%% ====================================================================
netconf(NC, F, A) ->
    Result = ct_netconfc:open(NC, []),
    case Result of 
        {ok, _} ->  Res = apply(ct_netconfc, F, A),
                    Status = case Res of
                                {error, _} ->
                                    Res;
                                _ ->
                                    ct_netconfc:close_session(NC)
                             end,
    %% Result of negative edit requests are seen first when closing session.
                    case Status of
                        ok ->
                             Res;
                        _ ->
                             ct_netconfc:close_session(NC),
                             Status
                    end;
        Other -> ct:pal("netconf command failed, reason ~p", [Other])
    end.

%% extract_element/2
%% ====================================================================
%% @doc From a general short xml syntax, extract a certain xml
%%              element

-spec extract_element(Element :: atom(), List :: list()) -> {ok, tuple()}.
%% ====================================================================
extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
    {ok, Value} ->
        {ok, Value};
    not_found ->
        extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
    {ok, Value} ->
        {ok, Value};
    not_found ->
        extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.
