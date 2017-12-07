%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_coli.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R1A/R2A/R4A/R5A/4
%%%
%%% @doc ==Common Test hook for sending and receiving coli messages==
%%% 
%%% This module is intended to be used from a Common Test suite, where the module is called as a ct_hook at the beginning of each testcases.<br/>
%%% 
%%% Hook formats:
%%% ```{rct_coli, [{N, Name, IPType, Opts}]}'''
%%%    Hook short formats
%%% ```{rct_coli, Name}                           -> {rct_coli, [{1, Name, oam_auto, []}]}
%%%    {rct_coli, {Name, Opts}                    -> {rct_coli, [{1, Name, oam_auto, Opts}]}}
%%%    {rct_coli, [Name1, Name2]                  -> {rct_coli, [{1, Name1, oam_auto, []},{2, Name2, oam_auto, []}]}}
%%%    {rct_coli, [{Name1, Opts1},{Name2, Opts2}] -> {rct_coli, [{1, Name1, oam_auto, Opts1},{2, Name2, oam_auto, Opts2}]}}'''
%%%
%%% Argument description:
%%% ```N        = integer()                      Used to match card in stp.cfg file when running on target.
%%%                                              Not used in simuleted environment.
%%%    Name     = atom()                         Used as identifier
%%%    IPType   =                                Used in target env to specify which IP address coli uses,
%%%               oam_auto |                      Management IP address will be automatically selected depending on precence of -oamap_ipv4 or -oamap_ipv6 flags to rct_run.sh or
%%%                                              `{jenkins_config,[{oamap_ipv4, []}]}', `{jenkins_config,[{oamap_ipv6, []}]}' config parameters.
%%%               ssh_lmt_ipv4 |                 LMT IPv4 is used, read from stp.cfg file
%%%               ssh_TN_A_ipv4 |                TNA IPv4 is used, read from stp.cfg file
%%%               ssh_TN_A_ipv4_alt|             Alternateive TNA IPv4 is used, read from stp.cfg file
%%%               ssh_lmt_ipv6 |                 LMT IPv6 is used, read from stp.cfg file
%%%               ssh_TN_A_ipv6 |                TNA IPv6 is used, read from stp.cfg file
%%%               ssh_TN_A_ipv6_alt|             Alternateive TNA IPv6 is used, read from stp.cfg file
%%%                                              Not used in simulated environment
%%%    Opts     = [Option]    
%%%    Option   = [] | {user, User} | {password, Password} | Print | manual_connect | {connect_timeout, ConnectTimeout}
%%%    User     = string()                       default = "expert"
%%%    Password = string()                       default = "expert"
%%%    Print    = print | noprint                default = print, print/noprint sent and received messages in html log
%%%                                              This can be overridden with Print argument in connect, send and disconnect
%%%    manual_connect                            No automatic connection to COLI at start of testcase, i.e. use rct_coli:connect.
%%%    ConnectTimeout                            default = 20 seconds, Intended for TLM'''
%%%
%%% Before each suite `pre_init_per_suite' will:<br/>
%%% - Verify existance of config parameter `{rct_coli, [{N, Name, Sname, IPType, Opts}]}'.<br/>
%%% Before each testcase `pre_init_per_testcase' will:<br/>
%%% - Connect to SUT if not Option = manual_connect.<br/>
%%% after each testcase `post_end_per_testcase' will:<br/>
%%% - Disconnect from SUT if not Option = manual_connect.<br/>
%%%
%%% If `rcstprep.sh' script (wrapped by `rcs_install.sh' script) has been run with `-oamap_ipv4' or `-oamap_ipv6' flag, OaM Accesspoint has been configured with either ipv4 or ipv6 address during installation.
%%%
%%% If `IPType=oam_auto' (default) is set in the testsuite, the management IP address will be automatically selected depending on precense of `-oamap_ipv4' or `-oamap_ipv6' as arguments to `rct_run.sh' or `{jenkins_config,[{oamap_ipv4, []}]}' or `{jenkins_config,[{oamap_ipv6, []}]}' as config parameters, Examples:
%%%
%%% ```rct_run.sh -stp dus5000                  will run coli commands over ssh_lmt_ipv4
%%%    rct_run.sh -stp dus5000 -oamap_ipv4      will run coli commands over ssh_TN_A_ipv4
%%%    rct_run.sh -stp dus5000 -oamap_ipv6      will run coli commands over ssh_TN_A_ipv6'''
%%%
%%% `-oamap_ipv4' and `-oamap_ipv6' has precedence over `{jenkins_config,[{oamap_ipv4, []}]}' and `{jenkins_config,[{oamap_ipv6, []}]}', i.e. the config parameters will only be checked if the arguments are not given to `rct_run.sh'.
%%% 
%%% Examples single node:
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_coli, coli}]}].'''
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_coli, {cloi_1,[manual_connect]}}]}].'''
%%% Examples clustered node:
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_coli, [coli_1, coli2]}]}].'''
%%%
%%% Testcase example.
%%% Send a coli cmd.
%%% ```coli_ex(_) ->
%%%        rct_coli:send(coli_1, "configure").'''
%%%
%%% Send a coli cmd and match a expected string in the received data.
%%% ```coli_ex(_) ->
%%%        rct_coli:send(coli_1, "configure", "\\(config\\)>").'''
%%%
%%% Send a coli cmd and get the recieved data.
%%% ```coli_ex(_) ->
%%%        {ok, {RecData, Match}} = rct_coli:send(coli_1, "configure", "\\(config\\)>").'''
%%%
%%% @end
-module(rct_coli). 
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R4A/R5A/4').
-date('2016-02-25').
-author('etxkols').
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
%% R1A/1      2012-08-14  etxivri    Created
%% R1A/2      2012-10-05  etxivri    Minor change in get_coli_port when using sim env.
%% R1A/3      2012-10-17  etxkols    Major changes to fit into Jenkins short_test.
%% R2A/2      2012-10-19  etxkols    Use rct_cli_coli_common
%% R2A/3      2013-09-04  etxkols    Fixed new coli prompt
%% R2A/4      2014-03-26  etxkols    Faulty init/2 return value
%% R2A/6      2014-04-23  etxkols    Added connect_timeout for TLM
%% R2A/7      2015-01-22  etxkols    Default prompt has changed
%% R4A/1      2015-05-28  etxkols    Fixed cluster
%% R4A/2      2015-12-03  etxkols    oam_auto default IPType, will select OAMAP or LMT
%% R5A/1      2016-02-02  etxkols    ipv6
%% R5A/2      2016-02-10  etxkols    Documentation
%% R5A/3      2016-02-10  etxkols    Documentation
%% R5A/4      2016-02-25  etxkols    Added send/5
%% -----------------------------------------------------------------------------
-export([send/2]).
-export([send/3]).
-export([send/4]).
-export([send/5]).
-export([connect/1]).
-export([connect/2]).
-export([connect/3]).
-export([connect/4]).
-export([connect/5]).
-export([change_prompt/2]).
-export([change_prompt/3]).
-export([disconnect/1]).
-export([disconnect/2]).

-export([init/2]).
-export([pre_init_per_suite/3]).
-export([pre_init_per_testcase/3]).
-export([post_end_per_suite/4]).
-export([post_end_per_testcase/4]).
-export([terminate/1]).

-import(ct_telnet,[start_log/1,cont_log/2,end_log/0]).
-include_lib("common_test/include/ct.hrl").

-define(DEFAULT_USER, "expert").
-define(DEFAULT_PASSWORD, "expert").
-define(TARGET_PORT, 4192).
%-define(DEFAULT_PROMPT, "coli>$").
-define(DEFAULT_PROMPT, "coli \\[.*\\]-> $").
-define(CLI_OR_COLI, coli).
%% @hidden
init(_Id, Opts) ->
    {ok,Opts}.

%% @spec pre_init_per_suite(Suite,Config,CthState) -> {Config, CthState} | {{fail,Reason}, CthStates}| {{skip,Reason}, CthStates}
%% @doc Verifies CT config parameters.<br/>
pre_init_per_suite(Suite,Config,Name) ->
    rct_cli_coli_common:pre_init_per_suite(Suite,Config,Name,{?CLI_OR_COLI,?DEFAULT_USER,?DEFAULT_PASSWORD,?TARGET_PORT,?DEFAULT_PROMPT}).

%% @hidden
post_end_per_suite(Suite,Config, Return, CthState) ->
    rct_cli_coli_common:post_end_per_suite(Suite,Config, Return, CthState).

%% @spec pre_init_per_testcase(TC,Config,CthStates) -> {Config, CthStates} | {{fail,Reason}, CthStates}
%% @doc Creates ets table for connections and connects to SUT if specified in ct_hook.<br/>
pre_init_per_testcase(TC,Config, CthStates) ->
    rct_cli_coli_common:pre_init_per_testcase(TC,Config, CthStates,?CLI_OR_COLI).

%% @spec post_end_per_testcase(TC,Config,Return,States) -> {Return, States} | {{fail,Reason}, States}
%% @doc Disconnects ssh coli port from SUT.<br/>
post_end_per_testcase(TC,Config,Return,States) ->
    rct_cli_coli_common:post_end_per_testcase(TC,Config,Return,States,?CLI_OR_COLI).

%% @spec terminate(States) -> ok
%% @doc terminates ssh coli hook<br/>
terminate(States) ->
    rct_cli_coli_common:terminate(States).

%% @doc see connect/4.
connect(Name) ->
    rct_cli_coli_common:connect(Name).

%% @doc see connect/4.
connect(Name, Print) ->
    rct_cli_coli_common:connect(Name, Print).

%% @doc see connect/4.
connect(Name, User, Password) ->
    rct_cli_coli_common:connect(Name, User, Password).

connect(Name, User, Password, Print) ->
    rct_cli_coli_common:connect(Name, User, Password, Print).

%% @spec connect(Name, User, Password, Prompt, Print) -> ok | {error, Reason}
%% @doc COLI connects to SUT with User/Password, Print overrides ct_hook setting.
%% ```Name = atom()                          Used as identifier.
%%    User = string()                        Username, default = "expert".
%%    Password = string()                    Password, default = "expert".
%%    Prompt = string()                      default = "coli>$"
%%    Print = print | noprint                sent commands and replies are printed in html. Overrides ct_hook setting.
%%    Reason = term()
%%    Short versions of connect
%%    connect(Name).                         ct_hook print option is used
%%    connect(Name, Print).                  print or noprint overrides ct_hook setting
%%    connect(Name, User, Password).         User/Password overrides default. ct_hook print option is used
%%    connect(Name, User, Password, Print).  print or noprint overrides ct_hook setting
%%    connect(Name, User, Password, Prompt). own definition of prompt. ct_hook print option is used'''
connect(Name, User, Password, Prompt, Print) ->
    rct_cli_coli_common:connect(Name, User, Password, Prompt, Print).

%% @doc see send/4.
send(Name, Cmd) ->
    rct_cli_coli_common:send(Name, Cmd).
%% @doc see send/4.
send(Name, Cmd, Print) when is_atom(Print) ->
    rct_cli_coli_common:send(Name, Cmd, Print);
%% @doc see send/4.
send(Name, Cmd, Match) when is_list(Match);
			    is_tuple(Match)->    
    rct_cli_coli_common:send(Name, Cmd, Match).
%% @doc see send/5.
send(Name, Cmd, Match, Print) ->    
    rct_cli_coli_common:send(Name, Cmd, Match, Print).

%% @spec send(Name, Cmd, Match, Print, Timeout) -> {ok, {Reply, Matched}} | {error, Reason}
%% @doc Runs COLI command and match result, Print overrides ct_hook setting.
%%
%% For description of Match, please see <a href="http://www.erlang.org/doc/man/re.html#run-3">re:run/3</a>.
%% ```Name = atom()              Used as identifier.
%%    Cmd = string()             coli command you want to send.
%%    Match = RE | {RE, Options} See OTP re:run/3 documentation.
%%    RE = string()              string you want to match from the AccData.
%%    Options = list()           default = [dotall,{capture, all, list}].
%%    Print = print | noprint    sent commands and replies are printed in html. Overrides ct_hook setting.
%%    Timeout = integer()        Sseconds to wait for prompt, default 5 Seconds.
%%    Reply = term()
%%    Matched = term()           Result from re:run/3
%%    Reason = term()
%%    Short versions of send
%%    send(Name, Cmd).           Does no check on reply, ct_hook print option is used
%%    send(Name, Cmd, Print).    Does no check on reply, print or noprint
%%    send(Name, Cmd, Check).    Checks reply, ct_hook print option is used'''
send(Name, Cmd, Match, Print, Timeout) ->
    rct_cli_coli_common:send(Name, Cmd, Match, Print, Timeout * 1000).

%% @doc see change_prompt/2.
change_prompt(Name, Prompt) ->
    rct_cli_coli_common:change_prompt(Name, Prompt).

%% @spec change_prompt(Name, Prompt, Print) -> ok | {error, Reason}
%% @doc Change expected prompt.
%%
%% For description of Prompt, please see <a href="http://www.erlang.org/doc/man/re.html#run-3">re:run/3</a>.
%% ```Name = atom()                Used as identifier.
%%    Prompt = string()            See argument RE in re:run/3.
%%    Print = print | noprint      sent commands and replies are printed in html. Overrides ct_hook setting.
%%    Reason = term()
%%    Short version of change_prompt
%%    change_prompt(Name, Prompt). Changes prompt, ct_hook print option is used'''
change_prompt(Name, Prompt, Print) ->
    rct_cli_coli_common:change_prompt(Name, Prompt, Print).

%% @doc see disconnect/2.
disconnect(Name) ->
    rct_cli_coli_common:disconnect(Name).

%% @spec disconnect(Name,Print) -> ok | {error, Reason}
%% @doc Disconnects from SUT, Print overrides ct_hook setting.
%% ```Name = atom()              Used as identifier.
%%    Print = print | noprint    sent commands and replies are printed in html. Overrides ct_hook setting.
%%    Reason = term()
%%    Short versions of send
%%    disconnect(Name).          Disconnects from SUT, ct_hook print option is used'''
disconnect(Name,Print) ->
    rct_cli_coli_common:disconnect(Name,Print).
