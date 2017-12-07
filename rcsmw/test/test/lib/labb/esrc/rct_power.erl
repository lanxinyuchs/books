%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_power.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R1A/R2A/R4A/2
%%% @doc ==Module to power off/on SUT via PDU in lab==
%%% 
%%% Protocol part in this module is stolen with pride from Jan Holmen.
%%% This module implements Svift protocol, see documents in Prim 1551-BMG980347, 
%%% 1551-BMG980348/2 and http://www.telamp.se/tekn/svift/docs/telamp03_104_e.pdf.
%%% 
%%% This module is intended to be used from a Common Test suite, where the module is called as a ct_hook at the beginning of each test suite.<br/>
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_power,Name = atom() | [{No = integer(), Name = atom()}]}]}.'''
%%% 
%%% There is a short format for testing towards one node:
%%% ```{rct_power, Name}  expands to {rct_power, [{1, Name}]}'''
%%% 
%%% There is a short format for testing towards clustered node:
%%% ```{rct_power, [Name1,Name2]} expands to {rct_power, [{1, Name1},{2, Name2}]}'''
%%% 
%%% Argument description:
%%% ```N        = integer()                      Used to match card in stp.cfg file when running on target.
%%%                                              Not used in simuleted environment.
%%%    Name     = atom()                         Used as identifier'''
%%% For usage of Name, see <a href="http://www.erlang.org/doc/man/ct_telnet.html">ct_telnet</a><br/>
%%% 
%%% Example single node:
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_power,node}]}].'''
%%% Example clustered nodes:
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_power,[node1,node2]}]}].'''
%%% Testcase example:
%%% ```testcase(_) ->
%%%        off = rct_power:off(node1)'''
%%% @end
-module(rct_power). 
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R4A/2').
-date('2016-03-08').
-author('etxkols').
-include_lib("common_test/include/ct.hrl").

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
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
%%% R1A/1      2012-01-31  etxkols    Created
%%% R1A/2      2012-03-23  etxkols    Completely changed for new PDU
%%% R1A/3      2012-04-20  etxkols    Timer adaptions to new console server
%%% R1A/4      2012-04-23  etxkols    Change return values for off and on to ok
%%%                                   to be backwards compatible
%%% R1A/5      2012-04-24  etxkols    Changed gen_tcp:connect timeout from 1 to 5 sec
%%% R1A/6      2012-05-08  etxkols    Extended check on open_tcp in func power
%%% R1A/7      2012-05-08  etxkols    Removed debug printouts
%%% R1A/8      2012-05-14  etxkols    Adaptions to crap bus btwn proc and outlet in PDU
%%% R1A/9      2012-05-30  etxkols    Retry once when crappy reply from PDU
%%% R1A/10     2012-06-19  etxkols    Changed error value from decode
%%% R2A/1      2012-10-24  etxkols    Handle simulated env 
%%% R2A/3      2012-11-13  etxkols    Added option for not sending action 3 times 
%%% R2A/3      2012-11-13  etxkols    Fixed option for not sending action 3 times
%%% R2A/4      2012-11-15  etxkols    Removed red printouts for Opt = no_retries
%%% R2A/5      2013-10-17  etxkols    More retries because of unreliable PDU
%%% R2A/6      2013-10-17  etxkols    Increased timer
%%% R2A/8      2014-03-26  etxkols    Faulty init/2 return value
%%% R4A/1      2015-06-02  etxkols    Cluster fixes
%%% R4A/2      2016-03-08  etxkols    5G
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([init/2]).
-export([pre_init_per_suite/3]).
-export([terminate/1]).
-export([on/1,on/2]).
-export([off/1,off/2]).
-export([cycle/1,cycle/2]).
-export([read/1,read/2]).
-export([init_tcp/8]).

%-define(TCP_TMO, 1500).      % Timeout in millisec waiting for data on tcp connection.
-define(TCP_TMO, 2000).      % Timeout in millisec waiting for data on tcp connection.
-define(NOF_TCP_RECONN, 20). % Number of retries when tcp connection is busy
-define(TIME_RECONN, 5).     % Time in sec between retries when tcp connection is busy

%% @spec off(Name) -> ok | {error, Reason}
%% Name = atom()
%% @doc power off PDU port to SUT.
off(Name) ->
    off(Name, retry).

%% @spec off(Name, no_retries) -> ok | {error, Reason}
%% Name = atom()
%% @doc power off PDU port to SUT, no retries because of unrobustness in PDU are made.
off(Name, Opt) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    ct:log(lightred,"~p: Power off NOK, Reason NOT supported in SIMULATED environment",[Name]);
        "cloudish" ->
	    ct:log(lightred,"~p: Power off NOK, Reason NOT supported in CLOUD environment",[Name]);
	"target" ->
	    case power(Name, off, Opt) of
		off ->
		    ct:log(internal,"~p: Power off OK",[Name]),
		    ok;
		Other ->
		    case Opt of 
			retry -> 
			    ct:log(lightred,"~p: Power off NOK, Reason ~p",[Name, Other]);
			no_retries -> 
			    ct:log(white,"~p: Power off NOK, Reason ~p",[Name, Other])
		    end,
		    Other
	    end
    end.


%% @spec on(Name) -> ok | {error, Reason}
%% Name = atom()
%% @doc power on PDU port to SUT.
on(Name) ->
    on(Name, retry).

%% @spec on(Name, no_retries) -> ok | {error, Reason}
%% Name = atom()
%% @doc power on PDU port to SUT, no retries because of unrobustness in PDU are made.
on(Name, Opt) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    ct:log(lightred,"~p: Power on NOK, Reason NOT supported in SIMULATED environment",[Name]);
        "cloudish" ->
	    ct:log(lightred,"~p: Power on NOK, Reason NOT supported in CLOUD environment",[Name]);
	"target" ->
	    case power(Name, on, Opt) of
		on ->
		    ct:log(internal,"~p: Power on OK",[Name]),
		    ok;
		Other ->
		    case Opt of 
			retry -> 
			    ct:log(lightred,"~p: Power on NOK, Reason ~p",[Name, Other]);
			no_retries -> 
			    ct:log(white,"~p: Power on NOK, Reason ~p",[Name, Other])
		    end,
		    Other
	    end
    end.
    
%% @spec cycle(Name) -> ok | {error, Reason}
%% Name = atom()
%% @doc power cycle (off/on) PDU port to SUT.
cycle(Name) ->
    cycle(Name, retry).

%% @spec cycle(Name, no_retries) -> ok | {error, Reason}
%% Name = atom()
%% @doc power cycle (off/on) PDU port to SUT, no retries because of unrobustness in PDU are made.
cycle(Name, Opt) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    ct:log(lightred,"~p: Power cycle NOK, Reason NOT supported in SIMULATED environment",[Name]);
        "cloudish" ->
	    ct:log(lightred,"~p: Power cycle NOK, Reason NOT supported in CLOUD environment",[Name]);
	"target" ->
	    case off(Name, Opt) of
		ok ->
		    timer:sleep(4000),
		    on(Name, Opt);
		Other ->
		    Other
	    end
    end.

%% @spec read(Name) -> on | off | {error, Reason}
%% Name = atom()
%% @doc Read PDU port state for SUT.
read(Name) ->
    read(Name, retry).

%% @spec read(Name, no_retries) -> on | off | {error, Reason}
%% Name = atom()
%% @doc Read PDU port state for SUT, no retries because of unrobustness in PDU are made.
read(Name, Opt) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    ct:log(lightred,"~p: Power read NOK, Reason NOT supported in SIMULATED environment",[Name]);
        "cloudish" ->
	    ct:log(lightred,"~p: Power read NOK, Reason NOT supported in CLOUD environment",[Name]);
	"target" ->
	    case power(Name, read, Opt) of
		Read when is_atom(Read) ->
		    ct:log(internal,"~p: Power read ~p",[Name, Read]);
		Read ->
		    case Opt of 
			retry -> 
			    ct:log(lightred,"~p: Power read NOK, Reason ~p",[Name, Read]);
			no_retries -> 
			    ct:log(white,"~p: Power read NOK, Reason ~p",[Name, Read])
		    end
	    end,
	    Read
    end.
    
%% The crappy PDU unit has a bus between processor unit and outlets.
%% When ordering power off/on/read, the processor unit replies ok,
%% but since the bus is not reliable, the outlet may not power off/on.
%% The easiest way to overcome this is to order 4 power off/on, this
%% should hopefully reduce the failed actions from 1/50 to 1/125000.
power(Name,OP, Opt) ->
    power(Name,OP, Opt,1).

power(Name,Op, Opt,N) ->
    [{power_cons_ip,Power_cons_ip},
     {power_cons_port,Power_cons_port},
     {power_port,Power_port}] = ct:get_config(Name),
    case open_tcp(Name, Power_cons_ip, Power_cons_port, Op, Opt, 10000) of
	{ok,Pid} ->
	    case get_data_tcp(Pid) of % Flush socket for incoming messages
		{ok,_} ->
		    Code_data=case Op of 
				  on->    {1,1};
				  off->   {1,0};		 
				  read -> {0,[]}
			      end,	
		    Msg=msg(1, list_to_integer(Power_port), Code_data),
		    S=frame(Msg),
		    Reply = case Opt of 
				retry -> % will send Op 3 times because of glitch in PDU
				    send_tcp(Pid, S), % first order of action
				    get_data_tcp(Pid),
				    send_tcp(Pid, S), % second order of action
				    get_data_tcp(Pid),
				    send_tcp(Pid, S), % third order of action
				    get_data_tcp(Pid),
				    send_tcp(Pid, S), % fourth order of action
				    {ok, Resp} = get_data_tcp(Pid),
				    case decode(Resp) of
					% Have seen once that checksum in reply from stupid PDU has been wrong
					% This is an attempt to resend the order once			    
					{error, _Reason} ->
					    send_tcp(Pid, S), % third order of action
					    {ok, Resp2} = get_data_tcp(Pid),
					    decode(Resp2);
					Other ->
					    Other
				    end;
				no_retries -> % gives the user the resposibility of retrying
				    send_tcp(Pid, S), 
				    {ok, Resp} = get_data_tcp(Pid),
				    decode(Resp)
			    end,
		    close_tcp(Pid),
		    Reply;
		{error, Reason} when Reason == tcp_port_unexpectedly_closed;
				     Reason == timeout ->
		    case Opt of 
			retry ->
			     % 1) If 2 tcp:connect are made with very short intervals towards 
		             %    console server, the first connection may occationally be closed.
		             %    This happen ~1 in 1000 when running towards 2 outlets in parallell
		             % 2) gen_tcp:connect may ~1 in 1000 timeout whitout apparent reason.
		             % This is an attempt to reestablish the connection once after ?TIME_RECONN
			    case N of
				0 -> 
				    ct:log(lightred,"~p: ~p TCP connection to consoleserver unexpecedly died while power ~p. No more retries are made",
					   [Name, ?MODULE, Op]),
				    {error, Reason};
				1 ->
				    ct:log(internal,"~p: ~p TCP connection to consoleserver unexpecedly died while power ~p. Will retry in ~p seconds",
					   [Name, ?MODULE, Op, ?TIME_RECONN]),
				    timer:sleep(?TIME_RECONN * 1000),
				    power(Name,Op, Opt,N-1)
			    end;
			no_retries -> 
			    {error, Reason}
		    end;
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%% @hidden
init(_Id, Opts) ->
    {ok,Opts}.

%% @hidden
%% @spec pre_init_per_suite(Suite, Config, States) -> {Config, States} | {{fail,Reason}, States}
%% @doc Common test ct_hook which runs before each test suite.<br/>
%% Verifies existance of config parameter `{rct_power,[telnet,port,power_port]}'.<br/>
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, CthState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CthState};
pre_init_per_suite(Suite,Config,States) when is_atom(States) ->
    pre_init_per_suite(Suite,Config,[States]);
pre_init_per_suite(_Suite,Config,States) ->
    SuiteType = case length(States) > 1 of
    		    true  -> clustered;
    		    false -> single
    		end,
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    {Config, States};
        "cloudish" ->
	    {Config, States};
	"target" ->
	    States2 = do_pre_init_per_suite(States,[],SuiteType,1),
	    {Config,States2}
    end.

do_pre_init_per_suite([],R,_SuiteType,_Num) ->
    R;
do_pre_init_per_suite([Name|T],R,SuiteType,Num) when is_atom(Name) ->
    do_pre_init_per_suite([{Num,Name}|T],R,SuiteType,Num);
do_pre_init_per_suite([{N,Name}|T],R,SuiteType,Num) ->
    BoardNum = rct_cluster_lib:get_target_du(N,SuiteType), 
    Board = ct:get_config({test_nodes,BoardNum}),
    case ct:get_config({Board,power}) of
	Power = [{power_cons_ip,_},
		 {power_cons_port,_},
		 {power_port,_}] ->
	    rct_multi_node_cfg:require(Name,Power),
	    do_pre_init_per_suite(T,R ++ [Name],SuiteType,Num + 1 );
	Other ->
            ct:log(lightred,"~p: ~p ~p Config parameters not matching {power,[power_cons_ip,power_cons_port,power_port]} for power unit control, Reason: ~p",
		   [Name, ?MODULE, pre_init_per_suite, {error, Other}]),
            {{fail,Other }, [Name]}
    end.

%%% @hidden
terminate(Names) ->
   case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    ok;
        "cloudish" ->
	    ok;
	"target" ->
	    do_terminate(Names)
    end.

do_terminate([]) ->
    ok;
do_terminate([Name|T])->
    rct_multi_node_cfg:remove_config(Name),
    do_terminate(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TCP PART TCP PART TCP PART TCP PART TCP PART TCP PART %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{ok,Pid} = rct_power:open_tcp(kalle,"137.58.180.20", 1016, off, 1000).

open_tcp(Name, IP, Port, Op, Opt, Timeout) ->
    Pid = spawn(?MODULE,init_tcp,[?NOF_TCP_RECONN, Name, self(), IP, Port, Op, Opt, Timeout]),
    receive 
	{ok,connected} ->
	    {ok,Pid};
	{error,Reason} ->
	    {error,Reason};
	Other ->
	    io:format("Other ~p~n",[Other])
    end.

send_tcp(Pid, Msg) ->
    call_tcp(Pid, {send, self(), Msg}).

%rct_power:get_data_tcp(Pid).
get_data_tcp(Pid) ->
    call_tcp(Pid, {get_data, self()}).

%rct_power:close_tcp(Pid).
close_tcp(Pid) ->
    call_tcp(Pid, {close, self()}).
    
%% @hidden
init_tcp(N, Name, From, Server, Port, Op, Opt, Timeout) ->
    case gen_tcp:connect(Server, Port, [list,{packet,0}], Timeout) of
	{ok,Sock} ->
	    From ! {ok,connected},
	    loop_tcp(Sock,[],no_state);
	{error, Reason} when Reason == econnrefused;
			     Reason == timeout ->
	    case Opt of 
		retry ->
		    case N of 
			0 ->
			    ct:log(lightred,"~p: ~p ~p Number of retries exceeded while connecting to console server for power ~p, Reason: ~p",
				   [Name, ?MODULE, init_tcp, Op, {error, econnrefused}]),
			    From ! {error, Reason};
			N ->
			    ct:log(internal,"~p: ~p ~p Console server port for power ~p busy or timeout. Will retry ~p times more with ~p seconds delay",
				   [Name, ?MODULE, init_tcp, Op, N, ?TIME_RECONN]),
			    timer:sleep(?TIME_RECONN * 1000),
			    init_tcp(N-1, Name, From, Server, Port, Op, Opt, Timeout)
		    end;
		no_retries ->
		    ct:log(white,"~p: ~p ~p Could not connect to console server for power ~p, Reason: ~p",
			   [Name, ?MODULE, init_tcp, Op, {error, Reason}]),	    
		    From ! {error, Reason}
	    end;
	{error, Reason} ->
	    case Opt of 
		retry ->
		    ct:log(lightred,"~p: ~p ~p Could not connect to console server for power ~p, Reason: ~p",
			   [Name, ?MODULE, init_tcp, Op, {error, Reason}]);
		no_retries ->
		    ct:log(white,"~p: ~p ~p Could not connect to console server for power ~p, Reason: ~p",
			   [Name, ?MODULE, init_tcp, Op, {error, Reason}])
	    end,
	    From ! {error,Reason}
    end.

loop_tcp(Sock,Acc,State) ->
    receive
	{tcp_closed,Sock} ->
%	    io:format("~p ~p ~p",[now(),self(),{tcp_closed,Sock}]),
	    get(caller) ! {error, tcp_port_unexpectedly_closed};
	{tcp,Sock,Data} ->
%	    io:format("~p ~p ~p",[now(),self(),{tcp,Sock,Data}]),
	    loop_tcp(Sock, [lists:reverse(Data) | Acc], State);
	{tcp_error,Sock,_Reason} ->
%	    io:format("~p ~p ~p",[now(),self(),{tcp_error,Sock,_Reason}]),
	    die;
	{send, From, Msg} ->
%	    io:format("~p ~p ~p",[now(),self(),{send, From, Msg}]),
	    From ! gen_tcp:send(Sock, Msg),	    	       		    
	    loop_tcp(Sock, Acc, State);
	{get_data, From} ->
%	    io:format("~p ~p ~p",[now(),self(),{get_data, From}]),
	    put(caller, From),
	    loop_tcp(Sock, Acc, receiving);
	{close, From} ->
%	    io:format("~p ~p ~p",[now(),self(),{close, From}]),
	    gen_tcp:close(Sock),         
	    From ! ok;
	Other ->
	    io:format("!!!!!!!!!!!!!!!!!! power tcp loop received ~p!!!!!!!!!!!!!!!!!!",[Other]),
	    loop_tcp(Sock, Acc, State)
%% Return data when tcp connection has been silent for TCP_TMO milliseconds.
%% The crappy bus between processor unit and outlet may take 1 sec to pass a message.
    after ?TCP_TMO -> 
	    case State of
		receiving ->
		    get(caller) ! {ok, lists:reverse(lists:append(Acc))},
		    loop_tcp(Sock, [], no_state);
		_ ->
		    loop_tcp(Sock, Acc, State)
	    end
    end.

call_tcp(Pid, Msg) ->
    case erlang:is_process_alive(Pid) of
	false -> 
	    {error, tcp_proc_not_alive};
	true ->
	    Pid ! Msg,
	    receive
		Reply ->
		    Reply
	    after 10000 ->
		    {error, timeout_calling_tcp_proc}
	    end
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PDU PART PDU PART PDU PART PDU PART PDU PART PDU PART %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% msg(PDU,Port,{read | write, on | off}).
msg(Dadr,Obj,{Code,Data})->                   %create a svift message 
					      %(simplified, only nstctl,relative mode 
    lists:flatten([
		   denib(4,1),                % hflg=requ,protocol=1
		   denib(2,Dadr),             % destination mode relative, destination address
		   denib(0,0),                % source address denib=0
                   8,                         % otyp=group
                   denib(Obj,2),              % code=start
		   7,                         % otyp=nstctl
		   denib(0,Code),             % object number, code (read | write)
                   Data,                      % data, optional depending on code
		   [],                        % sequence number, not used
		   []                         % optional checksum, not used
		  ]).

%read_voltage(Dadr,Obj) ->
%    lists:flatten([
%		   denib(4,1),                % hflg=requ,protocol=1
%		   denib(2,Dadr),             % destination mode relative, destination address
%		   denib(0,0),                % source address denib=0
%                   8,                         % otyp=group
%                   denib(Obj,2),              % (Obj, code=read)
%                   5,                         % otyp=rosan
%		   denib(0,0),                % object number, read
%                   1,                         % OutVolt
%		   [],                        % sequence number, not used
%		   []                         % optional checksum, not used
%		  ]).
    

frame(Msg)->                                  %create frame for message, simplified,
				              %only handles frames of less than 16 bytes
    Msglen=length(Msg),
    Hd=128+64+32+Msglen+2,  % 
    lists:flatten([
		   Hd,                         %Header
		   1,                          %protocol type=svift 
		   Msg,                        %svift message
		   csum([Hd,1,Msg])            %checksum
		  ]). 

csum(L)-> 255 bxor 255 band lists:sum(lists:flatten(L)).

%%implements ebyte, that is, a (possibly long) binary value, one or more bytes, 
%%bit 7 set indicates continuation in next byte
ebyte(X)-> ebyte(X,[]).
ebyte(X,Acc) ->
    Y=X band 127,
    case X<128 of
	true->lists:reverse([Y|Acc]);
	false-> ebyte((X-Y) div 128,[Y|Acc])
    end.

%%implements "double extendible nibble"  two binary values a and b, 
%%the 3 least significant bits of a are placed in bits 4 to 6 of the
%%first byte. setting bit 7 to 1 indicates that there are more bits in 
%%a, which are then coded as an ebyte following the first byte.
%%the 3 least significant bits of b are placed in bits 0 to 2 of the
%%first byte. setting bit 3 to 1 indicates that there are more bits in 
%%b, which are then coded as an ebyte following the first byte and the 
%%"ebyte" extension of a (if present)

denib(A,B)->		  
    A1=A band 7,
    B1=B band 7,
    {EA,XA}=case A>7 of 
		false ->{0+A1*16,[]};
		true ->{128+A1*16,ebyte((A-A1) div 8)}
	    end,
    {EB,XB}=case B>7 of 
		false ->{0+B1,[]};
		true ->{128+B1,ebyte((B-B1) div 8)}
	    end,
    lists:flatten([EB+EA,XA,XB]).

decode(L)->  %grossly simplified,verifies checksum,assumes data in last byte...
    case lists:reverse(L) of
	[Csum|RMsg]->
	    case Csum==csum(RMsg) of
		false-> 
		    {error,{checksum_error,L}};
		true -> [Data|_]=RMsg, Data,
			case Data of
			    0-> off;		 
			    1-> on
			end
	    end; 
	_-> {error,{faulty_response,L}}

    end.

