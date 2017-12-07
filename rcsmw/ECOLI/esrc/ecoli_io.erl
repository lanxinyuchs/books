%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ecoli_io.erl %
%%% @author etxlg
%%% @copyright Ericsson AB 2013-2015
%%% @version /main/R2A/R3A/1

%%% @doc ==COLI==
%%% An implementation of an erlang io-server
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(ecoli_io).
-vsn('/main/R2A/R3A/1').
-date('2015-03-29').
-author('etxlg').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2015 All rights reserved.
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
%%% Rx         2013-10-14 etxlg       Created
%%% R3A/1      2015-03-19 etxlg       Try to do flowcontrol
%%% ----------------------------------------------------------
%%% 

-export([io_request/3, io_reply/1]).
-export([maybe_unblock/2]).
-export([new_io/0, new_io/2]).

-record(io, {req, from, reply_as, cont = [], buf = <<>>}).

new_io() ->
    #io{}.

new_io(#io{buf = Buf} = Io, Chars) when is_binary(Chars) ->
    New_buf = <<Buf/binary, Chars/binary>>,
    if
	size(New_buf) >= 2000 ->
	    {full, Io#io{buf = New_buf}};
	true ->
	    {more, Io#io{buf = New_buf}}
    end;
new_io(#io{} = Io, Chars) ->
    new_io(Io, iolist_to_binary(Chars)).

io_request(Request, Is_alive, Io_state) ->
    {io_request, From, Reply_as, Req} = Request,
    Io_state2 = Io_state#io{from = From, reply_as = Reply_as},
    case io_req(Req, Is_alive, Io_state2) of
	{write, _, Reply, Chars} ->
	    {write, {From, Reply_as, Reply}, Chars};
	{read, New_io_state, Reply} ->
	    if
		size(New_io_state#io.buf) < 2000 ->
		    {read_ack, {From, Reply_as, Reply}, New_io_state};
		true ->
		    {read, {From, Reply_as, Reply}, New_io_state}
	    end;
	{blocking, New_io_state} ->
	    {blocking, New_io_state};
        {reply, _, Reply} ->
	    {reply, {From, Reply_as, Reply}}
    end.

maybe_unblock(#io{req = undefined} = _Io, _) ->
    ok;
maybe_unblock(#io{req = Req, from = From, reply_as = Reply_as} = Io,
	      Is_alive) ->
    case Req of
	{get_chars, _Enc, _Prompt, _N} ->
	    io_request({io_request, From, Reply_as, Req}, Is_alive, Io);
	{get_line, _Enc, _Prompt} ->
	    io_request({io_request, From, Reply_as, Req}, Is_alive, Io);
	{get_until, _Enc, _Prompt, _M, _F, _A} ->
	    io_request({io_request, From, Reply_as, Req}, Is_alive, Io)
    end.

io_reply({From, Reply_as, Reply}) ->
    From ! {io_reply, Reply_as, Reply}.

io_req({put_chars, _Enc, Chars}, _P, Io) when is_binary(Chars) ->
    {write, Io, ok, Chars};
io_req({put_chars, _Enc, Chars}, _P, Io) ->
    io_req({put_chars, _Enc, iolist_to_binary(Chars)}, _P, Io);
io_req({put_chars, _Enc, M, F, A}, _P, Io) ->
    try apply(M, F, A) of
	Chars when is_binary(Chars) ->
	    io_req({put_chars, _Enc, Chars}, _P, Io);
	Chars ->
	    io_req({put_chars, _Enc, iolist_to_binary(Chars)}, _P, Io)
    catch
	A:B ->
            erlang:display({?MODULE, "catch in put_chars apply:", A, B}),
	    {reply, Io, {error, {A,B}}}
    end;
io_req({get_chars, _Enc, _Prompt, N}, Is_alive, Io) ->
    case {Io#io.buf, Is_alive} of
	{<<>>, false} -> 
	    {read, #io{}, eof};
	{<<>>, true} -> 
	    {blocking, Io#io{req = {get_chars, _Enc, _Prompt, N}}};
	{Buf, _} when size(Buf) =< N -> 
	    {read, #io{}, Buf};
	{Buf, _} -> 
	    <<Deliver:N/binary, Remains/binary>> = Buf,
	    {read, #io{buf = Remains}, Deliver}
    end;
io_req({get_line, _Enc, _Prompt}, Is_alive, Io) ->
    case {Io#io.buf, Is_alive} of
	{<<>>, false} -> 
	    {read, #io{}, eof};
	{<<>>, true} -> 
	    {blocking, Io#io{req = {get_line, _Enc, _Prompt}}};
	{Buf, _} ->
	    case {binary:split(Buf, <<"\n">>), Is_alive} of
		{[_], true} ->
	    	    {blocking, Io#io{req = {get_line, _Enc, _Prompt}}};
		{[Last_line], false} ->
	    	    {read, #io{}, Last_line};
		{[Line,  Rest], _} ->
	    	    {read,#io{buf = Rest}, <<Line/binary, $\n>>}
	    	    % more what? {read, Io#io{buf = Rest}, <<Line/binary, $\n>>}
	    	    % what? {read, #io{buf = Rest}, Line ++ [$\n]}
	    end
    end;

%%HERE need to "binarize" this part??
io_req({get_until, _Enc, _Prompt, M, F, A}, Is_alive, Io) ->
    Buf = case {Io#io.buf, Is_alive} of
		{<<>>, false} -> eof;
		_ -> Io#io.buf
	end,
    try apply(M, F, [Io#io.cont, Buf | A]) of
        {done, Deliver, Rest} ->
	    {read, Io#io{buf = Rest}, Deliver};
	{more, New_cont} ->
	    {blocking,
		Io#io{req = {get_until, _Enc, _Prompt, M, F, A},
			cont = New_cont,
			buf = <<>>}}
    catch
	E1:E2 ->
            erlang:display({?MODULE, "catch in get_until apply:", E1, E2}),
	    {reply, Io, {error, {E1, E2}}}
    end;


io_req({requests, Reqs}, Is_alive, Io) ->
    multi_io_req(Reqs, Is_alive, {reply, Io, ok});
io_req(getopts, _, Io) ->
    {reply, Io, [{encoding, latin1}, {binary, false}]};
io_req(What, _, Io) ->
    erlang:display({?MODULE, "unsupported io request:", What}),
    {reply, Io, {error, request}}.

multi_io_req([], _, Result) -> Result;
multi_io_req(_, _, {reply, _, Reply} = Result)
		when element(1, Reply) =:= error ->
    Result;
multi_io_req([R | Reqs], Is_alive, Result) ->
    multi_io_req(Reqs, Is_alive, io_req(R, Is_alive, element(2, Result))).

%dbg_print_io(Io) ->
%lists:flatten(
    %io_lib:format("Io: req=~p, from=~p, reply_as=~p, cont=~p\nbuf=~p\n",
		  %[Io#io.req, Io#io.from, Io#io.reply_as, Io#io.cont,
		   %binary_to_list(Io#io.buf)])).
