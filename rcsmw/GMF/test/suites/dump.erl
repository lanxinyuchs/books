%%% ----------------------------------------------------------
%%% %CCaseFile:	dump.erl %
%%% Author:	erarafo
%%% Description: Various aids that can be loaded into the Erlang VM
%%% when needed. Reload the file by typing the following in an Erlang
%%% shell:
%%%
%%% code:purge(dump), code:load_abs("/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/GMF/GMF_CNX9012719/test/suites/dump").
%%%
%%% To rebuild the beam, run any of the test suites in this directory.
%%% The suites don't use this module but it gets recompiled anyway.
%%%
%%% Modules used: None.
%%%
%%% ----------------------------------------------------------
-module(dump).
-id('Updated by CCase').
-vsn('/main/R3A/R6A/1').
-date('2016-09-06').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% R3A/1      2015-03-06 erarafo     First version
%%% ----------------------------------------------------------

-export([help/0]).
-export([info/0]).
-export([dtf/0]).
-export([dumpToFile/1]).
-export([dump/1]).
-export([size/1]).

-export([xport/1]).                     % probably not useful

-compile({no_auto_import, [size/1]}).   % meaning?


help() ->
    io:format(user, "Available functions:~n", []),
    io:format(user, "  ~w:info().        Info about the running CS~n", [?MODULE]),
    io:format(user, "  ~w:dump(_).       Dump given table to the screen~n", [?MODULE]),
    io:format(user, "  ~w:dumpToFile(_). Dump given table to file~n", [?MODULE]),
    io:format(user, "  ~w:dtf().         Dump the imm_objects table to imm_objects.txt", [?MODULE]),
    io:format(user, "  ~w:size(_).       Show size of given table~n", [?MODULE]),
    io:format(user, "  ~n", []),
    ok.


%%% ----------------------------------------------------------
%%% @doc Lists info about the running CS.
%%% @end
%%% ----------------------------------------------------------

info() ->
    infoCxsVersion().

infoCxsVersion() ->
    case size(swInventory) of
	1 ->
	    {ok, Record, _NextKey} = getRecord(swInventory),
	    {swInventory, _, _, [Ldn]} = Record,
	    io:format(user, "SW inventory: ~s~n", [lists:last(string:tokens(Ldn, ","))]);
	N ->
	    io:format(user, "unable to determine SW inventory, n. of records: ~w~n", [N])
    end.


%%% ----------------------------------------------------------
%%% @doc Gets a record from a table of "set" kind.
%%% @end
%%% ----------------------------------------------------------
getRecord(Table) ->
        Key = mnesia:dirty_first(Table),
	[Record] = mnesia:dirty_read(Table, Key),
	NextKey = mnesia:dirty_next(Table, Key),
	{ok, Record, NextKey}.


%%% ----------------------------------------------------------
%%% @doc Dumps the imm_objects table to 'imm_objects.txt'.
%%% @end
%%% ----------------------------------------------------------
dtf() ->
    dumpToFile(imm_objects).

dumpToFile(Table) ->
    {ok, Stream} = file:open(atom_to_list(Table)++".txt", [write]),
    dump(Stream, Table),
    file:close(Stream).

dump(Table) ->
    dump(user, Table).


dump(Stream, Table) ->
    Key = mnesia:dirty_first(Table),
    ordsets:fold(
      fun(Record, _Acc) ->
	      io:format(Stream, "~p~n~n", [Record])
      end,
      ok,
      tableDumpHelper(Stream, Table, Key, 0, ordsets:new())).


tableDumpHelper(_Stream, _Table, '$end_of_table', _Count, Set) ->
    %io:format(Stream, "--- table: ~w, shown records: ~w~n~n", [Table, Count]),
    Set;

tableDumpHelper(Stream, Table, Key, Count, Set) ->
    Record = mnesia:dirty_read(Table, Key),
    NewCount = Count+1,
    %io:format(Stream, "~p~n~n", [Record]),
    NewSet = ordsets:add_element(Record, Set),
    tableDumpHelper(Stream, Table, mnesia:dirty_next(Table, Key), NewCount, NewSet).


%%% ----------------------------------------------------------
%%% @doc Returns the size of the given table.
%%% @end
%%% ----------------------------------------------------------
-spec size(atom()) -> integer().

size(Table) ->
    Key = mnesia:dirty_first(Table),
    tableSizeHelper(Table, Key, 0).


tableSizeHelper(_, '$end_of_table', Count) ->
    Count;

tableSizeHelper(Table, Key, Count) ->
    tableSizeHelper(Table, mnesia:dirty_next(Table, Key), Count+1).


%%% ----------------------------------------------------------
%%% @doc This function needs an explanation.
%%% @end
%%% ----------------------------------------------------------

xport(Table) ->
    FileName = randomFileName("terms"),
    {ok, IoDevice} = file:open(FileName, [write]),
    Key = mnesia:dirty_first(Table),
    xportHelper(Table, Key, IoDevice),
    compress(FileName).


xportHelper(_Table, '$end_of_table', IoDevice) ->
    file:close(IoDevice);

xportHelper(Table, Key, IoDevice) ->
    Record = mnesia:dirty_read(Table, Key),
    io:format(IoDevice, "~w.~n~n", [Record]),
    xportHelper(Table, mnesia:dirty_next(Table, Key), IoDevice).


compress(FileName) ->
    CompressedFileName = randomFileName("compr"),
    os:cmd(
      lists:flatten(
	io_lib:format(
	  "gzip <~s | hexdump -e '50/1 \"%02x\" \"\\n\"' >~s",
	  [FileName, CompressedFileName]))),
    CompressedFileName.

randomFileName(Prefix) ->
    filename:join(
      ["/tmp",
       lists:flatten(io_lib:format("~s~w", [Prefix, rand:uniform(999999)]))]).
