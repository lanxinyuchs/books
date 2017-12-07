%% #0.    BASIC INFORMATION
%% -----------------------------------------------------------------------------
%% %CCaseFile:	imm_test_app_create.erl %
%% @private
%% @author Ola Andersson <ola.a.andersson@ericsson.com>
%% @copyright Ericsson AB 2012
%% @doc 
%% @end
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012 All rights reserved.
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
%%% R2A/1      2012-10-23 eolaand     Created
%%% ----------------------------------------------------------
-module(imm_test_app_create).

%% API
-export([create/0]).

-define(APP_NAME, "imm_test_app").
-define(SAFE_LIB, "safe_lib").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec create() -> {ok, FileName::list()} | {error, Reason::atom()}
%% @end
%%--------------------------------------------------------------------
create() ->
    create(?APP_NAME ++ ".escript").

%% @private
create(OutFile) ->
    SafeDir = code:lib_dir(safe),
    [Safe | _] = lists:reverse(filename:split(SafeDir)),
    ok = file:make_symlink(SafeDir, Safe),
    AppMod = ?APP_NAME ++ ".beam",
    SafeLibMod = ?SAFE_LIB ++ ".beam",
    try
	EbinPath = code:where_is_file(atom_to_list(?MODULE) ++ ".beam"),
	EbinDir = filename:dirname(EbinPath),
	ok = file:make_symlink(filename:join([EbinDir,AppMod]), AppMod),
	ok = add_safe_patch(),
	{ok, {_File, ArchBin}} = 
	    zip:create(Safe ++ ".ez", 
		       [Safe, AppMod, SafeLibMod], 
		       [memory,
			{cwd, "."},
			{compress, all},
			{uncompress,[".beam",".app"]}]),
	file:delete(Safe),
	file:delete(AppMod),
	file:delete(SafeLibMod),
	ok = escript:create(OutFile,[shebang,{archive,ArchBin}]),
	file:change_mode(OutFile,8#775),
	{ok, OutFile}
    catch
	_:Reason ->
	    error_logger:format("Create escript failed: ~p~n", [Reason]),
	    file:delete(Safe),
	    file:delete(AppMod),
	    file:delete(SafeLibMod),
	    {error, failed_to_create_escript}
    end.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
add_safe_patch() ->
    Src = code:lib_dir(safe,src),
    ErlFile = ?SAFE_LIB ++ ".erl",
    {ok, Bin} = file:read_file(filename:join([Src, ErlFile])),
    Match = <<"load_driver[(]Driver, Arg[)] ->">>,
    FStr = "load_driver(Driver, Arg)",
    Replace = 
	lists:concat([FStr," -> ",?APP_NAME,":",FStr,".\n\ndum",FStr," ->"]),
    PatchBin = re:replace(Bin, Match, list_to_binary(Replace)),
    ok = file:write_file(ErlFile, PatchBin),
    {ok, _} = compile:file(ErlFile,[{i,Src}]),
    file:delete(ErlFile).

