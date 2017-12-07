#!/usr/bin/env escript
%% -*- erlang -*-
-module(has_testlicense).
-mode(compile).
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	has_testlicense.escript %
%%% Author:	etxarnu
%%% Description:
%%%
%%% This script checks if the node has a test license (used in lab environment)
%%% Used by the start_vrcs.sh script
%%%  It depends on the lmaLkf.beam file beeing in ../ebin/ directory 
%%%
%%% ----------------------------------------------------------
%%% 2017-011-27  etxarnu  Created
%%% ----------------------------------------------------------
%%% 
-define(LKF_FILE, "licensingKeyFile.xml").
-include_lib("public_key/include/public_key.hrl").


main([]) -> ok;  %to test for compilation errors

main([Node,LmaEbin]) ->
    code:add_pathz(LmaEbin),
    case get_lkf_fn(Node) of
	{ok, LKFPath} ->
	    case verify_lkf_file(LKFPath) of
		valid ->
		    case file:read_file(LKFPath) of
			{ok, FileData} ->
			    case look_for_test_license(FileData) of
				true ->
				    yes();
				false ->
				    no()
			    end;
			_ ->
			    no()
		    end;
		_ ->
		    no()
	    end;
	_ ->
	    no()
    end.


yes()->
    io:format("yes").

no() ->
    io:format("no").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% code from aicI.erl 
get_lkf_fn(Node) ->
    LKF = get_cfgfiles_path(Node,?LKF_FILE),
    case file:read_file_info(LKF) of
        {ok, _} ->
            {ok, LKF};
        {error, Reason} ->
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% code from aicServer.erl 
get_cfgfiles_path("R-VNFM",File) ->
    "/rcs/networkloader/" ++ File;
get_cfgfiles_path(_,File) ->
    "/rcs/aiconf/" ++ File.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% code from lmaDataInit.erl 
look_for_test_license(FileData) ->
    lmaDataInit:look_for_test_license(FileData).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_lkf_file(Path) ->
    lmaLkf:verify_lkf_file(Path).

