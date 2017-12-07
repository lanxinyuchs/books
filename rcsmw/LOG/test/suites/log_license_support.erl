%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	log_license_support.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R5A/R6A/R8A/R10A/1
%%% 

-module(log_license_support).
%%% Except as specifically authorized in writing by Ericsson, the 
%%% receiver of this document shall keep the information contained 
%%% herein confidential and shall protect the same in whole or in 
%%% part from disclosure and dissemination to third parties.
%%% 
%%% Disclosure and disseminations to the receivers employees shall 
%%% only be made on a strict need to know basis.
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R5A/1      2016-04-15 etxpejn     Created
%%% R5A/3      2016-06-22 etxkols     Added get_lma_dir/0
%%% R5A/4      2016-08-22 etxkols     Git migration requires that CC paths is not used 
%%% R5A/5      2016-08-22 etxkols     Reverted last Git migration change 
%%% R5A/6      2016-08-22 etxkols     Git migration again
%%% R8A/1      2016-12-21 etxpejn     Updated to latest LKF
%%% R10A/1     2017-10-03 etxpejn     Updated to latest LKF
%%% ----------------------------------------------------------
%%% 

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([install_lkf/2]).
-export([install_lkf/3]).

-define(LMA_DIR, rct_cc_git_path:find("RCS_TOP", ["LMA/LMA_CNX9013077/test/suites/", "LMA/test/suites/"])).

install_lkf(SFTP_URL, Password) ->
    LKF = "RCS_MSR_170517_130105.xml",
    install(SFTP_URL, Password, LKF).
    

install_lkf(SFTP_URL, Password, LKF) ->
    install(SFTP_URL, Password, LKF).
 
install(SFTP_URL, Password, LKF) -> 
    JenkinsNode = lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    SFTP_DIR = "/proj/rcs-tmp/stps/",
    {ok,_} = file:copy(?LMA_DIR++LKF, 
		       SFTP_DIR++JenkinsNode++"/"++LKF),
    FingerprintUpdateable= {'ManagedElement',
			    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			    [{managedElementId,[],["1"]},
			     {'SystemFunctions',
			      [{systemFunctionsId,[],["1"]},
			       {'Lm',
				[{xmlns,"urn:com:ericsson:ecim:LM"}],
				[{lmId,[],["1"]},
				 {fingerprintUpdateable,[],[]}
				]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    {ok, A} = ct_netconfc:get(nc1, FingerprintUpdateable),
    ct_netconfc:close_session(nc1),
    {ok,{fingerprintUpdateable,_,FPUpdateable}} = extract_element(fingerprintUpdateable, A),
    
    case FPUpdateable of
	["true"] ->
	    %% Set fingerprint
	    B = {'ManagedElement',
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		 [{managedElementId,[],["1"]},
		  {'SystemFunctions',[],
		   [{systemFunctionsId,[],["1"]},
		    {'Lm',
		     [{xmlns,"urn:com:ericsson:ecim:LM"}],
		     [{lmId,[],["1"]},
		      {fingerprint,[],["RCS_MSR"]}
		     ]}]}]},
	    {ok,_} = ct_netconfc:open(nc1,[]),
	    ok = ct_netconfc:edit_config(nc1, running, B),
	    ct_netconfc:close_session(nc1);
	_ ->
	    %% Fingerprint is already set
	    do_nada
    end,
        
    %% Install LKF
    C =  {'ManagedElement',
	  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	  [{managedElementId,[],["1"]},
	   {'SystemFunctions',[],
	    [{systemFunctionsId,[],["1"]},
	     {'Lm',
	      [{xmlns,"urn:com:ericsson:ecim:LM"}],
	      [{lmId,[],["1"]},
	       {'KeyFileManagement', [],
		[{keyFileManagementId,[],["1"]},
		 {installKeyFile,[],
		  [{uri, [], [SFTP_URL++SFTP_DIR++JenkinsNode
			      ++"/"++LKF]},
		   {password, [], [Password]}]}
		]}]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    ct_netconfc:action(nc1, C),
    ct_netconfc:close_session(nc1),
    ok = wait_until_install_complete(0).

wait_until_install_complete(30) ->
    nok;
wait_until_install_complete(No) ->
    InstallResult = {'ManagedElement',
		     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		     [{managedElementId,[],["1"]},
		      {'SystemFunctions',
		       [{systemFunctionsId,[],["1"]},
			{'Lm',
			 [{xmlns,"urn:com:ericsson:ecim:LM"}],
			 [{lmId,[],["1"]},
			  {'KeyFileManagement',
			   [{keyFileManagementId,[],["1"]},
			    {reportProgress,[],[]}
			   ]}]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    {ok, C2} = ct_netconfc:get(nc1, InstallResult),
    ct_netconfc:close_session(nc1),
    {ok,{_,_,Report}} = extract_element(reportProgress, C2),
    ct:pal("Report: ~p", [Report]),

    {ok,{_,_,Result}} = extract_element(result, Report),
    ct:pal("Result: ~p", [Result]),

    case Result of
	 ["SUCCESS"] ->
	    ok;
	_Else ->
	    timer:sleep(1000),
	    wait_until_install_complete(No +1)
    end.

%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

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
