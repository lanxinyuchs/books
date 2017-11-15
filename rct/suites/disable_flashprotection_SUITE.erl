%% Author: elavaku
%% Created: Sep 09, 2014
%%% @copyright Ericsson AB 2014
%% Description: Disable the Flash protection suite


-module(disable_flashprotection_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
%%% ----------------------------------------------------------
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
%%% Rev        Date        Name        What
%%% -----      ---------   --------    -----------------------
%%% 01         2014-09-04  ELAVAKU     Initial revision
%%% 02         2014-10-16  ELAVAKU     Added testcase to disable
%%%                                    and enable vendor credentials
%%%%----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-compile([export_all]).
-export([init_per_suite/1,
		 end_per_suite/1,
		 init_per_testcase/2,
		 end_per_testcase/2,
		 disable_flash_protection/1,
		 disable_vendorcredentials/1,
		 enable_vendorcredentials/1]).

-define(DEFAULT_TIMEOUT, 600000).
-define(DEBUG, ?MIN_IMPORTANCE).

suite() ->
	[{timetrap, {hours, 1}},
	 {ct_hooks, [{rct_htmllink,[]},
				 {rct_power,pow},
				 {rct_scp,scpn1},
				 {rct_consserv,cs},
				 {rct_rs232,rs232},
				 {cth_conn_log,[]}
			 	]}].

init_per_suite(Config) ->
	Config.

end_per_suite(_Config) ->
	ok.

%% ===========================================================================
%% @doc
%%		Connects to board and records start time of testcase<br>
%% @end
%% ===========================================================================
init_per_testcase(TestCase, Config) ->

  Test = "disable_vendorcredentials",

  case (Test == atom_to_list(TestCase)) of
	  true ->
	         case rct_power:cycle(pow) of
	                  ok ->
	                        {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:", [{timeout,180000},no_prompt_check]),
	                        ok = ct_telnet:send(rs232, "\n"),
	                        {ok, {ubootprompt,_}} =  ct_telnet:expect(rs232,[{notprompt_a,"=> A"}, 
				                                                   {notprompt_b,"=> B"}, 
										      {ubootprompt,"=> "}],
 	                                                                                   [{timeout,3000},no_prompt_check]),
                                ct:pal("In Uboot mode after power cycling");
	                   _ ->
                                ct:fail("Could not break the board in uboot mode")
	         end;
	  false ->
		 ct_telnet:send(rs232, "\n"),
		 case  ct_telnet:expect(rs232,[{notprompt_a,"=> A"},
                                               {notprompt_b,"=> B"},
			                       {ubootprompt,"=> "},
                                               {nlprompt,"networkloader"}],
 	                                        [{timeout,3000},no_prompt_check]) of
      	                        {ok, {ubootprompt,_}} ->
		                                          ct:pal("In Uboot prompt");
                                {ok, {nlprompt,_}} ->
		                                          ct:pal("In NL prompt");

		                 {_, _} ->
		                               ok = rct_rs232:login(rs232)
                 end
  end,
  ct:pal("Starting testcase ~p", [TestCase]),
  Config.

%% ===========================================================================
%% @doc
%%		Connects to board<br>
%% 		Downloads syslog from board<br>
%% 		Runs llog<br>
%%		Records end time of testcase<br>
%% @end
%% ===========================================================================
end_per_testcase(TestCase, Config) ->

  Test = "disable_flash_protection",

  case (Test == atom_to_list(TestCase)) of
     true ->
             ok = rct_rs232:login(rs232),
             ct_telnet:cmd(rs232, "llog");

     false ->
             ok
  end,

  ct:pal("Ending testcase ~p ", [TestCase]).


all() ->
	[{group, disable_VC_flash}, {group, disable_flash}, {group, enable_VC}].

%% ===========================================================================
%% @doc
%%		Groups <br>
%% @end
%% ===========================================================================
groups() ->
	[
%	  {disable_VC_flash, [], [disable_vendorcredentials, disable_flash_protection]},

%         {enable_VC, [], [enable_vendorcredentials]},
	  
	  {disable_flash, [], [disable_flash_protection]}

	].

%% ===========================================================================
%% @doc
%%	   Function to convert list to string appended by newline<br/>
%% @end
%% ===========================================================================
list_to_string(List) when is_list(List)->
	F = fun(X) ->
				X ++ "\n"
		end,
	lists:flatmap(F, List).


%% ===========================================================================
%% @doc
%%         Function to return if a board is secure or not
%% @end
%% ===========================================================================
is_secure_uboot() ->

   ct_telnet:send(rs232, "printenv productnumber"),
   Secure = case ct_telnet:expect(rs232, "productnumber=KDU 137 926/11", [{timeout,1000},no_prompt_check]) of
                {ok,_} -> true;

                {_,_} -> false
   end,

   Secure.

is_secure_type3() ->

   ct_telnet:send(rs232, "$RHAIDIR/bin/rhdc boardinfo"),
   Secure = case ct_telnet:expect(rs232, "Product number: KDU 137 926/11", [{timeout,8000},no_prompt_check]) of
                {ok,_} -> true;

                {_,_} -> false
   end,

   Secure.

%% ===========================================================================
%% @doc
%%           Function to extract a string at given memory address
%% @end
%% ===========================================================================

extract_vc(Addr, Word_num, Char) ->
  ct_telnet:sendf(rs232,"md.b ~s 4",[Addr]),
  timer:sleep(1000),
  {ok, Data} = ct_telnet:get_data(rs232),
  timer:sleep(1000),
  Sublist = lists:filter(fun(X) -> string:str(X, Addr)==0 end, Data),
  string:sub_string(string:sub_word(list_to_string(Sublist),Word_num), 1, Char).

%% ===========================================================================
%% @doc
%%	     Function waiting for command execution in uboot
%% @end
%% ===========================================================================

wait_for_ubootprompt(Time_out) ->
      {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,Time_out},no_prompt_check]).

%% ===========================================================================
%% @doc
%%           Function to disable Vendor credentials
%% @end
%% ===========================================================================

disable_vc() ->

Credential_string = "vecr",
ct:pal("Disabling the vendor credentials"),

% Verify that the board is uboot prompt

ct_telnet:send(rs232, "\n"),
case ct_telnet:expect(rs232,
	                   [{notprompt_a,"=> A"},
                            {notprompt_b,"=> B"},
	                    {ubootprompt,"=> "}],
                                 [{timeout,5000},no_prompt_check]) of
    {ok, {ubootprompt,_}} ->
	       ct:pal("Board in Uboot mode to start disabling vendor credentials");
    {_, _} ->
             ok = rct_rs232:login(rs232),
	     ok = ct_telnet:send(rs232,"pgh_restartbrd -t \"disable_vc_func: Initial setup\""),
	     ct:pal("Keeping the board in Uboot mode"),

             {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:", [{timeout,180000},no_prompt_check]),
	     timer:sleep(1000),
	     ok = ct_telnet:send(rs232, "\n"),
	     {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,5000},no_prompt_check]),

             ct:pal("In Uboot mode, Starting to disable vendor credentials ")
end,


% Read VC (vecr) to temporary memory area

ct_telnet:send(rs232,"sf probe; sf read 0x8000000 0x7f0000 0x10000"),
wait_for_ubootprompt(10000),

VC_avail = extract_vc("0x8000000", 6, 4),

if
   (VC_avail == "vecr") ->

                     % Update the VC information into backup spare partition  
		      
		      ct_telnet:send(rs232,"sf update 0x8000000 0x760000 0x10000"),
                      wait_for_ubootprompt(10000),

                      ct_telnet:send(rs232,"sf read 0x7000000 0x760000 0x10000"),
                      wait_for_ubootprompt(10000),

                      Credential_string = extract_vc("0x7000000", 6, 4),

		      ct_telnet:send(rs232,"cmp.b 0x7000000 0x8000000 0x10000"),
                      {ok, _} = ct_telnet:expect(rs232,"Total of 65536 byte\\(s\\) were the same",[{timeout,5000},no_prompt_check]),
                      wait_for_ubootprompt(3000),

                    % Extra backup in another spare sector.
		      ct_telnet:send(rs232,"sf update 0x8000000 0x6a0000 0x10000"),
                      wait_for_ubootprompt(10000),

		      ct_telnet:send(rs232,"sf probe; sf read 0x9000000 0x6a0000 0x10000"),
                      wait_for_ubootprompt(10000),

		      Credential_string = extract_vc("0x9000000", 6, 4),

		      ct_telnet:send(rs232,"cmp.b 0x9000000 0x8000000 0x10000"),
                      {ok, _} = ct_telnet:expect(rs232,"Total of 65536 byte\\(s\\) were the same",[{timeout,5000},no_prompt_check]),
                      wait_for_ubootprompt(3000),
		     
		    % erase the VC partition once all the above conditions are met
		     
		      ct_telnet:send(rs232,"sf erase 0x7f0000 0x10000"),
                      wait_for_ubootprompt(10000),

                      ct:pal("Vendor credentials disabled sucessfully");

    (VC_avail == "....") ->

		      ct:fail("Already empty, cant proceed with disabling VC again");

	true  ->
	       ct:fail("Dont find proper VC pattern in vendor credentials partition")
end,

timer:sleep(3000),
ct:pal("Now vendor credentials are disabled on the board"),

ok.

%% ===========================================================================
%% @doc
%%           Function used to restore Vendor credentials
%% @end
%% ===========================================================================

restore_vc()->

Credential_string = "vecr",
ct:pal("Restoring the vendor credentials"),

% Restore the vendor credentials from Spare flash partition

ct_telnet:send(rs232,"sf probe; sf read 0x8000000 0x760000 0x10000"),
wait_for_ubootprompt(10000),

ct_telnet:send(rs232,"sf probe; sf read 0x7000000 0x7f0000 0x10000"),
wait_for_ubootprompt(10000),

VC_avail1 = extract_vc("0x8000000", 6, 4),

if
   (VC_avail1 == "vecr") ->

			    VC_avail2 = extract_vc("0x7000000", 6, 4),

			    if
			        (VC_avail2 == "....") ->
			                           ct_telnet:send(rs232,"sf update 0x8000000 0x7f0000 0x10000"),
                                                   wait_for_ubootprompt(10000);

				(VC_avail2 == "vecr") ->
				                   ct:fail("Already has vendor credentials, no need to restore");

				true  ->
	                                          ct:fail("Dont find proper entry in vendor credentials backup partition")
			    end,

			    ct_telnet:send(rs232,"sf probe; sf read 0x7000000 0x7f0000 0x10000"),
                            wait_for_ubootprompt(10000),

			    Credential_string = extract_vc("0x7000000", 6, 4),

			    ct_telnet:send(rs232,"cmp.b 0x7000000 0x8000000 0x10000"),
                            {ok, _} = ct_telnet:expect(rs232,"Total of 65536 byte\\(s\\) were the same",[{timeout,5000},no_prompt_check]),
                            wait_for_ubootprompt(3000),

                            ct:pal("Restored VC successfully");

   (VC_avail1 == "....") ->

		      ct:fail("backup area empty, cant proceed with restoration of VC");

	true  ->
	      ct:fail("Dont find proper entry in vendor credentials backup partition")
end,

ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% T E S T C A S E S %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ===========================================================================
%% @doc
%%           Test case to disable the Vendor credentials
%% @end
%% ===========================================================================
disable_vendorcredentials(_Config) ->

ct_telnet:send(rs232, "\n"),

    case ct_telnet:expect(rs232,
	                        [{notprompt_a,"=> A"},
                                 {notprompt_b,"=> B"},
				 {ubootprompt,"=> "}],
 	                        [{timeout,5000},no_prompt_check]) of
        {ok, {ubootprompt,_}} ->
	     ct:pal("Board in Uboot mode"),
	     case is_secure_uboot() of
	                 true ->
			          disable_vc();

			 false ->
			          % Do nothing
				  ok
	     end;

	{_, _} ->
             ok = rct_rs232:login(rs232),
	     case is_secure_type3() of
	                 true ->
			          ok = ct_telnet:send(rs232,"pgh_restartbrd -t \"disable_vc_tc: Initial setup\""),
	                          ct:pal("Keeping the board in Uboot mode"),

                                  {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:", [{timeout,180000},no_prompt_check]),
	                          timer:sleep(1000),
	                          ok = ct_telnet:send(rs232, "\n"),
	                          {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,5000},no_prompt_check]),

                                  ct:pal("In Uboot mode and can proceed with VC restoration"),
				  disable_vc();

			 false ->
			          % Do nothing
				  ok
	     end

    end,

ok.


%% ===========================================================================
%% @doc
%%		Test case to disable the flash protection
%% @end
%% ===========================================================================

disable_flash_protection(Config) ->

    ct_telnet:send(rs232, "\n"),
    case ct_telnet:expect(rs232,
	                        [{notprompt_a,"=> A"},
                                 {notprompt_b,"=> B"},
				 {ubootprompt,"=> "},
				 {nlprompt,"networkloader"}],
 	                        [{timeout,5000},no_prompt_check]) of
        {ok, {ubootprompt,_}} ->
	     ct:pal("Board in Uboot mode");

        {ok, {nlprompt,_}} ->
	     ct:pal("Found the board in network loader prompt, fetch ESI and re-install"),
	     % Fetch the ESI
	     case aic_curl:fetch_esi(Config) of
                ok ->
	                 ok;
	        {_,_} ->
		         aic_curl:fetch_fake_esi(Config)
             end,
	     % Re install the board
	     ok = aic_curl:install_sw(),
             case ct_telnet:expect(rs232, ".*login:", [{timeout,180000},no_prompt_check]) of
	     {ok, _} ->
		    ok;
	      _ ->
		    ok = ct_telnet:send(rs232, ""),
                    {ok, _} = ct_telnet:expect(rs232, ".*login:", [{timeout,20000},no_prompt_check]) % login: prompt can be corrupted on dusX2
             end,

             timer:sleep(12000),
             ok = rct_rs232:login(rs232),

	     ok = ct_telnet:send(rs232,"pgh_restartbrd -t \"disable_flash_protection: Initial setup\""),
	     ct:pal("Keeping the board in Uboot mode"),

             {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:", [{timeout,180000},no_prompt_check]),
	     timer:sleep(1000),
	     ok = ct_telnet:send(rs232, "\n"),
	     {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,5000},no_prompt_check]),

             ct:pal("In Uboot mode");

	{_, _} ->
             ok = rct_rs232:login(rs232),
	     ok = ct_telnet:send(rs232,"pgh_restartbrd -t \"disable_flash_protection: Initial setup\""),
	     ct:pal("Keeping the board in Uboot mode"),

             {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:", [{timeout,180000},no_prompt_check]),
	     timer:sleep(1000),
	     ok = ct_telnet:send(rs232, "\n"),
	     {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,5000},no_prompt_check]),

             ct:pal("In Uboot mode")
    end,

    % Issue rcs2 command to bring up the board in flash protection disabled mode
       ct_telnet:send(rs232,"rcs 2"),
       ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),
       timer:sleep(12000),
       
    % Wait for the login prompt
       ok = rct_rs232:login(rs232),
       
    % Verify that Flash partition ( atleast one of ubootstage3 , uboot stage2 and dtb area ) is writable

       ct_telnet:send(rs232,"rcsversion -a | grep CXC1736593"),
       timer:sleep(5000),

       ct_telnet:send(rs232,"mtdinfo /dev/mtd3"),
     % {ok, _} = ct_telnet:expect(rs232, "Device is writable: .*true", [{timeout,10000},no_prompt_check]),
       
       timer:sleep(5000),
       ct:pal("Now the board should be in flash protection disabled mode"),
       ok.


%% ===========================================================================
%% @doc
%%           Test case to restore Vendor credentials
%% @end
%% ===========================================================================
enable_vendorcredentials(_Config) ->

ct_telnet:send(rs232, "\n"),

    case ct_telnet:expect(rs232,
	                        [{notprompt_a,"=> A"},
                                 {notprompt_b,"=> B"},
				 {ubootprompt,"=> "}],
 	                        [{timeout,5000},no_prompt_check]) of
        {ok, {ubootprompt,_}} ->
	     ct:pal("Board in Uboot mode"),
	     case is_secure_uboot() of
	                 true ->
			          restore_vc();

			 false ->
			          % Do nothing
				  ok
	     end;

	{_, _} ->
             ok = rct_rs232:login(rs232),
	     case is_secure_type3() of
	                 true ->
			          ok = ct_telnet:send(rs232,"pgh_restartbrd -t \"enable_vc_tc: Initial setup\""),
	                          ct:pal("Keeping the board in Uboot mode"),

                                  {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:", [{timeout,180000},no_prompt_check]),
	                          timer:sleep(1000),
	                          ok = ct_telnet:send(rs232, "\n"),
	                          {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,5000},no_prompt_check]),

                                  ct:pal("In Uboot mode and can proceed with VC restoration"),
				  restore_vc();

			 false ->
			          % Do nothing
				  ok
	     end

    end,

    ct_telnet:send(rs232,"reset"),
    timer:sleep(120000),

ok.
