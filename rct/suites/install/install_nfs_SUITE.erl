%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	install_nfs_SUITE.erl %
%%% @author elavaku
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/12
%%% @doc ==Installs ARM board==
%%% @end

-module(install_nfs_SUITE).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/12').
-date('2015-07-02').
-author('elavaku').
-include_lib("common_test/include/ct.hrl").
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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-07-03 etxkols     Created
%%% R2A/2      2014-07-04 etxkols     Removed temp fix for qfredjo
%%% R2A/3      2014-07-07 etxkols     Fixed missunderstanding
%%% R2A/4      2014-07-07 etxkols     Remove compiler warnings
%%% R2A/5      2014-07-24 etxarnu     Fixed correct fpga for tcu03
%%% R3A/1      2014-09-10 etxkols     Allow P revisions on uboot.
%%%                                   VC and secure boot allowed
%%% R3A/2      2014-09-10 etxkols     Fix
%%% R3A/3      2014-11-04 etxkols     Fix for 10.86.147.0 network
%%% R3A/5      2014-12-01 eerijan     Mailbox area update to boot  
%%%				      in test mode.
%%% R3A/6      2014-12-08 elavaku     Workaround for corrupted 
%%%                                   login: prompt
%%% R3A/7      2014-12-16 elavaku     Fail the suite with TFTP errors 
%%%                                   or FPGA load failures
%%% R3A/8      2015-01-15 elavaku     Handling the case of secure 
%%%                                   dusx2 boards with new uboot 
%%%                                   product numbers
%%% R3A/9      2015-05-11 eramkka     tcu04 target nfs installation
%%% R3A/10     2015-05-22 etxkols     Fix for Ki10
%%% R3A/11     2015-05-22 etxkols     Fix for sftp_server in Ki10
%%% R3A/12     2015-07-02 elavaku     Support for arria10
%%% ----------------------------------------------------------
%%% JenkinsNode
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         install/1]).


%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_power, rct_consserv, rct_ssh, rct_rs232, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [%{timetrap,{minutes,60}},
     {ct_hooks, [{rct_htmllink,[]},
                 {rct_power,power},
                 {rct_consserv,cs1},
		 {rct_ssh,{ssh,[manual_connect]}},
                 {rct_rs232,console},
                 {cth_conn_log,[]}]}].

%% @hidden
init_per_suite(Config) ->
    Config.
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.
%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [install]. 

%%--------------------------------------------------------------------
%% @doc 
%% Install ARM board using nfs (test of factory loader)<br/><br/>
%%    - Break in U-boot<br/>
%%    - Wait for login prompt and login
%% @end
%%--------------------------------------------------------------------
install(_Config) ->
    [{host, Host},{username, _Username},{password, _Password}] = ct:get_config(sftp_server),
    Hw = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    IP = ct:get_config({Hwa,ssh_lmt_ipv4,ssh}),
    case ct:get_config({Hwa,board_type}) of
	BOARDTYPE when BOARDTYPE == "tcu03";
	               BOARDTYPE == "tcu0401";
		       BOARDTYPE == "dus5201";
		       BOARDTYPE == "dus3201" ->
	    FpgaFile = case BOARDTYPE of
			   "tcu03" -> "top_taipan.periph.rbf";
			   "tcu0401" -> "top_katla.periph.rbf";
			    _      -> "top_cobra.periph.rbf"
		       end,
            [One,Two,Three,_] = string:tokens(IP, "."),
	    ok = break_uboot(BOARDTYPE,3,3),
	    ok = send_commands(["setenv tftpdir " ++ Hw,
				"setenv ipaddr " ++ IP,
				"setenv netmask 255.255.255.0",
				"setenv gatewayip "++One++"."++Two++"."++Three++".1",
				"setenv serverip " ++ Host,
				"setenv bootargs console=ttyAMA0,115200n8 earlyprintk debug initrd=0x8000000,30M ramdisk_size=30000 ip=${ipaddr}:${serverip}:${gatewayip}:${netmask}::eth0:off tftpdir=${tftpdir} rdinit=/addons/bin/factoryloader_script.sh",
				"rcs 1;",
				"mw 0x2000000c 0xAFFEEEEE;",
				"tftp 0x07000000 ${tftpdir}/"++ FpgaFile,
				"fpga load mem 0x07000000 344500;tftp 0x04000000 ${tftpdir}/linux.img"]),
	    ok = ct_telnet:send(console,"tftp 0x08000000 ${tftpdir}/bootfs.cpio.gz;bootm 0x04000000 - 0x05000000"),
	    case ct_telnet:expect(console, "login:", [{timeout,600000},no_prompt_check]) of
		{ok, _} ->
		    ok;
		_ ->
		    ok = ct_telnet:send(console, ""),
		    {ok, _} = ct_telnet:expect(console, "login:", [{timeout,20000},no_prompt_check]) % login: prompt can be corrupted 
	    end,
	    
	    ok = rct_rs232:login(console);
	BOARDTYPE ->	
	    ct:log(lightred, "Wrong boardtype~s NOT ARM",[BOARDTYPE]),
	    ct:fail("not arm board")
    end.

send_commands([]) ->
    ok;
send_commands([Cmd|T]) ->
    ok = ct_telnet:send(console,Cmd),
    case ct_telnet:expect(console, [{error,"TFTP error:"},
                                    {fpga_error,"RCS ERROR: CPM1 FPGA loading failed"},  
                                    {prompt_received,"=> "}],
                                      [{timeout,10000},no_prompt_check]) of
	{ok,{error,_}} ->
	    ct:log(lightred, "TFTP error after command ~p",[Cmd]),
	    ct:fail("TFTP error after command ~p",[Cmd]);
        {ok,{fpga_error,_}} ->
	    ct:log(lightred, "FPGA loading error after command ~p",[Cmd]),
	    ct:fail("FPGA error after command ~p",[Cmd]);
        {ok,{prompt_received,_}} ->
	    send_commands(T);	    
	_ ->
	    ct:log(lightred, "Did not receive => prompt after command ~p",[Cmd]),
	    ct:fail("Did not receive => prompt after command ~p",[Cmd])
    end.

break_uboot(_,0,_) ->
    ct:log(lightred, "Could not break board in uboot"),
    ct:fail("Could not break uboot");
break_uboot(BOARDTYPE,N,M) ->
    case N < M of
	true ->
	    ct:log(yellow,"Could not break board in uboot, retry ~p more times",[N]);
	_ ->
	    ok
    end,
    case rct_power:cycle(power) of
	ok ->
	    Result = case BOARDTYPE of
			"tcu03" ->
			    case ct_telnet:expect(console, ["Ericsson Version: 2/CXC1736593/03.*\\)",
                                                            "Ericsson Version: 4/CXC1736593/03.*\\)",
                                                            "Ericsson Version: 7/CXC1736593/03.*\\)"], [{timeout,30000}]) of
				{ok, Match} ->
				    case re:run(Match,"Ericsson Version: [0-9]+/CXC1736593/03_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end;
			 "tcu0401" ->
			    case ct_telnet:expect(console, ["Ericsson Version: 2/CXC1736593/04.*\\)",
							    "Ericsson Version: 4/CXC1736593/04.*\\)",
							    "Ericsson Version: 7/CXC1736593/04.*\\)"], [{timeout,30000}]) of
				{ok, Match} ->
				    case re:run(Match,"Ericsson Version: [0-9]/CXC1736593/04_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end;		    
			"dus5201" ->
			    case ct_telnet:expect(console, ["Ericsson Version: 2/CXC1736593/52.*\\)", 
			                                    "Ericsson Version: 4/CXC1736593/52.*\\)",
                                                            "Ericsson Version: 7/CXC1736593/52.*\\)"], [{timeout,30000}]) of
				{ok, Match} ->
				    case re:run(Match,"Ericsson Version: [0-9]+/CXC1736593/52_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end;
			"dus3201" ->
			    case ct_telnet:expect(console, ["Ericsson Version: 2/CXC1736593/32.*\\)",
			                                    "Ericsson Version: 4/CXC1736593/32.*\\)",
                                                            "Ericsson Version: 7/CXC1736593/32.*\\)"], [{timeout,30000}]) of
				{ok, Match} ->
				    case re:run(Match,"Ericsson Version: [0-9]+/CXC1736593/32_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end
		    end,
	    case Result of
		error ->
		    break_uboot(BOARDTYPE,N-1,M);
		Result ->
		    case ct_telnet:expect(console, "Hit any key to stop autoboot:", [{timeout,30000},no_prompt_check]) of
			{ok, _} ->
			    ct_telnet:send(console, "\n"),
			    case ct_telnet:expect(console, "=> ", [{timeout,10000},no_prompt_check]) of
				{ok, _} ->
				    ok;
				_ ->
				    break_uboot(BOARDTYPE,N-1,M)
			    end;
			_ ->
			    break_uboot(BOARDTYPE,N-1,M)
		    end
	    end;
	_ ->
	    break_uboot(BOARDTYPE,N-1,M)
    end.

