%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_laptop_SUITE.erl %
%%% @author etxderb
%%% @copyright Ericsson AB 2013
%%% @version /main/R2A/1
%%% @doc ==Test autintegration with laptop, normal and semi cases ==
%%% Command $RCT_TOP/test/bin/rct_run.sh -stp Nodename -suite nl_laptop_SUITE
%%%  Example:
%%%           $RCT_TOP/test/bin/rct_run.sh -stp dus012 -suite nl_laptop_SUITE
%%% @end

-module(nl_laptop_SUITE).
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
%%% R2A/1      2014-10-09 etxderb     Created
%%% R2A/2      2014-10-10 etxderb     Corr + Added preparations of tftp files
%%% R2A/3      2014-10-10 etxderb     Corr uboot cmd. Now working for f-reset and normal.
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
         reset_then_normal/1,
	 normal/1,
	 reset/1]).

-export([debug/1]).

%% CMDs for UBOOT one line command.
%% TODO : adapt for duw and tcu !!!!!!
-define(CMD_LOADADDR, "setenv loadaddr 0x8000000").
-define(CMD_DHCP(Dhcp_path, Image), "dhcp "++ Dhcp_path++"/"++Image).
-define(CMD_TFTP(Dhcp_path, Dtb), "tftp 0x3cae8 "++Dhcp_path++"/"++Dtb).
-define(CMD_FDT, "fdt addr 0x3cae8").
-define(CMD_SETENV, "setenv bootargs3 console=ttyS0,115200n8 root=/dev/ram rw riohdid=10 init=/addons/bin/labloader.sh").
-define(CMD_BOOTM, "bootm 0x8000000").
-define(UBOOT_CMD3_DUS(Dhcp_path, Image, Dtb),
	    ?CMD_LOADADDR++";"++
	    ?CMD_DHCP(Dhcp_path, Image) ++ ";"++
            ?CMD_TFTP(Dhcp_path, Dtb) ++ ";"++
            ?CMD_FDT++";"++
            ?CMD_SETENV++";"++
            ?CMD_BOOTM).

-define(UBOOT_CMDS(Dhcp_patch, Image, Dtb), [?CMD_LOAD_ADDR,
                                             ?CMD_DHCP(DHCP_path, Image),
                                             ?CMD_TFTP(Dhcp_path, Dtb),
                                             ?CMD_FDT,
                                             ?CMD_SETENV,
                                             ?CMD_BOOTM]). 
-define(CMD_LAPTOP, 
	"/tmp/nl/autointegrate -D -v 1024 -p dilbert -u "
	"sftp://mauve@10.68.200.11/etxderb/ai/AIWS/SiteInstallationFile.xml").

%% Directories containing temporary support files
-define(TFTPDIR(Rel_tftp), filename:join("/proj/rcs-tmp/tftpboot", Rel_tftp)).
-define(NL_TOP, filename:join(os:getenv("OS_TOP"),
			      "NL/NL_CNX9012629/NL_CAX1033097")).
-define(NL_PATCH_PATH, filename:join(?NL_TOP, "patch")).
-define(NL_SERVFILES_PATH, filename:join(?NL_TOP, "test/serverfiles")).

%% Temporary support filenames
-define(NL_PATCHJFFS2, "nl_patch.tar.gz").
-define(NL_MODEFILE, "netloader_mode.txt").



%%% #---------------------------------------------------------
%%% #3.1   CODE FOR "STATIC" CT FUNCTIONS
%%% #---------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_power, rct_consserv, rct_rs232
%% @end
%%--------------------------------------------------------------------

suite()  ->
    [{ct_hooks, [{rct_htmllink,[]},
                 {rct_coli, {coli, [manual_connect]}},
	         {rct_power,power},
                 {rct_consserv,cs1},
                 {rct_rs232,console}]}].

%%--------------------------------------------------------------------
%% @doc
%% Default testcases when running suite (->not specifying specific tc)
%% @end
%%--------------------------------------------------------------------
all() ->
    [reset_then_normal].


%% @hidden
init_per_suite(Config) ->
    %% Prepare tftp dir
    Node = atom_to_list(ct:get_config({test_nodes,1})),
    ok = prepare_tftp_files(Node),
    Config.
%% @hidden
end_per_suite(C) ->
    %% Clean up tftp dir
    Node = atom_to_list(ct:get_config({test_nodes,1})),
    ok = delete_tftp_files(Node),
    C.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    Config.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR TESTCASE FUNCTIONS
%%% #---------------------------------------------------------


%%--------------------------------------------------------------------
%i% @spec reset_then_normal(Config) -> Config
%% @doc
%% Autointegrate from scratch (virgin case) and then again after factory reset.<br/><br/>
%%  Normal and reset are run in same tc to save testtime.
%%  See normal/1 and reset/1 for further information
%% @end
%%--------------------------------------------------------------------
%% Running all with normal, reset works but not this .... why?
reset_then_normal(Config) ->
  ct:pal("reset_then_normal called with : ~p~n", [Config]),
  ok = reset(Config),
  ok = normal(Config),
  ok.



%%--------------------------------------------------------------------
%% @spec normal(Config) -> Config
%% @doc
%% Autointegrate from scratch (virgin case) and after factory reset.<br/><br/>
%%    1 Break in U-boot<br/>
%%    2 Load linux and start netloader<br/>
%%    3 Wait for [networkloader] prompt <br/>
%%    4 Run autointegration command which performs downloads and unpacking<br/>
%%    5 Wait for reboot is finished and we have login prompt <br/>
%%    6 Login and use ecoli command to perform factory reset to reboot into nl mode
%%    7 Perform 3-5 again
%% @end
%%--------------------------------------------------------------------
normal(Config) ->
      %% In production, this will be done by the test equipment.
    ct:pal("normal called with : ~p~n", [Config]),
 
    %% 1 Reboot and wait for Uboot
    ok = rct_power:cycle(power),
    {ok, _} = ct_telnet:expect(console, "3:rd Stage Boot Loader", [{timeout,60000}]),
    timer:sleep(1000),
    ok = ct_telnet:send(console, "\n"),
    {ok, _} = ct_telnet:expect(console, "U-Boot>", [{timeout,5000}]),

    
    %% 2. Put the UBOOT command line
    %% ct:break("Ready to put ubootcmd"),  
    BT = proplists:get_value(board_type,
			     ct:get_config(ct:get_config({test_nodes,1}))),
    Cmd = ubootcmd(BT),
    ct_telnet:send(console, Cmd),

    %% 3. Wait for network loader
    {ok, _} = ct_telnet:expect(console, "Welcome to Autointegration", 
			       [{timeout,1200000},no_prompt_check]),
    timer:sleep(1000),

     ok = ct_telnet:send(console, "\n"),% control chars may appered after prompt

    %% 4 Run the autointegration command
    ct_telnet:send(console, ?CMD_LAPTOP),

    %% 5 wait for login prompt
    {ok, _} = ct_telnet:expect(console, 
			       "du1 login", 
			       [{timeout,300000},no_prompt_check]),
 
    ok. 
    


reset(Config) ->
      %% In production, this will be done by the test equipment.
    ct:pal("reset called with : ~p~n", [Config]),
 

%% 6 Login with coli and do factory reset.
    ok = rct_coli:connect(coli),

    %% Change authority and do the factory reset command. 
    %% This will perform reboot down to netloader
    {ok,_} = rct_coli:send(coli,
    			   "/misc/set_auth_level disabled", 
    			   ""),
    {ok,_} = rct_coli:send(coli,"/sys/factory_reset", "Factory reset successful, rebooting"),

    %% 7 Wait for netloader mode. 
    {ok, _} = ct_telnet:expect(console, "Welcome to Autointegration",
                               [{timeout,1200000},no_prompt_check]),

    timer:sleep(1000),

    ok = ct_telnet:send(console, "\n"),% control chars may appered after prompt

    %% 8 Run the autointgration command
    ct_telnet:send(console, ?CMD_LAPTOP),

    %% 9 Wait for du login prompt.
    {ok, _} = ct_telnet:expect(console, 
    			    "du1 login", 
    	                    [{timeout,300000},no_prompt_check]),

 
    ok. 
    


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%% Computes the uboot cmd given the settings in config.
ubootcmd("dus4101") ->
   %% Read from config file. ct have it stored in an ets table.
   Dhcp_dir = atom_to_list(ct:get_config({test_nodes,1})),
   Image = "linux-bootfs-dus.img",
   %%Dtb = ct:get_config({Node, dtb}),
   ?UBOOT_CMD3_DUS(Dhcp_dir, Image, "acp.dtb").





prepare_tftp_files(Rel_tftp) ->
    {ok, _} = copy(?NL_PATCHJFFS2, ?NL_PATCH_PATH, ?TFTPDIR(Rel_tftp)),
    {ok, _} = copy(?NL_MODEFILE, ?NL_SERVFILES_PATH, ?TFTPDIR(Rel_tftp)),
    ok.

%% Copy (and potentially overwrite old versions) File from one dir to another
copy(Filename, Fromdir, Todir) ->
    file:copy(filename:join(Fromdir, Filename), 
	      filename:join(Todir, Filename)).

delete_tftp_files(Rel_tftp) ->
    ok = file:delete(filename:join(?TFTPDIR(Rel_tftp), ?NL_PATCHJFFS2)),
    ok = file:delete(filename:join(?TFTPDIR(Rel_tftp), ?NL_MODEFILE)).
		
%%% #---------------------------------------------------------
%%% #3.3   CODE FOR EXPORTED DEBUG FUNCTIONS
%%% #---------------------------------------------------------
debug(cp) ->
    prepare_tftp_files("dus012");
debug(delete) ->
    delete_tftp_files("dus012").

