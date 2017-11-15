%% Author: elavaku
%% Created: Mar 27, 2014
%%% @copyright Ericsson AB 2014-2016
%% Description: UBOOT test suite


-module(uboot_SUITE).
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
%%%
%%% EXPORT LISTS
%%% ----------------------------------------------------------
%%% EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-compile([export_all]).
-export([init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-define(DEFAULT_TIMEOUT, 800000).
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
%%      Init per testcase which gets executed
%%              for each testcase.
%%              Connects to the board and puts into Uboot prompt at
%%              the start of test suite for the first time, next time
%%              onwards continues to execute in UBoot mode.
%%
%% @end
%% ===========================================================================
init_per_testcase(TestCase, Config) ->

% Mention the testcases that requires to be started in type3 only to reduce
% unnecessary time delay in bringing up the board again.

  Test1 = "verify_hwwatchdog",
  Test2 = "verify_flash_protection",
  Test3 = "bootcmds_testcase",
  Test4 = "verify_semiPermStorage",
  Test5 = "verify_errorescalation",
  Test6 = "verify_L3cache",
  Test7 = "verify_fwupgrade_upgradablepartition",

  ct_telnet:send(rs232, "\n"),
  case ct_telnet:expect(rs232,
                         [{notprompt_a,"=> A"},
                          {notprompt_b,"=> B"},
                          {ubootprompt,"=> "}],
                          [{timeout,5000},no_prompt_check]) of
  {ok, {ubootprompt,_}} ->
       ct:pal("Board in Uboot mode");

  {_, _} ->
       ok = rct_rs232:login(rs232),
       case ((Test1 == atom_to_list(TestCase)) orelse (Test2 == atom_to_list(TestCase)) 
               orelse (Test3 == atom_to_list(TestCase)) orelse (Test4 == atom_to_list(TestCase))
               orelse (Test5 == atom_to_list(TestCase)) orelse (Test6 == atom_to_list(TestCase))
               orelse (Test7 == atom_to_list(TestCase))) of

          true ->
               ct:pal("Board is in type3 mode");

          false ->
               ok = ct_telnet:sendf(rs232,"pgh_restartbrd -t \"~s: Initial setup\"",[TestCase]),
               ct:pal("Keeping the board in Uboot mode"),

               {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:",
                                                   [{timeout,180000},no_prompt_check]),
               timer:sleep(1000),
               ok = ct_telnet:send(rs232, "\n"),
               {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,5000},no_prompt_check]),

                   ct:pal("In Uboot mode")
       end

  end,

  ct:pal("Starting ~p", [TestCase]),
  Config.

%% ===========================================================================
%% @doc
%%      Executes at the end of each testcase
%% @end
%% ===========================================================================
end_per_testcase(TestCase,Config) ->
    Test_case = "verify_fwupgrade_upgradablepartition",
    Status=?config(tc_status,Config),
    case (((Test_case == atom_to_list(TestCase)) andalso (Status /= ok)) orelse (Status == skip)) of
      true ->
          ok = ct_telnet:send(rs232, "\n"),
          case ct_telnet:expect(rs232, "=> ", [{timeout,1000},no_prompt_check]) of
                {ok, _} ->
                          cleanup_ubootenv();
                {_, _} ->
                          case rct_power:cycle(pow) of
                                  ok ->
                                       {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:",
                                                                        [{timeout,180000},no_prompt_check]),
                                       ok = ct_telnet:send(rs232, "\n"),
                                       {ok, {ubootprompt,_}} =  ct_telnet:expect(rs232,[{notprompt_a,"=> A"},
                                                                                        {notprompt_b,"=> B"},
                                                                                        {ubootprompt,"=> "}],
                                                                            [{timeout,3000},no_prompt_check]),
                                       ct:pal("Brought into uboot mode forcefully"),
                                       cleanup_ubootenv();
                                  _ ->
                                       ct:fail("Could not break the board in uboot mode")
                          end
         end;

     false ->
         ok
    end,

    ct:pal("Ending ~p", [TestCase]).

all() ->
    [{group, cont}].

%% ===========================================================================
%% @doc
%%      Groups <br>
%% @end
%% ===========================================================================
groups() ->
    [

      {common, [], [verify_L3cache, verify_hwwatchdog, verify_flash_protection,
                    verify_ubootcommands, verify_ioexp, env_testcase,
                    hwlog_flashcmds_testcase, memorycmds_testcase,
                    macaddresses_testcase, memtests_testcase, verify_type2data,
                    dtb_redundancy_testcase, param_redundancy_testcase,
                    uboot_redundancy_testcase, verify_load_process,
                    verify_errorescalation, verify_rcs_1_testmode]},

      {normal, [], [{group, common}, restarts_testcase, verify_secureboot,
                    reboot_to_normal]},

      {cont, [], [{group, common}, verify_semiPermStorage,
                   bootcmds_testcase, verify_mailbox_for_ubootrestarts,
                   verify_secureboot, reboot_to_normal,
                   verify_fwupgrade_upgradablepartition]},

      {release, [], [{group, common}, verify_semiPermStorage,
                   bootcmds_testcase, verify_mailbox_for_ubootrestarts,
                   verify_secureboot, verify_ecc, reboot_to_normal,
                   verify_fwupgrade_upgradablepartition]}

    ].

%%%%%%%%%%%%%%%%%%%%% CALLING the MAKE for different group types %%%%%%%%%%%%%%%%%%%%%
%%                                                                                  %%
%% dus32, dus52, tcu03, tcu04 gerrit pushes                                         %%
%%   "make -C rct ${NODE_NAME}-uboottest BOARDS=${NODENAME} RCT_GROUP=normal"       %%
%% dus32, dus52, tcu03, tcu04  continuous job                                       %%
%%   "make -C rct ${NODE_NAME}-uboottest BOARDS=${NODENAME} RCT_GROUP=cont"         %%
%% For verification of any specific release                                         %%
%%   "make -C rct ${NODE_NAME}-uboottest BOARDS=${NODENAME} RCT_GROUP=release"      %%
%%                                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% ===========================================================================
%% @doc
%%     Converts integer to string <br/>
%% @end
%% ===========================================================================
integer_to_string(N) ->
    lists:flatten(io_lib:format("~p", [N])).


%% ===========================================================================
%% @doc
%%      Converts list to string appended by newline<br/>
%% @end
%% ===========================================================================
list_to_string(List) when is_list(List)->
    F = fun(X) ->
                X ++ "\n"
        end,
    lists:flatmap(F, List).

%% ===========================================================================
%% @doc
%%      Converts list to string
%% @end
%% ===========================================================================
list_to_stringtrim(List) when is_list(List)->
    F = fun(X) ->
                X ++ ""
        end,
    lists:flatmap(F, List).



%% ===========================================================================
%% @doc
%%          Extract value with memory commands at a particular address
%% @end
%% ===========================================================================
extract_val_memcmds(Addr, Data, Word_num, Str) ->

   Sublist = lists:filter(fun(X) -> string:str(X, Addr)>0 end, Data),

   if
     (Str == 1) ->
      string:sub_string(string:sub_word(list_to_string(Sublist),Word_num), 1, 10);
     true ->
          string:to_integer(string:sub_word(list_to_string(Sublist),Word_num))
   end.


%% ===========================================================================
%% @doc
%%          Return the product revision of the board
%% @end
%% ===========================================================================
product_revision() ->

   ct_telnet:send(rs232, "printenv productrevision"),
   timer:sleep(1000),
   {ok, Rev} = ct_telnet:get_data(rs232),

   Sublist = lists:filter(fun(X) -> string:str(X, "=")>0 end, Rev),

   ct:pal ("rev: ~p",[Rev]),

   Product_revision = string:sub_string(list_to_string(Sublist), 17, 19),

   Product_revision.

%% ===========================================================================
%% @doc
%%     Verify the Uboot prompt
%% @end
%% ===========================================================================

verify_ubootprompt() ->

   {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:", [{timeout,180000},no_prompt_check]),
   ok = ct_telnet:send(rs232, "\n"),
   {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,1000},no_prompt_check]).

%% ===========================================================================
%% @doc
%%     Wait for Uboot prompt
%% @end
%% ===========================================================================

wait_for_ubootprompt(Time_out) ->
   {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,Time_out},no_prompt_check]).

%% ===========================================================================
%% @doc
%%     Verify the AXM1.0 or AXM1.1
%% @end
%% ===========================================================================
is_axm1_0() ->

   ct_telnet:send(rs232,"ncr read 0x156.0x0.0x34"),
   Axm1_0 =  case ct_telnet:expect(rs232, "0x01800009", [{timeout,1000},no_prompt_check]) of
                {ok,_} -> true;

                {_,_} -> false
   end,

   Axm1_0.

%% ===========================================================================
%% @doc
%%         Function to return if a board is a arrai10 board or stratix board
%% @end
%% ===========================================================================
get_boardtype(Target, Revision) ->
  ct_telnet:send(rs232,"fdt addr 0x05000000"),
  wait_for_ubootprompt(1000),

  ct_telnet:send(rs232,"fdt print \/hwdb_icm fpga_type"),
  case ct_telnet:expect(rs232, "fpga_type = \"arria10\"", [{timeout,1000},no_prompt_check]) of
         {ok,_} ->
                  if
                    ( Revision == "R10.*") ->
                              BoardType = Target ++ "_A10_closed";
                     true ->
                              BoardType = Target ++ "_A10_open"
                  end;
         {_,_} ->
                  if
                    ( Revision == "R3.*") ->
                              BoardType = Target ++ "_SV_closed";
                    ( Revision == "R7.*") ->
                              BoardType = Target ++ "_SV_closed";
                     true ->
                              BoardType = Target ++ "_SV_open"
                  end
  end,

  BoardType.

%% ===========================================================================
%% @doc
%%         Function to return if a board is secure or not and the boardtype
%% @end
%% ===========================================================================
is_secure() ->
  timer:sleep(5000),
  ct_telnet:send(rs232, "printenv productrevision"),
  timer:sleep(3000),
  {ok, Data} = ct_telnet:get_data(rs232),
  case re:run(Data, "productrevision=(.*)", [{capture, [1], list}]) of {match, Revtemp} -> ok end,
  Rev = list_to_stringtrim(Revtemp),

  ct_telnet:send(rs232, "printenv productnumber"),
  case ct_telnet:expect(rs232, [{secure_tcu03,"productnumber=KDU 137 926/11"},
                                 {secure_dus52,"productnumber=KDU 137 925/31"},
                                 {secure_dus32,"productnumber=KDU 137 925/41"},
                                 {dus32,"productnumber=KDU 137 925/4"},
                                 {dus52,"productnumber=KDU 137 925/3"},
                                 {tcu03,"productnumber=KDU 137 926/1"}, 
                                 {tcu04,"productnumber=KDU 137 815/1"}],
                                      [{timeout,5000},no_prompt_check]) of

       {ok, {secure_tcu03,_}} -> Boardtype = "cpm1_03",
                                 Secure = true;

       {ok, {secure_dus52,_}} -> Boardtype = get_boardtype("cpm1_52",Rev),
                                 Secure = true;

       {ok, {secure_dus32,_}} -> Boardtype = get_boardtype("cpm1_32",Rev),
                                 Secure = true;

       {ok, {dus32,_}} -> Boardtype = get_boardtype("cpm1_32",Rev),
                          if
                             ((Rev < "R5F") andalso (Boardtype == "cpm1_32_SV_open")) ->
                                                                      Secure = false;
                             ((Rev < "R8C") andalso (Boardtype == "cpm1_32_A10_open")) ->
                                                                      Secure = false;
                             true ->
                                    Secure = true
                          end;

       {ok, {dus52,_}} -> Boardtype = get_boardtype("cpm1_52",Rev),
                          if
                             ((Rev < "R5F") andalso (Boardtype == "cpm1_52_SV_open")) ->
                                                                      Secure = false;
                             ((Rev < "R8C") andalso (Boardtype == "cpm1_52_A10_open")) ->
                                                                      Secure = false;
                             true ->
                                    Secure = true
                          end;

       {ok, {tcu03,_}} -> Boardtype = "cpm1_03",
                          Secure = false;
       
       {ok, {tcu04,_}} -> Boardtype = "cpm1_04",
                          ct_telnet:send(rs232, "printenv productrevision"),
                          case ct_telnet:expect(rs232, [{p1a,"productrevision=P1A"},
                                                       {p1b,"productrevision=P1B"},
                                                       {p1b_1,"productrevision=P1B/1"}],
                                                      [{timeout,1000},no_prompt_check]) of
                                  {ok,_} -> Secure = false;
              
                                  {_, _} ->
                                            % confirm that it is a R revsion of the board
                                            ct_telnet:send(rs232, "printenv productrevision"),
                                            case ct_telnet:expect(rs232, "productrevision=R",
                                                                [{timeout,1000},no_prompt_check]) of
                                                   {ok,_} -> Secure = true;

                                                   {_, _} -> Secure = false,
                                                             ct:fail("Unknown version of TCU04 board detected")

                                            end

                          end;

       {_, _} -> Boardtype = "undefined",
                 Secure = undefined, 
                 ct:fail("Unknown board type")

   end,

   {Secure, Boardtype}.

%% ===========================================================================
%% @doc
%%    Restore the hwlog contents
%% @end
%% ===========================================================================
restore_hwlog_entries(OrigOp, Stptr, Val_Stptr, Wptr, Val_Wptr) ->

  ct:pal("Restoring hwlog entries"),

  ok = ct_telnet:send(rs232, "\n"),
  case ct_telnet:expect(rs232,
                            [{notprompt_a,"=> A"},
                             {notprompt_b,"=> B"},
                             {ubootprompt,"=> "}],
                            [{timeout,5000},no_prompt_check]) of
    {ok, {ubootprompt,_}} ->
          ct:pal("Board in Uboot mode and will proceed with hwlog restoration");

    {_, _} ->
          ct:fail("Dont find the board in Uboot to restore the hwlog entries")
    end,


% Write back the saved old hwlog back to flash

  ct_telnet:send(rs232,"hwlog erase"),
  wait_for_ubootprompt(8000),

  % debug traces

  ct_telnet:send(rs232,"md 0x23f00000"),
  wait_for_ubootprompt(5000),

  % Detect the flash and copy the old entries from memory to flash

  ct_telnet:send(rs232,"sf probe"),
  wait_for_ubootprompt(2000),
  ct_telnet:send(rs232,"sf probe; sf write 0x23f00000 0x00780000 0x60000"),
  wait_for_ubootprompt(50000),

  ct_telnet:sendf(rs232,"mw ~s ~s",[Stptr, Val_Stptr]),
  ct_telnet:sendf(rs232,"mw ~s ~s",[Wptr, Val_Wptr]),
  wait_for_ubootprompt(3000),

% Verify the hwlog entry is written back correctly

  ct_telnet:send(rs232,"hwlog read"),
  timer:sleep(15000),
  {ok, Data} = ct_telnet:get_data(rs232),
  % Ignore "ramlog is full" error string
  FinalOp = lists:filter(fun(X) -> string:str(X, "ramlog is full")==0 end, Data),
  timer:sleep(5000),

  if
     (FinalOp == OrigOp) ->
         ct:pal("HWlog reverted to its old content");
     true ->
         ct:pal("Not able to revert the HWlog contents"),
%        ct:pal("Manual copy with sf write 0x23f00000 0x00780000 0x60000 and verify!"),

         % Keep it clean by erasing the hwlog area
         ct_telnet:send(rs232,"hwlog erase"),
         wait_for_ubootprompt(15000),
         ct:fail("Reverting the hwlog to its old contents failed")
  end,


% Reset used temporary memory with 0s

  ct_telnet:send(rs232,"mw 0x23f00000 0 0x3000"),
  wait_for_ubootprompt(10000), 
ok.

%% ============================================================================
%% @doc
%%       Function restoring the DTB version after testing redundancy
%% end
%% ============================================================================

restore_dtb_version(DtbRev, Dtb_avail, Data_dtb_a, Data_dtb_b) ->

  if
     (Dtb_avail == true) ->
               % restoring DTB(2) area
               ct_telnet:send(rs232,"sf probe; sf update 0x23f00000 0x006c0000 0x20000"),
               wait_for_ubootprompt(3000),
               ct_telnet:send(rs232,"mw 0x23f00000 0 0x1000");

     (Dtb_avail == false) ->
               % Erase the DTB(2) area
               ct_telnet:send(rs232,"sf probe; sf erase 0x006c0000 0x20000"),
               wait_for_ubootprompt(10000)

  end,


  ct_telnet:send(rs232, "reset"),

  verify_ubootprompt(),

  if
     (Dtb_avail == false) ->
         ct_telnet:send(rs232, "printenv dtb_a_sequence"),
         {ok, _} = ct_telnet:expect(rs232, "Error: \"dtb_a_sequence\" not defined", [{timeout,3000},no_prompt_check]),

         ct_telnet:send(rs232, "printenv dtb_b_sequence"),
         {ok, _} = ct_telnet:expect(rs232, "Error: \"dtb_b_sequence\" not defined", [{timeout,3000},no_prompt_check]);
     (Dtb_avail == true) ->
         ct_telnet:send(rs232,"printenv dtb_a_sequence"),
         {ok, Finaldata_dtb_a} = ct_telnet:get_data(rs232),

         if
            (Finaldata_dtb_a == Data_dtb_a) ->
               ct:pal("Restored dtb_a_sequence");

            true ->
               ct:fail("Restoring dtb_a sequence number failed. Initial: ~p, Final: ~p ",[Data_dtb_a, Finaldata_dtb_a])
         end,

         ct_telnet:send(rs232,"printenv dtb_b_sequence"),
         {ok, Finaldata_dtb_b} = ct_telnet:get_data(rs232),

         if
            (Finaldata_dtb_b == Data_dtb_b) ->
               ct:pal("Restored dtb_b_sequence");

            true ->
               ct:fail("Restoring dtb_b sequence number failed. Initial: ~p, Final: ~p ",[Data_dtb_b, Finaldata_dtb_b])
         end
  end,

  ct_telnet:send(rs232,"rcs 1"),
  wait_for_ubootprompt(20000),

  ct_telnet:send(rs232,"fdt print \/ revision_dtb"),

  {ok, FinalDtbRev} = ct_telnet:get_data(rs232),

  DtbRev_Final = lists:filter(fun(X) -> string:str(X, "revision_dtb =")>0 end, FinalDtbRev),

  if
     (DtbRev_Final == DtbRev) ->
        ct:pal("Restored the DTB version back to its original"),
        ok;
     true ->
        ct:comment("Restoration of DTB version failed, Final: ~p, Initial: ~p",[FinalDtbRev, DtbRev])

  end.

%% ===================================================================================
%% @doc
%%       Function to restore the secondary HW parameter area after testing redundancy
%% @end
%% ===================================================================================

restore_secondary_paramarea(Param_address) ->

  Param_valid_1 = "Parameters: Watchdog 0 A\/B Valid 1\/0 A\/B ",
  Uboot_stop = "Hit any key to stop autoboot:",
  Warn_invalid2 = "RCS WARNING: Parameter area B invalid, rewrite area B with a copy from area A ... Successful!",

  ct_telnet:send(rs232, "printenv productname"),
  case ct_telnet:expect(rs232,
                             [{product_dus52,"productname=DUS 52"},
                              {product_tcu03,"productname=TCU 03"},
                              {product_dus32,"productname=DUS 32"},
                              {product_tcu04,"productname=TCU 04"}],
                                     [{timeout,1500},no_prompt_check]) of

      {ok, {product_dus52,_}} ->

               case is_axm1_0() of

                    true ->
                           file:delete("/proj/rcs-tmp/tftpboot/config-cpm52-uboot-axm1-0.bin");

                    false ->
                           file:delete("/proj/rcs-tmp/tftpboot/config-cpm52-uboot-axm1-1.bin")

               end;


      {ok, {product_tcu03,_}} ->
               
               case is_axm1_0() of

                    true ->
                          file:delete("/proj/rcs-tmp/tftpboot/config-cpm03-uboot-axm1-0.bin");

                    false ->
                          file:delete("/proj/rcs-tmp/tftpboot/config-cpm03-uboot-axm1-1.bin")

               end;

      {ok, {product_dus32,_}} ->

               case is_axm1_0() of

                    true ->
                          ok;

                    false ->
                          file:delete("/proj/rcs-tmp/tftpboot/config-cpm32-uboot-axm1-1.bin")

               end;

      {ok, {product_tcu04,_}} ->
               file:delete("/proj/rcs-tmp/tftpboot/config-cpm04-uboot.bin");

      {_,_}  ->  
              ct:fail("Unknown board type")

  end,

  ct_telnet:sendf(rs232,"sf probe; sf erase ~s 0x10000",[Param_address]),
  wait_for_ubootprompt(10000),

  % Verify that Stage3 corrects the erased param area
  ok = ct_telnet:send(rs232, "reset"),

  case ct_telnet:expect(rs232, [Param_valid_1, Warn_invalid2], [sequence, {halt,[Uboot_stop]},{timeout,10000},no_prompt_check]) of
       {ok,_} ->
          verify_ubootprompt(),
          ct:pal("Uboot stage2 copied the valid PARAM from area1 to area2");

       {_, _} ->
          ct:fail("Uboot stage2 correction of invalid area failed")

  end,

  ok.

%% ===============================================================================
%% @doc
%%         Function to restore the uboot-stage3 after testing redundancy
%% @end
%% ===============================================================================

restore_uboot_state(UbootRev) ->
  Uboot_stop = "Hit any key to stop autoboot:",
  Wdog_str = "U-Boot: Watchdog 0 A\/B Valid 1\/.* A\/B Sequence 0\/0 => A",

  timer:sleep(1000),

  ct_telnet:send(rs232,"setenv uboot_a_sequence"),
  ct_telnet:send(rs232,"setenv uboot_b_sequence"),
  ct_telnet:send(rs232,"saveenv"),
  {ok, _} = ct_telnet:expect(rs232, ["Erasing SPI flash...Writing to SPI flash...done", "=> "],
                                     [sequence, {timeout,20000},no_prompt_check]),
  ct_telnet:send(rs232, "\n"),
  {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,5000},no_prompt_check]),

%   ct_telnet:send(rs232,"sf probe; sf erase 0x00360000 0x80000"),
%   wait_for_ubootprompt(10000),

%  ct_telnet:send(rs232, "\n"),

%  {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,20000},no_prompt_check]),

  ct_telnet:send(rs232,"reset"),

  case ct_telnet:expect(rs232, [Wdog_str, "Ericsson Version: .*\/CXC1736593"], [sequence, {halt,[Uboot_stop]},
                                                                            {timeout,50000},no_prompt_check]) of

    {ok, _} ->
          verify_ubootprompt(),
          ct_telnet:send(rs232,"ver"),

          timer:sleep(1000),
          {ok, FinUbootRev} = ct_telnet:get_data(rs232),
          Output = lists:filter(fun(X) -> string:str(X, "U-Boot")>0 end, FinUbootRev),
          ct:comment("Uboot redundancy tested sucessfully, FinUbootRev :~p, InitialUbootRev:~p",[Output, UbootRev]);

    {_, _} ->
          ok = ct_telnet:send(rs232, "\n"),
          ct_telnet:expect(rs232, "=> ", [{timeout,500},no_prompt_check]),
          ct:fail("Uboot redundancy testcase restoration failed")
  end.

%%--------------------------------------------------------------------
%% @doc
%%       Switching from root to testbox user
%% @end
%%--------------------------------------------------------------------
switch_to_testbox()->
  ok = rct_rs232:login(rs232),
  % Login as Testbox user
  ct_telnet:cmd(rs232,"logout"),

  ct_telnet:cmd(rs232,"testbox"),
  ct_telnet:cmd(rs232,"testbox"),

  timer:sleep(5000),

  ct_telnet:send(rs232, "\n"),
  {ok, _} = ct_telnet:expect(rs232, "testbox@.*:",[{timeout,3000},no_prompt_check]).


%%--------------------------------------------------------------------
%% @doc
%%       Verify the testbox mode
%% @end
%%--------------------------------------------------------------------
verify_testbox_mode()->
 % Check that the testbox daemon is created and available
 ct_telnet:send(rs232, "ps -C testboxd | grep testboxd; echo \"RESULT=$?\""),
 case ct_telnet:expect(rs232, "RESULT=0", [{timeout,2000}, no_prompt_check]) of
         {ok, _} -> 
                ok = ct_telnet:send(rs232,"testbox version"),
                ct_telnet:expect(rs232, "BPM version info:",[{timeout,10000},no_prompt_check]),
                ok;

         _  ->  ct_telnet:send(rs232, "pgh_spawn -a /bin/testboxd --config /bin --inhibit_daemonize"),
                ct_telnet:send(rs232, "ps -C testboxd | grep testboxd; echo \"RESULT=$?\""),
                {ok, _} = ct_telnet:expect(rs232, "RESULT=0", [{timeout,2000}, no_prompt_check])
 end,

 % Wait for 15 more seconds before reattempting
 timer:sleep(15000),
 ok.      

%%--------------------------------------------------------------------
%% @doc
%%       Switch to root
%% @end
%%--------------------------------------------------------------------
switch_to_root()->
 % Login as root user
 ct_telnet:cmd(rs232,"logout"),

 ct_telnet:cmd(rs232,"root"),
 ct_telnet:cmd(rs232,"root"),
 timer:sleep(2000),

 ct_telnet:send(rs232, "\n"),
 {ok, _} = ct_telnet:expect(rs232, "root@.*:",[{timeout,3000},no_prompt_check]).

%%--------------------------------------------------------------------
%% @doc
%%       Boot the kernel and verify that it booted up with kernel as per
%%       boot count
%% @end
%%--------------------------------------------------------------------

boot_the_kernel(Boot_count) ->

% Check the bootcount value from mailbox

   ct_telnet:send(rs232,"md 0x20000004"),
   timer:sleep(1500),
   {ok, InitData} = ct_telnet:get_data(rs232),
   {InitValue,_} = extract_val_memcmds("20000004:", InitData, 2, 0),

   ct:pal("Initial boot count value :~p",[InitValue]),

   if
      ( Boot_count < 3) ->
                             Kernel = configured,
                             Load_kernel = "RCS: boot_count.*load configured kernel";
      ( Boot_count < 6) ->
                             Kernel = fallback,
                             Load_kernel = "RCS: boot_count.*load fallback kernel";
      ( Boot_count == 6 ) ->
                             Kernel = nl_type3,
                             Load_kernel = "RCS: boot_count.*load network loader \\(type3\\) kernel";
      ( Boot_count > 6 ) ->
                             Kernel = nl_type2,
                             Load_kernel = "RCS: boot_count.*load network loader \\(type2\\) kernel"
   end,

   ct:pal("Start the procedure to load ~p kernel",[Kernel]),

   ct_telnet:sendf(rs232,"mw 0x20000004 ~s",[integer_to_string(Boot_count)]),
   wait_for_ubootprompt(1500),

   c:flush(),

   ct_telnet:send(rs232,"md 0x20000004"),
   timer:sleep(1500),
   {ok, Data} = ct_telnet:get_data(rs232),
   {Value,_} = extract_val_memcmds("20000004:", Data, 2, 0),

   if
      (Value == Boot_count) ->
                  ct:pal("Boot count set to ~p and loading ~p",[Value, Kernel]);
      true ->
                  ct:fail("Boot count not set correctly to ~p",[Boot_count])
   end,

   ok = ct_telnet:send(rs232,"boot"),

   {ok, _} = ct_telnet:expect(rs232, [Load_kernel], [{timeout,20000},no_prompt_check]),

   if
      ( Boot_count > 5) ->
                 {ok, _} = ct_telnet:expect(rs232, "Welcome to Autointegration", [{timeout,120000},no_prompt_check]),
         
                 % Install the board from network loader prompt
                 ok = aic_curl:install_sw();
      true ->
                 ok
   end,

   ct_telnet:expect(rs232, ".*login", [{timeout,220000},no_prompt_check]),

   timer:sleep(9000),

    % Wait for the login prompt
   ok = rct_rs232:login(rs232),

   timer:sleep(3000),

   wait_for_dbbackup(),

   switch_to_testbox(),
   timer:sleep(5000),

   {ok, Init_data} = ct_telnet:cmd(rs232,"testbox --verbose 2 axmacc rmem -32 0x20000020", 18000),

   timer:sleep(1000),
   
   case lists:all(fun(X) -> string:str(X, "Connection refused")==0 end, Init_data) of

         false -> 
                  verify_testbox_mode(),
                  {ok, Data1} = ct_telnet:cmd(rs232,"testbox --verbose 2 axmacc rmem -32 0x20000020", 18000),
                  timer:sleep(1000);

         true ->
                  Data1 = Init_data
   end,

   switch_to_root(),

   {_,SubStr} = extract_val_memcmds("\[0x20000020\] =", Data1, 3, 0),


   BootStr = string:substr(SubStr, 6, 4),
   {BootConfigured,_} = string:to_integer(BootStr),

   if
     (Value<3) ->
            if
               (BootConfigured == 3333) ->
                      ct:pal("Loaded with configured kernel, BootConfigured: ~p",[BootConfigured]);
               true ->
                      ct:fail("Loading with configured kernel failed, BootConfigured: ~p",[BootConfigured])
            end;

     (Value<6) ->
            if
               (BootConfigured == 5555) ->
                      ct:pal("Loaded with fallback kernel, BootConfigured: ~p",[BootConfigured]);
               true ->
                      ct:fail("Loading with fallback kernel failed, BootConfigured: ~p",[BootConfigured])
            end;

     (Value == 6) ->
            if
               (BootConfigured > 0) ->
                      ct:pal("Loaded with network loader, BootConfigured: ~p",[BootConfigured]);
               true ->
                      ct:fail("Loading with network failed, BootConfigured: ~p",[BootConfigured])
            end;

     (Value > 6) ->
            if
               (BootConfigured > 0) ->
                      ct:pal("Loaded with network loader, BootConfigured: ~p",[BootConfigured]);
               true ->
                      ct:fail("Loading with network failed, BootConfigured: ~p",[BootConfigured])
            end
   end,

ok.

%%--------------------------------------------------------------------
%% @doc
%%       Read mailbox contents from type3
%% @end
%%--------------------------------------------------------------------
mailbox_read(Addr)->

   timer:sleep(3000),

   {ok, Init_data} = ct_telnet:cmdf(rs232,"testbox --verbose 2 axmacc rmem -32 ~s", [Addr],18000),

   case lists:all(fun(X) -> string:str(X, "Connection refused")==0 end, Init_data) of

         false -> 
                  verify_testbox_mode(),
                  {ok, Data} = ct_telnet:cmdf(rs232,"testbox --verbose 2 axmacc rmem -32 ~s", [Addr],18000);

         true ->
                  Data = Init_data
   end,

   Extract = string:concat(string:concat("\[", Addr),"\] ="),
   extract_val_memcmds(Extract, Data, 3, 1).

%%--------------------------------------------------------------------
%% @doc
%%       Print mailbox contents from type3
%% @end
%%--------------------------------------------------------------------
mailbox_print(Restartreason)->

     timer:sleep(3000),

     Boot_cnt = list_to_integer(string:substr(mailbox_read("0x20000004"), 3), 16),
     Rst_req =  mailbox_read("0x20000008"),
     Rst_reason = mailbox_read("0x2000000c"),
     Fault_low = mailbox_read("0x20000010"),
     Fault_high = mailbox_read("0x20000014"),
     Boot_Started = mailbox_read("0x20000020"),

     switch_to_root(),

     ct_telnet:cmd(rs232,"llog -l"),

     if
        (Restartreason == "cold") ->
                if 
                   (Rst_reason == "0xaffe8888") ->
                             ok;
                   true ->
                             ct:fail("Wrong restart reason for cold restart:\nBoot count: ~p\nRestart request: ~p\n
                                             Restart reason: ~p\nFault Indication: ~p, ~p\nBoot started: ~p\n",
                                                        [Boot_cnt, Rst_req, Rst_reason, Fault_low, Fault_high, Boot_Started])
                end;


        (Restartreason == "wd") ->
                if
                   (Rst_reason == "0xaffe2222") ->
                             ok;
                   true ->
                             ct:fail("Wrong restart reason for watchdog restart:\nBoot count: ~p\nRestart request: ~p\n
                                             Restart reason: ~p\nFault Indication: ~p, ~p\nBoot started: ~p\n",
                                                        [Boot_cnt, Rst_req, Rst_reason, Fault_low, Fault_high, Boot_Started])
                end;

        (Restartreason == "bpm") ->
                if
                   (Rst_reason == "0xaffe6666") ->
                             ok;
                   true ->
                             ct:fail("Wrong restart reason for bpm restart:\nBoot count: ~p\nRestart request: ~p\n
                                             Restart reason: ~p\nFault Indication: ~p, ~p\nBoot started: ~p\n",
                                                        [Boot_cnt, Rst_req, Rst_reason, Fault_low, Fault_high, Boot_Started])
                end;

        (Restartreason == "coldwtest") -> 
               ok;

        true ->
                ct:fail("Unknown restart type ~p",[Restartreason])

     end,


     if
        (Boot_cnt == 0) ->
                ct:pal("Boot count is either reset to 0 or started with configured");

        (Boot_cnt<3) ->
                if
                   (Boot_Started == "0xaffe3333") ->
                          ct:pal("Loaded with configured kernel!");
                   true ->

                          ct:fail("Loading with configured kernel failed:\nBoot count: ~p\nRestart request: ~p\nRestart reason: ~p\n
                                                                                       Fault Indication: ~p, ~p\nBoot started: ~p\n",
                                                                 [Boot_cnt, Rst_req, Rst_reason, Fault_low, Fault_high, Boot_Started])
                end;

        (Boot_cnt<6) ->
                if
                   (Boot_Started == "0xaffe5555") ->
                          ct:pal("Loaded with fallback kernel!");
                   true ->

                          ct:fail("Loading with fallback kernel failed:\nBoot count: ~p\nRestart request: ~p\nRestart reason: ~p\n
                                                                                     Fault Indication: ~p, ~p\nBoot started: ~p\n",
                                                               [Boot_cnt, Rst_req, Rst_reason, Fault_low, Fault_high, Boot_Started])
                end;

       (Boot_cnt>=6) ->
                if
                   (Boot_Started == "0xaffe7777") ->
                          ct:pal("Loaded with network loader!");
                   true ->

                          ct:fail("Loading with network failed:\nBoot count: ~p\nRestart request: ~p\nRestart reason: ~p\n
                                                                              Fault Indication: ~p, ~p\nBoot started: ~p\n",
                                                        [Boot_cnt, Rst_req, Rst_reason, Fault_low, Fault_high, Boot_Started])
               end
    end,


%      ct:pal("Upgrade Flag : ~p",[mailbox_read("0x2000028")]),
%      ct:pal("Trusted Anchors : ~p",[mailbox_read("0x200002c")]),
%      ct:pal("Vendor credentials : ~p",[mailbox_read("0x2000030")]).
      ok.


%%--------------------------------------------------------------------
%% @doc
%%       Mailbox stats evaluation
%% @end
%%--------------------------------------------------------------------
verify_mailbox_stats(Restarttype) ->

  ct_telnet:send(rs232, "printenv productname"),
  case ct_telnet:expect(rs232,
                             [{product_dus52,"productname=DUS 52"},
                              {product_dus32,"productname=DUS 32"},
                              {product_tcu03,"productname=TCU 03"},
                              {product_tcu04,"productname=TCU 04"}],
                                   [{timeout,1500},no_prompt_check]) of

      {ok, {product_dus52,_}} ->
                                Boot_cold_time = 40,
                                Boot_coldwtest_time = 350,
                                Boot_wd_time = 40,
                                Boot_bpm_time = 40;

      {ok, {product_dus32,_}} ->
                                Boot_cold_time = 40,
                                Boot_coldwtest_time = 330,
                                Boot_wd_time = 40,
                                Boot_bpm_time = 40;

      {ok, {product_tcu03,_}} ->
                                Boot_cold_time = 45,
                                Boot_coldwtest_time = 200,
                                Boot_wd_time = 45,
                                Boot_bpm_time = 45;
      {ok, {product_tcu04,_}} ->
                                Boot_cold_time = 45,
                                Boot_coldwtest_time = 200,
                                Boot_wd_time = 45,
                                Boot_bpm_time = 45

   end,

   ct_telnet:sendf(rs232,"restart ~s",[Restarttype]),

   {ok, _} = ct_telnet:expect(rs232,"RCS: First time stamp",[{timeout,25000},no_prompt_check]),
   Start_boottime = os:timestamp(),

   if
        (Restarttype == "cold") ->
                     RestartString = "RCS: Reset Status = SW Ordered \\(Cold\\)",
                     Boottime_limit = Boot_cold_time,
                     Time_out = 25000;

        (Restarttype == "coldwtest") -> 
                     RestartString = "RCS: Reset Status = SW Ordered \\(Cold with test\\)",
                     Boottime_limit = Boot_coldwtest_time,
                     Time_out = 500000;

        (Restarttype == "wd") ->
                     RestartString = "Reset Status = HW Watchdog",
                     Boottime_limit = Boot_wd_time,
                     Time_out = 18000;

        (Restarttype == "bpm") ->
                     RestartString = "Reset Status = Power On \\(BPM SW\\)",
                     Boottime_limit = Boot_bpm_time,
                     Time_out = 18000

   end,

   {ok, _} = ct_telnet:expect(rs232, ["RCS: Memory testing successful", "RCS: DBB5 debug switch in position 0", RestartString], 
                                                              [sequence, {halt,[".*login"]},{timeout,Time_out},no_prompt_check]),

   {ok, _} = ct_telnet:expect(rs232,"Starting kernel ...",[{timeout,100000},no_prompt_check]),
   End_boottime = os:timestamp(),

   ct_telnet:expect(rs232, ".*login", [{timeout,60000},no_prompt_check]),

   if

     (Restarttype == "coldwtest") ->
             % Wait for the login prompt after testmgr restart
             ct_telnet:expect(rs232,"RCS: First time stamp",[{timeout,200000},no_prompt_check]),
             {ok, _} = ct_telnet:expect(rs232, ["RCS: Reset Status", "Starting kernel ..."],
                                                             [sequence, {halt,[".*login"]},{timeout,Time_out},no_prompt_check]),
             rct_rs232:login(rs232);
      true ->
             % Wait for the login prompt
             rct_rs232:login(rs232)
   end,

   timer:sleep(9000),
   ok = rct_rs232:login(rs232),

   Boot_time = trunc(timer:now_diff(End_boottime, Start_boottime) / 1000 / 1000),

   if
    (Boot_time =< Boottime_limit) ->
                ct:pal("Boot up time for ~s restart is ~p secs",[Restarttype, Boot_time]),
                ok;
    true ->
             ct:fail("Boot up time for ~s restart is ~p secs, crossed the allowed limit of ~p secs",[Restarttype, Boot_time, Boottime_limit])
   end,

   

   timer:sleep(20000),
   switch_to_testbox(),

   mailbox_print(Restarttype).

%%--------------------------------------------------------------------
%% @doc
%%       Verify Led status command
%% @end
%%--------------------------------------------------------------------

verify_led(Led) ->

  ok = ct_telnet:send(rs232,"led status"),
  if
      (Led == "red") -> Led_on = "Red.*fault.*ON",
                        Led_off = "Red.*fault.*OFF";

      (Led == "green") -> Led_on = "Green.*operational.*ON",
                          Led_off = "Green.*operational.*OFF";

      (Led == "blue") -> Led_on = "Blue.*maintenance.*ON",
                         Led_off = "Blue.*maintenance.*OFF";

      (Led == "yellow") -> Led_on = "Yellow.*status.*ON",
                           Led_off = "Yellow.*status.*OFF"

  end,

  case ct_telnet:expect(rs232,[Led_off],[{timeout,5000},no_prompt_check]) of

           {ok, _} ->
           %ct:pal("~s led off",[Led]),
           ok = ct_telnet:sendf(rs232,"led ~s on",[Led]),
           %ct:pal("LED ~s ON",[Led]),
           ok = ct_telnet:send(rs232,"led status"),
           {ok, _} = ct_telnet:expect(rs232, [Led_on], [{timeout,1000},no_prompt_check]),
           ok = ct_telnet:sendf(rs232,"led ~s off",[Led]),
           %ct:pal("LED ~s OFF",[Led]),
           ok = ct_telnet:send(rs232,"led status"),
           {ok, _} = ct_telnet:expect(rs232, [Led_off], [{timeout,1000},no_prompt_check]);

       {_,_} ->
           %ct:pal("~s led on",[Led]),
           ok = ct_telnet:sendf(rs232,"led ~s off",[Led]),
           %ct:pal("LED ~s OFF",[Led]),
           ok = ct_telnet:send(rs232,"led status"),
           {ok, _} = ct_telnet:expect(rs232, [Led_off], [{timeout,1000},no_prompt_check]),
           %ct:pal("LED ~s ON",[Led]),
           ok = ct_telnet:send(rs232,"led status"),
           {ok, _} = ct_telnet:expect(rs232, [Led_on], [{timeout,1000},no_prompt_check])
  end.

%% ===========================================================================
%% @doc
%%        Cleanup the uboot files in /tmp on target
%% @end
%% ===========================================================================

clean_up_ubootfiles() ->

   ok = rct_rs232:login(rs232),
   ok = ct_telnet:send(rs232,"rm -rf /tmp/DTB"),
   ok = ct_telnet:send(rs232,"rm -rf /tmp/UBOOT_HW_PARAM"),
   ok = ct_telnet:send(rs232,"rm -rf /tmp/UBOOT_STAGE3"),
   ok = ct_telnet:send(rs232,"rm -rf /tmp/*.ucf"),
   timer:sleep(10000),
   rct_rs232:login(rs232),
ok.

%% ===========================================================================
%% @doc
%%        Cleanup the uboot env varaibles to select the upgradable partitions
%% @end
%% ===========================================================================

cleanup_ubootenv() ->

   ct:pal("Cleaning the uboot env variables that selects the upgradable partition"),

   ct_telnet:send(rs232,"env delete uboot_a_sequence"),
   ct_telnet:send(rs232,"env delete uboot_b_sequence"),

   ct_telnet:send(rs232,"env delete dtb_a_sequence"),
   ct_telnet:send(rs232,"env delete dtb_b_sequence"),

   ct_telnet:send(rs232,"saveenv"),

   {ok, _} = ct_telnet:expect(rs232, ["Erasing SPI flash...Writing to SPI flash...done", "=> "],
                                                   [sequence, {timeout,25000},no_prompt_check]),

   ct_telnet:send(rs232,"saveenv"),

   {ok, _} = ct_telnet:expect(rs232, ["Erasing SPI flash...Writing to SPI flash...done", "=> "],
                                                   [sequence, {timeout,25000},no_prompt_check]),

ok.

%% ===========================================================================
%% @doc
%%        Manual restore
%% @end
%% ===========================================================================

manual_restore() ->

  % Check that board is up
  ok = rct_rs232:login(rs232),

  ok = ct_telnet:send(rs232,"pgh_restartbrd -t \"Restart to recover from factoryrestore failure\""),
  verify_ubootprompt(),
  cleanup_ubootenv(),

  ok = ct_telnet:send(rs232,"rcs"),

  ct_telnet:expect(rs232, ".*login", [{timeout,60000},no_prompt_check]),

  % Check that board is up
  case rct_rs232:login(rs232) of
        ok ->  
              ok;
         _ -> 
              timer:sleep(15000),
              ok =  rct_rs232:login(rs232)
  end,

ok.

%%--------------------------------------------------------------------
%% @doc
%%       Formating the expect script output
%% @end
%%--------------------------------------------------------------------

extract_script_ouput(Output) ->

  Int_op = lists:map(fun(X) -> string:strip(X, both, $\r) end, string:tokens(Output,"\n\n")),
  Final_op = lists:filter(fun(X) -> (string:str(X, "root@")==0 andalso string:str(X, "load average")==0) end, Int_op),
  Final_op.


%% ===========================================================================
%% @doc
%%        Downloads file from board to host
%%        In the priv_dir under the common_test's logs directory
%% @end
%% ===========================================================================
fetch_file(Heading, Path, Name, Config) ->
  
  PrivDir = ?config(priv_dir, Config),
  Type = filename:extension(Path),
  PathInFS = PrivDir ++ Name,
  ct:log("PathInFS ~p", [PathInFS]),
  Result = rct_scp:from_target(scpn1, Path, PathInFS, 20000),
  %ct:pal(?DEBUG, "Copied file from target  : ~p", [Result]),
  case Result of
        {ok, _Log} ->
                 ct:log("~p",[Heading]),
                 ct:log("<a href=~p type=~p>~s</a>\n",[PathInFS,Type,Name]),
                 ok;
               _  -> 
                 ct:fail("Failed to copy file from board ~p", [Path])
  end.

%%--------------------------------------------------------------------
%% @doc
%%       Factore restore the upgradable partitions
%% @end
%%--------------------------------------------------------------------

fw_factoryrestore(Verify_restore, Config) ->

  Hw = atom_to_list(ct:get_config({test_nodes,1})),

  if
     ( Verify_restore == "false")  ->
                              ct:pal("Restoring the upgradable partition from primary contents"),
                              Restore_string = "Factory restore attempt";

     ( Verify_restore == "true") ->
                              ct:pal("Starting verification of firmware factory restore step"),
                              Restore_string = "Factory restore verification"
  end,

  Restore_op = os:cmd("./board/rcs/cpm1/tools/rcs_changeTYPE2.5.exp " ++ Hw ++ " -factoryrestore"),

  % For test purpose to run manually
  % Restore_op = os:cmd(""++os:getenv("PWD")++"/scripts/rcs_changeTYPE2.5.exp " ++ Hw ++ " -factoryrestore"),

  io:format(Restore_op),

  Int_restore_op = extract_script_ouput(Restore_op),
 
  case lists:all(fun(X) -> string:str(X, "ERROR: Connection refused")==0 end, Int_restore_op) of

         false -> 
                  % Fetch the syslog
                  fetch_file("Downloading syslog from node", "/var/log/syslog", "syslog.log",Config),

                  % retry the restore process with a delay of 10 seconds
                  ct:pal("First attempt of restoration failed, trying another attempt"),

                  Retry_restore_op = os:cmd("./board/rcs/cpm1/tools/rcs_changeTYPE2.5.exp " ++ Hw ++ " -factoryrestore"),
                  io:format(Retry_restore_op),
                  Fin_restore_op = extract_script_ouput(Retry_restore_op);
         true ->
                  Fin_restore_op = Int_restore_op
  end,

  % ct:pal("Output from factoryrestore script is \n ~p \n", [Fin_restore_op]),

  Expectedrestore_strs = ["Upgrading firmware...","HW parameter image restored",
                          "DTB image restored","U-Boot S3 image restored","done!",
                          "2.5 changer LM operation completed successfully",
                          "Reboot after type 2 change successful"],

  Rst_hwparam = "Parameters: Watchdog 0 A\/B Valid 1\/1 A\/B Sequence 0\/0 => A",
  Rst_uboot_s3 = "U-Boot: Watchdog 0 A\/B Valid 1\/1 A\/B Sequence 0\/0 => A",
  Rst_dtb_str1 = "RCS: DTB info: valid 1\/1, seq 0\/0, wd 0",
  Rst_dtb_str2 = "RCS: Fetching DTB-A area",

  % Check that the script executed successfully
  case (lists:suffix(Expectedrestore_strs, Fin_restore_op)) of
       true ->
                ct:pal("Factoryrestore script sucessfully executed"),

                % Validate that the firmware factoryrestore is sucessful

                % Check the board is in type3
                case rct_rs232:login(rs232) of
                       ok ->
                               ok;
                       _  ->
                               timer:sleep(15000),
                               ok =  rct_rs232:login(rs232)
                end,

                % Restart the board and validate the strings
                ok = ct_telnet:send(rs232,"pgh_restartbrd -t \"Restart to validate the firmware factoryrestore step\""),

                % Validate that the hw param, uboot stage 3 and dtb in upgradable partition are restored back as in primary partitions
                {Rststatus, _} = ct_telnet:expect(rs232, [Rst_hwparam, Rst_uboot_s3, Rst_dtb_str1, Rst_dtb_str2],
                                                     [sequence, {timeout,150000},no_prompt_check]),

                ct_telnet:expect(rs232, ".*login", [{timeout,60000},no_prompt_check]),

                % Wait for the login prompt
                case rct_rs232:login(rs232) of
                       ok ->
                               ok;
                       _  ->
                               timer:sleep(15000),
                               ok =  rct_rs232:login(rs232)
                end,

                if
                  (Rststatus == ok) ->
                                       ct:pal("~p sucessfully done",[Restore_string]);
                   true ->
                           % This step should not fail so as to not to keep the board in unwanted state.
                           % So atleast clean up the stage3 & DTB environment variables from uboot prompt

                           manual_restore(),
                           ct:fail("~p failed",[Restore_string])
                end;

       false ->
                ct:pal("Factoryrestore script execution failed"),

                % This step should not fail so as to not to keep the board in unwanted state.
                % So atleast clean up the stage3 & DTB environment variables from uboot prompt

                manual_restore(),
                ct:fail("~p failed",[Restore_string])

  end,

ok.


%% ===========================================================================
%% @doc
%% Wait for database backup
%% Wait for fl tool run completion status
%% @end
%% ===========================================================================
wait_for_dbbackup() ->
%   ct:pal("Wait for database copy to disk"),
    wait_for_dbbackup(120000).

wait_for_dbbackup(Timeout) when Timeout < 11000 ->
    ct_telnet:send(rs232,"cat /rcs/log/SwmInternal/SwmInternal.1  | grep \"Auto backup\";echo \"RESULT=$?\""),
%   ct:pal("Timeout : ~p",[Timeout]),
    case ct_telnet:expect(rs232, "RESULT=0", [{timeout,5000}, no_prompt_check]) of
       {ok, _} ->
            ct:pal("Database available");
       _  ->
            ct:fail("Database copy is not done even after max timeout.")
    end;

wait_for_dbbackup(Timeout) ->
    ct_telnet:send(rs232,"ls -1 /home/sirpa/autobackup.gz; echo \"RESULT=$?\""),
%   ct:pal("Timeout : ~p",[Timeout]),
    case ct_telnet:expect(rs232, "RESULT=0", [{timeout,10000}, no_prompt_check]) of
       {ok, _} ->
            ct:pal("Database available");
       _  ->
            wait_for_dbbackup(Timeout - 10000)
    end.

%% ===========================================================================
%% @doc
%% Mount the pramfs in xtra area
%% @end
%% ===========================================================================
mount_pramfs()->
   % Mount pramfs area in xtra area
   {ok, _} = ct_telnet:cmd(rs232,"dmesg --clear"),
   {ok, _} = ct_telnet:cmd(rs232,"mkdir -p /home/sirpa/xtra"),
   {ok, _} = ct_telnet:cmd(rs232,"mount -t pramfs -o physaddr=0x20010000 pramfs /home/sirpa/xtra").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% T E S T C A S E S %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% ===========================================================================
%% @doc
%%        This test case sets environmental variables in U-boot,
%%        and verifies if they can be saved and recalled after a
%%        reset. Erasing of variables is also tested.
%%        Entry: Uboot prompt, Exit: Uboot prompt
%%
%% @end
%% ===========================================================================

env_testcase(_Config) ->

   ct_telnet:send(rs232, "setenv test var"),
   ct_telnet:send(rs232, "saveenv"),
   {ok, _} = ct_telnet:expect(rs232, "Writing to SPI flash", [{timeout,8000},no_prompt_check]),
   ct_telnet:send(rs232, "printenv"),
   {ok, _} = ct_telnet:expect(rs232, "test", [{timeout,3000},no_prompt_check]),

   ok = ct_telnet:send(rs232, "reset"),

   verify_ubootprompt(),

   ct_telnet:send(rs232, "printenv"),
   {ok, _} = ct_telnet:expect(rs232, "test", [{timeout,3000},no_prompt_check]),


   ct_telnet:send(rs232, "setenv test"),
   timer:sleep(2000),

   ct_telnet:send(rs232, "printenv test"),

   {ok, _} = ct_telnet:expect(rs232, "Error: \"test\" not defined", [{timeout,3000},no_prompt_check]),

   ct_telnet:send(rs232, "saveenv"),
   {ok, _} = ct_telnet:expect(rs232, "Writing to SPI flash", [{timeout,8000},no_prompt_check]).



%% ===========================================================================
%% @doc
%%       This test case verifies the hw log by writing and erasing.
%%       It also verifies SPI flash commands.
%%       Entry: Uboot prompt, Exit: Uboot prompt
%%
%% end
%% ===========================================================================

hwlog_flashcmds_testcase(_Config) ->

% MAKE A COPY OF EXISTING ERROR LOG ENTRY

  c:flush(),

  ct_telnet:send(rs232,"hwlog read"),
  timer:sleep(12000),
  {ok, Data} = ct_telnet:get_data(rs232),

  % Ignore "ramlog is full" error string

  Int_Op = lists:filter(fun(X) -> string:str(X, "ramlog is full")==0 end, Data),

  InitOp = lists:filter(fun(X) -> string:str(X, "SF: Detected")==0 end, Int_Op),

  ct_telnet:send(rs232,"hwlog debug"),
  timer:sleep(2000),
  {ok, InitDebugOp} = ct_telnet:get_data(rs232),

  wait_for_ubootprompt(18000),

  StartPtr = extract_val_memcmds("start_ptr value", InitDebugOp, 4, 1),
  WritePtr = extract_val_memcmds("write_ptr value", InitDebugOp, 4, 1),
  StartAddrPtr = extract_val_memcmds("start_ptr address", InitDebugOp, 4, 1),
  WriteAddrPtr = extract_val_memcmds("write_ptr address", InitDebugOp, 4, 1),
  EraseblockSize = extract_val_memcmds("erase block size", InitDebugOp, 6, 1),

  SizeHex = string:substr(EraseblockSize, 3),
  CircLimit = 3072 - ((list_to_integer(SizeHex, 16))/128) + 10,

  % Store the existing hwlog entry in some temporary memory area

  ct_telnet:send(rs232,"sf probe"),
  wait_for_ubootprompt(3000),

  ct_telnet:send(rs232,"sf probe; sf read 0x23f00000 0x00780000 0x60000"),

  case ct_telnet:expect(rs232,"ERROR:", [{timeout,2000},no_prompt_check]) of
    {ok, _} ->
              ct:fail("Hwlog entries backup in memory failed");
    {_, _} ->
              ct:pal("Hwlog entries backup to memory area suceeded"),
              ct_telnet:send(rs232, "md 0x23f00000")
  end,

  wait_for_ubootprompt(5000),



% HWLOG ERASE TEST

  ct_telnet:send(rs232,"hwlog erase"),
  wait_for_ubootprompt(8000),

  ExpectedOp = ["Reading HW Log:","SeqNo Date    Time    LogId  Message","----- ----    ----    -----  -------"],

  ct_telnet:send(rs232,"hwlog read"),
  timer:sleep(15000),

  {ok,Output} = ct_telnet:get_data(rs232),

  % Remove any unwanted strings from the list

  TempOp = lists:filter(fun(X) -> string:str(X, "SF: Detected")==0 end, Output),

  % It is observed that sometimes the prompt symbol is also included in the list, so remove the command string as well

  ActualOp = lists:filter(fun(X) -> string:str(X, "hwlog read")==0 end, TempOp),

  % Verify the output is as expected after hwlog erase

  if
     (ExpectedOp == ActualOp) ->
            ok;
     true ->
            restore_hwlog_entries(InitOp, StartAddrPtr, StartPtr, WriteAddrPtr, WritePtr),
            ct:fail("hwlog erase failed")
  end,

% TEST THE HWLOG WRITE AND READ BY STORING NEW ENTRIES IN HWLOG AREA

  ct_telnet:send(rs232,"hwlog test"),
  wait_for_ubootprompt(10000),

  ct_telnet:send(rs232,"hwlog read"),
  case ct_telnet:expect(rs232, "U-Boot3 zz fault", [{timeout,18000},no_prompt_check]) of
          {ok, _} ->
                     ok;
          {FailReason, _} ->
                     restore_hwlog_entries(InitOp, StartAddrPtr, StartPtr, WriteAddrPtr, WritePtr),
                     ct:fail("hwlog read expect failed with reason ~p",[FailReason])

  end,

% TEST THE CIRCULAR BUFFER FEATURE

  % Testing the max limit for the hwlog entries

  ct_telnet:send(rs232,"hwlog erase"),
  wait_for_ubootprompt(8000),

  ct_telnet:send(rs232,"hwlog test3"),
  wait_for_ubootprompt(5000),
  ct_telnet:send(rs232,"hwlog test2"),
  wait_for_ubootprompt(5000),
  ct_telnet:send(rs232,"hwlog test2"),
  wait_for_ubootprompt(5000),
  ct_telnet:send(rs232,"hwlog test2"),
  wait_for_ubootprompt(5000),
  ct_telnet:send(rs232,"hwlog test2"),
  wait_for_ubootprompt(5000),
  ct_telnet:send(rs232,"hwlog test2"),
  wait_for_ubootprompt(5000),

  c:flush(),

  ct_telnet:send(rs232,"hwlog read"),
  timer:sleep(70000),
  {ok,MaxOp} = ct_telnet:get_data(rs232),

  Len_MaxOp = length(MaxOp),

  ct:pal("Length of MaxOp : ~p",[Len_MaxOp]),

  if
    (Len_MaxOp<3078) ->
                            ok;
     true ->
          restore_hwlog_entries(InitOp, StartAddrPtr, StartPtr, WriteAddrPtr, WritePtr),
          ct:fail("More than max entries, ~p",[Len_MaxOp])
  end,

  ct_telnet:send(rs232,"hwlog test"),
  wait_for_ubootprompt(3000),

  c:flush(),
  timer:sleep(1000),

  ct_telnet:send(rs232,"hwlog read"),
  timer:sleep(70000),
  {ok,CircOp} = ct_telnet:get_data(rs232),

  Len_CircOp = length(CircOp),

  ct:pal("Length of CircOp : ~p",[length(CircOp)]),

  if
    (Len_CircOp<(CircLimit+1)) ->
                 ok;
    true ->
                 restore_hwlog_entries(InitOp, StartAddrPtr, StartPtr, WriteAddrPtr, WritePtr),
                 ct:fail("No of entries more than expected, ~p",[Len_CircOp])
  end,

  First_entry = string:substr((lists:nth(5, CircOp)), (string:len(lists:nth(5, CircOp))- 17)),
  Last_entry = string:substr((lists:nth(Len_CircOp, CircOp)), (string:len(lists:nth(Len_CircOp, CircOp))- 16)),

  ct:pal("First_entry :~p, Last_entry :~p ",[First_entry, Last_entry]),

  if
    ( CircLimit == 2570) ->
                              FirstMatch = string:str(First_entry, "U-Boot2 power-on");
                    true ->
                              FirstMatch = string:str(First_entry, "U-Boot33 power-on")
  end,

  LastMatch = string:str(Last_entry, "U-Boot3 zz fault"),

  if
     (FirstMatch == 0) ->
             restore_hwlog_entries(InitOp, StartAddrPtr, StartPtr, WriteAddrPtr, WritePtr),
             ct:fail("Not expected first entry: ~p",[First_entry]);
      true ->
            ok
  end,

  if
     (LastMatch == 0) ->
             restore_hwlog_entries(InitOp, StartAddrPtr, StartPtr, WriteAddrPtr, WritePtr),
             ct:fail("Not expected last entry: ~p",[Last_entry]);
      true ->
            ok
  end,


% RESTORE THE HWLOG AREA WITH ITS ORIGINAL ENTRIES

  restore_hwlog_entries(InitOp, StartAddrPtr, StartPtr, WriteAddrPtr, WritePtr),

  ok.

%% ============================================================================
%% @doc
%%       This testcase verifies the restarts from Uboot.
%%       Entry: Uboot prompt, Exit: Uboot prompt
%% end
%% ============================================================================

restarts_testcase(_Config) ->

Rst_cold = "RCS: Reset Status = SW Ordered \\(Cold\\)",
Rst_coldwtest = "RCS: Reset Status = SW Ordered \\(Cold with test\\)",
Rst_wd = "Reset Status = HW Watchdog",
Rst_bpm = "Reset Status = Power On \\(BPM SW\\)",

% Cold restart

   ct:pal("Verifying cold restart from uboot"),

   ct_telnet:send(rs232,"restart cold"),

   {ok, _} = ct_telnet:expect(rs232, "RCS: Memory testing successful", [{timeout,60000},no_prompt_check]),

   verify_ubootprompt(),

   ct_telnet:send(rs232,"rcs 1"),

  {ok, _} = ct_telnet:expect(rs232, {"Welcome",Rst_cold}, [sequence, {halt,["RCS: Cold with test restart"]},
                                                                          {timeout,25000},no_prompt_check]),


% Cold with test restart

   ct:pal("Verifying coldwtest restart from uboot"),

   ct_telnet:send(rs232,"restart coldwtest"),

   {ok, _} = ct_telnet:expect(rs232, "RCS: Memory testing successful", [{timeout,600000},no_prompt_check]),

   verify_ubootprompt(),

   ct_telnet:send(rs232,"rcs 1"),

   {ok, _} = ct_telnet:expect(rs232, Rst_coldwtest, [{timeout,600000},no_prompt_check]),


% Watchdog

   ct_telnet:send(rs232, "\n"),

   {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,20000},no_prompt_check]),


   ct:pal("Verifying watchdog restart from uboot"),

   ct_telnet:send(rs232,"restart wd"),

   {ok, _} = ct_telnet:expect(rs232, "RCS: Memory testing successful", [{timeout,60000},no_prompt_check]),

   verify_ubootprompt(),

   ct_telnet:send(rs232,"rcs 1"),

   {ok, _} = ct_telnet:expect(rs232, Rst_wd, [{timeout,18000},no_prompt_check]),

   timer:sleep(10000),


% Power on Restart

   ct:pal("Verifying power on restart from uboot"),

   ct_telnet:send(rs232,"restart bpm"),

   {ok, _} = ct_telnet:expect(rs232, "RCS: Memory testing successful", [{timeout,60000},no_prompt_check]),

   verify_ubootprompt(),

   ct_telnet:send(rs232,"rcs 1"),
   timer:sleep(5000),

   {ok, _} = ct_telnet:expect(rs232, Rst_bpm, [{timeout,18000},no_prompt_check]),


ok.

%% ============================================================================
%% @doc
%%       This testcase verifies the mailbox updates from type3
%%       for the restarts from Uboot.
%%       Entry: Uboot prompt, Exit: Type3 prompt
%% end
%% ============================================================================

verify_mailbox_for_ubootrestarts(_Config) ->

% Cold restart

   ct:pal("Verifying cold restart from uboot"),

   verify_mailbox_stats("cold"),

   ct_telnet:cmd(rs232,"llog -l"),

   ct_telnet:send(rs232,"pgh_restartbrd -t \"Verify_mailbox_tc: Setup for coldwtest\""),
   verify_ubootprompt(),

% Coldwtest restart

   ct:pal("Verifying coldwtest restart from uboot"),

   verify_mailbox_stats("coldwtest"),

   ct_telnet:cmd(rs232,"llog -l"),

   ct_telnet:send(rs232,"pgh_restartbrd -t \"Verify_mailbox_tc: Setup for watchdog restart\""),
   verify_ubootprompt(),

% Watchdog restart

   ct:pal("Verifying watchdog restart from uboot"),

   verify_mailbox_stats("wd"),

   ct_telnet:cmd(rs232,"llog -l"),

   ct_telnet:send(rs232,"pgh_restartbrd -t \"Verify_mailbox_tc: Setup for poweron restart\""),
   verify_ubootprompt(),

% Power on restart

   ct:pal("Verifying bpm restart from uboot"),

   verify_mailbox_stats("bpm"),

ok.



%% ===========================================================================
%% @doc
%%        This testcase verifies the dtb redundancy feature
%%        Entry: Uboot prompt, Exit: Uboot prompt
%% @end
%% ===========================================================================

dtb_redundancy_testcase(_Config) ->

 Hw = atom_to_list(ct:get_config({test_nodes,1})),
 ct_telnet:send(rs232,"fdt addr 0x05000000"),
 wait_for_ubootprompt(1000),

 ct_telnet:send(rs232,"fdt print \/ revision_dtb"),
 timer:sleep(1500),
 {ok, InitDtbRev} = ct_telnet:get_data(rs232),

 DtbRev_actual = lists:filter(fun(X) -> string:str(X, "revision_dtb =")>0 end, InitDtbRev),

 ct_telnet:send(rs232,"printenv dtb_b_sequence"),
 case ct_telnet:expect(rs232, "Error: \"dtb_b_sequence\" not defined", [{timeout,3000},no_prompt_check]) of
     {ok, _} ->
            DtbAvail = false,
            DataDtb_a = 0,
            DataDtb_b = 0;

      {_, _} ->
            ct_telnet:send(rs232,"printenv dtb_a_sequence"),
            {ok, DataDtb_a} = ct_telnet:get_data(rs232),
            ct_telnet:send(rs232,"printenv dtb_b_sequence"),
            {ok, DataDtb_b} = ct_telnet:get_data(rs232),
            ct:comment("dtb_a_sequence :~p, dtb_b_sequence :~p already exists",[DataDtb_a,DataDtb_b]),

            ct_telnet:send(rs232,"sf probe; sf read 0x23f00000 0x006c0000 0x20000"),
            DtbAvail = true
 end,


 case is_secure() of
           {true, Dtb} ->
                          ct_telnet:send(rs232,"dhcp; tftp 0x5000000 " ++ Hw ++ "/" ++ Dtb ++ ".dtb.ptad.signed");
           {false, Dtb} ->
                          ct_telnet:send(rs232,"dhcp; tftp 0x5000000 " ++ Hw ++ "/" ++ Dtb ++ ".dtb")
 end,

 wait_for_ubootprompt(5000),

  % updating it to DTB(2) area
  ct_telnet:send(rs232,"sf probe; sf update 0x05000000 0x006c0000 0x20000"),
  wait_for_ubootprompt(5000),

  % Update environment variable to start the DTB(2)

  ct_telnet:send(rs232,"setenv dtb_a_sequence 7"),
  wait_for_ubootprompt(5000),
  ct_telnet:send(rs232,"setenv dtb_b_sequence 8"),
  wait_for_ubootprompt(5000),

  ct_telnet:send(rs232,"rcs 1"),
  case ct_telnet:expect(rs232,"Fetching DTB-B area",[{timeout,15000},no_prompt_check]) of

      {ok, _} ->
          ct:pal("DTB redundancy without any watchdog restart passed"),
          ct_telnet:send(rs232, "\n"),
          {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,20000},no_prompt_check]),

          if
              (DtbAvail == false) ->
                    ct_telnet:send(rs232,"saveenv"),
                    {ok, _} = ct_telnet:expect(rs232, ["Erasing SPI flash...Writing to SPI flash...done", "=> "],
                                                             [sequence, {timeout,15000},no_prompt_check]),

                    ct_telnet:send(rs232,"restart wd"),

                    verify_ubootprompt(),

                    ct_telnet:send(rs232,"rcs 1"),

                    case ct_telnet:expect(rs232,"Fetching DTB-A area",[{timeout,20000},no_prompt_check]) of

                          {ok, _} ->
                                     ct:pal("DTB redundancy with watchdog restart passed"),
                                     ct_telnet:send(rs232, "\n"),
                                     {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,22000},no_prompt_check]),
                                     ct_telnet:send(rs232,"setenv dtb_a_sequence"),
                                     wait_for_ubootprompt(5000),
                                     ct_telnet:send(rs232,"setenv dtb_b_sequence"),
                                     wait_for_ubootprompt(5000),
                                     ct_telnet:send(rs232,"saveenv"),
                                     {ok, _} = ct_telnet:expect(rs232, ["Erasing SPI flash...Writing to SPI flash...done", "=> "],
                                                                       [sequence, {timeout,15000},no_prompt_check]),
                                     restore_dtb_version(DtbRev_actual, DtbAvail, DataDtb_a, DataDtb_b);

                            {_, _} ->
                                     ct_telnet:send(rs232, "\n"),
                                     {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,20000},no_prompt_check]),
                                     ct_telnet:send(rs232,"setenv dtb_a_sequence"),
                                     wait_for_ubootprompt(5000),
                                     ct_telnet:send(rs232,"setenv dtb_b_sequence"),
                                     wait_for_ubootprompt(5000),
                                     ct_telnet:send(rs232,"saveenv"),
                                     {ok, _} = ct_telnet:expect(rs232, ["Erasing SPI flash...Writing to SPI flash...done", "=> "],
                                                                       [sequence, {timeout,15000},no_prompt_check]),
                                     restore_dtb_version(DtbRev_actual, DtbAvail, DataDtb_a, DataDtb_b),
                                     ct:fail("DTB redundancy with watchdog restart is failed")
                    end;

              (DtbAvail == true) ->
                    ct:pal(" Skipped testing DTB redundancy with watchdog restart"),
                    ct:comment("Skipped testing DTB redundancy with watchdog restart"),
                    restore_dtb_version(DtbRev_actual, DtbAvail, DataDtb_a, DataDtb_b)
          end;

      {_, _} ->
              restore_dtb_version(DtbRev_actual, DtbAvail, DataDtb_a, DataDtb_b),
              ct:fail("DTB redundancy not worked, DTB test version not selected")

   end,

ok.


%% ===============================================================================
%% @doc
%%        This testcase verifies the hardware parameter file redundancy feature
%%        Entry: Uboot prompt, Exit: Uboot prompt
%% @end
%% ===============================================================================

param_redundancy_testcase(_Config) ->

 Param_valid_1_2 = "Parameters: Watchdog 0 A\/B Valid 1\/1 A\/B ",
 Param_tbl = "Parameter Table Version:",
 Uboot_stop = "Hit any key to stop autoboot:",
 
 % Ensure that we are in Uboot prompt

 ok = ct_telnet:send(rs232, "\n"),
 {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,5000},no_prompt_check]),

 % Ensure that param areas are valid, Stage 2 should correct it if any area found invalid
 ct_telnet:send(rs232,"reset"),

 case ct_telnet:expect(rs232, [Param_valid_1_2, Param_tbl], [sequence, {halt,[Uboot_stop]},{timeout,10000},no_prompt_check]) of
       {ok,_} ->
          verify_ubootprompt(),
          % Using B area for testing
          Param_flash = "0x00050000",
          ct:pal("Param redundancy can be tested as both the PARAM areas are valid");

       {_, _} ->
          Param_flash = "ERROR",
          ct:fail("ERROR: One of the param area in invalid")

 end,

 Cpm52_axm1_0 = "/proj/rcs-tmp/tftpboot/config-cpm52-uboot-axm1-0.bin",
 Cpm52_axm1_1 = "/proj/rcs-tmp/tftpboot/config-cpm52-uboot-axm1-1.bin",
 Cpm03_axm1_0 = "/proj/rcs-tmp/tftpboot/config-cpm03-uboot-axm1-0.bin",
 Cpm03_axm1_1 = "/proj/rcs-tmp/tftpboot/config-cpm03-uboot-axm1-1.bin",
 Cpm32_axm1_1 = "/proj/rcs-tmp/tftpboot/config-cpm32-uboot-axm1-1.bin",
 Cpm04_bin = "/proj/rcs-tmp/tftpboot/config-cpm04-uboot.bin",

 {Secure_brd, Boardtype} = is_secure(),
 
 ct_telnet:send(rs232, "printenv productname"),
 case ct_telnet:expect(rs232,
                             [{product_dus52,"productname=DUS 52"},
                              {product_tcu03,"productname=TCU 03"},
                              {product_dus32,"productname=DUS 32"},
                              {product_tcu04,"productname=TCU 04"}],
                                     [{timeout,1500},no_prompt_check]) of

   {ok, {product_dus52,_}} ->
               case is_axm1_0() of

                 true ->
                     ct:pal("Detected AXM1.0 DUS52 board"),
                     Boardrevision = product_revision(),
                     
                     if
                        ((Boardrevision =< "P1D") andalso (Secure_brd == false)) ->

                            ct:pal("dus52 with rev ~p, AXM1.0 and Param R1A release and 4GB mem",[Boardrevision]),
                            {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm52-uboot-r1aseq1b.bin", Cpm52_axm1_0),
                            ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm52-uboot-axm1-0.bin");

                        ((Boardrevision > "P1D") andalso (Boardrevision =< "P2C") andalso (Secure_brd == false)) ->

                            ct:pal("dus52 with rev ~p,AXM1.0 and Param R2A release and 8GB mem",[Boardrevision]),
                            {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm52-uboot-r2aseq1.bin", Cpm52_axm1_0),
                            ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm52-uboot-axm1-0.bin");

                        true   ->
                            ct:fail("Unknown DUS52 board with axm1.0 and rev: ~p detected",[Boardrevision])
                     end;

                 false ->
                     ct:pal("Detected AXM1.1 DUS52 board"),
                     Boardrevision = product_revision(),
                     if
                        ((Boardrevision >= "P4A") andalso (Boardrevision =< "R4A") andalso (Secure_brd == false)) ->

                            ct:pal("dus52 with rev ~p,AXM1.1 and Param R3A release and 8GB mem",[Boardrevision]),
                            {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm52-uboot-r3b_x.bin", Cpm52_axm1_1),
                            ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm52-uboot-axm1-1.bin");

                        ((Boardrevision >= "R5A") andalso (Secure_brd == false) andalso (Boardtype == "cpm1_52_SV_open")) ->

                            ct:pal("dus52 with rev ~p,AXM1.1 and Param R4A release and 8GB mem",[Boardrevision]),
                            {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm52-uboot-r4a_x.bin", Cpm52_axm1_1),
                            ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm52-uboot-axm1-1.bin");

                        ((Boardrevision >= "R1A")  andalso (Boardrevision =< "R5F") andalso (Secure_brd == true) andalso (Boardtype == "cpm1_52_SV_open")) ->

                            ct:pal("Secure dus52 with rev ~p,AXM1.1 and Param R4A release and 8GB mem",[Boardrevision]),
                            {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm52-uboot-r4a_x.bin", Cpm52_axm1_1),
                            ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm52-uboot-axm1-1.bin");

                        ((Boardrevision >= "R6A") andalso (Secure_brd == true) andalso (Boardtype == "cpm1_52_SV_open")) ->

                            ct:pal("Secure dus52 with rev ~p,AXM1.1 and Param R5A release and 8GB mem",[Boardrevision]),
                            {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm52-uboot-r5a_x.bin", Cpm52_axm1_1),
                            ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm52-uboot-axm1-1.bin");

                        ((Boardrevision >= "R8A") andalso (Boardtype == "cpm1_52_A10_open")) ->

                            ct:pal("Secure dus52-A10 with rev ~p",[Boardrevision]),
                            {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm52-uboot-r4a_x.bin", Cpm52_axm1_1),
                            ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm52-uboot-axm1-1.bin");

                        true   ->
                            ct:fail("Unknown DUS52 board of type ~p with axm1.1 and rev: ~p detected",[Boardtype, Boardrevision])
                    end

               end;

   {ok, {product_tcu03,_}} ->
               case is_axm1_0() of

                 true ->
                     ct:pal("Detected AXM1.0 TCU03 board"),
                     Boardrevision = product_revision(),
                     if
                        (Boardrevision < "R3A") ->

                           ct:pal("tcu03 with rev ~p,AXM1.0 and R2B release and 4GB mem",[Boardrevision]),
                           {ok, _} = file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm03-uboot-r2bseq1.bin", Cpm03_axm1_0),
                           ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm03-uboot-axm1-0.bin");
                        true   ->
                           ct:fail("Unknown tcu03 board with axm1.0 and rev: ~p detected",[Boardrevision])
                     end;

                 false ->
                     ct:pal("Detected AXM1.1 TCU03 board"),
                     Boardrevision = product_revision(),
                     if
                        ((Boardrevision >= "R3A") andalso (Boardrevision =< "R3F")) ->

                            ct:pal("tcu03 with rev ~p,AXM1.1 and R3A release and 4GB mem",[Boardrevision]),
                            {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm03-uboot-r3b_x.bin", Cpm03_axm1_1),
                            ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm03-uboot-axm1-1.bin");
                        true   ->
                            ct:fail("Unknown tcu03 board with axm1.1 and rev: ~p detected",[Boardrevision])
                    end
               end;

  {ok, {product_dus32,_}} ->
              case is_axm1_0() of

                true ->
                   ct:pal("Detected AXM1.0 DUS32 board"),
                   Boardrevision = product_revision(),
                   ct:fail("dus32 board with axm1.0 and rev: ~p detected which is not yet supported in testing",[Boardrevision]);


                false ->
                   ct:pal("Detected AXM1.1 DUS32 board"),
                   Boardrevision = product_revision(),
                   if
                      ((Boardrevision >= "P4A") andalso (Boardrevision =< "R4A") andalso (Secure_brd == false)) ->

                          ct:pal("dus32 with rev ~p,AXM1.1 and Param R3A release and 4GB mem",[Boardrevision]),
                          {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm32-uboot-r3b_x.bin", Cpm32_axm1_1),
                          ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm32-uboot-axm1-1.bin");

                      ((Boardrevision >= "R5A") andalso (Secure_brd == false)  andalso (Boardtype == "cpm1_32_SV_open"))->

                          ct:pal("dus32 with rev ~p,AXM1.1 and Param R4A release and 4GB mem",[Boardrevision]),
                          {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm32-uboot-r4a_x.bin", Cpm32_axm1_1),
                          ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm32-uboot-axm1-1.bin");

                      ((Boardrevision >= "R1A")  andalso (Boardrevision =< "R5F") andalso (Secure_brd == true)  andalso (Boardtype == "cpm1_32_SV_open"))->

                          ct:pal("Secure dus32 with rev ~p,AXM1.1 and Param R4A release and 4GB mem",[Boardrevision]),
                          {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm32-uboot-r4a_x.bin", Cpm32_axm1_1),
                          ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm32-uboot-axm1-1.bin");

                      ((Boardrevision >= "R6A") andalso (Secure_brd == true)  andalso (Boardtype == "cpm1_32_SV_open"))->

                          ct:pal("Secure dus32 with rev ~p,AXM1.1 and Param R4A release and 4GB mem",[Boardrevision]),
                          {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm32-uboot-r5a_x.bin", Cpm32_axm1_1),
                          ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm32-uboot-axm1-1.bin");

                      ((Boardrevision >= "R8A") andalso (Boardtype == "cpm1_32_A10_open"))->

                          ct:pal("Secure dus32-A10 with rev ~p",[Boardrevision]),
                          {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm32-uboot-r4a_x.bin", Cpm32_axm1_1),
                          ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm32-uboot-axm1-1.bin");

                      true   ->
                          ct:fail("Unknown dus32 board of type ~p with axm1.1 and rev: ~p detected",[Boardtype, Boardrevision])
                   end
              end;

   {ok, {product_tcu04,_}} ->

              ct:pal("Detected TCU04 board"),
              Boardrevision = product_revision(),
              if
                 ((Boardrevision == "P1A") andalso (Secure_brd == false)) ->

                      ct:pal("tcu04 board with rev ~p, Param P1A release and 4GB mem",[Boardrevision]),
                      {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm04-uboot-p1a_x.bin", Cpm04_bin),
                      ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm04-uboot.bin");

                 ((Boardrevision >= "P1B") andalso (Boardrevision =< "R1B")) ->

                      ct:pal("tcu04 board with rev ~p, Param R1A release and 4GB mem",[Boardrevision]),
                      {ok, _}= file:copy(""++os:getenv("PWD")++"/scripts/uboot/config-cpm04-uboot-r1a_x.bin", Cpm04_bin),
                      ct_telnet:send(rs232,"dhcp; tftp 0x01000000 config-cpm04-uboot.bin");

                 true   ->
                      ct:fail("Unknown tcu04 board with rev: ~p detected",[Boardrevision])
              end;


   {_, _} ->   ct:fail("Unknown board type")


 end,

  wait_for_ubootprompt(8000),

  ct_telnet:sendf(rs232,"sf probe; sf erase ~s 0x10000",[Param_flash]),
  wait_for_ubootprompt(10000),

  ct_telnet:sendf(rs232,"sf probe; sf update 0x01000000 ~s 0x10000",[Param_flash]),
  wait_for_ubootprompt(10000),

  ct_telnet:send(rs232,"reset"),

  if
     (Param_flash == "0x00050000") ->
            String1Match = "Parameters: Watchdog 0 A\/B Valid 1\/1 A\/B Sequence 0\/1 => B",
            String2Match = "Parameters: Watchdog 1 A\/B Valid 1\/1 A\/B Sequence 0\/1 => A";
     (Param_flash == "0x00040000") ->
            String1Match = "Parameters: Watchdog 0 A\/B Valid 1\/1 A\/B Sequence 0\/1 => A",
            String2Match = "Parameters: Watchdog 1 A\/B Valid 1\/1 A\/B Sequence 0\/1 => B"
  end,

  case ct_telnet:expect(rs232, [String1Match, Param_tbl], [sequence, {halt,[Uboot_stop]},{timeout,10000},no_prompt_check]) of

       {ok,_} ->
             % verify the redundancy with watchdog restart

              verify_ubootprompt(),
              ct:pal("Param redundancy verification without watchdog restart sucessful"),
              ct_telnet:send(rs232,"restart wd"),

              case ct_telnet:expect(rs232, [String2Match, Param_tbl], [sequence, {halt,[Uboot_stop]},{timeout,10000},no_prompt_check]) of

                 {ok, _} ->
                          verify_ubootprompt(),
                          ct:pal("Param redundancy verification with watchdog restart sucessful"),
                          restore_secondary_paramarea(Param_flash);
                 {_, _} ->
                          ok = ct_telnet:send(rs232, "\n"),
                          ct_telnet:expect(rs232, "=> ", [{timeout,500},no_prompt_check]),
                          restore_secondary_paramarea(Param_flash),
                          ct:fail("Param redundancy check with watchdog restart failed")
             end;

       {_, _} ->
             ok = ct_telnet:send(rs232, "\n"),
             ct_telnet:expect(rs232, "=> ", [{timeout,500},no_prompt_check]),
             restore_secondary_paramarea(Param_flash),
             ct:fail("Param redundancy check failed")

  end,

  ok.


%% ===============================================================================
%% @doc
%%        This testcase verifies the uboot-stage3 redundancy
%%        Entry: Uboot prompt, Exit: Uboot prompt
%% @end
%% ===============================================================================

uboot_redundancy_testcase(_Config) ->

 Wdog_str = "U-Boot: Watchdog 1 A\/B Valid 1\/1 A\/B Sequence 7\/8 => A",
 No_wdog_str = "U-Boot: Watchdog 0 A\/B Valid 1\/1 A\/B Sequence 7\/8 => B",
 Eri_ver_str = "Ericsson Version: .*\/CXC1736593",

 Hw = atom_to_list(ct:get_config({test_nodes,1})),
 ct_telnet:send(rs232,"printenv uboot_b_sequence"),
 {ok, _} = ct_telnet:expect(rs232, "Error: \"uboot_b_sequence\" not defined", [{timeout,3000},no_prompt_check]),
 wait_for_ubootprompt(1000),

 ct_telnet:send(rs232,"ver"),
 {ok, InitUbootRev} = ct_telnet:get_data(rs232),
 timer:sleep(2000),

 Input = lists:filter(fun(X) -> string:str(X, "U-Boot")>0 end, InitUbootRev),

 case is_secure() of
           {true, _} ->
              ct_telnet:send(rs232,"dhcp; tftp 0x03000000 " ++ Hw ++ "/u-boot.img.ptad.signed");
           {false, _} ->
              ct_telnet:send(rs232,"dhcp; tftp 0x03000000 " ++ Hw ++ "/u-boot.img")
 end,

 case ct_telnet:expect(rs232, ["Load address:", "TFTP error"], [sequence, {halt,["=> "]},
                                                                               {timeout,6000},no_prompt_check]) of
  
    {ok, _} ->
              ct:fail("TFTP error while fetching the uboot image");
    {_,_} ->
              % Ensure uboot prompt and leave
              ok = ct_telnet:send(rs232, "\n"),
              {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,6000},no_prompt_check]),
              ok
 end,

 % updating it to UBOOT(2) area
 ct_telnet:send(rs232,"sf probe; sf update 0x03000000 0x00360000 0x80000"),
 wait_for_ubootprompt(8000),
 
 % Update environment variable to start the UBOOT(2)
  ct_telnet:send(rs232,"setenv uboot_a_sequence 7"),
  wait_for_ubootprompt(1000),
  ct_telnet:send(rs232,"setenv uboot_b_sequence 8"),
  wait_for_ubootprompt(1000),
  ct_telnet:send(rs232,"saveenv"),
  {ok, _} = ct_telnet:expect(rs232, ["Erasing SPI flash...Writing to SPI flash...done", "=> "],
                                                  [sequence, {timeout,15000},no_prompt_check]),
  ct_telnet:send(rs232,"printenv uboot_b_sequence"),
  case ct_telnet:expect(rs232, "Error: \"uboot_b_sequence\" not defined", [{timeout,3000},no_prompt_check]) of
       {ok, _} ->
                  ct:fail("Uboot sequence number B not set properly");
       {_, _} ->
                  ct_telnet:send(rs232,"printenv uboot_a_sequence"),
                  case ct_telnet:expect(rs232, "Error: \"uboot_a_sequence\" not defined", [{timeout,3000},no_prompt_check]) of
                      {ok, _} ->
                             ct:fail("Uboot sequence number A not set properly");
                      {_, _} ->
                             ok
                  end
  end,

  ct_telnet:send(rs232,"reset"),

  case ct_telnet:expect(rs232, [No_wdog_str, Eri_ver_str], [sequence, {halt,["Hit any key to stop autoboot:"]},
                                                                               {timeout,50000},no_prompt_check]) of

     {ok, _} ->
                verify_ubootprompt(),
                ct:pal("Uboot redundancy check without any watchdog restart is sucessful"),

                ct_telnet:send(rs232,"restart wd"),
                case ct_telnet:expect(rs232, [Wdog_str, Eri_ver_str], [sequence, {halt,["Hit any key to stop autoboot:"]},
                                                                                  {timeout,50000},no_prompt_check]) of

                         {ok, _} ->
                                     verify_ubootprompt(),
                                     ct:pal("Uboot redundancy check with a watchdog restart is sucessful"),
                                     restore_uboot_state(Input);
                         {_, _} ->
                                     ok = ct_telnet:send(rs232, "\n"),
                                     ct_telnet:expect(rs232, "=> ", [{timeout,500},no_prompt_check]),
                                     restore_uboot_state(Input),
                                     ct:fail("Uboot redundancy with watchdog restart is failed")

                end;

     {_, _} ->

                ok = ct_telnet:send(rs232, "\n"),
                ct_telnet:expect(rs232, "=> ", [{timeout,500},no_prompt_check]),
                restore_uboot_state(Input),
                ct:fail("Uboot redundancy test failed")
  end,

ok.



%% ===========================================================================
%% @doc
%%        This testcase verifies the memory commands mw,md,cp,cmp
%%        that can be used in Uboot
%%        Entry: Uboot prompt, Exit: Uboot prompt
%%
%% @end
%% ===========================================================================

memorycmds_testcase(_Config) ->

% Collect the memory information from temporary area that will be used

   ct_telnet:send(rs232,"md 0x23000000"),
   timer:sleep(1500),
   {ok, Data1} = ct_telnet:get_data(rs232),
   {Val1,_} = extract_val_memcmds("23000000:", Data1, 2, 0),

   ct_telnet:send(rs232,"md 0x23000010"),
   timer:sleep(1500),
   {ok, Data2} = ct_telnet:get_data(rs232),
   {Val2,_} = extract_val_memcmds("23000010:", Data2, 2, 0),

% Verfiy mw and md by writing into temporary memory area and reading it

   ct_telnet:send(rs232,"mw 0x23f00000 4"),
   ct_telnet:send(rs232,"md 0x23f00000"),

   ct_telnet:expect(rs232,"23f00000: 00000004",[{timeout,18000},no_prompt_check]),

% Verfiy memory copy and comparing by writing the same value in two memory locations

   ct_telnet:send(rs232,"cp 0x23f00000 0x23f00010 1"),
   ct_telnet:send(rs232,"cmp 0x23f00000 0x23f00010 1"),
   ct_telnet:expect(rs232,"Total of 1 word\\(s\\) were the same", [{timeout,18000},no_prompt_check]),

% Reset the used temporary memory area to its old value

   ct_telnet:sendf(rs232,"mw 0x23f00000 ~s",[integer_to_string(Val1)]),
   ct_telnet:sendf(rs232,"mw 0x23f00010 ~s",[integer_to_string(Val2)]).


%% ===========================================================================
%% @doc
%%        This testcase verifies there are sufficient number of
%%        mac addresses assigned to each of the available ports
%%        Entry: Uboot prompt, Exit: Uboot prompt
%%
%% @end
%% ===========================================================================
macaddresses_testcase(_Config) ->
   ct_telnet:send(rs232,"printenv"),
   timer:sleep(1000),

   {ok, Data} = ct_telnet:get_data(rs232),

   timer:sleep(1500),

%  Get the sublist with string ethaddr from the environment variables list

   Sublist = lists:filter(fun(X) -> string:str(X, "ethaddr")>0 end, Data),

   timer:sleep(1500),

   ct_telnet:send(rs232, "printenv productname"),
   case ct_telnet:expect(rs232,
                               [{product_tcu,"productname=TCU"},
                                {product_dus,"productname=DUS"}],
                                [{timeout,1500},no_prompt_check]) of

              {ok, {product_tcu,_}} ->
                         ct:pal("TCU03 or TCU04 board detected"),
                         if
                            (length(Sublist)>=18) ->
                                 ok;
                            true ->
                                 ct:fail("No of allowed MAC adresses are less than expected (18). count:~p",[length(Sublist)])
                         end;

              {ok, {product_dus,_}} ->
                         ct:pal("DUS52 or DUS32 board detected"),
                         if
                            (length(Sublist)>=24) ->
                                 ok;
                            true ->
                                 ct:fail("No of allowed MAC adresses are less than expected (24), count:~p",[length(Sublist)])
                         end;

              {_, _} -> ct:fail("Unknown board type")

   end.

%% ===========================================================================
%% @doc
%%        This testcase is to ensure that memtests are successful
%%        Entry: Uboot prompt, Exit: Uboot prompt
%%
%% @end
%% ===========================================================================
memtests_testcase(_Config) ->

  RcsTestArea1Start = "0x00400000",
  RcsTestArea1End   = "0x1fffffff",
  RcsTestArea2Start = "0x24000000",
  RcsTestArea2End   = "0x3fffffff",
  Pattern = "0x5a5a5a5a",
  Iterations = "2",

% Run the data bus test at rcs test areas

  ct_telnet:sendf(rs232,"smem databus ~s ~s",[RcsTestArea1Start, RcsTestArea1End]),
  {ok, _} = ct_telnet:expect(rs232, "Data line test finished with 0 errors", [{timeout,10000},no_prompt_check]),

  timer:sleep(3000),

  ct_telnet:sendf(rs232,"smem databus ~s ~s",[RcsTestArea2Start, RcsTestArea2End]),
  {ok, _} = ct_telnet:expect(rs232, "Data line test finished with 0 errors", [{timeout,10000},no_prompt_check]),

  timer:sleep(5000),

% Run the address bus test at rcs test areas

  ct_telnet:sendf(rs232,"smem addrbus ~s ~s",[RcsTestArea1Start, RcsTestArea1End]),
  {ok, _} = ct_telnet:expect(rs232, "Address line test finished with 0 errors", [{timeout,10000},no_prompt_check]),

  timer:sleep(3000),

  ct_telnet:sendf(rs232,"smem addrbus ~s ~s",[RcsTestArea2Start, RcsTestArea2End]),
  {ok, _} = ct_telnet:expect(rs232, "Address line test finished with 0 errors", [{timeout,10000},no_prompt_check]),

  timer:sleep(5000),

% Run the mem tests at rcs test areas

  ct_telnet:sendf(rs232,"smem memtest ~s ~s ~s ~s",[RcsTestArea1Start, RcsTestArea1End, Pattern, Iterations]),
  {ok, _} = ct_telnet:expect(rs232, "2 iteration\\(s\\) with 0 errors", [{timeout,10000},no_prompt_check]),

  timer:sleep(3000),

  ct_telnet:sendf(rs232,"smem memtest ~s ~s ~s ~s",[RcsTestArea2Start, RcsTestArea2End, Pattern, Iterations]),
  {ok, _} = ct_telnet:expect(rs232, "2 iteration\\(s\\) with 0 errors", [{timeout,10000},no_prompt_check]).


%% ===========================================================================
%% @doc
%%           This test case verfies the boot commands and the
%%           loading of kernel based on different values of boot count.
%%           It also verifies the reboot ( ordered restart )
%%           Entry: Type3 prompt, Exit: Type3 prompt
%%
%% @end
%% ===========================================================================

bootcmds_testcase(_Config) ->

   ct_telnet:send(rs232, "\n"),
   case ct_telnet:expect(rs232,
                         [{notprompt_a,"=> A"},
                          {notprompt_b,"=> B"},
                          {ubootprompt,"=> "}],
                          [{timeout,5000},no_prompt_check]) of
      {ok, {ubootprompt,_}} ->
          ct:pal("Board in Uboot mode"),

          % Issue reset command to bring up the board in type3
          ct_telnet:send(rs232,"boot"),

          ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

          % Wait for the login prompt
          ok = rct_rs232:login(rs232);


      {_, _} ->
          ok = rct_rs232:login(rs232)

   end,

   timer:sleep(8000),

   % Wait for the login prompt
   ok = rct_rs232:login(rs232),

   % Keep the node back in uboot mode with an ordered restart.

   ok = ct_telnet:send(rs232,"pgh_restartbrd -t \"bootcmds_tc: Setup for NL type2 loading\""),

   {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:", [{timeout,180000},no_prompt_check]),
   timer:sleep(1000),
   ok = ct_telnet:send(rs232, "\n"),
   {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,2000},no_prompt_check]),

   ct:pal("In Uboot mode"),

   % Verify the NL type2 loading based on boot count value
   boot_the_kernel(7),

   % Wait for the login prompt
   ok = rct_rs232:login(rs232),

   % Keep the node back in uboot mode

   ok = ct_telnet:send(rs232,"pgh_restartbrd -t \"bootcmds_tc: Setup for NL type3 loading\""),

   {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:", [{timeout,180000},no_prompt_check]),
   timer:sleep(1000),
   ok = ct_telnet:send(rs232, "\n"),
   {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,5000},no_prompt_check]),

   ct:pal("In Uboot mode"),

   % Verify the NL type3 loading based on boot count value
   boot_the_kernel(6),

   % Wait for the login prompt
   ok = rct_rs232:login(rs232),

   % Keep the node back in uboot mode

   ok = ct_telnet:send(rs232,"pgh_restartbrd -t \"bootcmds_tc: Setup for fallback kernel loading\""),

   {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:", [{timeout,180000},no_prompt_check]),
   timer:sleep(1000),
   ok = ct_telnet:send(rs232, "\n"),
   {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,5000},no_prompt_check]),

   ct:pal("In Uboot mode"),

   % Verify the fall back kernel loading based on boot count value
   boot_the_kernel(3),

   % Wait for the login prompt
   ok = rct_rs232:login(rs232),

   % Keep the node back in uboot mode

   ok = ct_telnet:send(rs232,"pgh_restartbrd -t \"bootcmds_tc: Setup for configured kernel loading\""),

   {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:", [{timeout,180000},no_prompt_check]),
    timer:sleep(1000),
    ok = ct_telnet:send(rs232, "\n"),
    {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,5000},no_prompt_check]),

    ct:pal("In Uboot mode"),



   % Verify the configured kernel loading based on boot count value

     boot_the_kernel(0),

   % Wait for the login prompt
    ok = rct_rs232:login(rs232),

   % Verify the reboot ( ordered restart ) and see that the boards comes up properly

   ok = ct_telnet:send(rs232, "pgh_restartbrd -t \"bootcmds_tc: Setup for restart verification\""),
   ct_telnet:expect(rs232,"Restarting system",[{timeout,80000},no_prompt_check]),

   ct_telnet:expect(rs232, ".*login", [{timeout,250000},no_prompt_check]),

   timer:sleep(12000),

   case rct_rs232:login(rs232) of
            ok ->
                    ok;
            {_,_} ->
                   {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:", [{timeout,180000},no_prompt_check]),
                   timer:sleep(1000),
                   ok = ct_telnet:send(rs232, "\n"),
                   {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,5000},no_prompt_check]),
                   ct:fail("Not able to login, board stopped in Uboot prompt")
   end,


   ct:pal("Verified ordered restart sucessfully").


%% ===========================================================================
%% @doc
%%    Verify the flash protection
%%        Entry: Type3 prompt, Exit: Type3 prompt
%% @end
%% ===========================================================================

verify_flash_protection(_Config) ->

  ct_telnet:send(rs232, "\n"),
  case ct_telnet:expect(rs232,
                         [{notprompt_a,"=> A"},
                          {notprompt_b,"=> B"},
                          {ubootprompt,"=> "}],
                          [{timeout,5000},no_prompt_check]) of
    {ok, {ubootprompt,_}} ->
          ct:pal("Board in Uboot mode"),

          % Issue reset command to bring up the board in type3
          ct_telnet:send(rs232,"reset"),

          ct_telnet:expect(rs232, ".*login", [{timeout,220000},no_prompt_check]),

          % Wait for the login prompt
          ok = rct_rs232:login(rs232);


    {_, _} ->
          ok = rct_rs232:login(rs232)

  end,

  % Verfying the flash protection status
  ok = ct_telnet:send(rs232,"mtdinfo /dev/mtd3"),
  case ct_telnet:expect(rs232, "Device is writable: .*false", [{timeout,80000},no_prompt_check]) of
          {ok, _} ->
                ct:pal("Flash protection enabled by default");
          {_, _} ->
                ct:fail("Flash protection is not enabled by default")
  end.


%% ===========================================================================
%% @doc
%%    Verify the L3 cache registers have correct value
%%        Entry: Type3 prompt, Exit: Type3 prompt
%% @end
%% ===========================================================================

verify_L3cache(_Config) ->

   ct_telnet:send(rs232, "\n"),
   case ct_telnet:expect(rs232,
                         [{notprompt_a,"=> A"},
                          {notprompt_b,"=> B"},
                          {ubootprompt,"=> "}],
                          [{timeout,5000},no_prompt_check]) of
    {ok, {ubootprompt,_}} ->
          ct:pal("Board in Uboot mode"),

          % Issue reset command to bring up the board in type3
          ct_telnet:send(rs232,"boot"),

          ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

          % Wait for the login prompt
          ok = rct_rs232:login(rs232);


    {_, _} ->
          ok = rct_rs232:login(rs232)

   end,

  % Give enough time to settle.

   timer:sleep(5000),
   switch_to_testbox(),
   timer:sleep(9000),

   ct_telnet:send(rs232,"testbox axmacc rmem -32 0x2000200018"),
   {ok, _} = ct_telnet:expect(rs232, "0x0000000c", [{timeout,5000},no_prompt_check]),

   ct_telnet:send(rs232,"testbox axmacc rmem -32 0x2000210018"),
   {ok, _} = ct_telnet:expect(rs232, "0x0000000c", [{timeout,5000},no_prompt_check]),

   ct_telnet:send(rs232,"testbox axmacc rmem -32 0x2000220018"),
   {ok, _} = ct_telnet:expect(rs232, "0x0000000c", [{timeout,5000},no_prompt_check]),

   ct_telnet:send(rs232,"testbox axmacc rmem -32 0x2000230018"),
   {ok, _} = ct_telnet:expect(rs232, "0x0000000c", [{timeout,5000},no_prompt_check]),

   ct_telnet:send(rs232,"testbox axmacc rmem -32 0x2000240018"),
   {ok, _} = ct_telnet:expect(rs232, "0x0000000c", [{timeout,5000},no_prompt_check]),

   ct_telnet:send(rs232,"testbox axmacc rmem -32 0x2000250018"),
   {ok, _} = ct_telnet:expect(rs232, "0x0000000c", [{timeout,5000},no_prompt_check]),

   ct_telnet:send(rs232,"testbox axmacc rmem -32 0x2000260018"),
   {ok, _} = ct_telnet:expect(rs232, "0x0000000c", [{timeout,5000},no_prompt_check]),

   ct_telnet:send(rs232,"testbox axmacc rmem -32 0x2000270018"),
   {ok, _} = ct_telnet:expect(rs232, "0x0000000c", [{timeout,5000},no_prompt_check]),

   switch_to_root(),

ok.


%% ===========================================================================
%% @doc
%%    Expire the watchdog and ensure watchdog restart happens
%%        Entry: Type3 prompt, Exit: Type3 prompt
%% @end
%% ===========================================================================


verify_hwwatchdog(_Config) ->

   ct_telnet:send(rs232, "\n"),
   case ct_telnet:expect(rs232,
                         [{notprompt_a,"=> A"},
                          {notprompt_b,"=> B"},
                          {ubootprompt,"=> "}],
                          [{timeout,5000},no_prompt_check]) of
     {ok, {ubootprompt,_}} ->
          ct:pal("Board in Uboot mode"),

          % Issue reset command to bring up the board in type3
          ct_telnet:send(rs232,"boot"),

          ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

          % Wait for the login prompt
          ok = rct_rs232:login(rs232);


     {_, _} ->
          ok = rct_rs232:login(rs232)

   end,

  % Give enough time to settle.
  
   timer:sleep(5000),  
   switch_to_testbox(),
   timer:sleep(9000), 

   % Generate HW watchdog

   ct_telnet:send(rs232,"testbox --verbose 2 axmacc wmem -32 0x20100910a0 0x10"),

%  verify_ubootprompt(),

%  ok = ct_telnet:send(rs232,"rcs 1"),

%  {ok, _} = ct_telnet:expect(rs232, "Reset Status = HW Watchdog", [{timeout,18000},no_prompt_check]),

   case ct_telnet:expect(rs232, "Connection refused", [{timeout,5000},no_prompt_check]) of
   
       {ok, _} -> 
                  verify_testbox_mode(),
                  ct_telnet:send(rs232,"testbox --verbose 2 axmacc wmem -32 0x20100910a0 0x10");
        _  ->
                  ok
   end,

   ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),
   timer:sleep(10000),
   
   % Wait for the login prompt
   ok = rct_rs232:login(rs232),

   timer:sleep(8000),
   switch_to_testbox(),

   timer:sleep(10000),
   
   Restart_reason = mailbox_read("0x2000000c"),

   switch_to_root(),

   if
      (Restart_reason == "0xaffe2222")  ->
                   ct:pal("HWwatchdog verified sucessfully");
        true ->
                   ct:fail("Watchdog restart failed, restart reason is ~p",[Restart_reason])
   end,


   ok.

%% ===========================================================================
%% @doc
%%    Verify load procedure and match the expected strings
%%        Entry: Uboot prompt, Exit: Type3 prompt
%% @end
%% ===========================================================================


verify_load_process(_Config) ->

Param = "Description: CPM1-.* AXM HW config CXC 173 6595/.*",

ProdNum = "RCS: productnumber KDU.*" ,

ProdRev = "RCS: productrevision",

ProdName = "RCS: productname",

ProdDate = "RCS: productdate",

SerialNum = "RCS: serialnumber",

DtbVer = "RCS: Ericsson DTB Version: .*CXC1736593.*",

Voltage = "RCS: BPM SVS configuration: 940 mV",

Uboot_prompt = "Hit any key to stop autoboot:",

HwWatchdog = "RCS: System watchdog enabled",

FpgaLoad = "RCS: CPM1 FPGA loading ... Successful!",

FitImg = "RCS: Looking for fit dtb image",

SoftDtb = "RCS: Looking for soft.dtb file",

Powerdown1 = "Setting up Coherency for Clusters:",

Powerdown2 = "Powering down cluster",

Powerdown_tcu_dus32 = "Brought up 8 CPUs",

Powerdown_dus52 = "Brought up 12 CPUs",


ct_telnet:send(rs232, "printenv productname"),
case ct_telnet:expect(rs232,
                               [{product_tcu,"productname=TCU 0"},
                                {product_dus52,"productname=DUS 52"},
                                {product_dus32,"productname=DUS 32"}],
                                      [{timeout,1500},no_prompt_check]) of

  {ok, {product_tcu,_}} ->
     
     ct:pal("TCU03 board detected"),
     % Reset the board
     ok = ct_telnet:send(rs232, "reset"),

     % Expect all the above strings in order
     {ok, _} = ct_telnet:expect(rs232, [Param, Voltage, Uboot_prompt, ProdNum, ProdRev, ProdName, ProdDate, SerialNum, 
                                         HwWatchdog, FpgaLoad, FitImg, DtbVer, SoftDtb, Powerdown1, Powerdown2, Powerdown_tcu_dus32 ],
                                                    [sequence, {halt,[".*login"]},{timeout,200000},no_prompt_check]);

  {ok, {product_dus52,_}} -> 
     
     ct:pal("DUS52 board detected"),
     % Reset the board
     ok = ct_telnet:send(rs232, "reset"),

     % Expect all the above strings in order
     {ok, _} = ct_telnet:expect(rs232, [Param, Voltage, Uboot_prompt, ProdNum, ProdRev, ProdName, ProdDate, SerialNum, 
                                               HwWatchdog, FpgaLoad, FitImg, DtbVer, SoftDtb, Powerdown1, Powerdown_dus52 ],
                               [sequence, {halt,[".*login"]},{timeout,200000},no_prompt_check]);

  {ok, {product_dus32,_}} ->

     ct:pal("DUS52 board detected"),
     % Reset the board
     ok = ct_telnet:send(rs232, "reset"),

     % Expect all the above strings in order
     {ok, _} = ct_telnet:expect(rs232, [Param, Voltage, Uboot_prompt, ProdNum, ProdRev, ProdName, ProdDate, SerialNum, 
                                               HwWatchdog, FpgaLoad, FitImg, DtbVer, SoftDtb, Powerdown1, Powerdown_tcu_dus32 ],
                                      [sequence, {halt,[".*login"]},{timeout,200000},no_prompt_check]);


  {_,_} ->    ct:fail("Unknown board type")

end,

ok.

%% ===========================================================================
%% @doc
%%    Verify Uboot commands
%%        Entry: Uboot prompt, Exit: Uboot prompt
%% @end
%% ===========================================================================

verify_ubootcommands(_Config) ->

  ct:pal("Executing few Uboot commands"),

  ok = ct_telnet:send(rs232,"bdinfo"),
  {ok, _} = ct_telnet:expect(rs232, ["arch_number", "FB base"], [sequence, {halt,["=> "]},{timeout,3000},no_prompt_check]),

  ok = ct_telnet:send(rs232,"clocks"),
  {ok, _} = ct_telnet:expect(rs232, ["System:", "Cpu:", "Memory:", "Fabric:", "Tree:", "Peripheral:", "SD/eMMC:"], 
                                                                [sequence, {halt,["=> "]},{timeout,3000},no_prompt_check]),

  ok = ct_telnet:send(rs232,"norinfo"),
  {ok, _} = ct_telnet:expect(rs232, ["Manufacturer ID:", "Memory Type:","Memory Capacity:"], [sequence, {halt,["=> "]},
                                                                                           {timeout,3000},no_prompt_check]),

  ok = ct_telnet:send(rs232,"ramlog"),
  {ok, _} = ct_telnet:expect(rs232, ["ramlog", "RCS U-Boot printf ramlog start:","Memory Capacity:"], 
                                                            [sequence, {halt,[".*login"]},{timeout,10000},no_prompt_check]),

  ok = ct_telnet:send(rs232,"pci"),
  {ok, _} = ct_telnet:expect(rs232, ["Scanning PCI devices on bus 0", "Network controller"], [sequence, {halt,["=> "]},
                                                                                           {timeout,3000},no_prompt_check]),

  ok = ct_telnet:send(rs232,"temp"),
  {ok, _} = ct_telnet:expect(rs232, ["AXM on die", "Primary side"], [sequence, {halt,["=> "]},{timeout,3000},no_prompt_check]),

  % Led verification
  verify_led("red"),
  verify_led("green"),
  verify_led("blue"),
  verify_led("yellow"),

ok.


%% ===========================================================================
%% @doc
%%        This test case verifies that the target is loaded with secureboot
%%        Entry: Uboot prompt, Exit: Uboot prompt
%% @end
%% ===========================================================================

verify_secureboot(_Config) ->

   Uboot_main_str = "Welcome to RCS\/U-boot main application",
   Secure_str = "RCS: Secure Boot Enabled",
   Img_str = "cmd_lsi_sbb: Image is Valid",
   
   case is_secure() of
           {true, _} ->
                       ok = ct_telnet:send(rs232, "rcs 1"),
                       {ok, _} = ct_telnet:expect(rs232, [Uboot_main_str, Img_str, Secure_str], [sequence, {halt,["=> "]},
                                                                                         {timeout,10000},no_prompt_check]);   
           {false, _} ->
                       ct:pal("Not a secure board, skipping this testcase"),
                       ct:comment("Not a secure board, skipping this testcase")
   end,

ok.

%% ===========================================================================
%% @doc
%%        This test case verifies the Semi Permanent storage
%%        Entry: Type3 prompt, Exit: Type3 prompt
%% @end
%% ===========================================================================

verify_semiPermStorage(_Config) ->

   ct_telnet:send(rs232, "\n"),
   case ct_telnet:expect(rs232,
                         [{notprompt_a,"=> A"},
                          {notprompt_b,"=> B"},
                          {ubootprompt,"=> "}],
                          [{timeout,5000},no_prompt_check]) of
       {ok, {ubootprompt,_}} ->
          ct:pal("Board in Uboot mode"),

          % BPM restart the board to bring up in type3
          ok = ct_telnet:send(rs232,"restart bpm"),

          ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

          timer:sleep(15000),
          % Wait for the login prompt
          ok = rct_rs232:login(rs232);


       {_, _} ->
          ok = rct_rs232:login(rs232)

   end,

   % Mount pramfs area in xtra area
   {ok, _} = ct_telnet:cmd(rs232,"dmesg --clear"),
   {ok, _} = ct_telnet:cmd(rs232,"mkdir /home/sirpa/xtra"),
   {ok, _} = ct_telnet:cmd(rs232,"mount -t pramfs -o physaddr=0x20010000,init=1M pramfs /home/sirpa/xtra"),
   {ok, _} = ct_telnet:cmd(rs232,"echo \"hello world\" > /home/sirpa/xtra/TESTFILE_WAS_HERE; sync; sync",25000),

   ok = ct_telnet:send(rs232,"ls -l /home/sirpa/xtra"),
   case ct_telnet:expect(rs232, "TESTFILE_WAS_HERE", [{timeout,10000},no_prompt_check]) of
          {ok, _} ->
                     ok = ct_telnet:send(rs232, "cat  /home/sirpa/xtra/TESTFILE_WAS_HERE"),
                     {ok, _} = ct_telnet:expect(rs232, "hello world", [{timeout,10000},no_prompt_check]);
          {_, _} ->
                     {ok, _} = ct_telnet:cmd(rs232,"dmesg")
   end,

   % Order cold restart
   ok = ct_telnet:send(rs232,"pgh_restartbrd 0"),

   % Stop in uboot prompt
   verify_ubootprompt(),

   % Fill test pattern in reserved area in SEMI
   ok = ct_telnet:send(rs232,"mw.l 0x23f00000 0x5a5a5a5a 0x200"),
   wait_for_ubootprompt(10000),

   ok = ct_telnet:send(rs232,"md 0x23f00000 1"),
   wait_for_ubootprompt(10000),

   % start Linux OS
   ok = ct_telnet:send(rs232,"rcs"),
   ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

   % Wait for the login prompt
   rct_rs232:login(rs232),
   timer:sleep(15000),
   ok = rct_rs232:login(rs232),

   % Verify that test files still exist in pramfs
   mount_pramfs(),

   ok = ct_telnet:send(rs232,"ls -l /home/sirpa/xtra"),
   case ct_telnet:expect(rs232, "TESTFILE_WAS_HERE", [{timeout,10000},no_prompt_check]) of
          {ok, _} ->
                     ok = ct_telnet:send(rs232, "cat  /home/sirpa/xtra/TESTFILE_WAS_HERE"),
                     {ok, _} = ct_telnet:expect(rs232, "hello world", [{timeout,10000},no_prompt_check]);
          {_, _} ->
                     {ok, _} = ct_telnet:cmd(rs232,"dmesg")
   end,

   % Order coldwtest restart
   ok = ct_telnet:send(rs232,"pgh_restartbrd 1"),

   % Stop in uboot. This requires more delay as it is coldwtest restart
   {ok, _} = ct_telnet:expect(rs232, "Hit any key to stop autoboot:", [{timeout,360000},no_prompt_check]),
   ok = ct_telnet:send(rs232, "\n"),
   {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,1000},no_prompt_check]),

   % verify test pattern in SEMI
   % fill same test pattern for compare test in ordinary RAM.
   ok = ct_telnet:send(rs232,"mw.l 0x10000000 0x5a5a5a5a 0x200"),
   wait_for_ubootprompt(10000),
   ok = ct_telnet:send(rs232,"md 0x23f00000 1"),
   wait_for_ubootprompt(10000),
   ok = ct_telnet:send(rs232,"cmp.l 0x10000000 0x23f00000 0x200"),

   {ok, _} = ct_telnet:expect(rs232, "Total of 512 word\\(s\\) were the same",
                                                              [{timeout,6000},no_prompt_check]),

   % start Linux OS
   ok = ct_telnet:send(rs232,"rcs"),
   ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

   % Wait for the login prompt
   rct_rs232:login(rs232),
   timer:sleep(15000),

   % There can be a testmgr restart after coldwtest restart. So wait for login prompt again
   ct_telnet:expect(rs232, "RCS: First time stamp", [{timeout,100000},no_prompt_check]),
   
   case ct_telnet:expect(rs232, [{restart_poweron,"RCS: Reset Status = Power On \\(BPM SW\\)"},
                                 {restart_cold,"RCS: Reset Status = SW Ordered \\(Cold\\)"}],
                                                [{timeout,100000},no_prompt_check]) of

       {ok, {restart_poweron,_}} -> ct:pal("Coldwtest restart seems to have failed with some reason"),
                                    Restarttype = "poweron";

       {ok, {restart_cold,_}} -> Restarttype = "cold";

       {_, _} -> Restarttype = "unknown",
                 ct:fail("Unknown restart after a coldwtest restart")
   end, 
   
   ct_telnet:expect(rs232, ".*login", [{timeout,100000},no_prompt_check]),

   % Wait for the login prompt
   rct_rs232:login(rs232),
   timer:sleep(15000),
   ok = rct_rs232:login(rs232),

   if 
      ( Restarttype == "poweron") ->
           ct:pal("Coldwtest tests failed and hence poweron restart"),
           % Read hwlog for debug purpose
           ct_telnet:cmd(rs232,"hwlog",30000),
           ct:comment("Failed CWT test results, Skipping validation during cwt"); 
  
      true -> 
           % Verify that test files still exist in pramfs
           mount_pramfs(),

           ok = ct_telnet:send(rs232,"ls -l /home/sirpa/xtra"),
           case ct_telnet:expect(rs232, "TESTFILE_WAS_HERE", [{timeout,10000},no_prompt_check]) of
                 {ok, _} ->
                        ok = ct_telnet:send(rs232, "cat  /home/sirpa/xtra/TESTFILE_WAS_HERE"),
                        {ok, _} = ct_telnet:expect(rs232, "hello world", [{timeout,10000},no_prompt_check]);
                 {_, _} ->
                        {ok, _} = ct_telnet:cmd(rs232,"dmesg")
           end,

           % Order cold restart
           ok = ct_telnet:send(rs232,"pgh_restartbrd 0"),

           % Stop in uboot prompt
           verify_ubootprompt(),

           % verify test pattern in SEMI
           % fill same test pattern for compare test in ordinary RAM.
           ok = ct_telnet:send(rs232,"mw.l 0x10000000 0x5a5a5a5a 0x200"),
           wait_for_ubootprompt(10000),
           ok = ct_telnet:send(rs232,"md 0x23f00000 1"),
           wait_for_ubootprompt(10000),
           ok = ct_telnet:send(rs232,"cmp.l 0x10000000 0x23f00000 0x200"),

           case ct_telnet:expect(rs232, "Total of 512 word\\(s\\) were the same",
                                                               [{timeout,6000},no_prompt_check]) of
              {ok, _} ->  
                         ok;
              {_, _} ->
                         ct:fail("Test pattern verification in uboot prompt failed after coldwtest restart")
           end,
   
           % start Linux OS
           ok = ct_telnet:send(rs232,"rcs"),
           ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

           % Wait for the login prompt
           rct_rs232:login(rs232),
           timer:sleep(15000),
           ok = rct_rs232:login(rs232),

           % Verify that test files still exist in pramfs
           mount_pramfs(),

           ok = ct_telnet:send(rs232,"ls -l /home/sirpa/xtra"),
           case ct_telnet:expect(rs232, "TESTFILE_WAS_HERE", [{timeout,10000},no_prompt_check]) of
                  {ok, _} ->
                        ok = ct_telnet:send(rs232, "cat  /home/sirpa/xtra/TESTFILE_WAS_HERE"),
                        {ok, _} = ct_telnet:expect(rs232, "hello world", [{timeout,10000},no_prompt_check]);
                  {_, _} ->
                        {ok, _} = ct_telnet:cmd(rs232,"dmesg")
           end
   end,   

   % Generate HW watchdog and stop in U-Boot
   switch_to_testbox(),
   timer:sleep(3000),

   % Generate HW watchdog
   ct_telnet:send(rs232,"testbox --verbose 2 axmacc wmem -32 0x20100910a0 0x10"),

   case ct_telnet:expect(rs232, [{retry,"Connection refused"},
                                 {uboot_s3,"Hit any key to stop autoboot:"}],
                                [{timeout,15000},no_prompt_check]) of

       {ok, {retry,_}} ->
                          verify_testbox_mode(),
                          ct_telnet:send(rs232,"testbox --verbose 2 axmacc wmem -32 0x20100910a0 0x10"),
                          verify_ubootprompt();

       {ok, {uboot_s3,_}} ->
                          ok = ct_telnet:send(rs232, "\n"),
                          {ok, _} = ct_telnet:expect(rs232, "=> ", [{timeout,1000},no_prompt_check]);
       {_, _} ->
                          verify_ubootprompt()
   end,

   % verify test pattern in SEMI
   % fill test pattern for compare test in ordinary RAM.
   ok = ct_telnet:send(rs232,"mw.l 0x10000000 0x5a5a5a5a 0x200"),
   wait_for_ubootprompt(10000),
   ok = ct_telnet:send(rs232,"md 0x23f00000 1"),
   wait_for_ubootprompt(10000),
   ok = ct_telnet:send(rs232,"cmp.l 0x10000000 0x23f00000 0x200"),

   {ok, _} = ct_telnet:expect(rs232, "Total of 0 word\\(s\\) were the same",
                                                              [{timeout,6000},no_prompt_check]),

   % start Linux OS
   ok = ct_telnet:send(rs232,"rcs"),
   ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

   % Wait for the login prompt
   rct_rs232:login(rs232),
   timer:sleep(15000),
   ok = rct_rs232:login(rs232),

   % Verify that test files still exist in pramfs
   mount_pramfs(),

   ok = ct_telnet:send(rs232,"ls -l /home/sirpa/xtra"),
   case ct_telnet:expect(rs232, "TESTFILE_WAS_HERE", [{timeout,10000},no_prompt_check]) of
         {ok, _} ->
                    ct:fail("TESTFILE still exists in \/var\/trace");
         {_, _} ->
                    ok
   end,

ok.

%% ===========================================================================
%% @doc
%%        This test case verifies the rcs1 testmode
%%        Entry: Uboot prompt, Exit: Type3 prompt
%% @end
%% ===========================================================================

verify_rcs_1_testmode(_Config) ->

   Testmode = "affeeeee",
   DBB_str = "RCS: DBB5 debug switch in position 0",

   % Check the restart reason
   ok = ct_telnet:send(rs232,"md 0x2000000c 1"),
   timer:sleep(1000),
   {ok, Data} = ct_telnet:get_data(rs232),
   Restartreason = extract_val_memcmds("2000000c:", Data, 2, 1),

   ct:pal("Restart reason :~s",[Restartreason]),

   % Put in test mode
   ct_telnet:send(rs232,"rcs 1"),

   % Verify timestamp adjustment

   {ok, _} = ct_telnet:expect(rs232, "RCS: Time stamp in mailbox adjusted with .*seconds", 
                                                                  [{timeout,8000},no_prompt_check]),

   wait_for_ubootprompt(15000),

   % Verify test mode
   ct_telnet:send(rs232,"md 0x2000000c 1"),
   timer:sleep(1000),
   {ok, Mode} = ct_telnet:get_data(rs232),
   Testmode = extract_val_memcmds("2000000c:", Mode, 2, 1),

   ct:pal("Test mode :~s",[Testmode]),

   % Verify restart reason is not cleared with rcs 1, testmode setting
   if
        (Restartreason == "affe8888") ->
                 RestartString = "RCS: Reset Status = SW Ordered \\(Cold\\)",
                 Time_out = 25000;

        (Restartreason == "affeaaaa") ->
                 RestartString = "RCS: Reset Status = SW Ordered \\(Cold with test\\)",
                 Time_out = 500000;

        (Restartreason == "affe2222") ->
                 RestartString = "Reset Status = HW Watchdog",
                 Time_out = 18000;

        (Restartreason == "affe6666") ->
                 RestartString = "Reset Status = Power On \\(BPM SW\\)",
                 Time_out = 18000

   end,

   ct_telnet:send(rs232,"rcs"),

   {ok, _} = ct_telnet:expect(rs232,[DBB_str, RestartString], [sequence, {halt,[".*login"]},
                                                                   {timeout,Time_out},no_prompt_check]),

   ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

   % Wait for the login prompt
   rct_rs232:login(rs232),
   timer:sleep(10000),
   ok = rct_rs232:login(rs232),
ok.

%% ===========================================================================
%% @doc
%%        This test case verifies few dtb contents
%%        Entry: Uboot prompt, Exit: Type3 prompt
%% @end
%% ===========================================================================

verify_type2data(_Config) ->

   % Type2 data
   Spl = "uboot_stage2_version.*CXC1736593.*",
   Stage3 = "uboot_stage3_version.*CXC1736593.*",
   Param = "hw_param_config_version.*AXM HW config CXC 173 6595.*",

   % ECC counters
   Smem_ecc_cnt = "smem_uncorr_ecc_cnt =",
   L3_ecc_cnt = "l3_uncorr_ecc_cnt =",
   L2_ecc_cnt = "l2_uncorr_ecc_cnt =",
   Cpu_ecc_cnt = "cpu_uncorr_ecc_cnt =",

   % Execute in rcs 1 mode
   ct_telnet:send(rs232,"rcs 1"),

   wait_for_ubootprompt(15000),

   % Verify type2 data available in DTB
   ct_telnet:send(rs232,"fdt print /rcs_nodes/type2_sw_data"),
   {ok, _} = ct_telnet:expect(rs232, [Spl, Stage3, Param ],[sequence, {halt,["=> "]},{timeout,8000},no_prompt_check]),

   % Verify that ecc counters are available in DTB
   ct_telnet:send(rs232,"fdt print /rcs_nodes/ecc_information_data"),
   {ok, _} = ct_telnet:expect(rs232, [Smem_ecc_cnt, L3_ecc_cnt, L2_ecc_cnt, Cpu_ecc_cnt],[sequence, {halt,["=> "]},
                                                                                        {timeout,8000},no_prompt_check]),

ok.


%% ===========================================================================
%% @doc
%%        Verify the ecc handling on tcu03 boards
%%        Skip/return success for dusx2 board as there is no ECC memory
%%        Entry: Uboot prompt, Exit: Type3 prompt
%% @end
%% ===========================================================================
verify_ecc(_Config) ->

  ct_telnet:send(rs232, "printenv productname"),
  case ct_telnet:expect(rs232,
                             [{product_dus,"productname=DUS"},
                              {product_tcu,"productname=TCU"}],
                                     [{timeout,1500},no_prompt_check]) of

      {ok, {product_dus,_}} ->
          ct:pal("Skipping the testcase as there is no ECC memory available on dusx2 boards");

      {ok, {product_tcu,_}} ->

          Ind_bit = "00000008",
          Reset_ind_bit = "00000000",
          %  Restart_ecc_reason = "0xaffedddd",

          ECC_str1 = "RCS: Uncorrectable ECC error found. Update restart reason in mailbox",
          ECC_str2 = "RCS: ECC error in SMEM",

          % Set bit3 in AXM persistent indication register
          ok = ct_telnet:send(rs232,"mw 0x900300dc 8"),

          % Flush L3
          ok = ct_telnet:send(rs232,"cmp.b 0x0 0x0 0x800000"),
          {ok, _} = ct_telnet:expect(rs232, "Total of 8388608 byte\\(s\\) were the same",
                                                                 [{timeout,5000},no_prompt_check]),

          % restart cold without retention
          ok = ct_telnet:send(rs232,"restart cold"),

          {ok, _} = ct_telnet:expect(rs232, "RCS: Reset Status = .*10", [{timeout,10000},no_prompt_check]),
          {ok, _} = ct_telnet:expect(rs232, "ddrRecovery = 0", [{timeout,5000},no_prompt_check]),

          % Stop in uboot prompt
          verify_ubootprompt(),

          % Verify that indication bit still exist after restart
          ok = ct_telnet:send(rs232,"md 0x900300dc 1"),
          timer:sleep(1000),
          {ok, Data} = ct_telnet:get_data(rs232),
          Ind_bit = extract_val_memcmds("900300dc:", Data, 2, 1),

          % Verify the reset status in rcs command
          ct_telnet:send(rs232,"rcs 1"),
          {ok, _} = ct_telnet:expect(rs232, [ECC_str1, ECC_str2],[sequence, {halt,["=> "]},
                                                                       {timeout,8000},no_prompt_check]),

          wait_for_ubootprompt(8000),

          % Verify that indication bit reset with rcs command
          ok = ct_telnet:send(rs232,"md 0x900300dc 1"),
          timer:sleep(1000),
          {ok, Reset_Data} = ct_telnet:get_data(rs232),
          Reset_ind_bit = extract_val_memcmds("900300dc:", Reset_Data, 2, 1),

          % start Linux OS
          ok = ct_telnet:send(rs232,"rcs"),
          ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

          % Wait for the login prompt
          rct_rs232:login(rs232),
          timer:sleep(10000),
          ok = rct_rs232:login(rs232),

          % Print the reset reason from mailbox

          switch_to_testbox(),
          timer:sleep(10000),
          Restart_reason = mailbox_read("0x2000000c"),
          ct:pal("Restart reason:~p",[Restart_reason]),
          switch_to_root()
  end,

ok.

%% ===========================================================================
%% @doc
%%        Testcase to verify ioexp command.
%%        Entry: Uboot prompt, Exit: Type3 prompt
%% @end
%% ===========================================================================
verify_ioexp(_Config) ->

  ok = ct_telnet:send(rs232,"ioexp input 1"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 1 value: 1"),
  ok = ct_telnet:send(rs232,"ioexp input 2"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 2 value: 1"),
  ok = ct_telnet:send(rs232,"ioexp input 5"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 5 value: 1"),
  ok = ct_telnet:send(rs232,"ioexp input 6"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 6 value: 1"),
  
  ok = ct_telnet:send(rs232,"ioexp clear 0"), 
  
  ok = ct_telnet:send(rs232,"ioexp input 1"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 1 value: 0"),
  ok = ct_telnet:send(rs232,"ioexp input 2"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 2 value: 0"),
  ok = ct_telnet:send(rs232,"ioexp input 5"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 5 value: 1"),
  ok = ct_telnet:send(rs232,"ioexp input 6"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 6 value: 1"),
  
   % Keep the board back in proper state.
  ok = ct_telnet:send(rs232,"rcs"),
  ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

  % Wait for the login prompt
  rct_rs232:login(rs232),
  timer:sleep(10000),
  ok = rct_rs232:login(rs232),

  ct_telnet:send(rs232,"pgh_restartbrd -t \"Verify_ioexp: Setup for ioexp results with bpmrestart \""),

  verify_ubootprompt(), 
  
  ok = ct_telnet:send(rs232,"ioexp input 1"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 1 value: 1"),
  ok = ct_telnet:send(rs232,"ioexp input 2"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 2 value: 1"),
  ok = ct_telnet:send(rs232,"ioexp input 5"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 5 value: 1"),
  ok = ct_telnet:send(rs232,"ioexp input 6"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 6 value: 1"),
    
  ok = ct_telnet:send(rs232,"restart bpm"),
  
  verify_ubootprompt(), 
  
  ok = ct_telnet:send(rs232,"ioexp input 1"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 1 value: 0"),
  ok = ct_telnet:send(rs232,"ioexp input 2"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 2 value: 1"),
  ok = ct_telnet:send(rs232,"ioexp input 5"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 5 value: 1"),
  ok = ct_telnet:send(rs232,"ioexp input 6"), 
  {ok, _} = ct_telnet:expect(rs232, "ioexp pin 6 value: 1"),
  
  % Keep the board back in proper state.
  ok = ct_telnet:send(rs232,"rcs"),
  ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

  % Wait for the login prompt
  rct_rs232:login(rs232),
  timer:sleep(10000),
  rct_rs232:login(rs232),
   
ok.

%% ===========================================================================
%% @doc
%%        Testcase to verify error escalation in uboot.
%%        Entry: Uboot prompt, Exit: Type3 prompt
%% @end
%% ===========================================================================

verify_errorescalation(_Config) ->

  Uboot_str = "Hit any key to stop autoboot",
  Config_str = "RCS.*load configured kernel",
  Failed_config = "RCS: Loading of configured kernel failed - try fallback kernel",
  Fallbk_str = "RCS: Load fallback kernel",
  Failed_fallbk = "RCS: Loading of fallback kernel failed - try network loader \\(type3\\) kernel",
  Nl3_str = "RCS: Load network loader \\(type3\\) kernel",

  ct_telnet:send(rs232, "\n"),
  case ct_telnet:expect(rs232,
                         [{notprompt_a,"=> A"},
                          {notprompt_b,"=> B"},
                          {ubootprompt,"=> "}],
                             [{timeout,5000},no_prompt_check]) of
    {ok, {ubootprompt,_}} ->
          ct:pal("Board in Uboot mode"),

          % BPM restart the board to bring up in type3
          ok = ct_telnet:send(rs232,"restart bpm"),

          ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

          timer:sleep(12000),
          % Wait for the login prompt
          ok = rct_rs232:login(rs232);


    {_, _} ->
          ok = rct_rs232:login(rs232)

  end,

  ct_telnet:send(rs232,"mount /dev/sda2/ /mnt"),
  ct_telnet:send(rs232,"cd /mnt/b1;rm -rf linux.img"),
  ct_telnet:send(rs232,"pgh_restartbrd -t \"Verify_corrupted_or_deleted_img_in_configured partition\""),

  {ok, _} = ct_telnet:expect(rs232, [Uboot_str], [{timeout,80000},no_prompt_check]),

  {ok, _} = ct_telnet:expect(rs232, [Config_str, Failed_config, Fallbk_str, Failed_fallbk, Nl3_str],
                                                 [sequence, {halt,[".*login"]},{timeout,90000},no_prompt_check]),

  {ok, _} = ct_telnet:expect(rs232, "Welcome to Autointegration", [{timeout,120000},no_prompt_check]),
  % Install the board from network loader prompt
  ok = aic_curl:install_sw(),
  ct_telnet:expect(rs232, ".*login", [{timeout,220000},no_prompt_check]),
  timer:sleep(10000),

  % Wait for the login prompt
  ok = rct_rs232:login(rs232),
  timer:sleep(3000),

  wait_for_dbbackup(),

  switch_to_testbox(),
  timer:sleep(5000),

  {ok, Init_data} = ct_telnet:cmd(rs232,"testbox --verbose 2 axmacc rmem -32 0x20000020", 18000),

  timer:sleep(1000),
  
  case lists:all(fun(X) -> string:str(X, "Connection refused")==0 end, Init_data) of

         false -> 
                  verify_testbox_mode(),
                  {ok, Data1} = ct_telnet:cmd(rs232,"testbox --verbose 2 axmacc rmem -32 0x20000020", 18000);

         true ->
                  Data1 = Init_data
  end,

  switch_to_root(),

  {_,SubStr} = extract_val_memcmds("\[0x20000020\] =", Data1, 3, 0),

  BootStr = string:substr(SubStr, 6, 4),
  {BootConfigured,_} = string:to_integer(BootStr),
  ct:pal("Loaded with bootConfigured: ~p  \(configured:3333, fallback:5555\)",[BootConfigured]),

 ok.

%% ===========================================================================
%% @doc
%%        Restore testcase to keep the node back in
%%        normal mode by power cycling the board from uboot
%%        Entry: Uboot prompt, Exit: Type3 prompt
%% @end
%% ===========================================================================

reboot_to_normal(Config) ->
   ct:pal("Keeping the node back in normal mode"),

%  Hw = atom_to_list(ct:get_config({test_nodes,1})),

   cleanup_ubootenv(),

% To enable rebuild option for jenkins test job, we will keep the files as it is
   
%   case is_secure() of
%           {true, Dtb} ->
%                  file:delete("/proj/rcs-tmp/tftpboot/" ++ Hw ++ "/u-boot.img.ptad.signed"),
%                  file:delete("/proj/rcs-tmp/tftpboot/" ++ Hw ++ "/" ++ Dtb ++ ".dtb.ptad.signed");
%           {false, Dtb} ->
%                  file:delete("/proj/rcs-tmp/tftpboot/" ++ Hw ++ "/u-boot.img"),
%                  file:delete("/proj/rcs-tmp/tftpboot/" ++ Hw ++ "/" ++ Dtb ++ ".dtb")
%   end,

   ok = ct_telnet:send(rs232, "restart bpm"),
   ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

   % Wait for the login prompt
   rct_rs232:login(rs232),
   timer:sleep(15000),
   % Confirm the login
   ok = rct_rs232:login(rs232),

   {ok, UbootVersion} = ct_telnet:cmd(rs232,"rhdc ubootversion",15000),
   {ok, DtbVersion} = ct_telnet:cmd(rs232,"rhdc dtbversion | grep \"DTB version\"",15000),
   
   Version = lists:filter(fun(X) -> string:str(X, "root@")==0 end, lists:append(UbootVersion, DtbVersion)),
      
   ct:pal("Uboot Versions used in testing :\n~p",[lists:filter(fun(X) -> string:str(X, "rhdc")==0 end, Version)]),

   % Fetch the ESI logs
   aic_curl:fetch_esi(Config),

ok.

%% ===========================================================================
%% @doc
%%        2.5changer testcase 
%%        Entry: Type3 prompt, Exit: Type3 prompt
%% @end
%% ===========================================================================
verify_fwupgrade_upgradablepartition(Config)->

  % Fetch the board info from the configuration
  Hw = atom_to_list(ct:get_config({test_nodes,1})), 

  % Ensure that the board is stable type3 prompt 
  ct_telnet:send(rs232, "\n"),
  case ct_telnet:expect(rs232,
                        [{notprompt_a,"=> A"},
                         {notprompt_b,"=> B"},
                         {ubootprompt,"=> "}],
                         [{timeout,5000},no_prompt_check]) of
    {ok, {ubootprompt,_}} ->
          ct:pal("Board in Uboot mode"),

          % Issue reset command to bring up the board in type3
          ct_telnet:send(rs232,"boot"),

          ct_telnet:expect(rs232, ".*login", [{timeout,200000},no_prompt_check]),

          % Wait for the login prompt
          case rct_rs232:login(rs232) of
                ok ->
                     ok;
                _  ->
                     timer:sleep(15000),
                     ok =  rct_rs232:login(rs232)
          end;

    {_, _} ->
          ok = rct_rs232:login(rs232)

  end,

  % Ensure that flash protection is enabled

  ct_telnet:send(rs232,"mtdinfo /dev/mtd3"),
  % {ok, _} = ct_telnet:expect(rs232, "Device is writable: .*false", [{timeout,10000},no_prompt_check]),

  % Change the current working directory
  
  {ok, Old_wd} = file:get_cwd(),
  file:set_cwd(os:getenv("PWD")),

  % Usecase 1: Verify the fw_upgrade

  Upg_op = os:cmd("./board/rcs/cpm1/tools/rcs_changeTYPE2.5.exp " ++ Hw ++ " /proj/rcs-tmp/tftpboot/" ++ Hw ++ " -upgrade 0"),

  % For test purpose to run manually
  % Upg_op = os:cmd(""++os:getenv("PWD")++"/scripts/rcs_changeTYPE2.5.exp " ++ Hw ++ " /proj/rcs-tmp/tftpboot/" ++ Hw ++ " -upgrade 0"),

  io:format(Upg_op),

  Fin_upg_op = extract_script_ouput(Upg_op),

  % ct:pal("Ouput from  fw_upgrade script: \n ~p \n", [Fin_upg_op]),

  ExpectedUp_strs = ["Upgrading firmware...","HW parameter image updated",
                    "DTB image updated","U-Boot S3 image updated","done!",
                    "2.5 changer LM operation completed successfully",
                    "Reboot after type 2 change successful"],

  Upg_hwparam = "Parameters: Watchdog 0 A\/B Valid 1\/1 A\/B Sequence 0\/1 => B",
  Upg_uboot_s3 = "U-Boot: Watchdog 0 A\/B Valid 1\/1 A\/B Sequence 0\/1 => B",
  Upg_dtb_str1 = "RCS: DTB info: valid 1\/1, seq 0\/1, wd 0",
  Upg_dtb_str2 = "RCS: Fetching DTB-B area",

  case (lists:suffix(ExpectedUp_strs, Fin_upg_op)) of
       true ->   
              ct:pal("Firmware upgrade script executed sucessfully"),

              % Validate that the upgrade is sucessful

              % Check that board is up
              case rct_rs232:login(rs232) of
                    ok ->
                           ok;
                    _ ->
                           timer:sleep(15000),
                           ok =  rct_rs232:login(rs232)
              end,

              % Restart the board and validate the strings
              ok = ct_telnet:send(rs232,"pgh_restartbrd -t \"Restart to validate the firmware upgrade step\""),

              % Validate that the hw param, uboot stage 3 and dtb are selected from upgradable partition
              {Upgstatus, _} = ct_telnet:expect(rs232, [Upg_hwparam, Upg_uboot_s3, Upg_dtb_str1, Upg_dtb_str2],
                                                     [sequence, {timeout,150000},no_prompt_check]),

              ct_telnet:expect(rs232, ".*login", [{timeout,60000},no_prompt_check]),

              % Check that board is up
              case rct_rs232:login(rs232) of
                    ok ->
                           ok;
                    _ ->
                           timer:sleep(15000),
                           ok =  rct_rs232:login(rs232)
              end,

              if
                 (Upgstatus == ok) ->
                                   % Verify the fw_restore
                                   Verify_restore = "true",
                                   fw_factoryrestore(Verify_restore, Config);
                 true ->
                          Verify_restore = "false",
                          fw_factoryrestore(Verify_restore, Config),

                          % Reset back the working directory  
                          file:set_cwd(Old_wd),
  
                          ct:fail("Firmware upgrade validation failed")
              end;

       false ->
              ct:pal("Firmware upgrade script execution failed"),       

              Verify_restore = "false",
              fw_factoryrestore(Verify_restore, Config),

              % Reset back the working directory
              file:set_cwd(Old_wd),
  
              ct:fail("Firmware upgrade script failed")
  end,

  % Usecase 3: Empty file name
  clean_up_ubootfiles(),

  Empty_upgfile_op = os:cmd("./board/rcs/cpm1/tools/rcs_changeTYPE2.5.exp " ++ Hw ++ " /proj/rcs-tmp/tftpboot/" ++ Hw ++ " -upgrade 1"),

  % For test purpose to run manually
  % Empty_upgfile_op = os:cmd(""++os:getenv("PWD")++"/scripts/rcs_changeTYPE2.5.exp " ++ Hw ++ " /proj/rcs-tmp/tftpboot/" ++ Hw ++ " -upgrade 1"),

  io:format(Empty_upgfile_op),

  Fin_emptyupgfile_op = extract_script_ouput(Empty_upgfile_op),

  % ct:pal("Ouput from script with empty upgrade file: \n ~p \n", [Fin_emptyupgfile_op]),

  Expected_emptyupgfile_strs = ["Upgrading firmware...","not done!",
                                "FW_UPGRADE_ERROR: firmware upgrade failed"],

  case (lists:suffix(Expected_emptyupgfile_strs, Fin_emptyupgfile_op)) of
       true ->
              ct:pal("Usecase with empty upgrade file verified sucessfully"),

              % Ensure that board is still up in working state
              ok = rct_rs232:login(rs232);

       false ->
              % Reset back the working directory
              file:set_cwd(Old_wd),

              ct:fail("Verification with empty upgrade file failed")
  end,

  % Usecase 4: Wrong upgrade file name
  clean_up_ubootfiles(),

  Wrong_upgfile_op = os:cmd("./board/rcs/cpm1/tools/rcs_changeTYPE2.5.exp " ++ Hw ++ " /proj/rcs-tmp/tftpboot/" ++ Hw ++ " -upgrade 2"),

  % For test purpose to run manually
  % Wrong_upgfile_op = os:cmd(""++os:getenv("PWD")++"/scripts/rcs_changeTYPE2.5.exp " ++ Hw ++ " /proj/rcs-tmp/tftpboot/" ++ Hw ++ " -upgrade 2"),

  io:format(Wrong_upgfile_op),

  Fin_wrongupgfile_op = extract_script_ouput(Wrong_upgfile_op),

  % ct:pal("Ouput from script with wrong upgrade filename: \n ~p \n", [Fin_wrongupgfile_op]),

  Expected_wrongupgfile_strs = ["FW_UPGRADE_ERROR: ucf file not found"],

  case (lists:suffix(Expected_wrongupgfile_strs, Fin_wrongupgfile_op)) of
       true ->
              ct:pal("Usecase with wrong upgrade filename verified sucessfully"),

              % Ensure that board is still up in working state
              ok = rct_rs232:login(rs232);

       false ->
              % Reset back the working directory
              file:set_cwd(Old_wd),

              ct:fail("Verification with wrong upgrade filename failed")
  end,

  % Reset back the working directory
  file:set_cwd(Old_wd),

  ok.
