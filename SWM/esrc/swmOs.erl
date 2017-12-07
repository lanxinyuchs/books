%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmOs.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/17

%%% @doc ==OS manipulating functions==
%%% This module contains OS installation manipulating functions which are
%%% later going to be covered by Linux EE
%%% @end

-module(swmOs).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/17').
-date('2017-10-16').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB %CCaseTemplateCopyrightYear% All rights reserved.
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
%%% R2A/1      2014-03-11 etxjotj     Created
%%% R2A/5      2014-03-17 etxjotj     Adding call to reboot in tcu fallback fix
%%% R2A/6      2014-03-18 etxjotj     Fixed bootfs for tcu03
%%% R2A/7      2014-03-18 etxjotj     Fix HS32665
%%% R2A/8      2014-04-04 erarafo     Added 'umount' support
%%% R2A/9      2014-04-04 erarafo     Privileges adjusted
%%% R2A/10     2014-04-05 erarafo     Moving patches: guard for missing source
%%% R2A/11     2014-04-06 erarafo     Elaborated the unmount helpers
%%% R2A/12     2014-04-06 erarafo     More informative warnings
%%% R2A/13     2014-04-07 erarafo     Use -l option only with install_ee_tcu03
%%% R2A/14     2014-04-09 etxjotj     Test of boot counter
%%% R2A/15     2014-04-10 etxjotj     Switched back to temp fallback solution
%%% R2A/21     2014-05-05 etxjotj     get_active_instance/0 added
%%% R2A/22     2014-05-05 erarafo     Typo fixed
%%% R2A/23     2014-05-13 etxjotj     Upgrade boot pointer
%%% R2A/24     2014-05-15 etxjotj     Support for taipan upgrade
%%% R2A/25     2014-05-15 etxjotj     Support for both ug ways in tcu03
%%% R2A/26     2014-05-16 etxjotj     Dialyzer fixes
%%% R2A/27     2014-05-22 erarafo     HS62272
%%% R2A/28     2014-06-09 etxjotj     Handle multiple OTP versions
%%% R2A/30     2014-06-12 etxjotj     Split EE
%%% R2A/30     2014-06-17 etxarnu     Fixed get_uboot_version
%%% R2A/33     2014-06-23 etxberb     TR HS62175: Incorrect use of 'filelib:
%%%                                   is_file' in remove_mount_point/2 fixed.
%%% R2A/34     2014-07-09 etxjotj     Fixed concurrency problem in unpack_cxp
%%% R2A/35     2014-07-09 etxjotj     Adaptions for bundles
%%% R2A/36     2014-07-10 etxjotj     Bugfix
%%% R2A/37     2014-07-11 etxjotj     Exported cmdres/1
%%% R2A/38     2014-07-21 etxjotj     Handling of optional metadata elements
%%%                                   Improved handling of erroneous meta files
%%% R2A/39     2014-07-24 etxjotj     Path fix in heart_fallback file
%%% R2A/40     2014-07-24 etxjotj     HS34552 Support for dus52 board type
%%% R2A/41     2014-07-25 etxjotj     Fail on cup errors
%%%                                   Support for dus52 uboot
%%% R2A/42     2014-07-28 etxjotj     Recreate UP, modify Up for test
%%% R2A/43     2014-07-31 etxjotj     New cup to handle os restore
%%% R2A/44     2014-08-01 etxjotj     Check disk space, HS23701
%%% R2A/45     2014-08-04 etxjotj     Bugfix
%%% R2A/46     2014-08-04 etxjotj     Manage disk space
%%%                                   Bugfix in reading uboot version
%%% R2A/48     2014-08-13 erarafo     "Move patches" handled uniformly
%%% R2A/49     2014-08-14 etxjotj     Fix of HS87494 unpack bundle from /tmp
%%% R2A/50     2014-09-04 etxjotj     Support for signed software
%%% R2A/51     2014-09-11 etxjotj     New method of recognizing UBOOT
%%% R2A/52     2014-09-11 etxjotj     New uboot at commit
%%% R3A/1      2014-09-24 etxjotj     Support for dus32
%%% R3A/2      2014-09-25 etxjotj     Support for dus32 UBOOT
%%% R2A/53     2014-10-03 etxjotj     Clarified disk space shortage message
%%% R3A/4      2014-10-22 etxjotj     Support for dus32 UBOOT P1B
%%% R3A/5      2014-10-28 erarafo     Mounting the 'os' CXP first of all
%%% R3A/6      2014-10-29 erarafo     Handle missing 'os' marker
%%% R3A/7      2014-10-31 erarafo     Fixed missing newline in log message
%%% R3A/8      2014-11-07 etxjotj     Better handling of size string
%%% R3A/9      2014-11-10 etxjotj     Bugfix in previous change
%%% R3A/10     2014-11-12 etxjotj     Size compare fix
%%% R3A/11     2014-11-26 etxjotj     Cup error codes
%%% R3A/12     2015-01-23 etxderb     Upg nl: prepare_nl_boot, write_nl_boot
%%% R3A/13     2015-03-20 etxderb     Added reset_boot/0, for BoardRestore
%%% R3A/15     2015-04-09 etxjotj     Cleaned compiler warning
%%% R3A/16     2015-04-24 etxjotj     Printout improvement
%%% R3A/17     2015-05-08 etxderb     reset_boot => reset_boot/1
%%% R3A/18     2015-05-21 etxjotj     HT76614 Send alarm for cxp umount failure
%%% R3A/19     2015-06-05 etxjotj     Update cxp list, get lv list
%%% R4A/1      2015-05-20 etxjotj     Support for upgrade of new board types
%%% R4A/2      2015-06-05 etxjotj     Merge from R3A/19
%%% R4A/3      2015-07-16 etxjotj     TCU upgrade fix for mount list
%%% R4A/4      2015-07-20 etxjotj     Lots of printouts moved to swmInternal
%%%                                   and only in erlang log if needed
%%% R4A/5      2015-07-20 etxjotj     Bugfix for sim
%%% R4A/6      2015-07-21 etxjotj     Dialyzer fixes
%%% R4A/7      2015-08-11 etxarnu     Added clear_applicationtmp
%%% R4A/9      2015-09-04 etxjotj     Added support for fsck
%%% R4A/11     2015-09-08 etxjotj     HU15047 Check if cxp is mounted 
%%% R4A/14     2015-11-11 etxjotj     Removed uboot version check
%%% R5A/1      2016-03-03 etxjotj     Removed test for /tmp/cxp_list
%%% R5A/2      2016-03-09 etxberb     Adjustment for HAL functionality.
%%% R5A/3      2016-03-10 etxberb     Temporary disabling of HAL routines.
%%% R5A/6      2016-03-15 etxberb     Enabled HAL routines.
%%% R5A/7      2016-03-18 etxberb     Handle backup restore to UP without HAL.
%%% R5A/8      2016-04-01 etxberb     Bug fix in HAL dir calculation.
%%% R5A/10     2016-04-14 etxberb     Return value fr read_swp_selected changed.
%% ----    ---------- -------  ------------------------------------------------
%% R6A/1   2016-04-21 etxjotj  No use for cup -w anymore
%% R6A/2   2016-04-28 etxberb  Changed "RADIO" to "OTHER".
%% R6A/3   2016-05-20 etxpejn  Change for BPU upgrade in make_fallback_script (RCS* -> *)
%% R6A/5   2016-06-03 etxpejn  Corr in activate_os
%% R6A/6   2016-06-07 etxpejn  Added * to clear_applicationlogs & clear_applicationtmp
%% R6A/7   2016-06-07 etxpejn  Corr for clear_applicationlogs & clear_applicationtmp
%% R6A/8   2016-08-05 etxjotj  Use preload for moving patches only
%% R6A/9   2016-08-17 etxjotj  New cup for reading active instance
%% R6A/10  2016-08-29 etxpejn  New cup for signed software revocation
%% R6A/11  2016-09-01 etxjotj  Rollback at restore + activate fix
%% R6A/12  2016-09-01 etxjotj  Time measurements for wr8
%% R6A/14  2016-09-14 etxberb  Added get_fallback_instance/0 &
%%                             set_boot_instance/2 and keeping mount of
%%                             other sw dir (/rcs/swm/homeX) in homes/1.
%% R6A/15  2016-09-14 etxjotj  Added error message for corrupt file
%% ----    ---------- -------  ------------------------------------------------
%% R7A/1   2016-09-30 etxarnu  Corrected switch_over script for vrcs
%% R7A/2   2016-09-30 etxarnu  Bugfix
%% R7A/3   2016-09-30 etxjotj  Added info message for os cxp
%% R7A/4   2016-10-04 etxjotj  Fix for os_cxp handling HV30481
%% R7A/5   2016-10-18 etxberb  HV33559: Added make_cxpfilename/2.
%% R7A/6   2016-10-28 etxjotj  HV37348: Checksum printouts for corrupt files
%% ----    ---------- -------  ------------------------------------------------
%% R8A/1   2016-10-31 etxpejn  Disabled check_crl
%% R8A/3   2016-12-07 etxberb  Bug fix in log_format/1.
%% R8A/4   2016-12-29 etxberb  Added remove_last_nl/1.
%% R8A/6   2017-01-15 etxpejn  Added WP5618 Signed SW Certificate Revocation &
%%                             Anti-rollback
%% R8A/8   2017-02-02 etxberb  HV60949: HAL correction in find_os_cxp_file/3.
%% ----    ---------- -------  ------------------------------------------------
%% R9A/1   2017-04-27 etxberb  Exported make_links/3.
%% R9A/2   2017-05-15 etxberb  Added get_inactive_instance/0.
%% R9A/3   2017-05-18 etxberb  HV88108: Added check on ProductsOTH in
%%                             cxp_files/3.
%% R9A/4   2017-05-19 etxberb  HAL fix; Added make_links/4 & dir/3.
%% R9A/5   2017-05-30 etxberb  HV90906: Removed old_remove_cxp/2.
%% R9A/6   2017-06-13 etxberb  HV94465: Added check on ProductsOTH in
%%                             install_bundle/8.
%% R9A/7   2017-06-16 etxberb  Added install_up/2.
%% ----    ---------- -------  ------------------------------------------------
%% R10A/1  2017-05-31 etxarnu  Removed os:putenv (obsolete and dangerous)
%% R10A/2  2017-06-13 etxberb  Merge from R9A/6.
%% R10A/3  2017-06-16 etxberb  Merge from R9A/7.
%% R10A/5  2017-07-10 etxjotj  Changed API to install_up
%% R10A/6  2017-07-12 etxjotj  Fixed map bug and move mount_sda1 from swmLib
%%                             Optimized UP install
%% R10A/8  2017-07-13 etxjotj  Install EE first
%% ----    ---------- -------  ------------------------------------------------
%% R11A/1  2017-08-03 etxberb  Bootfallback enabled:
%%                             * Added is_home_mounted/1,remove_metadata_other/0
%%                               remove_installation_inactive/0preload/1.
%% R11A/2  2017-08-04 etxberb  Handle find os cxp when it's not unpacked
%% R11A/3  2017-08-08 etxjotj  Bugfix in install_up
%% R11A/4  2017-08-17 etxberb  Detailed ERROR logging in extract_cxp_data/1.
%% R11A/5  2017-08-23 etxberb  Added BRCS-EE case
%% R11A/6  2017-09-04 etxjotj  Added remounnt tmpfs cup command
%% R11A/7  2017-09-04 etxjotj  Fixed get_active_instance for SIM
%% R11A/8  2017-09-05 etxjotj  Use rcs_mode_2 
%% R11A/9  2017-09-05 etxjotj  Bugfix
%% R11A/10 2017-09-08 etxjotj  Fixed get_active_instance for vrcs
%% R11A/11 2017-09-08 etxjotj  Fixed more rcs_mode_2 cases for vrcs
%% R11A/12 2017-09-11 etxjotj  Fixed more rcs_mode_2 cases for vrcs
%% R11A/13 2017-09-12 etxpejn  Fix in commit for simulated
%% R11A/14 2017-09-18 etxberb  HW29014: Added find_os_product/1.
%% R11A/16 2017-10-12 etxberb  Fixed dialyzer problems.
%% R11A/17 2017-10-16 etxjotj  OTP20 adaptions
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([homes/1, home_dir_other/0, is_home_mounted/1]).
-export([preload/0, preload/1,
	 activate/0, activate_os/1,
	 commit/0,
	 rollback_at_restore/0,
	 clear/0, remove_installation_inactive/0]).

-export([install_up/1, install_up/2]).
-export([mount_cxp_deferring/2]). 

-export([make_links/3, make_links/4]).

-export([recreate_up/1, modify_up_version/1]).

-export([disk_free/0, disk_usage/1]).

-export([get_active_instance/0,
	 get_fallback_instance/0,
	 get_inactive_instance/0]).
%% -export([run_swm_wrapper/1, run_swm_wrapper_res/1]).
-export([unmount/2]).
-export([get_installed_cxps/0, remove_cxp/1]).

-export([reset_boot/1]).
-export([set_boot_instance/2]).

-export([clear_application_logs/0, clear_application_tmp/0]).

-export([cmdres/1, cmdreslog/1]).

-export([fsck/0, fsck/1]).

%% WP5618 signed software revocation & anti-rollback
-export([get_crl_ids/1]).
-export([store_crl/1]).
-export([check_crl/1]).

%% Mount HAL-SWP area
-export([mount_sda1/0,
	 mount_sda1/1,
	 umount_sda1/0,
	 umount_sda1/1]).

%%% swmI
-export([remount_tmpfs_size_cmd/2]).


%% -export([fuser_check/1]).
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsSwM.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").
-include("SwmInternal.hrl").

%% This path is used if unsquashfs cannot be found in the
%% ordinary PATH.
-define(SQUASHFS_TOOLS_SIM, "/app/rbs/wrtools/tools-sdk-20130220/usr/sbin").

%-compile(export_all).
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Mount or unmount both home volumes
%%% Volumes are mounted under /rcs/swm/home{1,2}
%%% @end
%%% ----------------------------------------------------------

-spec homes(Op::mount|umount) -> ok.

homes(Op) ->
    homes(sysEnv:rcs_mode_2(), sysEnv:board_type(), Op).

homes(target, BT, Cmd) when BT == dus ; BT == duw2 ->
    erlang:error(unsupported_board_type, [target, BT, Cmd]);

homes(target, _, mount) ->
    cmd("sudo cup --mount '/dev/rootvg/hfs1-lv /rcs/swm/home1'"),
    cmd("sudo cup --mount '/dev/rootvg/hfs2-lv /rcs/swm/home2'"),
    %% run_swm_wrapper("mount /dev/rootvg/hfs1-lv /rcs/swm/home1"),
    %% run_swm_wrapper("mount /dev/rootvg/hfs2-lv /rcs/swm/home2"),
    file:write_file(filename:join(home_dir(inactive), "mounted_rcs_swm"), <<>>),
    file:write_file(filename:join(home_dir(active), "mounted_rcs_swm"), <<>>),
    ok;
homes(target, _, umount) ->
    file:delete(filename:join(home_dir(active), "mounted_rcs_swm")),
    case get_active_instance() of
	1 ->
	    %% run_swm_wrapper("umount /rcs/swm/home1");
	    cmd("sudo cup --umount '/rcs/swm/home1'");
	2 ->
	    %% run_swm_wrapper("umount /rcs/swm/home2")
	    cmd("sudo cup --umount '/rcs/swm/home2'")
    end,
    ok;
homes(simulated, _, _) ->
    ok;
homes(vrcs, _, _) ->
    ok.


%%% ----------------------------------------------------------
is_home_mounted(Instance) ->
    filelib:is_file(filename:join(home_dir(Instance), "mounted_rcs_swm")).

%%% ----------------------------------------------------------
%%% @doc Returns the path to the non active home dir
%%% Classical cases are handled by sysEnv
%%% @end
%%% ----------------------------------------------------------

home_dir_other() ->
    home_dir(inactive).

%%% ----------------------------------------------------------
home_dir(Instance) ->
    home_dir(sysEnv:rcs_mode_2(), sysEnv:board_type(), Instance).
    
%%% ----------------------------------------------------------
home_dir(target, BT, _) when BT == dus ; BT == duw2 ->
    erlang:error(unsupported_board_type, [target, BT]);
home_dir(target, _, inactive) ->
    case get_inactive_instance() of
	1 -> "/rcs/swm/home1/sirpa";
	2 -> "/rcs/swm/home2/sirpa"
    end;
home_dir(target, _, active) ->
    case get_active_instance() of
	1 -> "/rcs/swm/home1/sirpa";
	2 -> "/rcs/swm/home2/sirpa"
    end;
home_dir(simulated, _, inactive) ->
    PrivDir = code:priv_dir(sys),
    {match, [{Start, Length}]} = re:run(PrivDir, "home.?/"),
    case string:substr(PrivDir, Start+1, Length-1) of
	"home" ->
	    filename:join([sysEnv:rcs_root(), "home2", os:getenv("USER")]);
	"home2" ->
	    sysEnv:home_dir()
    end;
home_dir(_, _, _) ->
    sysEnv:home_dir().


%%% ----------------------------------------------------------
%%% @doc Install all load module containers and prepare the other home area
%%% Given an UP MO, unpack (and later mount) the included
%%% loadmodule containers on the mount point.
%%% This function presumes that both home directories are available
%%% Input: 
%%%  UP:#upgradePackage{}
%%%    Opts:map() with keys
%%%      hsiEnabled::true|false|undefined - optional, 'undefined' if not present
%%%      installMode::legacy|optimized|no_mount - opt, 'legacy' if not present
%%%               no_mount - only logical volumes are created - no mount
%%%               mount - normal mount
%%%               legacy - old implementation
%%% @end
%%% ----------------------------------------------------------

-spec install_up(UP::#upgradePackage{}) -> ok.

install_up(UP) ->
    Opts = #{hsiEnabled => is_hsi_enabled()},
    install_up(UP, Opts).

%%% ----------------------------------------------------------
is_hsi_enabled() ->
    case swmLib:get_variable(hsi_state) of
	on ->
	    true;
	_ ->
	    false
    end.

%%% ----------------------------------------------------------
-spec install_up(UP::#upgradePackage{}, Opts::map()) -> ok.

install_up(UP, Opts) when is_map(Opts)->
    info_msg("Installing ~p~n~p~n",
	     [element(4,UP#upgradePackage.upgradePackageId),
	      Opts]),
    HsiEnabled = case swmLib:map_get_optional(hsiEnabled, Opts) of
		     {ok, V} -> V;
		     badkey -> undefined
		 end,

    SwVersionDir = swmLib:software_dir_other(),
    cmd("mkdir -p "++SwVersionDir),

    ArchiveDir = get_up_archive_dir(UP#upgradePackage.administrativeData),

    BoardTypesOTH =
	case HsiEnabled of
	    true ->
		swmBoardList:boardTypes_oth();
	    _ ->
		all
	end,
    ProductsOTH =
	case
	    swmBoardList:swp([products],
			     [{boardTypesOTH, BoardTypesOTH},
			      {options, [{global, ArchiveDir}]}])
	    of
	    {ok, [{products, P_oth}]} ->
		P_oth;
	    {error, Reason1} ->
		throw({uc_error, Reason1})
	end,
    {ProductsBB, SwpIds} =
	case
	    swmBoardList:swp([products, swp_id],
			     [{boardTypeBB, swmBoardList:boardType()},
			      {options, [{global, ArchiveDir}]}])
	    of
	    {ok, [{products, P_bb}, {swp_id, SIs}]} ->
		{P_bb, SIs};
	    {error, Reason2} ->
		throw({uc_error, Reason2})
	end,
    Products = ProductsBB ++ ProductsOTH,
    RcsMode = sysEnv:rcs_mode_2(),
    InstallMode = swmLib:map_get_optional(installMode, Opts),
    try case InstallMode of
	    {ok, no_mount} ->
		Env = #{archiveDir => ArchiveDir,
			swpIds => SwpIds, 
			hsiEnabled => HsiEnabled,
			installMode => no_mount,
			productsOTH => ProductsOTH},
		install_software(RcsMode, Products, Env),
		info_msg("After linking to software:~n~n~s~n",
			 [os:cmd("echo " ++ SwVersionDir ++ 
				 " ; ls -la " ++ SwVersionDir)]);
	    {ok, mount} ->
		Env = #{archiveDir => ArchiveDir,
			swpIds => SwpIds, 
			hsiEnabled => HsiEnabled,
			installMode => mount,
			productsOTH => ProductsOTH},
		install_software(RcsMode, Products, Env),
		info_msg("After linking to software:~n~n~s~n",
			 [os:cmd("echo " ++ SwVersionDir ++ 
				 " ; ls -la " ++ SwVersionDir)]);
	    _ ->
		CxpFiles = cxp_files(Products, ArchiveDir, ProductsOTH, SwpIds),
		mount_software(RcsMode, CxpFiles)
	end
    catch
	throw : {cxp_files, metadata_filename_missing} ->
	    %% Restore backup to a UP with old metadata files. Backup without
	    %% HAL functionality.
	    CxpFiles2 = filelib:wildcard(filename:join(ArchiveDir, "*.cxp")),
	    mount_software(sysEnv:rcs_mode_2(), CxpFiles2)
    end,

    case InstallMode of
	{ok, no_mount} -> ok;
	_ ->
	    info_msg("After mounting of software:~s~n~s~n",
		     ["ls -la "++os:cmd("echo $RCS_ROOT/software"),
		      os:cmd("ls -la $RCS_ROOT/software")])
    end,

    %% Prepare to generate necessary files

    MetaFile = get_up_file(filename:join(ArchiveDir, "*-up.xml")),
    cmd("cp "++MetaFile++" "++SwVersionDir++"/"),

    case InstallMode of
	{ok, no_mount} ->
	    ok;
	{ok, mount} ->
	    CxsPath = filename:join(SwVersionDir, 
				    filename:basename(MetaFile)),
	    swmModel:update_sw_version(CxsPath),
	    swmInventory:update_sw_version(CxsPath);
	    
	_ ->
	    %% Make links to /software
	    case sysEnv:rcs_mode_2() of
		target ->
		    make_links(ArchiveDir, SwVersionDir, Products, 
			       SwpIds, ProductsOTH),
		    info_msg("After linking to software:~n~n~s~n",
			     [os:cmd("echo " ++ SwVersionDir ++ 
					 " ; ls -la " ++ SwVersionDir)]),
		    CxsPath = filename:join(SwVersionDir, 
					    filename:basename(MetaFile)),
		    
		    swmModel:update_sw_version(CxsPath),
		    swmInventory:update_sw_version(CxsPath);
		_ ->
		    %% For SIM this is done in unpack_cxp
		    ok
	    end		
    end,
    ok.

%%% ----------------------------------------------------------
cxp_files([{_, {_, undefined}} = Product | _], GlobalDir, _, _) ->
    sysInitI:warning_report([{?MODULE, cxp_files},
			     {old_UP, metadata_filename_missing},
			     Product,
			     {globalDir, GlobalDir}]),
    throw({cxp_files, metadata_filename_missing});
cxp_files([{{Name, Id, Version}, {Source, CxpFile}} | Tail],
	  GlobalDir,
	  ProductsOTH,
	  SwpIds) ->
    Dir = dir(Source, GlobalDir, SwpIds),
    CxpPath = filename:join(Dir, CxpFile),
    case filelib:is_regular(CxpPath) of
 	true ->
 	    [CxpPath | cxp_files(Tail, GlobalDir, ProductsOTH, SwpIds)];
 	false ->
	    Bundles = filelib:wildcard(filename:join(Dir, "cxp*.xml")),
	    case find_bundle(Bundles, Name, Id, Version, Source) of
		{_Bundle, Products} ->
		    cxp_files(Products ++ Tail, GlobalDir, ProductsOTH, SwpIds);
		undefined ->
		    case lists:keymember({Name, Id, Version}, 1, ProductsOTH) of
			false ->
			    %% BaseBand SWP. Shall always be stored in archive.
			    Ls_CxpPath = os:cmd("ls -la " ++ CxpPath),
			    Ls_DirPath = os:cmd("ls -la " ++ Dir),
			    ErrInfo = [Name,
				       Id,
				       Version,
				       Source,
				       CxpFile,
				       Dir,
				       Bundles,
				       Ls_CxpPath,
				       Ls_DirPath],
			    erlang:error(bundle_not_found, ErrInfo);
			true ->
			    %% OTHER SWP. May have been removed from archive by
			    %% audit_software.
			    %% If not in archive -> will be downloaded later
			    %% when CAT registers a board with add_board.
			    sysInitI:info_report(
			      [{?MODULE, cxp_files},
			       {"OTHER", {Name, Id, Version}},
			       {not_found, CxpPath}]),
			    cxp_files(Tail, GlobalDir, ProductsOTH, SwpIds)
		    end
	    end
    end;
cxp_files([], _, _, _) ->
    [].

%%% ----------------------------------------------------------
make_links(GlobalArchiveDir, SwVersionDir, Products) ->
    make_links(GlobalArchiveDir,
	       SwVersionDir,
	       Products,
	       swmBoardList:read_swp_selected(swp_id)).

%%% ----------------------------------------------------------
make_links(GlobalArchiveDir, SwVersionDir, Products, SwpIds) ->
    make_links(GlobalArchiveDir, SwVersionDir, Products, SwpIds, []).

%%% ----------------------------------------------------------
make_links(GlobalArchiveDir, SwVersionDir, Products, SwpIds, ProductsOTH) ->
    SquashFsDir = swmLib:squash_fs_dir(),
    [begin
	 ArchiveDir = dir(Source, GlobalArchiveDir, SwpIds),
	 Basename = Name ++ "_" ++ Id ++ "_" ++ Version,
	 CxpDir = filename:join(SquashFsDir, Basename),
	 case filelib:is_dir(CxpDir) of
	     true ->
		 cmdreslog("cd " ++ SwVersionDir ++ " ; ln -s " ++ CxpDir);
	     false ->
		 install_bundle(Name,
				Id,
				Version,
				ArchiveDir,
				SwVersionDir,
				Source,
				SwpIds,
				ProductsOTH)
	 end
     end
     || {{Name, Id, Version}, {Source, _File}} <- Products],
    ok.

%%% ----------------------------------------------------------
dir(global, Dir, _) ->
    Dir;
dir(hal, _, SwpIds) ->
    case lists:keyfind(hal, 1, SwpIds) of
	{hal, HalSwpId} ->
	    filename:join(swmLib:software_hal_dir(), HalSwpId);
	false ->
	    swmLib:software_hal_dir()
    end.

%%% ----------------------------------------------------------
install_bundle(Name,
	       Id,
	       Version,
	       ArchiveDir,
	       SwVersionDir,
	       Source,
	       SwpIds,
	       ProductsOTH) ->
    CxpP = filename:join(ArchiveDir, "cxp*.xml"),
    Bundles = filelib:wildcard(CxpP),
    case find_bundle(Bundles, Name, Id, Version, Source) of
	{Bundle, Products} ->
	    cmd(["cp ", Bundle, " ", SwVersionDir]),
	    make_links(ArchiveDir, SwVersionDir, Products, SwpIds, ProductsOTH);
	undefined ->
	    case lists:keyfind({Name, Id, Version}, 1, ProductsOTH) of
		false ->
		    %% BaseBand SWP. Shall always be stored in archive.
		    Ls_ArchPath = os:cmd("ls -la " ++ ArchiveDir),
		    Ls_SwVerPath = os:cmd("ls -la " ++ SwVersionDir),
		    ErrInfo = [Name,
			       Id,
			       Version,
			       ArchiveDir,
			       SwVersionDir,
			       Source,
			       SwpIds,
			       ProductsOTH,
			       Bundles,
			       Ls_ArchPath,
			       Ls_SwVerPath],
		    erlang:error(bundle_not_found, ErrInfo);
		{_, {_, FileOTH}} ->
		    %% OTHER SWP. May have been removed from archive by
		    %% audit_software.
		    %% If not in archive -> will be downloaded later
		    %% when CAT registers a board with add_board.
		    sysInitI:info_report(
		      [{?MODULE, insert_bundle},
		       {"OTHER", {Name, Id, Version}},
		       {not_found, filename:join(ArchiveDir,FileOTH)}]),
		    ok
	    end
    end.

%%% ----------------------------------------------------------
find_bundle([Bundle | Tail], BundleName, BundleId, BundleVersion, Source) ->
    {ConfigurationE, []} = xmerl_scan:file(Bundle),
    ProductE = find_element(product, ConfigurationE),
    case {find_attribute(name, ProductE),
	  find_attribute(id, ProductE),
	  find_attribute(version, ProductE)} of
	{BundleName, BundleId, BundleVersion} ->
	    ContentInfoE = find_element(contentinfo, ConfigurationE),
	    Products =
		[{{find_attribute(name, ProdE),
		   find_attribute(id, ProdE),
		   find_attribute(version, ProdE)},
		  {Source, try find_attribute(filename, ProdE)
			   catch throw : {not_found, _} -> undefined
			   end}}
		 || ProdE <- ContentInfoE#xmlElement.content,
		    ProdE#xmlElement.name == product],
	    {Bundle, Products};
	_ ->
	    find_bundle(Tail, BundleName, BundleId, BundleVersion, Source)
    end;
find_bundle([], _, _, _, _) ->
    undefined.



%%% ----------------------------------------------------------
%%% @doc Clear the other home directory
%%% This presumes both home directories are available
%%% @end
%%% ----------------------------------------------------------

-spec clear() -> ok.

clear() ->
    info_msg("Clearing...~n"),
    HomeInactive = home_dir(inactive),
    case sysEnv:rcs_mode_2() of
	target ->
	    swmFallbackList:remove_bootfallback_complete(),
	    cmd(["sudo cup --clear_home ", HomeInactive]);
	    %% run_swm_wrapper(["clear_ee -h ",HomeInactive]);
	simulated ->
	    cmd(["rm -rf ", HomeInactive]);
	_ ->
	    ok
    end,
    ok.


%%% ----------------------------------------------------------
%%% @doc Remove files  from the inactive home directory that indicate a UP
%%% installation:
%%% - the UP metadata file.
%%% - the .cxp_list file
%%% This presumes both home directories are available.
%%%
%%%   Fast Restore is using the UP metadata file as an indication of an
%%%   installed UP. I.e. to remove an installation - just remove metadata!
%%% @end
%%% ----------------------------------------------------------
remove_installation_inactive() ->
    SwDirInactive = swmLib:software_dir(home_dir(inactive)),
    Pattern = filename:join(SwDirInactive, "*-up.xml"),
    case filelib:wildcard(Pattern) of
	[MetadataFile] ->
	    {0, _} = cmdres(["rm -f ", MetadataFile]),
	    swmLib:make_cxp_list(SwDirInactive, []);
	_ ->
	    ok
    end.

%%% ----------------------------------------------------------
%%% @doc Install the various part of the OS to it's correct places
%%% @end
%%% ----------------------------------------------------------
-spec preload() -> ok.

preload() ->
    preload({inactive_home, swmLib:software_dir_other()}).

%%% ----------------------------------------------------------
-spec preload(SwDir :: {DirLocation :: inactive_home | archive,
			Dir :: string()}) ->
    ok.

preload(SoftwareDir) ->
    info_msg("Preloading...~nDir: ~p~n", [SoftwareDir]),
    preload(sysEnv:rcs_mode_2(), sysEnv:board_type(), SoftwareDir),
    ok.

%%% ----------------------------------------------------------
%%% @doc Edit in boot flash to make the board boot on the other rfs partition
%%% @end
%%% ----------------------------------------------------------
-spec activate() -> ok.

activate() ->
    info_msg("Activating... ~n"),
    activate(sysEnv:rcs_mode_2(), sysEnv:board_type()),
    ok.

%%% ----------------------------------------------------------
%%% @doc Remove the rollback capacity.
%%% The new software state is now final
%%% @end
%%% ----------------------------------------------------------

-spec commit() -> ok.

commit() ->
    commit(sysEnv:rcs_mode_2(), sysEnv:board_type()).

%%% ----------------------------------------------------------
%%% @doc Switch boot pointers to boot on the other area
%%% @end
%%% ----------------------------------------------------------

rollback_at_restore() ->
    rollback_at_restore(sysEnv:rcs_mode_2(), sysEnv:board_type()).
    


%%% ----------------------------------------------------------
%%% @doc Returns the number of the active instance
%%% Instance, fetched Uboot env-space on DUS41 and from boot flash
%%% boot space on TCU03
%%% @end
%%% ----------------------------------------------------------
-spec get_active_instance() -> 0|1|2|not_supported.
get_active_instance() ->
    get_boot_instance(sysEnv:rcs_mode_2(), "configured").

%%% ----------------------------------------------------------
%%% @doc Returns the number of the fallback instance
%%% @end
%%% ----------------------------------------------------------
-spec get_fallback_instance() -> 0|1|2|not_supported.
get_fallback_instance() ->
    get_boot_instance(sysEnv:rcs_mode_2(), "fallback").

%%% ----------------------------------------------------------
%%% @doc Returns the number of the inactive instance
%%% @end
%%% ----------------------------------------------------------
-spec get_inactive_instance() -> 0|1|2|not_supported.
get_inactive_instance() ->
    case get_active_instance() of
	1 ->
	    2;
	2 ->
	    1;
	Other ->
	    Other
    end.

%%% ----------------------------------------------------------
get_boot_instance(target, Ptr) ->
    case cup_vsn("2") of
	ok -> 
	    {_, Res} = cmdreslog(["sudo cup --boot_instance_get -p " ++ Ptr]),
	    try list_to_integer(Res--"\n") of
		Value when Value == 1; Value == 2 -> Value;
		Other ->
		    warning_msg("cup --boot_instance_get ~p returned ~p~n",
				[Ptr, Other])
	    catch
		error : badarg ->
		    Emsg = "Could not convert cup boot_instance_get " ++ Ptr,
		    error_msg(Emsg ++ ": ~p~n", [Res]),
		    not_supported
	    end;
	_CupVsn -> 
	    {ok, Return} = file:read_link("/opt/rcs_ee/mounts/boot/" ++ Ptr),
	    {match, [{Start, Length}]} = re:run(Return, "b[0-9]"),
	    list_to_integer(string:substr(Return, Start+2, Length-1))
    end;
get_boot_instance(simulated, _) ->
    1;
get_boot_instance(vrcs, _) ->
    not_supported.



%%% ----------------------------------------------------------
%%% @doc Test for cup version
%%% Returns ok if the cup version is comaptible with the version given
%%% Otherwise it returns the current cp version
%%% @end
%%% ----------------------------------------------------------

-spec cup_vsn(Vsn::string()) -> ok|string().

cup_vsn(Vsn) ->
    case os:cmd("cup --version "++Vsn) of
	"" ->
	    ok;
	"3\n" ->
	    ok;
	"Current cup version is "++N -> N--"\n"
    end.

%%% ----------------------------------------------------------
%%% @doc Returns the free space and total space of the root volume
%%% Size given in bytes
%%% @end
%%% ----------------------------------------------------------

-spec disk_free() -> {FreeBytes::non_neg_integer()|infinity,
		      TotalBytes::non_neg_integer()|infinity}.

disk_free() ->
    disk_free(sysEnv:rcs_mode_2(), sysEnv:board_type()).

disk_free(target, BT) when BT == dus ; BT == duw2 ->
    erlang:error(unsupported_board_type, [target, BT]);    
disk_free(target, BT) ->
    {0, Res} = cmdreslog("sudo cup --diskfree"),
    %% {0, Res} = run_swm_wrapper_res("diskfree"),
    {match, [{Start, Length}]} = re:run(Res, "[0-9]+B"),
    {list_to_integer(string:substr(Res, Start+1, Length-1)),
     disk_size(target, BT)};
disk_free(RcsMode, BT) ->
    {infinity, disk_size(RcsMode, BT)}.


disk_size(target, BT) when BT == dus ; BT == duw2 ->
    infinity; % This alternative should not happen anyway
disk_size(target, _) ->
   case swmLib:get_ram_variable(disksize) of
	undefined ->
	    {0, Res} = cmdreslog("sudo cup --disksize"),
	    {match, [{Start, Length}]} = re:run(Res, "[0-9]+B"),
	    Size = list_to_integer(string:substr(Res, Start+1, Length-1)),
	    swmLib:set_ram_variable(disksize, Size),
	    Size;
	Size -> 
	    Size
    end;
disk_size(simulated, _) ->
    infinity;
disk_size(vrcs, _) ->
    Res = os:cmd("df /rcs"),
    error_msg("Fix disksize check: df /rcs: ~s~n",[Res]),
    infinity.





%%% ----------------------------------------------------------
%%% @doc Returns the expected size of the logical volume for a cxp
%%% @end
%%% ----------------------------------------------------------

disk_usage(Cxp) ->
    {0, Res} = cmdreslog(["sudo cup -s ",Cxp]),
    %% {0, Res} = run_swm_wrapper_res(["sudo cup -s ",Cxp]),
    Last = lists:last(string:tokens(Res, "\n")),
    {match, [{Start,Length}|_]} = re:run(Last, "[0-9]+"),
    list_to_integer(string:substr(Last, Start+1, Length)).


%%% ----------------------------------------------------------
%%% @doc BoardRestore by Reset/remove boot pointers to configured 
%%%      fallback and upgrade
%%% @end
%%% ----------------------------------------------------------
reset_boot(Type) ->
    Typestring = atom_to_list(Type),
    cmd(["sudo cup --reset_boot ", Typestring]),
    %% run_swm_wrapper(["reset_boot " ++ Typestring]),
   ok.

%%% ----------------------------------------------------------
%%% @doc Set the boot pointer to a specific instance.
%%% @end
%%% ----------------------------------------------------------
set_boot_instance(Ptr, Instance) ->
    cmd("sudo cup --boot_instance_set -p " ++ sysUtil:term_to_string(Ptr) ++
	" -e " ++ sysUtil:term_to_string(Instance)).

%%% ----------------------------------------------------------
%%% doc Install the list of cxp files that are to be mounted in the flash area
%%% end
%%% ----------------------------------------------------------

get_installed_cxps()->
    get_installed_cxps(sysEnv:rcs_mode_2()).

get_installed_cxps(simulated) ->
    {ok, Files} = file:list_dir(swmLib:squash_fs_dir()),
    Files;
get_installed_cxps(target) ->
    [filename:basename(X)||
	X<-string:tokens(cmd("sudo cup -d"), "\n")].

remove_cxp(Cxp) ->
    remove_cxp(sysEnv:rcs_mode_2(), Cxp).

remove_cxp(simulated, Cxp) ->
    cmd(["rm -rf ",filename:join(swmLib:squash_fs_dir(), Cxp)]),
    ok;

remove_cxp(target, Cxp) ->
    case is_cxp_mounted(target, Cxp) of
	false ->
	    new_remove_cxp(target, Cxp);
	true ->
%%%	    old_remove_cxp(target, Cxp)   % HV90906: Replaced by logging below
	    %% This CXP is supposed to have been removed from the list (Files)
	    %% when calling swmLib:make_cxp_list/2 ==> not mounted after next
	    %% node restart and thus possible to remove.
	    Msg = [{"CXP is mounted, cannot be removed in runtime", Cxp}],
	    sysInitI:warning_report([{?MODULE, remove_cxp}, Msg]),
	    log_SwmInternal(warning, Msg)
    end.

%% HU15047
%% Select the correct command depending on if CXP is mounted or not
is_cxp_mounted(target, Cxp) ->
    Dir = swmLib:squash_fs_dir(),
    CxpPath = filename:join([Dir, Cxp, "cxp*.xml"]),
    case filelib:wildcard(CxpPath) of
	[] -> %% We can't read from this cxp, it's not mounted
	    false;
	_ -> %% Content is available. This cxp is mounted
	    true
    end.
		    

new_remove_cxp(target, Cxp) ->
    case cmdres(["sudo cup -r ", Cxp]) of
	{0, _} ->
	    ok;
	{_, ERes} ->
	    error_msg("Cup remove failed for ~s~n~s~n~s~n",
		      [Cxp, "sudo cup -r "++Cxp, ERes]),
	    error
    end.
    

%% old_remove_cxp(target, Cxp) ->
%%     %% Old command is used if for some reason the cxp list is not used
%%     Dir = swmLib:squash_fs_dir(),
%%     UIcommand = ["sudo cup -u -i ", filename:join(Dir, Cxp)],
%%     case cmdres(UIcommand) of
%% 	{0, _} ->
%% 	    ok;
%% 	{_, Eres2} -> 
%% 	    error_msg("Cup unmount failed for ~s~n~s~n~s~n",
%% 		      [Cxp, lists:flatten(UIcommand), Eres2]),
%% 	    error
%%     end.
    


fsck() ->
    fsck("").

fsck(Args) ->
    case sysEnv:rcs_mode_2() of
	target ->
	    cmdres(["sudo cup --fsck ",Args]);
	simulated ->
	    ok;
	vrcs->
	    error_msg("Somebody called fsck~n"),
	    ok
    end.


clear_application_logs() ->
    clear_application_logs(sysEnv:rcs_mode_2()).

clear_application_logs(target) ->
    %% run_swm_wrapper("clear_applicationlogs");
    cmdres("sudo cup --clear_applicationlogs '*/*'");
clear_application_logs(simulated) ->
    cmd(["\\rm -rf ", sysEnv:rcs_dir(), "/applicationlogs/*/*"]);
clear_application_logs(vrcs) ->
    cmd(["\\rm -rf ", sysEnv:rcs_dir(), "/applicationlogs/*/*"]).


%%% ----------------------------------------------------------
%%% @doc Clear applicationtmp
%%% @end
%%% ----------------------------------------------------------

clear_application_tmp() ->
   clear_application_tmp(sysEnv:rcs_mode_2()).

clear_application_tmp(target) ->
    cmdres("sudo cup --clear_applicationtmp '*/*'");
clear_application_tmp(simulated) ->
    cmd(["\\rm -rf ", sysEnv:rcs_dir(), "tmp/applicationtmp/*/*"]);
clear_application_tmp(vrcs) ->
    cmd(["\\rm -rf ", sysEnv:rcs_dir(), "tmp/applicationtmp/*/*"]).



%%% ----------------------------------------------------------
%%% #           get_crl_ids(RcsMode)
%%% Input: RcsMode::target|simulated|vrcs
%%%        
%%% Output:
%%% Exceptions:
%%% Description: Print CRL's in given instance/area
%%%              
%%% ----------------------------------------------------------

get_crl_ids(target) ->
    Current = integer_to_list(get_active_instance()),
    Next = integer_to_list(get_inactive_instance()),
    R_Current = cmd(["sudo cup --crl-identities ", Current]),
    R_Next = cmd(["sudo cup --crl-identities ",Next]),
    "Signing certificate versions, current area:\n" ++ R_Current ++ "\n" ++
	"Signing certificate versions, secondary area:\n" ++ R_Next;
get_crl_ids(simulated) ->
    "No singning certificate versions to present";
get_crl_ids(vrcs) ->
    "No singning certificate versions to present".


%%% ----------------------------------------------------------
%%% #           check_crl(RcsMode)
%%% Input: RcsMode::target|simulated|vrcs
%%%        
%%% Output:
%%% Exceptions:
%%% Description: Check if the CRL's information in the flash needs to be updated
%%%              
%%% ----------------------------------------------------------

check_crl(target) ->
    Current = get_active_instance(),
    Cmd = ["sudo cup --check-crl-store ", integer_to_list(Current)],
    {_, Text} = cmdreslog(Cmd),
    Options = [global, {capture, all, binary}],
    case re:run(Text, "Update require", Options) of
	{match,_} ->
	    update_req;
	_ ->
	    update_not_req
    end;
    %% case cmd(["sudo cup --check-crl-store ",
    %% 	      integer_to_list(Current)]) of
    %% 	"Update not required\n" ->
    %% 	    update_not_req;
    %% 	"Update required\n" ->
    %% 	    update_req;
    %% 	_Else ->
    %% 	    %% Unsupported HW
    %% 	    update_not_req
    %% end;
check_crl(_) ->
    update_not_req.

%%% ----------------------------------------------------------
%%% #           store_crl(RcsMode)
%%% Input: RcsMode::target|simulated
%%%        
%%% Output:
%%% Exceptions:
%%% Description: Update the CRL's information in flash based on CRL's 
%%%              in given instance
%%%              
%%% ----------------------------------------------------------

store_crl(target)->
    Current = get_active_instance(),
    cmd(["sudo cup --update-crl-store ",integer_to_list(Current)]),
    ok;
store_crl(_) ->
    ok.


%%% ----------------------------------------------------------
%%% #           mount_sda1
%%% Input: 
%%%        
%%% Output:
%%% Exceptions:
%%% Description: Mount the sda1 area where HAL-SWP is stored
%%% ----------------------------------------------------------

mount_sda1() ->
    mount_sda1(swmLib:sda1_dir()).

mount_sda1(MntDir) ->
    mount_sda1(sysEnv:rcs_mode_2(), MntDir).

mount_sda1(target, MntDir) ->
    Cmd = "sudo cup --mount_sda1 " ++ MntDir,
    case swmOs:cmdres(Cmd) of
	{0, _} ->
	    ok;
	{1, _} = Result ->
	    AlreadyMnt = "/dev/sda1 on " ++ MntDir,
	    case string:str(os:cmd("mount"), AlreadyMnt) of
		0 ->
		    error_msg("mount_sda1: ~s~nReturned ~p~n",[Cmd, Result]);
		_ ->
		    info_msg("mount_sda1: sda1 already mounted~n",[])
	    end,
	    Result;
	Result ->
	    error_msg("do_mount_sda1:~s~n~s~n",[Cmd, Result]),
	    Result
    end;
mount_sda1(_, _) ->
    ok.


%%% ----------------------------------------------------------
umount_sda1() ->
    umount_sda1(swmLib:sda1_dir()).

umount_sda1(MntDir) ->
    umount_sda1(sysEnv:rcs_mode_2(), MntDir).

umount_sda1(target, MntDir) ->
    Cmd = "sudo cup --umount '" ++ MntDir ++ "'",
    case swmOs:cmdres(Cmd) of
	{0, _} ->
	    ok;
	Result ->
	    Result
    end;
umount_sda1(_, _) ->
    ok.


remount_tmpfs_size_cmd(Size, Path) ->
    Cmd = "sudo cup --mount '-o remount,size=" ++
	Size ++ " tmpfs " ++ Path ++"'",
    cmdres(Cmd).



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

prepare_nl_boot() ->
    %% {_, Res} = run_swm_wrapper_res(["prepare_nl_upgrade"]),
    {_, Res} = cmdres(["sudo cup --prepare_nl_upgrade"]),
    Res.
  


write_nl_boot(OsCxp) ->
    Cmd = ["sudo cup --nl-boot-install ", " -p ", OsCxp],
    case cmdres(Cmd) of
	{0, _} ->
	    info_msg("Installed nl from ~s~n",[OsCxp]),
	    ok;
	{1, ERes} ->
	    error_msg("~s~n~s~n",[lists:flatten(Cmd), ERes]),
	    erlang:error(cup_internal_error, [target, OsCxp]);
	{2, ERes} ->
	    error_msg("~s~n~s~n~s~nsize: ~w~n",[lists:flatten(Cmd), ERes,
						os:cmd(["md5sum ", OsCxp]),
						filelib:file_size(OsCxp)]),
	    throw({fail, "Preload failed due to corrupt or unsigned software file"})
   end.

%%% ----------------------------------------------------------
%%% #           preload(RcsMode, BoardType)
%%% Input: RcsMode::target|simulated
%%%        BoardType::dus|duw2|tcu03|tcu04|dus52|dus32
%%% Output:
%%% Exceptions:
%%% Description: The preload function will install the new root file system
%%%              on the disk. This is not the final solution as the root file
%%%              system should also be a SquashFs file with no write
%%%              permissions.
%%%
%%%              For the moment this function also contains various tricks
%%%              made to the root file system which is not possible to
%%%              reproduce in a read only environment. These fixes must be
%%%              replaced by proper EE commands or removed entirely.
%%%
%%%              No OS switch occurs in simulated
%%% ----------------------------------------------------------

preload(target, BT, _) when BT == dus ; BT == duw2 ->
    erlang:error(unsupported_board_type, BT);
preload(target, BT, SoftwareDir) ->
    movePatches(home_dir(inactive)),

    %% dbg:tracer(),
    %% dbg:p(self(), c),
    %% [dbg:tpl(swmOs, X, x)||X<-[find_os_cxp, 
    %% 			       find_os_cxp_archive_bundle,
    %% 			       find_os_cxp_file,
    %% 			       find_EE_in_bundle,
    %% 			       find_in_bundle]],
			       
    OsCxp = find_os_cxp(SoftwareDir), % HV30481
    info_msg("Installing ~s~n",[OsCxp]),
    
    Next = integer_to_list(get_inactive_instance()),
    
    Cmd = ["sudo cup -k ", Next, " -p ", OsCxp],
    case cmdres(Cmd) of
	{0, _} ->
	    info_msg("Installed ~s~n",[OsCxp]);
	{1, ERes} ->
	    error_msg("~s~n~s~n",[lists:flatten(Cmd), ERes]),
	    erlang:error(cup_internal_error, [target, BT]);
	{2, ERes} ->
	    error_msg("~s~n~s~n~s~nsize: ~w~n",[lists:flatten(Cmd), ERes, 
						os:cmd(["md5sum ", OsCxp]),
						filelib:file_size(OsCxp)]),
	    throw({fail, "Preload failed due to corrupt or unsigned software file"})
    end,
    prepare_nl_boot(),
    %% Copy old nl boot files to a temporary dir. 
    %% Old files and new files will be swapped/deleted at confirm
    write_nl_boot(OsCxp);
preload(simulated, _, _) ->
    %% Don't forget changing rollback_at_restore

    %% No need to install a new OS in simulated environment, but we need
    %% to setup the switcher command.
    Home = filename:join(sysEnv:rcs_root(), "home"),
    Home2 = filename:join(sysEnv:rcs_root(), "home2"),
    Tmp = filename:join(sysEnv:rcs_root(), "hometmp"),

    movePatches(filename:join(Home2, os:getenv("USER"))),

    RestartHelperPid =
	case os:getenv("RESTART_HELPER_PID") of
	    false ->
		"RestartHelperPidUnknown,see:start_rcs.sh";
	    Value ->
		Value
	end,

    filelib:ensure_dir(switch_over_path()),
    {ok, Fd} = file:open(switch_over_path(), [write]),
    io:format(Fd, "/bin/date >> date.log ~n",  []),
    io:format(Fd, "mv ~s ~s~n",[Home, Tmp]),
    io:format(Fd, "mv ~s ~s~n",[Home2, Home]),
    io:format(Fd, "mv ~s ~s~n",[Tmp, Home2]),
    io:format(Fd, "/bin/kill -s USR1 ~s~n", [RestartHelperPid]),    
    ok = file:close(Fd),
    cmd(["chmod a+x ",switch_over_path()]);
preload(vrcs, _, _) ->
    ok.


%%% ----------------------------------------------------------
%% Fix for HV30481
find_os_cxp({inactive_home, SoftwareDir}) ->
%    info_msg("Searching for os in hal / inactive home ~p~n", [SoftwareDir]),
    Pattern = filename:join([SoftwareDir, "*", "cxp*.xml"]),
    CxpFiles = filelib:wildcard(Pattern),
%    info_msg("CxpFiles = ~p~n",[CxpFiles]),
    find_os_cxp(CxpFiles, SoftwareDir);
find_os_cxp({archive, SoftwareDir}) ->
    info_msg("Searching for os in hal / archive ~p~n", [SoftwareDir]),
    {ok, [{products, Products},
	  {swp_id, SwpIds}]} =
	swmBoardList:swp([products, swp_id],
			 [{boardTypeBB, swmBoardList:boardType()},
			  {options, [{global, SoftwareDir}]}]),
    case find_os_product(Products) of
	{_, {hal, OsFileName}} ->
	    %% No bundles in hal :-)
	    [HalSwpId] = [SwpId || {hal, SwpId} <- SwpIds],
	    filename:join([swmLib:software_hal_dir(), HalSwpId, OsFileName]);
	_ ->   % Not found, or global
	    LmcMetaFiles =
		filelib:wildcard(filename:join([SoftwareDir, "cxp*.xml"])),
	    try
		%% Legacy: RCSEE is delivered in a bundle.
		find_os_cxp_archive_bundle(LmcMetaFiles, SoftwareDir)
	    catch
		error : no_cxp_found ->
		    %% New delivery structure:
		    %% RCSEE is delivered as a stand-alone LMC.
		    LmcFiles =
			filelib:wildcard(filename:join([SoftwareDir, "*.cxp"])),
		    LmcFileNames =
			[filename:basename(LmcFile) || LmcFile <- LmcFiles],
		    [OsLmcFileName] =
			[LmcFileName || LmcFileName <- LmcFileNames,
					is_LmcFileName_os(LmcFileName)],
		    info_msg("os found: ~p~n", [OsLmcFileName]),
		    filename:join(SoftwareDir, OsLmcFileName)
	    end
    end.

%%% ----------------------------------------------------------
find_os_product([{_, {_, FileName}} = Product | Tail]) ->
    case is_LmcFileName_os(FileName) of
	false ->
	    find_os_product(Tail);
	true ->
	    Product
    end;
find_os_product([]) ->
    false.

%%% ----------------------------------------------------------
find_os_cxp([CxpFile|CxpFiles], SoftwareDir) ->
    {ConfigurationE, _} = xmerl_scan:file(CxpFile),
    try find_element(os, ConfigurationE) of
	_ ->
	    ProductE = find_element(product, ConfigurationE),
	    Name = find_attribute(name, ProductE),
	    Id = find_attribute(id, ProductE),
	    Version = find_attribute(version, ProductE),
	    info_msg("os found: ~p~n",[{Name,Id,Version}]),
	    find_os_cxp_file(Name, Id, Version, SoftwareDir)
    catch _:_ ->
	    find_os_cxp(CxpFiles, SoftwareDir)
    end;
find_os_cxp([], _) ->
    erlang:error(no_cxp_found, []).

%%% ----------------------------------------------------------
is_LmcFileName_os("RCSEE" ++ _) ->
    true;
is_LmcFileName_os("BRCS-EE" ++ _) ->
    %% Deprecated. BRCS (node type BPU) will be terminated.
    true;
is_LmcFileName_os(_) ->
    false.

%%% ----------------------------------------------------------
find_os_cxp_archive_bundle([CxpFile | Tail], SoftwareDir) ->
    try
	{ok, Filename} = find_EE_in_bundle(CxpFile),
	filename:join(SoftwareDir, Filename)
    catch
	_ : _ ->
	    %% Not RCSEE...
	    find_os_cxp_archive_bundle(Tail, SoftwareDir)
    end;
find_os_cxp_archive_bundle([], _) ->
    erlang:error(no_cxp_found).

%%% ----------------------------------------------------------
find_os_cxp_file(Name, Id, Version, SoftwareDir) ->    
    {ok, [{products, Products},
	  {swp_id, SwpIds}]} =
	swmBoardList:swp([products, swp_id],
			 [{boardTypeBB, swmBoardList:boardType()},
			  {boardTypesOTH, swmBoardList:boardTypes_oth()},
			  {options, [{global, SoftwareDir}]}]),
    {Source, File} = 
	case lists:keysearch({Name, Id, Version}, 1, Products) of
	    {value, {_, SourceAndFilename}} ->
		SourceAndFilename;
	    false ->
		%% Look for bundle
		Pattern = filename:join(SoftwareDir, "cxp*.xml"),
		Bundles = filelib:wildcard(Pattern),
		try
		    %% Always global - No bundles in hal :-)
		    {global, find_in_bundle(Name, Id, Version, Bundles)}
		catch
		    throw : {not_found, filename} ->
			{global, make_cxpfilename(Name, Id)}
		end
	end,
    Pattern2 = filename:join(SoftwareDir, "*-up.xml"),
    [UpFile] = filelib:wildcard(Pattern2),
    {ConfigurationE, _} = xmerl_scan:file(UpFile),
    ProductE = find_element(product, ConfigurationE),
    UpName = find_attribute(name, ProductE),
    UpId = find_attribute(id, ProductE),
    UpVersion = find_attribute(version, ProductE),
    Path =
	case Source of
	    global ->
		swmServer:get_up_archive_dir(UpName, UpId, UpVersion);
	    hal ->
		[HalSwpId] = [SwpId || {hal, SwpId} <- SwpIds],
		filename:join(swmLib:software_hal_dir(), HalSwpId)
	end,
    filename:join(Path, File).

%%% ----------------------------------------------------------
make_cxpfilename("RCSEE" ++ _ = Name, Id) ->
    %% HV33559: For backup restore from 17A to 16B and
    %%          Bootfallback from 16B to 17A.
    %% The filename of RCSEE CXP is not specified in the 16B -up.xml.
    Name ++ "_" ++ Id ++ ".cxp".
    
%%% ----------------------------------------------------------
find_in_bundle(Name, Id, Version, [Bundle|Bundles]) ->
    {ConfigurationE, _} = xmerl_scan:file(Bundle),
    ContentInfoE = find_element(contentinfo, ConfigurationE),
    case do_find_in_bundle(Name,Id,Version, ContentInfoE#xmlElement.content) of
	{ok, Filename} ->
	    Filename;
	{error, not_found} ->
	    find_in_bundle(Name, Id, Version, Bundles)
    end;
find_in_bundle(Name, Id, Version, []) ->
    erlang:error(cxp_file_not_located, [Name, Id, Version,[]]).
    

%%% ----------------------------------------------------------
do_find_in_bundle(Name, Id, Version, [ProductE|Content]) 
  when ProductE#xmlElement.name == product ->
    case find_attribute(id, ProductE) of
	Id ->
	    {ok, find_attribute(filename, ProductE)};
	_ ->
	    do_find_in_bundle(Name, Id, Version, Content)
    end;
do_find_in_bundle(Name, Id, Version, [_|Content]) ->
    do_find_in_bundle(Name, Id, Version, Content);
do_find_in_bundle(_, _, _, _) ->
    {error, not_found}.

%%% ----------------------------------------------------------
find_EE_in_bundle(CxpFile) ->
    {ConfigurationE, _} = xmerl_scan:file(CxpFile),
    try find_element(bundle, ConfigurationE) of
	_ ->
	    ContentInfoE = find_element(contentinfo, ConfigurationE),
	    find_EE(ContentInfoE#xmlElement.content)
    catch
	_ : _ ->
	    {error, not_bundle}
    end.

find_EE([ProductE | Tail]) when ProductE#xmlElement.name == product ->
    case find_attribute(name, ProductE) of
	"RCSEE" ++ _ = Name ->
	    Id = find_attribute(id, ProductE),
	    Version = find_attribute(version, ProductE),
	    info_msg("os (EE) found: ~p~n", [{Name, Id, Version}]),
	    {ok, find_attribute(filename, ProductE)};
	_ ->
	    find_EE(Tail)
    end;
find_EE([_ | Tail]) ->
    find_EE(Tail);
find_EE(_) ->
    {error, ee_not_found}.



%%% ----------------------------------------------------------
movePatches(HomeOther) ->
    UgPatches = filename:join(swmLib:swm_dir(), "ug_patches"),
    DevPatches = filename:join(HomeOther, "dev_patches"),
    {0, _} = cmdreslog(["mkdir -p ", DevPatches]),
    Res = os:cmd(["if [[ ! -d ",
		  UgPatches,
		  " ]]; then echo no ",
		  UgPatches,
		  " directory; elif [[ -z `ls -1 ",
		  UgPatches,
		  "` ]]; then echo no patches in ",
		  UgPatches,
		  "; else cp -v -R ",
		  UgPatches,
		  "/* ",
		  DevPatches,
		  "; fi"]),
    info_msg("Moving patches: ~n~s~n",[Res]),
    ok.



%%% ----------------------------------------------------------
%%% #           switch_over_path()
%%% Input:
%%% Output: string()
%%% Exceptions:
%%% Description: The path to the switch_over script used in the simulator
%%%              to change directory structure after erlang has stopped.
%%%              Similar functionality is not needed on target since we
%%%              manipulate the boot flash memory instead
%%% ----------------------------------------------------------

switch_over_path() ->
    filename:join(swmLib:swm_dir(), "switch_over").

%%% ----------------------------------------------------------
%%% #           activate(RcsMode, BoardType)
%%% Input: RcsMode::target|simulated
%%%        BoardType::dus|duw2|tcu03|dus52
%%% Output:
%%% Exceptions:
%%% Description: Makes the necessary changes in the boot flash to make
%%%              the board boot with the other kernel and root file system
%%%              the next type.
%%%
%%%              NOTE! For the moment the change is permanent, so we need
%%%              a temporary fallback fix that can kick in if the new software
%%%              fails and change the boot flash to it's old settings.
%%%              This is made by creating a small script that is installed
%%%              as fallback when the new software is started, so if we reach
%%%              so far as to the erlang start, we can rely on this fix.
%%%              However, we cannot handle startup failures that occur any
%%%              sooner, so they will lead to cyclic restarts.
%%% ----------------------------------------------------------

activate(target, BT) when BT == dus ; BT == duw2 ->
    erlang:error(unsupported_board_type, BT);
activate(target, _) ->
    HomeInactive = home_dir(inactive),
    swmLib:make_cxp_list(HomeInactive),

    Current = get_active_instance(),
    Next = integer_to_list(get_inactive_instance()),
    make_fallback_script(Current), % HS32665

    %% Cause the uboot to reboot once on the new configuration
    %% and then rollback
    activate_os(Next),
    %% run_swm_wrapper("activate_os -u true -i "++Next),
    ok = sysRhai:setbootptr(upgrade);
activate(simulated,_) ->
    SwoPath = switch_over_path(),
    %% No temporary fallback fix for sim. It's faster to simply reinstall
    info_msg("Activating ~p~n",[SwoPath]),
    ok = heart:set_cmd(SwoPath);
activate(vrcs, _) ->
    ok.


activate_os(Index) ->
    cmd(["sudo cup --activate_os ", Index]).
    



extract_cxp_data(CxpFile) ->
    TmpDir = os:cmd("mktemp -d /tmp/swm.XXXXXX")--"\n",
    TarCmd =
	["cd ", TmpDir, ";"
	 "tar --wildcards --no-wildcards-match-slash -xf ", CxpFile, " cxp*.xml"],
    case cmdreslog(TarCmd) of
	{0, _} ->
	    ok;
	{Code, _} ->
	    FileInfo = 
		case file:read_file_info(CxpFile) of
		    {ok, FI} ->
			sysUtil:record_format(record_info(fields, file_info),
					      FI);
		    {error, Reason} ->
			[{error, sysUtil:reason_phrase(Reason)}]
		end,
	    error_msg("Faulty file:~s~nmd5sum: ~s~n~p~n",
		      [CxpFile, os:cmd(["md5sum ", CxpFile]), FileInfo]),
	    erlang:error({exit_code,Code}, [CxpFile])
    end,
    try
	read_cxp_file(TmpDir)
    after
	os:cmd(["rm -rf ", TmpDir])
    end.
    


%%% ----------------------------------------------------------
%%% #           commit(RcsMode, BoardType)
%%% Input: RcsMode::target|simulated
%%%        BoardType::tcu03|dus52|dus32
%%% Output:
%%% Exceptions:
%%% Description: Makes the new software permanent
%%%              The proper way is to commit the change to the boot flash
%%%              but that function is not in place. We rely on the
%%%              heart_fallback solution instead.
%%% ----------------------------------------------------------

commit(target, BT) when BT == dus ; BT == duw2 ->
    erlang:error(unsupported_board_type, [target, BT]);
commit(target, BT) when BT==dus52 ; BT==dus32 ->
    info_msg("Reverting heart command~n"),
    Heart = os:getenv("HEART_COMMAND"),
    ok = heart:set_cmd(Heart),
    HeartPath = filename:join(sysEnv:home_dir(), "heart_fallback"),
    file:rename(HeartPath, HeartPath++".old"),
    Next = integer_to_list(get_inactive_instance()),
    cmd(["sudo cup --confirm_os " ,Next]),
    %% run_swm_wrapper("confirm_os -i "++Next),
    cmd("ls -lR /opt/rcs_ee/mounts/boot"),
    ok = sysRhai:setbootptr(configured),
    ok = sysRhai:setbootcounter(0);
commit(target, _) ->
    info_msg("Reverting heart command~n"),
    Heart = os:getenv("HEART_COMMAND"),
    ok = heart:set_cmd(Heart),
    HeartPath = filename:join(sysEnv:home_dir(), "heart_fallback"),
    file:rename(HeartPath, HeartPath++".old"),
    Next = integer_to_list(get_inactive_instance()),
    cmd(["sudo cup --confirm_os ", Next]),
    %% run_swm_wrapper("confirm_os -i "++Next),
    ok = sysRhai:setbootptr(configured),
    ok = sysRhai:setbootcounter(0);
commit(vrcs, _) ->
    ok;
commit(simulated, _) ->
    ok.


%%% ----------------------------------------------------------
%%% #           rollback_at_restore(RcsMode, BoardType)
%%% Input: RcsMode::target|simulated
%%%        BoardType::tcu03|dus52|dus32
%%% Output:
%%% Exceptions:
%%% Description: Makes the old software permanent
%%% ----------------------------------------------------------

rollback_at_restore(target, _) ->
    Next = integer_to_list(get_inactive_instance()),
    cmd(["sudo cup --confirm_os ", Next]),
    %% This is for verification only
    %% Let it fail if themount doesn't exist
    cmd("ls -lR /opt/rcs_ee/mounts/boot"),
    ok;
rollback_at_restore(simulated, _) ->
    %% From preload
    Home = filename:join(sysEnv:rcs_root(), "home"),
    Home2 = filename:join(sysEnv:rcs_root(), "home2"),
    Tmp = filename:join(sysEnv:rcs_root(), "hometmp"),

    movePatches(filename:join(Home2, os:getenv("USER"))),

    RestartHelperPid =
	case os:getenv("RESTART_HELPER_PID") of
	    false ->
		"RestartHelperPidUnknown,see:start_rcs.sh";
	    Value ->
		Value
	end,
    SwoPath = switch_over_path(),
    filelib:ensure_dir(SwoPath),
    {ok, Fd} = file:open(SwoPath, [write]),
    io:format(Fd, "/bin/date >> date.log ~n",  []),
    io:format(Fd, "mv ~s ~s~n",[Home, Tmp]),
    io:format(Fd, "mv ~s ~s~n",[Home2, Home]),
    io:format(Fd, "mv ~s ~s~n",[Tmp, Home2]),
    io:format(Fd, "/bin/kill -s USR1 ~s~n", [RestartHelperPid]),
    ok = file:close(Fd),
    cmd(["chmod a+x ", SwoPath]),

    %% From activate
    info_msg("Activting ~p~n",[SwoPath]),
    ok = heart:set_cmd(SwoPath);
rollback_at_restore(vrcs, _) ->
    error_msg("No defined action for vrcs at rollback_at_restore~n"),
    ok.


		    

%%% ----------------------------------------------------------
%%% #           mount_software(RcsMode, SwVersionDir, CxpFiles)
%%% Input: RcsMode::target|simulated
%%%        SwVersionDir::string()
%%%        CxpFiles::[string()]
%%% Output:
%%% Exceptions:
%%% Description: For each given CxpFile a new logical volume is created
%%%              and populated with the contents of the SquashFS file in
%%               the load module container. The mount point is created
%%%              in /software/Name_ProdNr_Rstate
%%%
%%%              In the simulator the SquashFs file is unpacked as real
%%%              files located below /software
%%% ----------------------------------------------------------

mount_software(target, CxpFiles) ->
    info_msg("mount_software/2, n. of CXP files: ~w~n", [length(CxpFiles)]),
    Backlog = lists:foldl(
		fun mount_cxp_deferring/2,
		#mountBacklog{nofCxpsLeft=length(CxpFiles)},
		CxpFiles),
    if
	Backlog#mountBacklog.isOsMounted =:= false ->
	    info_msg("no 'os' CXP among mounted CXPs~n", []);
	true ->
	    ok
    end,
    Deferred = Backlog#mountBacklog.deferred,
    if
	Deferred =/= [] ->
	    % this is a "cannot happen" case really
	    erlang:error({unhandled_cxps, Deferred}, [CxpFiles]);
	true ->
	    ok
    end;

mount_software(simulated, CxpFiles) ->
    info_msg("mount_software/2, n. of CXP files: ~w~n", [length(CxpFiles)]),
    [mount_cxp(simulated, CxpFile)||CxpFile<-CxpFiles];

mount_software(vrcs, _) ->
    %% Not for VRCS
    ok.

mount_cxp(simulated, CxpFile) ->
    SwVersionDir = swmLib:software_dir_other(),
    cmd(["mkdir -p ", SwVersionDir]),
    SquashFsDir = swmLib:squash_fs_dir(),
    info_msg("Unpacking ~s~n",[CxpFile]),
    unpack_cxp(CxpFile, SquashFsDir, SwVersionDir),
    info_msg("Unpacked ~s~n",[CxpFile]),
    ok.


install_software(target, Products, Env) ->
    %% When also mounting it is important that EE is mounted first, because
    %% the software signing certificates are stored there. But that seems not
    %% to be that important in this case, because we're only creating the 
    %% logical volumes. So do a straight forward install.

    InstalledCxps = get_installed_cxps(target),
    NewEnv = Env#{installedCxps => InstalledCxps,
		  isOsMounted => false},
    case is_filename_ok(Products) of
	true -> 
	    do_install_software(Products, NewEnv);
	false -> 
	    %% This is an older UP. Install all CXPs
	    ArchiveDir = maps:get(archiveDir, Env),
	    CxpFiles = filelib:wildcard(filename:join(ArchiveDir, "*.cxp")),
	    NewProducts = 
		[begin
		     {_, _, ProductData} = extract_cxp_data(CxpFile),
		     #'ProductData'{productName = Name, 
				    productNumber = Id,
				    productRevision = Version} = ProductData,
		     {{Name, no_slash(Id), Version}, 
		      {global, filename:basename(CxpFile)}}
		 end||
		    CxpFile<-CxpFiles],
	    do_install_software(NewProducts, NewEnv)
    end,
    Finished = get_installed_cxps(target),
    info_msg("Products: ~w Installed: ~w~n",
	     [length(Products), length(Finished)]),
    ok;
install_software(simulated, Products, Env) ->
    case is_filename_ok(Products) of
	true ->
	    [mount_cxp(simulated, CxpFile)||{_, {_, CxpFile}}<-Products];
	false ->
	    ArchiveDir = maps:get(archiveDir, Env),
	    CxpFiles = filelib:wildcard(filename:join(ArchiveDir, "*.cxp")),
	    [mount_cxp(simulated, CxpFile)||CxpFile<-CxpFiles]
    end;
install_software(vrcs, _, _) ->
    %% Not for VRCS
    ok.

%%% Description: Return 'false' if a CXP has a missing filename

is_filename_ok([{{Name, Id, Vsn},{Source,undefined}}|_]) ->
    warning_msg("Product ~s ~s ~s (~p) does not have an associated filename~n"
		"Assuming all CXPs should be installed~n",
		[Name, Id, Vsn, Source]),
    false;
is_filename_ok([_|T]) ->
    is_filename_ok(T);
is_filename_ok([]) -> 
    true.


do_install_software([{{Name, Id, Version}=Prod, {Source, CxpFile}}|T], Env) ->
    Args = [[{{Name, Id, Version}, {Source, CxpFile}}|T], Env],
    #{archiveDir := ArchiveDir,
      swpIds := SwpIds,
      isOsMounted := IsOsMounted} = Env,
    
    %% Setup link from /home/sirpa/software
    Basename = Name ++ "_" ++ no_slash(Id) ++ "_" ++Version,
    SquashFsDir = swmLib:squash_fs_dir(),
    SwVersionDir = swmLib:software_dir_other(),
    case file:read_link(filename:join(SwVersionDir, Basename)) of
	{ok, _} -> ok;
	{error, enoent} -> 
	    CxpDir = filename:join(SquashFsDir, Basename),
	    cmdreslog(["cd ", SwVersionDir, " ; ln -s ", CxpDir])
    end,

    Dir = dir(Source, ArchiveDir, SwpIds),
    CxpPath = filename:join(Dir, CxpFile),
    {IsMissing, IsOs} = 
	case filelib:is_regular(CxpPath) of
	    true ->
		{_, ConfigurationE, _} = extract_cxp_data(CxpPath),
		{false, isCxpMarked(os, ConfigurationE)};
	    false ->
		%% If the CXP is missing it cannot be the 'os' cxp
		{true, false}
	end,
    
    if IsOs and IsOsMounted ->
	    erlang:error({multiple_os_cxp, CxpPath}, Args);
       IsOs ->
	    %% Always do OS CXP even if LV exists
	    install_cxp(CxpPath, maps:get(installMode, Env)),
	    do_install_software(T, Env#{isOsMounted := true});
       IsMissing ->
	    %% Find bundle metadata or check if the CXP was deleted by HSI
	    Bundles = filelib:wildcard(filename:join(Dir, "cxp*.xml")),
	    case find_bundle(Bundles, Name, Id, Version, Source) of
		{BundleFile, Products} ->
		    cmdreslog(["cp ",BundleFile, " ", SwVersionDir, " ;"
			       "rm ",SwVersionDir,"/",Basename]),
		    do_install_software(Products++T, Env);
		undefined ->
		    case is_other_sw(Prod, Env) of
			false ->
			    erlang:error(bundle_not_found, Args);
			true ->
			    do_install_software(T, Env)
		    end
	    end;
       IsOsMounted ->
	    %% Install normal CXP
	    install_normal(Prod, CxpPath, Env),
	    do_install_software(T, Env);
       true ->
	    %% Defer installation until 'os' CXP has been installed
	    do_install_software(T, Env),
	    install_normal(Prod, CxpPath, Env)
    end;
do_install_software([], _) ->
    ok.


install_normal(Prod, CxpPath, Env) ->
    %% If we are here, then install mode definately exists, because
    %% legacy mode is default
    case maps:get(installMode, Env) of
	no_mount -> 
	    case is_lv(Prod, maps:get(installedCxps, Env)) of
		true -> %% The LV already exists. Don't do anything
		    ok;
		false -> %% Install the LV
		    install_cxp(CxpPath, no_mount)
	    end;
	mount ->
	    case is_mounted(Prod) of
		true ->
		    ok;
		false ->
		    install_cxp(CxpPath, mount)
	    end
    end.

is_lv({Name, Id, Vsn}, InstalledCxps) ->
    Basename = Name++"_"++no_slash(Id)++"_"++Vsn,
    lists:member(Basename, InstalledCxps).

is_mounted({Name, Id, Vsn}) ->
    Basename = Name++"_"++no_slash(Id)++"_"++Vsn,
    Pattern = filename:join([swmLib:squash_fs_dir(), Basename, "cxp*.xml"]),
    case filelib:wildcard(Pattern) of
	[_] ->
	    true;
	[] ->
	    false
    end.


install_cxp(CxpPath, MountFlag) ->
    Instance = integer_to_list(get_inactive_instance()),
    info_msg("Installing ~s~n",[CxpPath]),
    {Free, _} = disk_free(),
    Usage = disk_usage(CxpPath),
    if
	Usage > Free ->
	   throw({fail, "Mount failed due to disk space shortage"});
       true ->
	   ok
    end,
    Cmd = case MountFlag of
	      no_mount ->
		  ["sudo cup --cxp-install-and-mount --no-mount --cxp-path ",
		   CxpPath, " --instance ", Instance];
	      mount ->
		  ["sudo cup --cxp-install-and-mount --cxp-path ",
		   CxpPath, " --instance ", Instance]
	  end,
		  
    case timer:tc(fun() -> cmdreslog(Cmd) end) of
	{Time, {0, _}} ->
	    TimeStr = erlang:float_to_list(Time/1000000, [{decimals, 6}]),
	    Msg = "Mount execution time: "++TimeStr,
	    log_SwmInternal(info, Msg),
	    ok;
	{_, {1, ERes}} ->
	    error_msg("~s~n~s~n",[Cmd, ERes]),
	    erlang:error(cup_internal_error, [CxpPath]);
	{_, {2, ERes}} ->
	    error_msg("~s~n~s~n~s~nsize: ~w~n",[Cmd, ERes, 
						os:cmd(["md5sum ", CxpPath]),
						filelib:file_size(CxpPath)]),
	    Basename = filename:basename(CxpPath),
	    Fail = "Mount failed due to corrupt or unsigned file "++Basename,
	    throw({fail, Fail})
    end.
    

%%% Description: Retursn 'true' if the CXP belongs to the hwCategory OTHER

is_other_sw({Name, Id, Version}, Env) ->
    ProductsOTH = maps:get(productsOTH, Env),
    lists:keymember({Name, Id, Version}, 1, ProductsOTH).



%%% ----------------------------------------------------------
%%% @doc Mount the given CxpFile, or put it on the backlog for
%%% mounting later. Mounting immediately is chosen if the
%%% CxpFile has the 'os' marker, or if the backlog indicates
%%% that the 'os' CxpFile has been mounted already. The purpose
%%% is to provide a way to ensure that the 'os' CxpFile is the
%%% first one to be mounted.
%%%
%%% In the simulated environment the 'os' marker and the backlog
%%% is ignored and the given CxpFile is mounted promptly.
%%% @end
%%% ----------------------------------------------------------

-spec mount_cxp_deferring(string(), #mountBacklog{}) -> #mountBacklog{}.

mount_cxp_deferring(CxpFile,
		    #mountBacklog{bootInstance = BootInstance} = Backlog) ->
    case sysEnv:rcs_mode_2() of
	simulated ->
	    mount_cxp(simulated, CxpFile),
	    Backlog;
	target ->
	    {_, ConfigurationE, ProductData} = extract_cxp_data(CxpFile),
	    #'ProductData'{productName = Name,
			   productNumber = Id,
			   productRevision = Version} = ProductData,
	    IsBundle = isCxpMarked(bundle, ConfigurationE),
	    IsOs = isCxpMarked(os, ConfigurationE),
	    Deferred = Backlog#mountBacklog.deferred,


	    CxpsLeft = Backlog#mountBacklog.nofCxpsLeft,

	    Msg = lists:flatten(
		    io_lib:format("Defer mount ~s ~s ~s ~w isOs ~w isBundle ~w",
				  [Name, Id, Version, length(Deferred), 
				   IsOs, IsBundle])),
	    log_SwmInternal(info, Msg),
	    %% sysInitI:info_report([{swmOs, mount_cxp_deferring},
	    %% 			      {name, Name},
	    %% 			      {prod, Id},
	    %% 			      {version, Version},
	    %% 			      {isOs, IsOs},
	    %% 			      {isBundle, IsBundle},
	    %% 			      {nofDeferred, length(Deferred)}
	    %% 			      ]),

	    if
		IsBundle ->
		    % Expand and recurse over internal CXPs
		    TmpFile = filename:join("/tmp", filename:basename(CxpFile)),
		    {0, _} = cmdres(["mv -f ", CxpFile, " ", TmpFile]),
		    ArchiveDir = filename:dirname(CxpFile),
		    Res = cmd(["cd ", ArchiveDir, "; tar -xvzf ", TmpFile]),
		    cmd(["rm -f ", TmpFile]),
		    InternalCxps =
			[filename:join(ArchiveDir, X)
			   || X <- string:tokens(Res, "\n"),
			      filename:extension(X)==".cxp"],
		    lists:foldl(
		      fun mount_cxp_deferring/2,
		      Backlog#mountBacklog{nofCxpsLeft=CxpsLeft+length(InternalCxps)-1},
		      InternalCxps);
		IsOs andalso Backlog#mountBacklog.isOsMounted ->
		    erlang:error({multiple_os_cxp, CxpFile}, [CxpFile]);
		IsOs ->
		    mount_cxp_single(CxpFile, Name, Id, Version, BootInstance),
		    info_msg("Mounting ~p as os cxp~n",[CxpFile]),
		    swmLib:set_variable(os_cxp, CxpFile),
		    % Deferred CXPs can be mounted now
		    [mount_cxp_single(F, N, I, V, BootInstance)
		     || {F, N, I, V} <- Deferred],
		    Backlog#mountBacklog{isOsMounted=true,
					 deferred=[],
					 nofCxpsLeft=CxpsLeft-1};
		Backlog#mountBacklog.nofCxpsLeft =:= 1 ->
		    % this is the very last CXP; just mount the deferred CXPs now
		    % and finally this last one even though no 'os' seen
		    [mount_cxp_single(F, N, I, V, BootInstance)
		     || {F, N, I, V} <- Deferred],
		    mount_cxp_single(CxpFile, Name, Id, Version, BootInstance),
		    Backlog#mountBacklog{deferred=[],
					 nofCxpsLeft=CxpsLeft-1};
		Backlog#mountBacklog.isOsMounted ->
		    mount_cxp_single(CxpFile, Name, Id, Version, BootInstance),
		    Backlog#mountBacklog{nofCxpsLeft=CxpsLeft-1};
		true ->
		    Backlog#mountBacklog{deferred=Deferred++[{CxpFile, Name, Id, Version}],
					 nofCxpsLeft=CxpsLeft-1}
	    end
    end.


-spec isCxpMarked(atom(), #xmlElement{}) -> boolean().

isCxpMarked(MarkerE, ConfigurationE) ->
    try find_element(MarkerE, ConfigurationE) of
	_ ->
	    true
    catch
	_:_ ->
	    false
    end.


%%% ----------------------------------------------------------
%%% @doc Mount a CxpFile that is NOT a bundle.
%%% @end
%%% ----------------------------------------------------------

mount_cxp_single(CxpFile, Name, Id, Version, BootInstanceNo) ->
    BootInstance = integer_to_list(BootInstanceNo),
    %% HS23701 fix
    info_msg("Mounting ~s~n",[CxpFile]),
    {Free, _} = disk_free(),
    Usage = disk_usage(CxpFile),
    if
	Usage > Free ->
	   throw({fail, "Mount failed due to disk space shortage"});
       true ->
	   ok
    end,
    %% HS23701 ends
    %% HT39022 Always call cup regardles if the CXP is mounted or not
    %% Get the CXP directory: RCS_ROOT/software/CxpName_CxpId_CxpVersion
    Cmd = ["sudo cup -m -p ", CxpFile, " -e ", BootInstance],
    case timer:tc(fun() -> cmdres(Cmd) end) of
	{Time, {0, _}} ->
	    Msg = 
		io_lib:format("swmOs: Mount execution time;~p;~p;~p~n",
			      [Time/1000000, filename:basename(CxpFile),
			       filelib:file_size(CxpFile)]),
	    log_SwmInternal(info, Msg),
	    ok;
	{_, {1, ERes}} ->
	    error_msg("~s~n~s~n",[lists:flatten(Cmd), ERes]),
	    erlang:error(cup_internal_error, [CxpFile, Name, Id, Version]);
	{_, {2, ERes}} ->
	    error_msg("~s~n~s~n~s~nsize: ~w~n",[lists:flatten(Cmd), ERes, 
						os:cmd(["md5sum ", CxpFile]),
						filelib:file_size(CxpFile)]),
	    Basename = Name++"_"++no_slash(Id)++"_"++Version,
	    Fail = "Mount failed due to corrupt or unsigned file "++Basename,
	    throw({fail, Fail})
    end.


%%% ----------------------------------------------------------
%%% @doc Unmounts a file system on the given mount point.
%%% @end
%%% ----------------------------------------------------------

unmount(target, MountPoint) ->
    case filelib:wildcard(filename:join(MountPoint, "*")) of
	[] ->
	    {0, ""};
	_ ->
	    cmdres(["sudo cup -u -i ", MountPoint])
    end;

unmount(simulated, MountPoint) ->
    cmd("rm -rf "++MountPoint++"/*"),
    {0, ""};

unmount(vrcs, _) ->
    {0, ""}.




%%% ----------------------------------------------------------
%%% #           get_up_archive_dir(ProductData)
%%% Input: ProductData::[#'ProductData']|#'ProductData'
%%% Output: Path::string()
%%% Exceptions:
%%% Description: Returns the path of the archive directory where the UP is
%%%              stored. The ProductData struct is assumed to have UP info
%%% ----------------------------------------------------------

get_up_archive_dir([ProductData]) ->
    get_up_archive_dir(ProductData);
get_up_archive_dir(ProductData) ->
    #'ProductData'{productName = Name,
		   productNumber = Id,
		   productRevision = Version} = ProductData,
    filename:join(swmLib:archive_dir(),
		  Name++"_"++no_slash(Id)++"_"++Version).

%%% ----------------------------------------------------------
%%% #           no_slash(Str)
%%% Input: Str::string()
%%% Output: string()
%%% Exceptions:
%%% Description: Replace the "/" character with "_" in the given string
%%% ----------------------------------------------------------

no_slash(Str) ->
    [case X of
	 $/ -> $_;
	 _ -> X
     end || X<-Str].


%%% ----------------------------------------------------------
%%% #           unpack_cxp(CxpFile, SquashFsDir, SwVersionDir)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%%
%%% This function is only used in the simulator
%%% environment. Arguments are:
%%%
%%% CxpFile: a CXP in tar.gz format, such as
%%%     RCS_ROOT/rcs/swm/archive/UGT-SIM_CXS000_R1B/APP1-SIM_CXP001.cxp
%%%
%%% SquashFsDir: RCS_ROOT/software
%%%
%%% SwVersionDir: RCS_ROOT/home2/USER/software
%%%
%%% The returned value is 'undefined', except in case the OTP software is
%%% found as a tar.gz package on a special path; that package is then expanded
%%% and the path to its root is returned. NOTE: This feature is not being
%%% depended upon as long as "simulated soft upgrade" is not supported.
%%% ----------------------------------------------------------

-spec unpack_cxp(string(), string(), string()) -> ok.

unpack_cxp(CxpFile, SquashFsDir, SwVersionDir) ->
    info_msg("Unpacking ~p to ~p~n",[CxpFile, SquashFsDir]),

    % Create RCS_ROOT/software/tmp
    TmpDir = filename:join([SquashFsDir, "tmp"]),
    os:cmd("rm -rf "++TmpDir),
    os:cmd("mkdir "++TmpDir),

    % Inspect the OS file type; expect "gzip compressed data"
    cmd("file "++CxpFile),

    % Extract metadata to start with. Do not specify the compression
    % explicitly, the 'tar' utility will figure it out.
    {Code, _} =
	cmdreslog(["cd ", TmpDir,
		   "; tar --wildcards --no-wildcards-match-slash -xf ", CxpFile,
		   " cxp*.xml "]),
    case Code of
	0 -> ok;
	_ ->
	    erlang:error({exit, Code}, [CxpFile, SquashFsDir, SwVersionDir])
    end,

    {XmlFile, _, ProductData} = read_cxp_file(TmpDir),
    cmd("rm -f "++XmlFile),
    #'ProductData'{productName = Name,
		   productNumber = Id,
		   productRevision = Version} = ProductData,

    % Get the CXP directory: RCS_ROOT/software/CxpName_CxpId_CxpVersion
    Basename = Name++"_"++no_slash(Id)++"_"++Version,
    CxpDir = filename:join(SquashFsDir, Basename),
    info_msg("CXP directory: ~s", [CxpDir]),


    % Create a symlink in RCS_ROOT/home2/USER/software pointing to
    % RCS_ROOT/software/CxpName_CxpId_CxpVersion
    % TODO, use file:make_symlink/2 instead?
    {0, _} = cmdreslog("cd "++SwVersionDir++"; ln -s "++CxpDir),

    case filelib:is_dir(CxpDir) of
	true ->
	    % The CXP directory exists; assume the new CXP is identical
	    % to the existing one and do not unpack it
	    info_msg("refuse to unpack CXP already present: ~s~n", [CxpDir]);
	false ->
	    unpack_cxp_finalize(TmpDir, CxpFile, CxpDir)
    end.

read_cxp_file(Dir) ->
    case filelib:wildcard(filename:join(Dir, "cxp*.xml")) of
	[XmlFile] ->
	    case xmerl_scan:file(XmlFile) of
		{ConfigurationE, []} ->
		    try extract_product_data(ConfigurationE) of
			ProductData -> {XmlFile, ConfigurationE, ProductData}
		    catch
			throw:{fail, Reason} ->
			    % pass
			    throw({fail, Reason});
			Type:Error ->
			    sysInitI:warning_report(
			      [{Type, Error},
			       {stacktrace,erlang:get_stacktrace()}]),
			    os:cmd(["cat ",XmlFile]),
			    throw({fail, "Unreadable xml"})
		    end;
		_ ->
		    throw({fail, "Malformed metadata "++
			       filename:basename(XmlFile)})
	    end;
	[] ->
	    throw({fail, "No cxp*.xml file found in "++Dir});
	CxpFiles ->
	    throw({fail, "Multiple cxp*.xml files found in "++Dir++": "++
		       lists:foldl(
			 fun(CxpFile, A) -> filename:basename(CxpFile)++A end,
			 "", CxpFiles)})
    end.



-spec unpack_cxp_finalize(TmpDir::string(), CxpFile::string(), CxpDir::string()) -> ok.

unpack_cxp_finalize(TmpDir, CxpFile, CxpDir) ->
    cmd("cd "++TmpDir++"; tar -xf "++CxpFile),
    cmd("cd "++TmpDir++"; ar x sw.ar sqfs.img"),

    UnpackTool =
	case os:find_executable("unsquashfs") of
	    false ->
		?SQUASHFS_TOOLS_SIM++"/unsquashfs";
	    AbsPath ->
		AbsPath
	end,

    % The -i is a workaround for a bug in unsquashfs as installed
    % in the Stockholm hub SUSE environment; without -i the command
    % would crash.
    cmd("cd "++TmpDir++"; "++UnpackTool++" -f -n -d . sqfs.img"),
    cmd("cd "++TmpDir++"; rm -f sw.ar sqfs.img"),

    % Attempt renaming RCS_ROOT/software/tmp as
    % RCS_ROOT/software/CxpName_CxpId_CxpVersion
    info_msg("Attempt rename; old: ~s~nnew: ~s", [TmpDir, CxpDir]),
    case file:rename(TmpDir, CxpDir) of
	ok ->
	    ok;
	{error, eexist} ->
	    % Cannot happen since we checked before that the CxpDir
	    % does not exist
	    info_msg("IMPOSSIBLE: ~p", [{error, eexist}]),
	    os:cmd("rm -rf "++TmpDir),
	    throw(file:format_error(eexist));
	{error, enoent} ->
	    error_msg("file:rename(~p, ~p) = enoent~n",[TmpDir, CxpDir]),
	    os:cmd("rm -rf "++TmpDir),
	    throw(file:format_error(enoent));
	{error, Reason} ->
	    throw(file:format_error(Reason))
    end.

    %% % If the just-expanded CXP contains OTP as an embedded tar.gz, then
    %% % expand it now and return a directory path. NOTE: CXPs downloaded
    %% % from the CI web seem to have OTP already unpacked (which is sane
    %% % since double compression is meaningless), and so the value returned
    %% % will be 'undefined'.
    %% Pattern = filename:join([CxpDir,"OTP*","otp*","priv","tgt*","*.tar.gz"]),
    %% case filelib:wildcard(Pattern) of
    %% 	[] ->
    %% 	    undefined;
    %% 	[OtpFile] ->
    %% 	    info_msg("found: OTP embedded as tar.gz: ~s", [OtpFile]),
    %% 	    OtpDir = filename:dirname(OtpFile),
    %% 	    cmd("cd "++OtpDir++"; tar -xzf "++OtpFile),
    %% 	    OtpDir
    %% end.


%%% ----------------------------------------------------------
%%% #           extract_product_data()
%%% Input: ConfigurationE:#xmlElement{}
%%% Output: #'ProductData'{}
%%% Exceptions:
%%% Description: Get selected product data
%%% ----------------------------------------------------------
extract_product_data(ConfigurationE) ->
    UpProdE = find_element(product, ConfigurationE),
    Name = find_attribute(name, UpProdE),
    Id = find_attribute(id, UpProdE),
    Version = find_attribute(version, UpProdE),
    #'ProductData'{productName = Name,
		   productNumber = swmInventory:format_product_number(Id),
		   productRevision = Version}.




%%% ----------------------------------------------------------
%%% #           get_up_file(Pattern)
%%% Input: Pattern::string()
%%% Output: FilePath::string()
%%% Exceptions:
%%% Description: Search for the up metadata file
%%% ----------------------------------------------------------

get_up_file(Pattern) ->
    case filelib:wildcard(Pattern) of
	[] ->
	    error_msg("No file matching : ~s~n", [Pattern]),
	    erlang:error(no_such_file, [Pattern]);
	[File] ->
	    File;
	Files ->
	    error_msg("Multiple files matching: ~p~n~p~n",[Pattern,Files]),
	    erlang:error(multiple_files, [Pattern])
    end.


%%% ----------------------------------------------------------
%%% #           make_fallback_script(HomeOther, Current)
%%% Input: HomeOther::string() - Path to the other home dir
%%%        Current::integer() - The boot partition selector
%%% Output:
%%% Exceptions:
%%% Description: Places the "heart_fallback" mini script in the new home
%%%              directory. SWM will notice the file after upgrade reboot and
%%%              install it as heart fallback until the operator confirms the ug
%%% ----------------------------------------------------------
%% HS32665
make_fallback_script(Current)->
    HomeInactive = home_dir(inactive),

    %% Pattern = filename:join(
    %% 		 [HomeInactive, "software", "RCS*", "SWM*", "swm*", "priv"]),
    %% [PrivDir] = filelib:wildcard(Pattern),
    %% D = fun(X) -> filename:dirname(X) end,
    %% RcsCxp = filename:join(swmLib:software_dir(),
    %% 			   filename:basename(D(D(D(PrivDir))))),
    %% [_, SwmLabel] = string:tokens(filename:basename(D(PrivDir)), "-"),


    FallbackPath = filename:join(HomeInactive, "heart_fallback"),
    {ok, Fd} = file:open(FallbackPath, [write]),
    io:format(Fd, "cd /tmp~n", []),
    %% io:format(Fd,"sudo swm_wrapper.sh -c ~s -r ~s -- confirm_os -i ~s~n",
    %% 	      [RcsCxp, SwmLabel, integer_to_list(Current)]),
    io:format(Fd, "sudo cup --confirm_os ~s~n",[integer_to_list(Current)]),

    io:format(Fd, "sudo /sbin/reboot~n",[]),
    file:close(Fd),
    {0,_} = cmdreslog(["chmod a+x ", FallbackPath]),
    info_msg("Fallback heart cmd created~n").

%%% ----------------------------------------------------------
%%% #           run_swm_wrapper(Cmd)
%%% Input: Cmd::string() - A swm_wrapper supported command
%%% Output:
%%% Exceptions:
%%% Description: The swm_wrapper.sh script is allowed to run as root
%%%              and must therefore be able to support all things that needs
%%%              root priviledges to access
%%% ----------------------------------------------------------

%% -spec run_swm_wrapper(Cmd::string()|[string()]) -> string().

%% run_swm_wrapper(Cmd) ->
%%     PrivDir = code:priv_dir(swm),
%%     D = fun(X) -> filename:dirname(X) end,
%%     CurrentCxp = D(D(D(PrivDir))),
%%     [_, SwmLabel] = string:tokens(filename:basename(D(PrivDir)), "-"),
%%     cmd(["cd /tmp ; sudo swm_wrapper.sh -c ", CurrentCxp,
%% 	    " -r ", SwmLabel, " -- "]++Cmd).

%% run_swm_wrapper_res(Cmd) ->
%%     PrivDir = code:priv_dir(swm),
%%     D = fun(X) -> filename:dirname(X) end,
%%     CurrentCxp = D(D(D(PrivDir))),
%%     [_, SwmLabel] = string:tokens(filename:basename(D(PrivDir)), "-"),
%%     cmdreslog(["cd /tmp ; sudo swm_wrapper.sh -c ", CurrentCxp,
%% 	       " -r ", SwmLabel, " -- "]++Cmd).

%%% ----------------------------------------------------------
%%% #           cmd(Cmd)
%%% Input: Cmd::string() - A unix shell command
%%% Output: string()
%%% Exceptions:
%%% Description: os:cmd wrapper with printout
%%% ----------------------------------------------------------
cmd(Cmd) ->
    info_msg("~s~n~s~n",[lists:flatten(Cmd), Res = os:cmd(Cmd)]),
    Res.

%%% Output: {ExitCode::integer(), Output::string()}
cmdres(CmdList) ->
    Cmd = lists:flatten(CmdList),
    CmdR = Cmd++" ; echo -n \"Res=$?\"",
    info_msg("~s~n~s~n",[Cmd, Res = os:cmd(CmdR)]),
    Code = lists:last(string:tokens(Res, "\n=")),
    Rev = lists:reverse(Res),
    Result =
	case string:str(Rev, "\n") of
	    0 -> "";
	    Pos -> lists:reverse(string:sub_string(Rev, Pos))
	end,
    {list_to_integer(Code), Result}.

cmdreslog(CmdList) ->
    Cmd = lists:flatten(CmdList),
    log_SwmInternal(info, Cmd),
    CmdR = Cmd++" ; echo -n \"Res=$?\"",
    Res = os:cmd(CmdR),
    Code = list_to_integer(lists:last(string:tokens(Res, "\n="))),
    case Code of
	0 -> 
	    log_SwmInternal(info, Res);
	_ -> 
	    log_SwmInternal(error, Res),
	    warning_msg("~s~n~s~n",[Cmd, Res])
    end,
    Rev = lists:reverse(Res),
    Result =
	case string:str(Rev, "\n") of
	    0 -> "";
	    Pos -> lists:reverse(string:sub_string(Rev, Pos))
	end,
    {Code, Result}.

%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions: A badmatch occurs if the element does not exist
%%% Description: Finds a sub element to an xml element, or in a list
%%%              of element contents. Assumes there is only one element
%%%              with the same name
%%% ----------------------------------------------------------

find_element(ElementName, Element) when is_record(Element, xmlElement) ->
    find_element(ElementName, Element#xmlElement.content);
find_element(ElementName, ContentList) ->
    case lists:keysearch(ElementName, #xmlElement.name, ContentList) of
	{value, Element}  ->
	    Element;
	false ->
	    throw({not_found, ElementName})
    end.


%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions: erlang:error invoked if attribute does not exist
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ----------------------------------------------------------

find_attribute(AttributeName, Element) when is_record(Element, xmlElement) ->
    find_attribute(AttributeName, Element#xmlElement.attributes);
find_attribute(AttributeName, AttributeList) ->
    case lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList) of
        {value, Attribute} ->
            Attribute#xmlAttribute.value;
        false ->
	    throw({not_found, AttributeName})
    end.


info_msg(Format) ->
    info_msg(Format, []).
info_msg(Format, Args) ->
    Msg = lists:flatten(io_lib:format(Format, Args)),
    log_SwmInternal(info, Msg),
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

error_msg(Format) ->
    error_msg(Format, []).
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).


%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Recreates the original UP with bundle CXPs for testing purposes
%%% @end
%%% ----------------------------------------------------------

recreate_up(active) ->
    [ProductData] = swmInventory:get_current_sw_version(), % only one
    ArchiveDir = get_up_archive_dir(ProductData),
    Template = filename:join(swmLib:swm_dir(), "tmpup.XXXXXX"),
    TmpDir = os:cmd(["mktemp -d ", Template])--"\n",
    {ok, Files} = file:list_dir(ArchiveDir),
    [file:make_link(filename:join(ArchiveDir,F), filename:join(TmpDir, F))||
	F<-Files],
    BFIndex = [begin
		   WrkDir = cmd("mktemp -d /tmp/swm.XXXXXX")--"\n",
		   cmd(["cd ", WrkDir,
			"; tar --wildcards --no-wildcards-match-slash -xf ",
			filename:join(TmpDir, File), " cxp*.xml "]),
		   [P] = filelib:wildcard(filename:join(WrkDir, "cxp*.xml")),
		   {ConfE, []} = xmerl_scan:file(P),
		   cmd(["rm -rf ", WrkDir]),
		   ThisProductData = extract_product_data(ConfE),
		   #'ProductData'{productName = Name,
				  productNumber = Id,
				  productRevision = Version} = ThisProductData,
		   {{Name, Id, Version}, File}
	       end||File<-Files,
		    filename:extension(File)==".cxp"],
    sysInitI:info_report(BFIndex),
    BundleFilesP = filename:join(TmpDir, "cxp*.xml"),
    BundleFiles = filelib:wildcard(BundleFilesP),
    [begin
	 {ConfigurationE, []} = xmerl_scan:file(BF),
	 ThisProductData = extract_product_data(ConfigurationE),
	 #'ProductData'{productName = Name2,
			productNumber = Id2,
			productRevision = Version2} = ThisProductData,
	 BundleFile = ns(Name2++"_"++Id2++"_"++Version2),
	 cmd(["cd ",TmpDir, " ; tar cf ",BundleFile, " ",
	      filename:basename(BF), " ; rm -f ",BF]),
	 ContentInfoE = find_element(contentinfo, ConfigurationE),
	 [begin
	      N = find_attribute(name, ProdE),
	      I = find_attribute(id, ProdE),
	      V = find_attribute(version, ProdE),
	      CxpFile = case lists:keyfind({N,I,V}, 1, BFIndex) of
			    {_, CF} -> CF;
			    false ->
				{_, CF} =
				    lists:keyfind({N,rs(I),rs(V)}, 1, BFIndex),
				CF
			end,
	      cmd(["cd ",TmpDir, " ; tar rf ",BundleFile, " ",CxpFile, " ; "
		   "rm -f ", CxpFile])
	  end
	  ||ProdE <- ContentInfoE#xmlElement.content,
	    ProdE#xmlElement.name == product],
	 cmd(["cd ",TmpDir, " ; gzip --suffix .cxp ", BundleFile])
     end||BF<-BundleFiles],

    {ok, TmpDir}.

%%% Description: No slash
ns(String) ->
    [case X of $/ -> $_; _->X end ||X<-String].

%%% Description: Restore slash
rs(String) ->
    [case X of $_ -> $/; _->X end ||X<-String].

%%% ----------------------------------------------------------
%%% @doc Modify the version of an UP for testing purposes
%%% @end
%%% ----------------------------------------------------------

modify_up_version(Dir) ->
    Pattern = filename:join(Dir, "*-up.xml"),
    [UpFile] = filelib:wildcard(Pattern),
    {ConfigurationE, []} = xmerl_scan:file(UpFile),
    ProductE = find_element(product, ConfigurationE),
    {value, VersionA} = lists:keysearch(version, #xmlAttribute.name,
					ProductE#xmlElement.attributes),
    Version = VersionA#xmlAttribute.value,
    NewVersionA = VersionA#xmlAttribute{value = Version++"0"},
    NewProductAttributes = lists:keyreplace(version, #xmlAttribute.name,
					    ProductE#xmlElement.attributes,
					    NewVersionA),
    NewProductE = ProductE#xmlElement{attributes=NewProductAttributes},
    NewConfig = lists:keyreplace(product, #xmlElement.name,
				 ConfigurationE#xmlElement.content,
				 NewProductE),
    NewConfigE = ConfigurationE#xmlElement{content=NewConfig},
    Result = xmerl:export([NewConfigE], xmerl_xml, []),
    %% Note! It is necessary to delete the file, because it may be hard linked
    %% to the original file and we don't want to alter the original
    ok = file:delete(UpFile),
    {ok, Fd} = file:open(UpFile, [write]),
    io:format(Fd, Result, []),
    file:close(Fd),
    {ok, UpFile}.

%%% ----------------------------------------------------------
log_SwmInternal(Severity, Info) ->
    try
	logI:write_log("SwmInternal", "swmOs", Severity, log_format(Info))
    catch
	ErrClass : ErrReason ->
	    Stacktrace = erlang:get_stacktrace(),
	    case ets:info(logInternal) of
		undefined ->
		    sysInitI:info_report(["SWM internal log table not created",
					  {Severity, Info}]);
		_ ->
		    sysInitI:warning_report(["Logging failed",
					     {Severity, Info},
					     {ErrClass, ErrReason},
					     {stacktrace, Stacktrace}])
	    end
    end.

%%% ----------------------------------------------------------
log_format(Term) ->
    try
	io_lib:format("~s", [Term]),
	remove_last_nl(Term)
    catch
	_ : _ ->
	    remove_last_nl(lists:flatten(log_format_list(Term)))
    end.
%% log_format(Term) ->
%%     sysUtil:term_to_string(Term).

log_format_list([{T1, T2} | Tail]) ->
    ["    " ++
     sysUtil:term_to_string(T1) ++ ": " ++ sysUtil:term_to_string(T2) ++
     "\n"
     | log_format_list(Tail)];
log_format_list([Term | Tail]) ->
    ["    " ++
     sysUtil:term_to_string(Term) ++
     "\n"
     | log_format_list(Tail)];
log_format_list([]) ->
    [].

%%% ----------------------------------------------------------
remove_last_nl([]) ->
    [];
remove_last_nl(Str) ->
    case lists:last(Str) of
	$\n ->
	    remove_last_nl(lists:droplast(Str));
	_ ->
	    Str
    end.
