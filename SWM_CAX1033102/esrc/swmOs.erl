%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmOs.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R5A/7

%%% @doc ==OS manipulating functions==
%%% This module contains OS installation manipulating functions which are
%%% later going to be covered by Linux EE
%%% @end

-module(swmOs).
-vsn('/main/R2A/R3A/R4A/R5A/7').
-date('2016-03-18').
-author('etxberb').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([homes/1, home_dir_other/0]).
-export([install_up/1,
	 mount_cxp_deferring/2,
	 preload/0,
	 activate/0,
	 commit/0,
	 clear/0]).

-export([recreate_up/1, modify_up_version/1]).

-export([disk_free/0, disk_usage/1]).

-export([get_active_instance/0]).
-export([run_swm_wrapper/1, run_swm_wrapper_res/1]).
-export([unmount/2]).
-export([get_installed_cxps/0, remove_cxp/1]).

-export([reset_boot/1]).

-export([clear_application_logs/0, clear_application_tmp/0]).

-export([cmdres/1, cmdreslog/1]).

-export([fsck/0, fsck/1]).
-export([dns/1, arp/1, ldconfig/1]).

%% -export([fuser_check/1]).
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsSwM.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("SwmInternal.hrl").

%% This path is used if unsquashfs cannot be found in the
%% ordinary PATH.
-define(SQUASHFS_TOOLS_SIM, "/app/rbs/wrtools/tools-sdk-20130220/usr/sbin").

%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

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
    homes(sysEnv:rcs_mode(), sysEnv:board_type(), Op).

homes(target, BT, Cmd) when BT == dus ; BT == duw2 ->
    erlang:error(unsupported_board_type, [target, BT, Cmd]);

homes(target, _, mount) ->
    run_swm_wrapper("mount /dev/rootvg/hfs1-lv /rcs/swm/home1"),
    run_swm_wrapper("mount /dev/rootvg/hfs2-lv /rcs/swm/home2"),
    ok;
homes(target, _, umount) ->
    run_swm_wrapper("umount /rcs/swm/home1"),
    run_swm_wrapper("umount /rcs/swm/home2"),
    ok;
homes(simulated, _, _) ->
    ok.


%%% ----------------------------------------------------------
%%% @doc Returns the path to the non active home dir
%%% Classical cases are handled by sysEnv
%%% @end
%%% ----------------------------------------------------------

home_dir_other() ->
    home_dir_other(sysEnv:rcs_mode(), sysEnv:board_type()).

home_dir_other(target, BT) when BT == dus ; BT == duw2 ->
    erlang:error(unsupported_board_type, [target, BT]);
home_dir_other(target, _) ->
    case get_active_instance() of
	1 -> "/rcs/swm/home2/sirpa";
	2 -> "/rcs/swm/home1/sirpa"
    end;
home_dir_other(simulated, _) ->
    PrivDir = code:priv_dir(sys),
    {match, [{Start, Length}]} = re:run(PrivDir, "home.?/"),
    case string:substr(PrivDir, Start+1, Length-1) of
	"home" ->
	    filename:join([sysEnv:rcs_root(), "home2", os:getenv("USER")]);
	"home2" ->
	    sysEnv:home_dir()
    end.

%%% ----------------------------------------------------------
%%% @doc Install all load module containers and prepare the other home area
%%% Given an UP MO, unpack (and later mount) the included
%%% loadmodule containers on the mount point.
%%% This function presumes that both home directories are available
%%% Input: UP:#upgradePackage{}
%%% @end
%%% ----------------------------------------------------------

-spec install_up(UP::#upgradePackage{}) -> ok.

install_up(UP) ->
    info_msg("Installing ~p~n",[element(4,UP#upgradePackage.upgradePackageId)]),

    SwVersionDir = swmLib:software_dir_other(),
    cmd("mkdir -p "++SwVersionDir),

    ArchiveDir = get_up_archive_dir(UP#upgradePackage.administrativeData),

    BoardType_BB = swmBoardList:boardType(),
    BoardTypes_RADIO = all,
    Products =
	case
	    swmBoardList:products({BoardType_BB, BoardTypes_RADIO},
				  {global, ArchiveDir})
	    of
	    {ok, Result} ->
		Result;
	    {uc_error, Reason} ->
		throw({uc_error, Reason})
	end,
    try
	mount_software(sysEnv:rcs_mode(), cxp_files(Products, ArchiveDir))
    catch
	throw : {cxp_files, metadata_filename_missing} ->
	    %% Restore backup to a UP with old metadata files. Backup without
	    %% HAL functionality.
	    CxpFiles = filelib:wildcard(filename:join(ArchiveDir, "*.cxp")),
	    mount_software(sysEnv:rcs_mode(), CxpFiles)
    end,

    info_msg("After mounting of software:~n~n~s~n",
	     [os:cmd("echo $RCS_ROOT/software ; " ++
		     "ls -la $RCS_ROOT/software")]),

    %% Prepare to generate necessary files

    MetaFile = get_up_file(filename:join(ArchiveDir, "*-up.xml")),
    cmd("cp "++MetaFile++" "++SwVersionDir++"/"),

    %% Make links to /software
    case sysEnv:rcs_mode() of
	target ->
	    make_links(ArchiveDir, SwVersionDir, Products);
	simulated ->
	    %% This is done in unpack_cxp
	    ok
    end,
    info_msg("After linking to software:~n~n~s~n",
	     [os:cmd("echo " ++ SwVersionDir ++ " ; ls -la " ++ SwVersionDir)]),
    CxsPath = filename:join(SwVersionDir, filename:basename(MetaFile)),

    swmModel:update_sw_version(CxsPath),
    swmInventory:update_sw_version(CxsPath),
    ok.

%%% ----------------------------------------------------------
cxp_files([{_, {_, undefined}} = Product | _], GlobalDir) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     {old_UP, metadata_filename_missing},
			     Product,
			     {globalDir, GlobalDir}]),
    throw({cxp_files, metadata_filename_missing});
cxp_files([{{Name, Id, Version}, {Source, CxpFile}} | Tail], GlobalDir) ->
    Dir = dir(Source, GlobalDir),
    CxpPath = filename:join(Dir, CxpFile),
    case filelib:is_regular(CxpPath) of
 	true ->
 	    [CxpPath | cxp_files(Tail, GlobalDir)];
 	false ->
 	    Bundles = filelib:wildcard(filename:join(Dir, "cxp*.xml")),
 	    case find_bundle(Bundles, Name, Id, Version, Source) of
 		{_Bundle, Products} ->
 		    cxp_files(Products ++ Tail, GlobalDir);
 		undefined ->
		    Ls_CxpPath = os:cmd("ls -la " ++ CxpPath),
 		    ErrInfo = [Name,
			       Id,
			       Version,
			       Source,
			       CxpFile,
			       GlobalDir,
			       Ls_CxpPath],
 		    erlang:error(bundle_not_found, ErrInfo)
 	    end
    end;
cxp_files([], _) ->
    [].

%%% ----------------------------------------------------------
make_links(GlobalArchiveDir, SwVersionDir, Products) ->
    SquashFsDir = swmLib:squash_fs_dir(),
    [begin
	 ArchiveDir = dir(Source, GlobalArchiveDir),
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
				Source)
	 end
     end
     || {{Name, Id, Version}, {Source, _File}} <- Products],
    ok.

%%% ----------------------------------------------------------
dir(global, Dir) ->
    Dir;
dir(hal, _) ->
    swmLib:software_hal_dir().

%%% ----------------------------------------------------------
install_bundle(Name, Id, Version, ArchiveDir, SwVersionDir, Source) ->
    CxpP = filename:join(ArchiveDir, "cxp*.xml"),
    Bundles = filelib:wildcard(CxpP),
    case find_bundle(Bundles, Name, Id, Version, Source) of
	{Bundle, Products} ->
	    cmd(["cp ", Bundle, " ", SwVersionDir]),
	    make_links(ArchiveDir, SwVersionDir, Products);
	undefined ->
	    ErrInfo = [Name, Id, Version, ArchiveDir, SwVersionDir, Source, []],
	    erlang:error(bundle_not_found, ErrInfo)
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
    HomeOther = home_dir_other(),
    case sysEnv:rcs_mode() of
	target ->
	    run_swm_wrapper(["clear_ee -h ",HomeOther]);
	simulated ->
	    cmd(["rm -rf ", HomeOther])
    end,
    ok.


%%% ----------------------------------------------------------
%%% @doc Install the various part of the OS to it's correct places
%%% @end
%%% ----------------------------------------------------------
-spec preload() -> ok.

preload()->
    info_msg("Preloading...~n"),
    preload(sysEnv:rcs_mode(), sysEnv:board_type()),
    ok.

%%% ----------------------------------------------------------
%%% @doc Edit in boot flash to make the board boot on the other rfs partition
%%% @end
%%% ----------------------------------------------------------
-spec activate() -> ok.

activate() ->
    info_msg("Activating... ~n"),
    activate(sysEnv:rcs_mode(), sysEnv:board_type()),
    ok.

%%% ----------------------------------------------------------
%%% @doc Remove the rollback capacity.
%%% The new software state is now final
%%% @end
%%% ----------------------------------------------------------

-spec commit() -> ok.

commit() ->
    commit(sysEnv:rcs_mode(), sysEnv:board_type()).


%%% ----------------------------------------------------------
%%% @doc Returns the number of the active instance
%%% Instance, fetched Uboot env-space on DUS41 and from boot flash
%%% boot space on TCU03
%%% @end
%%% ----------------------------------------------------------
-spec get_active_instance() -> 0|1|2|not_supported.
get_active_instance() ->
    case sysEnv:rcs_mode() of
	simulated -> not_supported;
	target ->
	    {ok, Return} = file:read_link("/opt/rcs_ee/mounts/boot/configured"),
	    {match, [{Start, Length}]} = re:run(Return, "b[0-9]"),
	    list_to_integer(string:substr(Return, Start+2, Length-1))
    end.



%%% ----------------------------------------------------------
%%% @doc Returns the free space and total space of the root volume
%%% Size given in bytes
%%% @end
%%% ----------------------------------------------------------

-spec disk_free() -> {FreeBytes::non_neg_integer()|infinity,
		      TotalBytes::non_neg_integer()|infinity}.

disk_free() ->
    disk_free(sysEnv:rcs_mode(), sysEnv:board_type()).

disk_free(target, BT) when BT == dus ; BT == duw2 ->
    erlang:error(unsupported_board_type, [target, BT]);    
disk_free(target, BT) ->
    {0, Res} = run_swm_wrapper_res("diskfree"),
    {match, [{Start, Length}]} = re:run(Res, "[0-9]+B"),
    {list_to_integer(string:substr(Res, Start+1, Length-1)),
     disk_size(target, BT)};
disk_free(simulated, BT) ->
    {infinity, disk_size(simulated, BT)}.


disk_size(target, BT) when BT == dus ; BT == duw2 ->
    infinity; % This alternative should not happen anyway
disk_size(target, _) ->
    {0, Res} = run_swm_wrapper_res("disksize"),
    {match, [{Start, Length}]} = re:run(Res, "[0-9]+B"),
    list_to_integer(string:substr(Res, Start+1, Length-1));
disk_size(simulated, _) ->
    infinity.

%%% ----------------------------------------------------------
%%% @doc Returns the expected size of the logical volume for a cxp
%%% @end
%%% ----------------------------------------------------------

disk_usage(Cxp) ->
    {0, Res} = run_swm_wrapper_res(["cup -s ",Cxp]),
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
   run_swm_wrapper(["reset_boot " ++ Typestring]),
   ok.

%%% ----------------------------------------------------------
%%% doc Install the list of cxp files that are to be mounted in the flash area
%%% end
%%% ----------------------------------------------------------

%% install_cxp_list(Path) ->
%%     Current = integer_to_list(get_active_instance()),
%%     install_cxp_list(Path, Current).

%% install_cxp_list(Path, Area) ->
%%     %% Calculate md5 sum
%%     Md5Path = filename:join("/tmp", filename:basename(Path)++".md5sum"),
%%     cmd(["cd ", filename:dirname(Path), " ; "
%% 	 "md5sum ", filename:basename(Path), " > ", Md5Path]),
%%     run_swm_wrapper(
%%       ["install_cxp_list -f ",Path," -m ", Md5Path, " -i ", Area]).

%%% ----------------------------------------------------------
%%% doc Install the list of cxp files that are to be mounted in the flash area
%%% end
%%% ----------------------------------------------------------

get_installed_cxps()->
    get_installed_cxps(sysEnv:rcs_mode()).

get_installed_cxps(simulated) ->
    {ok, Files} = file:list_dir(swmLib:squash_fs_dir()),
    Files;
get_installed_cxps(target) ->
    [filename:basename(X)||
	X<-string:tokens(run_swm_wrapper("cup -d"), "\n")].

remove_cxp(Cxp) ->
    remove_cxp(sysEnv:rcs_mode(), Cxp).

remove_cxp(simulated, Cxp) ->
    cmd(["rm -rf ",filename:join(swmLib:squash_fs_dir(), Cxp)]),
    ok;

remove_cxp(target, Cxp) ->
    case is_cxp_mounted(target, Cxp) of
	false ->
	    new_remove_cxp(target, Cxp);
	true ->
	    old_remove_cxp(target, Cxp)
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
    case run_swm_wrapper_res(["cup -r ", Cxp]) of
	{0, _} ->
	    ok;
	{_, ERes} ->
	    error_msg("Cup remove failed for ~s~n~s~n~s~n",
		      [Cxp, "cup -r "++Cxp, ERes]),
	    error
    end.
    

old_remove_cxp(target, Cxp) ->
    %% Old command is used if for some reason the cxp list is not used
    Dir = swmLib:squash_fs_dir(),
    UIcommand = ["cup -u -i ", filename:join(Dir, Cxp)],
    case run_swm_wrapper_res(UIcommand) of
	{0, _} ->
	    ok;
	{_, Eres2} -> 
	    error_msg("Cup unmount failed for ~s~n~s~n~s~n",
		      [Cxp, lists:flatten(UIcommand), Eres2]),
	    error
    end.
    


fsck() ->
    fsck("").

fsck(Args) ->
    case sysEnv:rcs_mode() of
	target ->
	    run_swm_wrapper_res(["fsck ",Args]);
	simulated ->
	    ok
    end.

%%% NOT UPGRADE RELEATED FUNCTIONS

dns(Argv) ->
    run_swm_wrapper("dns "++Argv).

arp(Argv) ->
    run_swm_wrapper("arp "++Argv).

ldconfig(Argv) ->
    run_swm_wrapper("ldconfig "++Argv).

clear_application_logs() ->
    clear_application_logs(sysEnv:rcs_mode()).

clear_application_logs(target) ->
    run_swm_wrapper("clear_applicationlogs");
clear_application_logs(simulated) ->
    cmd(["\\rm -rf ", sysEnv:rcs_dir(), "/applicationlogs/*/*"]).


%%% ----------------------------------------------------------
%%% @doc Clear applicationtmp
%%% @end
%%% ----------------------------------------------------------

clear_application_tmp() ->
   clear_application_tmp(sysEnv:rcs_mode()).

clear_application_tmp(target) ->
    run_swm_wrapper("clear_applicationtmp");
clear_application_tmp(simulated) ->
    cmd(["\\rm -rf ", sysEnv:rcs_dir(), "tmp/applicationtmp/*/*"]).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

prepare_nl_boot() ->
    {_, Res} = run_swm_wrapper_res(["prepare_nl_upgrade"]),
    Res.
  


write_nl_boot(OsCxp) ->
    Cmd = ["cup --nl-boot-install ", " -p ", OsCxp],
    case run_swm_wrapper_res(Cmd) of
	{0, _} ->
	    info_msg("Installed nl from ~s~n",[OsCxp]),
	    ok;
	{1, ERes} ->
	    error_msg("~s~n~s~n",[lists:flatten(Cmd), ERes]),
	    erlang:error(cup_internal_error, [target, OsCxp]);
	{2, _} ->
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

preload(target, BT) when BT == dus ; BT == duw2 ->
    erlang:error(unsupported_board_type, BT);
preload(target, BT) ->
    OsCxp = swmLib:os_cxp(),
    info_msg("Installing ~s~n",[OsCxp]),

    movePatches(home_dir_other()),

    Next = case get_active_instance() of
	       1 -> "2";
	       2 -> "1"
	   end,


    case run_swm_wrapper_res(["cup -k ", Next, " -p ", OsCxp]) of
	{0, _} ->
	    info_msg("Installed ~s~n",[OsCxp]);
	{1, _} ->
	    erlang:error(cup_internal_error, [target, BT]);
	{2, _} ->
	    throw({fail, "Preload failed due to corrupt or unsigned software file"})
    end,
    prepare_nl_boot(),
    %% Copy old nl boot files to a temporary dir. 
    %% Old files and new files will be swapped/deleted at confirm
    write_nl_boot(OsCxp);
preload(simulated, _) ->
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
    cmd(["chmod a+x ",switch_over_path()]).


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
activate(target, BT) when BT==dus52 ; BT==dus32 ->
    HomeOther = home_dir_other(),
    swmLib:make_cxp_list(HomeOther),

    Current = get_active_instance(),
    Next = case Current of
	       1 -> "2";
	       2 -> "1"
	   end,
    
    make_fallback_script(Current), % HS32665
    replace_uboot(),
    %% Cause the uboot to reboot once on the new configuration
    %% and then rollback
    run_swm_wrapper("activate_os -u true -i "++Next),
    cmd("ls -lR /opt/rcs_ee/mounts/boot"),
    ok = sysRhai:setbootptr(upgrade),
    ok;
activate(target, _) ->
    HomeOther = home_dir_other(),
    swmLib:make_cxp_list(HomeOther),

    Current = get_active_instance(),
    Next = case Current of
	       1 -> "2";
	       2 -> "1"
	   end,
    make_fallback_script(Current), % HS32665
    replace_uboot(),
    %% Cause the uboot to reboot once on the new configuration
    %% and then rollback
    run_swm_wrapper("activate_os -u true -i "++Next),
    ok = sysRhai:setbootptr(upgrade);
activate(simulated,_) ->
    SwoPath = switch_over_path(),
    %% No temporary fallback fix for sim. It's faster to simply reinstall
    info_msg("Activating ~p~n",[SwoPath]),
    ok = heart:set_cmd(SwoPath).

%% make_cxp_list_other(Next) ->
%%     Path = "/tmp/cxp_list",
%%     HomeOther = home_dir_other(),
%%     {ok, Files} = file:list_dir(filename:join(HomeOther, "software")),
%%     {ok, Fd} = file:open(Path, [write]),
%%     [io:format(Fd, "~s~n",[Cxp])||Cxp<-Files,
%% 				  filename:extension(Cxp) /= ".xml"],
%%     file:close(Fd),
%%     install_cxp_list(Path, Next),
%%     ok.

replace_uboot() ->
    try do_replace_uboot() 
    catch T:E ->
	    error_msg("Uboot replace failed: ~p~n~p~n",
		      [{T,E}, erlang:get_stacktrace()])
    end.

do_replace_uboot() ->
    CxpFile = swmLib:get_variable(os_cxp),
    {_,_,ProductData} = extract_cxp_data(CxpFile),
    #'ProductData'{productName = Name,
		   productNumber = Id,
		   productRevision = Version} = ProductData,
    SquashFsDir = swmLib:squash_fs_dir(),
    MountPoint = filename:join(SquashFsDir, Name++"_"++Id++"_"++Version),
    case cmdreslog("cup -w "++MountPoint) of
	{0, _} ->
	    ok;
	{Code, _} ->
	    erlang:error({exit_code, Code}, [])
    end.
    

extract_cxp_data(CxpFile) ->
    TmpDir = os:cmd("mktemp -d /tmp/swm.XXXXXX")--"\n",
    TarCmd =
	["cd ", TmpDir, ";"
	 "tar --wildcards --no-wildcards-match-slash -xf ", CxpFile, " cxp*.xml"],
    case cmdreslog(TarCmd) of
	{0, _} ->
	    ok;
	{Code, _} ->
	    erlang:error({exit_code, Code}, [CxpFile])
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
    Current = get_active_instance(),
    Next = case Current of
	       1 -> "2";
	       2 -> "1"
	   end,
    run_swm_wrapper("confirm_os -i "++Next),
    cmd("ls -lR /opt/rcs_ee/mounts/boot"),
    ok = sysRhai:setbootptr(configured),
    ok = sysRhai:setbootcounter(0);
commit(target, _) ->
    info_msg("Reverting heart command~n"),
    Heart = os:getenv("HEART_COMMAND"),
    ok = heart:set_cmd(Heart),
    HeartPath = filename:join(sysEnv:home_dir(), "heart_fallback"),
    file:rename(HeartPath, HeartPath++".old"),
    Current = get_active_instance(),
    Next = case Current of
	       1 -> "2";
	       2 -> "1"
	   end,
    run_swm_wrapper("confirm_os -i "++Next),
    ok = sysRhai:setbootptr(configured),
    ok = sysRhai:setbootcounter(0);
commit(_, _) ->
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
    Path = os:getenv("PATH"),
    os:putenv("PATH", "/addons/bin:"++Path),
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
    [mount_cxp(simulated, CxpFile)||CxpFile<-CxpFiles].


mount_cxp(simulated, CxpFile) ->
    SwVersionDir = swmLib:software_dir_other(),
    cmd(["mkdir -p ", SwVersionDir]),
    SquashFsDir = swmLib:squash_fs_dir(),
    info_msg("Unpacking ~s~n",[CxpFile]),
    unpack_cxp(CxpFile, SquashFsDir, SwVersionDir),
    info_msg("Unpacked ~s~n",[CxpFile]),
    ok.


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

mount_cxp_deferring(CxpFile, Backlog) ->
    case sysEnv:rcs_mode() of
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
	    logI:write_log("SwmInternal", "swmOs", info, Msg),
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
		    mount_cxp_single(CxpFile, Name, Id, Version),
		    swmLib:set_variable(os_cxp, CxpFile),
		    % Deferred CXPs can be mounted now
		    [mount_cxp_single(F, N, I, V)|| {F, N, I, V} <- Deferred],
		    Backlog#mountBacklog{isOsMounted=true,
					 deferred=[],
					 nofCxpsLeft=CxpsLeft-1};
		Backlog#mountBacklog.nofCxpsLeft =:= 1 ->
		    % this is the very last CXP; just mount the deferred CXPs now
		    % and finally this last one even though no 'os' seen
		    [mount_cxp_single(F, N, I, V)|| {F, N, I, V} <- Deferred],
		    mount_cxp_single(CxpFile, Name, Id, Version),
		    Backlog#mountBacklog{deferred=[],
					 nofCxpsLeft=CxpsLeft-1};
		Backlog#mountBacklog.isOsMounted ->
		    mount_cxp_single(CxpFile, Name, Id, Version),
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

mount_cxp_single(CxpFile, Name, Id, Version) ->
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
    Current = get_active_instance(),
    Next = case Current of
	       1 -> "2";
	       2 -> "1"
	   end,
    Cmd = ["cup -m -p ", CxpFile, " -e ", Next],
    case run_swm_wrapper_res(Cmd) of
	{0, _} ->
	    ok;
	{1, ERes} ->
	    error_msg("~s~n~s~n",[lists:flatten(Cmd), ERes]),
	    erlang:error(cup_internal_error, [CxpFile, Name, Id, Version]);
	{2, _} ->
	    Basename = Name++"_"++no_slash(Id)++"_"++Version,
	    Fail = "Mount failed due to corrupt or unsigned file "++Basename,
	    throw({fail, Fail})
    end.


%%% ----------------------------------------------------------
%%% @doc Unmounts a file system on the given mount point.
%%% @end
%%% ----------------------------------------------------------

-spec unmount(target|simulated, string()) -> {integer(), string()}.

unmount(target, MountPoint) ->
    case filelib:wildcard(filename:join(MountPoint, "*")) of
	[] ->
	    {0, ""};
	_ ->
	    run_swm_wrapper_res(["cup -u -i ", MountPoint])
    end;

unmount(simulated, MountPoint) ->
    cmd("rm -rf "++MountPoint++"/*"),
    {0, ""}.

%% fuser_check(MountPoint) ->
%%     case run_swm_wrapper(["fuser -m ", MountPoint]) of
%% 	"" ->
%% 	    ok;
%% 	X ->
%% 	    Pids = string:tokens(X, " \n"),
%% 	    Processes = 
%% 		[begin
%% 		     [_, P|_] = string:tokens(Line, " "),
%% 		     {P, Line}
%% 		 end||Line<-string:tokens(os:cmd("ps -ef "), "\n")],
%% 	    Matches = 
%% 		lists:foldl(
%% 		  fun(Pid, Msg) ->
%% 			  Msg ++
%% 			      case lists:keyfind(Pid,1, Processes) of
%% 				  false -> Pid++" unknown";
%% 				  {Pid, Line} -> Line
%% 			      end
%% 		  end, "", Pids),
%% 	    case Matches of
%% 		[] -> ok;
%% 		_ -> 
%% 		    error_msg(Matches,[])
%% 	    end
%%     end.    


%%% ----------------------------------------------------------
%%% doc Removes the given mount point. The mount point is expected
%%% to be an empty directory with a well-formed basename (the
%%% leading character must be A-Za-z0-9_).
%%%
%%% TODO: Some checking was added here because apparently the
%%% function is called with weird arguments sometime, for example
%%% when running swm_ug_mod1_SUITE.
%%% end
%%% ----------------------------------------------------------

%% -spec remove_mount_point(target|simulated, string()) -> string().

%% remove_mount_point(simulated, AbsPath) ->
%%     cmd(["rm -rf ", AbsPath]);

%% remove_mount_point(target, AbsPath) ->
%%     Basename = filename:basename(AbsPath),
%%     case Basename of
%% 	"-"++_ ->
%% 	    sysInitI:warning_report(
%% 	      [{mfa, {?MODULE, remove_mount_point, [AbsPath]}},
%% 	       {decision, "ill-formed mount point name, removal not attempted"},
%% 	       {stack, erlang:get_stacktrace()}
%% 	      ]);
%% 	_ ->
%% 	    case filelib:is_dir(AbsPath) of
%% 		false ->
%% 		    case filelib:is_file(AbsPath) of
%% 			true ->
%% 			    sysInitI:warning_report(
%% 			      [{mfa, {?MODULE, remove_mount_point, [AbsPath]}},
%% 			       {decision, "mount point is a file; no action"},
%% 			       {stack, erlang:get_stacktrace()}
%% 			      ]);
%% 			false ->
%% 			    %% this case occurs when apparently the "umount"
%% 			    %% operation has both unmounted the resource and
%% 			    %% removed the mount point as well
%% 			    info_msg("mount point does not exist, "
%% 				     "nothing to remove: ~s~n",
%% 				     [AbsPath])
%% 		    end;
%% 		true ->
%% 		    remove_mount_point(AbsPath)
%% 	    end
%%     end.

%% remove_mount_point(AbsPath) ->
%%     Basename = filename:basename(AbsPath),
%%     run_swm_wrapper(["remove_mount_point ", Basename]),
%%     case os:cmd("[[ -d " ++ AbsPath ++ " ]] && ls -ld " ++ AbsPath) of
%% 	[] ->
%% 	    info_msg("mount point removed: ~s~n", [AbsPath]);
%% 	Other ->
%% 	    error_msg("mount point NOT removed:~n~s", [Other])
%%     end.


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
    HomeOther = home_dir_other(),

    Pattern = filename:join(
		 [HomeOther, "software", "RCS*", "SWM*", "swm*", "priv"]),
    [PrivDir] = filelib:wildcard(Pattern),
    D = fun(X) -> filename:dirname(X) end,
    RcsCxp = filename:join(swmLib:software_dir(),
			   filename:basename(D(D(D(PrivDir))))),
    [_, SwmLabel] = string:tokens(filename:basename(D(PrivDir)), "-"),


    FallbackPath = filename:join(HomeOther, "heart_fallback"),
    {ok, Fd} = file:open(FallbackPath, [write]),
    io:format(Fd, "cd /tmp~n", []),
    io:format(Fd,"sudo swm_wrapper.sh -c ~s -r ~s -- confirm_os -i ~s~n",
	      [RcsCxp, SwmLabel, integer_to_list(Current)]),
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

-spec run_swm_wrapper(Cmd::string()|[string()]) -> string().

run_swm_wrapper(Cmd) ->
    PrivDir = code:priv_dir(swm),
    D = fun(X) -> filename:dirname(X) end,
    CurrentCxp = D(D(D(PrivDir))),
    [_, SwmLabel] = string:tokens(filename:basename(D(PrivDir)), "-"),
    cmd(["cd /tmp ; sudo swm_wrapper.sh -c ", CurrentCxp,
	    " -r ", SwmLabel, " -- "]++Cmd).

run_swm_wrapper_res(Cmd) ->
    PrivDir = code:priv_dir(swm),
    D = fun(X) -> filename:dirname(X) end,
    CurrentCxp = D(D(D(PrivDir))),
    [_, SwmLabel] = string:tokens(filename:basename(D(PrivDir)), "-"),
    cmdreslog(["cd /tmp ; sudo swm_wrapper.sh -c ", CurrentCxp,
	       " -r ", SwmLabel, " -- "]++Cmd).

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
    logI:write_log("SwmInternal", "swmOs", info, Cmd),
    CmdR = Cmd++" ; echo -n \"Res=$?\"",
    Res = os:cmd(CmdR),
    Code = list_to_integer(lists:last(string:tokens(Res, "\n="))),
    case Code of
	0 -> 
	    logI:write_log("SwmInternal", "swmOs", info, Res);
	_ -> 
	    logI:write_log("SwmInternal", "swmOs", error, Res),
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
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%% error_msg(Format) ->
%%     error_msg(Format, []).
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
