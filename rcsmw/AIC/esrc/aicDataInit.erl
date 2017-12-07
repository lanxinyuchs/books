%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	aicDataInit.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/R10A/R11A/R12A/1
%%%
%%% @doc ==Initialization of Auto-Integration Control==
%%% This module contains the necessary initialization of auto
%%% integration control.
%%% ----------------------------------------------------------
-module(aicDataInit).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/R10A/R11A/R12A/1').
-date('2017-11-20').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% Rev     Date       Name        What
%%% -----   ---------  --------    ------------------------
%%% R1A/1   2014-02-10 etxtory     Created
%%% R1A/4   2014-04-14 etxtory     Removed printouts
%%% R2A/5   2014-05-13 etxderb     Rem activate call, called from gmfServer
%%% R2A/6   2014-05-14 etxderb     activate call back again, to be rem from
%%%                                   gmf
%%% R2A/7   2014-06-16 etxtory     Added EA (html and css)
%%% R2A/8   2014-08-15 etxtory     Added esi register
%%% R2A/9   2014-08-21 etxtory     factoryreset.html handled
%%% R2A/10  2014-10-09 etxtory     Now also copy not_allowed.html
%%% R3A/1   2014-11-07 etxjotj     HT20101 event registrations
%%% R3A/2   2014-11-24 etxtory     Added support for NL-ESI generation
%%% R3A/3   2014-01-08 etxtory     factory reset renamed to board restore
%%% R4A/    ---------- -------  ------------------------------------------------
%%% R4A/1   2015-07-07 etxberb  Changed mnesia:create_table to
%%%                             clhI:mnesia_create_table.
%%% R4A/3   2015-09-03 etxarnu  Only start aicServer on core
%%% R4A/4   2015-09-09 etxpejn  Added init_board/0
%%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% R5A/2   2016-03-03 evanbel  Added autointegration.html to the EA_HTML_FILES
%%% R5A/3   2016-04-14 evanbel  Renamed autointegration.html to aicomplete.html
%%% R6A/1   2016-06-10 evanbel  Added help.html, export.html to EA_HTML_FILES
%%% R6A/2   2016-06-14 evanbel  Bootlogs now added to esi dirs in all cases
%%% ----    ---------- -------  ------------------------------------------------
%%% R9A/1   2017-02-21 etxpeno  support for aicDb
%%% R9A/2   2017-03-06 evanbel  fixed html file list to properly include help pages
%%% ----    ---------- -------  ------------------------------------------------
%%% R10A/1  2017-05-05 etxtory  ea for vrcs 
%%% R10A/2  2017-05-11 emariad  Changed activate for "R-VNFM", activate from SEC
%%% R10A/3  2017-05-15 etxtory  Updated nl_esi_dirs (incorrect mnesia dir)
%%% R10A/4  2017-05-19 etxjotj  Moved aicModel init to post init
%%% R10A/5  2017-05-24 emariad  Removed aicModel init from post init
%%% R10A/6  2017-05-30 etxjotj  Enabled aicModel:init in post init again!
%%% ----    ---------- -------  ------------------------------------------------
%%% R11A/1  2017-08-06 uabesvi  added aicHbServer
%%% R12A/1  2017-11-20 uabesvi  added rcs/aiconf to ESI
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([instPhParallel_init/1,
	 instPhParallel_init_data/0,
	 instPhParallel_init_board/0,
	 instPhParallel_post_init/0]).

-export([children/0,
	 activate/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RmeAI.hrl").

-define(EA_CSS_FILE, "css_files.tar").

-define(EA_HTML_FILES, ["ea.html",
                        "export.html",
                        "help.html",
			"help_ea.html",
			"boardrestore.html",
			"not_allowed.html",
			"aicomplete.html"]).

-define(VRCS_EA_HTML_FILES, ["vrcs-ea.html",        %% Special version for vRCS
			     "export.html",
			     "vrcs-help_ea.html"]). %% Special version for vRCS

-define(NL_ESI_FILE, "nl_esi_dirs").

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% -type children()->                                      %#
%%%     {ok,[]}                                             %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Children supervisor specification
%%% ----------------------------------------------------------
children() ->
    {ok, case clhI:mp_role() of
	     core ->
		 [
		  {aicServer, {aicServer, start, []},
		   permanent, 1000, worker, [aicServer]},
		  {aicHbServer, {aicHbServer, start, []},
		   permanent, 1000, worker, [aicHbServer]}
		 ];
	     _ ->
		 []
	 end
    }.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% -type init()->                                          %#
%%%     ok                                                  %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Create the Mnesia tables needed for LM ECIM
%%%              model.
%%% ----------------------------------------------------------
instPhParallel_init(DbNodes) ->
    MO_Tables = [{autoProvisioning, ?autoProvisioning_types}],
    ok = create_mo_table(MO_Tables, DbNodes),
    ok = aicDb:init_tables(DbNodes).

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% -type init_data()->                                     %#
%%%     ok                                                  %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Initiate data
%%% ----------------------------------------------------------
instPhParallel_init_data() ->
    DirBL = filename:join([sysEnv:rcs_dir(), "bootlogs"]),
    ok    = logI:register_esi_dir(DirBL),
    DirAC = filename:join([sysEnv:rcs_dir(), "aiconf"]),
    ok    = logI:register_esi_dir(DirAC),
    ok    = aicDb:init_data(),
    case swmI:is_upgrade_ongoing() of
	false -> %% Initial installation
	    Opts = [{maxSize, 1}, {rotatingSegments, 3}, {public, true}],
	    logI:create_log("AiLog", Opts),

	    AP = #autoProvisioning{
		    autoProvisioningId = {"1","1","1"}},
	    Fun =
		fun() ->
			mnesia:write(AP)
		end,
	    {atomic, ok} = mnesia:transaction(Fun);
	true -> %% Upgrade
	    swmI:copy_old_table(autoProvisioning)
    end,
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% -type init_board()->                                    %#
%%%     ok                                                  %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Initiate each board in a MP cluster
%%% ----------------------------------------------------------
instPhParallel_init_board() ->
    DocRoot = sysEnv:www_doc_root(),
    PrivDir = code:priv_dir(aic),
    BinDir = filename:join([PrivDir, "bin"]),

    %% Copy html files to www_doc_root
    case sysEnv:vrcs() of
	true ->
	    FileNames = [{HtmlFile, get_html_dest_filename(HtmlFile)}
			 || HtmlFile <- ?VRCS_EA_HTML_FILES],
	    [file:copy(filename:join([BinDir, SrcFile]),
		       filename:join([DocRoot, DestFile]))
	     || {SrcFile, DestFile} <- FileNames];
	_ ->
	    [file:copy(filename:join([BinDir, HtmlFile]),
		       filename:join([DocRoot, HtmlFile]))
	     || HtmlFile <- ?EA_HTML_FILES]
    end,

    %% Copy css-file to www_doc_root, untar and remove tar-file
    CssSrc = filename:join([BinDir, ?EA_CSS_FILE]),
    CssDest = filename:join([DocRoot, ?EA_CSS_FILE]),
    file:copy(CssSrc, CssDest),
    os:cmd("tar xvf " ++ CssSrc ++ " -C " ++ DocRoot),
    file:delete(CssDest),

    %% Fetch log dirs from LOG (both for install and upgrade)
    %% and get mnesia directory. Feed these dirs to NL.
    %% This is needed in the case MW or EE/Linux crashes and
    %% falls down to networkloader without an ESI.
    EsiDirs = logI:get_reg_esi_dirs(),
    ABDir = 
	case catch swmI:get_autobackup_path() of
	    {'EXIT', _} ->
		[];
	    AutoBackupFile ->
		%% NL can only handle directories and not files
		[filename:dirname(AutoBackupFile)]
	end,

    AllDirs = EsiDirs ++ ABDir,
    AdaptedDirs = adapt_to_nl(AllDirs, []),

    NlEsiFile = filename:join([sysEnv:rcs_dir(), "networkloader", ?NL_ESI_FILE]),
    case file:write_file(NlEsiFile, term_to_binary(AdaptedDirs)) of
	ok ->
	    ok;
	Other ->
	    sysInitI:error_msg("~p: Error when writing NL dirs ~p ~n",
				   [?MODULE, Other])
    end,
    ok.

get_html_dest_filename(HtmlFile) ->
    case string:tokens(HtmlFile, "-") of
	["vrcs", FileName] ->
	    FileName;
	FileName ->
	    FileName
    end.

adapt_to_nl([], Acc) ->
    Acc;
adapt_to_nl(["/home/"++Rest | T], Acc) ->
    %% Duplicate all /home/ entries with /home1/ and /home2/
    H1 = "/home1/" ++ Rest,
    H2 = "/home2/" ++ Rest,
    adapt_to_nl(T, [H1, H2 | Acc]);
adapt_to_nl([Dir | T], Acc) ->
    %% No adaptions needed for this directory (Dir).
    adapt_to_nl(T, [Dir | Acc]).

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% -type post_init()->                                     %#
%%%     ok                                                  %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
instPhParallel_post_init()->
    aicModel:init(),
    %% TR HT20101
    comsaI:register_subscriptions(
      "RcsAI", [{"AutoProvisioning", autoProvisioning}]),
    ok.

%%% ----------------------------------------------------------
%%% -type activate()->                                      %#
%%%     ok                                                  %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
activate() ->
    case swmI:node_type() of
        "R-VNFM" ->
            ok;
        _ ->
            aicServer:activate(), %% This called from from sysApp
            ok
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           create_mo_table(Tables, DbNodes)
%%% Input:      Tables, DbNodes
%%% Output:     ok
%%% Exceptions:
%%% Description: Creates RAM copy tables for each MO class
%%% ----------------------------------------------------------
create_mo_table([], _DbNodes) ->
    ok;
create_mo_table([{Table, Types}| Rest], DbNodes) ->
    Fields = [Field || {Field, _} <- Types],
    {atomic, ok} =
	clhI:mnesia_create_table(Table, [{type, set},
					 {disc_copies, DbNodes},
					 {attributes, Fields} |
					 add_clh_option(Table)]),
    create_mo_table(Rest, DbNodes).

%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].
