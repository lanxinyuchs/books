%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysEnv.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R11A/R12A/7

%%% @doc ==System environment==
%%% This module contains access function to different system environment
%%% variables and paths. These are to be used when locating things in the
%%% file system. Paths should not be hard coded.
-module(sysEnv).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R11A/R12A/7').
-date('2017-12-05').
-author('etxarnu').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% -----      -------    --------    ------------------------
%%% R1A/1      2012-01-30 etxjotj     Created
%%% R1A/2      2012-02-10 etxlg       Added find_private_binary/2
%%% R1A/6      2012-02-23 etxpeno     Added target/0, target/1, ssit/0
%%% R1A/7      2012-03-06 etxpeno     Added dev_patches_dir/0
%%% R1A/9      2012-05-07 etxarnu     Added library_dir/0
%%% R1A/10     2012-06-20 etxlg       Support arch: powerpc-unknown-linux-gnu
%%% -----      -------    --------    ------------------------
%%% R2A/7      2013-04-19 etxarnu     Added armv7l to architecture/1
%%% R2A/8      2013-06-11 etxarnu     Added get_port_conf/2
%%% R2A/9      2013-09-25 erarafo     Added function architectureByMetaData/1
%%% R2A/10     2013-11-05 etxarnu     Added function get_uptime/0
%%% R2A/11     2013-11-05 etxarnu     Changed get_uptime/0 to return a tuple
%%%                                   with last MW restart and current time
%%% R2A/14     2014-02-19 eolaand     Add function get_initial_port_conf.
%%% R2A/15     2014-02-24 eolaand     Get cli and netconf ports from OOT.
%%% R2A/16     2014-03-17 erarafo     Added function for finding trace utils
%%% R2A/17     2014-03-18 erarafo     Dropped function for finding trace utils
%%% R2A/20     2014-05-07 etxarnu     Added proot/0
%%% R2A/21     2014-06-12 etxarnu     Updtd target_bin_dir/0 with ARMDIR handling
%%% R2A/22     2014-06-13 etxtory     Added www_doc_root/0 for www
%%% -----      -------    --------    ------------------------
%%% R3A/1      2014-12-08 etxarnu     Added role/1
%%% R3A/2-3    2014-12-08 etxarnu     Updated role/1
%%% R3A/4      2015-01-09 erarafo     Added tmp_dir/0, needed by extended FI
%%% R3A/5      2015-01-13 etxpeno     Added own_mp_id/0
%%% R3A/6      2015-01-13 etxpeno     Added get_mp_id/1, get_erlang_node/1
%%% R3A/7      2015-01-15 etxtory     Added server_root
%%% R3A/8      2015-01-27 etxpeno     Use MpId to decide role
%%% R3A/9      2015-01-29 etxarnu     Read arch from mnesia if possible
%%% R3A/10     2015-01-30 etxtory     www mv from /rcs to /home/sirpa
%%% R3A/11     2015-02-03 etxtory     Rollback didn't work for above; mv to releases
%%% -----      -------    --------    ------------------------
%%% R4A/3      2015-09-01 etxlg       Ensure tls ports are fetched from sysM
%%% R4A/4      2015-09-03 etxasta     Added www_sec_server_root, www_sec_doc_root
%%% R4A/5      2015-10-20 etxberb     Added com_mibs/0.
%%% -----      -------    --------    ------------------------
%%% R5A/1      2016-01-28 etxlg       More find_priv -stuff
%%% R5A/3      2016-02-23 etxarnu     added architectureByMetaData("i386_32")
%%% R5A/4      2016-03-12 erarafo     added find_private_binary/4
%%% -----      -------    --------    ------------------------
%%% R6A/1      2016-04-21 etxarnu     added vrcs/0
%%% R6A/3      2016-05-12 erarafo     added struct_ver/0
%%% R6A/4      2016-06-13 erarafo     Revised result type of struct_ver/0
%%% R6A/5      2016-06-22 etxarnu     Added rcs_mode_2 for vrcs
%%% -----      -------    --------    ------------------------
%%% R7A/1      2016-08-15 etxarnu     added support for aarch64 architecture
%%% R7A/2      2016-09-15 erarafo     added load_average/1 and load_average_init/1
%%% R7A/3      2016-09-21 erarafo     refactor the load_average[_init] functions
%%% R7A/4      2016-10-12 etxpeno     Hard code attribute as return value
%%%                                   in struct_ver/0
%%% R7A/5      2016-10-12 etxpeno     Revert change in R7A/4
%%% R7A/6      2016-10-18 etxpeno     Struct As Attribute
%%% R7A/7      2016-10-18 etxpeno     Revert change in R7A/6
%%% R7A/8      2016-10-18 etxpeno     Struct As Attribute
%%% -----      -------    --------    ------------------------
%%% R8A/1      2016-10-24 etxberb     Added node_type/0.
%%% R8A/2      2016-10-25 etxberb     Removed node_type/0.
%%% R8A/3      2016-11-16 etxpeno     Struct As Attribute
%%% R8A/4      2016-11-24 uabesvi     Added vnf_dir/0
%%% R8A/7      2016-12-19 eolaand     Activate vnf_dir/0 in cloud environment
%%% R8A/8      2016-12-22 etxpeno     Removed struct_ver/0
%%% -----      -------    --------    ------------------------
%%% R9A/1      2017-02-13 etxberb     Addded shared_lcm_dir/0 shared_vnfm_dir/0.
%%% -----      -------    --------   ------------------------
%%% R11A/2     2017-08-25 elarrun    Added get_computeName
%%% R11A/3     2017-09-05 etxjotj    Removed obsolete home_dir_other
%%%                                  Current version is in SWM
%%% R11A/4-6   2017-09-20 elarrun    Added get_computeName
%%% R11A/7     2017-09-21 etxarnu    Updated get_computeName and get_computeName
%%%                                  Added functions for sim32
%%% R11A/8     2017-10-17 etxberb    Adaptions to OTP20.
%%% R11A/9     2017-10-18 etxarnu    Cached value of node_type()
%%% R12A/1     2017-10-24 etxarnu    Added rcs_mode_3
%%% R12A/3     2017-11-15 etxarnu    Updated get_systemUUID
%%% R12A/4     2017-11-17 etxarnu    Use BT environment in board_type
%%% R12A/5     2017-11-22 etxarnu    rcs_mode_3 can return vrcs32
%%% R12A/6     2017-11-23 etxpeno    update of get_systemUUID/0 and get_computeName/0
%%% R12A/7     2017-12-05 etxarnu    Made get_systemUUID and get_ComputeName more forgiving
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% File structure
-export([rcs_root/0,
	 tmp_dir/0,
	 rcs_dir/0, block_dir/1,
	 shared_lcm_dir/0,
	 shared_vnfm_dir/0,
	 vnf_dir/0,
         home_dir/0,
	 dev_patches_dir/0,
	 library_root/0,
	 library_dir/0,
	 target_bin_dir/0,
	 releases_dir/0,
	 releases_vsn_dir/0,
	 releases_vsn_dir/1,
	 www_server_root/0,
	 www_sec_server_root/0,
    	 www_doc_root/0,
    	 www_sec_doc_root/0]).

-export([com_mibs/0,
	 com_top/0]).

%%% Environment
-export([rcs_mode/0,
	 rcs_mode_2/0,
	 rcs_mode_3/0,
	 target/0,
	 target/1,
	 ssit/0,
	 proot/0,
	 sim32/0,
	 role/0,
	 vrcs/0,
	 architectureByMetaData/1,
	 architecture/0,
	 architecture/1,
	 own_mp_id/0,
	 is_dus/0,
         get_systemUUID/0,
	 get_computeName/0]).
%%% Mapping between erlang node and MP id
-export([get_mp_id/1, get_erlang_node/1]).

-export([get_initial_port_conf/0]).
-export([get_initial_port_conf/1]).
-export([get_port_conf/0]).
-export([get_port_conf/1]).
-export([get_port_conf/2]).

%-export([get_uptime/0]).

%%% Hw environment
-export([board_type/0]).

%%% File structure and Environment??
-export([find_private_binary/2, find_private_binary/3, find_private_binary/4]).
-export([find_private_executable/3]). %looks in .../priv/bin

-export([load_average_init/2, load_average/2]).

-include("sys.hrl").
-include("SysWeb.hrl").

%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	try
	    sysInitI:error_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:error_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_INFO(__ReportInfo),
	try
	    sysInitI:info_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:info_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_WARN(__ReportInfo),
	try
	    sysInitI:warning_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:warning_report(?RepInfo(__ReportInfo))
	end).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(STACKTRACE_C,   % Current stacktrace
	element(2, process_info(self(), current_stacktrace))).
-define(STACKTRACE_E,   % Stacktrace at Exception
	erlang:get_stacktrace()).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-spec rcs_root() -> string().

rcs_root() ->
    getenv("RCS_ROOT").

%%% #---------------------------------------------------------
-spec tmp_dir() -> string().

tmp_dir() ->
    filename:join(rcs_root(), "tmp").

%%% #---------------------------------------------------------
-spec proot() -> boolean().

proot() ->
    os:getenv("useProot") == "yes".

%%% #---------------------------------------------------------
-spec role() -> active | standby | regular .

role() ->
    %% Hardcode roles.
    %% This must be updated when the standby role is introduced
    %% etxpeno 20150127
    case os:getenv("MPID") of
	"1" -> active;
	false -> active;
	_ -> regular
    end.

    %% case os:getenv("MP_ROLE") of
    %% 	"active" -> active;
    %% 	"standby" -> standby;
    %% 	"regular" -> regular;
    %% 	false -> active
    %% end.

%%% #---------------------------------------------------------
-spec rcs_dir() -> string().

rcs_dir() ->
    filename:join(rcs_root(), "rcs").

%%% #---------------------------------------------------------
-spec shared_lcm_dir() -> string().

shared_lcm_dir() ->
    filename:join(shared_vnfm_dir(), "lcm").

%%% #---------------------------------------------------------
-spec shared_vnfm_dir() -> string().

shared_vnfm_dir() ->
    "/shared".

%%% #---------------------------------------------------------
-spec vnf_dir() -> string().

vnf_dir() ->
    case vrcs() of
    	true ->
    	    filename:join(rcs_root(), "vnf");
    	false ->
    	    rcs_dir()
    end.


%%% #---------------------------------------------------------
-spec home_dir() -> string().

home_dir() ->
    filename:join([rcs_root(), "home", getenv("USER")]).

%%% #---------------------------------------------------------
-spec dev_patches_dir() -> string().

dev_patches_dir() ->
    getenv("DEV_PATCHES").

%%% #---------------------------------------------------------
-spec library_root() -> string().

library_root() ->
    case os:getenv("RCS_LIB_ROOT") of
	false ->
	    "";
	Env ->
	    Env
    end.

%%% #---------------------------------------------------------
-spec library_dir() -> string().

library_dir() ->
    case os:getenv("RCS_LIB_DIR") of
	false ->
	    "";
	Env ->
	    Env
    end.

%%% #---------------------------------------------------------
-spec releases_dir() -> string().

releases_dir() ->
    filename:join(home_dir(), "releases").

%%% #---------------------------------------------------------
-spec releases_vsn_dir() -> string().

releases_vsn_dir() ->
    Releases = release_handler:which_releases(),
    {value, {_, Vsn, _, permanent}} = lists:keysearch(permanent, 4, Releases),
    releases_vsn_dir(Vsn).

releases_vsn_dir(Vsn) ->
    filename:join(releases_dir(), Vsn).

%%% #---------------------------------------------------------
-spec www_server_root() -> string().

www_server_root() ->
    filename:join([releases_vsn_dir(), "www", "server_root"]).

%%% #---------------------------------------------------------
-spec www_sec_server_root() -> string().

www_sec_server_root() ->
    filename:join([releases_vsn_dir(), "www_sec", "server_root"]).


%%% #---------------------------------------------------------
-spec www_doc_root() -> string().

www_doc_root() ->
    filename:join([releases_vsn_dir(), "www", "document_root"]).

%%% #---------------------------------------------------------
-spec www_sec_doc_root() -> string().

www_sec_doc_root() ->
    filename:join([releases_vsn_dir(), "www_sec", "document_root"]).

%%% #---------------------------------------------------------
-spec block_dir(Name::string()) -> string().

block_dir(Name) ->
    filename:join(rcs_dir(), Name).

%%% #---------------------------------------------------------
-spec com_mibs() -> string().

com_mibs() ->
    filename:join(back_to_priv(com_top()), "mibs").

%%% #---------------------------------------------------------
back_to_priv(["/" | _] = SplitPath) ->
    case lists:last(SplitPath) of
	"priv" ->
	    filename:join(SplitPath);
	_ ->
	    back_to_priv(lists:droplast(SplitPath))
    end;
back_to_priv([_ | _] = Path) ->
    back_to_priv(filename:split(Path));
back_to_priv([]) ->
    [].

%%% #---------------------------------------------------------
-spec com_top() -> string().

com_top() ->
    {ok,Dir} = application:get_env(comte,com_top),
    Dir.


%%% #---------------------------------------------------------
-spec rcs_mode() -> target|simulated.

rcs_mode() ->
    case getenv("RCS_MODE") of
	"target" -> target;
	"simulated" -> simulated;
	"sim32" -> simulated;
	Unknown ->
	    sysInitI:info_msg("~p:RCS MODE unknown: ~p", [?MODULE,Unknown]),
	    target
    end.

-spec rcs_mode_2() -> target|simulated|vrcs.
rcs_mode_2() ->
        case rcs_mode() of
	    target -> target;
	    simulated ->
		case vrcs() of
		    false ->
			simulated;
		    true ->
			vrcs
		end
	end.

-spec rcs_mode_3() -> target|vrcs|vrcs32|hostsim|qemusim.

rcs_mode_3() ->
    case getenv("RCS_MODE") of
	"target" -> target;
	"simulated" ->
	    case vrcs() of
		true -> case architecture() of
			    {"i686_32",_} -> vrcs32;
			    {"x86_64",_} -> vrcs
			end;
		false -> hostsim
	    end;
	"sim32" -> qemusim
    end.


target() -> target(rcs_mode()).

target(target) -> true;
target(simulated) -> false.

ssit() -> not target().

sim32() ->
    getenv("RCS_MODE")  == "sim32".

%%% #---------------------------------------------------------
-spec vrcs() -> boolean().

vrcs() ->
    case getenv("BT") of
	"VM" -> true;
           _ -> false
    end.

%%% #---------------------------------------------------------

%% @doc Returns the internal architecture descriptor that
%% corresponds to the given metadata attribute value.
%% Metadata that carry architecture specifications are
%% described in the LMH and GMF IWDs.
%%
%% The use of "i686" and "powerpc" as a metadata attribute value is
%% deprecated.

-spec architectureByMetaData(string()) -> {string(), string()} | false.

architectureByMetaData("powerpc") ->     % deprecated
    {"powerpc", "lib32"};

architectureByMetaData("armhf") ->
    {"arm", "lib32"};

architectureByMetaData("x86_64") ->
    {"x86_64", "lib64"};

architectureByMetaData("i386") ->
    {"i686", "lib32"};

architectureByMetaData("i386_32") ->
    {"i686_32", "lib32"};

architectureByMetaData("i686") ->         % deprecated
    {"i686", "lib32"};

architectureByMetaData(_Other) ->
    false.


%%% #---------------------------------------------------------
%% @doc Returns the same as architecture(normal).

-spec architecture() -> {string(), string()}.

architecture() ->
    architecture(normal).

%%% #---------------------------------------------------------
%% @doc Returns an internal architecture descriptor that
%% reflects the actual execution architecture. If the given
%% argument is 'normal' the mapping from arch (1) output
%% to result is:
%%
%%   i686   ->  {"i686_32", "lib32"}
%%   x86_64 ->  {"i686", "lib32"}
%%   powerpc -> {"powerpc", "lib32"}
%%   ppc ->     {"powerpc", "lib32"}
%%   armv7l ->  {"arm", "lib32"}
%%
%% If the given argument is anything else than 'normal' the
%% mapping of x86_64 becomes
%%
%%   x86_64 ->  {"i686", "lib64"}
%%
%% An erlang:error/2 occurs if the execution architecture
%% cannot be mapped.
%% @end
%%-----------------------------------------------------------------------------

-spec architecture(atom()) -> {string(), string()}.

architecture(_Type) ->
    Arch =
	case catch mnesia:dirty_read(sysVariables,{arch,node()}) of
	    {'EXIT',_} ->
		os:cmd("arch");
	    [] ->
		A=os:cmd("arch"),
		mnesia:dirty_write(#sysVariables{key={arch,node()},value=A}),
		A;
	    [#sysVariables{value=A}] ->
		A
	end,
    case Arch of
	"i686\n" ->
	    case os:getenv("BT") of
		"VM" ->
		    {"i686_32","lib32"};
		_  ->
		    {"i686","lib32"} %% g2sim32, i.e. rcssim-v2 in qemu
	    end;

	"x86_64\n" ->
	    case os:getenv("BT") of
		"VM" ->
		    {"x86_64","lib64"};
		_  ->
		    {"i686","lib32"} %% 32bit on x86_64 host, i.e. old rcssim
	    end;

	Arch when Arch == "armv7l\n" orelse Arch == "aarch64\n" ->
	    {"arm","lib32"};
	Arch ->
	    erlang:error({unknown, Arch}, [])
    end.



%%% #---------------------------------------------------------
-spec get_initial_port_conf() -> {ok, [tuple()]}.

get_initial_port_conf() ->
    PortConfPath = filename:join(releases_vsn_dir(), "port.conf"),
    file:consult(PortConfPath).


%%% #---------------------------------------------------------
-spec get_initial_port_conf(atom()) -> integer().

get_initial_port_conf(Protocol) ->
    {ok, Data} = get_initial_port_conf(),
    get_port_conf(Protocol,Data).

%%% #---------------------------------------------------------
-spec get_port_conf() -> {ok, [tuple()]}.

get_port_conf() ->
    {ok, Data} = get_initial_port_conf(),
    Cli_tls_port_list =
	case lists:keyfind(port, 1, comsaI:get_cli_tls_config()) of
	    {port, Cli_tls_port} ->
		[{cli_tls, Cli_tls_port}];
	    _ ->
		[]
	end,
    Nconf_tls_port_list =
	case lists:keyfind(port, 1, comsaI:get_netconf_tls_config()) of
	    {port, Nconf_tls_port} ->
		[{netconf_tls, Nconf_tls_port}];
	    _ ->
		[]
	end,
    {ok, lists:foldl(fun({Type, _Port} = KV, Acc) ->
			     lists:keystore(Type, 1, Acc, KV)
		     end, Data, ootI:get_oap_port_conf() ++
				Cli_tls_port_list ++
				Nconf_tls_port_list)}.


%%% #---------------------------------------------------------
-spec get_port_conf(atom()) -> integer().

get_port_conf(Protocol) ->
    {ok, Data} = get_port_conf(),
    get_port_conf(Protocol,Data).

%%% #---------------------------------------------------------
-spec get_port_conf(atom(), [tuple()]) -> integer().

get_port_conf(Protocol,Data) ->
    {value, {_, Value}} = lists:keysearch(Protocol, 1, Data),
    Value.

%%-----------------------------------------------------------------------------
%% @doc Returns the full filepath to File, by searching first in patches
%%      directory, then in applications specific private bin dir.
%%      If not found the default is returned.
%% @end
%%-----------------------------------------------------------------------------
-spec find_private_executable(Application::atom(), File::string(), Default::any()) -> any().
find_private_executable(Application, File, Default) ->
    check_patches_first(Application, "bin", File, Default).
%%-----------------------------------------------------------------------------
%% @doc Returns the full filepath to File, by searching first in patches
%%      directory, then in applications architecture specific private dir.
%%      If not found the default is returned.
%% @end
%%-----------------------------------------------------------------------------
-spec find_private_binary(File::string(), Default::string()) ->
				 string() | no_return().
find_private_binary(File, Default) ->
    case application:get_application() of
	{ok, Application} ->
	    find_private_binary(Application, File, Default);
	_ ->
	    erlang:error("Called outside of application")
    end.
%%-----------------------------------------------------------------------------
%% @doc Returns the full filepath to File, by searching first in patches
%%      directory, then in applications architecture specific private dir.
%%      If not found the default is returned.
%% @end
%%-----------------------------------------------------------------------------
-spec find_private_binary(Application::atom(), File::string(), Default::string()) ->
				 string() | no_return().
find_private_binary(Application, File, Default) ->
    check_patches_first(Application, target_bin_dir(), File, Default).

%%-----------------------------------------------------------------------------
%% @doc Same as find_private_binary/3, for use when vc_state is known.
%% @end
%%-----------------------------------------------------------------------------
-spec find_private_binary(Application::atom(), File::string(), Default::string(),
			  VcState::vc_state()) ->
				 string() | no_return().
find_private_binary(Application, File, Default, VcState) ->
    check_patches_first_vc(Application, target_bin_dir(), File, Default,
			   VcState).

%%-----------------------------------------------------------------------------
%% @doc Returns the current boardtype as an atom
%% @end
%%-----------------------------------------------------------------------------

board_type() ->
    case rcs_mode() of
	simulated ->
	    simulated;
	target ->
	    list_to_atom(os:getenv("BT"))
    end.

%%-----------------------------------------------------------------------------
%% @doc Returns the MP identifier
%% @end
%%-----------------------------------------------------------------------------
-spec own_mp_id() -> integer().
own_mp_id() ->
    case os:getenv("MPID") of
	false ->
	    1;
	MpId ->
	    list_to_integer(MpId)
    end.

-spec get_mp_id(atom()) -> integer().
get_mp_id(ErlangNode) ->
    {match, [Id]} = re:run(atom_to_list(ErlangNode),"du([0-9]+)",
			   [{capture, [1], list}]),
    list_to_integer(Id).

-spec get_erlang_node(integer()) -> atom().
get_erlang_node(MpId) ->
    Sname = "du"++integer_to_list(MpId),
    list_to_atom(re:replace(atom_to_list(node()), "du[0-9]+", Sname,
			    [{return,list}])).

%%% #---------------------------------------------------------
-spec is_dus() -> boolean().
is_dus() -> is_dus(board_type()).

is_dus(dus32) -> true;
is_dus(dus52) -> true;
is_dus(_)     -> false.

%%-----------------------------------------------------------------------------
%% @doc Returns the SystemUUID
%% @end
%%-----------------------------------------------------------------------------
-spec get_systemUUID() -> string().
get_systemUUID() ->
    case type_of_node() of
	simulated->
	    UuidFile = filename:join([ rcs_root(),"rcs","sim_uuid" ]),
	    case file:read_file(UuidFile) of
		{ok,Bin} ->
		    [Uuid|_] = string:tokens(binary_to_list(Bin), "\n"),
		    Uuid;
		_ ->
		    ""
	    end;

	Node when Node  == "vRC";
		  Node  == "vPP";
		  Node  == "vSD" ->
	    case os:getenv("VNFC_RESOURCE_ID") of
		false -> read_cloud_data(); %If running without VNFM
		Uuid -> Uuid
	    end;

	"R-VNFM"  ->
	    read_cloud_data();

	Node when Node == "RadioNode";
		  Node == "RadioTNode" ->
	    read_target_uuid();

	_Node  -> %Unknown node type, try generic
	    case rcs_mode() of
		target ->
		    read_target_uuid();
		_ ->
		    read_cloud_data()
	    end
    end.

read_target_uuid() ->
    case  os:getenv("SYS_UUID")  of
	false -> "";
	Uuid -> Uuid
    end.
    
read_cloud_data() ->
    case file:read_file("/var/lib/cloud/data/instance-id") of
	{ok,Bin} ->
	    [Uuid|_] = string:tokens(binary_to_list(Bin), "\n"),
	    Uuid;
	_ ->
	    ""
    end.

%%-----------------------------------------------------------------------------
%% @doc Returns COMPUTE_NAME
%% @end
%%-----------------------------------------------------------------------------
-spec get_computeName() -> string().
get_computeName() ->
    case type_of_node() of
	simulated->
	    "RadioNode";

	Node when Node  == "vRC";
		  Node  == "vPP";
		  Node  == "vSD" ->
	    case os:getenv("VNFC_NAME") of
		false -> Node;
		ComputeName -> ComputeName
	    end;

	Node  ->
	    Node
    end.




%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @doc Returns the architecture specific path element used in the path
%%	to binaries under block private directory.
%%	exit() if called on not supported architecture
%%      For ARM a env variable might be set to point out different versions
%% @end
%%-----------------------------------------------------------------------------
-spec target_bin_dir() -> string() | no_return().
target_bin_dir() ->
    Ark =
	case {architecture(),os:getenv("ARMDIR")} of
	    {{Arch,_}, false} ->
		Arch;
	    {{"arm",_}, Dir} ->
	       Dir
	end,
    filename:join(["tgt_"++Ark, "bin"]).


-define(LOAD_AVERAGE_DATA_KEY, load_average_456598645986396).
-define(MS_PER_TICK, 10).

%%-----------------------------------------------------------------------------
%% @doc Initializes for use of load_average/1. The given Tag must be used
%% by subsequent calls to load_average/1. Multiple tags can be used if
%% separate series of interleaved samples are needed.
%% @end
%%-----------------------------------------------------------------------------
-spec load_average_init(atom(), orddict:orddict()) -> orddict:orddict().

load_average_init(Tag, Dict) ->
    NewData = get_stat_info(true),
    orddict:store(Tag, NewData, Dict).


%%-----------------------------------------------------------------------------
%% @doc Computes a load average since the last call, or since initialization.
%% The result is a 5-tuple containing
%%
%%   Load average in per mille
%%   Milliseconds spent in IO wait and IRQ servicing
%%   Milliseconds elapsed since previous call
%%   Number of CPUs
%%   Sanity indicator
%%
%% A load average value of 1000 means that all CPUs are busy all the time.
%%
%% The "IO wait and IRQ servicing" time is a sum over all CPUs.
%%
%% The sanity indicator is a millisecond value that is expected to be close
%% to zero. It indicates that the time reported by
%% erlang:monotonic_time(milli_seconds) and the time accounted for in
%% /proc/stat are in agreement.
%%
%% The sysTestServer module provides a convenient interface for using
%% this function.
%% @end
%%-----------------------------------------------------------------------------
-spec load_average(atom(), orddict:orddict()) ->
	  {{integer(), integer(), integer(), integer(), integer()},
	   orddict:orddict()}.

load_average(Tag, Dict) ->
    % get old data
    case orddict:find(Tag, Dict) of
	error ->
	    {{-1, -1, -1, -1, -1}, Dict};
	{_, {OldClockMillis, OldNcpu, OldUser, OldNice, OldSys,
	     OldIdle, OldIoWait, OldIrq, OldSirq}} ->
	    {Now, _, NewUser, NewNice, NewSys,
	     NewIdle, NewIoWait, NewIrq, NewSirq} =
		get_stat_info(false),
	    % store new data
	    NewDict =
		orddict:store(
		  Tag,
		  {Now, OldNcpu, NewUser, NewNice, NewSys,
		   NewIdle, NewIoWait, NewIrq, NewSirq},
		  Dict),
	    % report result
	    ElapsedMillis = Now - OldClockMillis,
	    DeltaBusy =
		?MS_PER_TICK*(NewUser - OldUser +
				  NewNice - OldNice + NewSys - OldSys),
	    DeltaIdle = ?MS_PER_TICK*(NewIdle - OldIdle),
	    DeltaWait = ?MS_PER_TICK*(NewIoWait - OldIoWait +
					  NewIrq - OldIrq + NewSirq - OldSirq),
	    PerMille = min(1000, (DeltaBusy*1000) div (OldNcpu*ElapsedMillis)),
	    SanityCheck = DeltaBusy + DeltaIdle + DeltaWait -
			      OldNcpu*ElapsedMillis,
	    {{PerMille, DeltaWait, ElapsedMillis, OldNcpu, SanityCheck},
	     NewDict}
    end.


%%-----------------------------------------------------------------------------
%% @doc Reads /proc/stat and returns a tuple
%%      {NOW, NCPU, USER, NICE, SYS, IDLE, IOWAIT, IRQ, SOFTIRQ}
%%
%% The NCPU entry is valid only when Initial is 'true'.
%% @end
%%-----------------------------------------------------------------------------
-spec get_stat_info(boolean()) -> tuple().

get_stat_info(Initial) ->
    Now = erlang:monotonic_time(milli_seconds),
    case file:open("/proc/stat", [read]) of
	{error, _} ->
	    {Now, -11, 0, 0, 0, 0, 0, 0, 0};
	{ok, Stream} ->
	    case io:get_line(Stream, "") of
		eof ->
		    file:close(Stream),
		    {Now, -12, 0, 0, 0, 0, 0, 0, 0};
		{error, _} ->
		    file:close(Stream),
		    {Now, -13, 0, 0, 0, 0, 0, 0, 0};
		Line1 ->
		    case re:run(Line1, "cpu\s\s([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) .*", [anchored]) of
			nomatch ->
			    {0, 0, 0, 0, 0, 0, 0, 0};
			{match, [{_, _}, {A, AL}, {B, BL}, {C, CL}, {D, DL}, {E, EL}, {F, FL}, {G, GL}]} ->
			    User = list_to_integer(string:substr(Line1, A+1, AL)),
			    Nice = list_to_integer(string:substr(Line1, B+1, BL)),
			    Sys = list_to_integer(string:substr(Line1, C+1, CL)),
			    Idle = list_to_integer(string:substr(Line1, D+1, DL)),
			    IoWait = list_to_integer(string:substr(Line1, E+1, EL)),
			    Irq = list_to_integer(string:substr(Line1, F+1, FL)),
			    Sirq = list_to_integer(string:substr(Line1, G+1, GL)),
			    Ncpu = get_stat_info_ncpu(Stream, "-99", Initial),
			    {Now, Ncpu, User, Nice, Sys, Idle, IoWait, Irq, Sirq}
		    end
	    end
    end.


%%-----------------------------------------------------------------------------
%% @doc If the 3rd argument is false close the stream immediately and return
%% -1. Otherwise keep reading and find out the number of CPUs, and close the
%% stream.
%% @end
%%-----------------------------------------------------------------------------
-spec get_stat_info_ncpu(file:io_device(), string(), boolean()) -> integer().

get_stat_info_ncpu(Stream, _, false) ->
    file:close(Stream),
    -1;

get_stat_info_ncpu(Stream, R, true) ->
    case io:get_line(Stream, "") of
	eof ->
	    file:close(Stream),
	    -2;
	{error, _} ->
	    file:close(Stream),
	    -3;
	Line ->
	    case re:run(Line, "cpu([0-9]+)\s.*", [anchored]) of
		nomatch ->
		    file:close(Stream),
		    1 + list_to_integer(R);
		{match, [{_, _}, {P, L}]} ->
		    get_stat_info_ncpu(Stream, string:substr(Line, P+1, L), true)
	    end
    end.


%%% ----------------------------------------------------------
%%% #           getenv(Env)
%%% Input: Env:string()
%%% Output:
%%% Exceptions:
%%% Description: string()
%%% ----------------------------------------------------------

getenv(Env) ->
    try os:getenv(Env) of
	false ->
	    ?LOG_ERR([{unknown_env, Env}]),
	    erlang:error(unknown_env,[Env]);
	Result ->
	    Result
    catch
	ErrClass : ErrReason ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([{env, Env},
		      {ErrClass, ErrReason},
		      {callstack, Stacktrace}]),
	    erlang:ErrClass(ErrReason)
    end.

%%-----------------------------------------------------------------------------
%% @doc Same as check_patches_first/4, for use when vc_state is known.
%% @end
%%-----------------------------------------------------------------------------
check_patches_first_vc(Application, Relpath, File, Default, VcState) ->
    case code:priv_dir(Application) of
	{error, _} ->
	    Default;
	Path ->
	    Full_path = filename:join([Path, Relpath]),
	    File_paths =
		case sysInitI:is_secure(VcState) of
		    true ->
			[filename:join([Full_path, File])];
		    false ->
			[filename:join([dev_patches_dir(), File]),
			 filename:join([Full_path, File])]
		end,
	    check_patches_first(File_paths, Default)
    end.

%%% ----------------------------------------------------------
%%% #           check_patches_first(Application, Relpath, File, Default)
%%% Input: (Application::atom(), Relpath::string(), File::string(), Default::any()) -> any().
%%% Output: Filepath::string() | Default::any()
%%% Exceptions:
%%% Description: string()
%%% ----------------------------------------------------------
check_patches_first(Application, Relpath, File, Default) ->
    case code:priv_dir(Application) of
	{error, _} ->
	    Default;
	Path ->
	    Full_path = filename:join([Path, Relpath]),
	    check_patches_first(Full_path, File, Default)
    end.
check_patches_first(Path, File, Default) ->
    File_paths =
	case sysInitI:is_secure() of
	    true ->
		[filename:join([Path, File])];
	    false ->
		[filename:join([dev_patches_dir(), File]),
		 filename:join([Path, File])]
	end,
    check_patches_first(File_paths, Default).
check_patches_first([], Default) ->
    Default;
check_patches_first([H | T], Default) ->
    case file:read_file_info(H) of
        {ok, _} ->
            H;
        {error, _} ->
	    check_patches_first(T, Default)
    end.


%%% ----------------------------------------------------------
%%% #   type_of_node()
%%% Input: -
%%% Output: simulated | string()
%%% Exceptions:
%%% Description: Returns the node type or simulated
%%% ----------------------------------------------------------


type_of_node() ->
    case get(sys_node_type) of
	undefined ->
	    R=
		case rcs_mode_2() of
		    simulated ->
			simulated;
		    _ ->
			proplists:get_value(type,
					    swmI:get_current_up_metadata())
		end,
	    put(sys_node_type,R),
	    R;
	NT ->
	    NT
    end.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
