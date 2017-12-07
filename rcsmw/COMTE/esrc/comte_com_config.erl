%%%-------------------------------------------------------------------
%%% @author Lukas Larsson <lukas@erlang-solutions.com>
%%% @author Raimo Niskanen
%%%
%%% @doc COM configuration module.
%%%
%%% <p><em>Option Values (Configuration Items)</em></p>
%%%
%%% <p>In the text below <b>`{Key}'</b> refers to the value of
%%% the option (configuration item) named <b>`Key'</b>.</p>
%%%
%%% <p><em>COM vs ComtE configuration matching</em></p>
%%%
%%% <p>For the ComtE application to run correctly, it is required that
%%% some application variables match the corresponding values
%%% in the COM configuration files and it needs an executable that
%%% starts COM with that configuration.</p>
%%%
%%% <p>This module generates the COM configuration files
%%% in a specified directory, and an executable shell script
%%% to use as the start program.
%%% The configuration content is mostly copied from the regular
%%% COM and ComtE installation and tweaked according to
%%% user supplied options given to the {@link install/1}
%%% function.</p>
%%%
%%% @end
%%% Created : 4 Dec 2012 by Raimo Niskanen, Erlang/OTP
%%%-------------------------------------------------------------------
-module(comte_com_config).

%% -compile(export_all).

%% API exports
-export([install/2,install/1,xml_parse_file/3,xml_transform_file/3,updateIpVersion/1]).

-include_lib("kernel/include/file.hrl").


%%%===================================================================
%%% Options and defaults
%%%===================================================================

env_keys() ->
    [com_port,
     com_ip,
     comte_port,
     comte_ip,
     com_start_mw,
     com_oam_sas].

xml_decl() ->
    <<"<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n">>.

model_cfg() ->
    "model_file_list.cfg".
com_cfg() ->
    "com.cfg".
start_com_prog() ->
    "start_com".
com_stdout_log() ->
    "com_stdout.log".

sa_prefix() ->
    "lib".
lib_suffix() ->
    ".so".
cfg_suffix() ->
    ".cfg".
xml_suffix() ->
    ".xml".
example_suffix() ->
    ".example".

com_default_models() ->
    ["ECIM_CommonLibrary",
     "ComTop",
     "ComFm",
     "ComSysM"
     %% "ComSecM",
     %% "ComLocalAuthorization",
     %% "ComLdapAuthentication"
    ].
com_default_oam_sas()->
    SP = sa_prefix(),
    [ [SP,"com_common_component"],
      [SP,"com_cli_agent"],
      [SP,"com_ext_cm_router_service"],
      [SP,"com_fm"],
      [SP,"com_fm_snmp"],
      [SP,"com_netconf_agent"],
      [SP,"com_notification_service"],
      [SP,"com_security_mgmt_service"],
      [SP,"maf_session_management_component"],
      [SP,"maf_svs"],
      {if_exists, [SP,"com_visibility_controller"],
       [ {if_exists, [SP,"cli_basic_component"],
	  [ {if_exists, [SP,"cli_legacy"], [], []} ],
	  [ [SP,"cli_basic"] ] }],
       [ [SP,"cli_basic"] ] } ].

comea_snmp_agentx_socket() ->
    "agentx-socket".
comea_snmp_pwd_file() ->
    "snmpPwdFile".



-type file_path() :: string().
%%
%% A file path.  May be relative.

-type abs_file_path() :: string().
%%
%% An absolute file path.

-type priority() :: {facility(), severity_level()}.
%%
%% A combination of a facility and a severity level
%% based on RFC 5424.
%% See the RFC for further info

-type facility() :: 0..23 | 100 | 101.
%%
%% The different facilities of COM based
%% on RFC 5424. COM extends it with alarm(100)
%% and alert(101)
%% See the RFC for further info.


-type severity_level() :: 0..8.
%%
%% The different available logging severity levels of COM.
%% A higher value also includes the logging of the values below it.
%% <ol start="0">
%%   <li>Emergency: The system is unusable.</li>
%%   <li>Alert: Action must be taken immediately.</li>
%%   <li>Critical: Critical condition.</li>
%%   <li>Error: Error condition.</li>
%%   <li>Warning: Warning condition.</li>
%%   <li>Notice: Normal but significant condition.</li>
%%   <li>Info: Informative message.</li>
%%   <li>Debug: Debug message.</li>
%%   <li>Trace: System tracing.</li>
%% </ol>

-type com_oam_config_val() ::
	atom() | integer() | string() | binary() | inet:ip_address().

-type oam_component() ::
	{Roots :: [comte_types:mo_name()],
	  {OamComponent :: string(),
	   [{IfName :: string(), Vsn :: integer()}]}} |
	{Roots :: [comte_types:mo_name()],
	  {OamComponent :: string(),
	   [{IfName :: string(), Vsn :: integer()}]},
	 Attrs :: [{Attr :: string(), Value :: com_oam_config_val()}]}.
%% An `OamComponent' and where in the MIM it should be used.
%% `Vsn' defaults to `1'.  The optional `Attrs' are generic XML
%% attributes to the `participant' tag in COM's configuration file.

-type com_oam_sa() ::
	string() |
	{if_exists, string(), [com_oam_sa()], [com_oam_sa()]} |
	{LibFilePath :: abs_file_path(), CfgFilePath :: abs_file_path()}.
%%
%% A COM OAM SA shared library either as a file basename relative
%% to the COM component directory (<b>`{com_top}'</b>`/opt/com/lib/comp')
%% where a corresponding shared object file and XML configuration
%% file is supposed to exist, or a `{LibFilePath,CfgFilePath}' tuple.
%%
%% The conditional form `{if_exists,Filename,Yes,No}' tests
%% if there exists a shared object file and XML configuration file
%% according to the previous paragraph and if so uses it and
%% processes the `Yes' list of `SA':s but if it does not exist
%% processes the `No' list of `SA':s.

-type com_oam_sa_config() ::
	{{Name :: string(), Item :: string()}, Value :: com_oam_config_val() }.
%%
%% A configuration item in the XML configuration on this form:
%% <pre>
%% &lt;component&gt;
%%   &lt;name&gt;Name&lt;/name&gt;
%%   &lt;Name&gt;
%%      &lt;Item&gt;Value&lt;/Item&gt;
%%    &lt;/Name&gt;
%% &lt;/component&gt;
%% </pre>

-type com_model() :: string() | abs_file_path().
%%
%% A COM model file name, either as a name regarded as a file basename
%% of a .xml file in the COM model directory
%% (<b>`{com_top}'</b>`/opt/com/etc/model') or as a full file
%% path to a model .xml file.

-type comte_oam_component() :: alarm_producer | cm_event_producer | pm_event_producer.
%% Available OaM components that can be toggled in ComtE

-type conf_item() ::
	{comte_lib,file_path()} |
	{comte_ip,string()|inet:ip_address()} |
	{comte_port,non_neg_integer()} |
        {comte_enable_oam_components, [comte_oam_component()]} |
        {comte_logging,boolean()} |
	{comte_logging_level,severity_level()} |
        {comte_log_force_facilities, [facility() | priority()]} |
        {com_logging_level,severity_level()} |
	{com_audit_logging,boolean()} |
	{com_audit_logging_level,severity_level()} |
	{com_ip,string()} |
	{com_port,non_neg_integer()} |
	{netconf_port,non_neg_integer()} |
	{com_top,abs_file_path()} |
	{com_conf_dir,file_path()} |
	{com_run_dir,file_path()} |
        {com_log_dir, file_path()} |
        {com_log_size, non_neg_integer()} |
        {com_log_rotation, non_neg_integer()} |
	{com_oam_components,[Component :: oam_component()]} |
	{com_default_models,[Model :: com_model()]} |
	{com_models,[Model :: com_model()]} |
	{com_default_oam_sas,[SA :: com_oam_sa()]} |
	{com_oam_sas,[SA :: com_oam_sa()]} |
	{com_oam_sas_config,[SAConfig :: com_oam_sa_config()]} |
        {com_start_mw,boolean()} |
	{ld_library_path,abs_file_path()} |
	{cli_port,non_neg_integer()} |
	{cli_welcome_message,string()} |
	{security,boolean()}.
%%
%% Install configuration items to customize the generated
%% COM configuration files.
%%
%% Configuration items below that refers to
%% {@link comte:install_lib_config/1} take their default value
%% from the corresponding ComtE application environment variable
%% to make {@link install/1} behave the same.
%% <dl>
%%    <dt><b>`comte_lib'</b></dt>
%%    <dd>Path to the ComtE shared library.  May be relative to
%%       the current working directory.  The library and a .cfg
%%       file for it will be added to the list
%%       of `SA':s as <b>`com_oam_sas'</b> below.
%%    </dd>
%%    <dt><b>`comte_ip'</b></dt>
%%    <dd>See the corresponding application environment variable
%%       documented for {@link comte:install_lib_config/1}.
%%    </dd>
%%    <dt><b>`comte_port'</b></dt>
%%    <dd>See the corresponding application environment variable
%%       documented for {@link comte:install_lib_config/1}.
%%    </dd>
%%    <dt><b>`comte_enable_oam_components'</b></dt>
%%    <dd>Toggle ComtE OaM components. The alarm- and
%%        configuration management event producer can
%%        be toggled by updating the comma separated list.
%%        The API comte:send_alarm/5 will return an error
%%        if the alarm_producer is unavailable.
%%        The API comte:notify/1 will return an error
%%        if the cm_event_producer is unavailable.
%%    </dd>
%%    <dt><b>`comte_logging'</b></dt>
%%    <dd>Whether or not log events should be sent to the
%%       callback registered by
%%       {@link comte:register_log_write_callback/1}.
%%       <br />Default: `false'.
%%    </dd>
%%    <dt><b>`comte_logging_level'</b></dt>
%%    <dd>Log events of lesser severity than this will be
%%       discarded for <b>`comte_logging'</b>.
%%       <br />Default: `6' (Info).
%%    </dd>
%%    <dt><b>`comte_log_force_facilities'</b></dt>
%%    <dd>Log facilities that unconditionally are forwarded
%%        for <b>`comte_logging'</b>. A per facility severity
%%        can optionally be provided by using the priority tuple.
%%       <br />Default: `100(Alarm),101(Alert)'.
%%    </dd>
%%    <dt><b>`com_logging_level'</b></dt>
%%    <dd>The logging level to be done by COM.  `1' is minimum logging
%%       and `8' is maximum. See {@link severity_level()}.
%%       <br />Default: `6' (Info).
%%    </dd>
%%    <dt><b>`com_audit_logging'</b></dt>
%%    <dd>Whether or not audit log events should be written
%%       to a the audit log file.
%%       <br />Default: `false'.
%%    </dd>
%%    <dt><b>`com_audit_logging_level'</b></dt>
%%    <dd>Audit log events of lesser severity than this will not
%%       be logged to the audit log file.
%%       <br />Default: `6' (Info)
%%    </dd>
%%    <dt><b>`com_ip'</b></dt>
%%    <dd>See the corresponding application environment variable
%%       documented for {@link comte:install_lib_config/1}.
%%    </dd>
%%    <dt><b>`com_port'</b></dt>
%%    <dd>See the corresponding application environment variable
%%       documented for {@link comte:install_lib_config/1}.
%%    </dd>
%%    <dt><b>`netconf_port'</b></dt>
%%    <dd>The port on which the NETCONF server should be started.
%%       <br />Default: as in the COM installation, normally `9977'.
%%    </dd>
%%    <dt><b>`com_top'</b></dt>
%%    <dd>The path to where COM is installed.
%%       <b>`{com_top}'</b>`/opt/com/bin' should be the COM binary.
%%       It will be regarded as relative to `"/"' if it is not absolute.
%%       <br />Default: `"/"'.
%%    </dd>
%%    <dt><b>`com_conf_dir'</b></dt>
%%    <dd>Where to write all generated configuration files.
%%       The path may be relative the current working directory,
%%       which is the default value.
%%    </dd>
%%    <dt><b>`com_run_dir'</b></dt>
%%    <dd>The directory in which to run the COM binary.
%%       Two logs will be created here; `com_start.log'
%%       and `com_stdout.log'.
%%       This path will be regarded as relative to
%%       <b>`{com_conf_dir}'</b> if it is not absolute, and that is
%%       also the default value.
%%    </dd>
%%    <dt><b>`com_log_dir'</b></dt>
%%    <dd>The directory in which to store the logs.
%%       At least three logs will be created here; `com.log',
%%       `com_trace.log' and `com_alarm.log'.
%%       Other logs are configurable e.g `com_audit.log'.
%%       This path will be regarded as relative to
%%       <b>`{com_run_dir}'</b> if it is not absolute, and that is
%%       also the default value.
%%    </dd>
%%    <dt><b>`com_log_size'</b></dt>
%%    <dd>The maximum size in bytes for each log file.
%%        <br />Default: 1048576 (1 MB).
%%    </dd>
%%    <dt><b>`com_log_rotation'</b></dt>
%%    <dd>The maximum number of files to keep for each log.
%%        <br />Default: 5.
%%    </dd>
%%    <dt><b>`com_start_mw'</b></dt>
%%    <dd>See the corresponding application environment variable
%%       documented for {@link comte:install_lib_config/1}.
%%    </dd>
%%    <dt><b>`com_oam_components'</b></dt>
%%    <dd>List of OAM components to write
%%       configuration for under `"ComCmRouterService"'.  Specified as
%%       a list of `Component' tuples.
%%       <br />Default:
%%       `[{[<<"ManagedElement">>],
%%          {"OamComtEComponent",
%%           [{"MafOamSpiManagedObject", 3},
%%            {"MafOamSpiTransactionalResource", 1} ] } }]'.
%%    </dd>
%%    <dt><b>`com_default_models'</b></dt>
%%    <dd>List of default `Model' names/paths.
%%       <br />Default:
%%       `["ECIM_CommonLibrary","ComTop","ComFm","ComSysM",
%%         "ComSecM","ComLocalAuthorization","ComLdapAuthentication"]'.
%%    </dd>
%%    <dt><b>`com_models'</b></dt>
%%    <dd>List of additional `Model' names/paths.
%%       <br />Default: `[]'.
%%    </dd>
%%    <dt><b>`com_default_oam_sas'</b></dt>
%%    <dd>List of default `SA':s (Support Agents), see {@link com_oam_sa()}.
%%       <br />Default: `["libcli_basic",
%%       "libcom_common_component",
%%       "libcom_cli_agent",
%%       "libcom_ext_cm_router_service",
%%       "libcom_fm",
%%       "libcom_netconf_agent",
%%       "libcom_notification_service",
%%       "libcom_security_mgmt_service"
%%       "libmaf_session_management_component",
%%       "libmaf_svs",
%%       {if_exists,"libcom_visibility_controller",
%%        [{if_exists,"libcli_basic_component",
%%          [{if_exists,"libcli_legacy",[],[]}],
%%          ["libcli_basic"]}],
%%        ["libcli_basic"]}]'.
%%    </dd>
%%    <dt><b>`com_oam_sas'</b></dt>
%%    <dd>List of additional `SA' (Support Agents):s, see {@link com_oam_sa()}.
%%       <br />Default: `[]'.
%%    </dd>
%%    <dt><b>`com_oam_sas_config'</b></dt>
%%    <dd>List of configuration variables to set in the SA's
%%      XML configuration files. See {@link com_oam_sa_config()}.
%%    </dd>
%%    <dt><b>`ld_library_path'</b></dt>
%%    <dd>The path to any special libraries needed
%%       by COM, e.g. Poco, netsnmp etc.
%%       <br />Default: <b>`{com_top}'</b>`/opt/com/lib/sdk'.
%%    </dd>
%%    <dt><b>`cli_port'</b></dt>
%%    <dd>The port on which the COM CLI server should be started.
%%       <br />Default: as in the COM installation, normally `9889'.
%%    </dd>
%%    <dt><b>`cli_welcome_message'</b></dt>
%%    <dd>The message that will be displayed when you log on to COM
%%       CLISS (CLI through SSH Subsystem).
%%       <br />Default: the long Ericsson disclaimer in the COM installation.
%%    </dd>
%%    <dt><b>`security'</b></dt>
%%    <dd>Whether to generate a `"ComSecurityManagement"'
%%       component configuration under `"services"' in `"comCfg"' or not.
%%    </dd>
%%    <dt><b>`report_interval_max_msgs'</b></dt>
%%    <dd>This is an environment variable used in module `"comte_error_logger.erl"'.
%%       <br />This variable is used to specify the maximum number of messages can be reported to 
%%       `"erlang.log"' within `"report_interval_time"'.
%%    </dd>
%%    <dt><b>`report_interval_time'</b></dt>
%%    <dd>This is an environment variable used in module `"comte_error_logger.erl"'.
%%        <br />This variable is used as time interval in which erlang errors and warnings can be 
%%         reported to `"erlang.log"'.
%%    </dd>
%%    <dt><b>`report_exception_max_term_depth'</b></dt>
%%    <dd>This is an environment variable used in module `"comte_error_logger.erl"'.
%%       <br /> The background is that the exception from application could be nested term, this 
%%        variable is used to specify hte maximum depth allowed to be reported to `"erlang.log"'.
%%    </dd>
%% </dl>

conf_val_type(Opt, Opts) ->
    case Opt of
	comte_lib -> value;
	comte_ip -> value;
	comte_port -> value;
	comte_logging -> value;
	comte_logging_level -> value;
	com_audit_logging -> value;
	com_audit_logging_level -> value;
        comte_log_force_facilities -> value;
	com_ip -> value;
	com_port -> value;
        %%comea_conf_dir -> value;
        comte_enable_oam_components -> value;
	netconf_port -> value;
	comea_snmp_agentx_socket -> value;
        comea_snmp_pwd_file -> value;
	comea_snmp_ipVersion -> {default,"ipv4"};
	com_top -> {default,"/"};
	com_conf_dir -> {default,""};
	com_run_dir -> {default,""};
        com_log_dir -> {default,""};
        com_log_size -> {default, 1048576};
        com_log_rotation -> {default, 5};
	com_oam_components ->
	    {default_fun,
	     fun () ->
		     [{[<<"ManagedElement">>],
		       {"OamComtEComponent",
			[{"MafOamSpiManagedObject", 3},
			 {"MafOamSpiTransactionalResource", 1} ]}}]
	     end};
	com_oam_sas_config -> {default,[]};
	com_default_models ->
	    {default_fun,fun com_default_models/0};
	com_models -> {default,[]};
	com_default_oam_sas -> {default,com_default_oam_sas()};
	com_oam_sas -> {default,[]};
	com_logging_level -> value;
	com_start_mw -> value;
	ld_library_path ->
	    {default_fun,
	     fun () ->
		     filename:join(conf_val(com_lib_dir, Opts), "sdk")
	     end};
	cli_port -> value;
	cli_welcome_message -> value;
	security -> {default,false};
	_ -> undefined
    end.

%% Configuration value front-ends.  Provides deduced options.
%%
conf_cond(Opt, Opts, Yes, No) ->
    try conf_val(Opt, Opts) of
	Val ->
	    Yes(Val)
    catch
	error:_ ->
	    No()
    end.
%%
conf_val(Opt, Opts) ->
    Val =
	case conf_val_type(Opt, Opts) of
	    undefined ->
		conf_val_deduced(Opt, Opts);
	    value ->
		option_value(Opt, Opts);
	    {Type,Default} when Type =:= default; Type =:= default_fun ->
		case option_is_defined(Opt, Opts) of
		    true ->
			option_value(Opt, Opts);
		    false when Type =:= default ->
			Default;
		    false when Type =:= default_fun ->
			Default()
		end
	end,
    conf_val_fix(Opt, Opts, Val).

%% Option value fixups - make path absolute, canonify component list, etc.
%%
conf_val_fix(comte_lib, _, Val) ->
    filename:absname(Val);
conf_val_fix(com_top, _, Val) ->
    filename:join("/", Val);
conf_val_fix(com_conf_dir, _, Val) ->
    Res = filename:absname(Val),
    comte:set_env(com_conf_dir, Res),
    Res;
conf_val_fix(com_run_dir, Opts, Val) ->
    filename:join(conf_val(com_conf_dir, Opts), Val);
conf_val_fix(com_log_dir, Opts, Val) ->
    filename:join(conf_val(com_run_dir, Opts), Val);
conf_val_fix(com_log_size, _Opts, Val) when is_integer(Val) ->
    integer_to_list(Val);
conf_val_fix(com_log_rotation, _Opts, Val) when is_integer(Val) ->
    integer_to_list(Val);
conf_val_fix(com_oam_components, _Opts, Val) ->
    [case Comp of
	 {Roots,CompIfList} ->
	     {Roots,CompIfList,[]};
	 {_Roots,_CompIfList,_Attrs} ->
	     Comp
     end || Comp <- Val];
conf_val_fix(com_all_models, Opts, Val) ->
    ComModelDir = conf_val(com_model_dir, Opts),
    [case filename:pathtype(Model) of
	 absolute ->
	     Model;
	 _ ->
	     filename:join(ComModelDir, [Model,xml_suffix()])
     end || Model <- Val];
conf_val_fix(com_all_oam_sas, Opts, Val) ->
    CompDir = conf_val(com_comp_dir, Opts),
    conf_val_fix_all_oam_sas(CompDir, Opts, Val);
conf_val_fix(ld_library_path, Opts, Val) ->
    conf_val(com_lib_dir, Opts)
	++ ":"
	++ filename:join(
	     conf_val(com_dir, Opts),
	     Val)
	++ ":"
	++ conf_val(com_comp_dir, Opts);
conf_val_fix(comte_log_force_facilities, _, Val) ->
    parse_integer_list(Val);
conf_val_fix(comte_enable_oam_components, _, []) ->
    [];
conf_val_fix(comte_enable_oam_components, _, Vals) ->
    CVals = lists:flatten([atom_to_list(Val)++"," || Val <- Vals]),
    string:substr(CVals, 1, string:len(CVals)-1);

%% Currently not user configurable
%% conf_val_fix(com_logging_fname, Opts, Val) ->
%%     filename:join(conf_val(com_conf_dir, Opts), Val);
%% conf_val_fix(comea_dir, Opts, Val) ->
%%     filename:join(conf_val(com_dir, Opts), Val);
conf_val_fix(_, _, Val) ->
    Val.

conf_val_fix_all_oam_sas(_CompDir, _Opts, []) ->
    [];
conf_val_fix_all_oam_sas(CompDir, Opts, [SA|SAs]) ->
    case SA of
	{if_exists,Name,Yes,No} ->
	    SharedObjectFile = filename:join(CompDir, [Name,lib_suffix()]),
	    SourceConfigFile = filename:join(CompDir, [Name,cfg_suffix()]),
	    case filelib:is_file(SharedObjectFile)
		andalso filelib:is_file(SourceConfigFile)
	    of
		true ->
		    [{SharedObjectFile,SourceConfigFile}
		     |conf_val_fix_all_oam_sas(CompDir, Opts, Yes)
		     ++ conf_val_fix_all_oam_sas(CompDir, Opts, SAs)];
		false ->
		    conf_val_fix_all_oam_sas(CompDir, Opts, No)
			++ conf_val_fix_all_oam_sas(CompDir, Opts, SAs)
	    end;
	{Lib,Cfg} -> % {SharedObjectFile,SourceConfigFile}
	    [{filename:join(CompDir, Lib),
	      filename:join(CompDir, Cfg)}
	     |conf_val_fix_all_oam_sas(CompDir, Opts, SAs)];
	Name when not is_tuple(Name) ->
	    SharedObjectFile = filename:join(CompDir, [Name,lib_suffix()]),
	    SourceConfigFile = filename:join(CompDir, [Name,cfg_suffix()]),
	    [{SharedObjectFile,SourceConfigFile}
	     |conf_val_fix_all_oam_sas(CompDir, Opts, SAs)]
    end.

parse_integer_list([]) ->
    "";
parse_integer_list(IntList) ->
   parse_integer_list(IntList, "").
parse_integer_list([], Acc) ->
     string:substr(Acc, 1, string:len(Acc)-1);
parse_integer_list([Int|Tail], Acc) when is_integer(Int) ->
    parse_integer_list(Tail, Acc++integer_to_list(Int)++",");
parse_integer_list([{Fac,Sev}|Tail], Acc) when is_integer(Fac),
                                               is_integer(Sev) ->
    PrioStr = integer_to_list(Fac)++":"++integer_to_list(Sev),
    parse_integer_list(Tail, Acc++PrioStr++",").




%%
%% Deduced configuration items; processed combinations of others.
%%
conf_val_deduced(com_dir, Opts) ->
    filename:join([conf_val(com_top, Opts),"opt","com"]);
conf_val_deduced(com_all_models, Opts) ->
    conf_val(com_default_models, Opts)
	++ conf_val(com_models, Opts);
conf_val_deduced(com_bin, Opts) ->
    filename:join([conf_val(com_dir, Opts),"bin","com"]);
conf_val_deduced(com_etc_dir, Opts) ->
    filename:join(conf_val(com_dir, Opts), "etc");
conf_val_deduced(com_cfg_src, Opts) ->
    filename:join(conf_val(com_etc_dir, Opts), com_cfg());
conf_val_deduced(com_model_dir, Opts) ->
    filename:join(conf_val(com_etc_dir, Opts), "model");
conf_val_deduced(com_lib_dir, Opts) ->
    filename:join(conf_val(com_dir, Opts), "lib");
conf_val_deduced(com_comp_dir, Opts) ->
    filename:join(conf_val(com_lib_dir, Opts), "comp");
conf_val_deduced(com_all_oam_sas, Opts) ->
    conf_val(com_default_oam_sas, Opts)
	++ conf_val(com_oam_sas, Opts)
	++
	conf_cond(
	  comte_lib, Opts,
	  fun (ComtELib) ->
		  [{ComtELib,
		    filename:join(
		      comte:template_dir(),
		      [filename:basename(ComtELib, lib_suffix()),
		       cfg_suffix()])}]
	  end,
	  fun () -> [] end);
conf_val_deduced(com_cfg, Opts) ->
    filename:join(conf_val(com_conf_dir, Opts), com_cfg());
conf_val_deduced(model_cfg, Opts) ->
    filename:join(conf_val(com_conf_dir, Opts), model_cfg());
conf_val_deduced(start_com_prog, Opts) ->
    filename:join(conf_val(com_conf_dir, Opts), start_com_prog());
conf_val_deduced(com_stdout_log, Opts) ->
    filename:join(conf_val(com_run_dir, Opts), com_stdout_log());
conf_val_deduced(comea_dir, Opts) ->
    filename:join(conf_val(com_dir, Opts), "comea");
conf_val_deduced(comea_conf_dir, Opts) ->
    filename:join(conf_val(com_conf_dir, Opts), "comea");
conf_val_deduced(comea_run_dir, Opts) ->
    filename:join(conf_val(comea_conf_dir, Opts), "run");
conf_val_deduced(comea_snmp_agentx_socket, Opts) ->
    filename:join(conf_val(comea_run_dir, Opts), comea_snmp_agentx_socket());
conf_val_deduced(comea_snmp_pwd_file, Opts) ->
    filename:join(conf_val(comea_run_dir, Opts), comea_snmp_pwd_file());
conf_val_deduced(Opt, _) ->
    erlang:error({undefined,Opt}).

example(start_com_prog) ->
    filename:join(comte:template_dir(), [start_com_prog(),example_suffix()]).

%%%===================================================================
%%% SA Config
%%%===================================================================
%%% Transformations that change values in XML configuration files
%%% into what is set in configuration items

%% Translation from XML hiearchy and event into configuation item name
%% AKA deduced event.
%%
%% libcom_cli_agent.cfg
xml_cfg(
  {characters,_},
  ["internalTCPServerPort","ComCliAgent",{name,"ComCliAgent"},
   "component"|_]) ->
    cli_port;
xml_cfg(
  {characters,_},
  ["IntroductoryMessage","ComCliAgent",{name,"ComCliAgent"},
   "component"|_]) ->
    cli_welcome_message;
%%
%% libcom_netconf_agent.cfg
xml_cfg(
  {characters,_},
  ["internalTCPServerPort","ComNetconfAgent",{name,"ComNetconfAgent"},
   "component"|_]) ->
    netconf_port;
%%
%% libcom_fm_snmp.cfg
xml_cfg(
  {characters,_},
  ["snmpMasterAgentPwdFile","ComFmSnmp",{name,"ComFmSnmp"},
   "component"|_]) ->
    comea_snmp_pwd_file;
xml_cfg(
  {characters,_},
  ["snmpMasterAgentAgentxSocket","ComFmSnmp",{name,"ComFmSnmp"},
   "component"|_]) ->
    comea_snmp_agentx_socket;

xml_cfg(
  {characters,_},
  ["ipVersion","ComFmSnmp",{name,"ComFmSnmp"},
   "component"|_]) ->
    comea_snmp_ipVersion;
%%
%% libComtE.cfg
xml_cfg(
  {characters,_},
  ["mandatory","com",{name,"MwComtEComponent"},
   "component"|_]) ->
    com_start_mw;
xml_cfg(
  {characters,_},
  ["alwaysRunning","com",{name,"MwComtEComponent"},
   "component"|_]) ->
    com_start_mw;
%% accept any internalCfg variable that matches what exists
%% in the .xml example file i.e:
%%     com_start_mw, com_port, com_ip, comte_port, comte_ip,
%%     com_audit_logging, com_audit_logging_level,
%%     comte_logging, comte_logging_level,
%%     comte_log_force_facilities or comte_enable_oam_components
xml_cfg(
  {characters,_Val},
  [ComtEOpt,"internalCfg",{name,Name},
   "component"|_])
  when Name =:= "MwComtEComponent";
       Name =:= "OamComtEComponent" ->
    try erlang:list_to_existing_atom(ComtEOpt)
    catch
	error:badarg ->
            undefined
    end;

%% xml_cfg(
%%   {endElement,_,"comte_disable_oam_components",_},
%%   ["internalCfg"|_]) ->
%%     comte_disable_oam_components;

%%
%% com.cfg
xml_cfg(
  {startElement,_,"libraries",_,_},
  ["comCfg"|_]) ->
    com_conf_dir;
xml_cfg(
  {endElement,_,"libraries",_},
  ["libraries","comCfg"|_]) ->
    com_all_oam_sas;
xml_cfg(
  {characters,_},
  ["modelSpecFile","internalCfg",{name,"MafOamSpiModelRepositoryService"},
   "component"|_]) ->
    model_cfg;
xml_cfg(
  {startElement,_,"participant",_,_},
  ["participants","internalCfg",{name,"MafCmRouterService"},
   "component","services","comCfg"|_]) ->
    participant;
xml_cfg(
  {endElement,_,"participants",_},
  ["participants","internalCfg",{name,"MafCmRouterService"},
   "component","services","comCfg"|_]) ->
    com_oam_components;
xml_cfg(
  {endElement,_,"services",_},
  ["services","comCfg"|_]) ->
    security;
%%
%% Generic component config item
xml_cfg({characters,_}, [Item,Name,{name,Name},"component"|_]) ->
    {com_oam_sas_config,Name,Item};
%%
xml_cfg(_P, _S) ->
    undefined.  %% Causes passthrough

%% Tranformation of XML events according to configuration item name
xml_cfg({characters,_}=Event, {com_oam_sas_config=Opt,Name,Item}, Opts) ->
    conf_cond(
      Opt, Opts,
      fun (CompConfig) ->
	      Key = {Name,Item},
	      case gb_trees:is_defined(Key, CompConfig) of
		  true ->
		      xmlgen_characters(gb_trees:get(Key, CompConfig));
		  false ->
		      [Event]
	      end
      end,
      fun () ->
	      [Event]
      end);
xml_cfg({characters,Characters}, Opt, Opts) ->
    xmlgen_characters_opt(Opt, Opts, Characters);
xml_cfg({startElement,_,_,_,Attributes}=Event, com_conf_dir, Opts) ->
    ConfDir = conf_val(com_conf_dir, Opts),
    Repl = list_to_gb_trees([{"configDir",ConfDir},{"dir","/"}]),
    [setelement(5, Event, xml_set_attrvals(Repl, Attributes))];

xml_cfg({startElement,_,_,_,_}, participant, _) ->
    skip;

xml_cfg({endElement,URI,_,{Prefix,_}}=Event, Opt, Opts) ->
    case Opt of
	com_all_oam_sas ->
	    ComSAs = conf_val(Opt, Opts),
	    Libs =
		[xmlgen_element(URI, Prefix, "lib", [{"name",Lib}], [])
		 || {Lib,_} <- ComSAs],
	    lists:flatten(Libs) ++ [Event];
	com_oam_components ->
	    Components = conf_val(Opt, Opts),
	    Participants =
		[xmlgen_participant(
		   URI, Prefix, Component, IfList, Roots, Attrs)
		 || {Roots,{Component,IfList},Attrs} <- Components],
	    lists:flatten(Participants) ++ [Event];
	security ->
	    lists:flatten(
	      case conf_val(Opt, Opts) of
		  true ->
		      xmlgen_component(URI, Prefix, "ComSecurityManagement");
		  false ->
		      []
	      end) ++ [Event];
	_ ->
	    [Event]
    end;


%%
xml_cfg(Event, _, _) ->
    [Event].

xmlgen_characters_opt(Opt, Opts, Characters) ->
    xmlgen_characters(
      conf_cond(
	Opt, Opts,
	fun (Val) -> Val end,
	fun () -> Characters end)).

xmlgen_characters(Characters) ->
    [{characters,to_chars(Characters)}].

xmlgen_participant(URI, Prefix, Component, IfList, Roots, Attrs) ->
    xmlgen_element(
      URI, Prefix, "participant",
      Attrs,
      [xmlgen_element(
	 URI, Prefix, "roots",
	 [],
	 [xmlgen_element(URI, Prefix, "root", [{"name",Root}], [])
	  || Root <- Roots]) |
       [xmlgen_participant_element(URI, Prefix, participant_elem(IfName),
                                   Component, IfName, IfVsn)
        || {IfName, IfVsn} <- IfList]
      ]).

participant_elem("MafOamSpiManagedObject") ->
    "managedObject";
participant_elem("MafOamSpiTransactionalResource") ->
    "txResource".

xmlgen_participant_element(URI, Prefix, Element, Component, IfName, IfVsn) ->
    xmlgen_element(
      URI, Prefix, Element,
      [{"compName",Component},{"ifName",IfName},{"version",IfVsn}],
      []).

xmlgen_component(URI, Prefix, Name) ->
    xmlgen_element(
      URI, Prefix, "component",
      [],
      [xmlgen_element(URI, Prefix, "name", [], xmlgen_characters(Name)),
       xmlgen_element(URI, Prefix, "version", [], xmlgen_characters(1)),
       xmlgen_element(
	 URI, Prefix, "com",
	 [],
	 [xmlgen_element(
	    URI, Prefix, "mandatory",
	    [],
	    xmlgen_characters(true)),
	  xmlgen_element(
	    URI, Prefix, "alwaysRunning",
	    [],
	    xmlgen_characters(false))]),
       xmlgen_element(URI, Prefix, "internalCfg", [], [])]).

xml_set_attrvals(_, []) ->
    [];
xml_set_attrvals(Repl, [{_,_,Name,_}=Attr|Attrs]) ->
    case gb_trees:is_defined(Name, Repl) of
	true ->
	    [setelement(4, Attr, gb_trees:get(Name, Repl))
	     |xml_set_attrvals(Repl, Attrs)];
	false ->
	    [Attr|xml_set_attrvals(Repl, Attrs)]
    end.

xmlgen_element(URI, Prefix, Name, Attrs, Content) ->
    [{startElement,URI,Name,{Prefix,Name},
      [{URI,Prefix,Attr,to_chars(Val)} || {Attr,Val} <- Attrs]},
     Content,
     {endElement,URI,Name,{Prefix,Name}},
     {ignorableWhitespace,"\n"}].

%% Configuration term to string
to_chars(X) when is_atom(X) ->
    atom_to_list(X);
to_chars(X) when is_integer(X) ->
    integer_to_list(X);
to_chars(X) when is_tuple(X) ->
    inet_parse:ntoa(X);
to_chars(X) when is_list(X);
                 is_binary(X) ->
    X.



%% Shell script substitutions

transform_line(<<"COMTE_COM_STDOUT_LOG=",Tail/binary>>=Bin, Opts) ->
    subst_tail(Bin, Tail, com_stdout_log, Opts);
transform_line(<<"COMTE_COM_LD_LIBRARY_PATH=",Tail/binary>>=Bin, Opts) ->
    subst_tail(Bin, Tail, ld_library_path, Opts);
transform_line(<<"COMTE_COM_RUN_DIR=",Tail/binary>>=Bin, Opts) ->
    subst_tail(Bin, Tail, com_run_dir, Opts);
transform_line(<<"COMTE_COM_BIN=",Tail/binary>>=Bin, Opts) ->
    subst_tail(Bin, Tail, com_bin, Opts);
transform_line(<<"COMTE_COM_CFG=",Tail/binary>>=Bin, Opts) ->
    subst_tail(Bin, Tail, com_cfg, Opts);
transform_line(<<"COM_LOGGING_FNAME=",Tail/binary>>=Bin, Opts) ->
    cond_subst_tail(Bin, Tail, com_logging_fname, Opts);

transform_line(<<"MAF_LOGGING_LEVEL=",Tail/binary>>=Bin, Opts) ->
    cond_subst_tail(Bin, Tail, com_logging_level, Opts);

transform_line(<<"COM_LOGGING_LEVEL=",Tail/binary>>=Bin, Opts) ->
    cond_subst_tail(Bin, Tail, com_logging_level, Opts);

transform_line(<<"COMEA_ROOT_DIR=",Tail/binary>>=Bin, Opts) ->
    cond_subst_tail(Bin, Tail, comea_conf_dir, Opts);
transform_line(<<"COMEA_CONF_DIR=",Tail/binary>>=Bin, Opts) ->
    cond_subst_tail(Bin, Tail, comea_conf_dir, Opts);
transform_line(Bin, _) ->
    Bin.

-compile({inline,[{subst_tail,4},{cond_subst_tail,4},{chop_tail,2}]}).

subst_tail(Bin, Tail, Opt, Opts) ->
    [chop_tail(Bin, Tail)|qqstr(conf_val(Opt, Opts))].

cond_subst_tail(Bin, Tail, Opt, Opts) ->
    conf_cond(
      Opt, Opts,
      fun (Val) ->
	      [chop_tail(Bin, Tail)|qqstr(to_chars(Val))]
      end,
      fun () -> Bin end).

chop_tail(Bin, Tail) ->
    binary_part(Bin, {0,size(Bin)-size(Tail)}).

qqstr(Cs) -> [$",shqq(Cs),$"].

%% Bourne shell " quoting
shqq([]) -> [];
shqq([C|Cs]) when C =:= $$; C =:= $`; C =:= $"; C =:= $\\; C =:= $\n ->
    [$\\,C|shqq(Cs)];
shqq([C|Cs]) ->
    [C|shqq(Cs)].

%%%===================================================================
%%% Option backend
%%%===================================================================

option_is_defined(Opt, Opts) ->
    gb_trees:is_defined(Opt, Opts).

option_value(Opt, Opts) ->
    gb_trees:get(Opt, Opts).

options() ->
    gb_trees:empty().

optlist_cons_options([], Opts) ->
    Opts;
optlist_cons_options([{Key,Val}|Optlist], Opts) ->
    optlist_cons_options(Optlist, gb_trees:enter(Key, Val, Opts)).

env_cons_options([], Opts) ->
    Opts;
env_cons_options([Key|Keys], Opts) ->
    env_cons_options(
      Keys,
      case comte:get_env(Key) of
	  {ok,Val} ->
	      gb_trees:enter(Key, Val, Opts);
	  undefined ->
	      Opts
      end).

list_to_gb_trees(L) ->
    gb_trees:from_orddict(orddict:from_list(L)).

%%%===================================================================
%%% API
%%%===================================================================

-type env_item() ::
	{start_com_prog,abs_file_path()} |
	{comte_ip,string()|inet:ip_address()} |
	{comte_port,non_neg_integer()} |
	{com_ip,string()} |
	{com_port,non_neg_integer()} |
	{com_start_mw,boolean()}.

-spec install(
	ConfDir :: file_path(),
	Optlist :: [conf_item()])
	     -> [env_item()].
%%
%% @doc The same as `install([{com_conf_dir,ConfDir}|Optlist])'.
install(ConfDir, Optlist) ->
    install([{com_conf_dir,ConfDir}|Optlist]).

-spec install([Optlist :: conf_item()]) -> [env_item()].
%%
%% @doc Install COM configuration files.
%%
%% Return application environment variable(s) that need to be set
%% for the application to start properly i.e <b>`start_com_prog'</b>
%% and <b>`com_conf_dir'</b>.
%% Typical use:
%% `[application:set_env(comte, E, V)
%%   || {E,V} <- comte_com_config:install(Opts)]' after loading
%% the application `comte' and before starting it.
%%
%% Generate configuration files under directory specified by
%% option <b>`com_conf_dir'</b>.
%%
%% A `model_file_list.cfg' file will be created that lists the
%% option <b>`com_default_models'</b> and the <b>`com_models'</b> models.
%%
%% The .cfg files belonging to the OAM SA:s specified by the options
%% <b>`com_default_oam_sas'</b>, <b>`com_oam_sas'</b> and <b>`comte_lib'</b>
%% are also copied to
%% the same directory, and the content of some files is changed
%% according to <b>`Optlist'</b>.
%%
%% Most of the changes are done to the option <b>`comte_lib'</b> .cfg file.
%%
%% To alter an SA configuration, use <b>`com_oam_sas_config'</b>. For
%% instance to change the connection timeout for the CLI agent, add the
%% following to the list: `{{"ComCliAgent", "connectionTimeOut"}, 60}'
%%
%% If there is a directory <b>`{com_top}'</b>`/opt/com/comea' it is
%% recursively copied to the option <b>`com_conf_dir'</b> directory
%% and environment variables in the option <b>`start_com_prog'</b>
%% script are set to use these files; to ensure write access
%% for the `comea' program when invoked from COM.
%%
%% If you e.g. want stateful alarms to be shown in you configuration model
%% you have to install the `ComFmMoImplComponent' OAM component
%% to take care of the `FmAlarm' MIM root, i.e. put
%% `{[<<"FmAlarm">>],{"ComFmMoImplComponent", [{"MafOamSpiManagedObject", 1},
%%  {"MafOamSpiTransactionalResource", 1}]}}' in
%% the <b>`com_oam_components'</b> list.
%%
install(Optlist) ->
    %% Gather given opts and merge with already defined
    %% configuration variable
    Opts_1 =
	optlist_cons_options(Optlist,
                             env_cons_options(env_keys(), options())),

    %% Gather configuration variables to set in the SA's
    %% XML configuration file.
    Opts =
	optlist_cons_options([{com_oam_sas_config,
                               list_to_gb_trees(conf_val(com_oam_sas_config,
                                                         Opts_1))}],
                             Opts_1),
    ConfDir = conf_val(com_conf_dir, Opts),

    %% Model file list to be written
    ComModels = conf_val(com_all_models, Opts),
    ModelCfg = conf_val(model_cfg, Opts),
    ok = file:write_file(ModelCfg, [[Model,"\n"] || Model <- ComModels]),

    %% Update Support Agents' configuration (XMLs)
    ComSAs = conf_val(com_all_oam_sas, Opts),
    [begin
	 ConfFile =
	     filename:join(ConfDir, filename:basename(SAConfFile)),
    	 ok = transform_cfg(SAConfFile, ConfFile, Opts)
     end || {_,SAConfFile} <- ComSAs,
	    filelib:is_regular(SAConfFile)],

    %% Update com.cfg file
    ComCfgSrc = conf_val(com_cfg_src, Opts),
    ComCfg = conf_val(com_cfg, Opts),
    ok = transform_cfg(ComCfgSrc, ComCfg, Opts),


    %% Create start script in ConfDir
    ExampleComStartProg = example(start_com_prog),
    ComStartProg = filename:join(ConfDir, start_com_prog()),
    ok =
	transform_start_script(
	  ExampleComStartProg, ComStartProg, Opts),


    %% Copy comea related files
    ComeaDir = conf_val(comea_dir, Opts),
    filelib:is_dir(ComeaDir) andalso
	file_copy_tree(
	  ComeaDir, conf_val(comea_conf_dir, Opts)),

    %% Return path to start_com script and updated
    %% configuration variables
    [{start_com_prog,ComStartProg}]
	++ install_env(env_keys(), Opts).

install_env([], _) ->
    [];
install_env([Key|Keys], Opts) ->
    %% Return the ones that differs from the app env vars
    case comte:get_env(Key) of
	{ok,Val} ->
	    case option_value(Key, Opts) of
		Val ->
		    install_env(Keys, Opts);
		NewVal ->
		    [{Key,NewVal}|install_env(Keys, Opts)]
	    end;
	undefined ->
	    case option_is_defined(Key, Opts) of
		true ->
		    [{Key,option_value(Key, Opts)}|install_env(Keys, Opts)];
		false ->
		    install_env(Keys, Opts)
	    end
    end.



-type deduced_event() ::
	cli_port |
	cli_welcome_message |
	netconf_port |
	comea_snmp_pwd_file |
	comea_snmp_agentx_socket |
	com_start_mw |
	com_port |
	com_ip |
	comte_port |
	comte_ip |
	com_conf_dir |
	com_all_oam_sas |
	model_cfg |
	participant |
	com_oam_components |
	security.

-type devent_fun() ::
	fun((Event :: atom()|tuple(),
	     Location :: {string(),string(),integer()},
	     DEvent :: deduced_event(),
	     State :: any()) -> NewState :: any()).
%% An event fun as in `xmerl_sax_parser:EventFun/3'
%% but with an inserted 3:rd argument `Cfg' that gives a deduced
%% pseudo-event that tells where a configuration item is
%% found and/or would be (re)placed.
%%
%% If the event fun returns the atom `skip', the old `State'
%% is kept and all events are skipped upto and including the matching
%% `endElement' XML tag. This only works for deduced events that
%% corresponds to `startElement' XML tags i.e:
%% <ul>
%%   <li><b>`com_conf_dir'</b></li>
%%   <li><b>`participant'</b></li>
%% </ul>

-spec xml_parse_file(
	Infile :: file_path(),
	DEventFun :: devent_fun(),
	StartState :: any()) -> FinalState :: any().
%% @doc Parse `Infile' using `xml_sax_parser' and supply a deduced
%% event to `DEventFun' that can be used to act on the same configuration
%% items in .xml files that {@link install/1} does.
%%
xml_parse_file(File, DEventFun, State) ->
    XmlParseHierState = {false,[],State},
    {ok,{_,_,FinalState},_} =
	xmerl_sax_parser:file(
	  File,
	  [{event_fun,fun_xml_parse_hier(DEventFun)},
	   {event_state,XmlParseHierState}]),
    FinalState.

-type transform_devent_fun() ::
	fun((Event :: atom()|tuple(),
	     DEvent :: deduced_event()) ->
		   [NewEvent :: atom()|tuple()] | 'skip').
%% An XML event transformation fun that takes events like
%% a `devent_fun()' and returns a transformed list of events
%% that may be empty, or the atom `skip' that will act just like for a
%% `devent_fun()'.

-spec xml_transform_file(
	Infile :: file_path(),
	Outfile :: file_path(),
	TransformFun :: transform_devent_fun()) ->
				ok | {error,atom()}.
%% @doc Parse `Infile' using `xml_sax_parser' and supply a deduced
%% event to `TransformFun' that can be used to act on the same
%% configuration items in .xml files that {@link install/1} does.
%% Writes the transformed XML document to `Outfile' and returns
%% the result of `file:write_file/2'.
xml_transform_file(Infile, Outfile, Fun) ->
    FinalDomState =
	xml_parse_file(
	  Infile,
	  fun (Event, Location, undefined, DomState) ->
		  transform_dom([Event], Location, DomState);
	      (Event, Location, DEvent, DomState) ->
		  transform_dom(Fun(Event, DEvent), Location, DomState)
	  end,
	  xmerl_sax_old_dom:initial_state()),
    Dom = xmerl_sax_old_dom:get_dom(FinalDomState),
    Content =
	[xmerl:export(Dom, xmerl_xml, [{prolog,xml_decl()}]),
	 "\n"],
    file:write_file(Outfile, unicode:characters_to_binary(Content)).

%%%===================================================================
%%% Internal Install Functions
%%%===================================================================

transform_cfg(Infile, Outfile, Opts) ->
    xml_transform_file(
      Infile, Outfile,
      fun (Event, DEvent) ->
	      xml_cfg(Event, DEvent, Opts)
      end).

%% Collect the transformed Document Object Model
transform_dom(skip, _, _) ->
    skip; % UGLY. Assumes DomState =/= 'skip'
transform_dom([], _, DomState) ->
    DomState;
transform_dom([Event|Events], Location, DomState) ->
    transform_dom(
      Events, Location,
      xmerl_sax_old_dom:event(Event, Location, DomState)).

%% Track the parse tree hierarchy
fun_xml_parse_hier(Fun) ->
    fun (Event, Location, {Skip,Hier,State}) ->
	    NewState =
		case Skip of
		    false ->
			Fun(Event, Location, xml_cfg(Event, Hier), State);
		    _ ->
			State
		end,
	    {NewSkip,NewHier} =
		case {Event,Hier} of
		    {{characters,_},
		     _} when Skip =/= false ->
			%% Prevent getting {name,_} into Hier during skip
			{Skip,Hier};
		    {{characters,Characters},
		     ["name"|_]} ->
			{Skip,[{name,Characters}|Hier]};
		    %%
		    {{startElement,_,LocalName,_,_},
		     _} when NewState =:= skip ->
			{LocalName,[{skip,LocalName}|Hier]};
		    {{startElement,_,LocalName,_,_},
		     _} ->
			{Skip,[LocalName|Hier]};
		    %%
		    {{endElement,_,Skip,_},
		     [{skip,Skip}|OldHier]} ->
			{false,OldHier};
		    %%
		    {{endElement,_,"name",_},
		     [{name,_}=Name,"name"|OldHier]} ->
		    {Skip,[Name|OldHier]};
		    {{endElement,_,LocalName,_},
		     [{name,_},LocalName|OldHier]} ->
			{Skip,OldHier};
		    {{endElement,_,LocalName,_},
		     [LocalName|OldHier]} ->
			{Skip,OldHier};
		    _ ->
			{Skip,Hier}
		end,
	    {NewSkip,NewHier,
	     case NewState of
		 skip -> State;
		 _ -> NewState
	     end}
    end.



transform_start_script(Infile, Outfile, Opts) ->
    {ok,IF} = file:open(Infile, [read,binary,{encoding,utf8}]),
    try
	{ok,OF} = file:open(Outfile, [write,{encoding,utf8}]),
	try
	    transform_start_script_loop(IF, OF, Opts)
	after
	    file:sync(OF), file:close(OF),
	    case file:read_file_info(Infile) of
		{ok,#file_info{mode=Mode}} ->
		    file:change_mode(Outfile, Mode);
		_ ->
		    ok
	    end
	end
    after
	file:close(IF)
    end.

transform_start_script_loop(IF, OF, Opts) ->
    case io:get_line(IF, "") of
	Bin when is_binary(Bin) ->
	    Size = size(Bin),
	    Size_1 = Size - 1,
	    Size_2 = Size - 2,
	    case Bin of
		<<Line:Size_2/binary,$\r,$\n>> ->
		    transform_start_script_loop(
		      IF, OF, Opts, Line, <<$\r,$\n>>);
		<<Line:Size_1/binary,EOL>>
		when EOL =:= $\n; EOL =:= $\r ->
		    transform_start_script_loop(
		      IF, OF, Opts, Line, <<EOL>>);
		_ ->
		    transform_start_script_loop(
		      IF, OF, Opts, Bin, <<>>)
	    end;
	eof ->
	    ok
    end.

transform_start_script_loop(IF, OF, Opts, Line, NL) ->
    OutLine = transform_line(Line, Opts),
    ok = io:put_chars(OF, [OutLine,NL]),
    transform_start_script_loop(IF, OF, Opts).



%% Copy a file tree a'la cp -rp (expanding symlinks)
file_copy_tree(Src, Dst) ->
    case file:read_file_info(Src) of
	{ok,#file_info{type=directory}=FileInfo} ->
	    case file:make_dir(Dst) of
		ok ->
		    file_copy_tree_dir(Src, Dst);
		{error,eexist} ->
		    file_copy_tree_dir(Src, Dst);
		{error,Reason} ->
		    erlang:error({make_dir,Dst,Reason})
	    end,
	    file:write_file_info(Dst, FileInfo);
	{ok,#file_info{type=regular}=FileInfo} ->
	    case file:copy(Src, Dst) of
		{ok,_} ->
		    file:write_file_info(Dst, FileInfo);
		{error,Reason} ->
		    erlang:error({copy,Src,Dst,Reason})
	    end;
	{ok,_} ->
	    ignore;
	{error,Reason} ->
	    erlang:error({read_file_info,Src,Reason})
    end.

file_copy_tree_dir(SrcDir, DstDir) ->
    case file:list_dir(SrcDir) of
	{ok,Files} ->
	    file_copy_tree_dir(SrcDir, DstDir, Files);
	_ ->
	    ok
    end.
%%
file_copy_tree_dir(_, _, []) ->
    ok;
file_copy_tree_dir(SrcDir, DstDir, [File|Files]) ->
    file_copy_tree(filename:join(SrcDir, File), filename:join(DstDir, File)),
    file_copy_tree_dir(SrcDir, DstDir, Files).


updateIpVersion(IPversion)->
    {ok, ConfDir} = comte:get_env(com_conf_dir),
    Src = filename:join(ConfDir, "libcom_fm_snmp.cfg"),
    Opts = list_to_gb_trees(
	     [{comea_snmp_ipVersion, IPversion},
	      {com_oam_sas_config, gb_trees:empty()}]),
    transform_cfg(Src, Src, Opts).
