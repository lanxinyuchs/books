%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfImmUg.hrl %
%%% Author:	erarafo
%%% Description:
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/1').
-hrl_date('2017-01-04').
-hrl_author('ecaiyan').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R2A/1      2013-05-28 erarafo     Created
%%% R2A/5      2013-06-16 erarafo     ICTI support
%%% R2A/6      2013-06-17 erarafo     added type rdn()
%%% R2A/7      2013-06-19 erarafo     added #oldClassInfo{}
%%% R2A/8      2013-06-27 erarafo     parentName() type adjusted
%%% R2A/9      2013-06-28 erarafo     improved log macros
%%% R2A/10     2013-07-01 erarafo     dialyzer complaint workaround
%%% R2A/11     2013-09-16 erarafo     support for pre-restart schema check
%%% R2A/12     2013-09-17 erarafo     added a 'path' field in schema
%%% R2A/13     2013-09-19 erarafo     eliminated the #schema{} record
%%% R2A/14     2013-09-27 erarafo     Adapted to #ldap{} change in safs_imm_db
%%% R2A/15     2013-10-03 erarafo     Added macro for CRITICAL messages
%%% R2A/16     2013-10-21 erarafo     Function calls replace inlining in macros
%%% R2A/17     2013-10-22 erarafo     Added WARNING macro
%%% R2A/18     2013-10-24 erarafo     Solving the log MO instances problem
%%% R2A/19     2013-10-29 erarafo     Cleanup
%%% R2A/20     2013-11-12 erarafo     Adapted to changes in SAFs record
%%%                                     definitions (#ldap{}, #imm_class{})
%%% R2A/21     2013-11-14 erarafo     Refactoring
%%% R2A/22     2014-01-13 erarafo     Use safs_imm_db:get_imm_objects_payload/1
%%% R2A/23     2014-01-17 erarafo     CRITICAL -> Erlang log ERROR REPORT
%%% R2A/24     2014-03-06 erarafo     RESTART_COLD macro added
%%% R2A/25     2014-05-05 erarafo     Revised error handling
%%% R2A/26     2014-07-01 erarafo     Comments update
%%% R2A/27     2014-07-01 erarafo     FAULT macro adjusted
%%% R2A/28     2014-07-31 etxberb     Corrected cause value for AVLI
%%% R2A/29     2014-08-05 erarafo     Comments update
%%% R2A/30     2014-08-06 erarafo     Using erlang:orddict() properly
%%% R2A/31     2014-08-14 erarafo     Performance lift related to HS84194
%%% R3A/1      2014-11-06 erarafo     Persistent runtime attributes
%%% R3A/2      2014-11-11 erarafo     Handle writeInstances when pers runtime
%%% R3A/3      2015-02-11 erarafo     Prepare for change of SAF IMM repr
%%% R3A/4      2015-03-04 erarafo     Extensions to types and macros
%%% R3A/5      2015-03-17 erarafo     Constant RESERVED_BY added
%%% R3A/6      2015-03-31 erarafo     Constant MAX_STRLEN added
%%% R3A/7      2015-04-01 erarafo     Trivial change
%%% R3A/8      2015-04-22 erarafo     restoredInstance.implementer added
%%% R3A/9      2015-04-23 erarafo     instanceGroup.implementer added
%%% R4A/1      2015-04-29 erarafo     instanceGroup split into specialized types
%%% R4A/2      2015-05-19 erarafo     New insertion algorithm
%%% R4A/3      2015-05-26 erarafo     Transparent adjustment
%%% R4A/4      2015-05-28 erarafo     Added an 'exception' record type
%%% R4A/5      2015-06-01 erarafo     Stacktrace is default with exceptions
%%% R4A/6      2015-06-04 erarafo     Support for SAF IMM error string
%%% R4A/7      2015-06-09 erarafo     One type spec removed
%%% R4A/8      2015-06-10 erarafo     Redundant definitions removed
%%% R4A/9      2015-07-04 erarafo     More redundant definitions removed
%%% R6A/1      2016-05-13 erarafo     Provisional addition of safsImmCsStruct
%%% R6A/2      2016-05-18 erarafo     Added type indicator IMM_STRUCT
%%% R6A/3      2016-06-07 erarafo     Sharpened the restoredValues() type
%%% R6A/4      2016-06-15 etxpeno     Removed safsImmCsStruct
%%% R8A/1      2017-01-04 ecaiyan     Added SA_ERR_UNAVAILABLE

-include("safs_imm_ct_p.hrl").

%%% duplicated from safs_log.hrl
-define(SA_OK,                   sa_ais_ok).
-define(SA_ERR_LIBRARY,          sa_ais_err_library).
-define(SA_ERR_TIMEOUT,          sa_ais_err_timeout).
-define(SA_ERR_TRY_AGAIN,        sa_ais_err_try_again).
-define(SA_ERR_BAD_HANDLE,       sa_ais_err_bad_handle).
-define(SA_ERR_BAD_FLAG,         sa_ais_err_bad_flag).
-define(SA_ERR_BAD_OPERATION,    sa_ais_err_bad_operation).
-define(SA_ERR_FAILED_OPERATION, sa_ais_err_failed_operation).
-define(SA_ERR_INIT,             sa_ais_err_init).
-define(SA_ERR_INVALID_PARAM,    sa_ais_err_invalid_param).
-define(SA_ERR_NO_RESOURCES,     sa_ais_err_no_resources).
-define(SA_ERR_NO_MEMORY,        sa_ais_err_no_memory).
-define(SA_ERR_NOT_EXIST,        sa_ais_err_not_exist).
-define(SA_ERR_EXIST,            sa_ais_err_exist).
-define(SA_ERR_VERSION,          sa_ais_err_version).
-define(SA_ERR_NO_OP,            sa_ais_err_no_op).
-define(SA_ERR_NOT_SUPPORTED,    sa_ais_err_not_supported).
-define(SA_ERR_BUSY,             sa_ais_err_busy).
-define(SA_ERR_UNAVAILABLE,      sa_ais_err_unavailable).


%% The SAFS version that we expect the CS to offer.
-define(SAFS_VERSION,
    #safsVersion{releaseCode = $A,
			     majorVersion = 2,
			     minorVersion = 11}).


%% Instances of these classes should be copied automatically
%% at start of upgrade. SaLogStream is actually a SA_RUNTIME
%% object and will not be copied because of this.
-define(LOG_CLASSES, [<<"SaAmfApplication">>, <<"SaLogStreamConfig">>, <<"SaLogStream">>]).


%% Instances of these classes should be fetched from the
%% From-state dump even if "replay" is requested.
-define(USE_RECENT_INSTANCES, [<<"OamAccessPoint">>]).


%% The attribute name used in bidirectional references.
-define(RESERVED_BY, <<"reservedBy">>).


%% This value is coupled to the ~160p format used in many printouts;
% the rule is MAX_STRLEN = 160-4.
-define(MAX_STRLEN, 160-4).


%% General macro for 'info' log messages.

-define(INFO(FORMAT, DATA),
	gmfImmUgLib:log(?MODULE, ?LINE, info, FORMAT, DATA)).


%% General macro for 'warning' log messages.

-define(WARNING(FORMAT, DATA),
	gmfImmUgLib:log(?MODULE, ?LINE, warning, FORMAT, DATA)).


%% Handle a fault. If 'error' is among the options a simple
%% erlang:error/1 call is made. Otherwise a message and a stack
%% trace is written to the SwmInternal log (the stack trace is
%% optional), and an ERROR REPORT is written to the Erlang log,
%% and a cold restart is optionally requested.
%%
%% The 'source' option, if given, can be one of 'auto', 'copy',
%% 'read' or 'write'.

-define(FAULT(OPTIONS, FORMAT, DATA),
	gmfImmUgLib:handleFault(OPTIONS,
				?MODULE,
				?LINE,
				FORMAT,
				DATA,
				erlang:get_stacktrace())).

%% The meaning of arguments is the same as for ?FAULT. Use this
%% for identical diagnostics that may be issued a great number of
%% times.  TODO, no users yet?
-define(MFAULT(OPTIONS, FORMAT, DATA),
	gmfImmUgScoreboard:xDiagnostic(
	  OPTIONS, ?MODULE, ?LINE, FORMAT, DATA)).


-define(AVLI_Cause_UpgradeFailure, "UpgradeFailure").

-define(CALL_TIMEOUT, infinity).
-define(CALL(SERVER_REF, MESSAGE), gen_server:call(SERVER_REF, MESSAGE, ?CALL_TIMEOUT)).



% A specialization of orddict:orddict().
-type sortedMap() :: [{non_neg_integer(), term()}].


% Used by the master for identifying clients.
-type handle()  :: pos_integer().


%% The three sources of actions where instances are written to the new
%% database.

% experimental: allow 'undefined' as well
-type source() :: auto | copy | write.

-type categDefined() :: s0 | s1v0 | s1v1 | s1v2 | s1v3.
-type category()     :: categDefined() | none.
-type immMode()      :: config|persistentRuntime.
-type classNames()   :: [className()].
-type schemaName()   :: binary().
-type attrName()     :: binary().
-type parentName()   :: binary() | undefined.  % like this, <<"a=aaaa,b=bbb">>, in IMM order
-type rdn()          :: binary().              % like this, <<"abc=1">>, used by MIM instances

-type implementer()  :: binary()|undefined.    % OI object implementer


%% -----------------------------------------------------------------
%% TODO, table manipulation the IMM way, should somehow be imported
%% rather than duplicated?
%%
%% Importing from "safs_imm_db.hrl" does not seem possible. Also the
%% getSchema function is duplicated code, stolen from
%% safs_imm_db:get_schema/1.
%% TODO, reuse the function in safs_imm_db?

-type immRdnType() ::
	  sa_imm_attr_sastringt.

-type immAttrType() ::
	  sa_imm_attr_saint32t |
	  sa_imm_attr_saint64t |
	  sa_imm_attr_sauint32t |
	  sa_imm_attr_sauint64t |
	  sa_imm_attr_satimet |
	  sa_imm_attr_sastringt |
	  sa_imm_attr_sanamet |
	  sa_imm_attr_sadoublet |
	  sa_imm_attr_safloatt |
	  sa_imm_attr_saanyt.

-define(IMM_INT32, sa_imm_attr_saint32t).
-define(IMM_INT64, sa_imm_attr_saint64t).
-define(IMM_UINT32, sa_imm_attr_sauint32t).
-define(IMM_UINT64, sa_imm_attr_sauint64t).
-define(IMM_TIME, sa_imm_attr_satimet).
-define(IMM_STRING, sa_imm_attr_sastringt).
-define(IMM_NAME, sa_imm_attr_sanamet).
-define(IMM_STRUCT, sa_imm_attr_csstructt).
-define(IMM_DOUBLE, sa_imm_attr_sadoublet).
-define(IMM_FLOAT, sa_imm_attr_safloatt).
-define(IMM_ANY, sa_imm_attr_saanyt).




%% Gleaned from safs_imm_xml.erl
-type imm_attr_flag()  :: multi_value|writable|cached|persistent|no_dangling|notify|no_duplicates.
-type imm_rdn_flag()   :: imm_attr_flag()|initialized.


%% #imm_attr{}: The 'default' field is 'undefined' or a specific default value.
%% TODO, what is the meaning of the 'values' field?

-record(imm_attr,
	{name        :: atom(),
	 type        :: immAttrType(),
	 category    :: config|runtime,
	 flags=[]    :: [imm_attr_flag()],
	 default     :: any(),
	 values=[]
	}).

%% Example:
%%
%% #imm_attr{
%%    name=mustHave,
%%    type=sa_imm_attr_sanamet,
%%    category=config,
%%    flags=[writable,notify,no_dangling],
%%    default=undefined,
%%    values=[]},


-record(imm_rdn,
	{name        :: atom(),
	 type        :: sa_imm_attr_sastringt,
	 category    :: config|runtime,
	 flags=[]    :: [imm_rdn_flag()]
	}).

%% Example:
%%
%% #imm_rdn{
%%     name=flemingId,
%%     type=sa_imm_attr_sastringt,
%%     category=config,
%%     flags=[initialized,notify]},

-record(imm_class, {
		    name          :: atom(),
		    rdn           :: #imm_rdn{},
		    category      :: config|runtime,
		    attrs=[]      :: [#imm_attr{}],
		    oi,
		    oi_appliers=[]
		   }).

%% Examples:
%%
%% #imm_class{
%%     name='NOBLEAstatine',
%%     rdn={imm_rdn,astatineId,sa_imm_attr_sastringt,runtime,[cached]},
%%     category=runtime,
%%     attrs=[{imm_attr,volatileBird,sa_imm_attr_sastringt,runtime,[cached],undefined,[]}],
%%     oi=undefined,
%%     oi_appliers=[]}
%%
%% #imm_class{
%%     name='NOBLEFleming',
%%     rdn={imm_rdn,flemingId,sa_imm_attr_sastringt,config,[initialized,notify]},
%%     category=config,
%%     attrs=[{imm_attr,reservedBy,sa_imm_attr_sanamet,config,[writable,multi_value],undefined,[]},
%%            {imm_attr,mustHave,sa_imm_attr_sanamet,config,[writable,notify,no_dangling],undefined,[]},
%%            {imm_attr,age,sa_imm_attr_saint32t,config,[writable,notify],undefined,[]}
%%           ],
%%     oi=undefined,
%%     oi_appliers=[]}


%% Instance as restored from the old-version dump.
%%
%% The key is a list of binaries of the form <<"abc=def">>, where "abc"
%% is the RDN name as specified in the class description (the imm_class
%% table). The order is top-to-bottom.
%%
%% Attribute descriptors are 2-tuples of atomic attribute name
%% and the atom '$imm_runtime' or a list of values. Value types are
%% integer, binarized string, struct, and possibly others. There are
%% no explicit type indicators; type info has to be looked up in the
%% class definition.
%%
%% WP5493: Struct values will have this format -
%% {safsImmCsStruct, StructClassName::binary(), [#safsImmAttrValues_2{}]}.
%%
%% The 'implementer' field is significant only for persistent
%% runtime instances.

-type restoredValues()  ::  [term()]|'$imm_runtime'.

-record(restoredInstance, {
			   class                   :: atom(),
			   key                     :: [binary()],
			   attrs                   :: [{atom(), restoredValues()}],
			   implementer=undefined   :: implementer()
			  }).

%% Below is a real-life example of a #restoredInstance{} record:
%%     #restoredInstance{
%%           class='VlanPort',
%%           key=[<<"transportId=1">>,<<"vlanPortId=2">>],
%%           attrs=[{vlanId,[1026]},
%%                  {userLabel,[<<"OaM_VLAN">>]},
%%                  {reservedBy,[<<"interfaceIPv4Id=2,routerId=1,transportId=1">>]},
%%                  {isTagged,[1]},
%%                  {encapsulation,[<<"ethernetPortId=1,transportId=1">>]}
%%                 ],
%%           implementer=undefined}
%%
%% and here is another one
%%
%%     #restoredInstance{
%%           class='SystemConstants',
%%           key=[<<"SystemConstantsId=1">>],
%%           attrs=[{sysConstants,[<<" ">>]}
%%                 ],
%%           implementer= <<"SystemConstantsImplementer">>}


%% Instance group created from the From-system dump. The
%% implPers field indicates that the class is implied persistent.

-record(instanceGroup, {className              :: className(),
			instances=[]           :: [#restoredInstance{}],
			maxLength=0            :: integer(),
			implPers=false         :: boolean()
		       }).

%% IMM-like instances, created by application 'write' operations

-record(immInstanceGroup, {className              :: className(),
			   immInsts=[]            :: [#safsImmCtInstance{}],
			   maxLength=0            :: integer(),
			   implementer            :: implementer()
			  }).


%% Wrapper for binarized classname and IMM file.

-record(classAndFile, {
		       class= <<"">>   :: className(),
		       category=config :: immMode(),
		       immFile=""      :: string()
		      }).


-record(exception,
	{key=undefined              :: atom(),
	 text= <<"">>               :: binary(),
	 data                       :: any(),
	 opts=[restart, stack]      :: [any()],
	 errorStrings=[]            :: [string()]
	}).
