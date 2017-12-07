%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmf.hrl %
%%% Author:	etxbjca
%%% Description:
%%%
%%% ----------------------------------------------------------
-hrl_vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R10A/R11A/1').
-hrl_date('2017-10-19').
-hrl_author('eolaand').
-hrl_shaid('6bbb99df59220f51b9ad2edd55c62086f48d99ec').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% R1A/1      2012-04-10 etxbjca     Created
%%% R2A/4      2013-05-30 erarafo     Lifted some constants up here
%%% R2A/6      2013-06-04 erarafo     Changed version-related attribute names
%%% R2A/8      2013-06-17 erarafo     Just two levels of version attributes
%%% R2A/9      2013-06-19 erarafo     Type info made more precise
%%% R2A/10     2013-09-16 etxpeno     Add bi_dir to #gmfMimClass{}
%%% R2A/11     2013-09-18 erarafo     Removed unused constants
%%% R2A/12     2013-09-24 erarafo     Support for CLI extensions
%%% R2A/14     2013-09-25 erarafo     #cliSharedLib{} redefined
%%% R2A/16     2013-09-29 erarafo     Comments improved
%%% R3A/2      2015-03-10 erarafo     Type info added in #gmfMimClass{}
%%% R4A/1      2015-06-03 erarafo     Type info elaborated
%%% R4A/2      2015-06-09 erarafo     Some type specs moved here
%%% R4A/3      2015-06-10 erarafo     Redundant record removed
%%% R6A/2      2016-09-15 etxpeno     Changes due to improvements of MIB sync
%%% R7A/1      2016-09-08 etxpeno     Dialyzer fixes
%%% R7A/3      2016-09-15 etxpeno     Uplift from R6A/2
%%% R7A/4      2016-09-15 etxpeno     Dialyzer fixes
%%% R7A/5      2016-10-06 erarafo     ICTI request ids gathered here
%%% R7A/6      2016-10-07 etxpeno     Changes due to improvements of MIB sync
%%% R10A/1     2016-07-05 etxpeno     Add record #gmfMimRelation{}
%%% R11A/1     2016-10-19 eolaand     Add macro REQUEST_ID_COMSA_1
%%% ----------------------------------------------------------


%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description:
%%% ----------------------------------------------------------

%%% Request ids for RCS-internal use of ICTI
-define(REQUEST_ID_OOT_1,   100001).
-define(REQUEST_ID_OOT_2,   100002).
-define(REQUEST_ID_GMF_1,   200001).
-define(REQUEST_ID_COMSA_1, 300001).


-define(GMFAPP_MIM, "gmfMim").
-define(GMFAPP_IMM, "gmfImm").
-define(GMFAPP_CLI_EXTENSION, "cli_extension").


%% Filetype classes | objects
-define(GMF_IMM_FT_CLASS,   "classes").
-define(GMF_IMM_FT_OBJECT,  "objects").
%% Element names
-define(GMF_IMM_INFOS,      immInfos).
-define(GMF_IMM_INFO,       immInfo).


%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           gmfClient
%%% Description:
%%% ----------------------------------------------------------

-record(gmfClient, {fragment, port}).

%%% ----------------------------------------------------------
%%% #           record_name
%%% Description:
%%% ----------------------------------------------------------

-record(gmfFragment, {fragment, path}).

%%% ----------------------------------------------------------
%%% #           record_name
%%% Description:
%%% ----------------------------------------------------------

%% Mnesia
-record(gmfMoc, {name, fragment, attributes}).
-record(gmfAttribute, {name, mandatory, default}).

-record(gmfAction, {name, fragment}).


%% IMM info, part of CXP information

-type immInfoVersion()            :: [non_neg_integer()].      % length 3 required
-type immInfoFromVersion()        :: [non_neg_integer()].      % length 3, 2 or 1

-record(immInfo,
	{file                     :: string(),
	 path                     :: string(),
	 abspath                  :: string(),
	 type                     :: unnamedSchema | namedSchema | objects,
	 schemaName=""            :: string(),
	 version=[0, 0, 0]        :: immInfoVersion(),
	 fromVersions=[]          :: [immInfoFromVersion()],
	 cat=none                 :: none | s0 | s1v0 | s1v1 | s1v2 | s1v3 | string()
	}).


%% CLI Extension info, part of CXP information
%%
%% See sysEnv:architectureByMetaData for
%% possible 'type' tuples.
-record(cliSharedLib,
	{type           :: {string(), string()},
	 relpath        :: string()  %% pathname relative to CXP dir
	}).


%% #gmfCxpRev{}: CXP information
%%
%% This record holds information from the appdata files that
%% refer to model metadata files.
%%
%% The 'key' field is a 2-tuple of CXP product id and revision.
%%
%% The 'ldn' field is a list of MOM root instances. Each MOM root
%% instance is specified by a list such as
%% [<<"ManagedElement">>,<<"NodeSupport">>,<<"MpClusterHandling">>]
%% i e classnames in top-down order with no explicit RDN values.
%%
%% The 'imm_root' field provides root instance info, e g like this:
%% [["nodeSupportId=1","NodeSupport"],["transportId=1","Transport"]]
%% The list may be empty. TODO, does this reflect MOM name prefixing
%% possibly?
%%
%% The 'cxp_path' field is the CXP directory abspath, e g:
%% "/home/sirpa/software/RBSNC_CXP9030284_3_R10SG"
%%
%% Metadata specifications are 3-lists specifying file paths
%% and handling info, e g:
%% ["RmeSupport_mp.xml", "COMSA*/comsa-*/priv/imm", "imm"]
%% ["RcsOamAccessPoint.xml", "OOT*/oot-*/priv/etc", "mim"]
%%
%% Beware: The 'cat' field is used for different purposes in
%% gmfImmUgVerifyUpgrade and gmfImmUgMaster.
%%
%% IMPORTANT NOTE: Be careful if ever changing this record format.
%% The GMF upgrade-related modules must be able to deal with all
%% known versions of the record format, not just the most recent
%% one.

-type metaDataSpec() :: [string()].

-record(gmfCxpRev, {key             :: {string(), string()},
		    ldn=[]          :: [binary()],
		    imm_root=[],
		    cxp_path        :: string(),
		    mim_info=[]     :: [metaDataSpec()],
		    imm_info=[]     :: [#immInfo{}],
		    cli_info=[]     :: [#cliSharedLib{}],
		    active=false    :: boolean()}).

%% Mnesia
-record(gmfMimInfo, {path,
		     ldn,
		     root,
		     imm_data,
		     cb_module}).


%%
-record(gmfImmData, {imm_handle,
		     owner_handle,
		     mode}).


%%% ----------------------------------------------------------
%%% #           imm records
%%% Description:
%%% ----------------------------------------------------------



%%% ----------------------------------------------------------
%%% #           mnesia tables
%%% Description:
%%% ----------------------------------------------------------

-record(gmfMimClass,
	{key            :: {string()|'_',           % MIM name
			    string()|'_'}|'_',      % Classname (no prefix)
	 root = false,
	 parent = []    :: [{string(),              % Parent classname (no prefix)
			     string()}]|atom(),     % MIM name
	 attributes = [],
	 actions = [],
	 imm_ns = ""    :: string()|atom(),         % MOM name, when used
	 imm_rdn        :: string()|atom(),         % IMM RDN attribute name
	 cxp_info       :: {string()|'_',           % CXP id
			    string()|'_'}|'_'       % CXP rev
	}).


-record(gmfMimStruct, {key,        %% {MimName, Name}
		       attributes = [],
		       imm_ns = ""}). %% Prefix to be added

-record(gmfMimDerivedType, {key,   %% {MimName, Name},
			    basetype = []}). %% Base type of the derived type

-record(gmfMetaBiDir,
	{
	  mim_class :: gmfMimClass(),
	  imm_class :: gmfImmClassKey() | 'undefined',
	  bidir_list = []
	}
       ).

-type attrProp()  :: type | flag | category | 'default-value'.

-type gmfMimClass()  :: {string(),   %% MIM-name (?)
			 string()}.  %% root class (?)

-type gmfImmClassKey()    :: string().         %% class name


%% The #gmfImmClass{} record represents an IMM class. The
%% fields are:
%%
%% key       ImmClassName                  - string
%%
%% root      boolean()                     - false always?
%%
%% category  {category, Cat}               - Cat: "SA_CONFIG" | "SA_RUNTIME"
%%
%% rdn       {Name, [{type, "SA_STRING_T"},
%%                   {category, "SA_CONFIG"},
%%                   {flag, "SA_INITIALIZED"},
%%                   ...]}
%%
%% attributes                              - a list of tuples like the above
%%
%% gmfMimClass    {MomName,                - pair of strings
%%                 MimClassName}
%%
%% imm_file_name                           - absolute path, string
%%
%% cxp_info  {CxpId,
%%            CxpVer}
%%
%% Many of the fields are optionally '_', gmfAppData:igmc/1 depends on this.

-record(gmfImmClass,
	{key           :: gmfImmClassKey(),
         root=false    :: boolean() | '_',
	 category      :: {category, string()} | '_' | 'undefined',
	 rdn           :: {string(), [{attrProp(), string()}]} | '_' | 'undefined',
	 attributes=[] :: [{string(), [{attrProp(), string()}]}] | '_',
         gmfMimClass   :: gmfMimClass() | '_' | 'undefined',
         imm_file_name :: string() | '_',
	 cxp_info      :: {string(),               % CXP id
			   string()} | '_'         % CXP rev
	}).

%% This is an example of a real-life gmfImmClass record,
%% captured at start of upgrade:
%%
%% #gmfImmClass{
%% 	key={"FrequencySyncIO","CXP9030859_1","R7JF"},
%% 	root=false,
%% 	category={category,"SA_CONFIG"},
%% 	rdn={"frequencySyncIOId",[{flag,"SA_NOTIFY"},{flag,"SA_INITIALIZED"},{category,"SA_CONFIG"},{type,"SA_STRING_T"}]},
%% 	attributes=[{"reservedBy",[{flag,"SA_NOTIFY"},{flag,"SA_MULTI_VALUE"},{flag,"SA_WRITABLE"},{category,"SA_CONFIG"},{type,"SA_NAME_T"}]},
%% 		    {"operationalState",[{category,"SA_RUNTIME"},{type,"SA_INT32_T"}]},
%% 		    {"availabilityStatus",[{flag,"SA_MULTI_VALUE"},{category,"SA_RUNTIME"},{type,"SA_INT32_T"}]},
%% 		    {"encapsulation",[{flag,"SA_NO_DANGLING"},{flag,"SA_NOTIFY"},{flag,"SA_WRITABLE"},{category,"SA_CONFIG"},{type,"SA_NAME_T"}]},
%% 		    {"operPortMode",[{category,"SA_RUNTIME"},{type,"SA_INT32_T"}]},
%% 		    {"adminPortDirection",[{'default-value',"1"},{flag,"SA_NOTIFY"},{flag,"SA_WRITABLE"},{category,"SA_CONFIG"},{type,"SA_INT32_T"}]}],
%% 	gmfMimClass={"RsyncFrequencySyncIO","FrequencySyncIO","CXP9030859_1","R7JF"},
%% 	imm_file_name="/home/sirpa/software/SYNC-DUSG2_CXP9030859_1_R7JF/share/sync-central/imm/RsyncFrequencySyncIO_immR3_classes.xml"}

-type attrPropName()    :: attrProp().
-type attrPropValue()   :: atom().        %% to be sharpened
-type immCat()          :: config|persistentRuntime|runtime.
-type className()       :: binary().

-record(oldClassInfo,
	{className      :: className(),
	 rdnName        :: binary(),
	 category       :: immCat(),
	 attributes     :: [{string(), [{attrProp(), string()}]}]
	}).


%% This table will be updated at appdata_complete/activate
-record(gmfImmObject, {key,        %% {Name, Class, Cxp, Rev}
		       root = false, %% updated at appdata_complete/activate
		       attributes = [], %% Don't care about object attributes for the timebeing
		       imm_file_name}).

-record(gmfImmObjectFile, {key, %% {FileName, Cxp, Rev}
			   path,
			   root}).

%% Needed for the maintaining gmf variables
-record(gmfVars, {key,
		  value}).



-record(gmfMims, {key,
		  mim_list = []}).

%% The #gmfDnMap{} record represents infomation of a path to an ECIM MO.
%% The values of the path are not interesting

%% The fields are:
%% mim_names      A list of binaries - Name components in reversed order
%%
%% imm_names      A list of binaries - Name components
%%
%% mim_class      Pointer to the mim class
%%
%% Example of a record that contains information related to the ECIM MO
%% ManagedElement=*,Equipment=*,FieldReplaceableUnit=*,EcPort=*
%% #gmfDnMap{mim_names = [<<"EcPort">>,<<"FieldReplaceableUnit">>,<<"Equipment">>,<<"ManagedElement">>],
%%           imm_names = [<<"ecPortId">>,<<"fieldReplaceableUnitId">>,<<"equipmentId">>],
%%           mim_class = {"ReqEcPort","EcPort"}}

-record(gmfDnMap, {mim_names      :: [binary()] | 'undefined',
		   imm_names = [] :: [binary()] | atom(),
		   mim_class      :: gmfMimClass() | atom()}).

%% The #gmfMimChildren{} record contains information of the children to
%% a MIM class. This information is the same as the information in the field
%% parent in #gmfMimClass{} but in the reversed direction.

%% Example of a record

%% #gmfMimChildren{key = {"ECIM_SysM","SysM"},
%%                 children = [{"RcsOamAccessPoint","OamAccessPoint"}]}

-record(gmfMimChildren, {key           :: gmfMimClass(),
			 children = [] :: [gmfMimClass()]}).

%% The #gmfMimRelation{} record contains the information of
%% the parent-child relations in the system. This information is used to
%% populate the field parent in #gmfMimClass{}.
%% Perhaps this information should replace the related information in
%% #gmfMimClass{}

%% Example of a record

%% #gmfMimRelation{child  = {"RcsOamAccessPoint", "OamAccessPoint"},
%%                 parent = {"SysM",              "ECIM_SysM"}}

-record(gmfMimRelation, {child   :: gmfMimClass(),
			 parent  :: {string(), % Parent classname (no prefix)
				     string()} % MIM name
			}
       ).

-define(L2A(_List), try
			list_to_existing_atom(_List)
		    catch error:badarg ->
			    list_to_atom(_List)
		    end).
