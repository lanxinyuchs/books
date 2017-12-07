#!/usr/bin/env escript
%% -*- erlang -*-
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sim_setup.escript %
%%% Author:     etxjotj
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------

-module(sim_setup).
-vsn('/main/R1A/R2A/7').
-date('2013-01-30').
-author('etxjotj').
-mode(compile).
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2013 All rights reserved.
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
%%% -----      ---------- --------    ------------------------
%%% R1A/1      2012-01-24 etxjotj     Created
%%% R1A/2      2012-01-24 etxjotj     Metadata added
%%% R1A/4      2012-02-15 etxjotj     Added dev_patches creation
%%% R1A/7      2012-02-27 etxjotj     Removed RCS_ROOT/erlang dir 
%%% R2A/1      2013-01-11 etxjotj     Unpack software on RCS_ROOT/software
%%% ----------------------------------------------------------

%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% In escripts main/1 is always called first

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include_lib("xmerl/include/xmerl.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

main(["-h"]) ->
    ModVsn = proplists:get_value(vsn, ?MODULE:module_info(attributes)),
    io:format("Usage: sim_setup.escript@@~s~n~n"
	      "Options: ~n"
	      "-p Path - Path is a upgrade package in compressed tar format (tgz)~n"
	      "          Default value is the RCS UP file in the rcs vob",
	    [atom_to_list(hd(ModVsn))] ),
    ok;
main(["-p", Path]) ->

    CxsPath = normalize_path(Path),
    setup(CxsPath);
main([]) ->
    setup("/vobs/rcs/delivery/RCP_CSX10179_1/RCP-SIM_CXS101549_2/"
	  "doc/19010/RCP-SIM_CXS101549_2.cxs").

setup(CxsFilePath) ->
    ModVsn = proplists:get_value(vsn, ?MODULE:module_info(attributes)),
    io:format("Running sim_setup.escript@@~s~n",[atom_to_list(hd(ModVsn))]),

    case filelib:is_file(CxsFilePath) of
	true -> ok;
	false ->
	    io:format("No such file: ~p~n",[CxsFilePath]),
	    erlang:error({error, enoent}, [CxsFilePath])
    end,

    User = os:getenv("USER"),
    Host = os:getenv("HOST"),

    RcsRoot = 
	case os:getenv("RCS_SIM_ROOT") of
	    false ->
		R = filename:join(["/local", "scratch", User, "RCS_ROOT"]),
		io:format("RCS_SIM_ROOT not set, using ~p~n",[R]),
		R;
	    RcsRootEnv ->
		RcsRootEnv
	end,

    cmd("rm -rf "++ RcsRoot ++ "/*"),


    TmpDir = filename:join("/tmp", User++"@"++Host),

    HomeDir = filename:join([RcsRoot, "home", User]),
    SquashFsDir = filename:join([RcsRoot, "software"]),
    ReleasesDir = filename:join(HomeDir, "releases"),
    MnesiaDir = filename:join(HomeDir, "mnesia"),
    SoftwareDir = filename:join(HomeDir, "software"),
    DevPatchesDir = filename:join(HomeDir, "dev_patches"),

    RcsDir = filename:join(RcsRoot, "rcs"),
%    ErlangDir = filename:join(RcsRoot, "erlang"), % Dir not used??? jotj
    ArchiveDir = filename:join([RcsRoot, "rcs", "swm", "archive"]),
    ArchiveTmp = filename:join(ArchiveDir, "tmp"),

    %% Directory setup

    cmd("mkdir -p "++SquashFsDir),
    cmd("mkdir -p "++RcsDir),
    cmd("mkdir -p "++TmpDir),
    cmd("mkdir -p "++DevPatchesDir),

    cmd("rm -rf "++ReleasesDir),
    cmd("rm -rf "++MnesiaDir),

    %% Unpacking UP file to $RCS_ROOT/rcs/swm/archive/tmp
    %% Then change the name of the "tmp" dir to the name of the UP

    cmd("rm -rf "++ArchiveDir),
    cmd("mkdir -p "++ArchiveTmp),

    cmd("cd "++ArchiveTmp++" ; tar xfz "++CxsFilePath),
    CxsMetaPath = 
	case filelib:wildcard(filename:join(ArchiveTmp, "*-up.xml")) of
	    [] ->
		case filelib:wildcard(filename:join(ArchiveTmp, "cxs*.xml")) of
		    [] ->
			erlang:error({no_metadata_file, ArchiveTmp},
				     [CxsFilePath]);
		    [CMP] -> CMP
		end;
	    [CMP] -> CMP
	end,
    
    %% Read metadata to find UP name
    {CxsConfigurationE, []} = xmerl_scan:file(CxsMetaPath),
    
    CxsProductE = find_element(product, CxsConfigurationE),
    CxsName = find_attribute(name, CxsProductE),
    CxsProdId = find_attribute(id, CxsProductE),
    CxsVersion = find_attribute(version, CxsProductE),

    CxsDir = filename:join(ArchiveDir, CxsName++"_"++CxsProdId++"_"++CxsVersion),

    %% Unpack all included CXP files in to $RCS_ROOT/home/$USER/software

    cmd("rm -rf "++SoftwareDir),
    cmd("mkdir -p "++SoftwareDir),
    cmd("cp "++CxsMetaPath++" "++SoftwareDir),
    cmd("mv "++ArchiveTmp++" "++CxsDir),

    {ok, CxsContent} = file:list_dir(CxsDir),
    
    [unpack_cxp(filename:join(CxsDir, CxpFile), SoftwareDir, SquashFsDir)||
	CxpFile<-CxsContent],

    %% New UP format with SquashFS. In SIM the squash fs CXPs are unpacked 
    %% instead of mounted

    [unpack_squash_fs(filename:join(CxsDir, CxpFile), SquashFsDir)||
	CxpFile<-CxsContent],

    ok.

unpack_cxp(CxpFile, SoftwareDir, SquashFsDir) ->
    case filename:extension(CxpFile) of
	".xml" -> %% This is the metadata file
	    ok;
	_ ->
	    do_unpack_cxp(CxpFile, SoftwareDir, SquashFsDir)
    end.

do_unpack_cxp(CxpFile, SoftwareDir, SquashFsDir) ->
    cmd("mkdir -p "++SoftwareDir++"/tmp"),
    cmd("cd "++SoftwareDir++"/tmp ; tar xfz "++CxpFile),
    
    XmlPattern = filename:join([SoftwareDir, "tmp", "cxp*.xml"]),
    [CxpMeta] = filelib:wildcard(XmlPattern),

    {ConfigurationE, []} = xmerl_scan:file(CxpMeta),
    ProductE = find_element(product, ConfigurationE),
    Name = find_attribute(name, ProductE),
    ProdId = find_attribute(id, ProductE),
    Version = find_attribute(version, ProductE),
    
    CxpDir = Name++"_"++ProdId++"_"++Version,
    Destination = filename:join(SoftwareDir, CxpDir),
    Source = filename:join(SquashFsDir, CxpDir),
    ok = file:make_symlink(Source, Destination),
    cmd("rm -rf "++SoftwareDir++"/tmp ").

unpack_squash_fs(CxpFile, SquashFsDir) ->
    case filename:extension(CxpFile) of
	".xml" -> %% This is the metadata file
	    ok;
	_ ->
	    do_unpack_squash_fs(CxpFile, SquashFsDir)
    end.

do_unpack_squash_fs(CxpFile, SquashFsDir) ->
    cmd("mkdir -p "++SquashFsDir++"/tmp"),
    cmd("cd "++SquashFsDir++"/tmp ; tar xfz "++CxpFile),

    %% This opens the outer shell of the cxp, the software should now
    %% search for the squash fs image, but the tools aren't there yet
    %% so we asuume the old format.
    
    XmlPattern = filename:join([SquashFsDir, "tmp", "cxp*.xml"]),
    [CxpMeta] = filelib:wildcard(XmlPattern),

    {ConfigurationE, []} = xmerl_scan:file(CxpMeta),
    ProductE = find_element(product, ConfigurationE),
    Name = find_attribute(name, ProductE),
    ProdId = find_attribute(id, ProductE),
    Version = find_attribute(version, ProductE),

    CxpDir = filename:join(SquashFsDir, Name++"_"++ProdId++"_"++Version),
    cmd("mv "++SquashFsDir++"/tmp "++CxpDir).
	    

cmd(Cmd) ->
    io:format("~s~n",[Cmd]),
    io:format("~s~n",[os:cmd(Cmd)]).




%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions:
%%% Description: Finds a sub element to an xml element, or in a list
%%%              of element contents. Assumes there is only one element
%%%              with the same name
%%% ----------------------------------------------------------

find_element(ElementName, Element) when is_record(Element, xmlElement) ->
    find_element(ElementName, Element#xmlElement.content);
find_element(ElementName, ContentList) ->
    {value, Element} =
        lists:keysearch(ElementName, #xmlElement.name, ContentList),
    Element.

%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions:
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
            erlang:error({badmatch, false}, [AttributeName, AttributeList])
    end.

%%% ----------------------------------------------------------
%%% #           normalize_path(Path
%%% Input: Path:string() - A file path, relative or absolute
%%% Output: 
%%% Exceptions: 
%%% Description: Make relative paths absolute, which means expand ~ to the 
%%%              user's home directory, and relative paths relative to the
%%%              current working directory
%%% ----------------------------------------------------------

normalize_path(Path) ->
    case hd(Path) of
	$/ ->
	    Path;
	$~ ->
	    case tl(Path) of
		[] ->
		    os:getenv("HOME");
		[$/|Tail] ->
		    filename:join(os:getenv("HOME"), Tail);
		Other ->
		    Split = filename:split(Other),
		    Base = filename:dirname(os:getenv("HOME")),
		    filename:join([Base|Split])
	    end;
	_ ->
	    {ok, Cwd} = file:get_cwd(),
	    filename:join(Cwd, Path)
    end.
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



