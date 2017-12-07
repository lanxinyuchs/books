#!/usr/bin/env escript
%% -*- erlang -*-

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gupmaker.escript %
%%% @author etxberb
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/35

%%% @doc ==Global UP metadata file generator==
%%% This script takes a basic UP metadata file and extends it with
%%% hardware compatibility information
%%% @end

-module(gupmaker).
-vsn('/main/35').
-date('2017-03-16').
-author('etxberb').
-mode(compile).
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% /main/1    2016-02-02 etxjotj     Created
%%% /main/3    2016-02-23 etxjotj     First complete version
%%% /main/11   2016-04-14 etxberb     Changed hwSwCompatibilityIndex to
%%%                                   hwSwCompatibility.
%%% /main/12   2016-05-16 etxberb     Changed "RADIO" to "OTHER".
%%% /main/13   2016-05-16 etxjotj     Added input data list in header
%%% /main/14   2016-07-01 etxjotj     Fixed a problem with ranges
%%% /main/16   2016-08-03 etxberb     Added preHAL_products/2.
%%% /main/17   2016-08-08 etxberb     Changed preHAL_products to
%%%                                   compatible_contentinfo and added BB-T.
%%% /main/18   2016-08-11 etxberb  Bugfix in is_letters_higher/2.
%%% /main/21   2016-11-02 etxberb  Added validate_stringNotEmpty/1.
%%% /main/23   2016-11-07 etxberb  More legible logging in
%%%                                validate_stringNotEmpty/1.
%%% /main/24   2016-12-07 etxjotj  Support for P-ranges
%%% /main/25   2017-01-20 etxjotj  Radio to category OTHER
%%% /main/26   2017-01-20 etxjotj  Ignore CXA package metadata files
%%% /main/27   2017-01-23 etxjotj  Fixed debug options
%%% /main/28   2017-01-24 etxjotj  Simplified radio model names
%%% /main/29   2017-02-06 etxjotj  Bugfixes for radio hw
%%% /main/30   2017-02-27 etxjotj  COmpatiblity for 16B
%%% /main/31   2017-02-28 etxkols  Revert to /main/29
%%% /main/32   2017-02-28 etxjotj  Compatiblity for 16B again...
%%% /main/33   2017-03-10 etxberb  Compatiblity for 16B again...
%%% /main/34   2017-03-15 etxberb  * Changed warning to info in warning_radio/1.
%%%                                * Added write_header_note_radio/1.
%%% /main/35   2017-03-16 etxberb  * Changed WARNING to WRNNG in warning_radio/1
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-include_lib("xmerl/include/xmerl.hrl").
-export([main/1]).

-define(HwTypes_contentinfo, [{"BASEBAND", "BB521_S"},
			      {"BASEBAND-T", "BBT503"},
			      {"BASEBAND-T", "BBT605"}]).

-define(COMMENT_Divider,
	"# =======================================================~n").

%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	error_logger:error_report(?RepInfo(__ReportInfo))).
-define(LOG_INFO(__ReportInfo),
	error_logger:info_report(?RepInfo(__ReportInfo))).
-define(LOG_WARN(__ReportInfo),
	error_logger:warning_report(?RepInfo(__ReportInfo))).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Generates a global UP metadata file 
%%% Hardware compatibility information from different sources are compiled
%%% and listed according to specification
%%% Any existing hw compatibility information is overwritten
%%% @end
%%% ----------------------------------------------------------

main([RadioInDir]) ->
    put(debug, true),
    %% put(radio, name),
    extract_radio_info(normalize_path(RadioInDir));
    
main(Args) ->
    %% dbg:tracer(),
    %% dbg:p(all, c),
    %% dbg:tpl(?MODULE, x),
    %% dbg:tpl(?MODULE, is_compatible, x),
    main(Args, []).

main(["-help"|_],_) ->
    main(x,x);
main(["--help"|_],_) ->
    main(x,x);

main(["-metafile", Path|Args], Opts) ->
    main(Args, [{metafile, Path}|Opts]);
main(["-out", Path|Args], Opts) ->
    main(Args, [{out, Path}|Opts]);
main(["-index", Index|Args], Opts) ->
    main(Args, [{index, Index}|Opts]);   
main(["-schema", Schema|Args], Opts) ->
    main(Args, [{schema, Schema}|Opts]);    
main(["-uptype", UpType|Args], Opts) ->
    main(Args, [{upType, UpType}|Opts]);    
main(["-debug"|Args], Opts) ->
    put(debug, true),
    main(Args, Opts);
main(["-radioIn", Path|Args], Opts) ->
    main(Args, [{radioIn, Path}|Opts]);
main(["-useKrc"|Args], Opts) ->
    put(radio, name),
    main(Args, Opts);

main(Args, Opts) when Args /= [], Args /= x ->
    start([{datadirs, Args}|Opts]);

main(_, _) ->
    io:format("Generate hardware compatiblity info in UP metadata~n"
	      "This tool uses an already existing info file with UP metadata~n"
	      "and add on hardware information given in the data dirs~n"
	      "~n"
	      "Usage: gupmaker.escript [Flags] DataDir DataDir ...~n"
	      "DataDir: Path to a directory where hw compatiblity info is stored~n"
	      "Flags:~n"
	      "  -metafile Path : A UP metadata file wihtout hw information~n"
	      "  -out Path      : A directory where the out result is stored~n"
	      "  -index Index   : A hardware-software compatibility index~n"
	      "  -schema Path   : Enables XSD schema file check for Global UP~n"
	      "  -uptype Type   : BASEBAND | BASEBAND-T | OTHER~n"
	      "  -debug         : Enable debug printouts~n"
	      "  -radioIn       : Path to a directory with load module containers"
	      "  -help          : Show this text~n"
	      "  --help         : Show this text~n"
	     ,[]),
    halt(1).



%%% ----------------------------------------------------------
%%% adoc
%%% aend
%%% ----------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           start(Opts)
%%% Input: Opts:[{atom(),term()} - a key-value tuple lists with options
%%% Output: 
%%% Exceptions: 
%%% Description: The main sequence of events
%%% ----------------------------------------------------------

-record(basic, {typeA, product, date, description, type, release, 
		framework, contentinfo}).

start(Opts) ->
    %% dbg:tracer(),
    %% dbg:p(all, c),
    %% dbg:tpl(?MODULE, is_default_included, x),
    %% dbg:tp(ets, match, x),
    debug("Options parsed~n"),
    
    UpType = 
	case proplists:get_value(upType, Opts, undefined) of
	    UT when UT=="BASEBAND"; UT=="BASEBAND-T"; UT=="OTHER" ->
		UT;
	    undefined -> 
		undefined;
	    UT ->
		error_logger:error_msg("Unrecognized UP-type: ~p~n",[UT]),
		throw({unknown_upType, UT})
	end,

    Metafile = normalize_path(proplists:get_value(metafile, Opts)),
    Out = normalize_path(proplists:get_value(out, Opts)),
    Index = proplists:get_value(index, Opts),
    
    RadioDataDir = 
	case proplists:get_value(radioIn, Opts, undefined) of
	    undefined ->
		[];
	    RadioInDir ->
		RadioInPath = normalize_path(RadioInDir),
		debug("Extracting radio info from: ~p~n",[RadioInPath]),
		extract_radio_info(RadioInPath)
	end,
    debug("Radio info in: ~p~n",[RadioDataDir]),

    try 
	DataDirs = 
	    [normalize_path(Dir)||Dir<-proplists:get_value(datadirs, Opts)]++
	    RadioDataDir,
	debug("Metafile: ~p~nIndex: ~p~nOut: ~p~nDataDirs = ~p~n",
	      [Metafile, Index, Out, DataDirs]),
	
	%% Read UP meta file
	{ConfigurationE, []} = xmerl_scan:file(Metafile, []),
	BasicMetadata = extract_basic_metadata(ConfigurationE),
	debug("Basic: ~p~n~s~n",[BasicMetadata,os:cmd("cat "++Metafile)]),
	collect_data(DataDirs),
	
	Path = filename:join(Out, filename:basename(Metafile)),
	generate_gup_file(BasicMetadata, Index, UpType, Path),
	case proplists:get_value(schema, Opts, undefined) of
	    undefined ->
		ok;
	    LocalSchemaPath ->
		Schema = normalize_path(LocalSchemaPath),
		verify(Path, Schema)
	end
    after 
	case get(debug) of
	    true ->
		ok;
	    _ ->
		cmd("rm -rf "++RadioDataDir)
	end
    end,
    ok.


extract_radio_info(RadioInPath) ->
    TmpDir = cmd("mktemp -d /tmp/gupmaker.model.XXXXX")--"\n",
    debug("Generated files will be stored at ~p~n",[TmpDir]),
    {ok, Files} = file:list_dir(RadioInPath),
    [extract_radio_info(TmpDir, filename:join(RadioInPath, File))||
	File<-Files,
	filename:extension(File) == ".cxp"],
    [TmpDir].

extract_radio_info(Out, File) ->
    SquashFsDir = cmd("mktemp -d "++"/tmp/gupmaker.cxp.XXXXX")--"\n",
    try
	unpack(SquashFsDir, File),
				
	generate_data_files(SquashFsDir, Out)
    catch T:E ->
	    io:format("ls -l ~s ~n~s~n",
		      [SquashFsDir, cmd("ls -l "++SquashFsDir)]),
	    
	    erlang:T({E, erlang:get_stacktrace()})
    after 
	cmd(["rm -rf ", SquashFsDir])
    end.

generate_data_files(SquashFsDir, Out) ->
    [MetaPath] = filelib:wildcard(filename:join([SquashFsDir, "*","cxp*.xml"])),
    {ConfigurationE, _} = xmerl_scan:file(MetaPath),
    ProductE = find_element(product, ConfigurationE),
    Name = find_attribute(name, ProductE),
    ProdId = find_attribute(id, ProductE),
    Version = find_attribute(version, ProductE),
    Basename = Name++"_"++ProdId++"_"++Version,


    ContentInfoE = find_element(contentinfo, ConfigurationE),
    AppdataEs = 
	lists:foldl(fun(ProductE3,A) ->
			    A++
				[AppdataE||
				    AppdataE<-ProductE3#xmlElement.content,
				    AppdataE#xmlElement.name == appdata]
		    end, [], 
		    [ProductE2||ProductE2<-ContentInfoE#xmlElement.content,
			       ProductE2#xmlElement.name == product]),
    [begin
	 RelPath = find_attribute(relpath, AppdataE),
	 File = find_attribute(file, AppdataE),
	 Path = filename:join([SquashFsDir, Basename, RelPath, File]),
	 case scan_appdata_file(Path) of
	     [] -> no_model;
	     LmLists ->
		 HwMPath = filename:join(Out, Basename++"_hwmodel.xml"),
		 {ok, HwMFd} = file:open(HwMPath, [write]),
		 generate_hw_model(HwMFd, Name, ProdId, Version, 
				   LmLists, Basename),
		 file:close(HwMFd),
		 HwCPath = filename:join(Out, Basename++"_hwc.xml"),
		 {ok, HwCFd} = file:open(HwCPath, [write]),
		 generate_hwc(HwCFd, Name, ProdId, Version, Basename),
		 file:close(HwCFd)
	 end
     end||AppdataE<-AppdataEs],
    ok.

scan_appdata_file(Path) ->
    {AppdataE, _} = xmerl_scan:file(Path),
    case find_attribute(target, AppdataE) of
	"appm" ->
	    [LmlistE||LmlistE<-AppdataE#xmlElement.content,
		      LmlistE#xmlElement.name == lmlist,
		      is_radio_lm(LmlistE)];
	_ ->
	    []
    end.

is_radio_lm(LmlistE) ->
    try find_attribute(hwcategory, LmlistE) of
	"radio" ->
	    true;
	Other ->
	    debug("Category is ~p~n",[Other]),
	    false
    catch _:_ -> 
	    case get(radio) of
		name ->
		    try find_attribute(boardType, LmlistE) of
			"KRC"++_ -> true;
			"KRD"++_ -> true;
			BoardType -> 
			    debug("Boardtype is ~p~n",[BoardType]),
			    false
		    catch _:_ -> false
		    end;
		_ ->
		    false
	    end
    end.



generate_hw_model(Fd, ProdName, ProdId, Rstate, LmLists, Basename) ->
    write(Fd, 0, "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>~n"),
    write(Fd, 0, "<!--~n"),
    write(Fd, 0, ?COMMENT_Divider),
    write(Fd, 0, "#~n"),
    {{Year,_,_}=Day,Time} = calendar:local_time(),
    write(Fd, 0, "# Copyright (c) Ericsson AB ~w All rights reserved~n",[Year]),
    write(Fd, 0, "# ~n"),
    Vsn= hd(proplists:get_value(vsn, ?MODULE:module_info(attributes), unknown)),
    Version = atom_to_list(Vsn),
    write(Fd, 0, "# Generated by gupmaker.escript@@~s~n",[Version]),
    write(Fd, 0, "#~n"),
    write(Fd, 0, ?COMMENT_Divider),
    write(Fd, 0, "# Retrieved data from ~s~n",[Basename]),
    write(Fd, 0, "# -->~n"),
    write(Fd, 0, "~n"),
    
    DocNo = "x1/192 02-"++ProdId,
    Date = format_date({Day,Time}),

    write(Fd, 0, "<hwBaseline docNo=~p rev=~p date=\"~s\">~n",[DocNo,Rstate,Date]),
    write(Fd, 1, "<boardLists>~n"),
    write(Fd, 2, "<boardList hwcategory=~p hwmodel=~p>~n", ["OTHER", ProdName]),
    [begin
	 Pid = find_attribute(boardType, LmListE),
	 write(Fd, 3, "<boardType productNumber=~p revision=~p />~n",[Pid, "R1-"])
     end||LmListE<-LmLists],
    write(Fd, 2, "</boardList>~n"),
    %% Backwards compatibility with 16B
    write(Fd, 2, "<boardList hwcategory=~p hwmodel=~p>~n", ["RADIO", ProdName]),
    [begin
	 Pid = find_attribute(boardType, LmListE),
	 write(Fd, 3, "<boardType productNumber=~p revision=~p />~n",[Pid, "R1-"])
     end||LmListE<-LmLists],
    write(Fd, 2, "</boardList>~n"),
    %% Backwards compatiblity ends here
    write(Fd, 1, "</boardLists>~n"),
    write(Fd, 0, "</hwBaseline>~n").
    

generate_hwc(Fd, Name, ProdId, Rstate, Basename) ->
    write(Fd, 0, "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>~n"),
    write(Fd, 0, "<!--~n"),
    write(Fd, 0, ?COMMENT_Divider),
    write(Fd, 0, "#~n"),
    {{Year,_,_}=Day,Time} = calendar:local_time(),
    write(Fd, 0, "# Copyright (c) Ericsson AB ~w All rights reserved~n",[Year]),
    write(Fd, 0, "# ~n"),
    Vsn= hd(proplists:get_value(vsn, ?MODULE:module_info(attributes), unknown)),
    Version = atom_to_list(Vsn),
    write(Fd, 0, "# Generated by gupmaker.escript@@~s~n",[Version]),
    write(Fd, 0, "#~n"),
    write(Fd, 0, ?COMMENT_Divider),
    write(Fd, 0, "# Retrieved data from ~s~n",[Basename]),
    write(Fd, 0, "# -->~n"),
    write(Fd, 0, "~n"),
    
    DocNo = "x2/192 02-"++ProdId,
    Date = format_date({Day,Time}),

    write(Fd, 0, "<hwcompatibility docNo=~p rev=~p date=\"~s\">~n",
	  [DocNo,Rstate,Date]),
    write(Fd, 1, "<product name=~p id=~p version=~p>~n",[Name,ProdId,Rstate]),
    write(Fd, 2, "<board hwcategory=~p hwmodel=~p />~n",["OTHER", Name]),
    %% Backwards compatiblity with 16B
    write(Fd, 2, "<board hwcategory=~p hwmodel=~p />~n",["RADIO", Name]),
    %% Ends here
    write(Fd, 1, "</product>~n"),
    write(Fd, 0, "</hwcompatibility>~n").
    
	


unpack(SquashFsDir, CxpFile) ->
    debug("Unpacking ~s~n",[CxpFile]),
    UnSquashFsCmd = "/app/rbs/wrtools/tools-sdk-20130220/usr/sbin/unsquashfs",
    cmd(["mkdir -p ",SquashFsDir, "/tmp"]),
    cmd("cd "++SquashFsDir++"/tmp; tar xf "++CxpFile),
    cmd("cd "++SquashFsDir++"/tmp; ar x sw.ar sqfs.img"),
    R = cmd("cd "++SquashFsDir++"/tmp; " ++
		UnSquashFsCmd ++ " -f -d . -n sqfs.img"),
    debug("~s~n~s~n",["cd "++SquashFsDir++"/tmp; " ++ UnSquashFsCmd ++ 
			  " -f -d . -n sqfs.img", R]),
    cmd("cd "++SquashFsDir++"/tmp; rm sw.ar sqfs.img"),
    XmlPattern = filename:join([SquashFsDir, "tmp", "cxp*.xml"]),
    [CxpMeta] = 
	case filelib:wildcard(XmlPattern) of
	    [] -> 
		error_logger:error_msg(
		  "No metadata found in ~s/tmp~n~s~n",
		  [SquashFsDir, 
		   cmd(["ls -la ",SquashFsDir,"/tmp"])]),
		erlang:error(no_metadata_found,[CxpFile, SquashFsDir]);
	    [CM] -> [CM];
	    MultipleFiles ->
		error_logger:error_msg(
		  "Multiple metadata found in ~s/tmp~n~s~n",
		  [SquashFsDir, 
		   cmd(["ls -la ",SquashFsDir,"/tmp"])]),
		erlang:error({multiple_metadata_found, MultipleFiles},
			     [CxpFile, SquashFsDir])
	end,
    
    {ConfigurationE, []} = xmerl_scan:file(CxpMeta),
    ProductE = find_element(product, ConfigurationE),
    Name = find_attribute(name, ProductE),
    ProdId = find_attribute(id, ProductE),
    Version = find_attribute(version, ProductE),
    
    CxpName = Name++"_"++ProdId++"_"++Version,
    CxpDir = filename:join(SquashFsDir, CxpName),
    cmd("mv "++SquashFsDir++"/tmp "++CxpDir).

%% Bundles are ignored
    %% unpack(SquashFsDir,CxpDir).  %recursive unpack, to handle new split RCS

%%% ----------------------------------------------------------
%%% #           extract_basic_metadata(ConfigurationE)
%%% Input: ConfigurationE: #xmlElement{} of a UP metadata
%%% Output: 
%%% Exceptions: 
%%% Description: Takes a UP metafile and adds (overwrites) hw 
%%%              compatibility info
%%% ----------------------------------------------------------

extract_basic_metadata(ConfigurationE) ->
    TypeA = try find_attribute(type, ConfigurationE) of
		A -> A
	    catch _:_ -> undefined
	    end,
    ProductE = find_element(product, ConfigurationE),
    Product = format_product(ProductE),
    Date = find_text(find_element(date, ConfigurationE)),
    Description = try find_element(description, ConfigurationE) of
		      D -> find_text(D)
		  catch _:_ -> undefined
		  end,
    Type = try find_element(type, ConfigurationE) of
	       T -> find_text(T)
	   catch _:_ -> undefined
	   end,
    Release = try find_element(release, ConfigurationE) of
		  R -> find_text(R)
	      catch _:_ -> undefined
	      end,
    FrameworkE = find_element(framework, ConfigurationE),
    Framework = [format_product(Element)||
		    Element<-FrameworkE#xmlElement.content,
		    Element#xmlElement.name == product],
    ContentInfoE = find_element(contentinfo, ConfigurationE),
    ContentInfo = [format_product(Element)||
		      Element<-ContentInfoE#xmlElement.content,
		      Element#xmlElement.name == product],
    #basic{typeA = TypeA,
	   product = Product,
	   date = Date,
	   description = Description,
	   type = Type,
	   release = Release,
	   framework = Framework,
	   contentinfo = ContentInfo}.

%%% ----------------------------------------------------------
%%% #           format
%%% Input: ProductE: #xmlElement{} - A product element
%%% Output: 
%%% Exceptions: 
%%% Description: Collapses a XML element into a 4-size tuple
%%% {Name, Id, Version, Filename}. The first three are mandatory. Filename is
%%% optional and can be 'undefined'
%%% ----------------------------------------------------------

format_product(ProductE) ->
    {find_attribute(name, ProductE),
     find_attribute(id, ProductE),
     find_attribute(version, ProductE),
     try find_attribute(filename, ProductE) of
	 F -> F
     catch _:_ -> undefined
     end}.
    
%%% ----------------------------------------------------------
%%% #           collect_data(DataDirs)
%%% Input: DataDirs:[string()] - A list of data file paths
%%% Output: 
%%% Exceptions: 
%%% Description: Collects all available hw compatiblity data
%%% ----------------------------------------------------------

collect_data([]) ->
    erlang:error(no_data_directories, [[]]);
collect_data(DataDirs) ->
    ets:new(boardlist, [set, public, named_table]),
    ets:new(compatibility, [bag, public, named_table]),
    ets:new(filedata, [set, public, named_table]),
    [collect_data_dir(Dir)||Dir<-DataDirs],
    case ets:tab2list(boardlist) of
	[] -> 
	    erlang:error(no_boardlist_info, [DataDirs]);
	BoardList ->
	    debug("~80..=s~nHW baseline data:~n~p~n",["",BoardList])
    end.
	


collect_data_dir(DataDir) ->
    Files = filelib:wildcard(filename:join(DataDir, "*.xml")),
    debug("Files: ~p~n",[Files]),
    [parse_data_file(File)||File<-Files],
    ok.


%%% ----------------------------------------------------------
%%% #           parse_data_file(File)
%%% Input: File:string() - Path to file 
%%% Output: 
%%% Exceptions: 
%%% Description: Selects parsing method, between boardLIsts and hwcompatibility
%%% ----------------------------------------------------------

parse_data_file(File) ->
    debug("Now reading ~p~n",[File]),
    Res = xmerl_scan:file(File),
    case Res of
	{Element, _} when Element#xmlElement.name==hwBaseline ->
	    store_metadata(File, Element),
	    BoardListsE = find_element(boardLists, Element),
	    parse_board_lists(BoardListsE);
	{Element, _} when Element#xmlElement.name == hwcompatibility ->
	    store_metadata(File, Element),
	    parse_hw_compatibility(Element);
	{error, Reason} ->
	    error_logger:error_msg("Error reading ~p~n~s~n",
				   [File, file:format_error(Reason)]),
	    throw({error, Reason});
	{Element, _} when is_record(Element, xmlElement) ->
	    %% Not a recognized format
	    error_logger:info_msg("Omitting ~p~n",[File])
    end.

store_metadata(File, Element) ->
    DocNo = find_attribute(docNo, Element),
    Rev = find_attribute(rev, Element),
    Date = find_attribute(date, Element),
    case ets:insert_new(filedata, {DocNo, File, Rev, Date}) of
	false ->
	    error_logger:error_msg("Duplicate document number found!~n"
				   "This: ~p ~p ~p ~p~n"
				   "Already parsed:~n~p~n",
				   [DocNo, Rev, Date, File, 
				    ets:tab2list(filedata)]),
	    throw(duplicate_docno);
	_->
	    ok
    end.
    
    



%%% ----------------------------------------------------------
%%% #           parse_board_lists(BoardListsE)
%%% Input: BoardListsE:#xmlElement{} 
%%% Output: 
%%% Exceptions: 
%%% Description: Parse a boardlist file and store entries in boardlist ets-table
%%% ----------------------------------------------------------

parse_board_lists(BoardListsE) ->
    [begin
	 {HwCategory, HwModel} = board_list_id(BoardListE),
	 warning_radio({HwCategory, HwModel}),
	 BoardTypes = 
	     [board_type_id(BoardTypeE)||
		 BoardTypeE<-BoardListE#xmlElement.content,
		 BoardTypeE#xmlElement.name == boardType],
	 Record = {{HwCategory, HwModel}, BoardTypes},
	 case ets:insert_new(boardlist, Record) of
	     true -> ok;
	     false -> 
		 error_logger:error_report(
		   [{?MODULE, parse_board_lists},
		    {board_list_exists, {HwCategory,
					 HwModel}}]),
		 throw(board_list_exists)
	 end
     end
     ||BoardListE<-BoardListsE#xmlElement.content,
       BoardListE#xmlElement.name == boardList],
    debug("~p~n",[compatiblity]),
    ok.

%%% ----------------------------------------------------------
warning_radio({"RADIO", HwModel}) ->
    error_logger:info_msg("WRNNG: HwCategory \"RADIO\" is obsolete in ~p~n",
			  [{"RADIO", HwModel}]);
warning_radio(NotDeprecated) ->
    ok.
    

%%% ----------------------------------------------------------
%%% #           board_list_id(BoardListE)
%%% Input: BoardListE#xmlElement{}
%%% Output: 
%%% Exceptions: 
%%% Description: Extracts the identifying attributes for a board list
%%% ----------------------------------------------------------

board_list_id(BoardListE) ->
    try {find_attribute(hwcategory, BoardListE),
	 find_attribute(hwmodel, BoardListE)} of
	{HwC, HwM} -> {HwC, HwM}
    catch _:_ ->
	    error_logger:error_report(
	      [{?MODULE, board_list_id},
	       {error, incorrect_boardlist_identifier},
	       {correct_keys, [hwcategory, hwmodel]}|
	       [{A#xmlAttribute.name, A#xmlAttribute.value}||
		   A<-BoardListE#xmlElement.attributes]]),
		throw(incorrect_boardlist_identifier)
    end.

%%% ----------------------------------------------------------
%%% #           board_type_id(BoardTypeE)
%%% Input: BoardTypeE:#xmlElement{}
%%% Output: 
%%% Exceptions: 
%%% Description: Extracts the identifying attributes for a board type
%%% ----------------------------------------------------------

board_type_id(BoardTypeE) ->
    try {find_attribute(productNumber, BoardTypeE),
	 find_attribute(revision, BoardTypeE)} of
	{PN, R} -> {PN, R}
    catch _:_ ->
	    error_logger:error_report(
	      [{?MODULE, board_type_id},
	       {error, incorrect_boardtype_identifier},
	       {correct_keys, [productNumber, revision]}|
	       [{A#xmlAttribute.name, A#xmlAttribute.value}||
		   A<-BoardTypeE#xmlElement.attributes]]),
		throw(incorrect_boardtyp_identifier)
    end.


%%% ----------------------------------------------------------
%%% #           parse_hw_compatibility
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Extracts the compatiblity data for a type
%%% ----------------------------------------------------------
parse_hw_compatibility(HwCompatibilityE) ->
    ProductEs = [ProductE||ProductE<-HwCompatibilityE#xmlElement.content,
			   ProductE#xmlElement.name == product],
    [begin
	 Name = find_attribute(name,ProductE),
	 Id = find_attribute(id, ProductE),
	 Version = find_attribute(version, ProductE),
	 [begin
	      HwCategory = find_attribute(hwcategory, BoardE),
	      HwModel = find_attribute(hwmodel, BoardE),
	      ets:insert(compatibility, 
			 {{HwCategory, HwModel}, Name, Id, Version})
	  end
	  ||BoardE<-ProductE#xmlElement.content,
	    BoardE#xmlElement.name == board]
     end
     ||ProductE<-ProductEs],
    debug("~80..=s~nCompatibility data:~n~p~n",["",ets:tab2list(compatibility)]),
    ok.

%%% ----------------------------------------------------------
%%% #           
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

generate_gup_file(BasicData, Index, UpType, Path) ->
    debug("Writing ~p~n",[Path]),
    {ok, Fd} = file:open(Path, [write]),
    write_header(Fd),
    ContentInfo = BasicData#basic.contentinfo,
    write_basic(Fd,
		BasicData#basic{contentinfo =
				compatible_contentinfo(UpType, ContentInfo)}),
    write(Fd, 1, "<hwSwCompatibility index=~p />~n",[Index]),
    write_board_lists(Fd, 1, UpType, ContentInfo),
    write(Fd, 0, "</configuration>~n"),
    file:close(Fd),
    ok.

write_header(Fd) ->
    write(Fd, 0, "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>~n"),
    write(Fd, 0, "<!--~n"),
    write(Fd, 0, ?COMMENT_Divider),
    write(Fd, 0, "#~n"),
    {{Year,_,_},_} = calendar:local_time(),
    write(Fd, 0, "# Copyright (c) Ericsson AB ~w All rights reserved~n",[Year]),
    write(Fd, 0, "# ~n"),
    Vsn= hd(proplists:get_value(vsn, ?MODULE:module_info(attributes), unknown)),
    Version = atom_to_list(Vsn),
    write(Fd, 0, "# Generated by gupmaker.escript@@~s~n",[Version]),
    write(Fd, 0, "#~n"),
    write(Fd, 0, ?COMMENT_Divider),
    write(Fd, 0, "# Included data files~n"),
    [write(Fd,
	   0,
	   "#   ~s Rev ~s ~s \t~s~n",
	   [DocNo, Rev, Date, filename:basename(File)])
     || {DocNo, File, Rev, Date} <- ets:tab2list(filedata)],
    write(Fd, 0, ?COMMENT_Divider),
    write_header_note_radio(Fd),
    write(Fd, 0, "-->~n"),
    write(Fd, 0, "~n").

write_header_note_radio(Fd) ->
    try
	Cnt = ets:select_count(boardlist,
			       [{{{"RADIO", '_'}, '_'}, [], [true]}]),
	write_header_note_radio(Cnt, Fd)
    catch
	_ : _ ->
	    ok
    end.

write_header_note_radio(Cnt, Fd) when Cnt > 0 ->
    write(Fd,
	  0,
	  "# NOTE: ~w <boardList hwcategory=\"RADIO\"> - obsolete after 16B.~n",
	  [Cnt]),
    write(Fd,
	  0,
	  "#   For backwards compatibility with UPs in release 16B,~n"),
    write(Fd,
	  0,
	  "#   each boardList with hwcategory=\"OTHER\" is copied to an~n"),
    write(Fd,
	  0,
	  "#   identical boardList with hwcategory=\"RADIO\".~n"),
    write(Fd, 0, ?COMMENT_Divider).

write_basic(Fd, BasicData) ->
    case BasicData#basic.typeA of
	undefined ->
	    write(Fd, 0, "<configuration>~n", []);
	TypeA ->
	    write(Fd, 0, "<configuration type=~p>~n",[TypeA])
    end,
    write_product(Fd, 1, BasicData#basic.product),
    write(Fd, 1, "<date>~s</date>~n",[format_date(calendar:local_time())]),
    write(Fd, 1, "<description>~s</description>~n",
	  [BasicData#basic.description]),
    write(Fd, 1, "<type>~s</type>~n",[BasicData#basic.type]),
    write(Fd, 1, "<release>~s</release>~n",[BasicData#basic.release]),
    write(Fd, 1, "<framework>~n"),
    [write_product(Fd, 2, FwP)||FwP<-BasicData#basic.framework],
    write(Fd, 1, "</framework>~n"),
    write(Fd, 1, "<contentinfo>~n"),
    [write_product(Fd, 2, CIP)||CIP<-BasicData#basic.contentinfo],
    write(Fd, 1, "</contentinfo>~n"),
    ok.

write_board_lists(Fd, Indent, UpType, ContentInfo) ->
    write(Fd, Indent, "<boardLists>~n"),
    write_board_list(Fd, Indent+1, UpType, ContentInfo, ets:first(boardlist)),
    write(Fd, Indent, "</boardLists>~n").    

write_board_list(_, _, _, _, '$end_of_table') ->
    ok;
write_board_list(Fd, Indent, UpType, ContentInfo, BoardListKey) ->
    [BoardList] = ets:lookup(boardlist, BoardListKey),
    {{HwCategory, _}, _} = BoardList,
    case {HwCategory, UpType} of
	{"BASEBAND", "BASEBAND"} -> 
	    do_write_board_list(Fd, Indent, UpType, ContentInfo, BoardListKey);
	{"BASEBAND-T", "BASEBAND-T"} -> 
	    do_write_board_list(Fd, Indent, UpType, ContentInfo, BoardListKey);
	{"OTHER", "BASEBAND"} -> 
	    do_write_board_list(Fd, Indent, UpType, ContentInfo, BoardListKey);
	{"RADIO", "BASEBAND"} -> 
	    do_write_board_list(Fd, Indent, UpType, ContentInfo, BoardListKey);
	_ ->
	    ok
    end,
    write_board_list(Fd, Indent, UpType, ContentInfo,
		     ets:next(boardlist, BoardListKey)).

do_write_board_list(Fd, Indent, UpType, ContentInfo, BoardListKey) ->
    [BoardList] = ets:lookup(boardlist, BoardListKey),
    {{HwCategory, HwModel}, BoardTypes} = BoardList,
    case is_applicable_boardlist(UpType, ContentInfo, BoardList) of
	true ->
	    debug("~80..=s~nWriting boardlist for ~s ~s~n",
		  ["",HwCategory, HwModel]),
	    write(Fd, Indent, "<boardList hwcategory=~p hwmodel=~p>~n",
		  [HwCategory, HwModel]),
	    [write_board_type(Fd, Indent+1, BoardType)||BoardType<-BoardTypes],
	    [write_product(Fd, Indent+1, P)
	     || P <- ContentInfo,
		is_compatible(HwCategory, HwModel, UpType, P)],
	    write(Fd, Indent, "</boardList>~n");
	false ->
	    debug("~80..=s~nNo products found for ~s ~s~n",
		  ["",HwCategory, HwModel]),
	    ok
    end.

%% #############################################################################
%% compatible_contentinfo
%%
%% ###=======================================================================###
compatible_contentinfo(UpType, ContentInfo) ->
    [{_, Compatible_Products} | _] = Tagged_Compatible_Products =
	[begin
	     C_Ps =
		 [P || P <- ContentInfo,
		       is_compatible(HwCategory, HwModel, UpType, P)],
	     {HwType, C_Ps}
	 end
	 || {HwCategory, HwModel} = HwType <- ?HwTypes_contentinfo,
	    HwCategory == UpType],
    case diffTaggedLists(Tagged_Compatible_Products) of
	[] ->
	    Compatible_Products;
	Diff ->
	    Info = "Compatibility difference between equivalent models:",
	    Reason =
		"Unable to determine a compatible product list for contentinfo",
	    error_logger:error_report([{?MODULE, ?FUNCTION},
				       Info
				       | Diff]),
	    erlang:error(Reason, [UpType, ContentInfo])
    end.

%% #############################################################################
%% diffTaggedLists
%%
%% ###=======================================================================###
diffTaggedLists(List) ->
    Diff = diffTaggedLists(List, [Values || {_Tag, Values} <- List]),
    EmptyListFun =
	fun({_, []}) ->
		true;
	   (_) ->
		false
	end,
    case lists:all(EmptyListFun, Diff) of
	true ->
	    [];
	false ->
	    Diff
    end.

diffTaggedLists([{Tag, Values} | Tail], ValuesList) ->
    DiffValues = diff_values(Values, ValuesList),
    [{Tag, DiffValues} | diffTaggedLists(Tail, ValuesList)];
diffTaggedLists([], _) ->
    [].

diff_values([Val | Tail], ValuesList) ->
    MemberFun =
	fun(Values) ->
		lists:member(Val, Values)
	end,
    case lists:all(MemberFun, ValuesList) of
	true ->
	    diff_values(Tail, ValuesList);
	false ->
	    [Val | diff_values(Tail, ValuesList)]
    end;
diff_values([], _) ->
    [].

%% #############################################################################
%% 
%%
%% ###=======================================================================###
is_applicable_boardlist(UpType, ContentInfo, BoardList) ->
    {{HwCategory, HwModel}, _} = BoardList,
    case {HwCategory, UpType} of
	{"BASEBAND", "BASEBAND"} ->
	    is_any_compatible(HwCategory, HwModel, UpType, ContentInfo);
	{"BASEBAND-T", "BASEBAND-T"} ->
	    is_any_compatible(HwCategory, HwModel, UpType, ContentInfo);
	{"OTHER", "BASEBAND"} ->
	    is_any_compatible(HwCategory, HwModel, UpType, ContentInfo);
	{"RADIO", "BASEBAND"} ->
	    is_any_compatible(HwCategory, HwModel, UpType, ContentInfo);
	_  ->
	    false
    end.

%% #############################################################################
%% 
%%
%% ###=======================================================================###
is_any_compatible(HwCategory, HwModel, UpType, [P|ContentInfo]) ->
    case is_compatible(HwCategory, HwModel, UpType, P) of
	true ->
	    true;
	false ->
	    is_any_compatible(HwCategory, HwModel, UpType, ContentInfo)
    end;
is_any_compatible(_, _, _, []) ->
    false.


%% #############################################################################
%% 
%%
%% ###=======================================================================###
is_compatible(HwCategory, HwModel, _, Product) when HwCategory == "OTHER" orelse
						    HwCategory == "RADIO" ->
    Particular = ets:lookup(compatibility, {HwCategory, HwModel}),
    General = ets:lookup(compatibility,  {HwCategory, "*"}),
    is_product_match(Product, Particular++General);
is_compatible(HwCategory, HwModel, _, Product) ->
    Particular = ets:lookup(compatibility, {HwCategory, HwModel}),
    General = ets:lookup(compatibility,  {HwCategory, "*"}),
    is_product_match(Product, Particular++General) or
	is_default_included(Product).

is_product_match(P={Name,ThisId,ThisVsn, _}, [{_, _, ThisId, Range}|Matches]) ->
	
    case lists:last(Range) of
	$- -> 
	    %% This is an open range ("R1-")
	    case is_rev_inRange(ThisVsn, Range) of
		true -> 
		    true;
		false ->
		    debug("~s ~s ~s does not match range ~p~n",
			  [Name, ThisId, ThisVsn, Range]),
		    is_product_match(P, Matches)
	    end; 
	_  ->
	    %% This is a closed range ("R1-R2") or an explicit version ("R1")
	    case string:tokens(Range, "-") of
		[_, _] ->
		    case is_rev_inRange(ThisVsn, Range) of
			true -> true;
			false ->
			    debug("~s ~s ~s is outside range ~p~n",
				  [Name, ThisId, ThisVsn, Range]),
			    is_product_match(P, Matches)
		    end;
		[_] ->
		    case is_rev_inRange(ThisVsn, Range++"-"++Range) of
			true ->
			    true;
			false ->
			    debug("~s ~s ~s does not match explicit "
				  "version ~p~n",
				  [Name, ThisId, ThisVsn, Range]),
			    is_product_match(P, Matches)
		    end
	    end
    end;

is_product_match(Product, [_|Matches]) ->
    is_product_match(Product, Matches);
is_product_match(_, []) ->
    false.



is_default_included({Name, Id,_, _}) ->
    case ets:match(compatibility, {'_', '_', Id, '_'}) of
	[] ->
	    debug("~s ~s default included.~n",
		  [Name, Id]),
	    true;
	_ ->
	    false
    end.

write_board_type(Fd, Indent, {PNr, Rev}) ->
    write(Fd, Indent, "<boardType productNumber=~p revision=~p />~n",
	  [PNr, Rev]).
    
write_product(Fd, Indent, {N, P, V, undefined}) ->
    validate_stringNotEmpty([{"product name", N},
			     {"product id", P},
			     {"product version", V}]),
    write(Fd, Indent, "<product name=~p id=~p version=~p />~n", [N,P,V]);
write_product(Fd, Indent, {N, P, V, F}) ->
    validate_stringNotEmpty([{"product name", N},
			     {"product id", P},
			     {"product version", V},
			     {"product filename", F}]),
    write(Fd,
	  Indent,
	  "<product name=~p id=~p version=~p filename=~p />~n",
	  [N, P, V, F]).

validate_stringNotEmpty(Params) ->
    validate_stringNotEmpty(Params, Params).

validate_stringNotEmpty([{_, [_ | _]} | Tail], Params) ->
    validate_stringNotEmpty(Tail, Params);
validate_stringNotEmpty([{_, Value} = Param | _], Params) ->
    catch throw(generate_stacktrace),
    Error =
	case Value of
	    "" ->
		{"Mandatory value missing", Param};
	    _ ->
		{"Value not a string", Param}
	end,
    ?LOG_ERR(["--- ERROR ---",
	      Error,
	      "--- Input values ---"
	      | Params]),
    ?LOG_INFO(["--- Stacktrace ---" | erlang:get_stacktrace()]),
    timer:sleep(500),
    erlang:error(Error);
validate_stringNotEmpty([], _) ->
    ok.

	
write(Fd, Indent, Fmt) ->
    write(Fd, Indent, Fmt, []).
   
write(Fd, Indent, Fmt, Args) ->
    IndentStr = lists:append(tl(["  "||_<-lists:seq(0,Indent)])),
    io:format(Fd, IndentStr++Fmt, Args).

format_date({{Year,Month,Day},{Hour, Minute, Second}}) ->
    lists:append([io_lib:format("~w-",[Year]),
		  io_lib:format("~2..0w-",[Month]),
		  io_lib:format("~2..0wT",[Day]),
		  io_lib:format("~2..0w:",[Hour]),
		  io_lib:format("~2..0w:",[Minute]),
		  io_lib:format("~2..0w",[Second])]).

verify(Path, Schema) ->
    Cmd = ["xmllint --noout --schema ",Schema," ", Path],
    case cmdres(Cmd) of
	{0, _} ->
	    ok;
	{1, Res} ->
	    error_logger:error_msg("Unclassified error:~n~s~n",[Res]),
	    throw({xmllint, unclassified});
	{2, Res} ->
	    error_logger:error_msg("Error in DTD:~n~s~n",[Res]),
	    throw({xmllint, dtdError});
	{3, Res} ->
	    error_logger:error_msg("Validation error:~n~s~n",[Res]),
	    throw({xmllint, validationError3});
	{4, Res} ->
	    error_logger:error_msg("Validation error:~n~s~n",[Res]),
	    throw({xmllint, validationError4});
	{5, Res} ->
	    error_logger:error_msg("Error in schema compilation:~n~s~n",[Res]),
	    throw({xmllint, schemaCompilationError});
	{6, Res} ->
	    error_logger:error_msg("Error writing output:~n~s~n",[Res]),
	    throw({xmllint, outputWriteError});
	{7, Res} ->
	    error_logger:error_msg("Error in pattern:~n~s~n",[Res]),
	    throw({xmllint, patternError});
	{8, Res} ->
	    error_logger:error_msg("Error in reader registration:~n~s~n",[Res]),
	    throw({xmllint, readerRegistrationError});
	{9, Res} ->
	    error_logger:error_msg("Out of memory error:~n~s~n",[Res]),
	    throw({xmllint, outOfMemoryError})
    end.
    
cmdres(CmdList) ->
    Cmd = lists:flatten(CmdList),
    CmdR = Cmd++" ; echo -n \"Res=$?\"",
    debug("~s~n~s~n",[Cmd, Res = os:cmd(CmdR)]),
    Code = lists:last(string:tokens(Res, "\n=")),
    Rev = lists:reverse(Res),
    Result =
	case string:str(Rev, "\n") of
	    0 -> "";
	    Pos -> lists:reverse(string:sub_string(Rev, Pos))
	end,
    {list_to_integer(Code), Result}.


%%% ----------------------------------------------------------
%%% #           
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           normalize_path(Path)
%%% Input: Path:string()
%%% Output: string()
%%% Exceptions:
%%% Description: Expand a short or relative path to an absolute path
%%% ----------------------------------------------------------

normalize_path([$/|_]=Path) -> 
    Path;
normalize_path([$~,$/|Path]) ->
    Home = os:getenv("HOME"),
    filename:join(Home, Path);
normalize_path([$~|Path]) ->
    Home = os:getenv("HOME"),
    filename:join([filename:dirname(Home)|filename:split(Path)]);
normalize_path(Path) ->
    {ok, Cwd} = file:get_cwd(),
    filename:join(Cwd, Path).

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
%%% #           find_text(Element)
%%% Input: Element:#xmlElement{} or
%%% Output: Value:string()
%%% Exceptions:
%%% Description: Extract the text content from an xmlText element
%%% ----------------------------------------------------------

find_text(Element) when is_record(Element, xmlElement) ->
    [Text] = Element#xmlElement.content,
    Text#xmlText.value.

%%% ----------------------------------------------------------
%%% #           debug(Format, Args)
%%% Input: 
%%% Output: 
%%% Exceptions:
%%% Description: Print in the shell if debug flag is used
%%% ----------------------------------------------------------

debug(Format) ->
    debug(Format, []).
    
debug(Format, Args) ->
    case get(debug) of
	true ->
	    io:format(Format, Args);
	_ ->
	    ok
    end.


%%% WARNING!
%%% This code should match wath's in SYS and NL. If a bug is found it needs
%%% to be corrected in all three places.


%% Usecase Error
-define(UC_ERR_3(Reason, ReportInfo), {uc_error, Reason, ?RepInfo(ReportInfo)}).

%% Reason for Usecase Error
-define(UC_ERR_InvalidRevFormat, "Invalid revision format").
-define(UC_ERR_InvalidRevRange, "Invalid revision range").
-define(UC_ERR_RevLengthOutOfRange, "Revision length out of range").
-define(UC_ERR_RevNotAString, "Revision is not a string").
is_rev_eqOrHi({_, _, _, _, _} = IsRev, {_, _, _, _, _} = CompRev) ->
    is_rev_equiv(IsRev, CompRev) orelse is_rev_higher(IsRev, CompRev);
is_rev_eqOrHi(IsRev, CompRev) when not is_tuple(IsRev) andalso
				   not is_tuple(CompRev) ->
    is_rev_eqOrHi(rev_split(IsRev), rev_split(CompRev));
is_rev_eqOrHi(IsRev, {_, _, _, _, _} = CompRev) when not is_tuple(IsRev) ->
    is_rev_eqOrHi(rev_split(IsRev), CompRev);
is_rev_eqOrHi({_, _, _, _, _} = IsRev, CompRev) when not is_tuple(CompRev) ->
    is_rev_eqOrHi(IsRev, rev_split(CompRev)).

is_rev_equiv(IsRev, CompRev) when not is_tuple(IsRev) andalso
				  not is_tuple(CompRev) ->
    is_rev_equiv(rev_split(IsRev), rev_split(CompRev));
is_rev_equiv(IsRev, {_, _, _, _, _} = CompRev) when not is_tuple(IsRev) ->
    is_rev_equiv(rev_split(IsRev), CompRev);
is_rev_equiv({_, _, _, _, _} = IsRev, CompRev) when not is_tuple(CompRev) ->
    is_rev_equiv(IsRev, rev_split(CompRev));
is_rev_equiv({_, _, _, _, _} = Rev, Rev) ->
    true;
is_rev_equiv({RevState, ordinary, RevNr, "", _},
	     {RevState, CompRevType, RevNr, _, _})
  when (CompRevType == ordinary orelse
	CompRevType == verification_state) ->
    true;
is_rev_equiv({RevState, IsRevType, RevNr, _, _},
	     {RevState, ordinary, RevNr, "", _})
  when (IsRevType == ordinary orelse
	IsRevType == verification_state) ->
    true;
is_rev_equiv({RevState, IsRevType, RevNr, RevLett, _},
	     {RevState, CompRevType, RevNr, RevLett, _})
  when ((IsRevType == ordinary orelse
	 IsRevType == verification_state) andalso
	(CompRevType == ordinary orelse
	 CompRevType == verification_state)) ->
    true;
is_rev_equiv(_, _) ->
    false.

is_rev_higher(IsRev, CompRev) when not is_tuple(IsRev) andalso
				   not is_tuple(CompRev) ->
    is_rev_higher(rev_split(IsRev), rev_split(CompRev));
is_rev_higher(IsRev, {_, _, _, _, _} = CompRev) when not is_tuple(IsRev) ->
    is_rev_higher(rev_split(IsRev), CompRev);
is_rev_higher({_, _, _, _, _} = IsRev, CompRev) when not is_tuple(CompRev) ->
    is_rev_higher(IsRev, rev_split(CompRev));
is_rev_higher({RevState, ordinary, RevNr, IsRevLett, _},
	      {RevState, ordinary, RevNr, CompRevLett, _}) ->
    is_chars_higher(IsRevLett, CompRevLett);
is_rev_higher({RevState, ordinary, IsRevNr, _, _},
	      {RevState, ordinary, CompRevNr, _, _}) ->
    is_chars_higher(IsRevNr, CompRevNr);
is_rev_higher({RevState, ordinary, RevNr, RevLett, _},
	      {RevState, verification_state, RevNr, RevLett, _}) ->
    true;
is_rev_higher({RevState, ordinary, RevNr, IsRevLett, _},
	      {RevState, verification_state, RevNr, CompRevLett, _}) ->
    is_chars_higher(IsRevLett, CompRevLett);
is_rev_higher({RevState, ordinary, IsRevNr, _, _},
	      {RevState, verification_state, CompRevNr, _, _}) ->
    is_chars_higher(IsRevNr, CompRevNr);
is_rev_higher({RevState, verification_state, RevNr, IsRevLett, _},
	      {RevState, ordinary, RevNr, CompRevLett, _}) ->
    is_chars_higher(IsRevLett, CompRevLett);
is_rev_higher({RevState, verification_state, IsRevNr, _, _},
	      {RevState, ordinary, CompRevNr, _, _}) ->
    is_chars_higher(IsRevNr, CompRevNr);
is_rev_higher({RevState, verification_state, RevNr, RevLett, IsRevAmt},
	      {RevState, verification_state, RevNr, RevLett, CompRevAmt}) ->
    is_chars_higher(IsRevAmt, CompRevAmt);
is_rev_higher({RevState, verification_state, RevNr, IsRevLett, _},
	      {RevState, verification_state, RevNr, CompRevLett, _}) ->
    is_chars_higher(IsRevLett, CompRevLett);
is_rev_higher({RevState, verification_state, IsRevNr, _, _},
	      {RevState, verification_state, CompRevNr, _, _}) ->
    is_chars_higher(IsRevNr, CompRevNr);
is_rev_higher({RevState, special, RevNr, RevLett, IsRevSuff},
	      {RevState, special, RevNr, RevLett, CompRevSuff}) ->
    case is_same_charType(IsRevSuff, CompRevSuff) of
	true ->
	    is_chars_higher(IsRevSuff, CompRevSuff);
	false ->
	    false
    end;
is_rev_higher({RevState, special, RevNr, IsRevLett, IsRevSuff},
	      {RevState, special, RevNr, CompRevLett, CompRevSuff}) ->
    case is_same_charType(IsRevSuff, CompRevSuff) of
	true ->
	    is_chars_higher(IsRevLett, CompRevLett);
	false ->
	    false
    end;
is_rev_higher({RevState, special, IsRevNr, _, IsRevSuff},
	      {RevState, special, CompRevNr, _, CompRevSuff}) ->
    case is_same_charType(IsRevSuff, CompRevSuff) of
	true ->
	    is_chars_higher(IsRevNr, CompRevNr);
	false ->
	    false
    end;
is_rev_higher(_, _) ->
    false.
%
is_rev_inRange(Rev, RevRange) ->
    case string:tokens(RevRange, "-") of
	[RawLowRev, RawHighRev] ->
	    %% Closed range
	    LowRev_split = rev_split(string:strip(RawLowRev)),
	    HighRev_split = rev_split(string:strip(RawHighRev)),
	    validate_rev(range, LowRev_split, RevRange),
	    validate_rev(range, HighRev_split, RevRange),
	    Rev_split = rev_split(Rev),
	    case is_rev_eqOrHi(Rev_split, LowRev_split) of
		true -> %above Low
		    is_rev_eqOrHi(HighRev_split, Rev_split);
		false  ->
		    false
	    end;
	[RawLimitRev] ->
	    LimitRev = string:strip(RawLimitRev),
	    case string:chr(RevRange, $-) of
		0 ->
		    %% Explicit R-state, absolute revision
		    LimitRev_split = rev_split(LimitRev),
		    validate_rev(explicit, LimitRev_split, RevRange),
		    is_rev_equiv(rev_split(Rev), LimitRev_split);
		1 ->
		    %% Open ended range, to revision
		    LimitRev_split = rev_split(LimitRev),
		    validate_rev(range, LimitRev_split, RevRange),
		    is_rev_eqOrHi(LimitRev_split, rev_split(Rev));
		_ ->
		    %% Open ended range, from revision
		    LimitRev_split = rev_split(LimitRev),
		    validate_rev(range, LimitRev_split, RevRange),
		    is_rev_eqOrHi(rev_split(Rev), LimitRev_split)
	    end;
	_ ->
	    throw(?UC_ERR_3(?UC_ERR_InvalidRevRange, [{rev_range, RevRange}]))
    end.

rev_split(Rev) ->
    validate_rev(Rev),
    {RevState, Tail1} = rev_state(Rev),
    {Number, Tail2} = rev_digits(Tail1),
    validate_revNumberLen(Number, Rev),
    {Letter, Tail3} = rev_letters(Tail2, Rev),
    validate_revLetterLen(Letter, Tail3, Rev),
    {Type, AmendmentOrSuffix} = rev_type(Tail3, Rev),
    {RevState, Type, Number, Letter, AmendmentOrSuffix}.

%%% ###########################################################################
%%% is_chars_higher
%%%
%%% ###=====================================================================###
is_chars_higher([_ | _], "") ->
    true;
is_chars_higher("", [_ | _]) ->
    false;
is_chars_higher(Chars1, Chars2) ->
    try
	list_to_integer(Chars1) > list_to_integer(Chars2)
    catch
	error : badarg ->
	    is_letters_higher(Chars1, Chars2)
    end.

%%% ###########################################################################
%%% is_letters_higher
%%%
%%% ###=====================================================================###
is_letters_higher(Chars1, Chars2) when length(Chars1) < length(Chars2) ->
    false;
is_letters_higher(Chars1, Chars2) when length(Chars1) > length(Chars2) ->
    true;
is_letters_higher(Chars1, Chars2) ->
    Chars1 > Chars2.

%%% ###########################################################################
%%% is_same_charType
%%%
%%% ###=====================================================================###
is_same_charType([Char1 | _], [Char2 | _])
  when (Char1 >= $0 andalso
	Char1 =< $9 andalso
	Char2 >= $0 andalso
	Char2 =< $9) ->
    true;
is_same_charType([Char1 | _], [Char2 | _])
  when (Char1 >= $A andalso
	Char1 =< $Z andalso
	Char2 >= $A andalso
	Char2 =< $Z) ->
    true;
is_same_charType(_, _) ->
    false.

%%% ###########################################################################
%%% rev_state
%%%
%%% ###=====================================================================###
rev_state([$R | Tail]) ->
    {r_state, Tail};
rev_state([$P | Tail]) ->
    {p_state, Tail};
rev_state(Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["Revision neither 'R'- nor 'P'-state",
		     {revision, Rev}])).

%%% ###########################################################################
%%% rev_digits
%%%
%%% ###=====================================================================###
rev_digits(RevTail) ->
    rev_digits(RevTail, []).

rev_digits([Char | Tail], Acc) when Char >= $0 andalso Char =< $9 ->
    rev_digits(Tail, Acc ++ [Char]);
rev_digits(Tail, Acc) ->
    {Acc, Tail}.

%%% ###########################################################################
%%% rev_letters
%%%
%%% ###=====================================================================###
rev_letters(RevTail, Rev) ->
    rev_letters(RevTail, [], Rev).

rev_letters([Char | Tail], Acc, Rev) when Char >= $A andalso Char =< $Z ->
    validate_revLetter(Char, Rev),
    rev_letters(Tail, Acc ++ [Char], Rev);
rev_letters(Tail, Acc, Rev) ->
    validate_revLetters(Acc, Rev),
    {Acc, Tail}.

%%% ###########################################################################
%%% rev_type
%%%
%%% ###=====================================================================###
rev_type([$/ | Suffix], Rev) ->
    validate_revSuffix(Suffix, Rev),
    {special, Suffix};
rev_type([Char | _] = Amendment, Rev) when Char >= $0 andalso Char =< $9 ->
    validate_revAmendment(Amendment, Rev),
    {verification_state, Amendment};
rev_type([], _) ->
    {ordinary, []};
rev_type(Chars, Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["Revision contains illegal character(s)",
		     {illegal_characters, Chars},
		     {revision, Rev}])).

%%% ###########################################################################
%%% validate_rev
%%%
%%% ###=====================================================================###
validate_rev(Rev) ->
    validate_revString(Rev),
    validate_revLength(Rev).

%%% ###=====================================================================###
validate_rev(explicit, {r_state, ordinary, [_ | _], [_ | _], ""}, _) ->
    ok;
validate_rev(explicit, {r_state, special, [_ | _], [_ | _], [_ | _]}, _) ->
    ok;
validate_rev(explicit, {p_state, _, _, _, _}, _) ->
    ok;
validate_rev(range, {_, ordinary, [_ | _], _, ""}, _) ->
    ok;
validate_rev(range, {_, special, [_ | _], [_ | _], [_ | _]}, _) ->
    ok;
validate_rev(_, _, RevRange) ->
    error_logger:warning_msg("In ~p, revision range is not supported. Results might be other than expected~n",[RevRange]).


%%% ###########################################################################
%%% validate_revAmendment
%%%
%%% ###=====================================================================###
validate_revAmendment(String, Rev) ->
    case rev_digits(String) of
	{[_, _], []} ->
	    ok;
	{[Char1, _, _] = String, []} when Char1 /= $0 ->
	    ok;
	_ ->
	    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
			    ["Verification State Amendment consists of",
			     "2 digits (01-99) or 3 digits (100-999)",
			     {faulty_revision_part, String},
			     {revision, Rev}]))
    end.

%%% ###########################################################################
%%% validate_revLength
%%%
%%% ###=====================================================================###
validate_revLength(Rev) ->
    validate_revLength(Rev, string:len(Rev)).

validate_revLength(_, Len) when Len >= 2 andalso Len =< 7 ->
    ok;
validate_revLength(Rev, _) ->
    throw(?UC_ERR_3(?UC_ERR_RevLengthOutOfRange,
		    [{revision, Rev},
		     {"Revision length limits", "2 - 7 characters"}])).

%%% ###########################################################################
%%% validate_revLetter
%%%
%%% ###=====================================================================###
validate_revLetter(Char, _) when Char /= $I andalso
				 Char /= $O andalso
				 Char /= $P andalso
				 Char /= $Q andalso
				 Char /= $R andalso
				 Char /= $W ->
    ok;
validate_revLetter(_, Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["The letters I, O, P, Q, R and W must not be used",
		     {revision, Rev}])).

%%% ###########################################################################
%%% validate_revLetters
%%%
%%% ###=====================================================================###
validate_revLetters([], _) ->
    ok;
validate_revLetters([_], _) ->
    ok;
validate_revLetters([_, _], _) ->
    ok;
validate_revLetters([_, Char2, Char3 | _], _) when Char2 /= $A andalso
						   Char2 /= $E andalso
						   Char2 /= $U andalso
						   Char2 /= $Y andalso
						   Char3 /= $A andalso
						   Char3 /= $E andalso
						   Char3 /= $U andalso
						   Char3 /= $Y ->
    ok;
validate_revLetters([_, _, _ | _], Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["Vowels (A, E, U, Y) are forbidden in letter position",
		     "2 and 3 for R-states with 3 and 4 letters",
		     {revision, Rev}])).

%%% ###########################################################################
%%% validate_revLetterLen
%%%
%%% ###=====================================================================###
validate_revLetterLen([_ | _] = String, _, _) when length(String) =< 4 ->
    ok;
validate_revLetterLen("", [], _) ->
    ok;
validate_revLetterLen("", _, Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["Revision letter is missing",
		     {revision, Rev}]));
validate_revLetterLen(_, _, Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["Revision letters exceeds 4 characters",
		     {revision, Rev}])).

%%% ###########################################################################
%%% validate_revNumberLen
%%%
%%% ###=====================================================================###
validate_revNumberLen([_ | _] = String, _) when length(String) =< 4 ->
    ok;
validate_revNumberLen("", Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["2nd position is not a digit",
		     {revision, Rev}]));
validate_revNumberLen(_, Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["Revision number exceeds 4 digits",
		     {revision, Rev}])).

%%% ###########################################################################
%%% validate_revString
%%%
%%% ###=====================================================================###
validate_revString(Rev) ->
    validate_revString(Rev, Rev).

validate_revString([Char | Tail], Rev) when is_integer(Char) ->
    validate_revString(Tail, Rev);
validate_revString([], _) ->
    ok;
validate_revString(_, Rev) ->
    throw(?UC_ERR_3(?UC_ERR_RevNotAString, [{revision, Rev}])).

%%% ###########################################################################
%%% validate_revSuffix
%%%
%%% ###=====================================================================###
validate_revSuffix(String, Rev) ->
    case rev_digits(String) of
	{[_], []} ->
	    ok;
	{[_, _], []} ->
	    ok;
	{[], String} ->
	    ok;
	_ ->
	    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
			    ["Special revision suffix may consist of",
			     "maximum 2 digits or one letter",
			     {faulty_revision_part, String},
			     {revision, Rev}]))
    end,
    case rev_letters(String, Rev) of
	{[_], []} ->
	    ok;
	{[], String} ->
	    ok;
	_ ->
	    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
			    ["Special revision suffix may consist of",
			     "maximum 2 digits or one letter",
			     {faulty_revision_part, String},
			     {revision, Rev}]))
    end.


cmd(Cmd) ->
    debug("~s~n~s~n",[Cmd, R = os:cmd(Cmd)]),
    R.





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

