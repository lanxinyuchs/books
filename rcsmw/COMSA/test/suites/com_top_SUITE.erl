%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	com_top_SUITE.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R7A/1
%%% 
%%% @doc == Test Suite for testing the transfer ESI from SUT using netconf.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% rct_netconf is used in ct_hooks:
%%% see <a href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/supp/esrc/rct_netconf.erl">rct_netconf.erl</a><br/>
%%% @end

-module(com_top_SUITE).
-vsn('/main/R2A/R3A/R7A/1').

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
%%% R1A/1      2012-09-20 etxjotj     Created
%%% R2A/1      2013-07-11 etxjotj     Completely rewritten
%%% R2A/3      2013-11-29 etxarnu     Increased allowed time diff
%%%                                   to 4 secs in localDateTime
%%% R3A/1      2015-02-28 etxkols     Preparation for cluster
%%% R3A/2      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R7A/1      2016-09-06 etxpejn     Changed random to rand
%%% ----------------------------------------------------------
%%% 

%compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 all/0]).

-export([simple_read_write/1,
	 localDateTime/1,
	 networkManagedElementId/1,
	 timeZone/1]).

-export([user_identity/1]).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_netconf,{nc1, html}},
                 {cth_conn_log,[]},
                 {rct_core,[]},
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       []}}]}}
		]}].

%% @hidden
init_per_suite(Config) ->
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    [{meData, MeData} |Config].
%% @hidden
end_per_suite(Config) ->
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId = 
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
    OldMeData = proplists:get_value(meData, Config),
    Edit = 
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"},
	  {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}
	 ],
	 [{managedElementId,[],[MeId]},
	  reset(dnPrefix, OldMeData),
	  reset(networkManagedElementId, OldMeData),
	  reset(siteLocation, OldMeData),
	  reset(userLabel, OldMeData)]},
    ok = netconf(edit_config, [nc1, running, Edit]).
%% @hidden
init_per_testcase(_TestCase, Config) ->
    %% This is because due to errors in COM there is no proper way to
    %% read the set managedElementId if it has changed from "1"
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId = 
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
    [{meId, MeId}|Config].
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @hidden
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []}
    ].

reset(Key=networkManagedElementId, OldMeData) ->
    case proplists:get_value(Key, OldMeData) of
	undefined ->
	    ct:pal("Special handling of networkManagedElementId since delete doesn't work"),
	    {Key, ["1"]};
	Value ->
	    {Key, [Value]}
    end;
reset(Key, OldMeData) ->
    case proplists:get_value(Key, OldMeData) of
	undefined ->
	    {Key, [{'xc:operation', "delete"}], []};
	Value ->
	    {Key, [Value]}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [simple_read_write,
     localDateTime,
     networkManagedElementId].

%%--------------------------------------------------------------------
%% @doc Simple read and write attributes
%% @end

simple_read_write(Config) ->
    MeId = proplists:get_value(meId, Config),
    Filter = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]}]},
    {ok, Res} = netconf(get, [nc1, Filter]),
    {ok, _} = extract_element(managedElementType, Res),
    {ok, _} = extract_element(release, Res),
    
    ct:pal("Read-only attributes ok~n"),
    
    DnPrefix = "DC=myrbs.ericsson.com",
    SiteLocation = "Kista",
    UserLabel = "Anemon",
    ProductIdentity = 
	[{productNumber,[],["ABC 123 4567/8"]},
	 {productRevision,[],["R1A"]},
	 {productDesignation,[],["RBS6000 G2"]}],
    Edit = 
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],[MeId]},
	  {dnPrefix, [DnPrefix]},
	  {siteLocation, [SiteLocation]},
	  {userLabel, [UserLabel]},
	  {productIdentity,
	   [{struct,"ProductIdentity"}],
	   ProductIdentity}]},
    ok = netconf(edit_config, [nc1, running, Edit]),
    {ok, Res2} = netconf(get_config, [nc1, running, Filter]),
    {ok, {dnPrefix, [], [DnPrefix]}} = extract_element(dnPrefix, Res2),
    {ok, {siteLocation, [], [SiteLocation]}} = 
    	extract_element(siteLocation, Res2),
    {ok, {userLabel, [], [UserLabel]}} = extract_element(userLabel, Res2),
    {ok, {productIdentity, _, ProductIdentity}} = 
	extract_element(productIdentity, Res2),
    ct:pal("Read-write attributes ok~n"),

    Edit2 = 
	{'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"},
	  {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}
	 ],
	 [{managedElementId,[],[MeId]},
	  {dnPrefix, [{'xc:operation', "delete"}], []},
	  %{networkManagedElementId, [{'xc:operation', "delete"}], []},
	  {siteLocation, [{'xc:operation', "delete"}], []},
	  {userLabel, [{'xc:operation', "delete"}], []}]},
    ok = netconf(edit_config, [nc1, running, Edit2]),
    ct:pal("Delete attributes ok~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Check that the local date time fits with the internal system clock
%% The system may not show the correct date and time, because of missing ntp
%% synchronization. That is thested in the com_sysm_SUITE
%% @end

localDateTime(Config) ->
    MeId = proplists:get_value(meId, Config),
    Filter = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {localDateTime, []}]},
    {ok, Res} = netconf(get, [nc1, Filter]),
    DateTime = rct_rpc:call(rpc_1, calendar, local_time, [], 3000),
    {ok, {localDateTime, _, [LocalDateTime]}} = 
	extract_element(localDateTime, Res),
    {ok, [Y,Mo,D,H,Mi,S],[]} = io_lib:fread("~d-~d-~dT~d:~d:~d", LocalDateTime),
    NetconfSecs = calendar:datetime_to_gregorian_seconds({{Y,Mo,D}, {H,Mi,S}}),
    LocalSecs = calendar:datetime_to_gregorian_seconds(DateTime),
    case abs(NetconfSecs-LocalSecs) of
	Diff when Diff < 5 ->
	    ct:pal("Diff is ~p seconds~n",[Diff]),
	    ok;
	Diff ->
	    ct:pal("Netconf local time: ~s~n"
		   "Erlang local time: ~p~n",
		   [LocalDateTime, DateTime]),
	    ct:fail({localDateTime_max_diff_exceeded, Diff})
    end.

%%--------------------------------------------------------------------
%% @doc 
%% Reconfigures the network managed element Id and tests that the
%% system is still accessible with the new address.
%% @end

networkManagedElementId(Config) ->
    MeId = proplists:get_value(meId, Config),
    XYZ = [rand:uniform(25)+$A, rand:uniform(25)+$A, rand:uniform(25)+$A],
    ct:pal("Random ID is ~p~n",[XYZ]),
    Edit = {'ManagedElement',
	    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	    [{managedElementId,[],[MeId]},
	     {networkManagedElementId, [XYZ]}
	    ]},
    ok = netconf(edit_config, [nc1, running, Edit]),
    Filter = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[XYZ]}]},
    {ok, Res} = netconf(get, [nc1, Filter]),
    {ok, {managedElementId, [], [XYZ]}}=extract_element(managedElementId, Res),
    ct:pal("mangedElementId ok",[]),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% Sets timezone to a various set of zones, and confirms that the
%% date time offsset changes.
%% @end

timeZone(Config) ->
    MeId = proplists:get_value(meId, Config),
    [begin
	 ct:print("~s~n",[TimeZone]),
	 Edit = {'ManagedElement',
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		 [{managedElementId,[],[MeId]},
		  {timeZone, [TimeZone]}
		 ]},
	 ok = netconf(edit_config, [nc1, running, Edit])
     end||TimeZone<-supported_zones()].

supported_zones() ->
["Africa/Abidjan","Africa/Accra",
 "Africa/Addis_Ababa",
 "Africa/Algiers","Africa/Asmara",
 "Africa/Asmera","Africa/Bamako",
 "Africa/Bangui","Africa/Banjul",
 "Africa/Bissau","Africa/Blantyre",
 "Africa/Brazzaville",
 "Africa/Bujumbura","Africa/Cairo",
 "Africa/Casablanca","Africa/Ceuta",
 "Africa/Conakry","Africa/Dakar",
 "Africa/Dar_es_Salaam",
 "Africa/Djibouti","Africa/Douala",
 "Africa/El_Aaiun","Africa/Freetown",
 "Africa/Gaborone","Africa/Harare",
 "Africa/Johannesburg","Africa/Juba",
 "Africa/Kampala","Africa/Khartoum",
 "Africa/Kigali","Africa/Kinshasa",
 "Africa/Lagos","Africa/Libreville",
 "Africa/Lome","Africa/Luanda",
 "Africa/Lubumbashi","Africa/Lusaka",
 "Africa/Malabo","Africa/Maputo",
 "Africa/Maseru","Africa/Mbabane",
 "Africa/Mogadishu","Africa/Monrovia",
 "Africa/Nairobi","Africa/Ndjamena",
 "Africa/Niamey","Africa/Nouakchott",
 "Africa/Ouagadougou",
 "Africa/Porto-Novo",
 "Africa/Sao_Tome","Africa/Timbuktu",
 "Africa/Tripoli","Africa/Tunis",
 "Africa/Windhoek","America/Adak",
 "America/Anchorage",
 "America/Anguilla","America/Antigua",
 "America/Araguaina",
 "America/Argentina","America/Aruba",
 "America/Asuncion",
 "America/Atikokan","America/Atka",
 "America/Bahia",
 "America/Bahia_Banderas",
 "America/Barbados","America/Belem",
 "America/Belize",
 "America/Blanc-Sablon",
 "America/Boa_Vista","America/Bogota",
 "America/Boise",
 "America/Buenos_Aires",
 "America/Cambridge_Bay",
 "America/Campo_Grande",
 "America/Cancun","America/Caracas",
 "America/Catamarca",
 "America/Cayenne","America/Cayman",
 "America/Chicago",
 "America/Chihuahua",
 "America/Coral_Harbour",
 "America/Cordoba",
 "America/Costa_Rica",
 "America/Creston","America/Cuiaba",
 "America/Curacao",
 "America/Danmarkshavn",
 "America/Dawson",
 "America/Dawson_Creek",
 "America/Denver","America/Detroit",
 "America/Dominica",
 "America/Edmonton",
 "America/Eirunepe",
 "America/El_Salvador",
 "America/Ensenada",
 "America/Fort_Wayne",
 "America/Fortaleza",
 "America/Glace_Bay",
 "America/Godthab",
 "America/Goose_Bay",
 "America/Grand_Turk",
 "America/Grenada",
 "America/Guadeloupe",
 "America/Guatemala",
 "America/Guayaquil","America/Guyana",
 "America/Halifax","America/Havana",
 "America/Hermosillo",
 "America/Indiana",
 "America/Indianapolis",
 "America/Inuvik","America/Iqaluit",
 "America/Jamaica","America/Jujuy",
 "America/Juneau","America/Kentucky",
 "America/Knox_IN",
 "America/Kralendijk",
 "America/La_Paz","America/Lima",
 "America/Los_Angeles",
 "America/Louisville",
 "America/Lower_Princes",
 "America/Maceio","America/Managua",
 "America/Manaus","America/Marigot",
 "America/Martinique",
 "America/Matamoros",
 "America/Mazatlan","America/Mendoza",
 "America/Menominee","America/Merida",
 "America/Metlakatla",
 "America/Mexico_City",
 "America/Miquelon","America/Moncton",
 "America/Monterrey",
 "America/Montevideo",
 "America/Montreal",
 "America/Montserrat",
 "America/Nassau","America/New_York",
 "America/Nipigon","America/Nome",
 "America/Noronha",
 "America/North_Dakota",
 "America/Ojinaga","America/Panama",
 "America/Pangnirtung",
 "America/Paramaribo",
 "America/Phoenix",
 "America/Port-au-Prince",
 "America/Port_of_Spain",
 "America/Porto_Acre",
 "America/Porto_Velho",
 "America/Puerto_Rico",
 "America/Rainy_River",
 "America/Rankin_Inlet",
 "America/Recife","America/Regina",
 "America/Resolute",
 "America/Rio_Branco",
 "America/Rosario",
 "America/Santa_Isabel",
 "America/Santarem",
 "America/Santiago",
 "America/Santo_Domingo",
 "America/Sao_Paulo",
 "America/Scoresbysund",
 "America/Shiprock","America/Sitka",
 "America/St_Barthelemy",
 "America/St_Johns",
 "America/St_Kitts",
 "America/St_Lucia",
 "America/St_Thomas",
 "America/St_Vincent",
 "America/Swift_Current",
 "America/Tegucigalpa",
 "America/Thule",
 "America/Thunder_Bay",
 "America/Tijuana","America/Toronto",
 "America/Tortola",
 "America/Vancouver","America/Virgin",
 "America/Whitehorse",
 "America/Winnipeg","America/Yakutat",
 "America/Yellowknife",
 "Antarctica/Casey",
 "Antarctica/Davis",
 "Antarctica/DumontDUrville",
 "Antarctica/Macquarie",
 "Antarctica/Mawson",
 "Antarctica/McMurdo",
 "Antarctica/Palmer",
 "Antarctica/Rothera",
 "Antarctica/South_Pole",
 "Antarctica/Syowa",
 "Antarctica/Vostok",
 "Arctic/Longyearbyen","Asia/Aden",
 "Asia/Almaty","Asia/Amman",
 "Asia/Anadyr","Asia/Aqtau",
 "Asia/Aqtobe","Asia/Ashgabat",
 "Asia/Ashkhabad","Asia/Baghdad",
 "Asia/Bahrain","Asia/Baku",
 "Asia/Bangkok","Asia/Beirut",
 "Asia/Bishkek","Asia/Brunei",
 "Asia/Calcutta","Asia/Choibalsan",
 "Asia/Chongqing","Asia/Chungking",
 "Asia/Colombo","Asia/Dacca",
 "Asia/Damascus","Asia/Dhaka",
 "Asia/Dili","Asia/Dubai",
 "Asia/Dushanbe","Asia/Gaza",
 "Asia/Harbin","Asia/Hebron",
 "Asia/Ho_Chi_Minh","Asia/Hong_Kong",
 "Asia/Hovd","Asia/Irkutsk",
 "Asia/Istanbul","Asia/Jakarta",
 "Asia/Jayapura","Asia/Jerusalem",
 "Asia/Kabul","Asia/Kamchatka",
 "Asia/Karachi","Asia/Kashgar",
 "Asia/Kathmandu","Asia/Katmandu",
 "Asia/Kolkata","Asia/Krasnoyarsk",
 "Asia/Kuala_Lumpur","Asia/Kuching",
 "Asia/Kuwait","Asia/Macao",
 "Asia/Macau","Asia/Magadan",
 "Asia/Makassar","Asia/Manila",
 "Asia/Muscat","Asia/Nicosia",
 "Asia/Novokuznetsk",
 "Asia/Novosibirsk","Asia/Omsk",
 "Asia/Oral","Asia/Phnom_Penh",
 "Asia/Pontianak","Asia/Pyongyang",
 "Asia/Qatar","Asia/Qyzylorda",
 "Asia/Rangoon","Asia/Riyadh",
 "Asia/Riyadh87","Asia/Riyadh88",
 "Asia/Riyadh89","Asia/Saigon",
 "Asia/Sakhalin","Asia/Samarkand",
 "Asia/Seoul","Asia/Shanghai",
 "Asia/Singapore","Asia/Taipei",
 "Asia/Tashkent","Asia/Tbilisi",
 "Asia/Tehran","Asia/Tel_Aviv",
 "Asia/Thimbu","Asia/Thimphu",
 "Asia/Tokyo","Asia/Ujung_Pandang",
 "Asia/Ulaanbaatar","Asia/Ulan_Bator",
 "Asia/Urumqi","Asia/Vientiane",
 "Asia/Vladivostok","Asia/Yakutsk",
 "Asia/Yekaterinburg","Asia/Yerevan",
 "Atlantic/Azores","Atlantic/Bermuda",
 "Atlantic/Canary",
 "Atlantic/Cape_Verde",
 "Atlantic/Faeroe","Atlantic/Faroe",
 "Atlantic/Jan_Mayen",
 "Atlantic/Madeira",
 "Atlantic/Reykjavik",
 "Atlantic/South_Georgia",
 "Atlantic/St_Helena",
 "Atlantic/Stanley","Australia/ACT",
 "Australia/Adelaide",
 "Australia/Brisbane",
 "Australia/Broken_Hill",
 "Australia/Canberra",
 "Australia/Currie",
 "Australia/Darwin","Australia/Eucla",
 "Australia/Hobart","Australia/LHI",
 "Australia/Lindeman",
 "Australia/Lord_Howe",
 "Australia/Melbourne",
 "Australia/NSW","Australia/North",
 "Australia/Perth",
 "Australia/Queensland",
 "Australia/South","Australia/Sydney",
 "Australia/Tasmania",
 "Australia/Victoria",
 "Australia/West",
 "Australia/Yancowinna","Brazil/Acre",
 "Brazil/DeNoronha","Brazil/East",
 "Brazil/West","Canada/Atlantic",
 "Canada/Central",
 "Canada/East-Saskatchewan",
 "Canada/Eastern","Canada/Mountain",
 "Canada/Newfoundland",
 "Canada/Pacific",
 "Canada/Saskatchewan","Canada/Yukon",
 "Chile/Continental",
 "Chile/EasterIsland","Etc/GMT",
 "Etc/GMT+0","Etc/GMT+1",
 "Etc/GMT+10","Etc/GMT+11",
 "Etc/GMT+12","Etc/GMT+2",
 "Etc/GMT+3","Etc/GMT+4",
 "Etc/GMT+5","Etc/GMT+6",
 "Etc/GMT+7","Etc/GMT+8",
 "Etc/GMT+9","Etc/GMT-0",
 "Etc/GMT-1","Etc/GMT-10",
 "Etc/GMT-11","Etc/GMT-12",
 "Etc/GMT-13","Etc/GMT-14",
 "Etc/GMT-2","Etc/GMT-3",
 "Etc/GMT-4","Etc/GMT-5",
 "Etc/GMT-6","Etc/GMT-7",
 "Etc/GMT-8","Etc/GMT-9",
 "Etc/GMT0","Etc/Greenwich",
 "Etc/UCT","Etc/UTC",
 "Etc/Universal","Etc/Zulu",
 "Europe/Amsterdam","Europe/Andorra",
 "Europe/Athens","Europe/Belfast",
 "Europe/Belgrade","Europe/Berlin",
 "Europe/Bratislava",
 "Europe/Brussels","Europe/Bucharest",
 "Europe/Budapest","Europe/Chisinau",
 "Europe/Copenhagen","Europe/Dublin",
 "Europe/Gibraltar","Europe/Guernsey",
 "Europe/Helsinki",
 "Europe/Isle_of_Man",
 "Europe/Istanbul","Europe/Jersey",
 "Europe/Kaliningrad","Europe/Kiev",
 "Europe/Lisbon","Europe/Ljubljana",
 "Europe/London","Europe/Luxembourg",
 "Europe/Madrid","Europe/Malta",
 "Europe/Mariehamn","Europe/Minsk",
 "Europe/Monaco","Europe/Moscow",
 "Europe/Nicosia","Europe/Oslo",
 "Europe/Paris","Europe/Podgorica",
 "Europe/Prague","Europe/Riga",
 "Europe/Rome","Europe/Samara",
 "Europe/San_Marino",
 "Europe/Sarajevo",
 "Europe/Simferopol","Europe/Skopje",
 "Europe/Sofia","Europe/Stockholm",
 "Europe/Tallinn","Europe/Tirane",
 "Europe/Tiraspol","Europe/Uzhgorod",
 "Europe/Vaduz","Europe/Vatican",
 "Europe/Vienna","Europe/Vilnius",
 "Europe/Volgograd","Europe/Warsaw",
 "Europe/Zagreb","Europe/Zaporozhye",
 "Europe/Zurich",
 "Indian/Antananarivo",
 "Indian/Chagos","Indian/Christmas",
 "Indian/Cocos","Indian/Comoro",
 "Indian/Kerguelen","Indian/Mahe",
 "Indian/Maldives","Indian/Mauritius",
 "Indian/Mayotte","Indian/Reunion",
 "Mexico/BajaNorte","Mexico/BajaSur",
 "Mexico/General","Mideast/Riyadh87",
 "Mideast/Riyadh88",
 "Mideast/Riyadh89","Pacific/Apia",
 "Pacific/Auckland","Pacific/Chatham",
 "Pacific/Chuuk","Pacific/Easter",
 "Pacific/Efate","Pacific/Enderbury",
 "Pacific/Fakaofo","Pacific/Fiji",
 "Pacific/Funafuti",
 "Pacific/Galapagos",
 "Pacific/Gambier",
 "Pacific/Guadalcanal","Pacific/Guam",
 "Pacific/Honolulu",
 "Pacific/Johnston",
 "Pacific/Kiritimati",
 "Pacific/Kosrae","Pacific/Kwajalein",
 "Pacific/Majuro","Pacific/Marquesas",
 "Pacific/Midway","Pacific/Nauru",
 "Pacific/Niue","Pacific/Norfolk",
 "Pacific/Noumea","Pacific/Pago_Pago",
 "Pacific/Palau","Pacific/Pitcairn",
 "Pacific/Pohnpei","Pacific/Ponape",
 "Pacific/Port_Moresby",
 "Pacific/Rarotonga","Pacific/Saipan",
 "Pacific/Samoa","Pacific/Tahiti",
 "Pacific/Tarawa","Pacific/Tongatapu",
 "Pacific/Truk","Pacific/Wake",
 "Pacific/Wallis","Pacific/Yap",
 "US/Alaska","US/Aleutian",
 "US/Arizona","US/Central",
 "US/East-Indiana","US/Eastern",
 "US/Hawaii","US/Indiana-Starke",
 "US/Michigan","US/Mountain",
 "US/Pacific","US/Samoa",
 %% This last one is doubled. Unncessary, but practical.
 "Europe/Stockholm"].


user_identity(Config) ->
    MeId = proplists:get_value(meId, Config),
    Filter = {'ManagedElement', [],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions', [],
		[{systemFunctionsId, ["1"]},
		 {'SecM', [],
		  [{secMId, ["1"]},
		   {'UserManagement', 
		    [{userManagementId, ["1"]}]}]}]}]},

    {ok, Res} = netconf(get_config, [nc1, running,Filter]),
    ct:pal("~p~n",[Res]).

%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------
%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    Res = apply(ct_netconfc, F, A),
    ok = ct_netconfc:close_session(nc1),
    Res.

%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.

