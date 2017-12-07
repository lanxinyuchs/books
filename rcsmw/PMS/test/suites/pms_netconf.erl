%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_netconf.erl %
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
%%% @version /main/R2A/R3A/R4A/R5A/4

%%% @doc 
%%% 
%%% 
%%% @end

%%% ----------------------------------------------------------
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
%%% R1A/1      2013-02-28 uabesvi     Created
%%% R1A/3      2013-06-24 uabesvi     Adaptions to new COM
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-module(pms_netconf).


-export([create_job/1]).
-export([create_job/2]).

-export([create_mr/4]).

-export([create_job_mr/3]).

-export([delete_job/1]).
-export([delete_mr/2]).

-export([rop_file_handling/1]).


-define(ME, "ManagedElement=1").
%%-define(ME, "ManagedElement=kalle").

-define(ME_ID, "1").
%%-define(ME_ID, "kalle").

-define(COMTOP,  {xmlns, "urn:com:ericsson:ecim:ComTop"}).
-define(ECIM_PM, {xmlns, "urn:com:ericsson:ecim:RcsPm"}).

-define(NC_NAMESPACE, "urn:ietf:params:xml:ns:netconf:base:1.0").

-define(DELETE,  [{'xmlns:nc',     ?NC_NAMESPACE},
		  {'nc:operation', "delete"}]).


-include("pms2_test.hrl").




%%========================================================================
%% create_job_mr(JobId, Attrs, Mr, PmGrp, Mt) -> ok | {error, Reason}
%%
%% JobId - string()
%% Mr    - string()
%% PmGrp - string()
%% Mt    - string()
%%========================================================================
create_job_mr(JobId, Attrs, {Mr, PmGrp}) ->
    create_job_mr(JobId, Attrs, [{Mr, PmGrp}]);
create_job_mr(JobId, Attrs, {Mr, PmGrp, Mt}) ->
    create_job_mr(JobId, Attrs, [{Mr, PmGrp, Mt}]);


create_job_mr(JobId, Attrs, MR) when is_list(MR) ->
    MrAttrs = cj_mr(MR, []),
    JobArgs = [{pmJobId, [], [JobId]}] ++ cj_attrs(Attrs) ++ MrAttrs,
    
    ct_netconfc:edit_config(
      nc1,
      running,
      {'ManagedElement',
       [?COMTOP],
       [{managedElementId, [], [?ME_ID]},
	{'SystemFunctions',
	 [], 
	 [{systemFunctionsId, [], ["1"]},
	  {'Pm',
	   [?ECIM_PM],
	   [{pmId, [], ["1"]},
	    {'PmJob', 
	     [],
	     JobArgs
	    }]
	  }]}]
      }).

cj_mr([], Acc) ->
    lists:reverse(Acc);
cj_mr([{Mr, []} | T], Acc) ->
    MrAttrs = {'MeasurementReader',
	       [],
	       [{measurementReaderId, [], [Mr]},
		{measurementSpecification, [], []}
	       ]
	      },
    cj_mr(T, [MrAttrs | Acc]);
cj_mr([{Mr, PmGrp} | T], Acc) ->
    PmGrpDn = ?ME ++
	",SystemFunctions=1,"
	"Pm=1,"
	"PmGroup=" ++ PmGrp,
    
    MrAttrs = {'MeasurementReader',
	       [],
	       [{measurementReaderId, [], [Mr]},
		{measurementSpecification, 
		 [{struct,"MeasurementSpecification"}], 
		 [{groupRef, [], [PmGrpDn]}]}
	       ]
	      },
    cj_mr(T, [MrAttrs | Acc]);
cj_mr([{Mr, [], []} | T], Acc) ->
    MrAttrs = {'MeasurementReader',
	       [],
	       [{measurementReaderId, [], [Mr]},
		{measurementSpecification, [], []}
	       ]
	      },
    cj_mr(T, [MrAttrs | Acc]);
cj_mr([{Mr, PmGrp, Mt} | T], Acc) ->
    PmGrpDn = ?ME ++
	",SystemFunctions=1,"
	"Pm=1,"
	"PmGroup=" ++ PmGrp,
    
    MtDn = PmGrpDn ++ ",MeasurementType=" ++ Mt,
    
    MrAttrs = {'MeasurementReader',
	       [],
	       [{measurementReaderId, [], [Mr]},
		{measurementSpecification, 
		 [{struct,"MeasurementSpecification"}], 
		 [{measurementTypeRef, [], [MtDn]}]}
	       ]
	      },
    cj_mr(T, [MrAttrs | Acc]).
    
%%========================================================================
%% create_job(JobId, Args) -> ok | {error, Reason}
%%
%% JobId - string()
%% Args  - [Arg]
%% Arg   - {"granularityPeriod", P} | {"reportingPeriod", P} |
%%         {"requestedJobState", Rjs} | {"jobPriority", Jp}
%% P     - 1..9  (See defines on top of the file)
%% Rjs   - 1..2  (See defines on top of the file)
%% Jp    - 1..3  (See defines on top of the file)
%%========================================================================
create_job(JobId) ->
    Attrs = [{"granularityPeriod", ?TEN_SECONDS},
	     {"reportingPeriod",   ?TEN_SECONDS},
	     {"requestedJobState", ?ACTIVE}],
    create_job(JobId, Attrs).

create_job(JobId, Attrs) ->
    JobArgs = [{pmJobId, [], [JobId]}] ++ cj_attrs(Attrs),
    
    ct_netconfc:edit_config(nc1,
			    running,
			    {'ManagedElement',
			     [?COMTOP],
			     [{managedElementId, [], [?ME_ID]},
			      {'SystemFunctions',
			       [], 
			       [{systemFunctionsId, [], ["1"]},
				{'Pm',
				 [?ECIM_PM],
				 [{pmId, [], ["1"]},
				  {'PmJob', [], JobArgs
				  }]
				}]}]
			    }).

%%========================================================================
%% delete_job(JobId) -> ok | {error, Reason}
%%
%% JobId - string()
%%========================================================================
delete_job(JobId) ->

    ct_netconfc:edit_config(nc1,
			    running,
			    {'ManagedElement',
			     [?COMTOP],
			     [{managedElementId, [], [?ME_ID]},
			      {'SystemFunctions',
			       [], 
			       [{systemFunctionsId, [], ["1"]},
				{'Pm',
				 [?ECIM_PM],
				 [{pmId, [], ["1"]},
				  {'PmJob', 
				   ?DELETE,
				   [{pmJobId, [], [JobId]}]
				  }]
				}]}]
			    }).


%%========================================================================
%% delete_mr(JobId, MrId) -> ok | {error, Reason}
%%
%% JobId - string()
%%========================================================================
delete_mr(JobId, MrId) ->
    ct_netconfc:edit_config(nc1,
			    running,
			    {'ManagedElement',
			     [?COMTOP],
			     [{managedElementId, [], [?ME_ID]},
			      {'SystemFunctions',
			       [], 
			       [{systemFunctionsId, [], ["1"]},
				{'Pm',
				 [?ECIM_PM],
				 [{pmId, [], ["1"]},
				  {'PmJob', 
				   [],
				   [{pmJobId, [], [JobId]},
				    {'MeasurementReader',
				     ?DELETE,
				     [{measurementReaderId, [], [MrId]}]
				    }]
				  }]}]
			       }]
			    }).


%%========================================================================
%% create_mr(JobId, Mr, PmGrp, Mt) -> ok | {error, Reason}
%%
%% JobId - string()
%% Mr    - string()
%% PmGrp - string()
%% Mt    - string()
%%========================================================================
create_mr(JobId, Mr, PmGrp, Mt) ->
    PmGrpDn = ?ME ++
	",SystemFunctions=1,"
	"Pm=1,"
	"PmGroup=" ++ PmGrp,
    
    MtDn = PmGrpDn ++ ",MeasurementType=" ++ Mt,
    
    ct_netconfc:edit_config(
      nc1,
      running,
      {'ManagedElement',
       [?COMTOP],
       [{managedElementId, [], [?ME_ID]},
	{'SystemFunctions',
	 [], 
	 [{systemFunctionsId, [], ["1"]},
	  {'Pm',
	   [?ECIM_PM],
	   [{pmId, [], ["1"]},
	    {'PmJob',
	     [],
	     [{pmJobId, [], [JobId]},
	      {'MeasurementReader',
	       [],
	       [{measurementReaderId, [], [Mr]},
		{measurementSpecification, 
		 [{struct,"MeasurementSpecification"}], 
		 [{groupRef, 
		   [], 
		   [PmGrpDn]},
		  {measurementTypeRef, [], [MtDn]}]}
	       ]
	      }
	     ]
	    }]
	  }]}]
      }).



cj_attrs(Attrs) ->
    cj_attrs(Attrs, []).

cj_attrs([], Acc) ->
    Acc;

cj_attrs([{AttrName, ?DELETE_ATTR} | T], Acc) ->
    Attr = list_to_atom(AttrName),
    cj_attrs(T, [{Attr, ?DELETE, []} | Acc]);

cj_attrs([{"granularityPeriod", Val} | T], Acc) ->
    cj_attrs(T, [{granularityPeriod, [], [cja_p(Val)]} | Acc]);
cj_attrs([{"reportingPeriod", Val} | T], Acc) ->
    cj_attrs(T, [{reportingPeriod, [], [cja_p(Val)]} | Acc]);
cj_attrs([{"currentJobState", Val} | T], Acc) ->
    cj_attrs(T, [{requestedJobState, [], [cja_state(Val)]} | Acc]);
cj_attrs([{"requestedJobState", Val} | T], Acc) ->
    cj_attrs(T, [{requestedJobState, [], [cja_state(Val)]} | Acc]);
cj_attrs([{"jobPriority", Val} | T], Acc) ->
    cj_attrs(T, [{jobPriority, [], [cja_prio(Val)]} | Acc]);
cj_attrs([{"jobGroup", Val} | T], Acc) ->
    cj_attrs(T, [{jobGroup, [], [Val]} | Acc]);
cj_attrs([H | _], _Acc) ->
    ct:pal("Unknown attribute ~p~n", [H]),
    error.


cja_p(?TEN_SECONDS)    -> "TEN_SECONDS";
cja_p(?THIRTY_SECONDS) -> "THIRTY_SECONDS";
cja_p(?ONE_MIN)        -> "ONE_MIN";
cja_p(?FIVE_MIN)       -> "FIVE_MIN";
cja_p(?FIFTEEN_MIN)    -> "FIFTEEN_MIN";
cja_p(?THIRTY_MIN)     -> "THIRTY_MIN";
cja_p(?ONE_HOUR)       -> "ONE_HOUR";
cja_p(?TWELVE_HOUR)    -> "TWELVE_HOUR";
cja_p(?ONE_DAY)        -> "ONE_DAY".

cja_state(?ACTIVE)  -> "ACTIVE";
cja_state(?STOPPED) -> "STOPPED".

cja_prio(?HIGH)   -> "HIGH";
cja_prio(?MEDIUM) -> "MEDIUM";
cja_prio(?LOW)    -> "LOW".



rop_file_handling(State) ->
    
    ct_netconfc:edit_config(
      nc1,
      running,
      {'ManagedElement',
       [?COMTOP],
       [{managedElementId, [], [?ME_ID]},
	{'NodeSupport',
	 [], 
	 [{nodeSupportId, [], ["1"]},
	  {'PmSupport',
	   [],
	   [{pmSupportId, [], ["1"]},
	    {ropFileHandling, [], [rfh_state(State)]}]
	  }]}]
      }).

rfh_state(?SINGLE_ROP) -> "SINGLE_ROP_FILE";
rfh_state(?MULTI_ROP)  -> "MULTIPLE_ROP_FILES".
    
