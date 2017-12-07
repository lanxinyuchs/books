-module(eTrackerSuper).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, eTrackerSuper}, ?MODULE, []).

init(_) ->
    {ok, {{one_for_one, 1, 60},
	  [{eTrackerServer, {eTrackerServer, start_link, []}, permanent,
	    brutal_kill, worker, [eTrackerServer]},
	   {eTrackerWWW, {eTracker, start_link, [www]}, permanent, brutal_kill,
	    worker, [eTracker, eTrackerWWW]},
	   {eTracker, {eTracker, start_link, [tracker]}, permanent, 10000,
	    worker, [eTracker, eTrackerServer]}
	  ]
	 }}.
