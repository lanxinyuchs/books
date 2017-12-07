-module(eTorrentSuper).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, eTorrentSuper}, ?MODULE, []).

init(_) ->
    {ok, {{one_for_one, 1, 60},
	  [{eTorrent, {eTorrent, start_link, []}, permanent, 10000,
	    worker, [eTorrent]},
	   {eTorrentListen, {eTorrentListen, start_link, []}, permanent, 
	    brutal_kill, worker, [eTorrentListen]}
	  ]
	 }}.
