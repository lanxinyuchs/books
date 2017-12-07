%% -*- erlang -*-
{application, eTracker, 
 [{description, "Bit torrent tracker"},
  {vsn, "R1A01"},
  {modules, [eTracker,
	     eTrackerSuper,
	     bencode]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, crypto, inets, mnesia]},
  {mod, {eTracker, []}}]}.

