%% -*- erlang -*-
{application, eTorrent, 
 [{description, "Bit torrent client/peer"},
  {vsn, "R1A01"},
  {modules, [eTorrent,
	     eTorrentClient,
	     eTorrentPeer,
	     eTorrentSuper,
	     eTorrentTable,
	     eTorrentUI,
	     bencode]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, mnesia, crypto, inets]},
  {mod, {eTorrent, []}}]}.

