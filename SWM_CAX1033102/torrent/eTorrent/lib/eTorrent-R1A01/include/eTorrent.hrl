
%%% Data about torrents
-record(eTorrent, {info_hash,
		   path,
		   announce,
		   comment, 
		   creation_date,
		   length,
		   name,
		   piece_length,
		   pieces_number,
		   pieces,
		   status,
		   uploaded = 0,
		   downloaded = 0,
		   left = 1,
		   bitfield}).

%%% Performance statistics for peers
-record(eTorrentPeer, {key, % {ip, port, info_hash}
		       pid, % pid to the peer controller process
		       remote_peer_id,    % binary()
		       socket,
		       am_choking = am_choking,
		       am_interested = am_not_interested,
		       peer_choking = peer_choking,
		       peer_interested = poer_not_interested
 		       }).

%%% Client registry

%% -record(eTorrentTasks, {key, % {info_hash, piece}
%% 		       }). 

%%% Bitfield information

-record(eTorrentBitfield, {key, % {InfoHash, PeerId}
			   bitfield % {1|0, ...}
			   }).
