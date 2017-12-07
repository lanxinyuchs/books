-record(request, {info_hash,
		  peer_id,
		  port,
		  uploaded,
		  downloaded,
		  left,
		  compact,
		  no_peer_id,
		  event,
		  ip,
		  numwant=50,
		  key,
		  tracker_id, 
		  supportcrypto
		}).

-record(response, {failure_reason,
		   warning_message,
		   interval,
		   min_interval,
		   tracker_id,
		   complete,
		   incomplete,
		   peers}).
		   
	
