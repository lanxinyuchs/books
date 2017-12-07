-module(eTrackerWWW).

-export([handle_request/4]).

handle_request(Socket, Uri, HttpValues, Data) ->
    case catch do_handle_request(Socket, Uri) of
	ok -> ok;
	{'EXIT', Reason} ->
	    error_logger:error_report(
	      [{?MODULE, handle_request, [Socket, Uri, HttpValues, Data]},
	       {mfa, {?MODULE, do_handle_request, [Socket, Uri]}},
	       {'EXIT', Reason}]),
	    send_error(Socket, Reason)
    end.

do_handle_request(Socket, {abs_path, Uri}) ->
    Home = os:getenv("HOME"),
    Path = case Uri of
	       "/" ->
		   filename:join([Home, "public_html", "index.html"]);
	       [$/|Tail] ->
		   filename:join([Home, "public_html", Tail])
	   end,
    case file:read_file(Path) of
	{ok, Bin} ->
	    ContentType = case filename:extension(Uri) of
			      ".html" -> "text/html";
			      ".txt" -> "text/plain";
			      ".torrent" -> "application/x-bittorrent"
			  end,x,
	    HttpHeader = 
		"HTTP/1.1 200 OK\r\n"
		"Connection: close\r\n"
		"Content-length: "++integer_to_list(size(Bin))++"\r\n"
		"Content-Type: "++ContentType++"\r\n"
		"Date: "++httpd_util:rfc1123_date()++"\r\n"
		"Server: eTorrent\r\n"
		"\r\n",
	    gen_tcp:send(Socket, lists:flatten([HttpHeader, Bin]));
	{error, enoent} ->
	    Msg = "<html><head><title>404: Not found</title></head>"
		"<body><h1>404: Not found</h1>"
		"<p>You request could not be served</p></body></html>",
	    HttpHeader = 
		"HTTP/1.1 404 OK\r\n"
		"Connection: close\r\n"
		"Content-length: "++integer_to_list(length(Msg))++"\r\n"
		"Content-Type: text/html\r\n"
		"Date: "++httpd_util:rfc1123_date()++"\r\n"
		"Server: eTorrent\r\n"
		"\r\n",
	    gen_tcp:send(Socket, lists:flatten([HttpHeader, Msg]));
	{error, Reason} ->
	    send_error(Socket, Reason)
    end,
    gen_tcp:close(Socket).

send_error(Socket, _) ->
    Msg = "<html><head><title>500: Internal server error</title></head>"
	"<body><h1>500: Internal server error</h1>"
	"<p>You request could not be served</p></body></html>",
    HttpHeader = 
        "HTTP/1.1 500 OK\r\n"
        "Connection: close\r\n"
        "Content-length: "++integer_to_list(length(Msg))++"\r\n"
        "Content-Type: text/html\r\n"
        "Date: "++httpd_util:rfc1123_date()++"\r\n"
        "Server: eTorrent\r\n"
        "\r\n",
    gen_tcp:send(Socket, lists:flatten([HttpHeader, Msg])),
    gen_tcp:close(Socket).
    
