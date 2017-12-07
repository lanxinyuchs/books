-module(vnfcc_eunit).
-include_lib("eunit/include/eunit.hrl").

url_ip_test_() ->
   [
      ?_assert(vnfcHttps:get_ip_type("https://123.123.123.1/abc/bb") =:= inet),
      ?_assert(vnfcHttps:get_ip_type("http://10.10.123.1/abc/bb%23%23&&??") =:= inet),
      ?_assert(vnfcHttps:get_ip_type("ftp://[::1]/abc/bb%23%23&&??") =:= inet6),
      ?_assert(vnfcHttps:get_ip_type("https://[fe:2120::8]/abc/bb%23%23&&??") =:= inet6)
   ].
