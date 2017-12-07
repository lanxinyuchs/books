#!/usr/bin/env escript
%% -*- erlang -*-

%% TODO Records copied from CERT. Include instead.

-record(certNC,
        {index,     % Same index as for nodeCredential table
         action   = no_action, % Ongoing between actions
         timeout,
         cert,
         csr,
         keyStore = [],
         ai = false,
         offline_p12}).

-record(certTC,
        {index,     % Same index as for trustedCertificate table
         timeout,
           fingerprint,
         cert}).

main(Args) when length(Args) =:= 2 ->

    [EsiLog | [OutDir] ] = Args,
    io:format("args = ~p ~p ~n",[EsiLog,OutDir]),
    case init_db(EsiLog) of 
      { aborted, Reason } ->
	    io:format("Could not parse log file ~p ~n",[Reason]);
      _ ->
	    export_tc_certs(OutDir),
	    export_node_certs(OutDir),
	    io:format("Done! ~n",[])
    end;

main(_) ->
    usage().

usage() ->
    io:format("readcert <esi log> <outdir> ~n",[]).
  

init_db(EsiLog) ->
  io:format("Starting mnesia ~n",[]),
  Dir = "/tmp/" ++ os:getenv("USER") ++ "_dumpcert",
  application:set_env(mnesia,dir,Dir),
   mnesia:create_schema([node()]),
  mnesia:start(),
  io:format("Import data from ~p ~n",[EsiLog]),
  mnesia:load_textfile(EsiLog).


export_tc_certs(OutDir) ->
    io:format("Search for Trusted Certs ~n",[]),
    Keys = mnesia:dirty_all_keys(certTC),
    io:format("Found following TCs ~p ~n",[Keys]),
    Fun = fun (Index) -> 
      [TC] = mnesia:dirty_read(certTC, Index),
      {_,_,_,_,Id} = Index,
      FileName = OutDir ++ "/tc_" ++ Id ++ ".der",
      io:format("Exporting TC ~p ~n",[FileName]),
      file:write_file(FileName,TC#certTC.cert)
    end,
   lists:foreach(Fun,Keys).


%% node cert is a list of certs 
		  
export_node_certs(OutDir) ->
    io:format("Search for Node Certs ~n",[]),
    Keys = mnesia:dirty_all_keys(certNC),
    io:format("Found following certs ~p ~n",[Keys]),
    Fun = fun (Index) ->
      {_,_,_,_,Id} = Index,
      CertList = get_nc_cert_list(Index),
      write_node_cert(OutDir,CertList,Id,1)
    end,
   lists:foreach(Fun,Keys).

%%TODO: Only supports one cert.
write_node_cert(_,[],_,_) ->
    ok;
write_node_cert(OutDir,[H|T],Id,Part) ->
    FileName = OutDir ++ "/nc_" ++ Id ++ "_" ++ integer_to_list(Part) ++ ".der",
    
    io:format("Exporting NC ~p  ~n",[FileName]),
    file:write_file(FileName,H),
    write_node_cert(OutDir,T,Id,Part+1).

get_nc_cert_list(Key) ->
    case mnesia:dirty_read(certNC, Key) of
        [] ->
            [];
        [Obj] ->
            case Obj#certNC.cert of
                undefined ->
                    [];
                Cert ->
                    Cert
            end
    end.

