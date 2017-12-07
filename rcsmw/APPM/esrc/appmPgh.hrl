%%% ----------------------------------------------------------
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
%%% 
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

-vsn('/main/R2A/R3A/R4A/R5A/R9A/R11A/R12A/1').
-date('2017-11-09').
-author('etxarnu').

%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% ----------------------------------------------------------
%%% R3A/2     20150114    etxarnu     Updated with spawnpgmex
%%% R11A/1    20171003    etxarnu     Added pgm_type to spawnpgmex
%%% R12A/1    2017-11-09 etxarnu     Removed all group related functions
%%% ----------------------------------------------------------
%%% @doc Types used by appmPgh.erl and appmPghServer.erl
-type msg_type() :: init            |
		    setdefcpuset    |
		    spawnpgm        |
		    spawnpgmex      |
		    destroypgm      |
		    signalpgm       |
		    getpgmrscusage  |
		    getpids         |
		    restartbrd      |
		    warmrestart     |
		    writellog       |
		    pgmcrash        |
		    pgmterm.

-type encode_option() :: notify_option()       |
			 cpus_def_option()     |
			 core_dump_option()    |
			 type_option()         |
			 location_option()     |
			 args_option()         |
			 envs_option()         |
			 uid_option()          |
			 gid_option()          |
			 pgm_cap_option()      |
			 pgm_type_option()      |
			 cpu_set_option()      |
			 memory_limit_option() |
			 pgm_id_option()       |
			 get_pids_option()     |
			 write_llog_option()   |
			 hw_test_option().

-type decode_option() :: {msg_type, msg_type()}   |
			 {seq, uint32()}          |
			 {result, atom()}         |
			 {capabilities, [cap()]}  |
			 {cpus_conf, [cpu()]}     |
			 pgm_id_option()          |
			 {memory_usage, uint64()} |
			 {cpu_usage, uint64()}    |
			 {rank, rank()}.

-type init_option() :: notify_option().
-type set_def_cpuset_option() :: cpus_def_option().

-type spawn_pgm_option() :: args_option()    |
			    envs_option()    |
			    uid_option()     |
			    gid_option()     |
			    pgm_cap_option() |
			    cpu_set_option() |
			    memory_limit_option().
-type spawn_pgmex_option() :: args_option()    |
			    envs_option()    |
			    uid_option()     |
			    gid_option()     |
			    pgm_cap_option() |
			    pgm_type_option() |
			    cpu_set_option() |
			    ns_option()      |
			    memory_limit_option().
-type destroy_pgm_option() :: core_dump_option() |
			      pgm_id_lst_option().
-type signal_to_pgm_option() :: pgm_id_option() |
				sig_no_option().
-type get_pids_option()      :: pgm_id_option() .
-type get_pgm_rscusage_option() :: pgm_id_option().
-type restart_brd_option() :: hw_test_option().
-type warm_restart_option() :: reason_option().

-type write_llog_option() :: rank_option()   |
			     reason_option() |
			     name_option()   |
			     extra_option().

-type notify_option() :: {notify, boolean()} |
			 notify.
-type cpus_def_option() :: {cpus_def, [cpu()]}.
-type core_dump_option() :: {core_dump, boolean()} |
			    core_dump.
-type type_option() :: {type, image_type()}.
-type location_option() :: {location, file:filename()}.
-type args_option() :: {args, [arg()]}.
-type envs_option() :: {envs, [env()]}.
-type uid_option() :: {uid, uint32()}.
-type gid_option() :: {gid, uint32()}.
-type ns_option() :: {ns, iolist()}.
-type pgm_cap_option() :: {capabilities, [pgm_cap()]}.
-type pgm_type_option() :: {pgmtype, pgm_type()}.
-type cpu_set_option() :: {cpu_set, uint32()}.
-type memory_limit_option() :: {memory_limit, mem_limit()}.
-type pgm_id_option() :: {pgm_id, pgm_id()}.
-type pgm_id_lst_option() :: {pgm_id, [pgm_id()]}.
-type sig_no_option() :: {sig_no, sig_no()}.
-type hw_test_option() :: {hw_test, boolean()} |
			  hw_test.
-type reason_option() :: {reason, string()}.
-type rank_option() :: {rank, atom()}.
-type name_option() :: {name, string()}.
-type extra_option() :: {extra, string()}.

-type response() :: {ok, [decode_option()]} | {error, atom()}.

-type uint32() :: 0..4294967295.
-type uint64() :: 0..18446744073709551615.
-type cap() :: cpuset |
	       memory |
	       cpuacc |
	       pgmcap.
-type cpu() :: 0..31.
-type arg() :: iolist().
-type env() :: iolist().
-type pgm_id() :: uint32().
-type sig_no() :: uint32().
-type pgm_cap() :: softrt |
		   rt |
		   net |
		   sys.
-type pgm_type() :: default |
		    dp.
-type mem_limit() :: unlimited |
		     1..18446744073709551615.
-type image_type() :: uint32().
-type rank() :: unspecified |
		pgm         |
		warm        |
		cold        |
		cold_with_test.
