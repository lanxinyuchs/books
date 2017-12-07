%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pesLog.erl %
%%% 
%%% Description:     
%%%
%% @doc
%% 
%% == PMS logging ==
%% 
%% pmsLog provides means of writing entires to a log file for debug
%% purposes.
%%
%% The log file will be written to $TMPDIR, if defined, otherwise
%% to /tmp. The file name is as follows PESLogfile_'date'_'time'.txt
%% The purpose is to provide a runtime possibility to turn on/off
%% logging of any application. 
%% 
%% To achieve this the modules must be prepared with calls to pesLog. 
%% The log entries are divided into different levels: 
%% error, warning, info, proc, db, if;
%% where proc stands for process, db for database, and if for interface logs.
%%
%% === Log macros (defined in pes.hrl) === 
%% 
%% ```
%% LOG_ERROR(String)
%% LOG_WARNING(String)
%% LOG_MISC(String)
%% LOG_PROC(String)
%% LOG_DB(String)
%% LOG_IF(String)
%% LOG_ROP(String)
%% LOG_LOOP(String)
%% 
%% LOG_ERROR(String, Args)
%% LOG_WARNING(String, Args)
%% LOG_MISC(String, Args)
%% LOG_PROC(String, Args)
%% LOG_DB(String, Args)
%% LOG_IF(String, Args)
%% LOG_ROP(String, Args)
%% LOG_LOOP(String, Args)
%% 
%% Examples:
%%   ?LOG_INFO("An info log example~n"),
%%   ?LOG_INFO("An info log example number ~p~n", [2]),
%% '''
%% 
%% Each log macro is connected to a filter name. 
%% ```
%% LOG_ERROR   - e
%% LOG_WARNING - w
%% LOG_MISC    - m
%% LOG_PROC    - p
%% LOG_DB      - db
%% LOG_IF      - i
%% LOG_ROP     - r
%% LOG_LOOP    - l
%% '''
%% 
%% Furthermore there is a special filter name, a, that is a union of all 
%% filter names.
%% 
%% == Runtime ==
%%
%% The log functionality is started by calling
%% ```
%% pesEnv:init().
%% '''
%%
%% By default nothing will be logged. The logs must be turned on individually
%% per module by calling 
%% ```
%% pesLog:filter_add(Module, Filters)
%% 
%%  where 
%%    Module  = string() 
%%    Filters = Filter | [Filter]
%%    Filter  = e | w | m | p | db | i | r | l | a
%% '''
%% Logging is turned of by calling
%% ```
%% pesLog:filter_rm(Module, Filters)
%% '''
%% It is possible to configure default filters 
%% by defining a file $USER_log.pes in the $TMPDIR, if defined, otherwise
%% in the /tmp catalog. The file contains a number of MFA definitions
%% that are called when pesEnv:init() is invoked.
%% (Note, the MFA can be any MFA, not only PES functions.
%% 
%% The file has the following syntax:
%% ```
%% M.
%% {F, A}.
%% {M, F, A}.
%% '''
%% After an 'M.' line several lines containing {F,A} can be defined.
%% All the functions will be called using the defined M, until another
%% 'M.' is found.
%% It is also possible to define {M,F,A} in which case M:F(A) will be called.
%% 
%% Example:
%% ```
%% pesLog.
%% {filter_add, [pesAppRegistry, [e,w,i]]}.
%% {filter_add, [pesAppJob, e]}.
%% {filter_add, [pesJob, a]}.
%% dbg.
%% dbg:tracer().
%% dbg:p(all,c).
%% dbg:tpl(pesSession, [{'_',[],[{exception_trace}]}]).
%% {code, add_path, ["home/user/patches"]}.
%% '''
%% 
%% === Example of usage === 
%%
%% Add a filter called testing and filter e (error)
%% <pre>
%% pesLog:filter_add(pesX, e).
%% </pre>
%%
%% Do not add time to the log entry (true by default)
%% <pre>
%% pesLog:print_time(false).
%% </pre>
%%
%% Do not print the tag to the log entry (true by default)
%% <pre>
%% pesLog:print_tag(false).
%% </pre>
%%
%% Remove a filter called testing
%% <pre>
%% pesLog:filter_rm(pesX).
%% </pre>
%%
%% == Log file ==
%% 
%% There is a script
%% 
%% ```
%% bin/peslog
%% '''
%% 
%% 
%% 
%% 
%% @end
%%% ----------------------------------------------------------
-module(pesLog).
-id('Updated by CCase').
-vsn('/main/R3A/R5A/2').
-date('2016-04-05').
-author('uabesvi').
-shaid('f38ffd604ec62e8afe098450a35fe739edb6000b').
