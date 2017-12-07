-module(path).
-export([find/1, find/2]).

% Find file, the first path pointing to an existing file from the list is returned.
%
% Error is rised when none of the paths exist.
find(Pathlist) ->
   lists:nth(1,  lists:filter(fun filelib:is_file/1, Pathlist)).

% Find file, first path, prefixed with the value of environent variable Env,
% pointing to an existing file  from the list is returned.
%
% Error is rised when none of the paths exist.
find(Env, Pathlist) ->
   find(lists:map(fun(P) -> os:getenv(Env)++"/"++P end, Pathlist)).
