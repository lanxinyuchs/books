%%--------------------------------------------------------------------
%% AIS Programming Model and Naming Conventions - Basic types
%%--------------------------------------------------------------------
-ifndef(safs_ais_hrl).
-define(safs_ais_hrl, true).

-type sa_int8()      :: integer().  % SaInt8T
-type sa_uint8()     :: integer().  % SaUint8T
-type sa_int16()     :: integer().  % SaInt16T
-type sa_uint16()    :: integer().  % SaUint16T
-type sa_int32()     :: integer().  % SaInt32T
-type sa_uint32()    :: integer().  % SaUint32T
-type sa_int64()     :: integer().  % SaInt64T
-type sa_uint64()    :: integer().  % SaUint64T
-type sa_size()      :: sa_int64(). % SaSizeT
-type sa_name()      :: string().   % SaNameT
-type sa_float()     :: float().    % SaFloatT
-type sa_double()    :: float().    % SaDoubleT
-type sa_string()    :: string().   % SaStringT
-type sa_any()       :: binary().   % SaAnyT

%%--------------------------------------------------------------------
%% AIS Programming Model and Naming Conventions - Time Type
%%--------------------------------------------------------------------
%% Int64
%% @type sa_time() = integer()
-type sa_time()      :: sa_int64(). % SaTimeT

%% SA_TIME_BEGIN represents the smallest timestamp value:
%%    Thu 1 Jan 00:00:00 UTC 1970.
-define(SA_TIME_BEGIN,   16#0).
%% SA_TIME_END represents the largest timestamp value:
%%    Fri Apr 11 23:47:16.854775807 UTC 2262.
-define(SA_TIME_END,     16#7FFFFFFFFFFFFFFF).
-define(SA_TIME_UNKNOWN, 16#8000000000000000).

-define(SA_TIME_ONE_MICROSECOND, 1000).
-define(SA_TIME_ONE_MILLISECOND, 1000000).
-define(SA_TIME_ONE_SECOND,      1000000000).
-define(SA_TIME_ONE_MINUTE,      60000000000).
-define(SA_TIME_ONE_HOUR,        3600000000000).
-define(SA_TIME_ONE_DAY,         86400000000000).
-define(SA_TIME_MAX,             ?SA_TIME_END).

-endif.
