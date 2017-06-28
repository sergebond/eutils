-module(erandom).

-export ([start_link/0]).

-behaviour (supervisor).
-export ([init/1]).

-export ([gen_objectid/0, next_requestid/0]). % API

-type unixtime() :: {integer(), integer(), integer()}. % {MegaSecs, Secs, MicroSecs}
-type unixsecs() :: integer(). % Unix Time in seconds
-type objectid() :: {<<_:96>>}.

%% Behaviour callbacks

start_link () -> supervisor:start_link ({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

%@doc Create global vars which will be owned by this supervisor (and die with it)
init ([]) ->
  ets:new (?MODULE, [named_table, public]),
  ets:insert (?MODULE, [ {oid_counter, 0}, {oid_machineprocid, oid_machineprocid()}, {requestid_counter, 0} ]),
  {ok, {{one_for_one,3,10}, []}}.

%% API functions

-spec next_requestid () -> mongo_protocol:requestid(). % IO
%@doc Fresh request id
next_requestid() -> ets:update_counter (?MODULE, requestid_counter, 1).

-spec gen_objectid () -> binary(). % IO
%@doc Fresh object id
gen_objectid() ->
  Now  = unixtime_to_secs(timenow()),
  MPid = ets:lookup_element (?MODULE, oid_machineprocid, 2),
  N    = ets:update_counter (?MODULE, oid_counter, 1),
  {Id} = objectid(Now, MPid, N),
  Id.

-spec oid_machineprocid () -> <<_:40>>. % IO
%@doc Fetch hostname and os pid and compress into a 5 byte id
oid_machineprocid() ->
  OSPid = list_to_integer (os:getpid()),
  {ok, Hostname} = inet:gethostname(),
  <<MachineId:3/binary, _/binary>> = erlang:md5(Hostname),
  <<MachineId:3/binary, OSPid:16/big>>.



-spec unixtime_to_secs (unixtime()) -> unixsecs().
unixtime_to_secs({MegaSecs, Secs, _}) -> MegaSecs * 1000000 + Secs.


-spec timenow () -> unixtime(). % IO
% Current unixtime to millisecond precision, ie. MicroSecs is always a multiple of 1000.
timenow() -> ms_precision (os:timestamp()).

-spec ms_precision (unixtime()) -> unixtime().
%@doc Truncate microsecs to millisecs since bson drops microsecs anyway, so time will be equal before and after serialization.
ms_precision({MegaSecs, Secs, MicroSecs}) ->
  {MegaSecs, Secs, MicroSecs div 1000 * 1000}.

% ObjectId %
-spec objectid (unixsecs(), <<_:40>>, integer()) -> objectid().
objectid(UnixSecs, MachineAndProcId, Count) ->
  {<<UnixSecs :32/big, MachineAndProcId :5/binary, Count :24/big>>}.
