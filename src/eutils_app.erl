-module(eutils_app).
-author("srg").

-behaviour(application).

-export([start/0]).

-export([start/2,
  stop/1]).

start() ->
  application:ensure_all_started(eutils).

start(_StartType, _StartArgs) ->
  case erandom:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

stop(_State) ->
  ok.

