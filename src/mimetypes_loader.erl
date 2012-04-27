-module(mimetypes_loader).

-export([start_link/0]).
-export([load/1]).

%% @doc Spawn a process to load configured mimetypes.
start_link() ->
    case load_sync_env() of
        false -> {ok, proc_lib:spawn_link(?MODULE, load, [false])};
        true -> proc_lib:start_link(?MODULE, load, [true])
    end.

%% @private
load(Sync) ->
    Timeout = load_timeout_env(),
    List = load_types_env(),
    load(Timeout, List),
    case Sync of
        false -> ignore;
        true -> proc_lib:init_ack({ok, self()})
    end.


%% @private
load(Timeout, [{Database, Types}|T]) ->
    mimetypes:load(Database, Types, Timeout),
    load(Timeout, T);
load(_Timeout, []) ->
    ok.

%% @private
load_timeout_env() ->
    case application:get_env(mimetypes, load_timeout) of
        undefined -> 5000;
        {ok, Timeout} -> Timeout
    end.

%% @private
load_sync_env() ->
    case application:get_env(mimetypes, load_sync) of
        undefined -> false;
        {ok, Sync} -> Sync
    end.

%% @private
load_types_env() ->
    case application:get_env(mimetypes, load) of
        undefined -> [];
        {ok, List} -> List
    end.
