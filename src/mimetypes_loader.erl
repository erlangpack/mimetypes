% Copyright © 2011-2012 Yurii Rashkovskii, Magnus Klaar. All Rights Reserved.
%
% Redistribution and use in source and binary forms, with or without modification,
% are permitted provided that the following conditions are met:
%
% 1. Redistributions of source code must retain the above copyright notice, this
% list of conditions and the following disclaimer.
%
% 2. Redistributions in binary form must reproduce the above copyright notice,
% this list of conditions and the following disclaimer in the documentation and/or
% other materials provided with the distribution.
%
% 3. The name of the author may not be used to endorse or promote products derived
% from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY [LICENSOR] "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
% SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
% EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
% OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
% IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
% OF SUCH DAMAGE.

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
