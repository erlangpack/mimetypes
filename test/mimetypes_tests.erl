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

-module(mimetypes_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% generators
extensions() ->
    oneof(mimetypes:extensions()).

types() ->
    oneof(mimetypes:types()).

%% properties

prop_extension() ->
    ?FORALL(Ext, extensions(),
            lists:member(Ext, mimetypes:extensions(mimetypes:extension(Ext)))).

prop_type() ->
    ?FORALL(Type, types(),
            lists:all(fun (X) -> X end,
                      [ case mimetypes:extension(Ext) of
                            Types when is_list(Types) ->
                                lists:member(Type, Types);
                            Type1 ->
                                Type1 =:= Type
                        end || Ext <- mimetypes:extensions(Type) ])).

prop_filename() ->
    ?FORALL(Ext, extensions(),
        begin
            Filename = "/etc/test." ++ binary_to_list(Ext),
            MimeTypes = mimetypes:filename(Filename),
            ExtList = mimetypes:extensions(MimeTypes),
            lists:member(Ext, ExtList)
        end).

%% eunit
t_properties() ->
    ?assertEqual([], proper:module(?MODULE,
                                   [{'on_output',
                                     fun(Format, Data) ->
                                             io:format(standard_error, Format, Data)
                                     end},
                                    {numtests, 1000}])).

mimetypes_test_() ->
    [{setup,
      fun() ->
              ok = application:start(mimetypes)
      end,
      fun(_) ->
              application:stop(mimetypes)
      end,
      [
       {timeout, 300, {"PropEr tests", ?_test(t_properties())}}
      ]}].

filename_test_() ->
    {"Test filename/1 for names without an extension and for unknown extensions.",
      {setup,local,
        fun() -> application:start(mimetypes) end,
        fun(_) -> application:stop(mimetypes) end,
        [?_assertEqual(mimetypes:filename("/etc/fstab"), [<<"application/octet-stream">>])
        ,?_assertEqual(mimetypes:filename("."), [<<"application/octet-stream">>])
        ,?_assertEqual(mimetypes:filename(""), [<<"application/octet-stream">>])
        ,?_assertEqual(mimetypes:filename("t.unknown_extension"), [<<"application/octet-stream">>])
%           ,?_assertEqual(mimetypes:filename("index.html"), [<<"text/html">>])
        ]}}.

extensions_test_() ->
    {"Test extensions/1 for bad content types.",
      {setup,local,
        fun() -> application:start(mimetypes) end,
        fun(_) -> application:stop(mimetypes) end,
        [?_assertEqual(mimetypes:extensions([<<"unknown/ext">>]), [])
        ,?_assertEqual(mimetypes:extensions(<<"unknown/ext">>), [])
        ]}}.

sync_loader_test_() ->
    {setup,local,
        fun() ->
            application:load(mimetypes),
            application:set_env(mimetypes, load, [
                {default, [{<<"foo1">>, <<"bar1">>}]}]),
            application:set_env(mimetypes, load_sync, true),
            application:start(mimetypes)
        end,
        fun(_) -> application:stop(mimetypes) end,
        [?_assertEqual([<<"bar1">>], mimetypes:ext_to_mimes(<<"foo1">>))]}.

async_loader_test_() ->
    {setup,local,
        fun() ->
            application:load(mimetypes),
            application:set_env(mimetypes, load, [
                {default, [{<<"foo2">>, <<"bar2">>}]}]),
            application:set_env(mimetypes, load_sync, false),
            application:start(mimetypes)
        end,
        fun(_) -> application:stop(mimetypes) end,
        [{timeout, 10000, ?_test(async_loader_onstart())},
         {timeout, 10000, ?_test(async_loader_wait())}
        ]}.

async_loader_onstart() ->
    ?assertEqual(undefined, mimetypes:ext_to_mimes(<<"foo2">>)).

async_loader_wait() ->
    receive after 5000 -> ok end, %% @todo don't use an arbitrary time
    ?assertEqual([<<"bar2">>], mimetypes:ext_to_mimes(<<"foo2">>)).
