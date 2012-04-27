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
