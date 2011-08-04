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
            lists:member(Ext, mimetypes:type(mimetypes:extension(Ext)))).

prop_type() ->
    ?FORALL(Type, types(),
            lists:all(fun (X) -> X end,
                      [ mimetypes:extension(Ext) =:= Type || Ext <- mimetypes:type(Type) ])).


%% eunit
t_properties() ->
    ?assertEqual([], proper:module(?MODULE,
                                   [{'on_output',
                                     fun(Format, Data) ->
                                             io:format(standard_error, Format, Data)
                                     end},
                                    {numtests, 100}])).

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
