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
