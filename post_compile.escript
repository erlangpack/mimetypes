#!/usr/bin/env escript
%%! -pa ebin

main(_) ->
    ok = application:start(mimetypes),
    ok = application:stop(mimetypes),
    {ok, [T]} = file:consult("ebin/mimetypes.app"),
    {application, mimetypes, PL} = T,
    DynMods = [mimetypes:dmodule(),mimetypes:mmodule()],
    Modules = DynMods ++ proplists:get_value(modules, PL),
    {ok, F} = file:open("ebin/mimetypes.app",[write]),
    NewPL = lists:keyreplace(modules, 1, PL, {modules, Modules}),
    NewApp = {application, mimetypes, NewPL},
    io:format(F, "~s.\n", [io_lib_pretty:print(NewApp)]),
    file:close(F).

