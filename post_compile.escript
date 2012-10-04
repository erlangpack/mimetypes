#!/usr/bin/env escript
%%! -pa ebin -kernel error_logger silent

main(_) ->
    {ok, [T]} = file:consult("ebin/mimetypes.app"),
    {application, mimetypes, PL} = T,

    DynMods = [mimetypes:dmodule(),mimetypes:mmodule()],
    BaseModules = proplists:get_value(modules, PL) -- DynMods,
    Modules = DynMods ++ BaseModules,

    Timestamps = last(["priv/mime.types"|files(BaseModules)]),
    
    [DModTimestamp,MModTimestamp] = last(files(DynMods)),

    DynMinTimestamp = erlang:min(DModTimestamp, MModTimestamp),

    case lists:any(fun(TS) ->
                           TS > DynMinTimestamp
                   end, Timestamps) of
        true ->
            [ file:delete(F) || F <- files(DynMods) ],
            ok = application:start(mimetypes),
            ok = application:stop(mimetypes),
            {ok, F} = file:open("ebin/mimetypes.app",[write]),
            NewPL = lists:keyreplace(modules, 1, PL, {modules, Modules}),
            NewApp = {application, mimetypes, NewPL},
            io:format(F, "~s.\n", [io_lib_pretty:print(NewApp)]),
            file:close(F);
        false ->
            %% everything is up to date
            ok
    end.

files([]) ->
    [];
files([H|T]) ->
    [ filename:join(["ebin",atom_to_list(H) ++ ".beam"]) | files(T) ].

last(Files) ->
    [ case filelib:last_modified(F) of 0 -> 0; M -> calendar:datetime_to_gregorian_seconds(M) end || F <- Files ].
