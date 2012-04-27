-module(mimetypes).

-behaviour(gen_server).

%% API - original
-export([start_link/0, extension/1, filename/1, extensions/1,
         types/0, extensions/0]).
-export([dmodule/0, mmodule/0]).

%% API - multiple databases
-export([create/1, load/2, load/3]).
-export([ext_to_mimes/1, ext_to_mimes/2]).
-export([path_to_mimes/1, path_to_mimes/2]).
-export([mime_to_exts/1, mime_to_exts/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(MAPMOD, mimetypes_map).
-define(DISPMOD, mimetypes_disp).

-type erlang_form() :: term().
-type compile_options() :: [term()].

-record(state, {}).


%%%===================================================================
%%% API
%%%===================================================================

mmodule() ->
    ?MAPMOD.

dmodule() ->
    ?DISPMOD.

%% @doc Transforms an extension to a list of mimetypes.
%%
%%      Returns application/octet-stream if there is no data about
%%      given extension.
%%
%%      Example:
%%
%% ```
%% 1> mimetypes:extension("c").    
%% [<<"text/x-c">>]
%% 2> mimetypes:extension("hrl").
%% [<<"application/octet-stream">>]
%% 3> mimetypes:extension(<<"html">>).   
%% [<<"text/html">>]
%% '''
-spec extension(Extension :: binary() | string()) -> [binary()].
extension(Ext) ->
    ext_to_mimes(Ext).


%% @doc Convert a filename to a list of mimetypes.
%%
%% Example:
%%
%% ```
%% 1> mimetypes:filename("test.cpp").    
%% [<<"text/x-c">>]
%% 2> mimetypes:filename("/etc/fstab").
%% ["<<application/octet-stream">>]
%% 3> mimetypes:filename("/etc/adduser.conf").
%% [<<"text/plain">>]
%% '''

-spec filename(Filename :: string()) -> [binary()].
filename(Filename) ->
    path_to_mimes(Filename).


%% @doc Convert mimetype to a list of extensions.
%%
%% Example:
%%
%% ```
%% 1> mimetypes:extensions([<<"text/html">>]).
%% [<<"htm">>,<<"html">>]
%% 2> mimetypes:extensions([<<"text/html">>, <<"text/x-c">>]).
%% [<<"c">>,<<"cc">>,<<"cpp">>,<<"cxx">>,<<"dic">>,<<"h">>,
%%  <<"hh">>,<<"htm">>,<<"html">>]
%% '''

-spec extensions([binary()] | binary()) -> [binary()].
extensions(Types) when is_list(Types) ->
    lists:usort(lists:flatten([ extensions(Type) || Type <- Types ]));
extensions(Type) when is_binary(Type) ->
    case ?DISPMOD:mime_to_exts(iolist_to_binary(Type), default) of
    'undefined' ->
        [];
    ResultList ->
        ResultList
    end.

%% @doc Return a list of all available mimetypes.
-spec types() -> [binary()].
types() ->    
    ?DISPMOD:mimes(default).

%% @doc Return a list of all available extensions.
-spec extensions() -> [binary()].
extensions() ->    
    ?DISPMOD:exts(default).


%% @doc Create a new database of extension-mimetype mappings.
%% The name 'default' is reserved.
%% @end
-spec create(term()) -> ok.
create(Database) when Database =/= default ->
    gen_server:call(?SERVER, {create, Database}).

%% @doc Load a set of extension-mimetype mappings.
-spec load(term(), [{binary(), binary()}]) -> ok.
load(Database, Mappings) ->
    gen_server:call(?SERVER, {load, Database, Mappings}).

%% @doc Load a set of extension-mimetype mappings.
-spec load(term(), [{binary(), binary()}], infinity | integer()) -> ok.
load(Database, Mappings, Timeout) ->
    gen_server:call(?SERVER, {load, Database, Mappings}, Timeout).


%% @doc Return the set of known mimetypes of an extension.
%% @equiv ext_to_mimes(Extension, default)
%% @end
-spec ext_to_mimes(string() | binary()) -> [binary()].
ext_to_mimes(Extension) ->
    ext_to_mimes(Extension, default).

%% @doc Return the set of known mimetypes of an extension.
%% @end
-spec ext_to_mimes(string() | binary(), term()) -> [binary()].
ext_to_mimes(Extension, Database) ->
    checkdefault(?DISPMOD:ext_to_mimes(iolist_to_binary(Extension), Database)).

%% @doc Return the set of known mimetypes based on the extension of a path.
-spec path_to_mimes(string() | binary()) -> [binary()].
path_to_mimes(Path) ->
    path_to_mimes(Path, default).

%% @doc Return the set of known mimetypes based on the extension of a path.
-spec path_to_mimes(string() | binary(), term()) -> [binary()].
path_to_mimes(Path, Database) ->
    checkdefault(case filename:extension(iolist_to_binary(Path)) of
        <<".", Ext/binary>> -> ext_to_mimes(Ext, Database);
        _Else -> undefined
    end).

%% @doc Return the set of known extensions of a mimetype.
%% @equiv mime_to_exts(MimeType, default)
%% @end
-spec mime_to_exts(string() | binary()) -> [binary()].
mime_to_exts(MimeType) ->
    mime_to_exts(MimeType, default).

%% @doc Return the set of known extensions of a mimetype.
%% @end
-spec mime_to_exts(string() | binary(), term()) -> [binary()].
mime_to_exts(MimeType, Database) ->
    ?DISPMOD:mime_to_exts(iolist_to_binary(MimeType), Database).

%% @private Map undefined to application/octet-stream.
-spec checkdefault(undefined | [binary()]) -> [binary()].
checkdefault(undefined) -> [<<"application/octet-stream">>];
checkdefault(Other=[_|_]) -> Other.
    

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    case code:which(?MAPMOD) of
        non_existing ->
            case code:priv_dir(mimetypes) of
                {error, bad_name} ->
                    File = filename:join([filename:dirname(code:which(?MODULE)),
                                          "..",
                                          "priv",
                                          "mime.types"
                                         ]);
                Priv ->
                    File = filename:join(Priv, "mime.types")
            end,
            {ok, B} = file:read_file(File),
            S = binary_to_list(B),
            {ok, Tokens, _} = mimetypes_scan:string(S),
            {ok, MimeTypes} = mimetypes_parse:parse(Tokens),
            Mapping = extract_extensions(MimeTypes),
            ok = write_dispatch([{default, ?MAPMOD}]),
            ok = write_mapping(?MAPMOD, Mapping);
        _ ->
            ok
    end,
    ok = filter_modules(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({create, Name}, _From, State) ->
    Modules = ?DISPMOD:modules(),
    case lists:keyfind(Name, 1, Modules) of
        {_, _} ->
            {reply, exists, State};
        false ->
            Index = length(Modules),
            Mod = list_to_atom("mimetypes_db_" ++ integer_to_list(Index)),
            NewMods = [{Name,Mod}|Modules],
            %% _always_ load the updated dispatch module before the new
            %% mapping module. If we don't we'll loose out reference to it.
            ok = load_dispatch(NewMods),
            ok = load_mapping(Mod, []),
            {reply, ok, State}
    end;

handle_call({load, Name, New}, _From, #state{}=State) ->
    Modules = ?DISPMOD:modules(),
    case lists:keyfind(Name, 1, Modules) of
        false ->
            {reply, noexists, State};
        {_, Mod} ->
            Exts = Mod:exts(),
            E2MS = fun(E) -> Mod:ext_to_mimes(E) end,
            Prev = lists:flatten([[{E,T} || T <- E2MS(E)] || E <- Exts]),
            Pairs = lists:usort(Prev ++ New),
            ok = load_mapping(Mod, Pairs),
            {reply, ok, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
extract_extensions([]) ->
    [];
extract_extensions([{Type, Exts}|Rest]) ->
    [{Ext, Type} || Ext <- Exts] ++ extract_extensions(Rest).


%% @private Load a list of mimetype-extension pairs.
-spec load_mapping(atom(), [{binary(), binary()}]) -> ok.
load_mapping(Module, Pairs) ->
    AbsCode = map_to_abstract(Module, Pairs),
    {ok, Module, Binary} = compile_forms(AbsCode, []),
    ok = load_binary(Module, Binary).

%% @private Write a list of mimetype-extension pairs.
%% The function should be called during pre-compilation when no module is
%% available for the default set of mimetype-extension paris.
-spec write_mapping(atom(), [{binary(), binary()}]) -> ok.
write_mapping(Module, Pairs) ->
    AbsCode = map_to_abstract(Module, Pairs),
    {ok, Module, Binary} = compile_forms(AbsCode, []),
    ok = write_binary(Module, Binary),
    ok = load_binary(Module, Binary).

%% @private Load a list of database-module pairs.
-spec load_dispatch([{term(), atom()}]) -> ok.
load_dispatch(Pairs) ->
    Module = ?DISPMOD,
    AbsCode = disp_to_abstract(Module, Pairs),
    {ok, Module, Binary} = compile_forms(AbsCode, []),
    ok = load_binary(Module, Binary).

%% @private Write a list of database-module pairs.
-spec write_dispatch([{term(), atom()}]) -> ok.
write_dispatch(Pairs) ->
    Module = ?DISPMOD,
    AbsCode = disp_to_abstract(Module, Pairs),
    {ok, Module, Binary} = compile_forms(AbsCode, []),
    ok = write_binary(Module, Binary),
    ok = load_binary(Module, Binary).

%% @private Generate an abstract dispatch module.
-spec disp_to_abstract(atom(), [{term(), atom()}]) -> [erl_syntax:syntaxTree()].
disp_to_abstract(Module, Dispatch) ->
    [erl_syntax:revert(E) || E <- disp_to_abstract_(Module, Dispatch)].

%% @private Generate an abstract dispatch module.
-spec disp_to_abstract_(atom(), [{term(), atom()}]) -> [erl_syntax:syntaxTree()].
disp_to_abstract_(Module, Dispatch) ->
    [%% -module(Module)
     erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
     %% -export([ext_to_mimes/2, mime_to_exts/2, exts/1, mimes/1, modules/0]).
     erl_syntax:attribute(
       erl_syntax:atom(export),
       [erl_syntax:list(
         %% ext_to_mimes/2
         [erl_syntax:arity_qualifier(
            erl_syntax:atom(ext_to_mimes),
            erl_syntax:integer(2)),
          %% mime_to_exts/2
          erl_syntax:arity_qualifier(
            erl_syntax:atom(mime_to_exts),
            erl_syntax:integer(2)),
          %% exts/1
          erl_syntax:arity_qualifier(
            erl_syntax:atom(exts),
            erl_syntax:integer(1)),
          %% mimes/1
          erl_syntax:arity_qualifier(
            erl_syntax:atom(mimes),
            erl_syntax:integer(1)),
          %% modules/0
          erl_syntax:arity_qualifier(
            erl_syntax:atom(modules),
            erl_syntax:integer(0))])]),
     %% ext_to_mimes(Extension, Database) -> [MimeType].
     erl_syntax:function(
       erl_syntax:atom(ext_to_mimes),
       disp_ext_to_mimes_clauses(Dispatch) ++
       [erl_syntax:clause(
         [erl_syntax:underscore(), erl_syntax:underscore()],
           none, [erl_syntax:abstract(error)])]),
     %% mime_to_exts(MimeType, Database) -> [Extension].
     erl_syntax:function(
       erl_syntax:atom(mime_to_exts),
       disp_mime_to_exts_clauses(Dispatch) ++
       [erl_syntax:clause(
         [erl_syntax:underscore(), erl_syntax:underscore()],
           none, [erl_syntax:abstract(error)])]),
     %% exts(Database) -> [Extension].
     erl_syntax:function(
       erl_syntax:atom(exts),
       disp_exts_clauses(Dispatch) ++
       [erl_syntax:clause(
         [erl_syntax:underscore()], none, [erl_syntax:abstract(error)])]),
     %% mimes(Database) -> [MimeType].
     erl_syntax:function(
       erl_syntax:atom(mimes),
       disp_mimes_clauses(Dispatch) ++
       [erl_syntax:clause(
         [erl_syntax:underscore()], none, [erl_syntax:abstract(error)])]),
     %% modules() -> [{Name, Mod}].
     erl_syntax:function(
       erl_syntax:atom(modules),
       [erl_syntax:clause([], none, [erl_syntax:abstract(Dispatch)])])].

%% @private Generate a set of ext_to_mimes clauses.
-spec disp_ext_to_mimes_clauses([{term(), atom()}]) -> [erl_syntax:syntaxTree()].
disp_ext_to_mimes_clauses(Dispatch) ->
    [erl_syntax:clause(
      [erl_syntax:variable("Ext"), erl_syntax:abstract(D)], none,
        [erl_syntax:application(
          erl_syntax:atom(M),
          erl_syntax:atom(ext_to_mimes),
          [erl_syntax:variable("Ext")])])
    || {D, M} <- Dispatch].


%% @private Generate a set of mime_to_exts clauses.
-spec disp_mime_to_exts_clauses([{term(), atom()}]) -> [erl_syntax:syntaxTree()].
disp_mime_to_exts_clauses(Dispatch) ->
    [erl_syntax:clause(
      [erl_syntax:variable("Type"), erl_syntax:abstract(D)], none,
        [erl_syntax:application(
          erl_syntax:atom(M),
          erl_syntax:atom(mime_to_exts),
          [erl_syntax:variable("Type")])])
    || {D, M} <- Dispatch].

%% @private Generate a set of exts clauses.
-spec disp_exts_clauses([{term(), atom()}]) -> [erl_syntax:syntaxTree()].
disp_exts_clauses(Dispatch) ->
    [erl_syntax:clause(
      [erl_syntax:abstract(D)], none,
        [erl_syntax:application(
          erl_syntax:atom(M), erl_syntax:atom(exts), [])])
    || {D, M} <- Dispatch].



%% @private Generate a set of mimes clauses.
-spec disp_mimes_clauses([{term(), atom()}]) -> [erl_syntax:syntaxTree()].
disp_mimes_clauses(Dispatch) ->
    [erl_syntax:clause(
      [erl_syntax:abstract(D)], none,
        [erl_syntax:application(
          erl_syntax:atom(M), erl_syntax:atom(mimes), [])])
    || {D, M} <- Dispatch].


%% @private Generate an abstract mimtype mapping module.
-spec map_to_abstract(atom(), [{binary(), binary()}]) -> [erl_syntax:syntaxTree()].
map_to_abstract(Module, Pairs) ->
    [erl_syntax:revert(E) || E <- map_to_abstract_(Module, Pairs)].

%% @private Generate an abstract mimtype mapping module.
-spec map_to_abstract_(atom(), [{binary(), binary()}]) -> [erl_syntax:syntaxTree()].
map_to_abstract_(Module, Pairs) ->
    [%% -module(Module).
     erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
     %% -export([ext_to_mimes/1, mime_to_exts/1, exts/0, mimes/0]).
     erl_syntax:attribute(
       erl_syntax:atom(export),
       [erl_syntax:list(
         %% ext_to_mimes/1
         [erl_syntax:arity_qualifier(
            erl_syntax:atom(ext_to_mimes),
            erl_syntax:integer(1)),
          %% mime_to_exts/1
          erl_syntax:arity_qualifier(
            erl_syntax:atom(mime_to_exts),
            erl_syntax:integer(1)),
          %% exts/0
          erl_syntax:arity_qualifier(
            erl_syntax:atom(exts),
            erl_syntax:integer(0)),
          %% mimes/0
          erl_syntax:arity_qualifier(
            erl_syntax:atom(mimes),
            erl_syntax:integer(0))])]),
     %% ext_to_mimes(Extension) -> [MimeType].
     erl_syntax:function(
       erl_syntax:atom(ext_to_mimes),
       ext_to_mimes_clauses(Pairs) ++
       [erl_syntax:clause(
         [erl_syntax:underscore()], none, [erl_syntax:abstract(undefined)])]),
     %% mime_to_exts(MimeType) -> [Extension].
     erl_syntax:function(
       erl_syntax:atom(mime_to_exts),
       mime_to_exts_clauses(Pairs) ++
       [erl_syntax:clause(
         [erl_syntax:underscore()], none, [erl_syntax:abstract(undefined)])]),
     %% exts() -> [Extension].
     erl_syntax:function(erl_syntax:atom(exts), [exts_clause(Pairs)]),
     %% mimes() -> [MimeType].
     erl_syntax:function(erl_syntax:atom(mimes), [mimes_clause(Pairs)])].

%% @private Generate a set of ext_to_mimes clauses.
-spec ext_to_mimes_clauses([{binary(), binary()}]) -> [erl_syntax:syntaxTree()].
ext_to_mimes_clauses(Pairs) ->
    Exts = lists:usort([E || {E,_} <- Pairs]),
    Groups = [{E, lists:usort([T || {F,T} <- Pairs, F =:= E])} || E <- Exts],
    [erl_syntax:clause([erl_syntax:abstract(E)], none, [erl_syntax:abstract(Ts)])
    || {E, Ts} <- Groups].


%% @private Generate a set of mime_to_exts clauses.
-spec mime_to_exts_clauses([{binary(), binary()}]) -> [erl_syntax:syntaxTree()].
mime_to_exts_clauses(Pairs) ->
    Types = lists:usort([T || {_,T} <- Pairs]),
    Groups = [{T, lists:usort([E || {E,U} <- Pairs, U =:= T])} || T <- Types],
    [erl_syntax:clause([erl_syntax:abstract(T)], none, [erl_syntax:abstract(Es)])
    || {T, Es} <- Groups].


%% @private Generate a clause returning the set of extensions.
-spec exts_clause([{binary(), binary()}]) -> [erl_syntax:syntaxTree()].
exts_clause(Pairs) ->
    Exts = lists:usort([E || {E,_} <- Pairs]),
    erl_syntax:clause([], none, [erl_syntax:abstract(Exts)]).


%% @private Generate a clause returning the set of mimetypes.
-spec mimes_clause([{binary(), binary()}]) -> [erl_syntax:syntaxTree()].
mimes_clause(Pairs) ->
    Types = lists:usort([T || {_,T} <- Pairs]),
    erl_syntax:clause([], none, [erl_syntax:abstract(Types)]).


%% @private Compile a module.
-spec compile_forms(erlang_form(), compile_options()) -> {ok, atom, binary()}.
compile_forms(AbsCode, Opts) ->
    case compile:forms(AbsCode, Opts) of
        {ok, ModName, Binary} ->
            {ok, ModName, Binary};
        {ok, ModName, Binary, _Warnings} ->
            {ok, ModName, Binary};
        Error ->
            exit({compile_forms, Error})
    end.

%% @private Load a module binary.
-spec load_binary(atom(), binary()) -> ok.
load_binary(Name, Binary) ->
    case code:load_binary(Name, "", Binary) of
        {module, Name}  -> ok;
        {error, Reason} -> exit({error_loading_module, Name, Reason})
    end.

%% @private Write a module binary to a file.
-spec write_binary(atom(), binary()) -> ok.
write_binary(Name, Binary) ->
    Dirname = filename:dirname(code:which(?MODULE)),
    Basename = atom_to_list(Name) ++ ".beam",
    Filename = filename:join([Dirname, Basename]),
    file:write_file(Filename, Binary).

%% @private Filter out known but not loaded modules and reload dispatch.
%% This is function should be called on startup to ensure that the
%% dispatch module is restored when the server crashed while loading
%% a database module.
-spec filter_modules() -> ok.
filter_modules() ->
    Modules = ?DISPMOD:modules(),
    Keep = [{D,M} || {D,M} <- Modules, code:which(M) =/= non_existing],
    case Keep of
        Modules -> ok;
        _Other  -> load_dispatch(Keep)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

codegen_test() ->
    ok = load_mapping(?MAPMOD, [{<<"b">>, <<"a">>}]),
    ?MAPMOD:module_info(),
    ?assertEqual([<<"a">>], ?MAPMOD:ext_to_mimes(<<"b">>)),
    ?assertEqual([<<"b">>], ?MAPMOD:mime_to_exts(<<"a">>)),
    ?assertEqual([<<"b">>], ?MAPMOD:exts()),
    ?assertEqual([<<"a">>], ?MAPMOD:mimes()).

dispatch_test() ->
    ok = load_mapping(?MAPMOD, [{<<"b">>, <<"a">>}]),
    ok = load_dispatch([{default, ?MAPMOD}]),
    ?assertEqual([<<"a">>], ?DISPMOD:ext_to_mimes(<<"b">>, default)),
    ?assertEqual([<<"b">>], ?DISPMOD:mime_to_exts(<<"a">>, default)),
    ?assertEqual([<<"b">>], ?DISPMOD:exts(default)),
    ?assertEqual([<<"a">>], ?DISPMOD:mimes(default)),
    ?assertEqual(error, ?DISPMOD:ext_to_mimes(<<"b">>, nodb)),
    ?assertEqual(error, ?DISPMOD:mime_to_exts(<<"a">>, nodb)),
    ?assertEqual(error, ?DISPMOD:exts(nodb)),
    ?assertEqual(error, ?DISPMOD:mimes(nodb)).

multi_test() ->
    ok = load_mapping(?MAPMOD, [{<<"b">>, <<"a">>}]),
    ok = load_dispatch([{default, ?MAPMOD}]),
    ?assertEqual([<<"a">>], mimetypes:ext_to_mimes(<<"b">>)),
    ?assertEqual([<<"a">>], mimetypes:ext_to_mimes(<<"b">>, default)),
    ?assertEqual([<<"a">>], mimetypes:ext_to_mimes("b")),
    ?assertEqual([<<"a">>], mimetypes:ext_to_mimes("b", default)),
    ?assertEqual([<<"b">>], mimetypes:mime_to_exts(<<"a">>)),
    ?assertEqual([<<"b">>], mimetypes:mime_to_exts(<<"a">>, default)),
    ?assertEqual([<<"b">>], mimetypes:mime_to_exts("a")),
    ?assertEqual([<<"b">>], mimetypes:mime_to_exts("a", default)).

create_test_() ->
    {setup,local,
        fun() -> application:start(mimetypes) end,
        fun(_) -> application:stop(mimetypes) end,
        [?_test(test_create()), ?_test(test_default())]}.

test_create() ->
    noexists = mimetypes:load(test_db_1, []),
    ok = mimetypes:create(test_db_1),
    ok = mimetypes:load(test_db_1, [{<<"e1">>, <<"m1">>}]),
    ?assertEqual([<<"m1">>], mimetypes:ext_to_mimes(<<"e1">>, test_db_1)),
    ok = mimetypes:load(test_db_1, [{<<"e1">>, <<"m2">>}]),
    ?assertEqual([<<"m1">>,<<"m2">>], mimetypes:ext_to_mimes(<<"e1">>, test_db_1)),
    ?assertEqual(exists, mimetypes:create(test_db_1)).

test_default() ->
    %% Ensure that ext_to_mimes/1+2, path_to_mimes/1+2, extension/1
    %% and filename/1 all returns the default type if none is found.
    Default = [<<"application/octet-stream">>],
    %% Check with non-existing extension for ext_to_mimes and extension
    ?assertEqual(Default, mimetypes:ext_to_mimes(<<"e2">>)),
    ?assertEqual(Default, mimetypes:ext_to_mimes("e2")),
    ?assertEqual(Default, mimetypes:ext_to_mimes(<<"e2">>, default)),
    ?assertEqual(Default, mimetypes:ext_to_mimes("e2", default)),
    ?assertEqual(Default, mimetypes:extension(<<"e2">>)),
    ?assertEqual(Default, mimetypes:extension("e2")),
    %% Also check with empty extension and none for path_to_mimes and filename
    ?assertEqual(Default, mimetypes:path_to_mimes(<<"a.e2">>)),
    ?assertEqual(Default, mimetypes:path_to_mimes("a.e2")),
    ?assertEqual(Default, mimetypes:path_to_mimes(<<"a.">>)),
    ?assertEqual(Default, mimetypes:path_to_mimes("a.")),
    ?assertEqual(Default, mimetypes:path_to_mimes(<<"a">>)),
    ?assertEqual(Default, mimetypes:path_to_mimes("a")),
    %% Identical tests, but for path_to_mimes/2 using the same database
    ?assertEqual(Default, mimetypes:path_to_mimes(<<"a.e2">>, default)),
    ?assertEqual(Default, mimetypes:path_to_mimes("a.e2", default)),
    ?assertEqual(Default, mimetypes:path_to_mimes(<<"a.">>, default)),
    ?assertEqual(Default, mimetypes:path_to_mimes("a.", default)),
    ?assertEqual(Default, mimetypes:path_to_mimes(<<"a">>, default)),
    ?assertEqual(Default, mimetypes:path_to_mimes("a", default)),
    %% Identical tests but for filename/2, should equal path_to_mimes/1
    ?assertEqual(Default, mimetypes:filename(<<"a.e2">>)),
    ?assertEqual(Default, mimetypes:filename("a.e2")),
    ?assertEqual(Default, mimetypes:filename(<<"a.">>)),
    ?assertEqual(Default, mimetypes:filename("a.")),
    ?assertEqual(Default, mimetypes:filename(<<"a">>)),
    ?assertEqual(Default, mimetypes:filename("a")).

-endif.
