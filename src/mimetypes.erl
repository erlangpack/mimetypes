-module(mimetypes).

-behaviour(gen_server).

%% API
-export([module/0, start_link/0, extension/1, filename/1, extensions/1,
         types/0, extensions/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(MAPMOD, mimetypes_map).
-define(DISPMOD, mimetypes_disp).
-define(TABLE, mimetypes_data).
-define(MODTABLE, mimetypes_modules).

-record(map_info, {
    db :: term(),
    ext :: binary(),
    mime :: binary()}).

-record(db_info, {
    db :: term(),
    mod :: atom()}).

-record(state, {}).

-type erlang_form() :: term().
-type compile_options() :: [term()].


%%%===================================================================
%%% API
%%%===================================================================

module() ->
    ?MAPMOD.

extension(Ext) ->
    ?DISPMOD:ext_to_mimes(iolist_to_binary(Ext), default).

filename(Filename) ->
    "." ++ Ext = filename:extension(Filename),
    extension(Ext).

extensions(Types) when is_list(Types) ->
    lists:usort(lists:flatten([ extensions(Type) || Type <- Types ]));
extensions(Type) when is_binary(Type) ->
    ?DISPMOD:mime_to_exts(iolist_to_binary(Type), default).


types() ->    
    ?DISPMOD:mimes(default).

extensions() ->    
    ?DISPMOD:exts(default).
    

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    %% The database index table should be owned by the mimetypes supervisor.
    %% This ensures that the table is not destroyed before the application
    %% crashes.
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [
                named_table,public,bag,
                {keypos, #map_info.db}]);
        _ ->
            ignore
    end,
    case ets:info(?MODTABLE) of
        undefined ->
            ets:new(?MODTABLE, [
                named_table,public,set,
                {keypos, #db_info.db}]),
            ets:insert(?MODTABLE, #db_info{db=default, mod=?MAPMOD});
        _ ->
            ignore
    end,
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
            load_mapping(?MAPMOD, Mapping),
            Dispatch = ets:select(?MODTABLE, [{
                #db_info{db='$1',mod='$2'}, [], [{{'$1','$2'}}]}]),
            load_dispatch(Dispatch);
        _ ->
            ok
    end,
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
    compile_and_load_forms(AbsCode, []).

%% @private Load a list of database-module pairs.
-spec load_dispatch([{term(), atom()}]) -> ok.
load_dispatch(Pairs) ->
    Module = ?DISPMOD,
    AbsCode = disp_to_abstract(Module, Pairs),
    compile_and_load_forms(AbsCode, []).

%% @private Generate an abstract dispatch module.
-spec disp_to_abstract(atom(), [{term(), atom()}]) -> [erl_syntax:syntaxTree()].
disp_to_abstract(Module, Dispatch) ->
    [erl_syntax:revert(E) || E <- disp_to_abstract_(Module, Dispatch)].

%% @private Generate an abstract dispatch module.
-spec disp_to_abstract_(atom(), [{term(), atom()}]) -> [erl_syntax:syntaxTree()].
disp_to_abstract_(Module, Dispatch) ->
    [%% -module(Module)
     erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
     %% -export([ext_to_mimes/2, mime_to_exts/2, exts/1, mimes/1]).
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
            erl_syntax:integer(1))])]),
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
         [erl_syntax:underscore()], none, [erl_syntax:abstract(error)])])].

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


%% @private Compile and load a module.
%% Copied from the meck_mod module of the meck project.
-spec compile_and_load_forms(erlang_form(), compile_options()) -> ok.
compile_and_load_forms(AbsCode, Opts) ->
    case compile:forms(AbsCode, Opts) of
        {ok, ModName, Binary} ->
            write_binary(ModName, Binary);
        {ok, ModName, Binary, _Warnings} ->
            write_binary(ModName, Binary);
        Error ->
            exit({compile_forms, Error})
    end.

%% @private Write a module & load it.
write_binary(Name, Binary) ->
    Filename = filename:join([filename:dirname(code:which(?MODULE)),
                              atom_to_list(Name) ++ ".beam"]),
    file:write_file(Filename, Binary),
    case code:ensure_loaded(Name) of
        {module, Name}  -> ok;
        {error, Reason} -> exit({error_loading_module, Name, Reason})
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

-endif.
