-module(mimetypes).

-behaviour(gen_server).

%% API
-export([start_link/0, extension/1, filename/1, extensions/1,
         types/0, extensions/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(MAPMOD, mimetypes_map).

-record(state, {}).

-type erlang_form() :: term().
-type compile_options() :: [term()].


%%%===================================================================
%%% API
%%%===================================================================

extension(Ext) ->
    ?MAPMOD:ext_to_mimes(iolist_to_binary(Ext)).

filename(Filename) ->
    "." ++ Ext = filename:extension(Filename),
    extension(Ext).

extensions(Types) when is_list(Types) ->
    lists:usort(lists:flatten([ extensions(Type) || Type <- Types ]));
extensions(Type) when is_binary(Type) ->
    ?MAPMOD:mime_to_exts(iolist_to_binary(Type)).


types() ->    
    ?MAPMOD:mimes().

extensions() ->    
    ?MAPMOD:exts().
    

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
    load_mapping(Mapping),
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
-spec load_mapping([{binary(), binary()}]) -> ok.
load_mapping(Pairs) ->
    Module = ?MAPMOD,
    AbsCode = map_to_abstract(Module, Pairs),
    compile_and_load_forms(AbsCode, []).


%% @private Generate an abstract mimtype mapping module.
-spec map_to_abstract(atom(), [{binary(), binary()}]) -> [erl_syntax:syntaxTree()].
map_to_abstract(Module, Pairs) ->
    [erl_syntax:revert(E) || E <- map_to_abstract_(Module, Pairs)].

%% @private Generate an abstract mimtype mapping module.
-spec map_to_abstract_(atom(), [{binary(), binary()}]) -> [erl_syntax:syntaxTree()].
map_to_abstract_(Module, Pairs) ->
    [%% -module(Module).
     erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
     %% -export([ext_to_mimes/1, mime_to_exts/1]).
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
            load_binary(ModName, Binary);
        {ok, ModName, Binary, _Warnings} ->
            load_binary(ModName, Binary);
        Error ->
            exit({compile_forms, Error})
    end.

%% @private Load a module.
%% Copied from the meck_mod module of the meck project.
load_binary(Name, Binary) ->
    case code:load_binary(Name, "", Binary) of
        {module, Name}  -> ok;
        {error, Reason} -> exit({error_loading_module, Name, Reason})
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

codegen_test() ->
    ok = load_mapping([{<<"b">>, <<"a">>}]),
    ?MAPMOD:module_info(),
    ?assertEqual([<<"a">>], ?MAPMOD:ext_to_mimes(<<"b">>)),
    ?assertEqual([<<"b">>], ?MAPMOD:mime_to_exts(<<"a">>)),
    ?assertEqual([<<"b">>], ?MAPMOD:exts()),
    ?assertEqual([<<"a">>], ?MAPMOD:mimes()).

-endif.
