-module(mimetypes).

-behaviour(gen_server).

%% API
-export([start_link/0, extension/1, filename/1, extensions/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          mime_types,
          extensions
         }).

%%%===================================================================
%%% API
%%%===================================================================

extension(Ext) ->
    gen_server:call(?SERVER, {extension, Ext}).

filename(Filename) ->
    "." ++ Ext = filename:extension(Filename),
    extension(Ext).

extensions(Type) ->
    gen_server:call(?SERVER, {extensions, Type}).
    


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
    Extensions = extract_extensions(MimeTypes),
    {ok, #state{
       mime_types = dict:from_list(MimeTypes),
       extensions = dict:from_list(Extensions)
      }}.

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
handle_call({extension, Ext}, _From, #state{ extensions = Exts } = State) ->
    case dict:is_key(Ext, Exts) of
        false ->
            Reply = undefined;
        true ->
            Reply = dict:fetch(Ext, Exts)
    end,
    {reply, Reply, State};

handle_call({extensions, Type}, _From, #state{ mime_types = Types } = State) ->
    case dict:is_key(Type, Types) of
        false ->
            Reply = undefined;
        true ->
            Reply = dict:fetch(Type, Types)
    end,
    {reply, Reply, State}.

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
    lists:concat([
                  [ {Ext, Type} || Ext <- Exts ],
                  extract_extensions(Rest)]).
