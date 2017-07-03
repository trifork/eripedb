-module(eripedb).
%%% **********************************************************************
%%% * PURPOSE: Server part, for containing state and servicing requests. *
%%% **********************************************************************
%%% | Design forces:
%%% | - Avoiding having a single process be a bottleneck.
%%% | - Avoiding downtime while a new database is being loaded.
%%% | - Efficient database lookup.
%%% | Design:
%%% | - Keep the database in a protected ETS table, and let other processes
%%% |   read directly from that table (at least in the common case).
%%% | - Table is sorted and is structured by {IPClass, IP, PrefixLength} key;
%%% |   this means that a single ets:prev()-lookup suffices to get to the
%%% |   relevant entry.
%%% | - When data is being loaded, it is inserted into another table.
%%% |   This table is then renamed, after which it is the master table.
%%% |   The necessary precautions are taken to ensure that lookups still
%%% |   work during this swapping period.

-behaviour(gen_server).

%% API
-export([start_link/1,
         lookup/2,
         sync_lookup/2,
         reload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE_NAME, eripedb_data).
-define(TEMP_TABLE_NAME, eripedb_temp).

%%%===================================================================
%%% API
%%%===================================================================

start_link(DatabaseFile) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {DatabaseFile}, []).

lookup(ipv4, {A,B,C,D}) ->
    async_lookup_with_sync_fallback(ipv4, <<A,B,C,D>>);
lookup(ipv6, {A,B,C,D, E,F,G,H}) ->
    async_lookup_with_sync_fallback(ipv6, <<A,B,C,D, E,F,G,H>>).

sync_lookup(Type, IP) when is_atom(Type), is_binary(IP) ->
    gen_server:call(?SERVER, {sync_lookup, Type, IP}).

reload() ->
    gen_server:call(?SERVER, reload, 20*1000).

%%%===================================================================
%%% Data structures
%%%===================================================================

-record(state, {
          database_file :: string(),
          table :: ets:tid(),
          populated :: boolean(),
          table_token :: reference()
         }).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({DatabaseFile}) ->
    self() ! reload,
    {ok, #state{
            database_file = DatabaseFile,
            table = ets:new(?TABLE_NAME, table_options()),
            populated = false,
            table_token = make_ref()
           }}.

table_options() ->
    [named_table, protected, % Allow access from outside.
     ordered_set,            % Allow prev()-access for non-keys.
     {keypos,1}].

handle_call({sync_lookup, Type, IP}, _From, State) ->
    case State of
        #state{populated=false} ->
            Reply = {error, no_database};
        #state{populated=true} ->
            Reply = do_lookup(Type, IP)
        end,
    {reply, Reply, State};
handle_call(reload, From, State) ->
    start_reload(From, State),
    {noreply, State};
handle_call({set_database_file, Filename}, From, State) when is_list(Filename) ->
    {reply, ok, State#state{database_file=Filename}};
handle_call(_Request, _From, State) ->
    %% TODO: Log.
%    {reply, Reply, State}.
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reload, State) ->
    start_reload(undefined, State),
    {noreply, State};
handle_info({'ETS-TRANSFER', Table, _FromPid, {ripe_table, Token, ReplyTo}},
            State=#state{table_token=TableToken, table=OldTable}) ->
    case Token==TableToken of
        true ->
            %% Replace the table:
            ets:delete(OldTable),
            RenamedTable = ets:rename(Table, ?TABLE_NAME),
            error_logger:info_msg("eripedb reload: succeeded (~p entries)",
                                  [ets:info(RenamedTable, size)]),
            State2 = State#state{table=RenamedTable, populated=true},
            Response = loaded;
        false ->
            error_logger:warn_msg("eripedb: Received a table with the wrong token: ~p vs expected ~p",
                                  [Token, TableToken]),
            Response = {error, wrong_token},
            State2 = State
    end,
    case ReplyTo of
        undefined -> ok;
        _ -> gen_server:reply(ReplyTo, Response)
    end,
    {noreply, State2};
handle_info(_Message, State) ->
    %% TODO: Log.
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
async_lookup_with_sync_fallback(Type, IP) when is_atom(Type), is_binary(IP) ->
    try do_lookup(Type,IP) of
        {error, inconsistent} ->
            sync_lookup(Type, IP);
        Result ->
            Result
    catch _:badarg -> % Table does not exist?
            sync_lookup(Type, IP)
    end.

do_lookup(Type, IP) when is_atom(Type), is_binary(IP) ->
    case ets:prev(?TABLE_NAME, {Type, IP, 1000}) of
        {Type, _, _}=Key ->
            %% TODO: Add check that IP is in fact within the prefix!
            case ets:lookup(?TABLE_NAME, Key) of
                [{_Key, Origin}] ->
                    {ok, Origin};
                [] ->
                    {error, inconsistent} % Huh. Unexpected, but can happen in the async case.
            end;
        _ ->
            %% Either '$end_of_table' or something of another class.
            {error, unknown}
    end.


start_reload(From, #state{table_token=Token, database_file=DatabaseFile}) ->
    Me = self(),
    proc_lib:spawn_link(fun() -> reload(DatabaseFile, Me, Token, From) end).

reload(DatabaseFile, ServerPid, Token, ReplyTo) ->
    error_logger:info_msg("eripedb: reload started from file ~p", [DatabaseFile]),
    try ets:new(?TEMP_TABLE_NAME, table_options()) of
        Table ->
            Callback = fun(Key,Value,T) ->
                               ets:insert(T, {Key,Value}),
                               T
                       end,
            error_logger:info_msg("eripedb reload: starting."),
            eripedb_parser2:read(DatabaseFile, Callback, Table),
            error_logger:info_msg("eripedb reload: done - entry_count: ~p", [ets:info(Table,size)]),
            ets:give_away(Table, ServerPid, {ripe_table, Token, ReplyTo})
    catch
        _:badarg ->
            error_logger:info_msg("eripedb reload: table creation failed."),
            gen_server:reply(ReplyTo, {error, already_loading})
    end.
