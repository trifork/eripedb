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
%%% |   this means that a single to a few ets:prev()-lookups suffice to
%%% |   get to the relevant entry.
%%% | - When data is being loaded, it is inserted into another table.
%%% |   This table is then renamed, after which it is the master table.
%%% |   The necessary precautions are taken to ensure that lookups still
%%% |   work during this swapping period.

-behaviour(gen_server).

%% API
-export([start_link/0,
         lookup/2,
         sync_lookup/2,
         reload/0,
         populated/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE_NAME, eripedb_data).
-define(TEMP_TABLE_NAME, eripedb_temp).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

lookup(ipv4, {A,B,C,D}) ->
    async_lookup_with_sync_fallback(ipv4, <<A,B,C,D>>);
lookup(ipv6, {A,B,C,D, E,F,G,H}) ->
    async_lookup_with_sync_fallback(ipv6, <<A,B,C,D, E,F,G,H>>).

sync_lookup(Type, IP) when is_atom(Type), is_binary(IP) ->
    gen_server:call(?SERVER, {sync_lookup, Type, IP}).

reload() ->
    gen_server:call(?SERVER, reload, 20*1000).

populated() ->
    gen_server:call(?SERVER, populated).

%%%===================================================================
%%% Data structures
%%%===================================================================

-record(state, {
          database_files :: string(),
          table :: ets:tid(),
          populated :: boolean(),
          table_token :: reference()
         }).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


-ifdef(TEST).
-define(delay_if_under_test(Amount), timer:sleep(Amount)).
-else.
-define(delay_if_under_test(_), ok).
-endif.

init({}) ->
    self() ! reload,
    %process_flag(trap_exit, false), % Be able to link to sub processes
    {ok, #state{
            database_files = [],
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
handle_call(populated, _From, State) ->
    {reply, State#state.populated, State};
handle_call({set_database_files, Filename}, _From, State) when is_list(Filename) ->
    {reply, ok, State#state{database_files=Filename}};
handle_call(_Request, _From, State) ->
    %% TODO: Log.
%    {reply, Reply, State}.
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reload, State) ->
    start_reload(undefined, State),
    {noreply, State};
handle_info({'ETS-TRANSFER', Table, _FromPid, {ripe_table, Token, ReplyTo, NewDatabaseFiles}},
            State=#state{table_token=TableToken, table=OldTable}) ->
    case Token==TableToken of
        true ->
            %% Replace the table:
            ets:delete(OldTable),
            ?delay_if_under_test(100),         % Expose race condition
            RenamedTable = ets:rename(Table, ?TABLE_NAME),
            error_logger:info_msg("eripedb reload: succeeded (~p entries)",
                                  [ets:info(RenamedTable, size)]),
            State2 = State#state{
                       table=RenamedTable,
                       populated=true,
                       database_files=NewDatabaseFiles
                      },
            Response = loaded;
        false ->
            error_logger:error_msg("eripedb: Received a table with the wrong token: ~p vs expected ~p",
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

do_lookup(Type, IP) when is_atom(Type), is_bitstring(IP) ->
    case ets:prev(?TABLE_NAME, {Type, IP, 1}) of
        {Type, KeyPrefix, _Dummy}=Key ->
            case ets:lookup(?TABLE_NAME, Key) of
                []  ->
                    Prefix=Origin=undefined;
                [{Key, Origin}] ->
                    Prefix = KeyPrefix;
                [{Key, Origin, Prefix}] ->      % Gap fill entry
                    ok
            end,
            case Prefix of
                undefined ->
                    {error, inconsistent}; % Slightly unexpected, but can happen in the async case.
                _ when is_bitstring(Prefix) ->
                    case is_bitstring_prefix(Prefix,IP) of
                        true ->
                            %% Prefix matches!
                            {ok, Origin};
                        _ ->
                            %% Prefix does not match: item is not in the database.
                            {error, unknown}
                    end
            end;
        {_OtherClass, _, _} ->
            {error, unknown};
        '$end_of_table' ->
            {error, unknown}
    end.


start_reload(ReplyTo, #state{table_token=Token}) ->
    Me = self(),
    supervisor:start_child(eripedb_sup,
                           {eripedb_loader,
                            {erlang, apply, [fun reload_process/3, [Me, Token, ReplyTo]]},
                            temporary,
                            100,
                            worker,
                            [?MODULE, eripedb_parser]}).
    %% proc_lib:spawn_link(fun() ->
    %%                             DatabaseFiles = database_files_from_config(),
    %%                             reload(DatabaseFiles, Me, Token, ReplyTo)
    %%                     end).

database_files_from_config() ->
    case application:get_env(eripedb, database_files) of
        undefined ->
            error_logger:error_msg("eripedb: No database_files have been configured."),
            DatabaseFiles = [];
        {ok, DatabaseFiles} ->
            ok
    end,
    DatabaseFiles.

reload_process(ServerPid, Token, ReplyTo) ->
    try
        DatabaseFiles = database_files_from_config(),
        ?delay_if_under_test(50),           % Expose loading state
        reload(DatabaseFiles, ServerPid, Token, ReplyTo)
    catch Cls:Err ->
            error_logger:error_msg("Reload failed: ~p:~p\n** Trace: ~p\n",
                                   [Cls, Err, Trace=erlang:get_stacktrace()]),
            erlang:raise(Cls, Err, Trace)       % Let crash.
    end.



reload(DatabaseFiles, ServerPid, Token, ReplyTo) ->
    error_logger:info_msg("eripedb: reloading from files ~p", [DatabaseFiles]),
    try ets:new(?TEMP_TABLE_NAME, table_options()) of
        Table ->
            Callback = fun(Key,Value,T) ->
                               ets:insert(T, {Key,Value}),
                               T
                       end,
            lists:foreach(fun(DBF) ->
                                  T0 = erlang:monotonic_time(1000000),
                                  error_logger:info_msg("eripedb reload: reading ~p...", [DBF]),
                                  case eripedb_parser:read(DBF, Callback, Table) of
                                      {ok, _} ->
                                          ok;
                                      {error, Reason} ->
                                          error_logger:error_msg("eripedb reload: reading ~p failed: ~p", [DBF, Reason]),
                                          error({failed_to_read_file, DBF, Reason})
                                  end,
                                  T1 = erlang:monotonic_time(1000000),
                                  error_logger:info_msg("eripedb reload: read ~p in ~Bms", [DBF, round((T1-T0)/1000.0)])
                         end,
                         DatabaseFiles),
            error_logger:info_msg("eripedb reload: done - entry_count=~p", [ets:info(Table,size)]),
            complete_table(Table),
            error_logger:info_msg("eripedb reload: Table gaps filled - entry_count=~p", [ets:info(Table,size)]),
            ets:give_away(Table, ServerPid, {ripe_table, Token, ReplyTo, DatabaseFiles})
    catch
        _:badarg ->
            error_logger:info_msg("eripedb reload: table creation failed."),
            gen_server:reply(ReplyTo, {error, already_loading})
    end.


%%% Complete table of {Prefix, Origin} with items of {FromKey, Origin, Prefix},
%%% so that a single ets:prev() lookup suffices to answer ASN queries.
%%% In the worst case, this doubles the table size.
%%% A new entry is needed whenever two neighbour entries A,B does not
%%% fulfill "A is prefix of B" or "A ends where B begins".

complete_table(Tab) ->
    complete_table(ets:first(Tab), [], Tab).

%% Stack is a list of [{Prefix,Origin}], with nested prefixes, most specific first.
%% Invariant 1: Prefixes in stack are nested.
%% Invariant 2: All gaps before 'Last' have been filled.
%% Invariant 3: 'Last' and 'Key' are neighbour keys (Last < Key).
complete_table(Key, Stack, Tab) ->
    Stack2 = fill_table_gap(Key, Stack, Tab),
    case Key of
        '$end_of_table' -> ok;
        _ ->
            [{Key={Class,Prefix,_}, Origin}] = ets:lookup(Tab, Key),
            NextKey = ets:next(Tab, Key),
            complete_table(NextKey, [{Class,Prefix,Origin} | Stack2], Tab)
    end.

%% fill_table_gap(): Ensure that a) the stack is popped such that all
%% its elements are prefixes of Key, with the same class; and b)
%% fill-elements are added to the table for all popped elements, as necessary.
fill_table_gap(_NextKey, [], _Tab) ->
    [];
fill_table_gap(NextKey, [{Class, Prefix, _} | RestStack]=Stack, Tab) ->
    DoPop =
        case NextKey of
            '$end_of_table' -> true;
            {NextClass, _NextPrefix, _} when NextClass /= Class -> true;
            {_NextClass, NextPrefix, _} ->
                not is_bitstring_prefix(Prefix, NextPrefix)
        end,
    case DoPop of
        false ->
            Stack;                              % Done popping.
        true ->
            %% Pop - and add table entry if necessary:
            case RestStack of
                [] ->
                    ok;                       % Nothing to fill with.
                [{ParentClass, ParentPrefix, ParentOrigin} | _] ->
                    FillFromPrefix = first_after_prefix(Prefix),
                    case FillFromPrefix > Prefix andalso % All-ones handling
                        is_bitstring_prefix(ParentPrefix, FillFromPrefix) of
                        true ->
                            FillFromKey = {ParentClass, FillFromPrefix, 0},
                            FillEntry = {FillFromKey, ParentOrigin, ParentPrefix},
                            ets:insert(Tab, FillEntry);
                        false ->
                            ok                  % Nothing to fill, or fill with.
                    end
            end,
            fill_table_gap(NextKey, RestStack, Tab)
    end.

first_after_prefix(Key) ->
    Bits = bit_size(Key),
    <<X:Bits>> = Key,
    <<(X+1):Bits>>.

is_bitstring_prefix(A,B) ->
    ALen = bit_size(A),
    case B of
        <<A:ALen/bitstring, _/bitstring>> -> true;
        _ -> false
    end.
