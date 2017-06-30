-module(eripedb).
%%% **********************************************************************
%%% * PURPOSE: Server part, for containing state and servicing requests. *
%%% **********************************************************************

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
    gen_server:gen_call({local, ?SERVER}, {sync_lookup, Type, IP}).

reload() ->
    gen_server:gen_call({local, ?SERVER}, reload, 20*1000).

%%%===================================================================
%%% Data structures
%%%===================================================================

-record(state, {
          table :: ets:tid(),
          populated :: boolean()
         }).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({DatabaseFile}) ->
    {ok, #state{
            table = ets:new(?TABLE_NAME, [named_table, {keypos,1}]),
            populated = false
           }}.

handle_call({sync_lookup, Type, IP}, _From, State) ->
    case State of
        #state{populated=false} ->
            Reply = {error, no_database};
        #state{populated=true} ->
            Reply = do_lookup(Type, IP)
        end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    %% TODO: Log.
%    {reply, Reply, State}.
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
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
            case ets:lookup(?TABLE_NAME, Key) of
                [{_Key, Origin}] ->
                    {ok, Origin};
                [] ->
                    {error, inconsistent} % Huh. Unexpected.
            end;
        _ ->
            %% Either '$end_of_table' or something of another class.
            {error, unknown}
    end.

