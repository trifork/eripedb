-module(lookup_test).

-include_lib("eunit/include/eunit.hrl").

%%% Assume that test data files are in test/data/

-define(TEMP_FILE, "test.tmp").

test_data_fixture(Filename, Fun) ->
    {setup,
     fun() ->
             application:start(sasl),
             application:load(eripedb),
             set_filename(Filename),
             application:start(eripedb)
     end,
     fun(_) ->
             application:stop(eripedb)
     end,
     Fun}.

temp_data_fixture(Fun) ->
    {setup,
     fun() ->
             ok = file:write_file(?TEMP_FILE, <<>>),
             application:start(sasl),
             application:load(eripedb),
             application:set_env(eripedb, database_files, [?TEMP_FILE]),
             application:start(eripedb),
             wait_for(fun() -> eripedb:populated() end, 1000)
     end,
     fun(_) ->
             %file:delete(?TEMP_FILE),
             application:stop(eripedb)
     end,
     Fun}.

simple_lookup_test_() ->
    test_data_fixture("ipv4.db",
                      fun() ->
                              wait_for(fun() -> eripedb:populated() end, 1000),

                              ?assertEqual({ok, <<"SUB24">>},
                                           eripedb:lookup(ipv4, {1,2,3,4})),
                              ?assertEqual({ok, <<"SUB24">>},
                                           eripedb:lookup(ipv4, {1,2,3,0})),
                              ?assertEqual({ok, <<"SUB24">>},
                                           eripedb:lookup(ipv4, {1,2,3,255})),
                              ?assertEqual({ok, <<"SUB16">>},
                                           eripedb:lookup(ipv4, {1,2,4,0})),
                              ?assertEqual({ok, <<"SUB12">>},
                                           eripedb:lookup(ipv4, {1,4,0,0})),
                              ?assertEqual({ok, <<"SUB12">>},
                                           eripedb:lookup(ipv4, {1,1,0,0})),
                              ?assertEqual({ok, <<"SUB12">>},
                                           eripedb:lookup(ipv4, {1,1,255,255})),
                              ?assertEqual({ok, <<"SUB12">>},
                                           eripedb:lookup(ipv4, {1,15,0,0})),
                              ?assertEqual({ok, <<"SUB12">>},
                                           eripedb:lookup(ipv4, {1,15,255,255})),
                              ?assertEqual({ok, <<"SUB8">>},
                                           eripedb:lookup(ipv4, {1,16,0,0})),
                              ?assertEqual({ok, <<"SUB8">>},
                                           eripedb:lookup(ipv4, {1,255,255,255})),
                              %% Around "SUB8" and subs:
                               ?assertEqual({error, unknown},
                                           eripedb:lookup(ipv4, {0,0,0,0})),
                              ?assertEqual({error, unknown},
                                            eripedb:lookup(ipv4, {2,0,0,0})),
                              ?assertEqual({error, unknown},
                                            eripedb:lookup(ipv4, {255,255,255,255})),

                              %% Probe edges of "BUS16":
                              ?assertEqual({error, unknown},
                                            eripedb:lookup(ipv4, {250,250,255,255})),
                              ?assertEqual({ok, <<"BUS16">>},
                                            eripedb:lookup(ipv4, {250,251,0,0})),
                              ?assertEqual({ok, <<"BUS16">>},
                                            eripedb:lookup(ipv4, {250,251,255,255})),
                              ?assertEqual({error, unknown},
                                            eripedb:lookup(ipv4, {250,252,0,0})),

                              %% Look at "SUB24-FIRST" and "SUB24-LAST" - zero and 255 element special cases:
                              ?assertEqual({ok, <<"SUB12">>},       eripedb:lookup(ipv4, {1,1,255,255})),
                              ?assertEqual({ok, <<"SUB24-FIRST">>}, eripedb:lookup(ipv4, {1,2,0,0})),
                              ?assertEqual({ok, <<"SUB24-FIRST">>}, eripedb:lookup(ipv4, {1,2,0,255})),
                              ?assertEqual({ok, <<"SUB16">>},       eripedb:lookup(ipv4, {1,2,1,0})),
                              ?assertEqual({ok, <<"SUB16">>},       eripedb:lookup(ipv4, {1,2,254,255})),
                              ?assertEqual({ok, <<"SUB24-LAST">>},  eripedb:lookup(ipv4, {1,2,255,0})),
                              ?assertEqual({ok, <<"SUB24-LAST">>},  eripedb:lookup(ipv4, {1,2,255,255})),
                              ?assertEqual({ok, <<"SUB12">>},       eripedb:lookup(ipv4, {1,3,0,0})),
                              ok
                      end).

ipv6_to_ipv4_fallback_test_() ->
    test_data_fixture("ipv4.db",
                      fun() ->
                              wait_for(fun() -> eripedb:populated() end, 1000),

                              ?assertEqual({ok, <<"SUB24">>},
                                           eripedb:lookup(ipv6, {0,0,0,0,0,65535,16#102,16#304})),
                              ok
                      end).


reload_gap_test_() ->
    test_data_fixture("ipv4.db",
                      fun() ->
                              wait_for(fun() -> eripedb:populated() end, 1000),

                              Me = self(),
                              proc_lib:spawn_link(fun() -> lookup_N_times(ipv4, {1,2,3,4}, 100, 1, Me) end),
                              timer:sleep(10),
                              set_filename("alternative.db"),
                              eripedb:reload(),
                              Results = [receive
                                             {lookup_result, Res} ->
                                                 Res
                                         after 100 ->
                                                 timeout
                                         end || _ <- lists:seq(1,100)],
                              ?assertEqual(100, length(Results)),
                              %io:format(user, "DB| reload_gap_test_:\n => ~p\n", [Results]),
                              ?assertEqual([{ok,<<"ALT24">>}, {ok,<<"SUB24">>}], % We get both "before" and "after" results, but no errors.
                                           lists:usort(Results))
                      end).

lookup_N_times(Class, IP, Iters, Delay, Destination) ->
    if Iters > 0 ->
            Res = eripedb:lookup(Class, IP),
            Destination ! {lookup_result, Res},
            timer:sleep(Delay),
            lookup_N_times(Class, IP, Iters-1, Delay, Destination);
       true ->
            ok
    end.

lookup_speed_test_() ->
    test_data_fixture("ipv4.db",
                      fun() ->
                              F = fun(0,_) -> ok;
                                     (N,F) ->
                                          eripedb:lookup(ipv4, {0,0,0,0}),
                                          eripedb:lookup(ipv4, {1,0,0,0}),
                                          eripedb:lookup(ipv4, {1,2,0,0}),
                                          eripedb:lookup(ipv4, {1,2,3,0}),
                                          eripedb:lookup(ipv4, {1,2,3,4}),
                                          eripedb:lookup(ipv6, {0,0,0,0,0,0,0,0}),
                                          F(N-1,F)
                                  end,
                              N=10000,
                              {Time,_} = timer:tc(fun() -> F(N, F) end),
                              Calls = 6*N,
                              io:format(user, "Lookup speed: ~.3fus/call; ~.1f calls/s\n", [Time/Calls, Calls/(Time/1.0e6)])
                      end).

random_test_() ->
    temp_data_fixture(
      {timeout, 60000,
       fun() ->
               Sizes = lists:seq(1,32),
               lists:foreach(fun(Size) ->
                                     Iterations = max(5, min(100, Size)),
                                     io:format(user, "Random tests of size ~b...\n", [Size]),
                                     lists:foreach(fun(_) ->
                                                           random_test_with_size(Size)
                                                   end,
                                                   lists:seq(1,Iterations))
                             end, Sizes)
       end
     }).

random_test_with_size(Size) ->
    Entries = random_entries(Size),
    io:format("DB| Running test (size ~p) with entries: ~p\n", [Size,Entries]),
    ok = file:write_file(?TEMP_FILE, format_ripedb_file(Entries)),
    {ok, loaded} = eripedb:reload(),
    QueryIPs = lists:map(fun(_) -> random_ip() end, lists:seq(1, max(Size,100))),
    %% io:format("DB| Querying with: ~p\n", [QueryIPs]),
    lists:foreach(fun(IP) ->
                          query_and_check(IP, Entries)
                  end,
                  QueryIPs).

query_and_check({Class,IP}, Entries) ->
    IPTuple = case byte_size(IP) of
                  4 -> ipv4_to_tuple(IP);
                  16 -> ipv6_to_tuple(IP)
              end,
    ActualResult = eripedb:lookup(Class, IPTuple),
    ExpectedResult = expected_result(Class, IP, Entries),
    ?assertEqual({ExpectedResult, IPTuple}, {ActualResult, IPTuple}).

expected_result(Class, IP, Entries) ->
    %% TODO: Model ipv4 embedding.
    Matching = [{Entry, -bit_size(Prefix)}
                || Entry={{C, Prefix}, Value} <- Entries,
                   C==Class,
                   is_bitstring_prefix(Prefix, IP)],
    case lists:keysort(2, Matching) of
        [] ->
            {error, unknown};
        [{{_,Value},_NegLen} | _] ->
            {ok, Value}
    end.


random_entries(Size) ->
    NEntries = random:uniform(2*Size),
    CPrefixes = [random_prefix(Size) || _ <- lists:seq(1,NEntries)],
    case length(CPrefixes) == length(lists:usort(CPrefixes)) of
        false ->
            % Collisions - retry:
            random_entries(Size);
        true ->
            [{CPrefix, random_name()} || CPrefix <- CPrefixes]
    end.

random_name() ->
    String = [($A + random:uniform(26)-1) || _ <- lists:seq(1,1+random:uniform(10))],
    list_to_binary(String).

random_ip() ->
    {Class,NBytes} = case random:uniform(2) of
              1 -> {ipv4,4};
              2 -> {ipv6,16}
            end,
    {Class, random_binary(NBytes)}.

random_binary(NBytes) ->
    NBits = 8*NBytes,
    case random:uniform(3) of
        1 ->
            %% Totally random.
            crypto:rand_bytes(NBytes);
        2 ->
            %% One bit set or unset.
            Base = random:uniform(2)-2, % 0 or -1.
            SingleBit = 1 bsl (random:uniform(8*NBytes)-1),
            <<(Base bxor SingleBit):NBits>>;
        3 ->
            %% Simple combinations.
            case random:uniform(4) of
                1 -> % Inversion.
                    <<X:NBits>> = random_binary(NBytes),
                    <<(X bxor -1):NBits>>;
                2 -> % AND.
                    <<X:NBits>> = random_binary(NBytes),
                    <<Y:NBits>> = random_binary(NBytes),
                    <<(X band Y):NBits>>;
                3 -> % OR.
                    <<X:NBits>> = random_binary(NBytes),
                    <<Y:NBits>> = random_binary(NBytes),
                    <<(X bor Y):NBits>>;
                4 -> % XOR - why not
                    <<X:NBits>> = random_binary(NBytes),
                    <<Y:NBits>> = random_binary(NBytes),
                    <<(X bor Y):NBits>>
            end
    end.

random_prefix(Size) ->
    {Class, IP} = random_ip(),
    MaxBits = if Size>20 -> bit_size(IP);
                 true -> min(bit_size(IP), Size)
              end,
    PrefixLength = random:uniform(MaxBits+1)-1,
    <<Prefix:PrefixLength/bitstring, _/bitstring>> = IP,
    {Class, Prefix}.

format_ripedb_file(Entries) ->
    unicode:characters_to_binary([format_ripedb_entry(E) || E <- Entries]).

format_ripedb_entry({{Class, Prefix}, Origin}) ->
    [case Class of
         ipv4 -> ["route: ", format_ipv4_prefix(Prefix)];
         ipv6 -> ["route6: ", format_ipv6_prefix(Prefix)]
     end,
     "\n",
     "origin: ", Origin, "\n",
     "\n"
    ].

format_ipv4_prefix(Prefix) ->
    [inet_parse:ntoa(ipv4_to_tuple(pad_to_length(32, Prefix))),
     "/", integer_to_list(bit_size(Prefix))].

format_ipv6_prefix(Prefix) ->
    [inet_parse:ntoa(ipv6_to_tuple(pad_to_length(128, Prefix))),
     "/", integer_to_list(bit_size(Prefix))].

%%%========== Utility: ========================================

set_filename(Filename) ->
    application:set_env(eripedb, database_files, ["../test/data/"++Filename]).


wait_for(Fun, TimeoutMillis) ->
    T0 = erlang:monotonic_time(1000),
    Deadline = T0 + TimeoutMillis,
    wait_until(Fun, Deadline, 100).

wait_until(Fun, Deadline, YieldsLeft) ->
    case Fun() of
        true ->
            ok;
        false ->
            T = erlang:monotonic_time(1000),
            case T >= Deadline of
                true ->
                    error(timeout);
                false ->
                    if YieldsLeft>0 -> erlang:yield();
                       true -> timer:sleep(1)
                    end,
                    wait_until(Fun, Deadline, YieldsLeft-1)
            end
    end.

is_bitstring_prefix(A,B) when is_bitstring(A), is_bitstring(B) ->
    ALen = bit_size(A),
    case B of
        <<A:ALen/bitstring, _/bitstring>> -> true;
        _ -> false
    end.

ipv4_to_tuple(<<A,B,C,D>>) -> {A,B,C,D}.
ipv6_to_tuple(<<A:16,B:16,C:16,D:16, E:16,F:16,G:16,H:16>>) -> {A,B,C,D,E,F,G,H}.

pad_to_length(L, Bitstring) ->
    PadSize = L - bit_size(Bitstring),
    <<Bitstring/bitstring, 0:PadSize>>.
