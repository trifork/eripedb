-module(lookup_test).

-include_lib("eunit/include/eunit.hrl").

%%% Assume that test data files are in test/data/


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


%%%========== Utility: ========================================

set_filename(Filename) ->
    application:set_env(eripedb, database_files, ["../test/data/"++Filename]).


wait_for(Fun, TimeoutMillis) ->
    T0 = erlang:monotonic_time(1000),
    Deadline = T0 + TimeoutMillis,
    wait_until(Fun, Deadline).

wait_until(Fun, Deadline) ->
    case Fun() of
        true ->
            ok;
        false ->
            T = erlang:monotonic_time(1000),
            case T >= Deadline of
                true ->
                    error(timeout);
                false ->
                    timer:sleep(2),
                    wait_until(Fun, Deadline)
            end
    end.
