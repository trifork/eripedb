-module(lookup_test).

-include_lib("eunit/include/eunit.hrl").

%%% Assume that test data files are in test/data/


test_data_fixture(Filename, Fun) ->
    {setup,
     fun() ->
             application:start(sasl),
             application:load(eripedb),
             application:set_env(eripedb, database_files, ["../test/data/"++Filename]),
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
                              ?assertEqual({ok, <<"SUB12">>},        eripedb:lookup(ipv4, {1,1,255,255})),
                              ?assertEqual({ok, <<"SUB24-FIRST">>}, eripedb:lookup(ipv4, {1,2,0,0})),
                              ?assertEqual({ok, <<"SUB24-FIRST">>}, eripedb:lookup(ipv4, {1,2,0,255})),
                              ?assertEqual({ok, <<"SUB16">>},       eripedb:lookup(ipv4, {1,2,1,0})),
                              ?assertEqual({ok, <<"SUB16">>},       eripedb:lookup(ipv4, {1,2,254,255})),
                              ?assertEqual({ok, <<"SUB24-LAST">>},  eripedb:lookup(ipv4, {1,2,255,0})),
                              ?assertEqual({ok, <<"SUB24-LAST">>},  eripedb:lookup(ipv4, {1,2,255,255})),
                              ?assertEqual({ok, <<"SUB12">>},        eripedb:lookup(ipv4, {1,3,0,0})),
                              ok
                      end).

%%%========== Utility: ========================================

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
