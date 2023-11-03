% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(test_controller).
-export([run_all/2, run/3]).

-define(ALL_TESTS, [udp_echo_test, tcp_echo_test, tcp_priv_server_test, getaddrinfo_test]).

run_all(ClientIP, Port) ->
    run(ClientIP, Port, ?ALL_TESTS).

run(ClientIP, Port, Tests) ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = socket:connect(Socket, #{family => inet, addr => ClientIP, port => Port}),
    lists:foreach(
        fun(Test) ->
            case lists:member(Test, ?ALL_TESTS) of
                true ->
                    run_test(Socket, ClientIP, Test);
                false ->
                    io:format("Unknown test ~s\n", [Test])
            end
        end,
        Tests
    ),
    ok = socket:close(Socket),
    ok.

run_test(Socket, ClientIP, Test) ->
    io:format("~p: ", [Test]),
    ok = socket:send(Socket, [atom_to_binary(Test, latin1), <<"\r\n">>]),
    case Test:run_controller_test(Socket, ClientIP) of
        ok ->
            {ok, Result} = socket:recv(Socket),
            io:format("~p\n", [Result]);
        {failed, Reason} ->
            io:format("failed (~s)\n", [Reason])
    end.
