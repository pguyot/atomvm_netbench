% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(test_controller).
-export([run/2]).

run(ClientIP, Port) ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = socket:connect(Socket, #{family => inet, addr => ClientIP, port => Port}),
    lists:foreach(
        fun(Test) ->
            run_test(Socket, ClientIP, Test)
        end,
        [udp_echo_test, tcp_echo_test, tcp_priv_server_test]
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
