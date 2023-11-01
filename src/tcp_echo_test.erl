% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(tcp_echo_test).
-export([run_device_test/2, run_controller_test/2]).

-define(TEST_PORT, 40890).
-define(MESSAGE_SIZE, 101).

% =========================================================================== %
% Device code
% =========================================================================== %
run_device_test(ClientSocket, PeerIP) ->
    {ok, <<"-READY\r\n">>} = socket:recv(ClientSocket),
    {ok, TestSocket} = socket:open(inet, stream, tcp),
    ok = socket:connect(TestSocket, #{family => inet, port => ?TEST_PORT, addr => PeerIP}),
    test_device_loop(TestSocket).

test_device_loop(TestSocket) ->
    {ok, Message} = socket:recv(TestSocket, ?MESSAGE_SIZE),
    ok = socket:send(TestSocket, Message),
    case Message of
        <<0, _/binary>> ->
            ok = socket:close(TestSocket),
            ok;
        _ ->
            test_device_loop(TestSocket)
    end.

% =========================================================================== %
% Controller code
% =========================================================================== %
run_controller_test(ClientSocket, _ClientIP) ->
    {ok, TestSocket} = socket:open(inet, stream, tcp),
    ok = socket:bind(TestSocket, #{family => inet, port => ?TEST_PORT, addr => any}),
    ok = socket:listen(TestSocket),
    ok = socket:send(ClientSocket, <<"-READY\r\n">>),
    {ok, ConnectedSocket} = socket:accept(TestSocket),
    ok = socket:close(TestSocket),
    test_controller_loop(ConnectedSocket, 10).

test_controller_loop(TestSocket, N) ->
    Message = list_to_binary([N | [N + X rem 256 || X <- lists:seq(1, 10), _ <- lists:seq(1, 10)]]),
    ?MESSAGE_SIZE = byte_size(Message),
    ok = socket:send(TestSocket, Message),
    case socket:recv(TestSocket, ?MESSAGE_SIZE) of
        {ok, Message} when N > 0 -> test_controller_loop(TestSocket, N - 1);
        {ok, Message} ->
            ok = socket:close(TestSocket),
            ok;
        {ok, _OtherMessage} ->
            {failed, <<"Packet mismatch">>};
        {error, Reason} ->
            {failed, list_to_binary(io_lib:format("Unexpected error ~p", [Reason]))}
    end.
