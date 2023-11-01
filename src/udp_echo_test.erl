% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(udp_echo_test).
-export([run_device_test/2, run_controller_test/2]).

-define(TEST_PORT, 40889).

% =========================================================================== %
% Device code
% =========================================================================== %
run_device_test(ClientSocket, _PeerIP) ->
    {ok, TestSocket} = socket:open(inet, dgram, udp),
    ok = socket:setopt(TestSocket, {socket, reuseaddr}, true),
    ok = socket:bind(TestSocket, #{family => inet, port => ?TEST_PORT, addr => any}),
    ok = socket:send(ClientSocket, <<"-READY\r\n">>),
    test_device_loop(TestSocket),
    ok = socket:close(TestSocket).

test_device_loop(TestSocket) ->
    {ok, {PeerAddr, Message}} = socket:recvfrom(TestSocket),
    ok = socket:sendto(TestSocket, Message, PeerAddr),
    case Message of
        <<0, _/binary>> -> ok;
        _ ->
            test_device_loop(TestSocket)
    end.

% =========================================================================== %
% Controller code
% =========================================================================== %
run_controller_test(ClientSocket, ClientIP) ->
    {ok, <<"-READY\r\n">>} = socket:recv(ClientSocket),
    {ok, TestSocket} = socket:open(inet, dgram, udp),
    ok = socket:bind(TestSocket, #{family => inet, port => 0, addr => any}),
    test_controller_loop(TestSocket, ClientIP, 10).

test_controller_loop(TestSocket, ClientIP, N) ->
    PeerAddr = #{family => inet, addr => ClientIP, port => ?TEST_PORT},
    Message = list_to_binary([N | [N + X rem 256 || X <- lists:seq(1, 10), _ <- lists:seq(1, 10)]]),
    case send_or_retry(TestSocket, Message, PeerAddr, 10) of
        ok when N > 0 -> test_controller_loop(TestSocket, ClientIP, N - 1);
        ok -> ok;
        {failed, _Reason} = FailedTuple -> FailedTuple
    end.

send_or_retry(_TestSocket, _Message, _PeerAddr, 0) -> {failed, <<"Did not receive packet">>};
send_or_retry(TestSocket, Message, PeerAddr, N) ->
    ok = socket:sendto(TestSocket, Message, PeerAddr),
    WaitStart = erlang:system_time(millisecond),
    case receive_loop(TestSocket, Message, PeerAddr, 5000) of
        ok ->
            WaitedTime = erlang:system_time(millisecond) - WaitStart,
            if
                WaitedTime > 1000 ->
                    io:format("Warning: waited for ~B milliseconds\n", [WaitedTime]);
                true ->
                    ok
            end;
        {error, timeout} ->
            io:format("Warning: resending packet\n"),
            send_or_retry(TestSocket, Message, PeerAddr, N - 1)
    end.

receive_loop(TestSocket, Message, #{addr := PeerAddrIP, port := PeerAddrPort} = PeerAddr, Timeout) ->
    case socket:recvfrom(TestSocket, 0, Timeout) of
        {ok, {#{addr := PeerAddrIP, port := PeerAddrPort}, Message}} ->
            ok;
        {ok, {#{addr := PeerAddrIP, port := PeerAddrPort}, _OtherMessage}} ->
            io:format("Unexpected packet, maybe from a past retry\n"),
            receive_loop(TestSocket, Message, PeerAddr, Timeout);
        {ok, {OtherAddr, _OtherMessage}} ->
            io:format("Unexpected packet from ~p\n", [OtherAddr]),
            receive_loop(TestSocket, Message, PeerAddr, Timeout);
        {error, timeout} ->
            {error, timeout}
    end.
