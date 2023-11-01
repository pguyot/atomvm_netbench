% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(test_device).
-export([run/1]).

% Code to run on a device to execute the various tests.
% First, listen using TCP and wait for the controller to connect.
run(Port) ->
    NetworkStart = try atomvm:platform() of
        pico -> true;
        esp32 -> true;
        _ -> false
    catch _:undef -> false
    end,
    {Pid, MonitorRef} = if
        NetworkStart ->
            start_network(Port);
        true ->
            spawn_opt(fun() -> start_test(Port) end, [monitor])
    end,
    receive
        {'DOWN', MonitorRef, process, Pid, normal} -> ok;
        {'DOWN', MonitorRef, process, Pid, Reason} ->
            io:format("Process exited with reason ~p\n", [Reason])
    end.

start_network(Port) ->
    Config = maps:get(sta, config:get()),
    io:format("Connecting to wifi access point\n"),
    {ok, {Address, _Netmask, _Gateway}} = network:wait_for_sta(Config, 30000),
    io:format("Device will listen on ~p port ~B\n", [Address, Port]),
    spawn_opt(fun() -> start_test(Port) end, [monitor]).

start_test(Port) ->
    {ok, ServerSocket} = socket:open(inet, stream, tcp),
    ok = socket:setopt(ServerSocket, {socket, reuseaddr}, true),
    ok = socket:bind(ServerSocket, #{family => inet, port => Port, addr => any}),
    ok = socket:listen(ServerSocket),
    test_loop(ServerSocket).

test_loop(ServerSocket) ->
    io:format("Waiting for connection from test controller\n"),
    {ok, ClientSocket} = socket:accept(ServerSocket),
    {ok, #{addr := PeerIP}} = socket:peername(ClientSocket),
    io:format("Connected, peer is ~p\n", [PeerIP]),
    test_run_loop(ClientSocket, PeerIP),
    test_loop(ServerSocket).

test_run_loop(ClientSocket, PeerIP) ->
    case socket:recv(ClientSocket) of
        {ok, Line} ->
            try
                [TestName, <<>>] = binary:split(Line, <<"\r\n">>),
                TestNameAtom = binary_to_atom(TestName, latin1),
                Start = erlang:system_time(millisecond),
                TestNameAtom:run_device_test(ClientSocket, PeerIP),
                End = erlang:system_time(millisecond),
                Elapsed = End - Start,
                ok = socket:send(ClientSocket, io_lib:format("+OK ~Bms\r\n", [Elapsed]))
            catch Ex:Err:Stacktrace ->
                io:format("Exception occurred\nError: {~p, ~p}\nTest line:\n----\n~s----\nStacktrace:\n~p\n", [Ex, Err, Line, Stacktrace]),
                ok = socket:send(ClientSocket, io_lib:format("+ERR {~p, ~p}\r\n", [Ex, Err]))
            end,
            test_run_loop(ClientSocket, PeerIP);
        {error, closed} ->
            ok
    end.
