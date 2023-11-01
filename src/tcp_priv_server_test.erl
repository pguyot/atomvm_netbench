% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(tcp_priv_server_test).
-export([run_device_test/2, run_controller_test/2]).

-define(TEST_PORT, 40891).
-define(APPLICATION, atomvm_netbench).
-define(PRIV_FILE_PATH, "AtomVM-logo.png").
-define(PARALLEL, 10).

% =========================================================================== %
% Device code
% =========================================================================== %
run_device_test(ClientSocket, _PeerIP) ->
    {ok, TestSocket} = socket:open(inet, stream, tcp),
    ok = socket:setopt(TestSocket, {socket, reuseaddr}, true),
    ok = socket:bind(TestSocket, #{family => inet, port => ?TEST_PORT, addr => any}),
    ok = socket:listen(TestSocket, ?PARALLEL),
    ok = socket:send(ClientSocket, <<"-READY\r\n">>),
    Workers = test_device_loop(TestSocket, ?PARALLEL, []),
    ok = socket:close(TestSocket),
    lists:foreach(
        fun({Pid, MonRef}) ->
            receive
                {'DOWN', MonRef, process, Pid, normal} ->
                    ok;
                {'DOWN', MonRef, process, Pid, Reason} ->
                    io:format("Worker process terminated with reason ~p\n", [Reason])
            end
        end,
        Workers
    ),
    ok.

test_device_loop(_TestSocket, 0, Workers) ->
    Workers;
test_device_loop(TestSocket, N, AccWorkers) ->
    {ok, ClientSocket} = socket:accept(TestSocket),
    {Pid, MonitorRef} = spawn_opt(fun() -> test_device_server_worker(ClientSocket) end, [monitor]),
    test_device_loop(TestSocket, N - 1, [{Pid, MonitorRef} | AccWorkers]).

test_device_server_worker(ClientSocket) ->
    Data =
        try
            atomvm:read_priv(?APPLICATION, ?PRIV_FILE_PATH)
        catch
            error:undef ->
                % OTP
                {ok, Binary} = file:read_file("priv/" ++ ?PRIV_FILE_PATH),
                Binary
        end,
    Size = byte_size(Data),
    SizeBin = <<Size:32>>,
    ok = sendloop(ClientSocket, SizeBin),
    ok = sendloop(ClientSocket, Data),
    % Wait for client close
    case socket:recv(ClientSocket, 1) of
        {error, closed} -> ok;
        {error, econnreset} -> ok
    end,
    ok = socket:close(ClientSocket),
    ok.

sendloop(ClientSocket, Data) ->
    case socket:send(ClientSocket, Data) of
        {ok, Rest} ->
            sendloop(ClientSocket, Rest);
        Other -> Other
    end.

% =========================================================================== %
% Controller code
% =========================================================================== %
run_controller_test(ClientSocket, ClientIP) ->
    {ok, <<"-READY\r\n">>} = socket:recv(ClientSocket),
    Workers = lists:map(
        fun(_) ->
            spawn_opt(fun() -> test_controller_client_worker(ClientIP) end, [monitor])
        end,
        lists:seq(1, ?PARALLEL)
    ),
    lists:foreach(
        fun({Pid, Ref}) ->
            receive
                {'DOWN', Ref, process, Pid, normal} ->
                    ok;
                {'DOWN', Ref, process, Pid, Reason} ->
                    io:format("Worker process terminated with reason ~p\n", [Reason])
            end
        end,
        Workers
    ),
    ok.

test_controller_client_worker(ClientIP) ->
    {ok, TestSocket} = socket:open(inet, stream, tcp),
    ok = socket:connect(TestSocket, #{family => inet, port => ?TEST_PORT, addr => ClientIP}),
    {ok, <<Size:32>>} = socket:recv(TestSocket, 4),
    test_controller_client_worker_loop(TestSocket, Size),
    ok = socket:close(TestSocket),
    ok.

test_controller_client_worker_loop(_TestSocket, 0) ->
    ok;
test_controller_client_worker_loop(_TestSocket, Size) when Size < 0 ->
    io:format("Warning: received more data than expected\n");
test_controller_client_worker_loop(TestSocket, Size) ->
    {ok, Data} = socket:recv(TestSocket),
    test_controller_client_worker_loop(TestSocket, Size - byte_size(Data)).
