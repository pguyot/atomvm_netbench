% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(https_client_test).
-export([run_device_test/2, run_controller_test/2]).

% =========================================================================== %
% Device code
% =========================================================================== %
run_device_test(_ClientSocket, _PeerIP) ->
    ok = ssl:start(),
    {ok, SSLSocket} = ssl:connect("erlang.org", 443, [
        {verify, verify_none}, {active, false}, {binary, true}
    ]),
    ok = ssl:send(
        SSLSocket, <<"GET / HTTP/1.1\r\nHost: erlang.org\r\nUser-Agent: atomvm_netbench\r\n\r\n">>
    ),
    {ok, <<"HTTP/1.1">>} = ssl:recv(SSLSocket, 8),
    ok = ssl:close(SSLSocket).

% =========================================================================== %
% Controller code
% =========================================================================== %
run_controller_test(_ClientSocket, _ClientIP) ->
    ok.
