% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(atomvm_netbench).
-export([start/0, main/1]).

-define(PORT, 40888).

% escriptize entry point for:
% - OTP-based controller (with an IP address)
% - OTP-based device (with --device)
main(["--device"]) ->
    test_device:run(?PORT);
main([ClientIPStr]) ->
    {ok, ClientIP} = inet:getaddr(ClientIPStr, inet),
    test_controller:run(ClientIP, ?PORT).

start() ->
    test_device:run(?PORT).
