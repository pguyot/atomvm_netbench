% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(getaddrinfo_test).
-export([run_device_test/2, run_controller_test/2]).

-define(EXISTING_NAMES, [
    "atomvm.net",
    "www.atomvm.net",
    "www.erlang.org",
    "www.google.com",
    "www.github.com"
]).

-define(NON_EXISTING_NAMES, [
    "invalid.atomvm.net",
    "dummy.invalid"
]).

% =========================================================================== %
% Device code
% =========================================================================== %
run_device_test(_ClientSocket, _PeerIP) ->
    lists:foreach(
        fun(Name) ->
            {ok, Results} = net:getaddrinfo(Name),
            true = length(Results) >= 2
        end,
        ?EXISTING_NAMES
    ),
    lists:foreach(
        fun(Name) ->
            {error, enoname} = net:getaddrinfo(Name)
        end,
        ?NON_EXISTING_NAMES
    ).

% =========================================================================== %
% Controller code
% =========================================================================== %
run_controller_test(_ClientSocket, _ClientIP) ->
    ok.
