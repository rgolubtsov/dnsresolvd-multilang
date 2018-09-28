%
% src/erlang/lib/dnsresolvd.erl
% =============================================================================
% DNS Resolver Daemon (dnsresolvd). Version 0.1
% =============================================================================
% A daemon that performs DNS lookups for the given hostname
% passed in an HTTP request, with the focus on its implementation
% using various programming languages. (Cowboy-boosted impl.)
% =============================================================================
% Copyright (C) 2017-2018 Radislav (Radicchio) Golubtsov
%
% (See the LICENSE file at the top of the source tree.)
%

%% @doc The main --application-- module of the daemon.
-module(dnsresolvd).

-export([start/2]).

%% @doc Starts up the daemon.<br />
%%      It has to be the application module callback, but used directly
%%      from the startup script of the daemon.
%%
%% @param Args A list containing the server port number to listen on
%%             as the first element.
%%
%% @returns The tuple containing the PID of the top supervisor process
%%          and the state of the running application (or an empty list).
start(_, Args) ->
    {Port_number, Daemon_name, Log} = Args,

%   {ok, _} = application:ensure_all_started(cowboy).
    ok.

% vim:set nu et ts=4 sw=4:
