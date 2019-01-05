%
% src/erlang/lib/dnsresolvs.erl
% =============================================================================
% DNS Resolver Daemon (dnsresolvd). Version 0.1
% =============================================================================
% A daemon that performs DNS lookups for the given hostname
% passed in an HTTP request, with the focus on its implementation
% using various programming languages. (Cowboy-boosted impl.)
% =============================================================================
% Copyright (C) 2017-2019 Radislav (Radicchio) Golubtsov
%
% (See the LICENSE file at the top of the source tree.)
%

%% @doc The main --supervisor-- module of the daemon.
-module(dnsresolvs).

-behaviour(supervisor).

-export([start_link/0, init/1]).

%% @doc Supervisor startup helper.<br />
%%      Used to directly start up the <code>dnsresolvs</code> supervisor
%%      process.
%%
%% @returns The PID of the <code>dnsresolvs</code> supervisor process.
start_link() ->
    supervisor:start_link(?MODULE, []).

%% @doc Supervisor <code>init/1</code> callback.<br />
%%      Gets called when the <code>dnsresolvs</code> supervisor
%%      is about to be started up.
%%
%% @returns The tuple containing supervisor flags
%%          and child processes' specifications.
init(_) ->
    Child_procs = [], % <== No child processes do we need.

    {ok, {
        #{strategy => one_for_one},
        Child_procs
    }}.

% vim:set nu et ts=4 sw=4:
