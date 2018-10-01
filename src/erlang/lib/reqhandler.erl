%
% src/erlang/lib/reqhandler.erl
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

%% @doc The default HTTP request handler.
-module(reqhandler).

-export([init/2]).

-include("dnsresolvd.h").

%% @doc The request handler `init/2` callback.<br />
%%      Gets called when a new incoming HTTP request is received.
%%
%% @param Req   The incoming HTTP request object.
%% @param State The initial state of the HTTP handler.
%%
%% @returns The tuple containing the HTTP response to be rendered
%%          and a new state of the HTTP handler.
init(Req, State) ->
    Mtd = cowboy_req:method(Req),

    io:put_chars("==> " ++ Mtd), io:nl(),

    % -------------------------------------------------------------------------
    % --- Parsing and validating request params - Begin -----------------------
    % -------------------------------------------------------------------------
    {ok, Params, Req_} = if (Mtd =:= ?_MTD_HTTP_GET ) ->
        {ok, cowboy_req:parse_qs(Req), Req};
                            (Mtd =:= ?_MTD_HTTP_POST) ->
        cowboy_req:read_urlencoded_body(Req);
                            (true                   ) ->
        {ok, [], false}
                         end,

    io:write(Params), io:nl(),
    % -------------------------------------------------------------------------
    % --- Parsing and validating request params - End -------------------------
    % -------------------------------------------------------------------------

    {ok,
        Req_,
        State % <== The state of the handler doesn't need to be changed.
    }.

% vim:set nu et ts=4 sw=4:
