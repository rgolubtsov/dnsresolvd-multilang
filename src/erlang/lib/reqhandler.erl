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

    % -------------------------------------------------------------------------
    % --- Parsing and validating request params - Begin -----------------------
    % -------------------------------------------------------------------------
    {ok, Params, Req_} = if (Mtd =:= ?_MTD_HTTP_GET ) ->
        {ok, cowboy_req:parse_qs(Req),  Req};
                            (Mtd =:= ?_MTD_HTTP_POST) ->
        cowboy_req:read_urlencoded_body(Req);
                            (true                   ) ->
        {ok, [                      ],  Req}
                         end,

    io:write(Params), io:nl(),

    Hostname_ = [V || {K, V} <- Params, (K =:= <<"h">>)], % <---------+
    %         +----GET----+-----+-----+          ^                    |
    %         |     |     |     |     |          |                    |
    %         |     |     |     |     |       +--+         +----------+-+
    %         v     v     v     v     v       |            |          | |
    % $ curl 'http://localhost:<port_number>/?h=<hostname>&f=<fmt>'   | |
    % $                                                               | |
    % $ curl -d 'h=<hostname>&f=<fmt>' http://localhost:<port_number> | |
    %         ^  |            |                                       | |
    %         |  +------------+---------------------------------------+ |
    %         |               |                                         |
    % POST----+               +------------------+                      |
    %                                            |                      |
    %                                            v                      |
    Fmt_      = [V || {K, V} <- Params, (K =:= <<"f">>)], % <-----------+

    Hostname0 = lists:filter(fun(V) -> (V =/= []) end, Hostname_),
    Hostname0_len = length(Hostname0), Hostname1 = if (Hostname0_len > 0) ->
        lists:nth(Hostname0_len, Hostname0);
                                                      (true             ) ->
        ?_EMPTY_STRING
                                                   end,

    Fmt0      = lists:filter(fun(V) -> (V =/= []) end, Fmt_     ),
    Fmt0_len      = length(Fmt0     ), Fmt1      = if (Fmt0_len      > 0) ->
        lists:nth(Fmt0_len,      Fmt0     );
                                                      (true             ) ->
        ?_EMPTY_STRING
                                                   end,

    Hostname  = if ((       Hostname1  =:= []  )
                or  (       Hostname1  =:= true)
                or  (length(Hostname1) =:= 0   )) ->
        ?_DEF_HOSTNAME;
                   (true                        ) ->
        if (Hostname1 =/= true) ->
            binary_to_list(Hostname1);
           (true              ) ->
            ?_DEF_HOSTNAME
        end
                end,

    Fmt2      = if ((       Fmt1       =:= []  )
                or  (       Fmt1       =:= true)
                or  (length(Fmt1     ) =:= 0   )) ->
        ?_PRM_FMT_JSON;
                   (true                        ) ->
        if (Fmt1      =/= true) ->
            string:to_lower(binary_to_list(Fmt1));
           (true              ) ->
            ?_PRM_FMT_JSON
        end
                end,

    Fmt2_     = lists:member(Fmt2, [
        ?_PRM_FMT_HTML,
        ?_PRM_FMT_JSON
    ]),

    Fmt       = if (not Fmt2_) -> ?_PRM_FMT_JSON;
                   (true     ) -> Fmt2
                end,
    % -------------------------------------------------------------------------
    % --- Parsing and validating request params - End -------------------------
    % -------------------------------------------------------------------------

    {ok,
        Req_,
        State % <== The state of the handler doesn't need to be changed.
    }.

% vim:set nu et ts=4 sw=4:
