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
    % Calling this just to suppress compiler warning:
    % function cleanups_fixate/1 is unused.
    cleanups_fixate([]),

    Mtd = cowboy_req:method(Req),

    % -------------------------------------------------------------------------
    % --- Parsing and validating request params - Begin -----------------------
    % -------------------------------------------------------------------------
    {ok, Params, Req0} = if (Mtd =:= ?_MTD_HTTP_GET ) ->
        {ok, cowboy_req:parse_qs(Req),  Req};
                            (Mtd =:= ?_MTD_HTTP_POST) ->
        cowboy_req:read_urlencoded_body(Req);
                            (true                   ) ->
        {ok, [                      ],  Req}
                         end,

    Hostname_ = [V || {K, V} <- Params, (K =:= <<"h">>)], % <---------+
    %         +----GET----+-----+-----+           ^                   |
    %         |     |     |     |     |           |                   |
    %         |     |     |     |     |       +---+        +----------+-+
    %         v     v     v     v     v       |            |          | |
    % $ curl 'http://localhost:<port_number>/?h=<hostname>&f=<fmt>'   | |
    % $                                                               | |
    % $ curl -d 'h=<hostname>&f=<fmt>' http://localhost:<port_number> | |
    %         ^  |            |                                       | |
    %         |  +------------+---------------------------------------+ |
    %         |               |                                         |
    % POST----+               +-------------------+                     |
    %                                             |                     |
    %                                             v                     |
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

    % Performing DNS lookup for the given hostname.
    Addr_ver = dns_lookup(Hostname),

    Addr = element(1, Addr_ver),
    Ver  = element(2, Addr_ver),

    Resp_buffer_ = if (Fmt =:= ?_PRM_FMT_HTML) ->
        "<!DOCTYPE html>"                                                                    ++ ?_NEW_LINE
++ "<html lang=\"en-US\" dir=\"ltr\">"                                                       ++ ?_NEW_LINE
++ "<head>"                                                                                  ++ ?_NEW_LINE
++ "<meta http-equiv=\""   ++ ?_HDR_CONTENT_TYPE_N      ++                "\"    content=\""
                           ++ ?_HDR_CONTENT_TYPE_V_HTML ++                "\"           />"  ++ ?_NEW_LINE
++ "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"                            />"  ++ ?_NEW_LINE
++ "<meta       name=\"viewport\"        content=\"width=device-width,initial-scale=1\" />"  ++ ?_NEW_LINE
++ "<title>" ++ ?_DMN_NAME ++ "</title>"                                                     ++ ?_NEW_LINE
++ "</head>"                                                                                 ++ ?_NEW_LINE
++ "<body>"                                                                                  ++ ?_NEW_LINE
++ "<div>"   ++ Hostname   ++ ?_ONE_SPACE_STRING;
                      (Fmt =:= ?_PRM_FMT_JSON) ->
           ?_CB1
        ++ ?_DAT_HOSTNAME_N
        ++ ?_DQ1
        ++ Hostname
        ++ ?_DQ2;
                      (true                  ) ->
        false
                   end,

    % If lookup error occurred.
    Resp_buffer0 = if (Addr =:= ?_ERR_PREFIX) ->
        if (Fmt =:= ?_PRM_FMT_HTML) ->
            Resp_buffer_ ++ ?_ERR_PREFIX
                         ++ ?_COLON_SPACE_SEP
                         ++ ?_ERR_COULD_NOT_LOOKUP;
           (Fmt =:= ?_PRM_FMT_JSON) ->
            Resp_buffer_ ++ ?_ERR_PREFIX
                         ++ ?_DQ1
                         ++ ?_ERR_COULD_NOT_LOOKUP;
           (true                  ) ->
            false
        end;
                      (true                 ) ->
        if (Fmt =:= ?_PRM_FMT_HTML) ->
            Resp_buffer_ ++ Addr
                         ++ ?_ONE_SPACE_STRING
                         ++ ?_DAT_VERSION_V
                         ++ integer_to_list(Ver);
           (Fmt =:= ?_PRM_FMT_JSON) ->
            Resp_buffer_ ++ ?_DAT_ADDRESS_N
                         ++ ?_DQ1
                         ++ Addr
                         ++ ?_DQ2
                         ++ ?_DAT_VERSION_N
                         ++ ?_DQ1
                         ++ ?_DAT_VERSION_V
                         ++ integer_to_list(Ver);
           (true                  ) ->
            false
        end
                   end,

    Resp_buffer = if (Fmt =:= ?_PRM_FMT_HTML) ->
        Resp_buffer0 ++ "</div>"  ++ ?_NEW_LINE
                     ++ "</body>" ++ ?_NEW_LINE
                     ++ "</html>" ++ ?_NEW_LINE;
                     (Fmt =:= ?_PRM_FMT_JSON) ->
        Resp_buffer0 ++ ?_CB2;
                     (true                  ) ->
        false
                  end,

    % Adding headers to the response.
    Req1 = add_response_headers(Fmt, Req0),

    Req2 = cowboy_req:set_resp_body(Resp_buffer, Req1),
    Req_ = cowboy_req:reply(?_RSC_HTTP_200_OK,   Req2),

    {ok,
        Req_,
        State % <== The state of the handler doesn't need to be changed.
    }.

%% @doc Adds headers to the response.
%%
%% @param Fmt The response format selector.
%% @param Req The consolidated HTTP request/response object.
%%
%% @returns The new consolidated HTTP request/response object.
add_response_headers(Fmt, Req) ->
    _HDR_CONTENT_TYPE_V = if (Fmt =:= ?_PRM_FMT_HTML) ->
        ?_HDR_CONTENT_TYPE_V_HTML;
                             (Fmt =:= ?_PRM_FMT_JSON) ->
        ?_HDR_CONTENT_TYPE_V_JSON;
                             (true                  ) ->
        false
                          end,

    cowboy_req:set_resp_headers(#{
        ?_HDR_CONTENT_TYPE_N  =>  _HDR_CONTENT_TYPE_V,
        ?_HDR_CACHE_CONTROL_N => ?_HDR_CACHE_CONTROL_V,
        ?_HDR_EXPIRES_N       => ?_HDR_EXPIRES_V,
        ?_HDR_PRAGMA_N        => ?_HDR_PRAGMA_V
    }, Req).

%##
% Performs DNS lookup action for the given hostname,
% i.e. (in this case) IP address retrieval by hostname.
%
% Args:
%     Hostname: The effective hostname to look up for.
%
% Returns:
%     The tuple containing IP address of the analyzing host/service
%     and corresponding IP version (family) used to look up in DNS:
%     "4" for IPv4-only hosts, "6" for IPv6-capable hosts.
%
dns_lookup(Hostname) ->
    Hostent4 = inet:gethostbyname(Hostname, inet ),

    % If the host doesn't have an A record (IPv4),
    % trying to find its AAAA record (IPv6).
    Hostent6 = if (element(1, Hostent4) =:= error) ->
               inet:gethostbyname(Hostname, inet6);
                  (true                          ) ->
               false
               end,

    if (element(1, Hostent4) =:= ok) ->
        {
            inet:ntoa(hd(element(6, element(2, Hostent4)))),
            4
        };
       (element(1, Hostent6) =:= ok) ->
        {
            inet:ntoa(hd(element(6, element(2, Hostent6)))),
            6
        };
       (true                       ) ->
        {?_ERR_PREFIX, []}
    end.

% vim:set nu et ts=4 sw=4:
