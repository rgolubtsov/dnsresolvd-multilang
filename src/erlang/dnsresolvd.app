%
% src/erlang/dnsresolvd.app
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

{application, dnsresolvd, [
    {description,
        "A daemon that performs DNS lookups for the given hostname passed in an HTTP request."
    },
    {applications, [
        cowboy
    ]},
    {mod, {
    }}
]}.

% vim:set nu et ts=4 sw=4:
