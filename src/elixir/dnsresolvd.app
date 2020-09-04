%
% src/elixir/dnsresolvd.app
% =============================================================================
% DNS Resolver Daemon (dnsresolvd). Version 0.1
% =============================================================================
% A daemon that performs DNS lookups for the given hostname
% passed in an HTTP request, with the focus on its implementation
% using various programming languages. (Cowboy-boosted impl.)
% =============================================================================
% Copyright (C) 2017-2020 Radislav (Radicchio) Golubtsov
%
% (See the LICENSE file at the top of the source tree.)
%

% === Note: This Erlang Application configuration can be used in those cases
% ===       when it is desired to run the daemon based on Elixir-provided flags
% ===       for running the Application module directly, i.e. bypassing
% ===       the daemon's initial startup script. In other words the daemon
% ===       might be started by the following command (based on this config):
% ===
% ===       $ ELIXIR_ERL_OPTIONS="-pz lib erlang_modules/deps/syslog/ebin \
% ===                                     erlang_modules/deps/cowboy/ebin \
% ===                                     erlang_modules/deps/cowlib/ebin \
% ===                                     erlang_modules/deps/ranch/ebin" \
% ===         elixir --no-halt --app dnsresolvd
% ===
% ===       But this configuration file is not needed at all if the daemon
% ===       is intended (and it should be a preferable way !) to be run
% ===       via its initial startup script -- i.e. without this:
% ===       "elixir --no-halt --app dnsresolvd".
% ===       
% ===       "elixir --no-halt" is replaced with the "Process.sleep(:infinity)" call.
% ===       "--app dnsresolvd" is replaced with the "{:ok, _} = Application.ensure_all_started(:cowboy)" call.
% ===
% ===       So KISS: $ ELIXIR_ERL_OPTIONS="-pz lib ..." ./dnsresolvd <port_number>

{application, dnsresolvd, [
    {description,
        "A daemon that performs DNS lookups for the given hostname passed in an HTTP request."
    },
    {applications, [
        cowboy
    ]},
    {mod, {
        'Elixir.DnsResolvd', [
        8765                   % <== <port_number>
    ]}}
]}.

% vim:set nu et ts=4 sw=4:
