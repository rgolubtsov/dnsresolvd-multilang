;
; src/lfe/lib/dnsresolvd.lfe
; =============================================================================
; DNS Resolver Daemon (dnsresolvd). Version 0.1
; =============================================================================
; A daemon that performs DNS lookups for the given hostname
; passed in an HTTP request, with the focus on its implementation
; using various programming languages. (Cowboy-boosted impl.)
; =============================================================================
; Copyright (C) 2017-2018 Radislav (Radicchio) Golubtsov
;
; (See the LICENSE file at the top of the source tree.)
;

(defmodule dnsresolvd
    "The main --application-- module of the daemon."

    (behaviour application)

    (export (start 2))
;           (stop  1))
)

(defun start (_ args)
    "Starts up the daemon.
     It has to be the application module callback, but used directly
     from the startup script of the daemon."

    'ok
)

(defun stop (_)
    "Does nothing. Required to satisfy the --application-- behaviour
                   callback module design only."

    'ok
)

; vim:set nu et ts=4 sw=4:
