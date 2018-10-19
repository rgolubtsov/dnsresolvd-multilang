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

#|
 | @param args A list containing the server port number to listen on
 |             as the first element.
 |
 | @returns The tuple containing the PID of the top supervisor process
 |          and the state of the running application (or an empty list).
 |#
(defun start (_ args)
    "Starts up the daemon.
     It has to be the application module callback, but used directly
     from the startup script of the daemon."

    (let (((tuple port-number daemon-name log) args))

    (let (((tuple 'ok _) (: application ensure_all_started 'cowboy))))

    (let ((dispatch (: cowboy_router compile (list
        (tuple '_ (list
            (tuple "/" 'reqhandler ())
        ))
    ))))

    ; Starting up the plain HTTP listener on <port_number>.
    (let ((ret- (: cowboy start_clear 'http-listener (list (tuple
        'port port-number
    )) (map
        'env (map 'dispatch dispatch)
    )))))))

    ; Trapping exit signals, i.e. transforming them into {'EXIT'} message.
    (process_flag 'trap_exit 'true)

    ; Inspecting the daemon's --application-- process message queue
    ; for the incoming #('EXIT) message until the message received.
    (receive
        (() (tuple 'EXIT '_ '_))
    )
)

(defun stop (_)
    "Does nothing. Required to satisfy the --application-- behaviour
                   callback module design only."

    'ok
)

; vim:set nu et ts=4 sw=4:
