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
    ))))

    ; Handling errors during start up of the listener.
    (cond
        ((=:= (element 1 ret-) 'error)
            (let ((ret0 (macroexpand '(: AUX EXIT-FAILURE))))

            (cond
                ((=:= (element 2 ret-) 'eaddrinuse)
                    (: io put_chars 'standard_error
                (++ daemon-name (macroexpand '(: AUX ERR-CANNOT-START-SERVER))
                                (macroexpand '(: AUX ERR-SRV-PORT-IS-IN-USE ))
                                (macroexpand '(: AUX NEW-LINE))
                                (macroexpand '(: AUX NEW-LINE))))

                    (: syslog log log 'err
                (++ daemon-name (macroexpand '(: AUX ERR-CANNOT-START-SERVER))
                                (macroexpand '(: AUX ERR-SRV-PORT-IS-IN-USE ))
                                (macroexpand '(: AUX NEW-LINE))))
                ) ('true
                    (: io put_chars 'standard_error
                (++ daemon-name (macroexpand '(: AUX ERR-CANNOT-START-SERVER))
                                (macroexpand '(: AUX ERR-SRV-UNKNOWN-REASON ))
                                (macroexpand '(: AUX NEW-LINE))
                                (macroexpand '(: AUX NEW-LINE))))

                    (: syslog log log 'err
                (++ daemon-name (macroexpand '(: AUX ERR-CANNOT-START-SERVER))
                                (macroexpand '(: AUX ERR-SRV-UNKNOWN-REASON ))
                                (macroexpand '(: AUX NEW-LINE))))
                )
            )

            (: AUX cleanups-fixate log)

            (halt ret0)
            )
        )
    )))

    (: io put_chars (++
  (macroexpand '(: AUX MSG-SERVER-STARTED-1))
                (integer_to_list port-number) (macroexpand '(: AUX NEW-LINE))
  (macroexpand '(: AUX MSG-SERVER-STARTED-2)) (macroexpand '(: AUX NEW-LINE))))

    (: syslog log log 'info (++
  (macroexpand '(: AUX MSG-SERVER-STARTED-1))
                (integer_to_list port-number) (macroexpand '(: AUX NEW-LINE))
  (macroexpand '(: AUX MSG-SERVER-STARTED-2))))
    )

    ; Starting up the daemon's provided --supervisor-- (optional).
    (: dnsresolvs start_link)

    ; Trapping exit signals, i.e. transforming them into {'EXIT'} message.
    (process_flag 'trap_exit 'true)

    ; Inspecting the daemon's --application-- process message queue
    ; for the incoming #('EXIT) message until the message received.
    (receive
        (() (tuple 'EXIT '_ '_))
    )
)

; -----------------------------------------------------------------------------

(defmodule reqhandler
    "The default HTTP request handler."

    (export (init 2))
)

#|
 | @param req   The incoming HTTP request object.
 | @param state The initial state of the HTTP handler.
 |
 | @returns The tuple containing the HTTP response to be rendered
 |          and a new state of the HTTP handler.
 |#
(defun init (req state)
    "The request handler <code>init/2</code> callback.
     Gets called when a new incoming HTTP request is received."

    (let ((mtd (: cowboy_req method req)))

    ; -------------------------------------------------------------------------
    ; --- Parsing and validating request params - Begin -----------------------
    ; -------------------------------------------------------------------------
    (let (((tuple 'ok params req0) (cond
        (  (=:= mtd (macroexpand '(: AUX MTD-HTTP-GET )))
            (tuple 'ok (: cowboy_req parse_qs req) req)
        ) ((=:= mtd (macroexpand '(: AUX MTD-HTTP-POST)))
            (: cowboy_req read_urlencoded_body     req)
        ) ('true
            (tuple 'ok (                         ) req)
        )
    )))

    (let ((hostname- (lc ((<-(tuple k v) params)(=:= k #"h")) v))) ; <---+
    ;         +----GET----+-----+-----+                  ^               |
    ;         |     |     |     |     |                  |               |
    ;         |     |     |     |     |       +----------+ +-------------+-+
    ;         v     v     v     v     v       |            |             | |
    ; $ curl 'http://localhost:<port_number>/?h=<hostname>&f=<fmt>'      | |
    ; $                                                                  | |
    ; $ curl -d 'h=<hostname>&f=<fmt>' http://localhost:<port_number>    | |
    ;         ^  |            |                                          | |
    ;         |  +------------+------------------------------------------+ |
    ;         |               |                                            |
    ; POST----+               +--------------------------+                 |
    ;                                                    |                 |
    ;                                                    v                 |
    (let ((fmt-      (lc ((<-(tuple k v) params)(=:= k #"f")) v))) ; <-----+

    (: io put_chars hostname-) (: io nl)
    (: io put_chars fmt-     ) (: io nl)
    ))))
    ; -------------------------------------------------------------------------
    ; --- Parsing and validating request params - End -------------------------
    ; -------------------------------------------------------------------------

    (tuple 'ok
        req
        state ; <== The state of the handler doesn't need to be changed.
    )
)

; -----------------------------------------------------------------------------

(defmodule dnsresolvs
    "The main --supervisor-- module of the daemon."

    (behaviour supervisor)

    (export (start_link 0)
            (init       1))
)

;; @returns The PID of the <code>dnsresolvs</code> supervisor process.
(defun start_link ()
    "Supervisor startup helper.
     Used to directly start up the <code>dnsresolvs</code> supervisor process."

    (: supervisor start_link (MODULE) ())
)

;; @returns The tuple containing supervisor flags
;;          and child processes' specifications.
(defun init (_)
    "Supervisor <code>init/1</code> callback.
     Gets called when the <code>dnsresolvs</code> supervisor
     is about to be started up."

    (let ((child-procs ())) ; <== No child processes do we need.

    (tuple 'ok (tuple
        (map 'strategy 'one_for_one)
        child-procs
    )))
)

; vim:set nu et ts=4 sw=4:
