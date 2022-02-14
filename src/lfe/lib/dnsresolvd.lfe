;
; src/lfe/lib/dnsresolvd.lfe
; =============================================================================
; DNS Resolver Daemon (dnsresolvd). Version 0.9.9
; =============================================================================
; A daemon that performs DNS lookups for the given hostname
; passed in an HTTP request, with the focus on its implementation
; using various programming languages. (Cowboy-boosted impl.)
; =============================================================================
; Copyright (C) 2017-2022 Radislav (Radicchio) Golubtsov
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

    ; Starting up the plain HTTP listener on <port-number>.
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
    ; $ curl 'http://localhost:<port-number>/?h=<hostname>&f=<fmt>'      | |
    ; $                                                                  | |
    ; $ curl -d 'h=<hostname>&f=<fmt>' http://localhost:<port-number>    | |
    ;         ^  |            |                                          | |
    ;         |  +------------+------------------------------------------+ |
    ;         |               |                                            |
    ; POST----+               +--------------------------+                 |
    ;                                                    |                 |
    ;                                                    v                 |
    (let ((fmt-      (lc ((<-(tuple k v) params)(=:= k #"f")) v))) ; <-----+

    (let ((hostname0 (: lists filter (lambda (v) (=/= v ())) hostname-)))
    (let ((hostname0-len (length hostname0)))
    (let ((hostname1 (cond
        ((> hostname0-len 0)
            (: lists nth hostname0-len hostname0)
        ) ('true
            (macroexpand '(: AUX EMPTY-STRING))
        )
    )))

    (let ((fmt0      (: lists filter (lambda (v) (=/= v ())) fmt-     )))
    (let ((fmt0-len      (length fmt0     )))
    (let ((fmt1      (cond
        ((> fmt0-len      0)
            (: lists nth fmt0-len      fmt0     )
        ) ('true
            (macroexpand '(: AUX EMPTY-STRING))
        )
    )))

    (let ((hostname  (cond
        (  (=:= hostname1                                   ())
            (macroexpand '(: AUX DEF-HOSTNAME))
        ) ((=:= hostname1                                'true)
            (macroexpand '(: AUX DEF-HOSTNAME))
        ) ((=:= hostname1 (macroexpand '(: AUX EMPTY-STRING#)))
            (macroexpand '(: AUX DEF-HOSTNAME))
        ) ('true
            (binary_to_list hostname1)
        )
    )))

    (let ((fmt2      (cond
        (  (=:= fmt1                                        ())
            (macroexpand '(: AUX PRM-FMT-JSON))
        ) ((=:= fmt1                                     'true)
            (macroexpand '(: AUX PRM-FMT-JSON))
        ) ((=:= fmt1      (macroexpand '(: AUX EMPTY-STRING#)))
            (macroexpand '(: AUX PRM-FMT-JSON))
        ) ('true
            (: string to_lower (binary_to_list fmt1))
        )
    )))

    (let ((fmt2-     (: lists member fmt2 (list
        (macroexpand '(: AUX PRM-FMT-HTML))
        (macroexpand '(: AUX PRM-FMT-JSON))
    ))))

    (let ((fmt       (cond
        ((not fmt2-) (macroexpand '(: AUX PRM-FMT-JSON)))
        ('true fmt2)
    )))
    ; -------------------------------------------------------------------------
    ; --- Parsing and validating request params - End -------------------------
    ; -------------------------------------------------------------------------

    ; Performing DNS lookup for the given hostname.
    (let ((addr-ver (dns-lookup hostname)))

    (let ((addr (element 1 addr-ver)))
    (let ((ver  (element 2 addr-ver)))

    (let ((resp-buffer- (cond
        (  (=:= fmt (macroexpand '(: AUX PRM-FMT-HTML)))
            (++ "<!DOCTYPE html>"                                                         (macroexpand '(: AUX NEW-LINE))
"<html lang=\"en-US\" dir=\"ltr\">"                                                       (macroexpand '(: AUX NEW-LINE))
"<head>"                                                                                  (macroexpand '(: AUX NEW-LINE))
"<meta http-equiv=\"" (macroexpand '(: AUX HDR-CONTENT-TYPE-N     ))   "\"    content=\""
                      (macroexpand '(: AUX HDR-CONTENT-TYPE-V-HTML))   "\"           />"  (macroexpand '(: AUX NEW-LINE))
"<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"                            />"  (macroexpand '(: AUX NEW-LINE))
"<meta       name=\"viewport\"        content=\"width=device-width,initial-scale=1\" />"  (macroexpand '(: AUX NEW-LINE))
"<title>"             (macroexpand '(: AUX DMN-NAME               )) "</title>"           (macroexpand '(: AUX NEW-LINE))
"</head>"                                                                                 (macroexpand '(: AUX NEW-LINE))
"<body>"                                                                                  (macroexpand '(: AUX NEW-LINE))
"<div>"      hostname (macroexpand '(: AUX ONE-SPACE-STRING       )))
        ) ((=:= fmt (macroexpand '(: AUX PRM-FMT-JSON)))
            (++ (macroexpand '(: AUX CB1                          ))
                (macroexpand '(: AUX DAT-HOSTNAME-N               ))
                (macroexpand '(: AUX DQ1                          ))
                                 hostname
                (macroexpand '(: AUX DQ2                          )))
        )
    )))

    ; If lookup error occurred.
    (let ((resp-buffer0 (cond
        ((=:= addr (macroexpand '(: AUX ERR-PREFIX))) (cond
            (  (=:= fmt (macroexpand '(: AUX PRM-FMT-HTML)))
                (++ resp-buffer- (macroexpand '(: AUX ERR-PREFIX          ))
                                 (macroexpand '(: AUX COLON-SPACE-SEP     ))
                                 (macroexpand '(: AUX ERR-COULD-NOT-LOOKUP)))
            ) ((=:= fmt (macroexpand '(: AUX PRM-FMT-JSON)))
                (++ resp-buffer- (macroexpand '(: AUX ERR-PREFIX          ))
                                 (macroexpand '(: AUX DQ1                 ))
                                 (macroexpand '(: AUX ERR-COULD-NOT-LOOKUP)))
            )
        )) ('true (cond
            (  (=:= fmt (macroexpand '(: AUX PRM-FMT-HTML)))
                (++ resp-buffer-                  addr
                                 (macroexpand '(: AUX ONE-SPACE-STRING    ))
                                 (macroexpand '(: AUX DAT-VERSION-V       ))
                                 (integer_to_list ver                      ))
            ) ((=:= fmt (macroexpand '(: AUX PRM-FMT-JSON)))
                (++ resp-buffer- (macroexpand '(: AUX DAT-ADDRESS-N       ))
                                 (macroexpand '(: AUX DQ1                 ))
                                                  addr
                                 (macroexpand '(: AUX DQ2                 ))
                                 (macroexpand '(: AUX DAT-VERSION-N       ))
                                 (macroexpand '(: AUX DQ1                 ))
                                 (macroexpand '(: AUX DAT-VERSION-V       ))
                                 (integer_to_list ver                      ))
            )
        ))
    )))

    (let ((resp-buffer (cond
        (  (=:= fmt (macroexpand '(: AUX PRM-FMT-HTML)))
            (++ resp-buffer0 "</div>"  (macroexpand '(: AUX NEW-LINE))
                             "</body>" (macroexpand '(: AUX NEW-LINE))
                             "</html>" (macroexpand '(: AUX NEW-LINE)))
        ) ((=:= fmt (macroexpand '(: AUX PRM-FMT-JSON)))
            (++ resp-buffer0           (macroexpand '(: AUX CB2     )))
        )
    )))

    ; Adding headers to the response.
    (let ((req1 (: AUX add-response-headers fmt req0)))

    (let((req2(: cowboy_req set_resp_body resp-buffer                   req1)))
    (let((req-(: cowboy_req reply(macroexpand '(: AUX RSC-HTTP-200-OK)) req2)))

    (tuple 'ok
        req-
        state ; <== The state of THE HANDLER doesn't need to be changed.
                               )             )
                               )  ) )   ) )  )
                              ) )           ) )
                             )                 )
                             )                 )
                              )               )
                                )     )     )
                                   ) ) ) )
;##
; Performs DNS lookup action for the given hostname,
; i.e. (in this case) IP address retrieval by hostname.
;
; Args:
;     hostname: The effective hostname to look up for.
;
; Returns:
;     The tuple containing IP address of the analyzing host/service
;     and corresponding IP version (family) used to look up in DNS:
;     "4" for IPv4-only hosts, "6" for IPv6-capable hosts.
;
(defun dns-lookup (hostname)
    (let ((hostent4 (: inet gethostbyname hostname 'inet )))

    ; If the host doesn't have an A record (IPv4),
    ; trying to find its AAAA record (IPv6).
    (let ((hostent6 (if (=:= (element 1 hostent4) 'error)
                    (: inet gethostbyname hostname 'inet6)
    )))

    (cond
        (  (=:= (element 1 hostent4) 'ok)
            (tuple
                (: inet ntoa (hd (element 6 (element 2 hostent4))))
                4
            )
        ) ((=:= (element 1 hostent6) 'ok)
            (tuple
                (: inet ntoa (hd (element 6 (element 2 hostent6))))
                6
            )
        ) ('true
            (tuple (macroexpand '(: AUX ERR-PREFIX)) ())
        )
    )))
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
