;
; src/lfe/lib/dnsresolvh.lfe
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

(defmodule AUX
    "The helper module for the daemon."

    (export-macro EXIT-FAILURE
                  EXIT-SUCCESS
                  EMPTY-STRING
                  COMMA-SPACE-SEP
                  NEW-LINE
                  ONE-SPACE-STRING
                  PRINT-BANNER-OPT)
; -----------------------------------------------------------------------------
    (export-macro ERR-PORT-MUST-BE-POSITIVE-INT
                  ERR-CANNOT-START-SERVER
                  ERR-SRV-UNKNOWN-REASON
                  ERR-SRV-PORT-IS-IN-USE)
; -----------------------------------------------------------------------------
    (export-macro DMN-NAME
                  DMN-DESCRIPTION
                  DMN-VERSION-S--
                  DMN-VERSION
                  DMN-COPYRIGHT--
                  DMN-AUTHOR)
; -----------------------------------------------------------------------------
    (export-macro ERR-MUST-BE-ONE-TWO-ARGS-1
                  ERR-MUST-BE-ONE-TWO-ARGS-2)
; -----------------------------------------------------------------------------
    (export-macro MSG-USAGE-TEMPLATE-1
                  MSG-USAGE-TEMPLATE-2)
; -----------------------------------------------------------------------------
    (export-macro MIN-PORT
                  MAX-PORT)
; -----------------------------------------------------------------------------
    (export-macro MSG-SERVER-STARTED-1
                  MSG-SERVER-STARTED-2)
; -----------------------------------------------------------------------------
    (export (cleanups-fixate 1)
            (separator-draw  1))
)

; Helper constants.
(defmacro EXIT-FAILURE     ()    1) ;    Failing exit status.
(defmacro EXIT-SUCCESS     ()    0) ; Successful exit status.
(defmacro EMPTY-STRING     ()   "")
(defmacro COLON-SPACE-SEP  () ": ")
(defmacro COMMA-SPACE-SEP  () ", ")
(defmacro NEW-LINE         () "\n")
(defmacro ONE-SPACE-STRING ()  " ")
(defmacro PRINT-BANNER-OPT () "-V")

; JSON entities :-).
(defmacro CB1 ()   "{\"")
(defmacro CB2 ()   "\"}")
(defmacro DQ1 () "\":\"")
(defmacro DQ2 () "\",\"")

; Common error messages.
(defmacro ERR-PREFIX                    () "error"                        )
(defmacro ERR-PORT-MUST-BE-POSITIVE-INT () (++ ": <port_number> must be "
                                               "a positive integer value, "
                                               "in the range 1024-49151."))
(defmacro ERR-CANNOT-START-SERVER       () ": FATAL: Cannot start server ")
(defmacro ERR-SRV-UNKNOWN-REASON        () (++ "for an unknown reason. "
                                               "Exiting..."              ))
(defmacro ERR-SRV-PORT-IS-IN-USE        () (++ "due to the port requested "
                                               "is in use. Exiting..."   ))
(defmacro ERR-COULD-NOT-LOOKUP          () "could not lookup hostname"    )

; Print this error message when there are no any args passed.
(defmacro ERR-MUST-BE-ONE-TWO-ARGS-1 () (++ ": There must be one or two args "
                                            "passed: "))
(defmacro ERR-MUST-BE-ONE-TWO-ARGS-2 () " args found"  )

; Print this usage info just after any inappropriate input.
(defmacro MSG-USAGE-TEMPLATE-1 () "Usage: "            )
(defmacro MSG-USAGE-TEMPLATE-2 () " <port_number> [-V]")

;; Constant: The minimum port number allowed.
(defmacro MIN-PORT ()  1024)

;; Constant: The maximum port number allowed.
(defmacro MAX-PORT () 49151)

; Common notification messages.
(defmacro MSG-SERVER-STARTED-1 () "Server started on port "        )
(defmacro MSG-SERVER-STARTED-2 () "=== Hit Ctrl+C to terminate it.")

; HTTP request methods and params.
(defmacro MTD-HTTP-GET  () <<"GET">> )
(defmacro MTD-HTTP-POST () <<"POST">>)
(defmacro PRM-FMT-HTML  ()   "html"  )
(defmacro PRM-FMT-JSON  ()   "json"  )

; HTTP response headers and status codes.
(defmacro HDR-CONTENT-TYPE-N      () "content-type"                 )
(defmacro HDR-CONTENT-TYPE-V-HTML () "text/html; charset=UTF-8"     )
(defmacro HDR-CONTENT-TYPE-V-JSON () "application/json"             )
(defmacro HDR-CACHE-CONTROL-N     () "cache-control"                )
(defmacro HDR-CACHE-CONTROL-V     () (++ "no-cache, no-store, "
                                         "must-revalidate"         ))
(defmacro HDR-EXPIRES-N           () "expires"                      )
(defmacro HDR-EXPIRES-V           () "Thu, 01 Dec 1994 16:00:00 GMT")
(defmacro HDR-PRAGMA-N            () "pragma"                       )
(defmacro HDR-PRAGMA-V            () "no-cache"                     )
(defmacro RSC-HTTP-200-OK         () 200                            )

; Response data names.
(defmacro DAT-HOSTNAME-N () "hostname")
(defmacro DAT-ADDRESS-N  () "address" )
(defmacro DAT-VERSION-N  () "version" )
(defmacro DAT-VERSION-V  () "IPv"     )

; Daemon name, version, and copyright banners.
(defmacro DMN-NAME        () "DNS Resolver Daemon (dnsresolvd)"       )
(defmacro DMN-DESCRIPTION () (++ "Performs DNS lookups for the given "
                                 "hostname passed in an HTTP request"))
(defmacro DMN-VERSION-S-- () "Version"                                )
(defmacro DMN-VERSION     () "0.1"                                    )
(defmacro DMN-COPYRIGHT-- () "Copyright (C) 2017-2018"                )
(defmacro DMN-AUTHOR      () "Radislav Golubtsov <ragolubtsov@my.com>")

;; Constant: The default hostname to look up for.
(defmacro DEF-HOSTNAME () "openbsd.org")

(defun cleanups-fixate (log)
    "Helper function. Makes final buffer cleanups, closes streams, etc."

    ; Closing the system logger.
    (cond
        ((=/= log ())
            (: syslog close log)
            (: syslog stop     )
        )
    )
)

(defun separator-draw (banner-text)
    "Helper function. Draws a horizontal separator banner."

    (let ((i (length banner-text)))

    (lc  ((<- _ (: lists seq 1 i))) (: io put_chars "="))) (: io nl)
)

; vim:set nu et ts=4 sw=4:
