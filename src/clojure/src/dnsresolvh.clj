;
; src/clojure/src/dnsresolvh.clj
; =============================================================================
; DNS Resolver Daemon (dnsresolvd). Version 0.1
; =============================================================================
; A daemon that performs DNS lookups for the given hostname
; passed in an HTTP request, with the focus on its implementation
; using various programming languages. (HTTP Kit-boosted impl.)
; =============================================================================
; Copyright (C) 2017-2019 Radislav (Radicchio) Golubtsov
;
; (See the LICENSE file at the top of the source tree.)
;

(ns dnsresolvh
    "The helper module for the daemon."
)

; Helper constants.
(defmacro EXIT-FAILURE     []      1) ;    Failing exit status.
(defmacro EXIT-SUCCESS     []      0) ; Successful exit status.
(defmacro EMPTY-STRING     []     "")
(defmacro COLON-SPACE-SEP  []   ": ")
(defmacro COMMA-SPACE-SEP  []   ", ")
(defmacro NEW-LINE         []   "\n")
(defmacro ONE-SPACE-STRING []    " ")
(defmacro PRINT-BANNER-OPT []   "-V")
(defmacro DIGITS           [] #"\d+")

; Common error messages.
(defmacro ERR-PORT-MUST-BE-POSITIVE-INT [](str ": <port_number> must be "
                                               "a positive integer value, "
                                               "in the range 1024-49151."))
(defmacro ERR-CANNOT-START-SERVER       [] ": FATAL: Cannot start server ")
(defmacro ERR-SRV-UNKNOWN-REASON        [](str "for an unknown reason. "
                                               "Exiting..."              ))
(defmacro ERR-SRV-PORT-IS-IN-USE        [](str "due to the port requested "
                                               "is in use. Exiting..."   ))

; Print this error message when there are no any args passed.
(defmacro ERR-MUST-BE-ONE-TWO-ARGS-1 [](str ": There must be one or two args "
                                            "passed: "))
(defmacro ERR-MUST-BE-ONE-TWO-ARGS-2 [] " args found"  )

; Print this usage info just after any inappropriate input.
(defmacro MSG-USAGE-TEMPLATE-1 [] "Usage: "            )
(defmacro MSG-USAGE-TEMPLATE-2 [] " <port_number> [-V]")

;; Constant: The minimum port number allowed.
(defmacro MIN-PORT []  1024)

;; Constant: The maximum port number allowed.
(defmacro MAX-PORT [] 49151)

; Common notification messages.
(defmacro MSG-SERVER-STARTED-1 [] "Server started on port "        )
(defmacro MSG-SERVER-STARTED-2 [] "=== Hit Ctrl+C to terminate it.")

; Daemon name, version, and copyright banners.
(defmacro DMN-NAME        [] "DNS Resolver Daemon (dnsresolvd)"       )
(defmacro DMN-DESCRIPTION [](str "Performs DNS lookups for the given "
                                 "hostname passed in an HTTP request"))
(defmacro DMN-VERSION-S-- [] "Version"                                )
(defmacro DMN-VERSION     [] "0.1"                                    )
(defmacro DMN-COPYRIGHT-- [] "Copyright (C) 2017-2019"                )
(defmacro DMN-AUTHOR      [] "Radislav Golubtsov <ragolubtsov@my.com>")

(defn cleanups-fixate
    "Helper function. Makes final buffer cleanups, closes streams, etc." [log]

    ; Closing the system logger.
    ; --- Calling <syslog.h> closelog(); ---
    (.shutdown log)
)

(defn separator-draw
    "Helper function. Draws a horizontal separator banner." [banner-text]

    (let [i (count banner-text)]

    (print (apply str (for [_ (range i)] "=")))) (newline)
)

; vim:set nu et ts=4 sw=4:
