;
; src/clojure/src/dnsresolvd.clj
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

(ns dnsresolvd
    "The main module of the daemon."

    (:require
        [dnsresolvh         :as             AUX]
        [org.httpkit.server :refer [run-server]]
    )
)

(defn reqhandler
    "The request handler callback.
    Gets called when a new incoming HTTP request is received.

    Args:
        req: The incoming HTTP request object.

    Returns:
        TODO: TBD.
    " [req]

    (println req) ; TODO: Implement the request handler callback.
)

(defn startup
    "Starts up the daemon.

    Args:
        args: A list containing the server port number to listen on
              as the first element.

    Returns:
        The exit code indicating the daemon overall execution status.
    " [args]

    (let [port-number (nth args 0)]
    (let [daemon-name (nth args 1)]
    (let [log         (nth args 2)]

    ; Trying to start up the HTTP Kit server on <port-number>.
    (try
        (run-server reqhandler {
            :port port-number
        })

        (println   (str (AUX/MSG-SERVER-STARTED-1) port-number (AUX/NEW-LINE)
                        (AUX/MSG-SERVER-STARTED-2)))

        (.info log (str (AUX/MSG-SERVER-STARTED-1) port-number (AUX/NEW-LINE)
                        (AUX/MSG-SERVER-STARTED-2)))

        (.join (Thread/currentThread)) ; <== Important !
    (catch java.net.BindException e
        (binding [*out* *err*]
        (println    (str daemon-name (AUX/ERR-CANNOT-START-SERVER)
                                     (AUX/ERR-SRV-PORT-IS-IN-USE )
                                     (AUX/NEW-LINE)))

        (.error log (str daemon-name (AUX/ERR-CANNOT-START-SERVER)
                                     (AUX/ERR-SRV-PORT-IS-IN-USE )
                                     (AUX/NEW-LINE)))
        )
    ) (catch Exception e
        (binding [*out* *err*]
        (println    (str daemon-name (AUX/ERR-CANNOT-START-SERVER)
                                     (AUX/ERR-SRV-UNKNOWN-REASON )
                                     (AUX/NEW-LINE)))

        (.error log (str daemon-name (AUX/ERR-CANNOT-START-SERVER)
                                     (AUX/ERR-SRV-UNKNOWN-REASON )
                                     (AUX/NEW-LINE)))
        )
    ) (finally
        (let [ret0 (AUX/EXIT-FAILURE)]

        (AUX/cleanups-fixate log)

        (System/exit ret0)
        )
    )))))
)

; vim:set nu et ts=4 sw=4:
