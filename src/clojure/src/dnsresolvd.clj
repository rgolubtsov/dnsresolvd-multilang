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

    (:import [java.net InetAddress ])
    (:import [java.net Inet4Address])
    (:import [java.net Inet6Address])
)

;##
; Performs DNS lookup action for the given hostname,
; i.e. (in this case) IP address retrieval by hostname.
;
; Args:
;     hostname: The effective hostname to look up for.
;
; Returns:
;     The list containing IP address of the analyzing host/service
;     and corresponding IP version (family) used to look up in DNS:
;     "4" for IPv4-only hosts, "6" for IPv6-capable hosts.
;
(defn dns-lookup [hostname]
    ; Trying to get an A record (IPv4) for the host or its AAAA record (IPv6).
    (try
        (let [hostent (InetAddress/getByName hostname)]

        (list
            (.getHostAddress hostent)
            (cond
                (instance? Inet4Address hostent) 4
                (instance? Inet6Address hostent) 6
            )
        ))
    (catch
        java.net.UnknownHostException e (list (AUX/ERR-PREFIX) nil)
    ))
)

(defn reqhandler
    "The request handler callback.
    Gets called when a new incoming HTTP request is received.

    Args:
        req: The incoming HTTP request object.

    Returns:
        TODO: TBD.
    " [req]

    (let [mtd (get req :request-method)]

    ; -------------------------------------------------------------------------
    ; --- Parsing and validating request params - Begin -----------------------
    ; -------------------------------------------------------------------------
    (let [params- (cond
        (= mtd :get )
            (let [params0 (get req :query-string)]
            (if (nil? params0) (AUX/EMPTY-STRING)        params0))
        (= mtd :post)
            (let [params0 (get req :body        )]
            (if (nil? params0) (AUX/EMPTY-STRING) (slurp params0)))
        :else
            (AUX/EMPTY-STRING)
    )]

    (let [params (clojure.walk/keywordize-keys
        (try
            (apply hash-map (clojure.string/split params- (AUX/PARAMS-SEPS)))
        (catch
            IllegalArgumentException e {
                :h (AUX/DEF-HOSTNAME)
                :f (AUX/PRM-FMT-JSON)
            }
        ))
    )]

    (let [hostname
        (let [hostname0 (get params :h)]
        (if (nil? hostname0) (AUX/DEF-HOSTNAME)
                             hostname0                       ))]

    (let [fmt-
        (let [fmt0      (get params :f)]
        (if (nil? fmt0     ) (AUX/PRM-FMT-JSON)
                             (clojure.string/lower-case fmt0)))]

    (let [fmt
        (if (not (some #{fmt-} (list
            (AUX/PRM-FMT-HTML)
            (AUX/PRM-FMT-JSON)
        ))) (AUX/PRM-FMT-JSON) fmt-)]
    ; -------------------------------------------------------------------------
    ; --- Parsing and validating request params - End -------------------------
    ; -------------------------------------------------------------------------

    ; Performing DNS lookup for the given hostname.
    (let [addr-ver (dns-lookup hostname)]

    (let [addr (nth addr-ver 0)]
    (let [ver  (nth addr-ver 1)]

    ; Returning HTTP status code, response headers, and a body of the response.
    {
        :status  (AUX/RSC-HTTP-200-OK)
        :headers (AUX/add-response-headers fmt)
        :body    (str addr (AUX/ONE-SPACE-STRING) ver)
    }
    )))))))))

    ; TODO: Implement the rest of the request handler callback.
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
