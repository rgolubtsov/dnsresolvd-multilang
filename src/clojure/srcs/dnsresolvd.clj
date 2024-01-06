;
; src/clojure/src/dnsresolvd.clj
; =============================================================================
; DNS Resolver Daemon (dnsresolvd). Version 0.9.9
; =============================================================================
; A daemon that performs DNS lookups for the given hostname
; passed in an HTTP request, with the focus on its implementation
; using various programming languages. (HTTP Kit-boosted impl.)
; =============================================================================
; Copyright (C) 2017-2024 Radislav (Radicchio) Golubtsov
;
; (See the LICENSE file at the top of the source tree.)
;

(ns dnsresolvd
    "The main module of the daemon."

    (:require
        [dnsresolvh         :as AUX]
        [clojure.walk       :refer [
            keywordize-keys
        ]]
        [org.httpkit.server :refer [
            run-server
            with-channel
            send!
        ]]
        [clojure.data.json  :refer [
            write-str
        ]]
    )

    (:import [java.net InetAddress ])
    (:import [java.net Inet4Address])
    (:import [java.net Inet6Address])
)

(defn dns-lookup
    "Performs DNS lookup action for the given hostname,
    i.e. (in this case) IP address retrieval by hostname.

    Args:
        hostname: The effective hostname to look up for.

    Returns:
        The list containing IP address of the analyzed host/service
        and a corresponding IP version (family) used to look up in DNS:
        "4" for IPv4-only hosts, "6" for IPv6-capable hosts.
    " [hostname]

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
        The HTTP status code, response headers, and a body of the response.
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

    (let [params (keywordize-keys
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
        (let [hostname0 (get params :h)] ; <------+----------------------+
        (if (nil? hostname0) (AUX/DEF-HOSTNAME) ; |                      |
                             hostname0))]       ; |                      |
    ;         +----GET----+-----+-----+           |                      |
    ;         |     |     |     |     |           |                      |
    ;         |     |     |     |     |       +---+        +-------------+-+
    ;         v     v     v     v     v       |            |             | |
    ; $ curl 'http://localhost:<port-number>/?h=<hostname>&f=<fmt>'      | |
    ; $                                                                  | |
    ; $ curl -d 'h=<hostname>&f=<fmt>' http://localhost:<port-number>    | |
    ;         ^  |            |                                          | |
    ;         |  +------------+------------------------------------------+ |
    ;         |               |                                            |
    ; POST----+               +------+                                     |
    ;                                |                                     |
    (let [fmt- ;                     v                                     |
        (let [fmt0      (get params :f)] ; <-------------------------------+
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

    (let [resp-buffer- (cond
        (= fmt (AUX/PRM-FMT-HTML))
            (str "<!DOCTYPE html>"                                                        (AUX/NEW-LINE)
"<html lang=\"en-US\" dir=\"ltr\">"                                                       (AUX/NEW-LINE)
"<head>"                                                                                  (AUX/NEW-LINE)
"<meta http-equiv=\"" (AUX/HDR-CONTENT-TYPE-N     )                    "\"    content=\""
                      (AUX/HDR-CONTENT-TYPE-V-HTML)                    "\"           />"  (AUX/NEW-LINE)
"<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"                            />"  (AUX/NEW-LINE)
"<meta       name=\"viewport\"        content=\"width=device-width,initial-scale=1\" />"  (AUX/NEW-LINE)
"<title>"             (AUX/DMN-NAME               ) "</title>"                            (AUX/NEW-LINE)
"</head>"                                                                                 (AUX/NEW-LINE)
"<body>"                                                                                  (AUX/NEW-LINE)
"<div>"      hostname (AUX/ONE-SPACE-STRING       ))
        (= fmt (AUX/PRM-FMT-JSON))
            (hash-map (AUX/DAT-HOSTNAME-N         )
             hostname)
    )]

    ; If lookup error occurred.
    (let [resp-buffer0 (cond
        (= addr (AUX/ERR-PREFIX)) (cond
            (= fmt (AUX/PRM-FMT-HTML))
                (str   resp-buffer- (AUX/ERR-PREFIX          )
                                    (AUX/COLON-SPACE-SEP     )
                                    (AUX/ERR-COULD-NOT-LOOKUP))
            (= fmt (AUX/PRM-FMT-JSON))
                (assoc resp-buffer- (AUX/ERR-PREFIX          )
                                    (AUX/ERR-COULD-NOT-LOOKUP))
        ) :else (cond
            (= fmt (AUX/PRM-FMT-HTML))
                (str   resp-buffer-  addr
                                    (AUX/ONE-SPACE-STRING    )
                                    (AUX/DAT-VERSION-V       )
                                     ver                     )
            (= fmt (AUX/PRM-FMT-JSON))
                (assoc resp-buffer- (AUX/DAT-ADDRESS-N       )
                                     addr
                                    (AUX/DAT-VERSION-N       )
                                    (str (AUX/DAT-VERSION-V  )
                                     ver                     ))
        )
    )]

    (let [resp-buffer (cond
        (= fmt (AUX/PRM-FMT-HTML))
            (str resp-buffer0 "</div>"  (AUX/NEW-LINE)
                              "</body>" (AUX/NEW-LINE)
                              "</html>" (AUX/NEW-LINE))
        (= fmt (AUX/PRM-FMT-JSON))
            (write-str resp-buffer0)
    )]

    ; Returning the HTTP status code, response headers,
    ; and a body of the response.
    (with-channel req channel (send! channel { ; <== Async mode !
        :status  (AUX/RSC-HTTP-200-OK)
        :headers (AUX/add-response-headers fmt)
        :body    resp-buffer
    }            )
                 )
                 )
                 )
                 )
              )  )  )
               ) ) )
                )))
                 )

(defn startup
    "Starts up the daemon.

    Args:
        args: A list containing the server port number to listen on
              as the first element.

    Returns:
        The exit code indicating the daemon overall termination status.
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
