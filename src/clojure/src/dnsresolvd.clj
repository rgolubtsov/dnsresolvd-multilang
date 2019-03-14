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

(ns dnsresolvd)

(defn startup
    "Starts up the daemon." [args]

    (let [port-number (nth args 0)]
    (let [daemon-name (nth args 1)]
    (let [log         (nth args 2)]

    (println   (str (AUX/MSG-SERVER-STARTED-1) port-number (AUX/NEW-LINE)
                    (AUX/MSG-SERVER-STARTED-2)))

    (.info log (str (AUX/MSG-SERVER-STARTED-1) port-number (AUX/NEW-LINE)
                    (AUX/MSG-SERVER-STARTED-2)))

    (println daemon-name)

;   TODO: Implement the rest of the daemon startup function.
    )))
)

; vim:set nu et ts=4 sw=4:
