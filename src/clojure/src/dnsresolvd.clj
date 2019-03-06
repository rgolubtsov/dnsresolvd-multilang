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
    "Starts up the daemon."
    [args]

    (println args)

;   TODO: Implement the daemon startup function.
)

; vim:set nu et ts=4 sw=4:
