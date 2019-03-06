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

(ns dnsresolvh)

; Helper constants.
(defmacro EXIT-FAILURE     []    1) ;    Failing exit status.
(defmacro EXIT-SUCCESS     []    0) ; Successful exit status.
(defmacro EMPTY-STRING     []   "")
(defmacro COLON-SPACE-SEP  [] ": ")
(defmacro COMMA-SPACE-SEP  [] ", ")
(defmacro NEW-LINE         [] "\n")
(defmacro ONE-SPACE-STRING []  " ")
(defmacro PRINT-BANNER-OPT [] "-V")

; TODO: Implement further helper stuff.

; vim:set nu et ts=4 sw=4:
