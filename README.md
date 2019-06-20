# DNS Resolver Daemon (dnsresolvd)

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request,
<br />with the focus on its implementation using various programming languages**

The following implementations are on the workbench (:small_blue_diamond: &ndash; complete, :small_orange_diamond: &ndash; planned/postponed, :cd: &ndash; in progress):

* :small_blue_diamond: **C ([GNU libmicrohttpd](https://gnu.org/software/libmicrohttpd "GNU libmicrohttpd"))**: `src/c/`
* :small_blue_diamond: **JavaScript ([Node.js](https://nodejs.org "Node.js"))**: `src/js/`
* :small_blue_diamond: **Lua ([Luvit](https://luvit.io "Luvit"))**: `src/lua/`
* :small_blue_diamond: **Perl 5 ([Mojolicious](http://mojolicious.org "Mojolicious"))**: `src/perl/`
* :small_blue_diamond: **Python 3 ([Twisted](http://twistedmatrix.com "Twisted"))**: `src/python/`
* :small_blue_diamond: **Vala ([GNOME libsoup](https://valadoc.org/libsoup-2.4/index.html "GNOME libsoup"))**: `src/vala/`
* :small_blue_diamond: **Genie ([GNOME libsoup](https://valadoc.org/libsoup-2.4/index.html "GNOME libsoup"))**: `src/genie/`
* :small_blue_diamond: **Elixir ([Cowboy](https://ninenines.eu "Cowboy"))**: `src/elixir/`
* :small_blue_diamond: **Erlang ([Cowboy](https://ninenines.eu "Cowboy"))**: `src/erlang/`
* :small_blue_diamond: **LFE ([Cowboy](https://ninenines.eu "Cowboy"))**: `src/lfe/`
* :small_blue_diamond: **Clojure ([HTTP Kit](http://http-kit.org "HTTP Kit"))**: `src/clojure/`
* :small_orange_diamond: **Java ([Vertosphere](http://atmosphere.github.io/atmosphere-vertx "Vertosphere"))**: `src/java/`

## Table of Contents

* **Building** (The main prerequisite for building the daemon is the **GNU Make** utility, where applicable.)
  * [C (GNU libmicrohttpd)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/c#building)
  * [JavaScript (Node.js)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/js#building)
  * [Lua (Luvit)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/lua#building)
  * [Perl 5 (Mojolicious)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/perl#building)
  * [Python 3 (Twisted)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/python#building)
  * [Vala (GNOME libsoup)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/vala#building)
  * [Genie (GNOME libsoup)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/genie#building)
  * [Elixir (Cowboy)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/elixir#building)
  * [Erlang (Cowboy)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/erlang#building)
  * [LFE (Cowboy)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/lfe#building)
  * [Clojure (HTTP Kit)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/clojure#building)
  * [Java (Vertosphere)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/java#building)
* **Running** (Examples of making HTTP requests against the running daemon are based on using the `curl` utility.)
  * [C (GNU libmicrohttpd)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/c#running)
  * [JavaScript (Node.js)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/js#running)
  * [Lua (Luvit)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/lua#running)
  * [Perl 5 (Mojolicious)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/perl#running)
  * [Python 3 (Twisted)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/python#running)
  * [Vala (GNOME libsoup)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/vala#running)
  * [Genie (GNOME libsoup)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/genie#running)
  * [Elixir (Cowboy)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/elixir#running)
  * [Erlang (Cowboy)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/erlang#running)
  * [LFE (Cowboy)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/lfe#running)
  * [Clojure (HTTP Kit)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/clojure#running)
  * [Java (Vertosphere)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/java#running)

---

**TODO:** Provide a brief description of the project and other related sections (overview, design, etc.) just like in [this readme doc](https://github.com/rgolubtsov/virtblkiosim/blob/master/README.md "VIRTual BLocK IO SIMulating (virtblkiosim)") or whatsoever. :cd:
