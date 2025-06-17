# DNS Resolver Daemon (dnsresolvd)

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request,
<br />with the focus on its implementation using various programming languages**

The following implementations are on the workbench:

* :small_blue_diamond: **C ([GNU libmicrohttpd](https://gnu.org/software/libmicrohttpd "GNU libmicrohttpd"))**: `src/c/libmicrohttpd/`
* :small_blue_diamond: **C ([GNOME libsoup](https://developer.gnome.org/libsoup "GNOME libsoup"))**: `src/c/libsoup/`
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
* :small_blue_diamond: **Java ([Eclipse Vert.x](http://vertx.io "Eclipse Vert.x"))**: `src/java/`
* :small_blue_diamond: **Bash ([Netcat](http://nc110.sourceforge.net "Netcat: the TCP/IP swiss army"))**: `src/bash/`
* :small_blue_diamond: **Go ([net/http](https://golang.org/pkg/net/http/ "Package http"))**: `src/go/`

## Table of Contents

* **Building** (The main prerequisite for building the daemon is the **GNU Make** utility, where applicable.)
  * [C (GNU libmicrohttpd)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/c/libmicrohttpd#building)
  * [C (GNOME libsoup)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/c/libsoup#building)
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
  * [Java (Eclipse Vert.x)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/java#building)
  * [Bash (Netcat)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/bash#building)
  * [Go (net/http)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/go#building)
* **Running** (Examples of making HTTP requests against the running daemon are based on using the `curl` utility.)
  * [C (GNU libmicrohttpd)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/c/libmicrohttpd#running)
  * [C (GNOME libsoup)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/c/libsoup#running)
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
  * [Java (Eclipse Vert.x)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/java#running)
  * [Bash (Netcat)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/bash#running)
  * [Go (net/http)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/go#running)

---

:floppy_disk:
