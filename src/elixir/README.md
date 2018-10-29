# DNS Resolver Daemon written in Elixir

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [Cowboy](https://ninenines.eu "Cowboy") HTTP server for Erlang/OTP)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.3](#building-under-openbsdamd64-63)
* **[Running](#running)**

## Building

This daemon implementation is known to be built and run successfully on OpenBSD, ~~Ubuntu Server, and Arch Linux~~ operating systems. So let's describe each build process sequentially.

### Building under OpenBSD/amd64 6.3

Install the necessary dependencies (`elixir`, `rebar19`, `syslog`, `cowboy`). Note that the `erlang` package will be installed automatically as a dependency to the `elixir` package:

```
$ sudo pkg_add -vvvvv elixir rebar19
$
$ elixir -v
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:2:2] [async-threads:10] [kernel-poll:false]

Elixir 1.6.4 (compiled with OTP 19)
```

The `syslog` and `cowboy` packages are essentially **Erlang** packages and they can be installed using normal **Elixir**'ish way &ndash; via its standard Mix build tool. But since the Mix utility doesn't used at all, let's build and install these packages via **rebar**. For that it needs to create a &quot;mock&quot; project and install all the necessary dependencies. The following compound one-liner script will actually do this job.

(Note that the `cowboy` package depends on the others: `cowlib` and `ranch`. They will be built and installed automatically too.)

```
$ cd src/elixir
```

```
$ export       E_LIB_ID=erlang_modules       && \
  mkdir   -p ${E_LIB_ID}                     && \
  cd         ${E_LIB_ID}                     && \
  rebar19 -f create-lib libid=${E_LIB_ID}    && \
  echo       '% ==============
% ./rebar.config
% ==============
{deps, [
    {syslog, {
        git, "git://github.com/Vagabond/erlang-syslog.git", {branch, "master"}
    }},
    {cowboy, {
        git, "git://github.com/ninenines/cowboy.git",       {branch, "master"}
    }}
]}.
% vim:set nu et ts=4 sw=4:' > ./rebar.config && \
  unset      E_LIB_ID                        && \
  rebar19    g-d                             && \
  rebar19    c-d                             && \
  rebar19    co                              && \
  cd         - # <== Just hit Enter here and wait for a while.))
==> erlang_modules (create-lib)
Writing src/erlang_modules.app.src
Writing src/erlang_modules.erl
==> erlang_modules (get-deps)
Pulling syslog from {git,"git://github.com/Vagabond/erlang-syslog.git",
                         {branch,"master"}}
Cloning into 'syslog'...
Pulling cowboy from {git,"git://github.com/ninenines/cowboy.git",
                         {branch,"master"}}
Cloning into 'cowboy'...
==> syslog (get-deps)
==> cowboy (get-deps)
Pulling cowlib from {git,"https://github.com/ninenines/cowlib","2.6.0"}
Cloning into 'cowlib'...
Pulling ranch from {git,"https://github.com/ninenines/ranch","1.6.2"}
Cloning into 'ranch'...
==> cowlib (get-deps)
==> ranch (get-deps)
==> syslog (check-deps)
==> cowlib (check-deps)
==> ranch (check-deps)
==> cowboy (check-deps)
==> erlang_modules (check-deps)
==> syslog (compile)
...
==> cowlib (compile)
...
==> ranch (compile)
...
==> cowboy (compile)
...
==> erlang_modules (compile)
Compiled src/erlang_modules.erl
/home/<username>/dnsresolvd-multilang/src/elixir
```

Now the daemon might be built.

```
$ gmake clean && gmake all
rm -f lib/*.beam
elixirc -o lib lib
```

Once this is done, check it out... just for fun:))

```
$ ls -al . lib
.:
total 40
drwxr-xr-x   4 <username>  <usergroup>   512 Oct  4 12:50 .
drwxr-xr-x  11 <username>  <usergroup>   512 Sep  8 01:25 ..
-rw-r--r--   1 <username>  <usergroup>   879 Oct  4 12:50 Makefile
-rwxr-xr-x   1 <username>  <usergroup>  4365 Oct  4 12:50 dnsresolvd
-rw-r--r--   1 <username>  <usergroup>  2256 Oct  4 12:50 dnsresolvd.app
drwxr-xr-x   6 <username>  <usergroup>   512 Oct  4 12:50 erlang_modules
drwxr-xr-x   2 <username>  <usergroup>   512 Oct  4 12:50 lib

lib:
total 108
drwxr-xr-x  2 <username>  <usergroup>    512 Oct  4 12:50 .
drwxr-xr-x  4 <username>  <usergroup>    512 Oct  4 12:50 ..
-rw-r--r--  1 <username>  <usergroup>   9364 Oct  4 12:50 Elixir.AUX.beam
-rw-r--r--  1 <username>  <usergroup>   5072 Oct  4 12:50 Elixir.DnsResolvd.beam
-rw-r--r--  1 <username>  <usergroup>   2300 Oct  4 12:50 Elixir.DnsResolvs.beam
-rw-r--r--  1 <username>  <usergroup>   9860 Oct  4 12:50 Elixir.ReqHandler.beam
-rw-r--r--  1 <username>  <usergroup>  13738 Oct  4 12:50 dnsresolvd.ex
-rw-r--r--  1 <username>  <usergroup>   5675 Oct  4 12:50 dnsresolvh.ex
$
$ file dnsresolvd lib/*
dnsresolvd:                 a elixir script text executable
lib/Elixir.AUX.beam:        Erlang BEAM file
lib/Elixir.DnsResolvd.beam: Erlang BEAM file
lib/Elixir.DnsResolvs.beam: Erlang BEAM file
lib/Elixir.ReqHandler.beam: Erlang BEAM file
lib/dnsresolvd.ex:          ASCII English text
lib/dnsresolvh.ex:          ASCII English text
```

**TODO:** Describe the daemon's dependencies' build/install process under Ubuntu Server and Arch Linux.

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

OpenBSD/amd64:

```
$ ELIXIR_ERL_OPTIONS="-pz lib erlang_modules/deps/syslog/ebin \
                              erlang_modules/deps/cowboy/ebin \
                              erlang_modules/deps/cowlib/ebin \
                              erlang_modules/deps/ranch/ebin" \
  ./dnsresolvd 8765
Server started on port 8765
=== Hit Ctrl+\ to terminate it.
```

Example of making **GET** and **POST** requests:

```
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" 'http://localhost:8765/?f=xyz&h=hexdocs.pm'
{"hostname":"hexdocs.pm","address":"151.101.85.181","version":"IPv4"}
=== 200
=== application/json
$
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" -d 'h=IPv6.CYBERNODE.com&f=HtmL' http://localhost:8765
<!DOCTYPE html>
<html lang="en-US" dir="ltr">
<head>
<meta http-equiv="content-type"    content="text/html; charset=UTF-8"           />
<meta http-equiv="X-UA-Compatible" content="IE=edge"                            />
<meta       name="viewport"        content="width=device-width,initial-scale=1" />
<title>DNS Resolver Daemon (dnsresolvd)</title>
</head>
<body>
<div>IPv6.CYBERNODE.com 2001:470:1:1B9::31 IPv6</div>
</body>
</html>

=== 200
=== text/html; charset=UTF-8
```
