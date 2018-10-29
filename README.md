# DNS Resolver Daemon (dnsresolvd)

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request,
<br />with the focus on its implementation using various programming languages**

---

**TODO:** Describe what this daemon is for, and emphasize its necessity to be tailored as a multilingual project.

---

The following implementations are on the bench (:small_blue_diamond: &ndash; complete, :small_orange_diamond: &ndash; planned/postponed, :cd: &ndash; in progress):

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
* :small_orange_diamond: :cd: :point_left: :question: :smiley:

## Table of Contents

* **[Building](#building)**
  * [C (GNU libmicrohttpd)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/c#building)
  * [JavaScript (Node.js)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/js#building)
  * [Lua (Luvit)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/lua#building)
  * [Perl 5 (Mojolicious)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/perl#building)
  * [Python 3 (Twisted)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/python#building)
  * [Vala (GNOME libsoup)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/vala#building)
  * [Genie (GNOME libsoup)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/genie#building)
  * [Elixir (Cowboy)](#elixir-cowboy)
  * [Erlang (Cowboy)](#erlang-cowboy)
  * [LFE (Cowboy)](#lfe-cowboy)
* **[Running](#running)**
  * [C (GNU libmicrohttpd)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/c#running)
  * [JavaScript (Node.js)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/js#running)
  * [Lua (Luvit)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/lua#running)
  * [Perl 5 (Mojolicious)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/perl#running)
  * [Python 3 (Twisted)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/python#running)
  * [Vala (GNOME libsoup)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/vala#running)
  * [Genie (GNOME libsoup)](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/genie#running)
  * [Elixir (Cowboy)](#elixir-cowboy-1)
  * [Erlang (Cowboy)](#erlang-cowboy-1)
  * [LFE (Cowboy)](#lfe-cowboy-1)

## Building

Every daemon implementation has its own build rules, so let's describe them sequentially.

### Elixir (Cowboy)

#### Building under OpenBSD/amd64 6.3

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

### Erlang (Cowboy)

#### Building under OpenBSD/amd64 6.3

Install the necessary dependencies (`rebar19`, `syslog`, `cowboy`). Note that the `erlang` package will be installed automatically as a dependency to the `rebar19` package:

```
$ sudo pkg_add -vvvvv rebar19
$
$ erl19 +V
Erlang (SMP,ASYNC_THREADS) (BEAM) emulator version 8.3
```

Create the following symlinks (required for compilation and execution):

```
$ sudo ln -sfn /usr/local/bin/erlc19    /usr/local/bin/erlc    && \
  sudo ln -sfn /usr/local/bin/escript19 /usr/local/bin/escript
```

The `syslog` and `cowboy` packages have to be built and installed via **rebar**. For that it needs to create a &quot;mock&quot; project and install all the necessary dependencies. The following compound one-liner script will actually do this job.

(Note that the `cowboy` package depends on the others: `cowlib` and `ranch`. They will be built and installed automatically too.)

```
$ cd src/erlang
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
/home/<username>/dnsresolvd-multilang/src/erlang
```

Now the daemon might be built.

```
$ gmake clean && gmake all
rm -f -vR lib/ebin
lib/ebin
if [ ! -d "lib/ebin" ]; then \
        mkdir lib/ebin; \
        erlc -o lib/ebin lib/*.erl; \
fi
```

Once this is done, check it out... just for fun:))

```
$ ls -al . lib lib/ebin
.:
total 32
drwxr-xr-x   4 <username>  <usergroup>   512 Oct  4 13:50 .
drwxr-xr-x  11 <username>  <usergroup>   512 Sep  8 01:25 ..
-rw-r--r--   1 <username>  <usergroup>   992 Oct  4 13:50 Makefile
-rwxr-xr-x   1 <username>  <usergroup>  4209 Oct  4 13:50 dnsresolvd
drwxr-xr-x   6 <username>  <usergroup>   512 Oct  4 13:50 erlang_modules
drwxr-xr-x   3 <username>  <usergroup>   512 Oct  4 13:50 lib

lib:
total 60
drwxr-xr-x  3 <username>  <usergroup>    512 Oct  4 13:50 .
drwxr-xr-x  4 <username>  <usergroup>    512 Oct  4 13:50 ..
-rw-r--r--  1 <username>  <usergroup>   3634 Oct  4 13:50 dnsresolvd.erl
-rw-r--r--  1 <username>  <usergroup>   4546 Oct  4 13:50 dnsresolvd.h
-rw-r--r--  1 <username>  <usergroup>   1455 Oct  4 13:50 dnsresolvs.erl
drwxr-xr-x  2 <username>  <usergroup>    512 Oct  4 13:50 ebin
-rw-r--r--  1 <username>  <usergroup>  10241 Oct  4 13:50 reqhandler.erl

lib/ebin:
total 24
drwxr-xr-x  2 <username>  <usergroup>   512 Oct  4 13:50 .
drwxr-xr-x  3 <username>  <usergroup>   512 Oct  4 13:50 ..
-rw-r--r--  1 <username>  <usergroup>  1908 Oct  4 13:50 dnsresolvd.beam
-rw-r--r--  1 <username>  <usergroup>   776 Oct  4 13:50 dnsresolvs.beam
-rw-r--r--  1 <username>  <usergroup>  3864 Oct  4 13:50 reqhandler.beam
$
$ file dnsresolvd lib/* lib/ebin/*
dnsresolvd:               a escript script text executable
lib/dnsresolvd.erl:       ASCII English text
lib/dnsresolvd.h:         ASCII English text
lib/dnsresolvs.erl:       ASCII English text
lib/ebin:                 directory
lib/reqhandler.erl:       ASCII English text
lib/ebin/dnsresolvd.beam: Erlang BEAM file
lib/ebin/dnsresolvs.beam: Erlang BEAM file
lib/ebin/reqhandler.beam: Erlang BEAM file
```

**TODO:** Describe the daemon's dependencies' build/install process under Ubuntu Server and Arch Linux.

### LFE (Cowboy)

#### Building under OpenBSD/amd64 6.3

Install the necessary dependencies (`rebar19`, `syslog`, `cowboy`). Note that the `erlang` package will be installed automatically as a dependency to the `rebar19` package:

```
$ sudo pkg_add -vvvvv rebar19
```

Create the following symlink (required for compilation and execution):

```
$ sudo ln -sfn /usr/local/bin/erl19 /usr/local/bin/erl
$
$ erl +V
Erlang (SMP,ASYNC_THREADS) (BEAM) emulator version 8.3
```

The `syslog` and `cowboy` packages have to be built and installed via **rebar**. For that it needs to create a &quot;mock&quot; project and install all the necessary dependencies. The following compound one-liner script will actually do this job.

(Note that the `cowboy` package depends on the others: `cowlib` and `ranch`. They will be built and installed automatically too.)

```
$ cd src/lfe
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
/home/<username>/dnsresolvd-multilang/src/lfe
```

Build and install the **LFE** "package". &ndash; Following a sequence of steps provided [here](https://github.com/rgolubtsov/dnsresolvd-multilang/blob/master/lfe-build-install.md#building-under-openbsdamd64-63 "Building and installing LFE under OpenBSD/amd64 6.3") might be considered helpful for this process.

```
$ lfe
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:2:2] [async-threads:10] [kernel-poll:false]

   ..-~.~_~---..
  (      \\     )    |   A Lisp-2+ on the Erlang VM
  |`-.._/_\\_.-':    |   Type (help) for usage info.
  |         g |_ \   |
  |        n    | |  |   Docs: http://docs.lfe.io/
  |       a    / /   |   Source: http://github.com/rvirding/lfe
   \     l    |_/    |
    \   r     /      |   LFE v1.3-dev (abort with ^G)
     `-E___.-'

lfe>
```

Now the daemon might be built.

```
$ gmake clean && gmake all
rm -f -vR lib/ebin
lib/ebin
if [ ! -d "lib/ebin" ]; then               \
        mkdir lib/ebin;                 \
        lfec -o lib/ebin lib/*.lfe; \
fi
lib/dnsresolvh.lfe:none: Warning: a term is constructed, but never used
```

Once this is done, check it out... just for fun:))

```
$ ls -al . lib lib/ebin
.:
total 32
drwxr-xr-x   4 <username>  <usergroup>   512 Oct 25 18:20 .
drwxr-xr-x  12 <username>  <usergroup>   512 Oct  5 13:20 ..
-rw-r--r--   1 <username>  <usergroup>  1019 Oct 25 18:20 Makefile
-rwxr-xr-x   1 <username>  <usergroup>  4915 Oct 25 18:20 dnsresolvd
drwxr-xr-x   6 <username>  <usergroup>   512 Oct 25 18:20 erlang_modules
drwxr-xr-x   3 <username>  <usergroup>   512 Oct 25 18:20 lib

lib:
total 64
drwxr-xr-x  3 <username>  <usergroup>    512 Oct 25 18:20 .
drwxr-xr-x  4 <username>  <usergroup>    512 Oct 25 18:20 ..
-rw-r--r--  1 <username>  <usergroup>  15395 Oct 25 18:20 dnsresolvd.lfe
-rw-r--r--  1 <username>  <usergroup>   8969 Oct 25 18:20 dnsresolvh.lfe
drwxr-xr-x  2 <username>  <usergroup>    512 Oct 25 18:20 ebin

lib/ebin:
total 44
drwxr-xr-x  2 <username>  <usergroup>   512 Oct 25 18:20 .
drwxr-xr-x  3 <username>  <usergroup>   512 Oct 25 18:20 ..
-rw-r--r--  1 <username>  <usergroup>  6000 Oct 25 18:20 AUX.beam
-rw-r--r--  1 <username>  <usergroup>  2356 Oct 25 18:20 dnsresolvd.beam
-rw-r--r--  1 <username>  <usergroup>  1052 Oct 25 18:20 dnsresolvs.beam
-rw-r--r--  1 <username>  <usergroup>  4640 Oct 25 18:20 reqhandler.beam
$
$ file dnsresolvd lib/* lib/ebin/*
dnsresolvd:               a lfescript script text executable
lib/dnsresolvd.lfe:       ASCII English text
lib/dnsresolvh.lfe:       ASCII English text
lib/ebin:                 directory
lib/ebin/AUX.beam:        Erlang BEAM file
lib/ebin/dnsresolvd.beam: Erlang BEAM file
lib/ebin/dnsresolvs.beam: Erlang BEAM file
lib/ebin/reqhandler.beam: Erlang BEAM file
```

**TODO:** Describe the daemon's dependencies' build/install process under Ubuntu Server and Arch Linux.

## Running

Starting the daemon is quite easy and very similar for all its implementations.

### Elixir (Cowboy)

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

### Erlang (Cowboy)

OpenBSD/amd64:

```
$ ERL_LIBS="erlang_modules/deps:lib" ./dnsresolvd 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
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

### LFE (Cowboy)

OpenBSD/amd64:

```
$ ERL_LIBS="erlang_modules/deps:lib" ./dnsresolvd 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
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

---

:cd:
