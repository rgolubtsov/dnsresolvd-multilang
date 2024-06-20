# DNS Resolver Daemon written in Erlang

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [Cowboy](https://ninenines.eu "Cowboy") HTTP server library for Erlang/OTP)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.3](#building-under-openbsdamd64-63)
  * [Building under Ubuntu Server (Ubuntu 18.04.1 LTS x86-64)](#building-under-ubuntu-server-ubuntu-18041-lts-x86-64)
* **[Running](#running)**

## Building

This daemon implementation might be built and run successfully on OpenBSD, Ubuntu Server, ~~and Arch Linux~~ operating systems. So let's describe each build process sequentially.

### Building under OpenBSD/amd64 6.3

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

### Building under Ubuntu Server (Ubuntu 18.04.1 LTS x86-64)

Install the necessary dependencies (`rebar`, `syslog`, `cowboy`). Note that the `erlang-base` package and other required `erlang-`related packages will be installed automatically as dependencies to the `rebar` package:

```
$ sudo apt-get update           && \
  sudo apt-get install rebar -y
$
$ erl +V
Erlang (SMP,ASYNC_THREADS) (BEAM) emulator version 9.2
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
  rebar   -f create-lib libid=${E_LIB_ID}    && \
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
  rebar      g-d                             && \
  rebar      c-d                             && \
  rebar      co                              && \
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
Pulling cowlib from {git,"https://github.com/ninenines/cowlib","2.7.0"}
Cloning into 'cowlib'...
Pulling ranch from {git,"https://github.com/ninenines/ranch","1.7.1"}
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
$ make clean && make all
rm -f -vR lib/ebin
if [ ! -d "lib/ebin" ]; then \
        mkdir lib/ebin; \
        erlc -o lib/ebin lib/*.erl; \
fi
```

Once this is done, check it out... just for fun:))

```
$ ls -al . lib lib/ebin
.:
total 40
drwxrwxr-x  4 <username> <usergroup>  4096 Dec  6 00:50 .
drwxrwxr-x 12 <username> <usergroup>  4096 Nov 28 17:35 ..
-rwxrwxr-x  1 <username> <usergroup>  4209 Dec  6 00:50 dnsresolvd
drwxrwxr-x  6 <username> <usergroup>  4096 Dec  6 00:50 erlang_modules
drwxrwxr-x  3 <username> <usergroup>  4096 Dec  6 00:50 lib
-rw-rw-r--  1 <username> <usergroup>   992 Dec  6 00:50 Makefile
-rw-rw-r--  1 <username> <usergroup> 11255 Dec  6 00:50 README.md

lib:
total 40
drwxrwxr-x 3 <username> <usergroup>  4096 Dec  6 00:50 .
drwxrwxr-x 4 <username> <usergroup>  4096 Dec  6 00:50 ..
-rw-rw-r-- 1 <username> <usergroup>  3634 Dec  6 00:50 dnsresolvd.erl
-rw-rw-r-- 1 <username> <usergroup>  4546 Dec  6 00:50 dnsresolvd.h
-rw-rw-r-- 1 <username> <usergroup>  1455 Dec  6 00:50 dnsresolvs.erl
drwxrwxr-x 2 <username> <usergroup>  4096 Dec  6 00:50 ebin
-rw-rw-r-- 1 <username> <usergroup> 10241 Dec  6 00:50 reqhandler.erl

lib/ebin:
total 20
drwxrwxr-x 2 <username> <usergroup> 4096 Dec  6 00:50 .
drwxrwxr-x 3 <username> <usergroup> 4096 Dec  6 00:50 ..
-rw-rw-r-- 1 <username> <usergroup> 1860 Dec  6 00:50 dnsresolvd.beam
-rw-rw-r-- 1 <username> <usergroup>  748 Dec  6 00:50 dnsresolvs.beam
-rw-rw-r-- 1 <username> <usergroup> 3824 Dec  6 00:50 reqhandler.beam
$
$ file dnsresolvd lib/* lib/ebin/*
dnsresolvd:               a /usr/bin/env escript script, ASCII text executable
lib/dnsresolvd.erl:       ASCII text
lib/dnsresolvd.h:         ASCII text
lib/dnsresolvs.erl:       ASCII text
lib/ebin:                 directory
lib/reqhandler.erl:       ASCII text
lib/ebin/dnsresolvd.beam: Erlang BEAM file
lib/ebin/dnsresolvs.beam: Erlang BEAM file
lib/ebin/reqhandler.beam: Erlang BEAM file
```

**TODO:** Describe the daemon's dependencies' build/install process under Arch Linux.

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

OpenBSD/amd64 | Ubuntu Server LTS x86-64:

```
$ cd src/erlang
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
