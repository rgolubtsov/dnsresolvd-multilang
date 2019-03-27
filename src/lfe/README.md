# DNS Resolver Daemon written in LFE (Lisp Flavoured Erlang)

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [Cowboy](https://ninenines.eu "Cowboy") HTTP server library for Erlang/OTP)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.3](#building-under-openbsdamd64-63)
* **[Running](#running)**

## Building

This daemon implementation is known to be built and run successfully on OpenBSD, ~~Ubuntu Server, and Arch Linux~~ operating systems. So let's describe each build process sequentially.

### Building under OpenBSD/amd64 6.3

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

Build and install the **LFE** "package". &ndash; Following a sequence of steps provided [here](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/lfe/lfe-build-install.md#building-under-openbsdamd64-63 "Building and installing LFE under OpenBSD/amd64 6.3") might be considered helpful for this process.

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

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

OpenBSD/amd64:

```
$ cd src/lfe
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
