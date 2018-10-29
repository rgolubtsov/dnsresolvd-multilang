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
  * [Genie (GNOME libsoup)](#genie-gnome-libsoup)
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
  * [Genie (GNOME libsoup)](#genie-gnome-libsoup-1)
  * [Elixir (Cowboy)](#elixir-cowboy-1)
  * [Erlang (Cowboy)](#erlang-cowboy-1)
  * [LFE (Cowboy)](#lfe-cowboy-1)

## Building

Every daemon implementation has its own build rules, so let's describe them sequentially.

### Genie (GNOME libsoup)

#### Building under OpenBSD/amd64 6.3

All the necessary build-time and run-time dependencies are exactly the same to what is being used for Vala build process &ndash; [see here](https://github.com/rgolubtsov/dnsresolvd-multilang#vala-gnome-libsoup "Vala (GNOME libsoup) build instructions").

The daemon's build process is straightforward, moreover it is identical to that used with Vala.

```
$ cd src/genie
$ gmake clean && gmake all
rm -f dnsresolvd
valac --target-glib=2.40 --cc=egcc -X -s -X -O3 -X -mtune=generic -X -pipe -X -fstack-protector-strong --pkg=posix --pkg=libsoup-2.4 --pkg=json-glib-1.0           -o dnsresolvd dnsresolvd.gs dnsresolvh.vala
/home/<username>/dnsresolvd-multilang/src/genie/dnsresolvd.vala.c: In function '__lambda6_':
/home/<username>/dnsresolvd-multilang/src/genie/dnsresolvd.vala.c:709:16: warning: assignment discards 'const' qualifier from pointer target type
        _tmp29_ = _tmp28_->data;
                ^
/usr/local/lib/libsoup-2.4.so.10.0: warning: strcpy() is almost always misused, please use strlcpy()
/usr/local/lib/libglib-2.0.so.4200.6: warning: stpcpy() is dangerous; do not use it
/usr/local/lib/libxml2.so.16.1: warning: strcat() is almost always misused, please use strlcat()
/usr/local/lib/libglib-2.0.so.4200.6: warning: vsprintf() is often misused, please use vsnprintf()
/usr/local/lib/libxml2.so.16.1: warning: sprintf() is often misused, please use snprintf()
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 112
drwxr-xr-x  2 <username>  <usergroup>    512 Aug  2 17:30 .
drwxr-xr-x  9 <username>  <usergroup>    512 Jul 26 12:45 ..
-rw-r--r--  1 <username>  <usergroup>   1734 Aug  2 17:30 Makefile
-rwxr-xr-x  1 <username>  <usergroup>  26288 Aug  2 17:30 dnsresolvd
-rw-r--r--  1 <username>  <usergroup>  14747 Aug  2 17:30 dnsresolvd.gs
-rw-r--r--  1 <username>  <usergroup>   6401 Aug  2 17:30 dnsresolvh.vala
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB shared object, x86-64, version 1
$
$ ldd dnsresolvd
dnsresolvd:
        Start            End              Type  Open Ref GrpRef Name
        0000064489600000 0000064489807000 exe   2    0   0      dnsresolvd
        0000064743f1c000 000006474420b000 rlib  0    1   0      /usr/local/lib/libsoup-2.4.so.10.0
        000006471d823000 000006471dbba000 rlib  0    2   0      /usr/local/lib/libxml2.so.16.1
        00000646f3f50000 00000646f4177000 rlib  0    1   0      /usr/local/lib/libjson-glib-1.0.so.6.0
        0000064696dea000 0000064697191000 rlib  0    3   0      /usr/local/lib/libgio-2.0.so.4200.6
        0000064759350000 0000064759554000 rlib  0    3   0      /usr/local/lib/libgmodule-2.0.so.4200.6
        000006476987a000 0000064769ac9000 rlib  0    4   0      /usr/local/lib/libgobject-2.0.so.4200.6
        0000064729d15000 000006472a034000 rlib  0    6   0      /usr/local/lib/libglib-2.0.so.4200.6
        000006475d65b000 000006475d866000 rlib  0    6   0      /usr/local/lib/libintl.so.6.0
        000006469d595000 000006469d7da000 rlib  0    6   0      /usr/local/lib/libpcre.so.3.0
        0000064784f80000 0000064785189000 rlib  0    9   0      /usr/lib/libpthread.so.25.1
        00000646ae92a000 00000646aec0a000 rlib  0    1   0      /usr/lib/libc.so.92.3
        00000647570ac000 00000647573a9000 rlib  0    7   0      /usr/local/lib/libiconv.so.6.0
        0000064689aca000 0000064689cd2000 rlib  0    3   0      /usr/local/lib/libffi.so.1.2
        0000064690515000 000006469072c000 rlib  0    4   0      /usr/lib/libz.so.5.0
        0000064774344000 000006477456b000 rlib  0    2   0      /usr/local/lib/liblzma.so.2.1
        000006471ccc6000 000006471ceee000 rlib  0    3   0      /usr/lib/libm.so.10.1
        0000064783428000 00000647837a4000 rlib  0    1   0      /usr/local/lib/libsqlite3.so.37.2
        0000064692300000 0000064692300000 ld.so 0    1   0      /usr/libexec/ld.so
```

The Genie daemon's build processes under Ubuntu Server and Arch Linux are exactly the same to those ones which are being used for Vala on those operating systems.

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

### Genie (GNOME libsoup)

OpenBSD/amd64 | Arch Linux:

```
$ ./src/genie/dnsresolvd 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

Example of making **GET** and **POST** requests:

```
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" 'http://localhost:8765/?h=github.com&f=html'
<!DOCTYPE html>
<html lang="en-US" dir="ltr">
<head>
<meta http-equiv="Content-Type"    content="text/html; charset=UTF-8"           />
<meta http-equiv="X-UA-Compatible" content="IE=edge"                            />
<meta       name="viewport"        content="width=device-width,initial-scale=1" />
<title>DNS Resolver Daemon (dnsresolvd)</title>
</head>
<body>
<div>github.com 192.30.253.113 IPv4</div>
</body>
</html>

=== 200
=== text/html; charset=UTF-8
$
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" -d 'f=yaml&h=yaml.org' http://localhost:8765
{"hostname":"yaml.org","address":"192.30.252.154","version":"IPv4"}
=== 200
=== application/json
```

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
