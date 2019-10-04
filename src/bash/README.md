# DNS Resolver Daemon written in Bash

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [Netcat](http://nc110.sourceforge.net "Netcat: the TCP/IP swiss army") TCP/UDP network connections utility)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.5](#building-under-openbsdamd64-65)
  * [Building under Ubuntu Server (Ubuntu 16.04.6 LTS x86-64)](#building-under-ubuntu-server-ubuntu-16046-lts-x86-64)
  * [Building under Arch Linux (kernel 5.2.13-arch1-1-ARCH x86-64)](#building-under-arch-linux-kernel-5213-arch1-1-arch-x86-64)
* **[Running](#running)**

## Building

This daemon implementation is known to be built and run successfully on OpenBSD, Ubuntu Server, and Arch Linux operating systems. So let's describe each build process sequentially.

### Building under OpenBSD/amd64 6.5

Install the necessary dependencies (`bash`); the Netcat utility (`nc`) is in the "base" set of the system (`base65.tgz`), so it doesn't need to be installed explicitly, because it is already installed and should work well:

```
$ sudo pkg_add -vvvvv bash
$
$ bash --version
GNU bash, version 5.0.3(1)-release (x86_64-unknown-openbsd6.5)
...
$
$ nc
usage: nc [-46cDdFhklNnrStUuvz] [-C certfile] [-e name] [-H hash] [-I length]
          [-i interval] [-K keyfile] [-M ttl] [-m minttl] [-O length]
          [-o staplefile] [-P proxy_username] [-p source_port] [-R CAfile]
          [-s source] [-T keyword] [-V rtable] [-W recvlimit] [-w timeout]
          [-X proxy_protocol] [-x proxy_address[:port]] [-Z peercertfile]
          [destination] [port]
```

The daemon itself doesn't need to be built. &ndash; But it's some kind of fun to check out on how it looks in the filesystem:))
```
$ cd src/bash
$ ls -al
total 48
drwxr-xr-x   2 <username>  <usergroup>   512 Sep 29 23:35 .
drwxr-xr-x  15 <username>  <usergroup>   512 Sep  8 20:05 ..
-rw-r--r--   1 <username>  <usergroup>  4726 Sep 29 23:35 README.md
-rwxr-xr-x   1 <username>  <usergroup>  7585 Sep 29 23:35 dnsresolvd
-rw-r--r--   1 <username>  <usergroup>  5464 Sep 29 23:35 dnsresolvd.h
$
$ file dnsresolv*
dnsresolvd:   a bash script text executable
dnsresolvd.h: ASCII English text
```

### Building under Ubuntu Server (Ubuntu 16.04.6 LTS x86-64)

Install the necessary dependencies (`netcat-openbsd`):

```
$ sudo apt-get update                    && \
  sudo apt-get install netcat-openbsd -y
$
$ bash --version
GNU bash, version 4.3.48(1)-release (x86_64-pc-linux-gnu)
...
$
$ nc -h
OpenBSD netcat (Debian patchlevel 1.105-7ubuntu1)
...
```

The daemon itself doesn't need to be built. &ndash; But it's some kind of fun to check out on how it looks in the filesystem:))

```
$ cd src/bash
$ ls -al
total 32
drwxrwxr-x  2 <username> <usergroup> 4096 Oct  3 22:35 .
drwxrwxr-x 15 <username> <usergroup> 4096 Sep  5 20:55 ..
-rwxrwxr-x  1 <username> <usergroup> 7585 Oct  3 22:35 dnsresolvd
-rw-rw-r--  1 <username> <usergroup> 5464 Oct  3 22:35 dnsresolvd.h
-rw-rw-r--  1 <username> <usergroup> 4726 Oct  3 22:35 README.md
$
$ file dnsresolv*
dnsresolvd:   Bourne-Again shell script, ASCII text executable
dnsresolvd.h: ASCII text
```

### Building under Arch Linux (kernel 5.2.13-arch1-1-ARCH x86-64)

Install the necessary dependencies (`nmap`):

```
$ sudo pacman -Sy nmap
$
$ bash --version
GNU bash, version 5.0.9(1)-release (x86_64-pc-linux-gnu)
...
$
$ ncat -h
Ncat 7.80 ( https://nmap.org/ncat )
...
```

The daemon itself doesn't need to be built. &ndash; But it's some kind of fun to check out on how it looks in the filesystem:))

```
$ cd src/bash
$ ls -al
total 32
drwxr-xr-x  2 <username> <usergroup> 4096 Oct  3 23:10 .
drwxr-xr-x 15 <username> <usergroup> 4096 Aug 20 22:40 ..
-rwxr-xr-x  1 <username> <usergroup> 7585 Oct  3 23:10 dnsresolvd
-rw-r--r--  1 <username> <usergroup> 5464 Oct  3 23:10 dnsresolvd.h
-rw-r--r--  1 <username> <usergroup> 4746 Oct  3 23:10 README.md
$
$ file dnsresolv*
dnsresolvd:   Bourne-Again shell script, ASCII text executable
dnsresolvd.h: ASCII text
```

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

OpenBSD/amd64 | Ubuntu Server LTS x86-64 | Arch Linux:

```
$ ./src/bash/dnsresolvd 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

Example of making **GET** and **POST** requests:

```
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" http://localhost:8765
{"hostname":"openports.se","address":"37.49.241.43","version":"IPv4"}
=== 200
=== application/json
$
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" -d '' http://localhost:8765
{"hostname":"openports.se","address":"37.49.241.43","version":"IPv4"}
=== 200
=== application/json
```
