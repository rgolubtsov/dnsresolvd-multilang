# DNS Resolver Daemon written in Bash

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [Netcat](http://nc110.sourceforge.net "Netcat: the TCP/IP swiss army") TCP/UDP network connections utility)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.5](#building-under-openbsdamd64-65)
  * [Building under Ubuntu Server (Ubuntu 16.04.6 LTS x86-64)](#building-under-ubuntu-server-ubuntu-16046-lts-x86-64)
  * [Building under Arch Linux (kernel 5.2.2-arch1-1-ARCH x86-64)](#building-under-arch-linux-kernel-522-arch1-1-arch-x86-64)
* **[Running](#running)**

## Building

This daemon implementation is known to be built and run successfully on OpenBSD, Ubuntu Server, and Arch Linux operating systems. So let's describe each build process sequentially.

### Building under OpenBSD/amd64 6.5

Install the necessary dependencies (): **TODO**

Now the daemon might be built. &ndash; **TODO**

Once this is done, check it out... just for fun:)) &ndash; **TODO**

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
total 28
drwxrwxr-x  2 <username> <usergroup> 4096 Sep  7 22:50 .
drwxrwxr-x 15 <username> <usergroup> 4096 Sep  5 20:55 ..
-rwxrwxr-x  1 <username> <usergroup> 7224 Sep  7 22:50 dnsresolvd
-rw-rw-r--  1 <username> <usergroup> 5154 Sep  7 22:50 dnsresolvd.h
-rw-rw-r--  1 <username> <usergroup> 3181 Sep  7 22:50 README.md
$
$ file dnsresolv*
dnsresolvd:   Bourne-Again shell script, ASCII text executable
dnsresolvd.h: ASCII text
```

### Building under Arch Linux (kernel 5.2.2-arch1-1-ARCH x86-64)

Install the necessary dependencies (`nmap`):

```
$ sudo pacman -Sy nmap
$
$ bash --version
GNU bash, version 5.0.7(1)-release (x86_64-pc-linux-gnu)
...
$
$ ncat -h
Ncat 7.70 ( https://nmap.org/ncat )
...
```

The daemon itself doesn't need to be built. &ndash; But it's some kind of fun to check out on how it looks in the filesystem:))

```
$ ls -al
total 28
drwxr-xr-x  2 <username> <usergroup> 4096 Sep  5 23:10 .
drwxr-xr-x 15 <username> <usergroup> 4096 Aug 20 22:40 ..
-rwxr-xr-x  1 <username> <usergroup> 5324 Sep  5 23:10 dnsresolvd
-rw-r--r--  1 <username> <usergroup> 4515 Sep  5 23:10 dnsresolvh
-rw-r--r--  1 <username> <usergroup> 3118 Sep  5 23:10 README.md
$
$ file dnsresolv*
dnsresolvd: Bourne-Again shell script, ASCII text executable
dnsresolvh: ASCII text
```

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

OpenBSD/amd64 | Ubuntu Server LTS x86-64 | Arch Linux:

```
$ ./src/bash/dnsresolvd 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

Example of making **GET** and **POST** requests: **TODO**
