# DNS Resolver Daemon written in Perl 5

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [Mojolicious](http://mojolicious.org "Mojolicious") real-time MVC web application framework)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.3](#building-under-openbsdamd64-63)
  * [Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)](#building-under-ubuntu-server-ubuntu-16044-lts-x86-64)
* **[Running](#running)**

## Building

This daemon implementation is known to be built and run successfully on OpenBSD, Ubuntu Server~~, and Arch Linux~~ operating systems. So let's describe each build process sequentially.

### Building under OpenBSD/amd64 6.3

Install the necessary dependencies (`cpanminus`, `Mojolicious`, `Net::DNS`):

```
$ sudo pkg_add -vvvvv p5-Mojolicious p5-Net-DNS
$
$ mojo version
CORE
  Perl        (v5.24.3, openbsd)
  Mojolicious (7.70, Doughnut)

OPTIONAL
  EV 4.0+                 (4.22)
  IO::Socket::Socks 0.64+ (n/a)
  IO::Socket::SSL 1.94+   (2.056)
  Net::DNS::Native 0.15+  (n/a)
  Role::Tiny 2.000001+    (n/a)

You might want to update your Mojolicious to 7.89!
```

### Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)

Install the necessary dependencies (`cpanminus`, `Mojolicious`, `Net::DNS::Native`):

```
$ sudo apt-get update               && \
  sudo apt-get install cpanminus -y && \
  sudo cpanm App::cpanminus
$
$ sudo cpanm Mojolicious Net::DNS::Native
$
$ mojo version
CORE
  Perl        (v5.22.1, linux)
  Mojolicious (7.59, Doughnut)

OPTIONAL
  EV 4.0+                 (n/a)
  IO::Socket::Socks 0.64+ (n/a)
  IO::Socket::SSL 1.94+   (n/a)
  Net::DNS::Native 0.15+  (0.15)
  Role::Tiny 2.000001+    (2.000001)

This version is up to date, have fun!
```

**TODO:** Describe the daemon's dependencies' build/install process under Arch Linux.

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

OpenBSD/amd64 | Arch Linux:

```
$ ./src/perl/dnsresolvd 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
[Tue Aug  7 15:00:00 2018] [info] Listening at "http://*:8765"
Server available at http://127.0.0.1:8765
```

Example of making **GET** and **POST** requests:

```
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" 'http://localhost:8765/?f=xml&h=testvitesse.videotron.ca'
{"address":"135.19.0.17","hostname":"testvitesse.videotron.ca","version":"IPv4"}

=== 200
=== application/json
$
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" -d 'h=ipv6.testvitesse.videotron.ca&f=xml' http://localhost:8765
{"address":"129.128.5.194","hostname":"openbsd.org","version":"IPv4"}

=== 200
=== application/json
```
