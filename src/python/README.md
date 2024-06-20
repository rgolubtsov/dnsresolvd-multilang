# DNS Resolver Daemon written in Python 3

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [Twisted](http://twistedmatrix.com "Twisted") event-driven networking engine)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.3](#building-under-openbsdamd64-63)
  * [Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)](#building-under-ubuntu-server-ubuntu-16044-lts-x86-64)
* **[Running](#running)**

## Building

This daemon implementation might be built and run successfully on OpenBSD, Ubuntu Server, ~~and Arch Linux~~ operating systems. So let's describe each build process sequentially.

### Building under OpenBSD/amd64 6.3

Install the necessary dependencies (`py3-pip`, `twisted`, `py3-dnspython`):

```
$ sudo pkg_add -vvvvv py3-pip py3-dnspython
$
$ sudo ln -sfn /usr/local/bin/pip3.6 /usr/local/bin/pip
```

```
$ sudo pip install twisted
```

### Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)

Install the necessary dependencies (`python3-twisted`, `python3-dnspython`):

```
$ sudo apt-get update                                       && \
  sudo apt-get install python3-twisted python3-dnspython -y
```

**TODO:** Describe the daemon's dependencies' build/install process under Arch Linux.

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

OpenBSD/amd64:

```
$ ./src/python/dnsresolvd 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

Example of making **GET** and **POST** requests:

```
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" 'http://localhost:8765/?h=google.com&f=xml'
{"hostname": "google.com", "address": "216.58.207.78", "version": "IPv4"}
=== 200
=== application/json
$
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" -d 'h=ipv6.google.com&f=xml' http://localhost:8765
{"hostname": "ipv6.google.com", "address": "2a00:1450:4001:825::200e", "version": "IPv6"}
=== 200
=== application/json
```
