# DNS Resolver Daemon written in Go

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [net/http](https://golang.org/pkg/net/http/ "Package http") package from Go Stdlib, that provides HTTP client and server implementations)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.6](#building-under-openbsdamd64-66)
  * [Building under Arch Linux 32 (kernel 5.4.2-arch1-1.0 i686)](#building-under-arch-linux-32-kernel-542-arch1-10-i686)
* **[Running](#running)**

## Building

This daemon implementation is known to be built and run successfully on OpenBSD, ~~Ubuntu Server, and~~ Arch Linux 32 operating systems. So let's describe each build process sequentially.

### Building under OpenBSD/amd64 6.6

Install the necessary dependencies (`gmake`, `go`):

```
$ sudo pkg_add -vvvvv gmake go
$
$ go version
go version go1.13.1 openbsd/amd64
```

Now the daemon might be built.

```
$ cd src/go
$ gmake clean && gmake all
rm -f dnsresolvd dnsresolvd.o
go tool compile -complete -o dnsresolvd.o dnsresolvd.go dnsresolvh.go
go tool link -s -w        -o dnsresolvd dnsresolvd.o
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 11240
drwxr-xr-x   2 <username>  <usergroup>      512 Feb 11 23:30 .
drwxr-xr-x  16 <username>  <usergroup>      512 Jan  7 15:00 ..
-rw-r--r--   1 <username>  <usergroup>     1078 Feb 11 23:30 Makefile
-rw-r--r--   1 <username>  <usergroup>     4844 Feb 11 23:30 README.md
-rwxr-xr-x   1 <username>  <usergroup>  5652480 Feb 11 23:30 dnsresolvd
-rw-r--r--   1 <username>  <usergroup>    11746 Feb 11 23:30 dnsresolvd.go
-rw-r--r--   1 <username>  <usergroup>    53980 Feb 11 23:30 dnsresolvd.o
-rw-r--r--   1 <username>  <usergroup>     5141 Feb 11 23:30 dnsresolvh.go
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB executable, x86-64, version 1
$
$ ldd dnsresolvd
dnsresolvd:
        Start            End              Type  Open Ref GrpRef Name
        0000000000400000 0000000000985000 exe   2    0   0      dnsresolvd
        00000002b2e06000 00000002b2e13000 rlib  0    1   0      /usr/lib/libpthread.so.26.1
        000000026b83b000 000000026b92f000 rlib  0    1   0      /usr/lib/libc.so.95.1
        00000002b4790000 00000002b4790000 ld.so 0    1   0      /usr/libexec/ld.so
```

### Building under Arch Linux 32 (kernel 5.4.2-arch1-1.0 i686)

Install the necessary dependencies (`go`):

```
$ sudo pacman -Sy go
$
$ go version
go version go1.13.5 linux/386
```

Now the daemon might be built.

```
$ cd src/go
$ make clean && make all
rm -f dnsresolvd dnsresolvd.o
go tool compile -complete -o dnsresolvd.o dnsresolvd.go dnsresolvh.go
go tool link -s -w        -o dnsresolvd dnsresolvd.o
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 4820
drwxr-xr-x  2 <username> <usergroup>    4096 Jan 31 22:30 .
drwxr-xr-x 16 <username> <usergroup>    4096 Jan 17 22:10 ..
-rwxr-xr-x  1 <username> <usergroup> 4849664 Jan 31 22:30 dnsresolvd
-rw-r--r--  1 <username> <usergroup>   11746 Jan 31 22:30 dnsresolvd.go
-rw-r--r--  1 <username> <usergroup>   47754 Jan 31 22:30 dnsresolvd.o
-rw-r--r--  1 <username> <usergroup>    5141 Jan 31 22:30 dnsresolvh.go
-rw-r--r--  1 <username> <usergroup>    1078 Jan 31 22:30 Makefile
-rw-r--r--  1 <username> <usergroup>    3184 Jan 31 22:30 README.md
$
$ file dnsresolvd
dnsresolvd: ELF 32-bit LSB executable, Intel 80386, version 1 (SYSV), dynamically linked, interpreter /lib/ld-linux.so.2, stripped
$
$ ldd dnsresolvd
        linux-gate.so.1 (0xb7f2b000)
        libpthread.so.0 => /usr/lib/libpthread.so.0 (0xb7ee0000)
        libc.so.6 => /usr/lib/libc.so.6 (0xb7d30000)
        /lib/ld-linux.so.2 => /usr/lib/ld-linux.so.2 (0xb7f2c000)
```

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

OpenBSD/amd64 | Arch Linux 32:

```
$ ./src/go/dnsresolvd 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

Example of making **GET** and **POST** requests:

```
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" 'http://localhost:8765/?h=openports.se&f=xml'
{"hostname":"openports.se","address":"37.49.241.43","version":"IPv4"}
=== 200
=== application/json
$
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" -d 'h=ipv6.google.com&f=HTml' http://localhost:8765
<!DOCTYPE html>
<html lang="en-US" dir="ltr">
<head>
<meta http-equiv="Content-Type"    content="text/html; charset=UTF-8"           />
<meta http-equiv="X-UA-Compatible" content="IE=edge"                            />
<meta       name="viewport"        content="width=device-width,initial-scale=1" />
<title>DNS Resolver Daemon (dnsresolvd)</title>
</head>
<body>
<div>ipv6.google.com 2a00:1450:401b:806::200e IPv6</div>
</body>
</html>

=== 200
=== text/html; charset=UTF-8
```
