# DNS Resolver Daemon written in Genie

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [GNOME libsoup](https://valadoc.org/libsoup-2.4/index.html "GNOME libsoup") HTTP client/server library)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.3](#building-under-openbsdamd64-63)
* **[Running](#running)**

## Building

This daemon implementation might be built and run successfully on OpenBSD, Ubuntu Server, and Arch Linux operating systems.

### Building under OpenBSD/amd64 6.3

All the necessary build-time and run-time dependencies are exactly the same to what is being used for [Vala](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/vala#building "Vala (GNOME libsoup) build instructions").

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
/usr/local/lib/libglib-2.0.so.4200.6: warning: stpcpy() is dangerous; do not use it
/usr/local/lib/libglib-2.0.so.4200.6: warning: vsprintf() is often misused, please use vsnprintf()
/usr/local/lib/libsoup-2.4.so.10.0: warning: strcpy() is almost always misused, please use strlcpy()
/usr/local/lib/libxml2.so.16.1: warning: strcat() is almost always misused, please use strlcat()
/usr/local/lib/libxml2.so.16.1: warning: sprintf() is often misused, please use snprintf()
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 124
drwxr-xr-x   2 <username>  <usergroup>    512 Nov 23 18:00 .
drwxr-xr-x  12 <username>  <usergroup>    512 Oct  5 13:20 ..
-rw-r--r--   1 <username>  <usergroup>   1734 Nov 23 18:00 Makefile
-rw-r--r--   1 <username>  <usergroup>   5752 Nov 23 18:00 README.md
-rwxr-xr-x   1 <username>  <usergroup>  26304 Nov 23 18:00 dnsresolvd
-rw-r--r--   1 <username>  <usergroup>  14817 Nov 23 18:00 dnsresolvd.gs
-rw-r--r--   1 <username>  <usergroup>   6408 Nov 23 18:00 dnsresolvh.vala
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB shared object, x86-64, version 1
$
$ ldd dnsresolvd
dnsresolvd:
        Start            End              Type  Open Ref GrpRef Name
        00001b4849200000 00001b4849407000 exe   2    0   0      dnsresolvd
        00001b4a5c4e8000 00001b4a5c7d7000 rlib  0    1   0      /usr/local/lib/libsoup-2.4.so.10.0
        00001b4a684d5000 00001b4a6886c000 rlib  0    2   0      /usr/local/lib/libxml2.so.16.1
        00001b4ac5e92000 00001b4ac60b9000 rlib  0    1   0      /usr/local/lib/libjson-glib-1.0.so.6.0
        00001b4ad214d000 00001b4ad24f4000 rlib  0    3   0      /usr/local/lib/libgio-2.0.so.4200.6
        00001b4a8a99a000 00001b4a8ab9e000 rlib  0    3   0      /usr/local/lib/libgmodule-2.0.so.4200.6
        00001b4b2b177000 00001b4b2b3c6000 rlib  0    4   0      /usr/local/lib/libgobject-2.0.so.4200.6
        00001b4ad4e63000 00001b4ad5182000 rlib  0    6   0      /usr/local/lib/libglib-2.0.so.4200.6
        00001b4a84d35000 00001b4a84f40000 rlib  0    6   0      /usr/local/lib/libintl.so.6.0
        00001b4ae0c23000 00001b4ae0e68000 rlib  0    6   0      /usr/local/lib/libpcre.so.3.0
        00001b4b28b63000 00001b4b28d6c000 rlib  0    9   0      /usr/lib/libpthread.so.25.1
        00001b4b0bd11000 00001b4b0bff1000 rlib  0    1   0      /usr/lib/libc.so.92.3
        00001b4b0f178000 00001b4b0f475000 rlib  0    7   0      /usr/local/lib/libiconv.so.6.0
        00001b4a5399c000 00001b4a53ba4000 rlib  0    3   0      /usr/local/lib/libffi.so.1.2
        00001b4a76c0a000 00001b4a76e21000 rlib  0    4   0      /usr/lib/libz.so.5.0
        00001b4b2d1e5000 00001b4b2d40c000 rlib  0    2   0      /usr/local/lib/liblzma.so.2.1
        00001b4ab5edd000 00001b4ab6105000 rlib  0    3   0      /usr/lib/libm.so.10.1
        00001b4b3d1cd000 00001b4b3d549000 rlib  0    1   0      /usr/local/lib/libsqlite3.so.37.2
        00001b4a70a00000 00001b4a70a00000 ld.so 0    1   0      /usr/libexec/ld.so
```

The Genie daemon's build processes under Ubuntu Server and Arch Linux are exactly the same to those ones which are being used for [Vala](https://github.com/rgolubtsov/dnsresolvd-multilang/tree/master/src/vala#building "Vala (GNOME libsoup) build instructions") on those operating systems.

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

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
