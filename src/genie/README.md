# DNS Resolver Daemon written in Genie

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [GNOME libsoup](https://valadoc.org/libsoup-2.4/index.html "GNOME libsoup") library)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.3](#building-under-openbsdamd64-63)
* **[Running](#running)**

## Building

This daemon implementation is known to be built and run successfully on OpenBSD, Ubuntu Server, and Arch Linux operating systems.

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
