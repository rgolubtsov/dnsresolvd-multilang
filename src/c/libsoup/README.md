# DNS Resolver Daemon written in C

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [GNOME libsoup](https://developer.gnome.org/libsoup "GNOME libsoup") HTTP client/server library)**

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

Install the necessary dependencies (`gmake`, `gcc`, `libsoup`, `json-glib`):

```
$ sudo pkg_add -vvvvv gmake gcc libsoup json-glib
$
$ egcc --version
egcc (GCC) 4.9.4
Copyright (C) 2015 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```

Now the daemon might be built.

```
$ cd src/c/libsoup
$ gmake clean && gmake all
rm -f dnsresolvd dnsresolvd.o
egcc -Wall -pedantic -std=c99 -O3 -march=x86-64 -mtune=generic -pipe -fstack-protector-strong -D_DEFAULT_SOURCE `pkg-config --cflags-only-I libsoup-2.4 json-glib-1.0`   -c -o dnsresolvd.o dnsresolvd.c
egcc   dnsresolvd.o  `pkg-config   --libs-only-l libsoup-2.4 json-glib-1.0` -o dnsresolvd
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 144
drwxr-xr-x  2 <username>  <usergroup>    512 Aug 15 21:50 .
drwxr-xr-x  4 <username>  <usergroup>    512 Aug 14 13:10 ..
-rw-r--r--  1 <username>  <usergroup>   1536 Aug 15 21:50 Makefile
-rw-r--r--  1 <username>  <usergroup>      0 Aug 15 21:50 README.md
-rwxr-xr-x  1 <username>  <usergroup>  23104 Aug 15 21:50 dnsresolvd
-rw-r--r--  1 <username>  <usergroup>  17762 Aug 15 21:50 dnsresolvd.c
-rw-r--r--  1 <username>  <usergroup>   4494 Aug 15 21:50 dnsresolvd.h
-rw-r--r--  1 <username>  <usergroup>  17192 Aug 15 21:50 dnsresolvd.o
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB shared object, x86-64, version 1
$
$ ldd dnsresolvd
dnsresolvd:
        Start            End              Type  Open Ref GrpRef Name
        000002489063a000 0000024890640000 exe   2    0   0      dnsresolvd
        0000024af3524000 0000024af35c4000 rlib  0    1   0      /usr/local/lib/libsoup-2.4.so.10.2
        0000024a9257d000 0000024a925ab000 rlib  0    1   0      /usr/local/lib/libjson-glib-1.0.so.6.0
        0000024ae22f9000 0000024ae24ee000 rlib  0    3   0      /usr/local/lib/libgio-2.0.so.4200.8
        0000024b25a6d000 0000024b25ac6000 rlib  0    4   0      /usr/local/lib/libgobject-2.0.so.4200.8
        0000024a9e1ce000 0000024a9e308000 rlib  0    6   0      /usr/local/lib/libglib-2.0.so.4201.1
        0000024af54b1000 0000024af54be000 rlib  0    8   0      /usr/local/lib/libintl.so.6.0
        0000024b75176000 0000024b7526b000 rlib  0    1   0      /usr/lib/libc.so.95.0
        0000024abd870000 0000024abda2a000 rlib  0    1   0      /usr/local/lib/libxml2.so.16.1
        0000024ae4f08000 0000024ae50bb000 rlib  0    1   0      /usr/local/lib/libsqlite3.so.37.5
        0000024b12bae000 0000024b12bc1000 rlib  0    1   0      /usr/local/lib/libpsl.so.1.2
        0000024b56d62000 0000024b56d69000 rlib  0    1   0      /usr/local/lib/libgmodule-2.0.so.4200.8
        0000024b83322000 0000024b8333d000 rlib  0    3   0      /usr/lib/libz.so.5.0
        0000024b5fea3000 0000024b5feaf000 rlib  0    7   0      /usr/lib/libpthread.so.26.1
        0000024b43082000 0000024b4308d000 rlib  0    1   0      /usr/local/lib/libffi.so.1.2
        0000024a9cfed000 0000024a9d034000 rlib  0    1   0      /usr/local/lib/libpcre.so.3.0
        0000024afc747000 0000024afc849000 rlib  0    6   0      /usr/local/lib/libiconv.so.6.0
        0000024b658c6000 0000024b658f3000 rlib  0    1   0      /usr/local/lib/liblzma.so.2.1
        0000024b22da5000 0000024b22dd4000 rlib  0    2   0      /usr/lib/libm.so.10.1
        0000024b30b69000 0000024b30b99000 rlib  0    1   0      /usr/local/lib/libidn2.so.1.0
        0000024aa4473000 0000024aa4819000 rlib  0    2   0      /usr/local/lib/libunistring.so.0.1
        0000024ab90fd000 0000024ab90fd000 ld.so 0    1   0      /usr/libexec/ld.so
```

### Building under Ubuntu Server (Ubuntu 16.04.6 LTS x86-64)

Install the necessary dependencies (`tcc`, `libsoup2.4-dev`, `libjson-glib-dev`):

```
$ sudo apt-get update                                         && \
  sudo apt-get install tcc libsoup2.4-dev libjson-glib-dev -y
$
$ tcc -v
tcc version 0.9.26 (x86-64 Linux)
```

Now the daemon might be built.

```
$ cd src/c/libsoup
$ make clean && make all
rm -f dnsresolvd dnsresolvd.o
tcc -Wall -pedantic -std=c99 -O3 -march=x86-64 -mtune=generic -pipe -fstack-protector-strong -D_DEFAULT_SOURCE `pkg-config --cflags-only-I libsoup-2.4 json-glib-1.0`   -c -o dnsresolvd.o dnsresolvd.c
tcc   dnsresolvd.o  `pkg-config   --libs-only-l libsoup-2.4 json-glib-1.0` -o dnsresolvd
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 96
drwxrwxr-x 2 <username> <usergroup>  4096 Aug 15 21:50 .
drwxrwxr-x 4 <username> <usergroup>  4096 Aug 14 13:05 ..
-rwxrwxr-x 1 <username> <usergroup> 32016 Aug 15 21:50 dnsresolvd
-rw-rw-r-- 1 <username> <usergroup> 17762 Aug 15 21:50 dnsresolvd.c
-rw-rw-r-- 1 <username> <usergroup>  4494 Aug 15 21:50 dnsresolvd.h
-rw-rw-r-- 1 <username> <usergroup> 21752 Aug 15 21:50 dnsresolvd.o
-rw-rw-r-- 1 <username> <usergroup>  1536 Aug 15 21:50 Makefile
-rw-rw-r-- 1 <username> <usergroup>     0 Aug 15 21:50 README.md
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/l, not stripped
$
$ ldd dnsresolvd
        linux-vdso.so.1 =>  (0x00007ffdf7ad5000)
        libsoup-2.4.so.1 => /usr/lib/x86_64-linux-gnu/libsoup-2.4.so.1 (0x00007f22c12e4000)
        libgio-2.0.so.0 => /usr/lib/x86_64-linux-gnu/libgio-2.0.so.0 (0x00007f22c0f5c000)
        libgobject-2.0.so.0 => /usr/lib/x86_64-linux-gnu/libgobject-2.0.so.0 (0x00007f22c0d09000)
        libglib-2.0.so.0 => /lib/x86_64-linux-gnu/libglib-2.0.so.0 (0x00007f22c09f8000)
        libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f22c062e000)
        libjson-glib-1.0.so.0 => /usr/lib/x86_64-linux-gnu/libjson-glib-1.0.so.0 (0x00007f22c0406000)
        libxml2.so.2 => /usr/lib/x86_64-linux-gnu/libxml2.so.2 (0x00007f22c004b000)
        libsqlite3.so.0 => /usr/lib/x86_64-linux-gnu/libsqlite3.so.0 (0x00007f22bfd76000)
        libgmodule-2.0.so.0 => /usr/lib/x86_64-linux-gnu/libgmodule-2.0.so.0 (0x00007f22bfb72000)
        libz.so.1 => /lib/x86_64-linux-gnu/libz.so.1 (0x00007f22bf958000)
        libselinux.so.1 => /lib/x86_64-linux-gnu/libselinux.so.1 (0x00007f22bf736000)
        libresolv.so.2 => /lib/x86_64-linux-gnu/libresolv.so.2 (0x00007f22bf51b000)
        libffi.so.6 => /usr/lib/x86_64-linux-gnu/libffi.so.6 (0x00007f22bf313000)
        libpcre.so.3 => /lib/x86_64-linux-gnu/libpcre.so.3 (0x00007f22bf0a3000)
        libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f22bee86000)
        /lib64/ld-linux-x86-64.so.2 (0x00007f22c15bb000)
        libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007f22bec82000)
        libicuuc.so.55 => /usr/lib/x86_64-linux-gnu/libicuuc.so.55 (0x00007f22be8ee000)
        liblzma.so.5 => /lib/x86_64-linux-gnu/liblzma.so.5 (0x00007f22be6cc000)
        libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f22be3c3000)
        libicudata.so.55 => /usr/lib/x86_64-linux-gnu/libicudata.so.55 (0x00007f22bc90c000)
        libstdc++.so.6 => /usr/lib/x86_64-linux-gnu/libstdc++.so.6 (0x00007f22bc58a000)
        libgcc_s.so.1 => /lib/x86_64-linux-gnu/libgcc_s.so.1 (0x00007f22bc374000)
```

### Building under Arch Linux (kernel 5.2.2-arch1-1-ARCH x86-64)

Install the necessary dependencies (`tcc`, `libsoup`, `json-glib`):

```
$ sudo pacman -Sy tcc libsoup json-glib
$
$ tcc -v
tcc version 0.9.27 (x86_64 Linux)
```

Now the daemon might be built.

```
$ cd src/c/libsoup
$ make clean && make all
rm -f dnsresolvd dnsresolvd.o
tcc -Wall -pedantic -std=c99 -O3 -march=x86-64 -mtune=generic -pipe -fstack-protector-strong -D_DEFAULT_SOURCE `pkg-config --cflags-only-I libsoup-2.4 json-glib-1.0`   -c -o dnsresolvd.o dnsresolvd.c
tcc   dnsresolvd.o  `pkg-config   --libs-only-l libsoup-2.4 json-glib-1.0` -o dnsresolvd
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 84
drwxr-xr-x 2 <username> <usergroup>  4096 Aug 15 21:50 .
drwxr-xr-x 4 <username> <usergroup>  4096 Aug 15 19:40 ..
-rwxr-xr-x 1 <username> <usergroup> 16716 Aug 15 21:50 dnsresolvd
-rw-r--r-- 1 <username> <usergroup> 17762 Aug 15 21:50 dnsresolvd.c
-rw-r--r-- 1 <username> <usergroup>  4494 Aug 15 21:50 dnsresolvd.h
-rw-r--r-- 1 <username> <usergroup> 20636 Aug 15 21:50 dnsresolvd.o
-rw-r--r-- 1 <username> <usergroup>  1536 Aug 15 21:50 Makefile
-rw-r--r-- 1 <username> <usergroup>     0 Aug 15 21:50 README.md
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, stripped
$
$ ldd dnsresolvd
        linux-vdso.so.1 (0x00007ffd1a73c000)
        libsoup-2.4.so.1 => /usr/lib/libsoup-2.4.so.1 (0x00007f0b25d55000)
        libc.so.6 => /usr/lib/libc.so.6 (0x00007f0b25b92000)
        libglib-2.0.so.0 => /usr/lib/libglib-2.0.so.0 (0x00007f0b25a70000)
        libgobject-2.0.so.0 => /usr/lib/libgobject-2.0.so.0 (0x00007f0b25a14000)
        libgio-2.0.so.0 => /usr/lib/libgio-2.0.so.0 (0x00007f0b2586b000)
        libjson-glib-1.0.so.0 => /usr/lib/libjson-glib-1.0.so.0 (0x00007f0b25841000)
        libgssapi_krb5.so.2 => /usr/lib/libgssapi_krb5.so.2 (0x00007f0b257ef000)
        libxml2.so.2 => /usr/lib/libxml2.so.2 (0x00007f0b25687000)
        libsqlite3.so.0 => /usr/lib/libsqlite3.so.0 (0x00007f0b2555c000)
        libpsl.so.5 => /usr/lib/libpsl.so.5 (0x00007f0b25549000)
        /lib64/ld-linux-x86-64.so.2 => /usr/lib64/ld-linux-x86-64.so.2 (0x00007f0b25e13000)
        libpcre.so.1 => /usr/lib/libpcre.so.1 (0x00007f0b254d6000)
        libpthread.so.0 => /usr/lib/libpthread.so.0 (0x00007f0b254b5000)
        libffi.so.6 => /usr/lib/libffi.so.6 (0x00007f0b254a8000)
        libgmodule-2.0.so.0 => /usr/lib/libgmodule-2.0.so.0 (0x00007f0b254a3000)
        libz.so.1 => /usr/lib/libz.so.1 (0x00007f0b2528c000)
        libmount.so.1 => /usr/lib/libmount.so.1 (0x00007f0b2522c000)
        libresolv.so.2 => /usr/lib/libresolv.so.2 (0x00007f0b25213000)
        libkrb5.so.3 => /usr/lib/libkrb5.so.3 (0x00007f0b25125000)
        libk5crypto.so.3 => /usr/lib/libk5crypto.so.3 (0x00007f0b250ee000)
        libcom_err.so.2 => /usr/lib/libcom_err.so.2 (0x00007f0b250e8000)
        libkrb5support.so.0 => /usr/lib/libkrb5support.so.0 (0x00007f0b250d8000)
        libdl.so.2 => /usr/lib/libdl.so.2 (0x00007f0b250d3000)
        libkeyutils.so.1 => /usr/lib/libkeyutils.so.1 (0x00007f0b250cc000)
        libicuuc.so.64 => /usr/lib/libicuuc.so.64 (0x00007f0b24ef4000)
        liblzma.so.5 => /usr/lib/liblzma.so.5 (0x00007f0b24ccc000)
        libm.so.6 => /usr/lib/libm.so.6 (0x00007f0b24b86000)
        libunistring.so.2 => /usr/lib/libunistring.so.2 (0x00007f0b24806000)
        libidn2.so.0 => /usr/lib/libidn2.so.0 (0x00007f0b247e7000)
        libblkid.so.1 => /usr/lib/libblkid.so.1 (0x00007f0b24791000)
        librt.so.1 => /usr/lib/librt.so.1 (0x00007f0b24786000)
        libicudata.so.64 => /usr/lib/libicudata.so.64 (0x00007f0b22d40000)
        libstdc++.so.6 => /usr/lib/libstdc++.so.6 (0x00007f0b22b58000)
        libgcc_s.so.1 => /usr/lib/libgcc_s.so.1 (0x00007f0b22b3e000)
```

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

OpenBSD/amd64 | Ubuntu Server LTS x86-64 | Arch Linux:

```
$ ./src/c/libsoup/dnsresolvd 8765
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
<div>ipv6.google.com 2a00:1450:401b:804::200e IPv6</div>
</body>
</html>

=== 200
=== text/html; charset=UTF-8
```
