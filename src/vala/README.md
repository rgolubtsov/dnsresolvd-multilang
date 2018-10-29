# DNS Resolver Daemon written in Vala

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [GNOME libsoup](https://valadoc.org/libsoup-2.4/index.html "GNOME libsoup") library)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.3](#building-under-openbsdamd64-63)
  * [Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)](#building-under-ubuntu-server-ubuntu-16044-lts-x86-64)
  * [Building under Arch Linux (kernel 4.16.13-2-ARCH x86-64)](#building-under-arch-linux-kernel-41613-2-arch-x86-64)
* **[Running](#running)**

## Building

This daemon implementation is known to be built and run successfully on OpenBSD, Ubuntu Server, and Arch Linux operating systems. So let's describe each build process sequentially.

### Building under OpenBSD/amd64 6.3

Install the necessary dependencies (`vala`, `libsoup`, `json-glib`):

```
$ sudo pkg_add -vvvvv vala libsoup json-glib
$
$ valac --version
Vala 0.38.8
```

Now the daemon might be built.

```
$ cd src/vala
$ gmake clean && gmake all
rm -f dnsresolvd
valac --target-glib=2.40 --cc=egcc -X -s -X -O3 -X -mtune=generic -X -pipe -X -fstack-protector-strong --pkg=posix --pkg=libsoup-2.4 --pkg=json-glib-1.0           -o dnsresolvd dnsresolvd.vala dnsresolvh.gs
/home/<username>/dnsresolvd-multilang/src/vala/dnsresolvd.vala.c: In function '__lambda6_':
/home/<username>/dnsresolvd-multilang/src/vala/dnsresolvd.vala.c:721:16: warning: assignment discards 'const' qualifier from pointer target type
        _tmp30_ = _tmp29_->data;
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
total 108
drwxr-xr-x  2 <username>  <usergroup>    512 Jul 25 13:20 .
drwxr-xr-x  8 <username>  <usergroup>    512 Jun 23 02:00 ..
-rw-r--r--  1 <username>  <usergroup>   1733 Jul 25 13:20 Makefile
-rwxr-xr-x  1 <username>  <usergroup>  26288 Jul 25 13:20 dnsresolvd
-rw-r--r--  1 <username>  <usergroup>  14614 Jul 25 13:20 dnsresolvd.vala
-rw-r--r--  1 <username>  <usergroup>   6070 Jul 25 13:20 dnsresolvh.gs
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB shared object, x86-64, version 1
$
$ ldd dnsresolvd
dnsresolvd:
        Start            End              Type  Open Ref GrpRef Name
        000000f1fb400000 000000f1fb607000 exe   2    0   0      dnsresolvd
        000000f414831000 000000f414b20000 rlib  0    1   0      /usr/local/lib/libsoup-2.4.so.10.0
        000000f4d1800000 000000f4d1b97000 rlib  0    2   0      /usr/local/lib/libxml2.so.16.1
        000000f4db79b000 000000f4db9c2000 rlib  0    1   0      /usr/local/lib/libjson-glib-1.0.so.6.0
        000000f45fff1000 000000f460398000 rlib  0    3   0      /usr/local/lib/libgio-2.0.so.4200.6
        000000f480e78000 000000f48107c000 rlib  0    3   0      /usr/local/lib/libgmodule-2.0.so.4200.6
        000000f4e0cad000 000000f4e0efc000 rlib  0    4   0      /usr/local/lib/libgobject-2.0.so.4200.6
        000000f4c301d000 000000f4c333c000 rlib  0    6   0      /usr/local/lib/libglib-2.0.so.4200.6
        000000f462445000 000000f462650000 rlib  0    6   0      /usr/local/lib/libintl.so.6.0
        000000f40706f000 000000f4072b4000 rlib  0    6   0      /usr/local/lib/libpcre.so.3.0
        000000f41a04f000 000000f41a258000 rlib  0    9   0      /usr/lib/libpthread.so.25.1
        000000f48cb2f000 000000f48ce0f000 rlib  0    1   0      /usr/lib/libc.so.92.3
        000000f4766ec000 000000f4769e9000 rlib  0    7   0      /usr/local/lib/libiconv.so.6.0
        000000f4df7bc000 000000f4df9c4000 rlib  0    3   0      /usr/local/lib/libffi.so.1.2
        000000f4d5ded000 000000f4d6004000 rlib  0    4   0      /usr/lib/libz.so.5.0
        000000f4b3ac9000 000000f4b3cf0000 rlib  0    2   0      /usr/local/lib/liblzma.so.2.1
        000000f4800fa000 000000f480322000 rlib  0    3   0      /usr/lib/libm.so.10.1
        000000f4001a6000 000000f400522000 rlib  0    1   0      /usr/local/lib/libsqlite3.so.37.2
        000000f4c9200000 000000f4c9200000 ld.so 0    1   0      /usr/libexec/ld.so
```

### Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)

Install the necessary dependencies (`valac`, `libsoup2.4-dev`, `libjson-glib-dev`):

```
$ sudo apt-get update                                           && \
  sudo apt-get install valac libsoup2.4-dev libjson-glib-dev -y
$
$ valac --version
Vala 0.30.1
```

Now the daemon might be built.

```
$ cd src/vala
$ make clean && make all
rm -f dnsresolvd
valac --target-glib=2.40 --cc=cc -X -s -X -O3 -X -mtune=generic -X -pipe -X -fstack-protector-strong --pkg=posix --pkg=libsoup-2.4 --pkg=json-glib-1.0           -o dnsresolvd dnsresolvd.vala dnsresolvh.gs
/home/<username>/dnsresolvd-multilang/src/vala/dnsresolvd.vala.c: In function ‘__lambda6_’:
/home/<username>/dnsresolvd-multilang/src/vala/dnsresolvd.vala.c:728:16: warning: assignment discards ‘const’ qualifier from pointer target type [-Wdiscarded-qualifiers]
        _tmp30_ = _tmp29_->data;
                ^
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 64
drwxr-xr-x 2 <username> <usergroup>  4096 Jul 25 18:10 .
drwxr-xr-x 8 <username> <usergroup>  4096 Jul 25 17:05 ..
-rwxr-xr-x 1 <username> <usergroup> 27440 Jul 25 18:10 dnsresolvd
-rw-r--r-- 1 <username> <usergroup> 14614 Jul 25 18:10 dnsresolvd.vala
-rw-r--r-- 1 <username> <usergroup>  6070 Jul 25 18:10 dnsresolvh.gs
-rw-r--r-- 1 <username> <usergroup>  1733 Jul 25 18:10 Makefile
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, for GNU/Linux 2.6.32, BuildID[sha1]=713cf6062e35b337b340e3628d756fdfc6237c69, stripped
$
$ ldd dnsresolvd
        linux-vdso.so.1 =>  (0x00007ffdcc3f2000)
        libsoup-2.4.so.1 => /usr/lib/x86_64-linux-gnu/libsoup-2.4.so.1 (0x00007f584f217000)
        libjson-glib-1.0.so.0 => /usr/lib/x86_64-linux-gnu/libjson-glib-1.0.so.0 (0x00007f584efef000)
        libgio-2.0.so.0 => /usr/lib/x86_64-linux-gnu/libgio-2.0.so.0 (0x00007f584ec67000)
        libgobject-2.0.so.0 => /usr/lib/x86_64-linux-gnu/libgobject-2.0.so.0 (0x00007f584ea14000)
        libglib-2.0.so.0 => /lib/x86_64-linux-gnu/libglib-2.0.so.0 (0x00007f584e703000)
        libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f584e4e6000)
        libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f584e11c000)
        libxml2.so.2 => /usr/lib/x86_64-linux-gnu/libxml2.so.2 (0x00007f584dd61000)
        libsqlite3.so.0 => /usr/lib/x86_64-linux-gnu/libsqlite3.so.0 (0x00007f584da8c000)
        libgmodule-2.0.so.0 => /usr/lib/x86_64-linux-gnu/libgmodule-2.0.so.0 (0x00007f584d888000)
        libz.so.1 => /lib/x86_64-linux-gnu/libz.so.1 (0x00007f584d66e000)
        libselinux.so.1 => /lib/x86_64-linux-gnu/libselinux.so.1 (0x00007f584d44c000)
        libresolv.so.2 => /lib/x86_64-linux-gnu/libresolv.so.2 (0x00007f584d231000)
        libffi.so.6 => /usr/lib/x86_64-linux-gnu/libffi.so.6 (0x00007f584d029000)
        libpcre.so.3 => /lib/x86_64-linux-gnu/libpcre.so.3 (0x00007f584cdb9000)
        /lib64/ld-linux-x86-64.so.2 (0x00007f584f4ee000)
        libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007f584cbb5000)
        libicuuc.so.55 => /usr/lib/x86_64-linux-gnu/libicuuc.so.55 (0x00007f584c821000)
        liblzma.so.5 => /lib/x86_64-linux-gnu/liblzma.so.5 (0x00007f584c5ff000)
        libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f584c2f6000)
        libicudata.so.55 => /usr/lib/x86_64-linux-gnu/libicudata.so.55 (0x00007f584a83f000)
        libstdc++.so.6 => /usr/lib/x86_64-linux-gnu/libstdc++.so.6 (0x00007f584a4bd000)
        libgcc_s.so.1 => /lib/x86_64-linux-gnu/libgcc_s.so.1 (0x00007f584a2a7000)
```

### Building under Arch Linux (kernel 4.16.13-2-ARCH x86-64)

Install the necessary dependencies (`vala`, `libsoup`, `json-glib`):

```
$ sudo pacman -Sy vala libsoup json-glib
$
$ valac --version
Vala 0.40.6
```

Now the daemon might be built.

```
$ cd src/vala
$ make clean && make all
rm -f dnsresolvd
valac --target-glib=2.40 --cc=cc -X -s -X -O3 -X -mtune=generic -X -pipe -X -fstack-protector-strong --pkg=posix --pkg=libsoup-2.4 --pkg=json-glib-1.0           -o dnsresolvd dnsresolvd.vala dnsresolvh.gs
dnsresolvd.vala:158.21-158.32: warning: Posix.SIGINT has been deprecated since vala-0.40. Use Posix.Signal.INT
dnsresolvd.vala:161.38-161.49: warning: Posix.SIGINT has been deprecated since vala-0.40. Use Posix.Signal.INT
dnsresolvd.vala:164.21-164.33: warning: Posix.SIGTERM has been deprecated since vala-0.40. Use Posix.Signal.TERM
dnsresolvd.vala:167.38-167.50: warning: Posix.SIGTERM has been deprecated since vala-0.40. Use Posix.Signal.TERM
/home/<username>/dnsresolvd-multilang/src/vala/dnsresolvd.vala.c: In function ‘__lambda6_’:
/home/<username>/dnsresolvd-multilang/src/vala/dnsresolvd.vala.c:749:16: warning: assignment discards ‘const’ qualifier from pointer target type [-Wdiscarded-qualifiers]
        _tmp25_ = _tmp24_->data;
                ^
Compilation succeeded - 4 warning(s)
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 64
drwxr-xr-x 2 <username> <usergroup>  4096 Jul 25 14:00 .
drwxr-xr-x 8 <username> <usergroup>  4096 Jun 29 12:40 ..
-rwxr-xr-x 1 <username> <usergroup> 27352 Jul 25 14:00 dnsresolvd
-rw-r--r-- 1 <username> <usergroup> 14614 Jul 25 14:00 dnsresolvd.vala
-rw-r--r-- 1 <username> <usergroup>  6070 Jul 25 14:00 dnsresolvh.gs
-rw-r--r-- 1 <username> <usergroup>  1733 Jul 25 14:00 Makefile
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB pie executable x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, for GNU/Linux 3.2.0, BuildID[sha1]=f445af34724f340d262106e87192748ea0e42237, stripped
$
$ ldd dnsresolvd
        linux-vdso.so.1 (0x00007ffdddfed000)
        libsoup-2.4.so.1 => /usr/lib/libsoup-2.4.so.1 (0x00007fbd23304000)
        libjson-glib-1.0.so.0 => /usr/lib/libjson-glib-1.0.so.0 (0x00007fbd230dd000)
        libgio-2.0.so.0 => /usr/lib/libgio-2.0.so.0 (0x00007fbd22d3d000)
        libgobject-2.0.so.0 => /usr/lib/libgobject-2.0.so.0 (0x00007fbd22ae9000)
        libglib-2.0.so.0 => /usr/lib/libglib-2.0.so.0 (0x00007fbd227d2000)
        libpthread.so.0 => /usr/lib/libpthread.so.0 (0x00007fbd225b4000)
        libc.so.6 => /usr/lib/libc.so.6 (0x00007fbd221f8000)
        libxml2.so.2 => /usr/lib/libxml2.so.2 (0x00007fbd21e92000)
        libsqlite3.so.0 => /usr/lib/libsqlite3.so.0 (0x00007fbd21b7a000)
        libgssapi_krb5.so.2 => /usr/lib/libgssapi_krb5.so.2 (0x00007fbd2192c000)
        libgmodule-2.0.so.0 => /usr/lib/libgmodule-2.0.so.0 (0x00007fbd21728000)
        libz.so.1 => /usr/lib/libz.so.1 (0x00007fbd21511000)
        libresolv.so.2 => /usr/lib/libresolv.so.2 (0x00007fbd212fa000)
        libmount.so.1 => /usr/lib/libmount.so.1 (0x00007fbd210a2000)
        libffi.so.6 => /usr/lib/libffi.so.6 (0x00007fbd20e99000)
        libpcre.so.1 => /usr/lib/libpcre.so.1 (0x00007fbd20c27000)
        /lib64/ld-linux-x86-64.so.2 => /usr/lib64/ld-linux-x86-64.so.2 (0x00007fbd23801000)
        libdl.so.2 => /usr/lib/libdl.so.2 (0x00007fbd20a23000)
        libicuuc.so.61 => /usr/lib/libicuuc.so.61 (0x00007fbd20669000)
        liblzma.so.5 => /usr/lib/liblzma.so.5 (0x00007fbd20443000)
        libm.so.6 => /usr/lib/libm.so.6 (0x00007fbd200ae000)
        libkrb5.so.3 => /usr/lib/libkrb5.so.3 (0x00007fbd1fdc5000)
        libk5crypto.so.3 => /usr/lib/libk5crypto.so.3 (0x00007fbd1fb92000)
        libcom_err.so.2 => /usr/lib/libcom_err.so.2 (0x00007fbd1f98e000)
        libkrb5support.so.0 => /usr/lib/libkrb5support.so.0 (0x00007fbd1f781000)
        libkeyutils.so.1 => /usr/lib/libkeyutils.so.1 (0x00007fbd1f57d000)
        libblkid.so.1 => /usr/lib/libblkid.so.1 (0x00007fbd1f32d000)
        libuuid.so.1 => /usr/lib/libuuid.so.1 (0x00007fbd1f126000)
        librt.so.1 => /usr/lib/librt.so.1 (0x00007fbd1ef1e000)
        libicudata.so.61 => /usr/lib/libicudata.so.61 (0x00007fbd1d379000)
        libstdc++.so.6 => /usr/lib/libstdc++.so.6 (0x00007fbd1cff0000)
        libgcc_s.so.1 => /usr/lib/libgcc_s.so.1 (0x00007fbd1cdd8000)
```

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

OpenBSD/amd64 | Arch Linux:

```
$ ./src/vala/dnsresolvd 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

Example of making **GET** and **POST** requests:

```
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" 'http://localhost:8765/?f=xml&h=valadoc.org'
{"hostname":"valadoc.org","address":"104.131.12.61","version":"IPv4"}
=== 200
=== application/json
$
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" -d 'h=elementary.io&f=json' http://localhost:8765
{"hostname":"elementary.io","address":"104.28.4.44","version":"IPv4"}
=== 200
=== application/json
```
