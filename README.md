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

## Building

Every daemon implementation has its own build rules, so let's describe them sequentially.

### C (GNU libmicrohttpd)

#### Building under OpenBSD/amd64 6.1

**Dependencies:** The only build and runtime dependency is the main library &ndash; **GNU libmicrohttpd**. Since OpenBSD doesn't have it neither in packages nor in ports, it needs to build it from source. Assuming the compiler GCC 4.9.4 is installed (preferably from packages: `$ sudo pkg_add -vvvvv gcc`), the build process is straightforward, just as stated in the docs (download, unpack, configure, build, install):

(Oh! Prior to this it needs the **GNU make** package has to be installed: `$ sudo pkg_add -vvvvv gmake`.)

```
$ curl -O http://ftp.gnu.org/gnu/libmicrohttpd/libmicrohttpd-0.9.55.tar.gz
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 1277k  100 1277k    0     0   425k      0  0:00:03  0:00:03 --:--:--  425k
```

```
$ tar -xzf libmicrohttpd-0.9.55.tar.gz
$ cd libmicrohttpd-0.9.55
```

```
$ ./configure
$ gmake
$ sudo gmake install
```

Now the daemon might be built.

```
$ cd src/c
$ gmake clean && gmake all
rm -f dnsresolvd dnsresolvd.o
cc -Wall -pedantic -std=c11 -O3 -march=x86-64 -mtune=generic -pipe -fstack-protector-strong   -c -o dnsresolvd.o dnsresolvd.c
dnsresolvd.c: In function '_query_params_iterator':
dnsresolvd.c:51:26: warning: assignment discards 'const' qualifier from pointer target type
                 hostname = value;
                          ^
dnsresolvd.c: In function 'dns_lookup':
dnsresolvd.c:240:18: warning: assignment discards 'const' qualifier from pointer target type
             addr = inet_ntop(AF_INET6, hent->h_addr_list[0], addr,
                  ^
dnsresolvd.c:246:14: warning: assignment discards 'const' qualifier from pointer target type
         addr = inet_ntop(AF_INET, hent->h_addr_list[0], addr,
              ^
cc   dnsresolvd.o  -lmicrohttpd -o dnsresolvd
dnsresolvd.o: In function `_request_handler':
dnsresolvd.c:(.text+0x1aa): warning: warning: strcat() is almost always misused, please use strlcat()
dnsresolvd.c:(.text+0x34a): warning: warning: sprintf() is often misused, please use snprintf()
/usr/local/lib/gcc/x86_64-unknown-openbsd6.1/4.9.4/../../../libmicrohttpd.so.55.0: warning: warning: strcpy() is almost always misused, please use strlcpy()
/usr/local/lib/gcc/x86_64-unknown-openbsd6.1/4.9.4/../../../libgcrypt.so.19.3: warning: warning: stpcpy() is dangerous; do not use it
/usr/local/lib/gcc/x86_64-unknown-openbsd6.1/4.9.4/../../../libgmp.so.10.0: warning: warning: vsprintf() is often misused, please use vsnprintf()
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 108
drwxr-xr-x  2 radic  radic    512 Jun 22 00:47 .
drwxr-xr-x  4 radic  radic    512 Jun 10 21:57 ..
-rw-r--r--  1 radic  radic   1038 Jun 13 02:55 Makefile
-rwxr-xr-x  1 radic  radic  16731 Jun 22 00:47 dnsresolvd
-rw-r--r--  1 radic  radic  12645 Jun 22 00:45 dnsresolvd.c
-rw-r--r--  1 radic  radic   3267 Jun 22 00:45 dnsresolvd.h
-rw-r--r--  1 radic  radic  10584 Jun 22 00:47 dnsresolvd.o
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB shared object, x86-64, version 1
```

#### Building under Ubuntu Server (Ubuntu 16.04.3 LTS x86-64)

**Dependencies:** The only build and runtime dependency is the main library &ndash; **GNU libmicrohttpd**. It has to be installed from packages:

```
$ sudo apt-get update
$ sudo apt-get install libmicrohttpd-dev
```

This package contains all the development stuff and is actually depends on the package `libmicrohttpd10` which provides runtime library (.so). The latter will be installed alongside automatically. The version of the GNU libmicrohttpd library is 0.9.44. (Also it needs to have the package `build-essential` installed, ensuring that the compiler GCC (version 5.4.0 in this Ubuntu release), GNU make, and all the required standard libs are always at hands.)

Now let's build the daemon.

```
$ cd src/c
$ make clean && make all
rm -f dnsresolvd dnsresolvd.o
cc -Wall -pedantic -std=c11 -O3 -march=x86-64 -mtune=generic -pipe -fstack-protector-strong -D_DEFAULT_SOURCE   -c -o dnsresolvd.o dnsresolvd.c
dnsresolvd.c: In function ‘_query_params_iterator’:
dnsresolvd.c:53:26: warning: assignment discards ‘const’ qualifier from pointer target type [-Wdiscarded-qualifiers]
                 hostname = value;
                          ^
dnsresolvd.c: In function ‘dns_lookup’:
dnsresolvd.c:242:18: warning: assignment discards ‘const’ qualifier from pointer target type [-Wdiscarded-qualifiers]
             addr = inet_ntop(AF_INET6, hent->h_addr_list[0], addr,
                  ^
dnsresolvd.c:248:14: warning: assignment discards ‘const’ qualifier from pointer target type [-Wdiscarded-qualifiers]
         addr = inet_ntop(AF_INET, hent->h_addr_list[0], addr,
              ^
cc   dnsresolvd.o  -lmicrohttpd -o dnsresolvd
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 64
drwxrwxr-x 2 radic radic  4096 Dec 18 18:50 .
drwxrwxr-x 7 radic radic  4096 Nov  3 02:55 ..
-rwxrwxr-x 1 radic radic 18456 Dec 18 18:50 dnsresolvd
-rw-rw-r-- 1 radic radic 13288 Oct 14 08:50 dnsresolvd.c
-rw-rw-r-- 1 radic radic  3379 Oct 14 08:50 dnsresolvd.h
-rw-rw-r-- 1 radic radic  9488 Dec 18 18:50 dnsresolvd.o
-rw-rw-r-- 1 radic radic  1381 Dec 18 18:43 Makefile
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, for GNU/Linux 2.6.32, BuildID[sha1]=624598826a0b72c42479fa6bfaa18a12a5716358, not stripped
```

#### Building under Arch Linux (kernel 4.13.9-1-ARCH x86-64)

This is quite equal to the process of building the daemon under Ubuntu. Install the necessary dependencies:

```
$ sudo pacman -Syu
$ sudo pacman -S make
$ sudo pacman -S gcc    (or $ sudo pacman -S gcc-multilib)
```

```
$ sudo pacman -S libmicrohttpd
```

The version of the compiler GCC used is 7.2.0. The version of the library GNU libmicrohttpd used is 0.9.58.

Now let's build the daemon.

```
$ cd src/c
$ make clean && make all
rm -f dnsresolvd dnsresolvd.o
cc -Wall -pedantic -std=c11 -O3 -march=x86-64 -mtune=generic -pipe -fstack-protector-strong -D_DEFAULT_SOURCE   -c -o dnsresolvd.o dnsresolvd.c
dnsresolvd.c: In function ‘_query_params_iterator’:
dnsresolvd.c:53:26: warning: assignment discards ‘const’ qualifier from pointer target type [-Wdiscarded-qualifiers]
                 hostname = value;
                          ^
dnsresolvd.c: In function ‘dns_lookup’:
dnsresolvd.c:242:18: warning: assignment discards ‘const’ qualifier from pointer target type [-Wdiscarded-qualifiers]
             addr = inet_ntop(AF_INET6, hent->h_addr_list[0], addr,
                  ^
dnsresolvd.c:248:14: warning: assignment discards ‘const’ qualifier from pointer target type [-Wdiscarded-qualifiers]
         addr = inet_ntop(AF_INET, hent->h_addr_list[0], addr,
              ^
dnsresolvd.c: In function ‘_request_handler’:
dnsresolvd.c:140:27: warning: ‘%u’ directive writing between 1 and 5 bytes into a region of size 2 [-Wformat-overflow=]
         sprintf(ver_str, "%u", ver);
                           ^~
dnsresolvd.c:140:26: note: directive argument in the range [0, 65535]
         sprintf(ver_str, "%u", ver);
                          ^~~~
dnsresolvd.c:140:9: note: ‘sprintf’ output between 2 and 6 bytes into a destination of size 2
         sprintf(ver_str, "%u", ver);
         ^~~~~~~~~~~~~~~~~~~~~~~~~~~
cc   dnsresolvd.o  -lmicrohttpd -o dnsresolvd
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 64
drwxr-xr-x 2 radic radic  4096 Dec 18 19:45 .
drwxr-xr-x 7 radic radic  4096 Nov 10 19:20 ..
-rwxr-xr-x 1 radic radic 18152 Dec 18 19:45 dnsresolvd
-rw-r--r-- 1 radic radic 13288 Oct 14 23:15 dnsresolvd.c
-rw-r--r-- 1 radic radic  3379 Oct 14 23:15 dnsresolvd.h
-rw-r--r-- 1 radic radic 10328 Dec 18 19:45 dnsresolvd.o
-rw-r--r-- 1 radic radic  1382 Dec 18 19:40 Makefile
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB shared object, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, for GNU/Linux 3.2.0, BuildID[sha1]=ec3612f676b40e5114fc5bd92fbf04868a5dc9ab, not stripped
```

### JavaScript (Node.js)

#### Building/installing dependencies

**TODO:** Describe the daemon's dependencies' build/install process under OpenBSD, Ubuntu Server, and Arch Linux.

### Lua (Luvit)

#### Building/installing dependencies

**TODO:** Describe the daemon's dependencies' build/install process under OpenBSD, Ubuntu Server, and Arch Linux.

### Perl 5 (Mojolicious)

#### Building under Ubuntu Server (Ubuntu 16.04.3 LTS x86-64)

```
$ sudo apt-get update && sudo apt-get install -y cpanminus && sudo cpanm App::cpanminus
```
```
$ sudo cpanm Mojolicious Net::DNS::Native
```
```
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

**TODO:** Describe the daemon's dependencies' build/install process under OpenBSD, Ubuntu Server, and Arch Linux.

### Python 3 (Twisted)

#### Building under Ubuntu Server (Ubuntu 16.04.3 LTS x86-64)

```
$ sudo apt-get update && sudo apt-get install -y python3-twisted python3-dnspython
```

**TODO:** Describe the daemon's dependencies' build/install process under OpenBSD, Ubuntu Server, and Arch Linux.
