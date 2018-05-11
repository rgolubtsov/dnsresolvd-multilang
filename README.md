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

#### Building under OpenBSD/amd64 6.3

**Dependencies:** The only build and runtime dependency is the main library &ndash; **GNU libmicrohttpd**. ~~Since OpenBSD doesn't have it neither in packages nor in ports, it needs to build it from source.~~ In OpenBSD 6.3 it is prebuilt as a package &ndash; `libmicrohttpd-0.9.59.tgz` can be installed as usual: `$ sudo pkg_add -vvvvv libmicrohttpd`. Assuming the compiler GCC 4.9.4 is installed (preferably from packages: `$ sudo pkg_add -vvvvv gcc`)~~, the build process is straightforward, just as stated in the docs (download, unpack, configure, build, install):~~

(Oh! Prior to this it needs the **GNU make** package has to be installed: `$ sudo pkg_add -vvvvv gmake`.)

Now the daemon might be built.

```
$ cd src/c
$ gmake clean && gmake all
rm -f dnsresolvd dnsresolvd.o
egcc -Wall -pedantic -std=c11 -O3 -march=x86-64 -mtune=generic -pipe -fstack-protector-strong -D_DEFAULT_SOURCE   -c -o dnsresolvd.o dnsresolvd.c
egcc   dnsresolvd.o  -lmicrohttpd -o dnsresolvd
dnsresolvd.o: In function `_request_handler':
dnsresolvd.c:(.text+0x754): warning: sprintf() is often misused, please use snprintf()
dnsresolvd.c:(.text+0x3ed): warning: strcat() is almost always misused, please use strlcat()
dnsresolvd.o: In function `_query_params_iterator':
dnsresolvd.c:(.text+0xe4): warning: strcpy() is almost always misused, please use strlcpy()
/usr/local/lib/gcc/x86_64-unknown-openbsd6.3/4.9.4/../../../libunistring.so.0.1: warning: stpcpy() is dangerous; do not use it
/usr/local/lib/gcc/x86_64-unknown-openbsd6.3/4.9.4/../../../libgmp.so.10.0: warning: vsprintf() is often misused, please use vsnprintf()
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 132
drwxr-xr-x  2 <username>  <usergroup>    512 May 12 02:00 .
drwxr-xr-x  7 <username>  <usergroup>    512 May  8 22:20 ..
-rw-r--r--  1 <username>  <usergroup>   1626 May 12 02:00 Makefile
-rwxr-xr-x  1 <username>  <usergroup>  16954 May 12 02:00 dnsresolvd
-rw-r--r--  1 <username>  <usergroup>  24317 May 12 02:00 dnsresolvd.c
-rw-r--r--  1 <username>  <usergroup>   3690 May 12 02:00 dnsresolvd.h
-rw-r--r--  1 <username>  <usergroup>  13864 May 12 02:00 dnsresolvd.o
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB shared object, x86-64, version 1
```

#### Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)

**Dependencies:** The only build and runtime dependency is the main library &ndash; **GNU libmicrohttpd**. It has to be installed from packages:

```
$ sudo apt-get update && sudo apt-get install libmicrohttpd-dev -y
```

This package contains all the development stuff and is actually depends on the package `libmicrohttpd10` which provides runtime library (.so). The latter will be installed alongside automatically. The version of the GNU libmicrohttpd library is 0.9.44. (Also it needs to have the package `build-essential` installed, ensuring that the compiler GCC (version 5.4.0 in this Ubuntu release), GNU make, and all the required standard libs are always at hands.)

Now let's build the daemon.

```
$ cd src/c
$ make clean && make all
rm -f dnsresolvd dnsresolvd.o
cc -Wall -pedantic -std=c11 -O3 -march=x86-64 -mtune=generic -pipe -fstack-protector-strong -D_DEFAULT_SOURCE -I/usr/local/include   -c -o dnsresolvd.o dnsresolvd.c
cc -L/usr/local/lib  dnsresolvd.o  -lmicrohttpd -o dnsresolvd
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 76
drwxr-xr-x 2 <username> <usergroup>  4096 May 11 19:45 .
drwxr-xr-x 7 <username> <usergroup>  4096 May 11 19:40 ..
-rwxr-xr-x 1 <username> <usergroup> 18896 May 11 19:45 dnsresolvd
-rw-r--r-- 1 <username> <usergroup> 24317 May 11 19:40 dnsresolvd.c
-rw-r--r-- 1 <username> <usergroup>  3690 May 11 19:40 dnsresolvd.h
-rw-r--r-- 1 <username> <usergroup> 12632 May 11 19:45 dnsresolvd.o
-rw-r--r-- 1 <username> <usergroup>  1626 May 11 19:40 Makefile
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, for GNU/Linux 2.6.32, BuildID[sha1]=4598dfb6e593925e7abb04ffe32e1608534d868a, not stripped
```

#### Building under Arch Linux (kernel 4.15.10-1-ARCH x86-64)

This is quite equal to the process of building the daemon under Ubuntu. Install the necessary dependencies:

```
$ sudo pacman -Syu
$ sudo pacman -S make
$ sudo pacman -S gcc    (or $ sudo pacman -S gcc-multilib)
```
```
$ sudo pacman -S libmicrohttpd
```

The version of the compiler GCC used is 7.3.0. The version of the library GNU libmicrohttpd used is 0.9.59.

Now let's build the daemon.

```
$ cd src/c
$ make clean && make all
rm -f dnsresolvd dnsresolvd.o
cc -Wall -pedantic -std=c11 -O3 -march=x86-64 -mtune=generic -pipe -fstack-protector-strong -D_DEFAULT_SOURCE -I/usr/local/include   -c -o dnsresolvd.o dnsresolvd.c
dnsresolvd.c: In function '_request_handler':
dnsresolvd.c:371:27: warning: '%u' directive writing between 1 and 5 bytes into a region of size 2 [-Wformat-overflow=]
         sprintf(ver_str, "%u", ver);
                           ^~
dnsresolvd.c:371:26: note: directive argument in the range [0, 65535]
         sprintf(ver_str, "%u", ver);
                          ^~~~
dnsresolvd.c:371:9: note: 'sprintf' output between 2 and 6 bytes into a destination of size 2
         sprintf(ver_str, "%u", ver);
         ^~~~~~~~~~~~~~~~~~~~~~~~~~~
cc -L/usr/local/lib  dnsresolvd.o  -lmicrohttpd -o dnsresolvd
```

Once this is done, check it out... just for fun:))

```
$ ls -al
total 76
drwxr-xr-x 2 <username> <usergroup>  4096 May 11 19:30 .
drwxr-xr-x 7 <username> <usergroup>  4096 Nov 10 11:20 ..
-rwxr-xr-x 1 <username> <usergroup> 19912 May 11 19:30 dnsresolvd
-rw-r--r-- 1 <username> <usergroup> 24317 May 11 19:25 dnsresolvd.c
-rw-r--r-- 1 <username> <usergroup>  3690 May 11 19:25 dnsresolvd.h
-rw-r--r-- 1 <username> <usergroup> 13816 May 11 19:30 dnsresolvd.o
-rw-r--r-- 1 <username> <usergroup>  1626 May 11 19:25 Makefile
$
$ file dnsresolvd
dnsresolvd: ELF 64-bit LSB shared object, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, for GNU/Linux 3.2.0, BuildID[sha1]=490829387497d1cb7c326b0499670fa694de3868, not stripped
```

### JavaScript (Node.js)

#### Building under Ubuntu Server (Ubuntu 16.04.3 LTS x86-64)

```
$ sudo apt-get update && sudo apt-get install -y nodejs npm
$
$ sudo ln -sfnv /usr/bin/nodejs /usr/local/bin/node
'/usr/local/bin/node' -> '/usr/bin/nodejs'
```
```
$ sudo npm i posix -g
$
$ npm ln posix
/home/<username>/dev/misc/github/dnsresolvd-multilang/src/js/node_modules/posix -> /usr/local/lib/node_modules/posix
```
```
$ node -v
v4.2.6
```

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
