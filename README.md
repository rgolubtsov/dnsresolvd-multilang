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
* :cd: **Vala ([libsoup](https://valadoc.org/libsoup-2.4/index.html "libsoup"))**: `src/vala/`
* :small_orange_diamond: **Genie ([libsoup](https://valadoc.org/libsoup-2.4/index.html "libsoup"))**: `src/genie/`

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
$ sudo pacman -Sy make gcc    (or $ sudo pacman -S make gcc-multilib)
```

```
$ sudo pacman -Sy libmicrohttpd
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

#### Building under OpenBSD/amd64 6.3

Install the necessary dependencies (`node`, `posix`):

```
$ sudo pkg_add -vvvvv node
```

```
$ node -v
v8.9.4
```

```
$ sudo npm i posix -g --unsafe-perm

> posix@4.1.2 install /usr/local/lib/node_modules/posix
> node-gyp rebuild

gmake: Entering directory '/usr/local/lib/node_modules/posix/build'
  /usr/bin/clang++ '-DNODE_GYP_MODULE_NAME=posix' '-DUSING_UV_SHARED=1'        \
                   '-DUSING_V8_SHARED=1' '-DV8_DEPRECATION_WARNINGS=1'         \
                   '-D_LARGEFILE_SOURCE' '-D_FILE_OFFSET_BITS=64'              \
                   '-DBUILDING_NODE_EXTENSION'                                 \
                    -I/root/.node-gyp/8.9.4/include/node                       \
                    -I/root/.node-gyp/8.9.4/src                                \
                    -I/root/.node-gyp/8.9.4/deps/uv/include                    \
                    -I/root/.node-gyp/8.9.4/deps/v8/include                    \
                    -I../node_modules/nan -fPIC -pthread -Wall -Wextra         \
                    -Wno-unused-parameter -m64 -O3 -fno-omit-frame-pointer     \
                    -fno-rtti -fno-exceptions -std=gnu++0x -MMD -MF            \
                    ./Release/.deps/Release/obj.target/posix/src/posix.o.d.raw \
                    -c -o Release/obj.target/posix/src/posix.o ../src/posix.cc
  /usr/bin/clang++  -shared -pthread -rdynamic -m64 -Wl,-z,wxneeded            \
                    -Wl,-soname=posix.node -o Release/obj.target/posix.node    \
                    -Wl,--start-group Release/obj.target/posix/src/posix.o     \
                    -Wl,--end-group
  rm -rf "Release/posix.node" && cp -pPRf "Release/obj.target/posix.node"      \
         "Release/posix.node"
gmake: Leaving directory '/usr/local/lib/node_modules/posix/build'
+ posix@4.1.2
```

Once this is done, check it out... just for fun:))

```
$ cd src/js
$ ls -al
total 48
drwxr-xr-x  2 <username>  <usergroup>    512 May  8 22:20 .
drwxr-xr-x  7 <username>  <usergroup>    512 May  8 22:20 ..
-rw-r--r--  1 <username>  <usergroup>   4880 May  8 22:20 dnsresolvd.h
-rwxr-xr-x  1 <username>  <usergroup>  12341 May  8 22:20 dnsresolvd.js
$
$ file dnsresolvd.js
dnsresolvd.js: a node script text executable
$
$ cd ../..
$ NODE_PATH=/usr/local/lib/node_modules ./src/js/dnsresolvd.js 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

#### Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)

Install the necessary dependencies (`nodejs`, `npm`, `posix`):

```
$ sudo apt-get update && sudo apt-get install nodejs npm -y
$
$ sudo ln -sfnv /usr/bin/nodejs /usr/local/bin/node
'/usr/local/bin/node' -> '/usr/bin/nodejs'
```

```
$ node -v
v4.2.6
```

```
$ sudo npm i posix -g

> posix@4.1.2 install /usr/local/lib/node_modules/posix
> node-gyp rebuild

make: Entering directory '/usr/local/lib/node_modules/posix/build'
  CXX(target) Release/obj.target/posix/src/posix.o
  SOLINK_MODULE(target) Release/obj.target/posix.node
  COPY Release/posix.node
make: Leaving directory '/usr/local/lib/node_modules/posix/build'
/usr/local/lib
└── posix@4.1.2
```

Once this is done, check it out... just for fun:))

```
$ cd src/js
$ ls -al
total 32
drwxrwxr-x 2 <username> <usergroup>  4096 May 15 23:03 .
drwxrwxr-x 7 <username> <usergroup>  4096 Nov  3  2017 ..
-rw-rw-r-- 1 <username> <usergroup>  4880 Mar 29 15:33 dnsresolvd.h
-rwxrwxr-x 1 <username> <usergroup> 12341 Mar 29 15:33 dnsresolvd.js
$
$ file dnsresolvd.js
dnsresolvd.js: a /usr/bin/env node script, ASCII text executable
$
$ cd ../..
$ NODE_PATH=/usr/local/lib/node_modules ./src/js/dnsresolvd.js 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

#### Building under Arch Linux (kernel 4.15.10-1-ARCH x86-64)

Install the necessary dependencies (`nodejs`, `npm`, `posix`):

```
$ sudo pacman -Sy nodejs npm
```

```
$ node -v
v9.8.0
```

```
$ sudo npm i posix -g --unsafe-perm

> posix@4.1.2 install /usr/lib/node_modules/posix
> node-gyp rebuild

make: Entering directory '/usr/lib/node_modules/posix/build'
  CXX(target) Release/obj.target/posix/src/posix.o
  SOLINK_MODULE(target) Release/obj.target/posix.node
  COPY Release/posix.node
make: Leaving directory '/usr/lib/node_modules/posix/build'
+ posix@4.1.2
```

Once this is done, check it out... just for fun:))

```
$ cd src/js
$ ls -al
total 32
drwxr-xr-x 2 <username> <usergroup>  4096 May  4 12:13 .
drwxr-xr-x 7 <username> <usergroup>  4096 Nov 10  2017 ..
-rw-r--r-- 1 <username> <usergroup>  4880 May  4 12:13 dnsresolvd.h
-rwxr-xr-x 1 <username> <usergroup> 12341 May  4 12:13 dnsresolvd.js
$
$ file dnsresolvd.js
dnsresolvd.js: a /usr/bin/env node script, ASCII text executable
$
$ cd ../..
$ NODE_PATH=/usr/lib/node_modules ./src/js/dnsresolvd.js 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

### Lua (Luvit)

#### Building/installing dependencies

**TODO:** Describe the daemon's dependencies' build/install process under OpenBSD, Ubuntu Server, and Arch Linux.

### Perl 5 (Mojolicious)

#### Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)

```
$ sudo apt-get update && sudo apt-get install cpanminus -y && sudo cpanm App::cpanminus
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

#### Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)

```
$ sudo apt-get update && sudo apt-get install python3-twisted python3-dnspython -y
```

**TODO:** Describe the daemon's dependencies' build/install process under OpenBSD, Ubuntu Server, and Arch Linux.
