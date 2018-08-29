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
* :small_blue_diamond: **Vala ([libsoup](https://valadoc.org/libsoup-2.4/index.html "libsoup"))**: `src/vala/`
* :small_blue_diamond: **Genie ([libsoup](https://valadoc.org/libsoup-2.4/index.html "libsoup"))**: `src/genie/`
* :cd: **Elixir ([Cowboy](https://ninenines.eu "Cowboy"))**: `src/elixir/`

---

* [Building](#building)
  * [C (GNU libmicrohttpd)](#c-gnu-libmicrohttpd)
  * [JavaScript (Node.js)](#javascript-nodejs)
  * [Lua (Luvit)](#lua-luvit)
  * [Perl 5 (Mojolicious)](#perl-5-mojolicious)
  * [Python 3 (Twisted)](#python-3-twisted)
  * [Vala (libsoup)](#vala-libsoup)
  * [Genie (libsoup)](#genie-libsoup)
* [Running](#running)
  * [C (GNU libmicrohttpd)](#c-gnu-libmicrohttpd-1)
  * [JavaScript (Node.js)](#javascript-nodejs-1)
  * [Lua (Luvit)](#lua-luvit-1)
  * [Perl 5 (Mojolicious)](#perl-5-mojolicious-1)
  * [Python 3 (Twisted)](#python-3-twisted-1)
  * [Vala (libsoup)](#vala-libsoup-1)
  * [Genie (libsoup)](#genie-libsoup-1)

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
$ sudo apt-get update                       && \
  sudo apt-get install libmicrohttpd-dev -y
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
$
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
$
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
```

#### Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)

Install the necessary dependencies (`nodejs`, `npm`, `posix`):

```
$ sudo apt-get update                && \
  sudo apt-get install nodejs npm -y
$
$ sudo ln -sfnv /usr/bin/nodejs /usr/local/bin/node
'/usr/local/bin/node' -> '/usr/bin/nodejs'
$
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
```

#### Building under Arch Linux (kernel 4.15.10-1-ARCH x86-64)

Install the necessary dependencies (`nodejs`, `npm`, `posix`):

```
$ sudo pacman -Sy nodejs npm
$
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
```

### Lua (Luvit)

#### Building under Arch Linux (kernel 4.16.13-2-ARCH x86-64)

Install the necessary dependencies (`luvi`, `lit`, `luvit`, `luarocks5.1`, `luaposix`). Note that some of these packages are not in the Arch Linux official repositories. Hence they have to be installed from the AUR repository. The AUR helper `yaourt` might be used for that:

```
$ yaourt -S luvi-bin
$ yaourt -S luvit
```

The `lit` package will be built and installed as a dependency to the `luvit` package. And after that it will no be longer required. So it is safe to remove it from the system:

```
$ sudo pacman -Rsc lit
```

The `luaposix` package can be installed from the AUR repository, but for now AUR suggests its outdated version, so a better way is to install it via **LuaRocks**:

```
$ sudo pacman -S luarocks5.1
$
$ sudo luarocks-5.1 install luaposix
```

Since **Luvit** searches modules based on &quot;local&quot; tree (`/usr/local/share/`, `/usr/local/lib/`), it needs to create the following symlinks:

```
$ sudo mkdir    /usr/local/share/lua /usr/local/lib/lua       && \
  sudo ln -sfnv /usr/share/lua/5.1   /usr/local/share/lua/5.1 && \
  sudo ln -sfnv /usr/lib/lua/5.1     /usr/local/lib/lua/5.1
'/usr/local/share/lua/5.1' -> '/usr/share/lua/5.1'
'/usr/local/lib/lua/5.1' -> '/usr/lib/lua/5.1'
$
$ luvit -v
luvit version: 2.14.1
luvi version: v2.7.6
rex version: 8.37 2015-04-28
libuv version: 1.9.1
ssl version: OpenSSL 1.0.2h  3 May 2016, lua-openssl 0.5.1
```

Once this is done, check it out... just for fun:))

```
$ cd src/lua
$ ls -al
total 32
drwxr-xr-x 2 <username> <usergroup>  4096 Aug  9 20:20 .
drwxr-xr-x 9 <username> <usergroup>  4096 Aug  8 19:10 ..
-rwxr-xr-x 1 <username> <usergroup> 13746 Aug  9 20:20 dnsresolvd.lua
-rw-r--r-- 1 <username> <usergroup>  4290 Aug  9 20:20 dnsresolvh.lua
$
$ file dnsresolvd.lua
dnsresolvd.lua: a /usr/bin/env luvit script, ASCII text executable
```

**TODO:** Describe the daemon's dependencies' build/install process under OpenBSD and Ubuntu Server.

### Perl 5 (Mojolicious)

#### Building under OpenBSD/amd64 6.3

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

#### Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)

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

### Python 3 (Twisted)

#### Building under OpenBSD/amd64 6.3

Install the necessary dependencies (`py3-pip`, `twisted`, `py3-dnspython`):

```
$ sudo pkg_add -vvvvv py3-pip py3-dnspython
$
$ sudo ln -sfn /usr/local/bin/pip3.6 /usr/local/bin/pip
```

```
$ sudo pip install twisted
```

#### Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)

Install the necessary dependencies (`python3-twisted`, `python3-dnspython`):

```
$ sudo apt-get update                                       && \
  sudo apt-get install python3-twisted python3-dnspython -y
```

**TODO:** Describe the daemon's dependencies' build/install process under Arch Linux.

### Vala (libsoup)

#### Building under OpenBSD/amd64 6.3

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

#### Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)

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

#### Building under Arch Linux (kernel 4.16.13-2-ARCH x86-64)

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

### Genie (libsoup)

#### Building under OpenBSD/amd64 6.3

All the necessary build-time and run-time dependencies are exactly the same to what is being used for Vala build process &ndash; [see here](https://github.com/rgolubtsov/dnsresolvd-multilang#vala-libsoup "Vala (libsoup) build instructions").

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

The Genie daemon's build processes under Ubuntu Server and Arch Linux are exactly the same to those ones which are being used for Vala on those operating systems.

## Running

Starting the daemon is quite easy and very similar for all its implementations.

### C (GNU libmicrohttpd)

OpenBSD/amd64 | Arch Linux:

```
$ ./src/c/dnsresolvd 8765
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
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" -d 'h=openports.se&f=xml' http://localhost:8765
{"hostname":"openbsd.org","address":"129.128.5.194","version":"IPv4"}
=== 200
=== application/json
```

### JavaScript (Node.js)

OpenBSD/amd64:

```
$ ./src/js/dnsresolvd.js 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

Arch Linux:

```
$ NODE_PATH=/usr/lib/node_modules ./src/js/dnsresolvd.js 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

Example of making **GET** and **POST** requests:

```
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" 'http://localhost:8765/?h=cybernode.com&f=xml'
{"hostname":"cybernode.com","address":"64.62.244.231","version":"IPv4"}
=== 200
=== application/json
$
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" -d 'h=ipv6.cybernode.com&f=xml' http://localhost:8765
{"hostname":"ipv6.cybernode.com","address":"2001:470:1:1b9::31","version":"IPv6"}
=== 200
=== application/json
```

### Lua (Luvit)

Arch Linux:

```
$ cd src/lua
$ ./dnsresolvd.lua 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

Example of making **GET** and **POST** requests:

```
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" 'http://localhost:8765/?h=ipv6.testvitesse.videotron.ca&f=xml'
{"address":"2607:fa48:2:30ff:0:0:0:a","hostname":"ipv6.testvitesse.videotron.ca","version":"IPv6"}
=== 200
=== application/json
$
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" -d 'f=xml&h=testvitesse.videotron.ca' http://localhost:8765
{"address":"135.19.0.17","hostname":"testvitesse.videotron.ca","version":"IPv4"}
=== 200
=== application/json
```

### Perl 5 (Mojolicious)

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

### Python 3 (Twisted)

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

### Vala (libsoup)

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

### Genie (libsoup)

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

---

:cd:
