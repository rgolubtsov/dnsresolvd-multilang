# DNS Resolver Daemon written in JavaScript

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [Node.js](https://nodejs.org "Node.js") event-driven runtime environment)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.3](#building-under-openbsdamd64-63)
  * [Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)](#building-under-ubuntu-server-ubuntu-16044-lts-x86-64)
  * [Building under Arch Linux (kernel 4.15.10-1-ARCH x86-64)](#building-under-arch-linux-kernel-41510-1-arch-x86-64)
* **[Running](#running)**

## Building

This daemon implementation might be built and run successfully on OpenBSD, Ubuntu Server, and Arch Linux operating systems. So let's describe each build process sequentially.

### Building under OpenBSD/amd64 6.3

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

The daemon itself doesn't need to be built; simply check it out in the filesystem... just for fun:))

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

### Building under Ubuntu Server (Ubuntu 16.04.4 LTS x86-64)

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

The daemon itself doesn't need to be built; simply check it out in the filesystem... just for fun:))

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

### Building under Arch Linux (kernel 4.15.10-1-ARCH x86-64)

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

The daemon itself doesn't need to be built; simply check it out in the filesystem... just for fun:))

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

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

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
