# DNS Resolver Daemon written in Lua

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [Luvit](https://luvit.io "Luvit") event-driven runtime environment)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under Arch Linux (kernel 4.16.13-2-ARCH x86-64)](#building-under-arch-linux-kernel-41613-2-arch-x86-64)
* **[Running](#running)**

## Building

This daemon implementation might be built and run successfully on ~~OpenBSD, Ubuntu Server, and~~ Arch Linux operating systems. So let's describe each build process sequentially.

### Building under Arch Linux (kernel 4.16.13-2-ARCH x86-64)

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

The daemon itself doesn't need to be built; simply check it out in the filesystem... just for fun:))

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

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

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
