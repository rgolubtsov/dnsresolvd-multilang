# DNS Resolver Daemon written in Clojure

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [HTTP Kit](http://http-kit.org "HTTP Kit") HTTP client/server library)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under Arch Linux (kernel 4.20.3-arch1-1-ARCH x86-64)](#building-under-arch-linux-kernel-4203-arch1-1-arch-x86-64)
* **[Running](#running)**

## Building

This daemon implementation is known to be built and run successfully on ~~OpenBSD, Ubuntu Server, and~~ Arch Linux operating systems. So let's describe each build process sequentially.

### Building under Arch Linux (kernel 4.20.3-arch1-1-ARCH x86-64)

Install the necessary dependencies (`clojure`, `maven`, `syslog4j`, `jna`). Note that the `jdk8-openjdk` package will be installed automatically as a dependency to the `clojure` package:

```
$ sudo pacman -Sy clojure maven
$
$ clojure -e '(clojure-version)'
"1.10.0"
```

The `syslog4j` and `jna` packages (as JARs) have to be installed via **Apache Maven**. The following compound one-liner script will actually do the job:

```
$ mvn dependency:get -Dartifact=org.graylog2:syslog4j:0.9.60 && \
  mvn dependency:get -Dartifact=net.java.dev.jna:jna:5.2.0
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------< org.apache.maven:standalone-pom >-------------------
[INFO] Building Maven Stub Project (No POM) 1
[INFO] --------------------------------[ pom ]---------------------------------
[INFO]
[INFO] --- maven-dependency-plugin:2.8:get (default-cli) @ standalone-pom ---
[INFO] Resolving org.graylog2:syslog4j:jar:0.9.60 with transitive dependencies
...
Downloading from central: https://repo.maven.apache.org/maven2/org/graylog2/syslog4j/0.9.60/syslog4j-0.9.60.jar
...
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  X.XXX s
[INFO] Finished at: XXXX-XX-XXTXX:XX:XX+XX:XX
[INFO] ------------------------------------------------------------------------
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------< org.apache.maven:standalone-pom >-------------------
[INFO] Building Maven Stub Project (No POM) 1
[INFO] --------------------------------[ pom ]---------------------------------
[INFO]
[INFO] --- maven-dependency-plugin:2.8:get (default-cli) @ standalone-pom ---
[INFO] Resolving net.java.dev.jna:jna:jar:5.2.0 with transitive dependencies
...
Downloading from central: https://repo.maven.apache.org/maven2/net/java/dev/jna/jna/5.2.0/jna-5.2.0.jar
...
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  X.XXX s
[INFO] Finished at: XXXX-XX-XXTXX:XX:XX+XX:XX
[INFO] ------------------------------------------------------------------------
```

Now the daemon might be built.

```
$ cd src/clojure
$ make clean && make all
rm -f -vR lib
removed directory 'lib'
if [ ! -d "lib" ]; then                                             \
        mkdir lib;                                               \
#               for ns in ; do                                           \
#                       clojure -e "(binding [*compile-path* \"lib\"] \
#                                         (println  *compile-path*)            \
#                                         (compile (symbol \"$ns\")))";       \
#               done                                                           \
fi
```

**TODO:** Update the build process described above when ahead-of-time (AOT) compilation will be in effect. :point_up::point_down:

Once this is done, check it out... just for fun:))

```
$ ls -al . src lib
...
$
$ file dnsresolvd src/* lib/*
...
```

**TODO:** Describe the daemon's dependencies' build/install process under OpenBSD and Ubuntu Server.

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

Arch Linux / Arch Linux 32:

```
$ ./dnsresolvd 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
dnsresolvd
```

Example of making **GET** and **POST** requests:

**TODO:** :point_up:
