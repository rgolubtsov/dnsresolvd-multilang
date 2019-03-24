# DNS Resolver Daemon written in Clojure

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [HTTP Kit](http://http-kit.org "HTTP Kit") HTTP client/server library)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under Arch Linux (kernel 5.0.2-arch1-1-ARCH x86-64)](#building-under-arch-linux-kernel-502-arch1-1-arch-x86-64)
* **[Running](#running)**

## Building

This daemon implementation is known to be built and run successfully on ~~OpenBSD, Ubuntu Server, and~~ Arch Linux operating systems. So let's describe each build process sequentially.

### Building under Arch Linux (kernel 5.0.2-arch1-1-ARCH x86-64)

Install the necessary dependencies (`clojure`, `maven`, `syslog4j`, `jna`, `http-kit`). Note that the `jdk8-openjdk` package will be installed automatically as a dependency to the `clojure` package:

```
$ sudo pacman -Sy clojure maven
$
$ clojure -e '(clojure-version)'
"1.10.0"
```

The `syslog4j`, `jna`, and `http-kit` packages (as JARs) have to be installed via **Apache Maven**. The following compound one-liner script will actually do the job:

```
$ mvn dependency:get -Dartifact=org.graylog2:syslog4j:0.9.60                                          && \
  mvn dependency:get -Dartifact=net.java.dev.jna:jna:5.2.0                                            && \
  mvn dependency:get -Dartifact=http-kit:http-kit:2.3.0 -DremoteRepositories=https://repo.clojars.org
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
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------< org.apache.maven:standalone-pom >-------------------
[INFO] Building Maven Stub Project (No POM) 1
[INFO] --------------------------------[ pom ]---------------------------------
[INFO]
[INFO] --- maven-dependency-plugin:2.8:get (default-cli) @ standalone-pom ---
[INFO] Resolving http-kit:http-kit:jar:2.3.0 with transitive dependencies
...
Downloading from temp: https://repo.clojars.org/http-kit/http-kit/2.3.0/http-kit-2.3.0.jar
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

Arch Linux:

```
$ ./dnsresolvd 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

Example of making **GET** and **POST** requests:

```
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" 'http://localhost:8765/?f=xyz&h=hexdocs.pm'
{"hostname":"hexdocs.pm","address":"151.101.86.217","version":"IPv4"}
=== 200
=== application/json
$
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" -d 'h=IPv6.CYBERNODE.com&f=HtmL' http://localhost:8765
<!DOCTYPE html>
<html lang="en-US" dir="ltr">
<head>
<meta http-equiv="content-type"    content="text/html; charset=UTF-8"           />
<meta http-equiv="X-UA-Compatible" content="IE=edge"                            />
<meta       name="viewport"        content="width=device-width,initial-scale=1" />
<title>DNS Resolver Daemon (dnsresolvd)</title>
</head>
<body>
<div>IPv6.CYBERNODE.com 2001:470:1:1b9:0:0:0:31 IPv6</div>
</body>
</html>

=== 200
=== text/html; charset=UTF-8
```
