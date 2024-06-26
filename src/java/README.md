# DNS Resolver Daemon written in Java

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [Eclipse Vert.x](http://vertx.io "Eclipse Vert.x") event-driven networking toolkit)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.5](#building-under-openbsdamd64-65)
  * [Building under Ubuntu Server (Ubuntu 16.04.6 LTS x86-64)](#building-under-ubuntu-server-ubuntu-16046-lts-x86-64)
  * [Building under Arch Linux (kernel 5.1.3-arch2-1-ARCH x86-64)](#building-under-arch-linux-kernel-513-arch2-1-arch-x86-64)
* **[Running](#running)**

## Building

This daemon implementation might be built and run successfully on OpenBSD, Ubuntu Server, and Arch Linux operating systems. So let's describe each build process sequentially.

### Building under OpenBSD/amd64 6.5

Install the necessary dependencies (`maven`, `syslog4j`, `jna`, `atmosphere-vertx`). Note that the `jdk` package will be installed automatically as a dependency to the `maven` package:

```
$ sudo pkg_add -vvvvv maven
$
$ JAVA_HOME=/usr/local/jdk-1.8.0; PATH=${PATH}:${JAVA_HOME}/bin java -version
openjdk version "1.8.0_202"
OpenJDK Runtime Environment (build 1.8.0_202-b08)
OpenJDK 64-Bit Server VM (build 25.202-b08, mixed mode)
```

The `syslog4j`, `jna`, and `atmosphere-vertx` packages (as JARs) have to be installed via **Apache Maven**. The following compound one-liner script will actually do the job:

```
$ mvn dependency:get -Dartifact=org.graylog2:syslog4j:0.9.60          && \
  mvn dependency:get -Dartifact=net.java.dev.jna:jna:5.3.1            && \
  mvn dependency:get -Dartifact=org.atmosphere:atmosphere-vertx:3.0.0
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
[INFO] Resolving net.java.dev.jna:jna:jar:5.3.1 with transitive dependencies
...
Downloading from central: https://repo.maven.apache.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar
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
[INFO] Resolving org.atmosphere:atmosphere-vertx:jar:3.0.0 with transitive dependencies
...
Downloading from central: https://repo.maven.apache.org/maven2/org/atmosphere/atmosphere-vertx/3.0.0/atmosphere-vertx-3.0.0.jar
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
$ cd src/java
$ gmake clean && gmake all
rm -f DnsResolvd.class dns_resolv/DnsLookupController.class dns_resolv/ControllerHelper.class
/usr/local/jdk-1.8.0/bin/javac -cp .:/home/<username>/.m2/repository/org/graylog2/syslog4j/0.9.61-SNAPSHOT/syslog4j-0.9.61-SNAPSHOT.jar:/home/<username>/.m2/repository/io/vertx/vertx-core/3.5.0/vertx-core-3.5.0.jar DnsResolvd.java
./dns_resolv/ControllerHelper.java:21: warning: URLClassPath is internal proprietary API and may be removed in a future release
import sun.misc.URLClassPath;
               ^
./dns_resolv/ControllerHelper.java:140: warning: URLClassPath is internal proprietary API and may be removed in a future release
        URLClassPath ucp = null;              try {
        ^
./dns_resolv/ControllerHelper.java:141: warning: URLClassPath is internal proprietary API and may be removed in a future release
                       ucp = (URLClassPath) fld.get(ucl);//System.out.println(ucp);
                              ^
3 warnings
```

Once this is done, check it out... just for fun:))

```
$ ls -al . dns_resolv
.:
total 60
drwxr-xr-x   3 <username>  <usergroup>   512 Jul 12 11:25 .
drwxr-xr-x  14 <username>  <usergroup>   512 Jun 20 17:53 ..
-rw-r--r--   1 <username>  <usergroup>  2600 Jul 12 11:25 DnsResolvd.class
-rw-r--r--   1 <username>  <usergroup>  4151 Jul 12 11:25 DnsResolvd.java
-rw-r--r--   1 <username>  <usergroup>  1474 Jul 12 11:25 Makefile
-rw-r--r--   1 <username>  <usergroup>  7547 Jul 12 11:25 README.md
drwxr-xr-x   2 <username>  <usergroup>   512 Jul 12 11:25 dns_resolv
-rwxr-xr-x   1 <username>  <usergroup>  2420 Jul 12 11:25 dnsresolvd

dns_resolv:
total 80
drwxr-xr-x  2 <username>  <usergroup>    512 Jul 12 11:25 .
drwxr-xr-x  3 <username>  <usergroup>    512 Jul 12 11:25 ..
-rw-r--r--  1 <username>  <usergroup>   5285 Jul 12 11:25 ControllerHelper.class
-rw-r--r--  1 <username>  <usergroup>   9302 Jul 12 11:25 ControllerHelper.java
-rw-r--r--  1 <username>  <usergroup>   7333 Jul 12 11:25 DnsLookupController.class
-rw-r--r--  1 <username>  <usergroup>  11145 Jul 12 11:25 DnsLookupController.java
$
$ file dnsresolvd *.class dns_resolv/*.class
dnsresolvd:                           a bash script text executable
DnsResolvd.class:                     compiled Java class data, version 52.0
dns_resolv/ControllerHelper.class:    compiled Java class data, version 52.0
dns_resolv/DnsLookupController.class: compiled Java class data, version 52.0
```

### Building under Ubuntu Server (Ubuntu 16.04.6 LTS x86-64)

Install the necessary dependencies (`openjdk-8-jdk`, `maven`, `syslog4j`, `jna`, `atmosphere-vertx`):

```
$ sudo apt-get update                         && \
  sudo apt-get install openjdk-8-jdk maven -y
$
$ java -version
openjdk version "1.8.0_212"
OpenJDK Runtime Environment (build 1.8.0_212-8u212-b03-0ubuntu1.16.04.1-b03)
OpenJDK 64-Bit Server VM (build 25.212-b03, mixed mode)
```

The `syslog4j`, `jna`, and `atmosphere-vertx` packages (as JARs) have to be installed via **Apache Maven**. The following compound one-liner script will actually do the job:

```
$ mvn dependency:get -Dartifact=org.graylog2:syslog4j:0.9.60          && \
  mvn dependency:get -Dartifact=net.java.dev.jna:jna:5.3.1            && \
  mvn dependency:get -Dartifact=org.atmosphere:atmosphere-vertx:3.0.0
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] Building Maven Stub Project (No POM) 1
[INFO] ------------------------------------------------------------------------
[INFO]
[INFO] --- maven-dependency-plugin:2.8:get (default-cli) @ standalone-pom ---
[INFO] Resolving org.graylog2:syslog4j:jar:0.9.60 with transitive dependencies
...
Downloading: https://repo.maven.apache.org/maven2/org/graylog2/syslog4j/0.9.60/syslog4j-0.9.60.jar
...
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  X.XXX s
[INFO] Finished at: XXXX-XX-XXTXX:XX:XX+XX:XX
[INFO] Final Memory: XXM/XXM
[INFO] ------------------------------------------------------------------------
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] Building Maven Stub Project (No POM) 1
[INFO] ------------------------------------------------------------------------
[INFO]
[INFO] --- maven-dependency-plugin:2.8:get (default-cli) @ standalone-pom ---
[INFO] Resolving net.java.dev.jna:jna:jar:5.3.1 with transitive dependencies
...
Downloading: https://repo.maven.apache.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar
...
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  X.XXX s
[INFO] Finished at: XXXX-XX-XXTXX:XX:XX+XX:XX
[INFO] Final Memory: XXM/XXM
[INFO] ------------------------------------------------------------------------
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] Building Maven Stub Project (No POM) 1
[INFO] ------------------------------------------------------------------------
[INFO]
[INFO] --- maven-dependency-plugin:2.8:get (default-cli) @ standalone-pom ---
[INFO] Resolving org.atmosphere:atmosphere-vertx:jar:3.0.0 with transitive dependencies
...
Downloading: https://repo.maven.apache.org/maven2/org/atmosphere/atmosphere-vertx/3.0.0/atmosphere-vertx-3.0.0.jar
...
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  X.XXX s
[INFO] Finished at: XXXX-XX-XXTXX:XX:XX+XX:XX
[INFO] Final Memory: XXM/XXM
[INFO] ------------------------------------------------------------------------
```

Now the daemon might be built.

```
$ cd src/java
$ make clean && make all
rm -f DnsResolvd.class dns_resolv/DnsLookupController.class dns_resolv/ControllerHelper.class
javac -cp .:/home/<username>/.m2/repository/org/graylog2/syslog4j/0.9.60/syslog4j-0.9.60.jar:/home/<username>/.m2/repository/io/vertx/vertx-core/3.5.0/vertx-core-3.5.0.jar DnsResolvd.java
./dns_resolv/ControllerHelper.java:21: warning: URLClassPath is internal proprietary API and may be removed in a future release
import sun.misc.URLClassPath;
               ^
./dns_resolv/ControllerHelper.java:140: warning: URLClassPath is internal proprietary API and may be removed in a future release
        URLClassPath ucp = null;              try {
        ^
./dns_resolv/ControllerHelper.java:141: warning: URLClassPath is internal proprietary API and may be removed in a future release
                       ucp = (URLClassPath) fld.get(ucl);//System.out.println(ucp);
                              ^
3 warnings
```

Once this is done, check it out... just for fun:))

```
$ ls -al . dns_resolv
.:
total 48
drwxrwxr-x  3 <username> <usergroup>  4096 Jul 14 01:00 .
drwxrwxr-x 14 <username> <usergroup>  4096 Jul 14 00:11 ..
drwxrwxr-x  2 <username> <usergroup>  4096 Jul 14 01:00 dns_resolv
-rwxrwxr-x  1 <username> <usergroup>  2420 Jul 14 01:00 dnsresolvd
-rw-rw-r--  1 <username> <usergroup>  2600 Jul 14 01:00 DnsResolvd.class
-rw-rw-r--  1 <username> <usergroup>  4151 Jul 14 01:00 DnsResolvd.java
-rw-rw-r--  1 <username> <usergroup>  1474 Jul 14 01:00 Makefile
-rw-rw-r--  1 <username> <usergroup> 13724 Jul 14 01:00 README.md

dns_resolv:
total 48
drwxrwxr-x 2 <username> <usergroup>  4096 Jul 14 01:00 .
drwxrwxr-x 3 <username> <usergroup>  4096 Jul 14 01:00 ..
-rw-rw-r-- 1 <username> <usergroup>  5285 Jul 14 01:00 ControllerHelper.class
-rw-rw-r-- 1 <username> <usergroup>  9302 Jul 14 01:00 ControllerHelper.java
-rw-rw-r-- 1 <username> <usergroup>  7333 Jul 14 01:00 DnsLookupController.class
-rw-rw-r-- 1 <username> <usergroup> 11145 Jul 14 01:00 DnsLookupController.java
$
$ file dnsresolvd *.class dns_resolv/*.class
dnsresolvd:                           Bourne-Again shell script, ASCII text executable
DnsResolvd.class:                     compiled Java class data, version 52.0 (Java 1.8)
dns_resolv/ControllerHelper.class:    compiled Java class data, version 52.0 (Java 1.8)
dns_resolv/DnsLookupController.class: compiled Java class data, version 52.0 (Java 1.8)
```

### Building under Arch Linux (kernel 5.1.3-arch2-1-ARCH x86-64)

Install the necessary dependencies (`jdk8-openjdk`, `maven`, `syslog4j`, `jna`, `atmosphere-vertx`):

```
$ sudo pacman -Sy jdk8-openjdk maven
$
$ java -version
openjdk version "1.8.0_212"
OpenJDK Runtime Environment (build 1.8.0_212-b01)
OpenJDK 64-Bit Server VM (build 25.212-b01, mixed mode)
```

The `syslog4j`, `jna`, and `atmosphere-vertx` packages (as JARs) have to be installed via **Apache Maven**. The following compound one-liner script will actually do the job:

```
$ mvn dependency:get -Dartifact=org.graylog2:syslog4j:0.9.60          && \
  mvn dependency:get -Dartifact=net.java.dev.jna:jna:5.3.1            && \
  mvn dependency:get -Dartifact=org.atmosphere:atmosphere-vertx:3.0.0
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
[INFO] Resolving net.java.dev.jna:jna:jar:5.3.1 with transitive dependencies
...
Downloading from central: https://repo.maven.apache.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar
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
[INFO] Resolving org.atmosphere:atmosphere-vertx:jar:3.0.0 with transitive dependencies
...
Downloading from central: https://repo.maven.apache.org/maven2/org/atmosphere/atmosphere-vertx/3.0.0/atmosphere-vertx-3.0.0.jar
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
$ cd src/java
$ make clean && make all
rm -f DnsResolvd.class dns_resolv/DnsLookupController.class dns_resolv/ControllerHelper.class
javac -cp .:/home/<username>/.m2/repository/org/graylog2/syslog4j/0.9.60/syslog4j-0.9.60.jar:/home/<username>/.m2/repository/io/vertx/vertx-core/3.5.0/vertx-core-3.5.0.jar DnsResolvd.java
./dns_resolv/ControllerHelper.java:21: warning: URLClassPath is internal proprietary API and may be removed in a future release
import sun.misc.URLClassPath;
               ^
./dns_resolv/ControllerHelper.java:140: warning: URLClassPath is internal proprietary API and may be removed in a future release
        URLClassPath ucp = null;              try {
        ^
./dns_resolv/ControllerHelper.java:141: warning: URLClassPath is internal proprietary API and may be removed in a future release
                       ucp = (URLClassPath) fld.get(ucl);//System.out.println(ucp);
                              ^
3 warnings
```

Once this is done, check it out... just for fun:))

```
$ ls -al . dns_resolv
.:
total 52
drwxr-xr-x  3 <username> <usergroup>  4096 Jul 16 11:40 .
drwxr-xr-x 14 <username> <usergroup>  4096 Jul  1 23:20 ..
drwxr-xr-x  2 <username> <usergroup>  4096 Jul 16 11:40 dns_resolv
-rwxr-xr-x  1 <username> <usergroup>  2420 Jul 16 11:40 dnsresolvd
-rw-r--r--  1 <username> <usergroup>  2600 Jul 16 11:40 DnsResolvd.class
-rw-r--r--  1 <username> <usergroup>  4151 Jul 16 11:40 DnsResolvd.java
-rw-r--r--  1 <username> <usergroup>  1474 Jul 16 11:40 Makefile
-rw-r--r--  1 <username> <usergroup> 19710 Jul 16 11:40 README.md

dns_resolv:
total 48
drwxr-xr-x 2 <username> <usergroup>  4096 Jul 16 11:40 .
drwxr-xr-x 3 <username> <usergroup>  4096 Jul 16 11:40 ..
-rw-r--r-- 1 <username> <usergroup>  5285 Jul 16 11:40 ControllerHelper.class
-rw-r--r-- 1 <username> <usergroup>  9302 Jul 16 11:40 ControllerHelper.java
-rw-r--r-- 1 <username> <usergroup>  7333 Jul 16 11:40 DnsLookupController.class
-rw-r--r-- 1 <username> <usergroup> 11145 Jul 16 11:40 DnsLookupController.java
$
$ file dnsresolvd *.class dns_resolv/*.class
dnsresolvd:                           Bourne-Again shell script, ASCII text executable
DnsResolvd.class:                     compiled Java class data, version 52.0 (Java 1.8)
dns_resolv/ControllerHelper.class:    compiled Java class data, version 52.0 (Java 1.8)
dns_resolv/DnsLookupController.class: compiled Java class data, version 52.0 (Java 1.8)
```

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

OpenBSD/amd64 | Ubuntu Server LTS x86-64 | Arch Linux:

```
$ cd src/java
$ ./dnsresolvd 8765
Server started on port 8765
=== Hit Ctrl+C to terminate it.
```

Example of making **GET** and **POST** requests:

```
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" 'http://localhost:8765/?f=xyz&h=vertx.io'
{"hostname":"vertx.io","address":"104.27.155.177","version":"IPv4"}
=== 200
=== application/json
$
$ curl -w "\n=== %{http_code}\n=== %{content_type}\n" -d 'h=IPv6.CYBERNODE.com&f=HtmL' http://localhost:8765
<!DOCTYPE html>
<html lang="en-US" dir="ltr">
<head>
<meta http-equiv="Content-Type"    content="text/html; charset=UTF-8"           />
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
