# DNS Resolver Daemon written in Java

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [Vertosphere](http://atmosphere.github.io/atmosphere-vertx "Vertosphere") HTTP server framework)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.5](#building-under-openbsdamd64-65)
* **[Running](#running)**

## Building

This daemon implementation is known to be built and run successfully on OpenBSD operating system.

### Building under OpenBSD/amd64 6.5

Install the necessary dependencies (`maven`, `syslog4j`, `jna`, `atmosphere-vertx`). Note that the `jdk` package will be installed automatically as a dependency to the `maven` package:

```
$ sudo pkg_add -vvvvv maven
$
$ java -version
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

TBD.

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

OpenBSD/amd64:

TBD.

Example of making **GET** and **POST** requests:

TBD.
