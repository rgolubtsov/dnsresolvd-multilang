# DNS Resolver Daemon written in Clojure

**A daemon that performs DNS lookups for the given hostname passed in an HTTP request
<br />(using the [HTTP Kit](http://http-kit.org "HTTP Kit") HTTP client/server library)**

---

## Table of Contents

* **[Building](#building)**
  * [Building under OpenBSD/amd64 6.3](#building-under-openbsdamd64-63)
  * [Building under Ubuntu Server (Ubuntu 16.04.6 LTS x86-64)](#building-under-ubuntu-server-ubuntu-16046-lts-x86-64)
  * [Building under Arch Linux (kernel 5.0.2-arch1-1-ARCH x86-64)](#building-under-arch-linux-kernel-502-arch1-1-arch-x86-64)
* **[Running](#running)**

## Building

This daemon implementation might be built and run successfully on OpenBSD, Ubuntu Server, and Arch Linux operating systems. So let's describe each build process sequentially.

### Building under OpenBSD/amd64 6.3

Install the necessary dependencies (`clojure`, `maven`, `syslog4j`, `jna`, `http-kit`). Note that the `jdk` package will be installed automatically as a dependency to the `clojure` package:

```
$ sudo pkg_add -vvvvv clojure maven
$
$ clojure -e '(clojure-version)'
"1.8.0"
```

The `syslog4j`, `jna`, and `http-kit` packages (as JARs) have to be installed via **Apache Maven**. The following compound one-liner script will actually do the job:

```
$ mvn dependency:get -Dartifact=org.graylog2:syslog4j:0.9.60                                          && \
  mvn dependency:get -Dartifact=net.java.dev.jna:jna:5.2.0                                            && \
  mvn dependency:get -Dartifact=http-kit:http-kit:2.3.0 -DremoteRepositories=https://repo.clojars.org
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
[INFO] Resolving net.java.dev.jna:jna:jar:5.2.0 with transitive dependencies
...
Downloading: https://repo.maven.apache.org/maven2/net/java/dev/jna/jna/5.2.0/jna-5.2.0.jar
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
[INFO] Resolving http-kit:http-kit:jar:2.3.0 with transitive dependencies
...
Downloading: https://repo.maven.apache.org/maven2/http-kit/http-kit/2.3.0/http-kit-2.3.0.jar
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
$ cd src/clojure
$ gmake clean && gmake all
rm -f -vR lib
lib
mv -v srcs src;                                                                      \
if [ ! -d "lib" ]; then                                                                            \
        mkdir lib;                                                                              \
        echo "------------ Start  ahead-of-time compilation for modules in src/ -------------"; \
        for ns in dnsresolvh dnsresolvd; do                                                                          \
                clojure -e "(defmacro DEP-PREF [] (str                                          \
                                          \"file:\" (System/getProperty \"user.home\") \"/.m2/repository/\")) \
                                          (defmacro DEP-URL0 [] (str                                          \
                                          (DEP-PREF) \"http-kit/http-kit/2.3.0/http-kit-2.3.0.jar\"))         \
                                          (defmacro DEP-URL1 [] (str                                          \
                                          (DEP-PREF) \"org/clojure/data.json/0.2.6/data.json-0.2.6.jar\"))    \
                (let [acl (ClassLoader/getSystemClassLoader)                   ]                              \
                (let [fld (aget (.getDeclaredFields java.net.URLClassLoader) 0)] (.setAccessible fld true)    \
                (let [ucp (.get    fld acl)                                    ]                              \
                          (.addURL ucp (java.net.URL. (DEP-URL0)))                                            \
                          (.addURL ucp (java.net.URL. (DEP-URL1)))                                            \
                )))                                                                                           \
                                          (binding [*compile-path* \"lib\"]                                \
                                          (compile (symbol \"$ns\")))";                                      \
        done;                                                                                         \
        echo "------------ Finish ahead-of-time compilation for modules in src/ -------------"; \
fi;                                                                                                   \
mv -v src srcs
srcs -> src
------------ Start  ahead-of-time compilation for modules in src/ -------------
#'user/DEP-PREF
#'user/DEP-URL0
#'user/DEP-URL1
dnsresolvh
#'user/DEP-PREF
#'user/DEP-URL0
#'user/DEP-URL1
dnsresolvd
------------ Finish ahead-of-time compilation for modules in src/ -------------
src -> srcs
```

Once this is done, check it out... just for fun:))

```
$ ls -al . srcs lib
.:
total 132
drwxr-xr-x   4 <username>  <usergroup>    512 Mar 26 16:50 .
drwxr-xr-x  13 <username>  <usergroup>    512 Mar 26 13:50 ..
-rw-r--r--   1 <username>  <usergroup>   2884 Mar 26 16:50 Makefile
-rw-r--r--   1 <username>  <usergroup>  45865 Mar 26 16:50 README.md
-rwxr-xr-x   1 <username>  <usergroup>   6052 Mar 26 16:50 dnsresolvd
drwxr-xr-x   3 <username>  <usergroup>   3584 Mar 26 16:50 lib
drwxr-xr-x   2 <username>  <usergroup>    512 Mar 26 16:50 srcs

lib:
total 376
drwxr-xr-x  3 <username>  <usergroup>   3584 Mar 26 16:50 .
drwxr-xr-x  4 <username>  <usergroup>    512 Mar 26 16:50 ..
-rw-r--r--  1 <username>  <usergroup>   1542 Mar 26 16:50 dnsresolvd$dns_lookup.class
-rw-r--r--  1 <username>  <usergroup>   1719 Mar 26 16:50 dnsresolvd$fn__242.class
-rw-r--r--  1 <username>  <usergroup>   2645 Mar 26 16:50 dnsresolvd$loading__5569__auto____10.class
-rw-r--r--  1 <username>  <usergroup>   1647 Mar 26 16:50 dnsresolvd$reqhandler$fn__245.class
-rw-r--r--  1 <username>  <usergroup>   7581 Mar 26 16:50 dnsresolvd$reqhandler.class
-rw-r--r--  1 <username>  <usergroup>   1622 Mar 26 16:50 dnsresolvd$startup$fn__249.class
-rw-r--r--  1 <username>  <usergroup>   1608 Mar 26 16:50 dnsresolvd$startup$fn__251.class
-rw-r--r--  1 <username>  <usergroup>   3264 Mar 26 16:50 dnsresolvd$startup.class
-rw-r--r--  1 <username>  <usergroup>   3876 Mar 26 16:50 dnsresolvd__init.class
-rw-r--r--  1 <username>  <usergroup>    669 Mar 26 16:50 dnsresolvh$CB1.class
-rw-r--r--  1 <username>  <usergroup>    669 Mar 26 16:50 dnsresolvh$CB2.class
-rw-r--r--  1 <username>  <usergroup>    681 Mar 26 16:50 dnsresolvh$COLON_SPACE_SEP.class
-rw-r--r--  1 <username>  <usergroup>    681 Mar 26 16:50 dnsresolvh$COMMA_SPACE_SEP.class
-rw-r--r--  1 <username>  <usergroup>    684 Mar 26 16:50 dnsresolvh$DAT_ADDRESS_N.class
-rw-r--r--  1 <username>  <usergroup>    686 Mar 26 16:50 dnsresolvh$DAT_HOSTNAME_N.class
-rw-r--r--  1 <username>  <usergroup>    684 Mar 26 16:50 dnsresolvh$DAT_VERSION_N.class
-rw-r--r--  1 <username>  <usergroup>    680 Mar 26 16:50 dnsresolvh$DAT_VERSION_V.class
-rw-r--r--  1 <username>  <usergroup>    687 Mar 26 16:50 dnsresolvh$DEF_HOSTNAME.class
-rw-r--r--  1 <username>  <usergroup>    808 Mar 26 16:50 dnsresolvh$DIGITS.class
-rw-r--r--  1 <username>  <usergroup>    713 Mar 26 16:50 dnsresolvh$DMN_AUTHOR.class
-rw-r--r--  1 <username>  <usergroup>    702 Mar 26 16:50 dnsresolvh$DMN_COPYRIGHT__.class
-rw-r--r--  1 <username>  <usergroup>   1064 Mar 26 16:50 dnsresolvh$DMN_DESCRIPTION.class
-rw-r--r--  1 <username>  <usergroup>    704 Mar 26 16:50 dnsresolvh$DMN_NAME.class
-rw-r--r--  1 <username>  <usergroup>    678 Mar 26 16:50 dnsresolvh$DMN_VERSION.class
-rw-r--r--  1 <username>  <usergroup>    686 Mar 26 16:50 dnsresolvh$DMN_VERSION_S__.class
-rw-r--r--  1 <username>  <usergroup>    670 Mar 26 16:50 dnsresolvh$DQ1.class
-rw-r--r--  1 <username>  <usergroup>    670 Mar 26 16:50 dnsresolvh$DQ2.class
-rw-r--r--  1 <username>  <usergroup>    676 Mar 26 16:50 dnsresolvh$EMPTY_STRING.class
-rw-r--r--  1 <username>  <usergroup>    716 Mar 26 16:50 dnsresolvh$ERR_CANNOT_START_SERVER.class
-rw-r--r--  1 <username>  <usergroup>    709 Mar 26 16:50 dnsresolvh$ERR_COULD_NOT_LOOKUP.class
-rw-r--r--  1 <username>  <usergroup>   1046 Mar 26 16:50 dnsresolvh$ERR_MUST_BE_ONE_TWO_ARGS_1.class
-rw-r--r--  1 <username>  <usergroup>    701 Mar 26 16:50 dnsresolvh$ERR_MUST_BE_ONE_TWO_ARGS_2.class
-rw-r--r--  1 <username>  <usergroup>   1168 Mar 26 16:50 dnsresolvh$ERR_PORT_MUST_BE_POSITIVE_INT.class
-rw-r--r--  1 <username>  <usergroup>    679 Mar 26 16:50 dnsresolvh$ERR_PREFIX.class
-rw-r--r--  1 <username>  <usergroup>   1049 Mar 26 16:50 dnsresolvh$ERR_SRV_PORT_IS_IN_USE.class
-rw-r--r--  1 <username>  <usergroup>   1035 Mar 26 16:50 dnsresolvh$ERR_SRV_UNKNOWN_REASON.class
-rw-r--r--  1 <username>  <usergroup>    769 Mar 26 16:50 dnsresolvh$EXIT_FAILURE.class
-rw-r--r--  1 <username>  <usergroup>    769 Mar 26 16:50 dnsresolvh$EXIT_SUCCESS.class
-rw-r--r--  1 <username>  <usergroup>    696 Mar 26 16:50 dnsresolvh$HDR_CACHE_CONTROL_N.class
-rw-r--r--  1 <username>  <usergroup>   1034 Mar 26 16:50 dnsresolvh$HDR_CACHE_CONTROL_V.class
-rw-r--r--  1 <username>  <usergroup>    694 Mar 26 16:50 dnsresolvh$HDR_CONTENT_TYPE_N.class
-rw-r--r--  1 <username>  <usergroup>    711 Mar 26 16:50 dnsresolvh$HDR_CONTENT_TYPE_V_HTML.class
-rw-r--r--  1 <username>  <usergroup>    703 Mar 26 16:50 dnsresolvh$HDR_CONTENT_TYPE_V_JSON.class
-rw-r--r--  1 <username>  <usergroup>    684 Mar 26 16:50 dnsresolvh$HDR_EXPIRES_N.class
-rw-r--r--  1 <username>  <usergroup>    706 Mar 26 16:50 dnsresolvh$HDR_EXPIRES_V.class
-rw-r--r--  1 <username>  <usergroup>    682 Mar 26 16:50 dnsresolvh$HDR_PRAGMA_N.class
-rw-r--r--  1 <username>  <usergroup>    684 Mar 26 16:50 dnsresolvh$HDR_PRAGMA_V.class
-rw-r--r--  1 <username>  <usergroup>    776 Mar 26 16:50 dnsresolvh$MAX_PORT.class
-rw-r--r--  1 <username>  <usergroup>    776 Mar 26 16:50 dnsresolvh$MIN_PORT.class
-rw-r--r--  1 <username>  <usergroup>    707 Mar 26 16:50 dnsresolvh$MSG_SERVER_STARTED_1.class
-rw-r--r--  1 <username>  <usergroup>    715 Mar 26 16:50 dnsresolvh$MSG_SERVER_STARTED_2.class
-rw-r--r--  1 <username>  <usergroup>    691 Mar 26 16:50 dnsresolvh$MSG_USAGE_TEMPLATE_1.class
-rw-r--r--  1 <username>  <usergroup>    703 Mar 26 16:50 dnsresolvh$MSG_USAGE_TEMPLATE_2.class
-rw-r--r--  1 <username>  <usergroup>    673 Mar 26 16:50 dnsresolvh$NEW_LINE.class
-rw-r--r--  1 <username>  <usergroup>    681 Mar 26 16:50 dnsresolvh$ONE_SPACE_STRING.class
-rw-r--r--  1 <username>  <usergroup>    813 Mar 26 16:50 dnsresolvh$PARAMS_SEPS.class
-rw-r--r--  1 <username>  <usergroup>    682 Mar 26 16:50 dnsresolvh$PRINT_BANNER_OPT.class
-rw-r--r--  1 <username>  <usergroup>    680 Mar 26 16:50 dnsresolvh$PRM_FMT_HTML.class
-rw-r--r--  1 <username>  <usergroup>    680 Mar 26 16:50 dnsresolvh$PRM_FMT_JSON.class
-rw-r--r--  1 <username>  <usergroup>    783 Mar 26 16:50 dnsresolvh$RSC_HTTP_200_OK.class
-rw-r--r--  1 <username>  <usergroup>   1180 Mar 26 16:50 dnsresolvh$add_response_headers.class
-rw-r--r--  1 <username>  <usergroup>    766 Mar 26 16:50 dnsresolvh$cleanups_fixate.class
-rw-r--r--  1 <username>  <usergroup>   1721 Mar 26 16:50 dnsresolvh$fn__12.class
-rw-r--r--  1 <username>  <usergroup>   1721 Mar 26 16:50 dnsresolvh$fn__14.class
-rw-r--r--  1 <username>  <usergroup>   1521 Mar 26 16:50 dnsresolvh$loading__5569__auto____10.class
-rw-r--r--  1 <username>  <usergroup>   1521 Mar 26 16:50 dnsresolvh$loading__5569__auto____12.class
-rw-r--r--  1 <username>  <usergroup>   1344 Mar 26 16:50 dnsresolvh$separator_draw$iter__67__71$fn__72$fn__73.class
-rw-r--r--  1 <username>  <usergroup>   2473 Mar 26 16:50 dnsresolvh$separator_draw$iter__67__71$fn__72.class
-rw-r--r--  1 <username>  <usergroup>    780 Mar 26 16:50 dnsresolvh$separator_draw$iter__67__71.class
-rw-r--r--  1 <username>  <usergroup>   1344 Mar 26 16:50 dnsresolvh$separator_draw$iter__69__73$fn__74$fn__75.class
-rw-r--r--  1 <username>  <usergroup>   2473 Mar 26 16:50 dnsresolvh$separator_draw$iter__69__73$fn__74.class
-rw-r--r--  1 <username>  <usergroup>    780 Mar 26 16:50 dnsresolvh$separator_draw$iter__69__73.class
-rw-r--r--  1 <username>  <usergroup>   1546 Mar 26 16:50 dnsresolvh$separator_draw.class
-rw-r--r--  1 <username>  <usergroup>  17799 Mar 26 16:50 dnsresolvh__init.class
drwxr-xr-x  3 <username>  <usergroup>    512 Mar 26 16:50 org

srcs:
total 44
drwxr-xr-x  2 <username>  <usergroup>    512 Mar 26 16:50 .
drwxr-xr-x  4 <username>  <usergroup>    512 Mar 26 16:50 ..
-rw-r--r--  1 <username>  <usergroup>  10431 Mar 26 16:50 dnsresolvd.clj
-rw-r--r--  1 <username>  <usergroup>   5554 Mar 26 16:50 dnsresolvh.clj
$
$ file dnsresolvd srcs/* lib/*
dnsresolvd:                                a clojure script text executable
srcs/dnsresolvd.clj:                       ASCII English text
srcs/dnsresolvh.clj:                       ASCII English text
lib/dnsresolvd$dns_lookup.class:           compiled Java class data, version 49.0
...
lib/dnsresolvd$reqhandler.class:           compiled Java class data, version 49.0
...
lib/dnsresolvd$startup.class:              compiled Java class data, version 49.0
lib/dnsresolvd__init.class:                compiled Java class data, version 49.0
...
lib/dnsresolvh$add_response_headers.class: compiled Java class data, version 49.0
lib/dnsresolvh$cleanups_fixate.class:      compiled Java class data, version 49.0
...
lib/dnsresolvh$separator_draw.class:       compiled Java class data, version 49.0
lib/dnsresolvh__init.class:                compiled Java class data, version 49.0
lib/org:                                   directory
```

### Building under Ubuntu Server (Ubuntu 16.04.6 LTS x86-64)

Install the necessary dependencies (`maven`, `clojure`, `syslog4j`, `jna`, `http-kit`). Note that the `openjdk-8-jre-headless:amd64` package will be installed automatically as a dependency to the `maven` package:

```
$ sudo apt-get update           && \
  sudo apt-get install maven -y
$
$ curl -O https://download.clojure.org/install/linux-install-1.10.0.442.sh
$
$ chmod +x linux-install-1.10.0.442.sh
$
$ sudo ./linux-install-1.10.0.442.sh
Downloading and expanding tar
Installing libs into /usr/local/lib/clojure
Installing clojure and clj into /usr/local/bin
Installing man pages into /usr/local/share/man/man1
Removing download
Use clj -h for help.
$
$ clojure -e '(clojure-version)'
"1.10.0"
```

The `syslog4j`, `jna`, and `http-kit` packages (as JARs) have to be installed via **Apache Maven**. The following compound one-liner script will actually do the job:

```
$ mvn dependency:get -Dartifact=org.graylog2:syslog4j:0.9.60                                          && \
  mvn dependency:get -Dartifact=net.java.dev.jna:jna:5.2.0                                            && \
  mvn dependency:get -Dartifact=http-kit:http-kit:2.3.0 -DremoteRepositories=https://repo.clojars.org
Warning: JAVA_HOME environment variable is not set.
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] Building Maven Stub Project (No POM) 1
[INFO] ------------------------------------------------------------------------
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
[INFO] Final Memory: XXM/XXM
[INFO] ------------------------------------------------------------------------
Warning: JAVA_HOME environment variable is not set.
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] Building Maven Stub Project (No POM) 1
[INFO] ------------------------------------------------------------------------
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
[INFO] Final Memory: XXM/XXM
[INFO] ------------------------------------------------------------------------
Warning: JAVA_HOME environment variable is not set.
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] Building Maven Stub Project (No POM) 1
[INFO] ------------------------------------------------------------------------
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
[INFO] Final Memory: XXM/XXM
[INFO] ------------------------------------------------------------------------
```

Now the daemon might be built.

```
$ cd src/clojure
$ make clean && make all
rm -f -vR lib
mv -v srcs src;                                                                      \
if [ ! -d "lib" ]; then                                                                            \
        mkdir lib;                                                                              \
        echo "------------ Start  ahead-of-time compilation for modules in src/ -------------"; \
        for ns in dnsresolvh dnsresolvd; do                                                                          \
                clojure -e "(defmacro DEP-PREF [] (str                                          \
                                          \"file:\" (System/getProperty \"user.home\") \"/.m2/repository/\")) \
                                          (defmacro DEP-URL0 [] (str                                          \
                                          (DEP-PREF) \"http-kit/http-kit/2.3.0/http-kit-2.3.0.jar\"))         \
                                          (defmacro DEP-URL1 [] (str                                          \
                                          (DEP-PREF) \"org/clojure/data.json/0.2.6/data.json-0.2.6.jar\"))    \
                (let [acl (ClassLoader/getSystemClassLoader)                   ]                              \
                (let [fld (aget (.getDeclaredFields java.net.URLClassLoader) 0)] (.setAccessible fld true)    \
                (let [ucp (.get    fld acl)                                    ]                              \
                          (.addURL ucp (java.net.URL. (DEP-URL0)))                                            \
                          (.addURL ucp (java.net.URL. (DEP-URL1)))                                            \
                )))                                                                                           \
                                          (binding [*compile-path* \"lib\"]                                \
                                          (compile (symbol \"$ns\")))";                                      \
        done;                                                                                         \
        echo "------------ Finish ahead-of-time compilation for modules in src/ -------------"; \
fi;                                                                                                   \
mv -v src srcs
'srcs' -> 'src'
------------ Start  ahead-of-time compilation for modules in src/ -------------
#'user/DEP-PREF
#'user/DEP-URL0
#'user/DEP-URL1
dnsresolvh
#'user/DEP-PREF
#'user/DEP-URL0
#'user/DEP-URL1
dnsresolvd
------------ Finish ahead-of-time compilation for modules in src/ -------------
'src' -> 'srcs'
```

Once this is done, check it out... just for fun:))

```
$ ls -al . srcs lib
.:
total 60
drwxrwxr-x  4 <username> <usergroup>  4096 Mar 26 00:50 .
drwxrwxr-x 13 <username> <usergroup>  4096 Mar 25 21:25 ..
-rwxrwxr-x  1 <username> <usergroup>  6052 Mar 26 00:50 dnsresolvd
drwxrwxr-x  3 <username> <usergroup>  4096 Mar 26 00:50 lib
-rw-rw-r--  1 <username> <usergroup>  2884 Mar 26 00:50 Makefile
-rw-rw-r--  1 <username> <usergroup> 31388 Mar 26 00:50 README.md
drwxrwxr-x  2 <username> <usergroup>  4096 Mar 26 00:50 srcs

lib:
total 332
drwxrwxr-x 3 <username> <usergroup>  4096 Mar 26 00:50 .
drwxrwxr-x 4 <username> <usergroup>  4096 Mar 26 00:50 ..
-rw-rw-r-- 1 <username> <usergroup>  1723 Mar 26 00:50 dnsresolvd$dns_lookup.class
-rw-rw-r-- 1 <username> <usergroup>  1719 Mar 26 00:50 dnsresolvd$fn__379.class
-rw-rw-r-- 1 <username> <usergroup>  3978 Mar 26 00:50 dnsresolvd__init.class
-rw-rw-r-- 1 <username> <usergroup>  2523 Mar 26 00:50 dnsresolvd$loading__6706__auto____145.class
-rw-rw-r-- 1 <username> <usergroup>  8639 Mar 26 00:50 dnsresolvd$reqhandler.class
-rw-rw-r-- 1 <username> <usergroup>  1681 Mar 26 00:50 dnsresolvd$reqhandler$fn__382.class
-rw-rw-r-- 1 <username> <usergroup>  3364 Mar 26 00:50 dnsresolvd$startup.class
-rw-rw-r-- 1 <username> <usergroup>  1683 Mar 26 00:50 dnsresolvd$startup$fn__386.class
-rw-rw-r-- 1 <username> <usergroup>  1669 Mar 26 00:50 dnsresolvd$startup$fn__388.class
-rw-rw-r-- 1 <username> <usergroup>  1294 Mar 26 00:50 dnsresolvh$add_response_headers.class
-rw-rw-r-- 1 <username> <usergroup>   669 Mar 26 00:50 dnsresolvh$CB1.class
-rw-rw-r-- 1 <username> <usergroup>   669 Mar 26 00:50 dnsresolvh$CB2.class
-rw-rw-r-- 1 <username> <usergroup>   766 Mar 26 00:50 dnsresolvh$cleanups_fixate.class
-rw-rw-r-- 1 <username> <usergroup>   681 Mar 26 00:50 dnsresolvh$COLON_SPACE_SEP.class
-rw-rw-r-- 1 <username> <usergroup>   681 Mar 26 00:50 dnsresolvh$COMMA_SPACE_SEP.class
-rw-rw-r-- 1 <username> <usergroup>   684 Mar 26 00:50 dnsresolvh$DAT_ADDRESS_N.class
-rw-rw-r-- 1 <username> <usergroup>   686 Mar 26 00:50 dnsresolvh$DAT_HOSTNAME_N.class
-rw-rw-r-- 1 <username> <usergroup>   684 Mar 26 00:50 dnsresolvh$DAT_VERSION_N.class
-rw-rw-r-- 1 <username> <usergroup>   680 Mar 26 00:50 dnsresolvh$DAT_VERSION_V.class
-rw-rw-r-- 1 <username> <usergroup>   687 Mar 26 00:50 dnsresolvh$DEF_HOSTNAME.class
-rw-rw-r-- 1 <username> <usergroup>   808 Mar 26 00:50 dnsresolvh$DIGITS.class
-rw-rw-r-- 1 <username> <usergroup>   713 Mar 26 00:50 dnsresolvh$DMN_AUTHOR.class
-rw-rw-r-- 1 <username> <usergroup>   702 Mar 26 00:50 dnsresolvh$DMN_COPYRIGHT__.class
-rw-rw-r-- 1 <username> <usergroup>  1064 Mar 26 00:50 dnsresolvh$DMN_DESCRIPTION.class
-rw-rw-r-- 1 <username> <usergroup>   704 Mar 26 00:50 dnsresolvh$DMN_NAME.class
-rw-rw-r-- 1 <username> <usergroup>   678 Mar 26 00:50 dnsresolvh$DMN_VERSION.class
-rw-rw-r-- 1 <username> <usergroup>   686 Mar 26 00:50 dnsresolvh$DMN_VERSION_S__.class
-rw-rw-r-- 1 <username> <usergroup>   670 Mar 26 00:50 dnsresolvh$DQ1.class
-rw-rw-r-- 1 <username> <usergroup>   670 Mar 26 00:50 dnsresolvh$DQ2.class
-rw-rw-r-- 1 <username> <usergroup>   676 Mar 26 00:50 dnsresolvh$EMPTY_STRING.class
-rw-rw-r-- 1 <username> <usergroup>   716 Mar 26 00:50 dnsresolvh$ERR_CANNOT_START_SERVER.class
-rw-rw-r-- 1 <username> <usergroup>   709 Mar 26 00:50 dnsresolvh$ERR_COULD_NOT_LOOKUP.class
-rw-rw-r-- 1 <username> <usergroup>  1046 Mar 26 00:50 dnsresolvh$ERR_MUST_BE_ONE_TWO_ARGS_1.class
-rw-rw-r-- 1 <username> <usergroup>   701 Mar 26 00:50 dnsresolvh$ERR_MUST_BE_ONE_TWO_ARGS_2.class
-rw-rw-r-- 1 <username> <usergroup>  1168 Mar 26 00:50 dnsresolvh$ERR_PORT_MUST_BE_POSITIVE_INT.class
-rw-rw-r-- 1 <username> <usergroup>   679 Mar 26 00:50 dnsresolvh$ERR_PREFIX.class
-rw-rw-r-- 1 <username> <usergroup>  1049 Mar 26 00:50 dnsresolvh$ERR_SRV_PORT_IS_IN_USE.class
-rw-rw-r-- 1 <username> <usergroup>  1035 Mar 26 00:50 dnsresolvh$ERR_SRV_UNKNOWN_REASON.class
-rw-rw-r-- 1 <username> <usergroup>   769 Mar 26 00:50 dnsresolvh$EXIT_FAILURE.class
-rw-rw-r-- 1 <username> <usergroup>   769 Mar 26 00:50 dnsresolvh$EXIT_SUCCESS.class
-rw-rw-r-- 1 <username> <usergroup>  1722 Mar 26 00:50 dnsresolvh$fn__147.class
-rw-rw-r-- 1 <username> <usergroup>  1722 Mar 26 00:50 dnsresolvh$fn__149.class
-rw-rw-r-- 1 <username> <usergroup>   696 Mar 26 00:50 dnsresolvh$HDR_CACHE_CONTROL_N.class
-rw-rw-r-- 1 <username> <usergroup>  1034 Mar 26 00:50 dnsresolvh$HDR_CACHE_CONTROL_V.class
-rw-rw-r-- 1 <username> <usergroup>   694 Mar 26 00:50 dnsresolvh$HDR_CONTENT_TYPE_N.class
-rw-rw-r-- 1 <username> <usergroup>   711 Mar 26 00:50 dnsresolvh$HDR_CONTENT_TYPE_V_HTML.class
-rw-rw-r-- 1 <username> <usergroup>   703 Mar 26 00:50 dnsresolvh$HDR_CONTENT_TYPE_V_JSON.class
-rw-rw-r-- 1 <username> <usergroup>   684 Mar 26 00:50 dnsresolvh$HDR_EXPIRES_N.class
-rw-rw-r-- 1 <username> <usergroup>   706 Mar 26 00:50 dnsresolvh$HDR_EXPIRES_V.class
-rw-rw-r-- 1 <username> <usergroup>   682 Mar 26 00:50 dnsresolvh$HDR_PRAGMA_N.class
-rw-rw-r-- 1 <username> <usergroup>   684 Mar 26 00:50 dnsresolvh$HDR_PRAGMA_V.class
-rw-rw-r-- 1 <username> <usergroup> 17902 Mar 26 00:50 dnsresolvh__init.class
-rw-rw-r-- 1 <username> <usergroup>  1581 Mar 26 00:50 dnsresolvh$loading__6706__auto____145.class
-rw-rw-r-- 1 <username> <usergroup>  1581 Mar 26 00:50 dnsresolvh$loading__6706__auto____147.class
-rw-rw-r-- 1 <username> <usergroup>   776 Mar 26 00:50 dnsresolvh$MAX_PORT.class
-rw-rw-r-- 1 <username> <usergroup>   776 Mar 26 00:50 dnsresolvh$MIN_PORT.class
-rw-rw-r-- 1 <username> <usergroup>   707 Mar 26 00:50 dnsresolvh$MSG_SERVER_STARTED_1.class
-rw-rw-r-- 1 <username> <usergroup>   715 Mar 26 00:50 dnsresolvh$MSG_SERVER_STARTED_2.class
-rw-rw-r-- 1 <username> <usergroup>   691 Mar 26 00:50 dnsresolvh$MSG_USAGE_TEMPLATE_1.class
-rw-rw-r-- 1 <username> <usergroup>   703 Mar 26 00:50 dnsresolvh$MSG_USAGE_TEMPLATE_2.class
-rw-rw-r-- 1 <username> <usergroup>   673 Mar 26 00:50 dnsresolvh$NEW_LINE.class
-rw-rw-r-- 1 <username> <usergroup>   681 Mar 26 00:50 dnsresolvh$ONE_SPACE_STRING.class
-rw-rw-r-- 1 <username> <usergroup>   813 Mar 26 00:50 dnsresolvh$PARAMS_SEPS.class
-rw-rw-r-- 1 <username> <usergroup>   682 Mar 26 00:50 dnsresolvh$PRINT_BANNER_OPT.class
-rw-rw-r-- 1 <username> <usergroup>   680 Mar 26 00:50 dnsresolvh$PRM_FMT_HTML.class
-rw-rw-r-- 1 <username> <usergroup>   680 Mar 26 00:50 dnsresolvh$PRM_FMT_JSON.class
-rw-rw-r-- 1 <username> <usergroup>   783 Mar 26 00:50 dnsresolvh$RSC_HTTP_200_OK.class
-rw-rw-r-- 1 <username> <usergroup>  1549 Mar 26 00:50 dnsresolvh$separator_draw.class
-rw-rw-r-- 1 <username> <usergroup>   786 Mar 26 00:50 dnsresolvh$separator_draw$iter__202__206.class
-rw-rw-r-- 1 <username> <usergroup>  2637 Mar 26 00:50 dnsresolvh$separator_draw$iter__202__206$fn__207.class
-rw-rw-r-- 1 <username> <usergroup>  1428 Mar 26 00:50 dnsresolvh$separator_draw$iter__202__206$fn__207$fn__208.class
-rw-rw-r-- 1 <username> <usergroup>   786 Mar 26 00:50 dnsresolvh$separator_draw$iter__204__208.class
-rw-rw-r-- 1 <username> <usergroup>  2637 Mar 26 00:50 dnsresolvh$separator_draw$iter__204__208$fn__209.class
-rw-rw-r-- 1 <username> <usergroup>  1428 Mar 26 00:50 dnsresolvh$separator_draw$iter__204__208$fn__209$fn__210.class
drwxrwxr-x 3 <username> <usergroup>  4096 Mar 26 00:50 org

srcs:
total 28
drwxrwxr-x 2 <username> <usergroup>  4096 Mar 26 00:50 .
drwxrwxr-x 4 <username> <usergroup>  4096 Mar 26 00:50 ..
-rw-rw-r-- 1 <username> <usergroup> 10368 Mar 26 00:50 dnsresolvd.clj
-rw-rw-r-- 1 <username> <usergroup>  5554 Mar 26 00:50 dnsresolvh.clj
$
$ file dnsresolvd srcs/* lib/*
dnsresolvd:                                a /usr/bin/env clojure script, ASCII text executable
srcs/dnsresolvd.clj:                       ASCII text
srcs/dnsresolvh.clj:                       ASCII text
lib/dnsresolvd$dns_lookup.class:           compiled Java class data, version 52.0 (Java 1.8)
...
lib/dnsresolvd__init.class:                compiled Java class data, version 52.0 (Java 1.8)
...
lib/dnsresolvd$reqhandler.class:           compiled Java class data, version 52.0 (Java 1.8)
...
lib/dnsresolvd$startup.class:              compiled Java class data, version 52.0 (Java 1.8)
...
lib/dnsresolvh$add_response_headers.class: compiled Java class data, version 52.0 (Java 1.8)
...
lib/dnsresolvh$cleanups_fixate.class:      compiled Java class data, version 52.0 (Java 1.8)
...
lib/dnsresolvh__init.class:                compiled Java class data, version 52.0 (Java 1.8)
...
lib/dnsresolvh$separator_draw.class:       compiled Java class data, version 52.0 (Java 1.8)
...
lib/org:                                   directory
```

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
mv -v srcs src;                                                                      \
if [ ! -d "lib" ]; then                                                                            \
        mkdir lib;                                                                              \
        echo "------------ Start  ahead-of-time compilation for modules in src/ -------------"; \
        for ns in dnsresolvh dnsresolvd; do                                                                          \
                clojure -e "(defmacro DEP-PREF [] (str                                          \
                                          \"file:\" (System/getProperty \"user.home\") \"/.m2/repository/\")) \
                                          (defmacro DEP-URL0 [] (str                                          \
                                          (DEP-PREF) \"http-kit/http-kit/2.3.0/http-kit-2.3.0.jar\"))         \
                                          (defmacro DEP-URL1 [] (str                                          \
                                          (DEP-PREF) \"org/clojure/data.json/0.2.6/data.json-0.2.6.jar\"))    \
                (let [acl (ClassLoader/getSystemClassLoader)                   ]                              \
                (let [fld (aget (.getDeclaredFields java.net.URLClassLoader) 0)] (.setAccessible fld true)    \
                (let [ucp (.get    fld acl)                                    ]                              \
                          (.addURL ucp (java.net.URL. (DEP-URL0)))                                            \
                          (.addURL ucp (java.net.URL. (DEP-URL1)))                                            \
                )))                                                                                           \
                                          (binding [*compile-path* \"lib\"]                                \
                                          (compile (symbol \"$ns\")))";                                      \
        done;                                                                                         \
        echo "------------ Finish ahead-of-time compilation for modules in src/ -------------"; \
fi;                                                                                                   \
mv -v src srcs
renamed 'srcs' -> 'src'
------------ Start  ahead-of-time compilation for modules in src/ -------------
#'user/DEP-PREF
#'user/DEP-URL0
#'user/DEP-URL1
dnsresolvh
#'user/DEP-PREF
#'user/DEP-URL0
#'user/DEP-URL1
dnsresolvd
------------ Finish ahead-of-time compilation for modules in src/ -------------
renamed 'src' -> 'srcs'
```

Once this is done, check it out... just for fun:))

```
$ ls -al . srcs lib
.:
total 44
drwxr-xr-x  4 <username> <usergroup>  4096 Mar 25 17:50 .
drwxr-xr-x 13 <username> <usergroup>  4096 Mar  5 13:05 ..
-rwxr-xr-x  1 <username> <usergroup>  6052 Mar 25 17:50 dnsresolvd
drwxr-xr-x  3 <username> <usergroup>  4096 Mar 25 17:50 lib
-rw-r--r--  1 <username> <usergroup>  2884 Mar 25 17:50 Makefile
-rw-r--r--  1 <username> <usergroup> 16284 Mar 25 17:50 README.md
drwxr-xr-x  2 <username> <usergroup>  4096 Mar 25 17:50 srcs

lib:
total 332
drwxr-xr-x 3 <username> <usergroup>  4096 Mar 25 17:50  .
drwxr-xr-x 4 <username> <usergroup>  4096 Mar 25 17:50  ..
-rw-r--r-- 1 <username> <usergroup>  1723 Mar 25 17:50 'dnsresolvd$dns_lookup.class'
-rw-r--r-- 1 <username> <usergroup>  1719 Mar 25 17:50 'dnsresolvd$fn__379.class'
-rw-r--r-- 1 <username> <usergroup>  2523 Mar 25 17:50 'dnsresolvd$loading__6706__auto____145.class'
-rw-r--r-- 1 <username> <usergroup>  1681 Mar 25 17:50 'dnsresolvd$reqhandler$fn__382.class'
-rw-r--r-- 1 <username> <usergroup>  8639 Mar 25 17:50 'dnsresolvd$reqhandler.class'
-rw-r--r-- 1 <username> <usergroup>  1683 Mar 25 17:50 'dnsresolvd$startup$fn__386.class'
-rw-r--r-- 1 <username> <usergroup>  1669 Mar 25 17:50 'dnsresolvd$startup$fn__388.class'
-rw-r--r-- 1 <username> <usergroup>  3364 Mar 25 17:50 'dnsresolvd$startup.class'
-rw-r--r-- 1 <username> <usergroup>  3978 Mar 25 17:50  dnsresolvd__init.class
-rw-r--r-- 1 <username> <usergroup>  1294 Mar 25 17:50 'dnsresolvh$add_response_headers.class'
-rw-r--r-- 1 <username> <usergroup>   669 Mar 25 17:50 'dnsresolvh$CB1.class'
-rw-r--r-- 1 <username> <usergroup>   669 Mar 25 17:50 'dnsresolvh$CB2.class'
-rw-r--r-- 1 <username> <usergroup>   766 Mar 25 17:50 'dnsresolvh$cleanups_fixate.class'
-rw-r--r-- 1 <username> <usergroup>   681 Mar 25 17:50 'dnsresolvh$COLON_SPACE_SEP.class'
-rw-r--r-- 1 <username> <usergroup>   681 Mar 25 17:50 'dnsresolvh$COMMA_SPACE_SEP.class'
-rw-r--r-- 1 <username> <usergroup>   684 Mar 25 17:50 'dnsresolvh$DAT_ADDRESS_N.class'
-rw-r--r-- 1 <username> <usergroup>   686 Mar 25 17:50 'dnsresolvh$DAT_HOSTNAME_N.class'
-rw-r--r-- 1 <username> <usergroup>   684 Mar 25 17:50 'dnsresolvh$DAT_VERSION_N.class'
-rw-r--r-- 1 <username> <usergroup>   680 Mar 25 17:50 'dnsresolvh$DAT_VERSION_V.class'
-rw-r--r-- 1 <username> <usergroup>   687 Mar 25 17:50 'dnsresolvh$DEF_HOSTNAME.class'
-rw-r--r-- 1 <username> <usergroup>   808 Mar 25 17:50 'dnsresolvh$DIGITS.class'
-rw-r--r-- 1 <username> <usergroup>   713 Mar 25 17:50 'dnsresolvh$DMN_AUTHOR.class'
-rw-r--r-- 1 <username> <usergroup>   702 Mar 25 17:50 'dnsresolvh$DMN_COPYRIGHT__.class'
-rw-r--r-- 1 <username> <usergroup>  1064 Mar 25 17:50 'dnsresolvh$DMN_DESCRIPTION.class'
-rw-r--r-- 1 <username> <usergroup>   704 Mar 25 17:50 'dnsresolvh$DMN_NAME.class'
-rw-r--r-- 1 <username> <usergroup>   678 Mar 25 17:50 'dnsresolvh$DMN_VERSION.class'
-rw-r--r-- 1 <username> <usergroup>   686 Mar 25 17:50 'dnsresolvh$DMN_VERSION_S__.class'
-rw-r--r-- 1 <username> <usergroup>   670 Mar 25 17:50 'dnsresolvh$DQ1.class'
-rw-r--r-- 1 <username> <usergroup>   670 Mar 25 17:50 'dnsresolvh$DQ2.class'
-rw-r--r-- 1 <username> <usergroup>   676 Mar 25 17:50 'dnsresolvh$EMPTY_STRING.class'
-rw-r--r-- 1 <username> <usergroup>   716 Mar 25 17:50 'dnsresolvh$ERR_CANNOT_START_SERVER.class'
-rw-r--r-- 1 <username> <usergroup>   709 Mar 25 17:50 'dnsresolvh$ERR_COULD_NOT_LOOKUP.class'
-rw-r--r-- 1 <username> <usergroup>  1046 Mar 25 17:50 'dnsresolvh$ERR_MUST_BE_ONE_TWO_ARGS_1.class'
-rw-r--r-- 1 <username> <usergroup>   701 Mar 25 17:50 'dnsresolvh$ERR_MUST_BE_ONE_TWO_ARGS_2.class'
-rw-r--r-- 1 <username> <usergroup>  1168 Mar 25 17:50 'dnsresolvh$ERR_PORT_MUST_BE_POSITIVE_INT.class'
-rw-r--r-- 1 <username> <usergroup>   679 Mar 25 17:50 'dnsresolvh$ERR_PREFIX.class'
-rw-r--r-- 1 <username> <usergroup>  1049 Mar 25 17:50 'dnsresolvh$ERR_SRV_PORT_IS_IN_USE.class'
-rw-r--r-- 1 <username> <usergroup>  1035 Mar 25 17:50 'dnsresolvh$ERR_SRV_UNKNOWN_REASON.class'
-rw-r--r-- 1 <username> <usergroup>   769 Mar 25 17:50 'dnsresolvh$EXIT_FAILURE.class'
-rw-r--r-- 1 <username> <usergroup>   769 Mar 25 17:50 'dnsresolvh$EXIT_SUCCESS.class'
-rw-r--r-- 1 <username> <usergroup>  1722 Mar 25 17:50 'dnsresolvh$fn__147.class'
-rw-r--r-- 1 <username> <usergroup>  1722 Mar 25 17:50 'dnsresolvh$fn__149.class'
-rw-r--r-- 1 <username> <usergroup>   696 Mar 25 17:50 'dnsresolvh$HDR_CACHE_CONTROL_N.class'
-rw-r--r-- 1 <username> <usergroup>  1034 Mar 25 17:50 'dnsresolvh$HDR_CACHE_CONTROL_V.class'
-rw-r--r-- 1 <username> <usergroup>   694 Mar 25 17:50 'dnsresolvh$HDR_CONTENT_TYPE_N.class'
-rw-r--r-- 1 <username> <usergroup>   711 Mar 25 17:50 'dnsresolvh$HDR_CONTENT_TYPE_V_HTML.class'
-rw-r--r-- 1 <username> <usergroup>   703 Mar 25 17:50 'dnsresolvh$HDR_CONTENT_TYPE_V_JSON.class'
-rw-r--r-- 1 <username> <usergroup>   684 Mar 25 17:50 'dnsresolvh$HDR_EXPIRES_N.class'
-rw-r--r-- 1 <username> <usergroup>   706 Mar 25 17:50 'dnsresolvh$HDR_EXPIRES_V.class'
-rw-r--r-- 1 <username> <usergroup>   682 Mar 25 17:50 'dnsresolvh$HDR_PRAGMA_N.class'
-rw-r--r-- 1 <username> <usergroup>   684 Mar 25 17:50 'dnsresolvh$HDR_PRAGMA_V.class'
-rw-r--r-- 1 <username> <usergroup>  1581 Mar 25 17:50 'dnsresolvh$loading__6706__auto____145.class'
-rw-r--r-- 1 <username> <usergroup>  1581 Mar 25 17:50 'dnsresolvh$loading__6706__auto____147.class'
-rw-r--r-- 1 <username> <usergroup>   776 Mar 25 17:50 'dnsresolvh$MAX_PORT.class'
-rw-r--r-- 1 <username> <usergroup>   776 Mar 25 17:50 'dnsresolvh$MIN_PORT.class'
-rw-r--r-- 1 <username> <usergroup>   707 Mar 25 17:50 'dnsresolvh$MSG_SERVER_STARTED_1.class'
-rw-r--r-- 1 <username> <usergroup>   715 Mar 25 17:50 'dnsresolvh$MSG_SERVER_STARTED_2.class'
-rw-r--r-- 1 <username> <usergroup>   691 Mar 25 17:50 'dnsresolvh$MSG_USAGE_TEMPLATE_1.class'
-rw-r--r-- 1 <username> <usergroup>   703 Mar 25 17:50 'dnsresolvh$MSG_USAGE_TEMPLATE_2.class'
-rw-r--r-- 1 <username> <usergroup>   673 Mar 25 17:50 'dnsresolvh$NEW_LINE.class'
-rw-r--r-- 1 <username> <usergroup>   681 Mar 25 17:50 'dnsresolvh$ONE_SPACE_STRING.class'
-rw-r--r-- 1 <username> <usergroup>   813 Mar 25 17:50 'dnsresolvh$PARAMS_SEPS.class'
-rw-r--r-- 1 <username> <usergroup>   682 Mar 25 17:50 'dnsresolvh$PRINT_BANNER_OPT.class'
-rw-r--r-- 1 <username> <usergroup>   680 Mar 25 17:50 'dnsresolvh$PRM_FMT_HTML.class'
-rw-r--r-- 1 <username> <usergroup>   680 Mar 25 17:50 'dnsresolvh$PRM_FMT_JSON.class'
-rw-r--r-- 1 <username> <usergroup>   783 Mar 25 17:50 'dnsresolvh$RSC_HTTP_200_OK.class'
-rw-r--r-- 1 <username> <usergroup>  1428 Mar 25 17:50 'dnsresolvh$separator_draw$iter__202__206$fn__207$fn__208.class'
-rw-r--r-- 1 <username> <usergroup>  2637 Mar 25 17:50 'dnsresolvh$separator_draw$iter__202__206$fn__207.class'
-rw-r--r-- 1 <username> <usergroup>   786 Mar 25 17:50 'dnsresolvh$separator_draw$iter__202__206.class'
-rw-r--r-- 1 <username> <usergroup>  1428 Mar 25 17:50 'dnsresolvh$separator_draw$iter__204__208$fn__209$fn__210.class'
-rw-r--r-- 1 <username> <usergroup>  2637 Mar 25 17:50 'dnsresolvh$separator_draw$iter__204__208$fn__209.class'
-rw-r--r-- 1 <username> <usergroup>   786 Mar 25 17:50 'dnsresolvh$separator_draw$iter__204__208.class'
-rw-r--r-- 1 <username> <usergroup>  1549 Mar 25 17:50 'dnsresolvh$separator_draw.class'
-rw-r--r-- 1 <username> <usergroup> 17902 Mar 25 17:50  dnsresolvh__init.class
drwxr-xr-x 3 <username> <usergroup>  4096 Mar 25 17:50  org

srcs:
total 28
drwxr-xr-x 2 <username> <usergroup>  4096 Mar 25 17:50 .
drwxr-xr-x 4 <username> <usergroup>  4096 Mar 25 17:50 ..
-rw-r--r-- 1 <username> <usergroup> 10368 Mar 25 17:50 dnsresolvd.clj
-rw-r--r-- 1 <username> <usergroup>  5554 Mar 25 17:50 dnsresolvh.clj
$
$ file dnsresolvd srcs/* lib/*
dnsresolvd:                                Clojure script text executable
srcs/dnsresolvd.clj:                       Clojure module source, ASCII text
srcs/dnsresolvh.clj:                       Clojure module source, ASCII text
lib/dnsresolvd$dns_lookup.class:           compiled Java class data, version 52.0 (Java 1.8)
...
lib/dnsresolvd$reqhandler.class:           compiled Java class data, version 52.0 (Java 1.8)
...
lib/dnsresolvd$startup.class:              compiled Java class data, version 52.0 (Java 1.8)
lib/dnsresolvd__init.class:                compiled Java class data, version 52.0 (Java 1.8)
lib/dnsresolvh$add_response_headers.class: compiled Java class data, version 52.0 (Java 1.8)
...
lib/dnsresolvh$cleanups_fixate.class:      compiled Java class data, version 52.0 (Java 1.8)
...
lib/dnsresolvh$separator_draw.class:       compiled Java class data, version 52.0 (Java 1.8)
lib/dnsresolvh__init.class:                compiled Java class data, version 52.0 (Java 1.8)
lib/org:                                   directory
```

## Running

To start up the daemon just specify a TCP port that should be used to listen on for incoming connections.

OpenBSD/amd64 | Ubuntu Server LTS x86-64 | Arch Linux:

```
$ cd src/clojure
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
