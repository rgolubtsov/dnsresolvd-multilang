/*
 * src/java/dns_resolv/ControllerHelper.java
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (Vertosphere-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2020 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

package dns_resolv;

import java.net.URLClassLoader;
import java.net.URL;
import java.net.MalformedURLException;
import java.lang.reflect.Field;
import sun.misc.URLClassPath;

import org.graylog2.syslog4j.impl.unix.UnixSyslog;

import io.vertx.core.http.HttpServerResponse;

/** The helper for the controller class and related ones. */
public class ControllerHelper {
    // Helper constants.
    public static final int    EXIT_FAILURE     =    1; //    Failing exit status.
    public static final int    EXIT_SUCCESS     =    0; // Successful exit status.
    public static final String EMPTY_STRING     =   "";
    public static final String ONE_SPACE_STRING =  " ";
    public static final String AMPER            =  "&";
    public static final String COLON_SPACE_SEP  = ": ";
    public static final String COMMA_SPACE_SEP  = ", ";
    public static final String PRINT_BANNER_OPT = "-V";
    public static final String NEW_LINE         = System.lineSeparator();

    // Common error messages.
    public static final String ERR_PREFIX                    = "error";
    public static final String ERR_PORT_MUST_BE_POSITIVE_INT = ": <port_number> must be "
                                                             + "a positive integer value, "
                                                             + "in the range 1024-49151.";
    public static final String ERR_CANNOT_START_SERVER       = ": FATAL: Cannot start server ";
    public static final String ERR_SRV_UNKNOWN_REASON        = "for an unknown reason. "
                                                             +  "Exiting...";
    public static final String ERR_SRV_PORT_IS_IN_USE        = "due to the port requested "
                                                             +  "is in use. Exiting...";
    public static final String ERR_COULD_NOT_LOOKUP          = "could not lookup hostname";
    public static final String ERR_ADDR_ALREADY_IN_USE       = "Address already in use";

    // Print this error message when there are no any args passed.
    public static final String ERR_MUST_BE_ONE_TWO_ARGS_1 = ": There must be one or two args passed: ";
    public static final String ERR_MUST_BE_ONE_TWO_ARGS_2 = " args found";

    // Print this usage info just after any inappropriate input.
    public static final String MSG_USAGE_TEMPLATE_1 = "Usage: ";
    public static final String MSG_USAGE_TEMPLATE_2 = " <port_number> [-V]";

    /** Constant: The minimum port number allowed. */
    public static final int MIN_PORT = 1024;

    /** Constant: The maximum port number allowed. */
    public static final int MAX_PORT = 49151;

    // Common notification messages.
    public static final String MSG_SERVER_STARTED_1 = "Server started on port ";
    public static final String MSG_SERVER_STARTED_2 = "=== Hit Ctrl+C to terminate it.";

    // HTTP request params.
    public static final String PRM_FMT_HTML = "html";
    public static final String PRM_FMT_JSON = "json";

    // HTTP response headers and status codes.
    public static final String HDR_CONTENT_TYPE_N      = "Content-Type";
    public static final String HDR_CONTENT_TYPE_V_HTML = "text/html; charset=UTF-8";
    public static final String HDR_CONTENT_TYPE_V_JSON = "application/json";
    public static final String HDR_CONTENT_LENGTH_N    = "Content-Length";
    public static final String HDR_CACHE_CONTROL_N     = "Cache-Control";
    public static final String HDR_CACHE_CONTROL_V     = "no-cache, no-store, "
                                                       + "must-revalidate";
    public static final String HDR_EXPIRES_N           = "Expires";
    public static final String HDR_EXPIRES_V           = "Thu, 01 Dec 1994 16:00:00 GMT";
    public static final String HDR_PRAGMA_N            = "Pragma";
    public static final String HDR_PRAGMA_V            = "no-cache";
    public static final int    RSC_HTTP_200_OK         = 200;

    // Response data names.
    public static final String DAT_HOSTNAME_N = "hostname";
    public static final String DAT_ADDRESS_N  = "address";
    public static final String DAT_VERSION_N  = "version";
    public static final String DAT_VERSION_V  = "IPv";

    // Daemon name, version, and copyright banners.
    public static final String DMN_NAME        = "DNS Resolver Daemon (dnsresolvd)";
    public static final String DMN_DESCRIPTION = "Performs DNS lookups for the given "
                                               + "hostname passed in an HTTP request";
    public static final String DMN_VERSION_S__ = "Version";
    public static final String DMN_VERSION     = "0.1";
    public static final String DMN_COPYRIGHT__ = "Copyright (C) 2017-2020";
    public static final String DMN_AUTHOR      = "Radislav Golubtsov <ragolubtsov@my.com>";

    /** Constant: The default hostname to look up for. */
    public static final String DEF_HOSTNAME = "openbsd.org";

    // --- add_classpath()-related constants.
    public static final String DEP_PROT = "file:";
    public static final String DEP_PREF = DEP_PROT
        + System.getProperty("user.home") + "/.m2/repository/";
    public static final String DEP_URL0 = DEP_PREF
        + "org/graylog2/syslog4j/0.9.61-SNAPSHOT/syslog4j-0.9.61-SNAPSHOT.jar";
//      + "org/graylog2/syslog4j/0.9.60/syslog4j-0.9.60.jar";
//      + "org/graylog2/syslog4j/0.9.61/syslog4j-0.9.61.jar";
//                               ^ ^ ^
//                               | | |
//                          +----+ | |
//                          | +----+ |
//                          | | +----+
//                          | | |
//  Note: When the syslog4j 0.9.61 release will be out, including the following
//        fix: https://github.com/graylog-labs/syslog4j-graylog2/pull/27 ,
//        the daemon will be ready to run on OpenBSD without adjusting/
//        tweaking of building its extra classpath in any fitted manner.
    public static final String DEP_URL1 = DEP_PREF
        + "net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar";

    // Helper method. Adds a custom classpath entry (dir/jar).
    public static void add_classpath() {
        // --- Var names stand for: -------+
        // ucl ==> java.net.URLClassLoader |
        // fld ==> java.lang.reflect.Field |
        // ucp ==> sun.misc.URLClassPath   |
        // --------------------------------+

        URLClassLoader ucl = (URLClassLoader) ClassLoader.getSystemClassLoader();
        //URL[]urls=ucl.getURLs();for(int i=0;i<urls.length;i++)System.out.println(urls[i]);
        Field          fld = URLClassLoader.class.getDeclaredFields()[0];//System.out.println(fld);
        fld.setAccessible(true); // <== Important: suppressing Java language access checking.
        URLClassPath ucp = null;              try {
                       ucp = (URLClassPath) fld.get(ucl);//System.out.println(ucp);
        } catch (IllegalAccessException e) {} try {
        ucp.addURL(new URL(DEP_URL0));//System.out.println();
        //     urls=ucl.getURLs();for(int i=0;i<urls.length;i++)System.out.println(urls[i]);
        ucp.addURL(new URL(DEP_URL1));//System.out.println();
        //     urls=ucl.getURLs();for(int i=0;i<urls.length;i++)System.out.println(urls[i]);
        } catch (MalformedURLException  e) {}
    }

    /**
     * Adds headers to the response.
     *
     * @param resp The HTTP response object.
     * @param fmt  The response format selector.
     */
    public static void add_response_headers(final HttpServerResponse resp,
                                            final String             fmt) {

        String HDR_CONTENT_TYPE_V = EMPTY_STRING;

               if (fmt.compareTo(PRM_FMT_HTML) == 0) {
            HDR_CONTENT_TYPE_V = HDR_CONTENT_TYPE_V_HTML;
        } else if (fmt.compareTo(PRM_FMT_JSON) == 0) {
            HDR_CONTENT_TYPE_V = HDR_CONTENT_TYPE_V_JSON;
        }

        resp.putHeader(HDR_CONTENT_TYPE_N,  HDR_CONTENT_TYPE_V );
        resp.putHeader(HDR_CACHE_CONTROL_N, HDR_CACHE_CONTROL_V);
        resp.putHeader(HDR_EXPIRES_N,       HDR_EXPIRES_V      );
        resp.putHeader(HDR_PRAGMA_N,        HDR_PRAGMA_V       );
    }

    // Helper method. Makes final buffer cleanups, closes streams, etc.
    public static void cleanups_fixate(final UnixSyslog log) {
        // Closing the system logger.
        // --- Calling <syslog.h> closelog(); ---
        log.shutdown();
    }

    // Helper method. Draws a horizontal separator banner.
    public static void separator_draw(final String banner_text) {
        int i = banner_text.length();

        do { System.out.print('='); i--; } while (i > 0); System.out.println();
    }
}

// vim:set nu et ts=4 sw=4:
