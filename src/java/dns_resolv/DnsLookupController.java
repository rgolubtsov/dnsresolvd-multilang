/*
 * src/java/dns_resolv/DnsLookupController.java
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (Vertosphere-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2019 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

package dns_resolv;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Vertx;
import io.vertx.core.Context;
import io.vertx.core.Future;

import io.vertx.core.http.HttpServer;

/** The controller class of the daemon. */
public class DnsLookupController extends AbstractVerticle {
    /**
     * Reference to the Vert.x instance that deployed this verticle. &ndash;
     * From the docs.
     */
    private Vertx vertx;

    /** An HTTP and WebSockets server. &ndash; From the docs. */
    private HttpServer server;

    /**
     * Overridden method that initializes the verticle.
     *
     * @param vertx   The deploying Vert.x object instance.
     * @param context The context object of the verticle.
     */
    public void init(final Vertx vertx, final Context context) {
        this.vertx  = vertx;
        this.server = vertx.createHttpServer();
    }

    /**
     * Overridden method that starts up the verticle.
     *
     * @param startFuture The Future object that should be invoked
     *                    after the verticle startup procedure is complete.
     */
    public void start(final Future<Void> startFuture) {
        System.out.println("--- start ---");
    }

    /**
     * Overridden method that undeploys the verticle.
     *
     * @param stopFuture The Future object that should be invoked
     *                   after the verticle undeploy procedure is complete.
     */
    public void stop(final Future<Void> stopFuture) {
        server.close();
    }
}

// vim:set nu et ts=4 sw=4:
