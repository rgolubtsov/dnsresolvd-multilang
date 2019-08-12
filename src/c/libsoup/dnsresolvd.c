/*
 * src/c/libsoup/dnsresolvd.c
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (GNOME libsoup-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2019 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

#include "dnsresolvd.h"

/* Main callback. Default request handler. */
void _request_handler(      SoupServer        *dmn,
                            SoupMessage       *msg,
                      const char              *pth,
                            GHashTable        *qry,
                            SoupClientContext *cln,
                            gpointer           usr) {

    char *resp_buffer = "<!DOCTYPE html><html><head><title>" _DMN_NAME \
                        "</title></head><body><div>"         _DMN_NAME \
                        "</div></body></html>";

    char *mtd;

    char *hostname; /* The effective hostname to look up for. */
    char *fmt;      /* The response format selector.          */

    SoupMessageBody *req_body;

    char *req_body_data, *req_body_data_, *param;

    int param_val_len, i;

    bool _fmt;

    char *fmt_[] = {
        _PRM_FMT_HTML,
        _PRM_FMT_JSON
    };

    char *HDR_CONTENT_TYPE_V;

    /* --------------------------------------------------------------------- */
    /* --- Parsing and validating request params - Begin ------------------- */
    /* --------------------------------------------------------------------- */
    mtd = (gchar *) msg->method;

           if (mtd == SOUP_METHOD_GET ) {
        if (qry      != NULL) {
            hostname  = g_hash_table_lookup(qry, "h"); /*
                                                  ^
                                                  |
                                                  +-+
                                                    |
            $ curl 'http://localhost:<port-number>/?h=<hostname>&f=<fmt>'
                                                                 |
                                                  +--------------+
                                                  |
                                                  v */
            fmt       = g_hash_table_lookup(qry, "f");
        } else {
            hostname  = malloc(sizeof(char) * (HOST_NAME_MAX + 1));
            fmt       = malloc(sizeof(char) * (HOST_NAME_MAX + 1));

            hostname  = strcpy(hostname, _EMPTY_STRING);
            fmt       = strcpy(fmt,      _EMPTY_STRING);
        }
    } else if (mtd == SOUP_METHOD_POST) {
        req_body = msg->request_body;

        hostname = malloc(sizeof(char) * (HOST_NAME_MAX + 1));
        fmt      = malloc(sizeof(char) * (HOST_NAME_MAX + 1));

        hostname = strcpy(hostname, _EMPTY_STRING);
        fmt      = strcpy(fmt,      _EMPTY_STRING);

        if((req_body != NULL) && (req_body->length > 0)) {
            req_body_data_ = req_body_data = strdup(req_body->data); /*

            $ curl -d 'h=<hostname>&f=<fmt>' http://localhost:<port_number>
                       |            |
                       |            +--------------------------------------+
                       +-------------------------------------------------+ |
                                                                         | | */
            while (param = strsep(&req_body_data, _AMPER)) {          /* | | */
                param_val_len = strlen(param) - 2;                    /* | |
                                                                         | | */
                       if (strncmp("h=", param, 2) == 0) { /* <----------+ | */
                    hostname = strncpy(hostname,param+2,param_val_len); /* | */
                    hostname[param_val_len] = '\0';                     /* | */
                } else if (strncmp("f=", param, 2) == 0) { /* <------------+ */
                    fmt      = strncpy(fmt,     param+2,param_val_len);
                    fmt     [param_val_len] = '\0';
                }
            }

            free(req_body_data_);
        }
    } else {
        return; /* <== In case of HTTP method not supported. */
    }

    if ((hostname == NULL) || (strlen(hostname) == 0            )
                           || (strlen(hostname)  > HOST_NAME_MAX)) {

        hostname = strcpy(hostname, _DEF_HOSTNAME);
    }

    if ((fmt      == NULL) || (strlen(fmt     )  < 3            )
                           || (strlen(fmt     )  > 4            )) {

        fmt      = strcpy(fmt,      _PRM_FMT_JSON);
    } else {
        for (i = 0; fmt[i]; i++) { fmt[i] = tolower(fmt[i]); }

        _fmt = false;

        for (i = 0; i < 2; i++) {
            if (strcmp(fmt, fmt_[i]) == 0) {
                _fmt = true; break;
            }
        }

        if (!_fmt) {
            fmt  = strcpy(fmt,      _PRM_FMT_JSON);
        }
    }
    /* --------------------------------------------------------------------- */
    /* --- Parsing and validating request params - End --------------------- */
    /* --------------------------------------------------------------------- */

    /* Adding headers to the response. */
    HDR_CONTENT_TYPE_V = add_response_headers(msg->response_headers, fmt);

    soup_message_set_status  (msg, SOUP_STATUS_OK);
    soup_message_set_response(msg, HDR_CONTENT_TYPE_V, SOUP_MEMORY_COPY,
                              resp_buffer, strlen(resp_buffer));

    free(HDR_CONTENT_TYPE_V);

    if ((qry == NULL) || (mtd == SOUP_METHOD_POST)) {
        free(fmt     );
        free(hostname);
    }
}

/* The daemon entry point. */
int main(int argc, char *const *argv) {
    int ret = EXIT_SUCCESS;

    char *daemon_name = argv[0];
    unsigned short port_number;

    char *print_banner_opt = _EMPTY_STRING;

    int argv2_len, i; /*
        ^          ^
        |          |
        +----------+--- Needs this for toupper'ing argv[2] only. */

    SoupServer *dmn = NULL;
    GMainLoop *loop = NULL;
    GError   *error = NULL;

    if (argc > 2) {
        argv2_len = strlen(argv[2]);

        print_banner_opt = malloc(argv2_len);

        for (i = 0; i <= argv2_len; i++) {
            print_banner_opt[i] = toupper(argv[2][i]);
        }
    }

    if (strcmp(print_banner_opt, _PRINT_BANNER_OPT) == 0) {
        _separator_draw(_DMN_DESCRIPTION);

        printf(_DMN_NAME        _COMMA_SPACE_SEP                         \
               _DMN_VERSION_S__ _ONE_SPACE_STRING _DMN_VERSION _NEW_LINE \
               _DMN_DESCRIPTION                                _NEW_LINE \
               _DMN_COPYRIGHT__ _ONE_SPACE_STRING _DMN_AUTHOR  _NEW_LINE);

        _separator_draw(_DMN_DESCRIPTION);
    }

    if (argc > 2) {
        free(print_banner_opt);
    }

    /* Opening the system logger. */
    openlog(NULL, LOG_CONS | LOG_PID, LOG_DAEMON);

    /* Checking for args presence. */
    if (argc == 1) {
        ret = EXIT_FAILURE;

        fprintf(stderr, _ERR_MUST_BE_ONE_TWO_ARGS _NEW_LINE _NEW_LINE,
                         daemon_name, (argc - 1));

        syslog(LOG_ERR, _ERR_MUST_BE_ONE_TWO_ARGS _NEW_LINE _NEW_LINE,
                         daemon_name, (argc - 1));

        fprintf(stderr, _MSG_USAGE_TEMPLATE _NEW_LINE _NEW_LINE, daemon_name);

        _cleanups_fixate(NULL);

        return ret;
    }

    port_number = atoi(argv[1]);

    /* Checking for port correctness. */
    if ((port_number < _MIN_PORT) || (port_number > _MAX_PORT)) {
        ret = EXIT_FAILURE;

        fprintf(stderr, _ERR_PORT_MUST_BE_POSITIVE_INT _NEW_LINE _NEW_LINE,
                         daemon_name);

        syslog(LOG_ERR, _ERR_PORT_MUST_BE_POSITIVE_INT _NEW_LINE _NEW_LINE,
                         daemon_name);

        fprintf(stderr, _MSG_USAGE_TEMPLATE _NEW_LINE _NEW_LINE, daemon_name);

        _cleanups_fixate(NULL);

        return ret;
    }

    /* Creating the HTTP server. */
    dmn = soup_server_new(SOUP_SERVER_SERVER_HEADER, _DMN_NAME, NULL);

    /* Creating the main loop. */
    loop = g_main_loop_new(NULL, FALSE);

    if ((dmn == NULL) || (loop == NULL)) {
        ret = EXIT_FAILURE;

        fprintf(stderr, _ERR_CANNOT_START_SERVER _ERR_SRV_UNKNOWN_REASON
                        _NEW_LINE _NEW_LINE, daemon_name);

        syslog(LOG_ERR, _ERR_CANNOT_START_SERVER _ERR_SRV_UNKNOWN_REASON
                        _NEW_LINE _NEW_LINE, daemon_name);

        _cleanups_fixate(NULL);

        return ret;
    }

    /* Attaching Unix signal handlers to ensure daemon clean shutdown. */
    g_unix_signal_add(SIGINT,  (GSourceFunc) _cleanups_fixate, loop);
    g_unix_signal_add(SIGTERM, (GSourceFunc) _cleanups_fixate, loop);

    /*
     * Attaching HTTP request handlers to process incoming requests
     * and producing the response.
     */
    soup_server_add_handler(dmn, NULL, _request_handler, NULL, NULL);

    /* Setting up the daemon to listen on all TCP IPv4 interfaces. */
    if (soup_server_listen_all(dmn, port_number,
        SOUP_SERVER_LISTEN_IPV4_ONLY, &error)) {

        printf(         _MSG_SERVER_STARTED_1 _NEW_LINE _MSG_SERVER_STARTED_2 \
                        _NEW_LINE, port_number);

        syslog(LOG_INFO,_MSG_SERVER_STARTED_1 _NEW_LINE _MSG_SERVER_STARTED_2 \
                        _NEW_LINE, port_number);

        /* Starting up the daemon by running the main loop. */
        g_main_loop_run(loop);
    } else {
        ret = EXIT_FAILURE;

        if (strstr(error->message, _ERR_ADDR_ALREADY_IN_USE) != NULL) {
            fprintf(stderr, _ERR_CANNOT_START_SERVER _ERR_SRV_PORT_IS_IN_USE
                            _NEW_LINE _NEW_LINE, daemon_name);

            syslog(LOG_ERR, _ERR_CANNOT_START_SERVER _ERR_SRV_PORT_IS_IN_USE
                            _NEW_LINE _NEW_LINE, daemon_name);
        } else {
            fprintf(stderr, _ERR_CANNOT_START_SERVER _ERR_SRV_UNKNOWN_REASON
                            _NEW_LINE _NEW_LINE, daemon_name);

            syslog(LOG_ERR, _ERR_CANNOT_START_SERVER _ERR_SRV_UNKNOWN_REASON
                            _NEW_LINE _NEW_LINE, daemon_name);
        }

        g_clear_error(&error);

        _cleanups_fixate(loop);

        return ret;
    }

    /* Making final cleanups. */
    _cleanups_fixate(loop);

    return ret;
}

/**
 * Adds headers to the response.
 *
 * @param resp_hdrs The response headers object.
 * @param fmt       The response format selector.
 *
 * @return The <code>"Content-Type"</code> response header value
 *         used in the caller's <code>soup_message_set_response()</code>
 *         function.
 */
char *add_response_headers(SoupMessageHeaders *resp_hdrs, const char *fmt) {
    char *HDR_CONTENT_TYPE_V;

    soup_message_headers_append(resp_hdrs, _HDR_CACHE_CONTROL_N,
                                           _HDR_CACHE_CONTROL_V);
    soup_message_headers_append(resp_hdrs, _HDR_EXPIRES_N,
                                           _HDR_EXPIRES_V      );
    soup_message_headers_append(resp_hdrs, _HDR_PRAGMA_N,
                                           _HDR_PRAGMA_V       );

           if (strcmp(fmt, _PRM_FMT_HTML) == 0) {
        HDR_CONTENT_TYPE_V = malloc(sizeof(_HDR_CONTENT_TYPE_V_HTML));
        HDR_CONTENT_TYPE_V = strcpy(HDR_CONTENT_TYPE_V,
                                   _HDR_CONTENT_TYPE_V_HTML);
    } else if (strcmp(fmt, _PRM_FMT_JSON) == 0) {
        HDR_CONTENT_TYPE_V = malloc(sizeof(_HDR_CONTENT_TYPE_V_JSON));
        HDR_CONTENT_TYPE_V = strcpy(HDR_CONTENT_TYPE_V,
                                   _HDR_CONTENT_TYPE_V_JSON);
    }

    return HDR_CONTENT_TYPE_V;
}

/* Helper function. Makes final buffer cleanups, closes streams, etc. */
void _cleanups_fixate(const gpointer loop) {
    /* Stopping the daemon. */
    if ((loop != NULL) && (g_main_loop_is_running(loop))) {
        g_main_loop_quit(loop);
    }

    /* Closing the system logger. */
    closelog();
}

/* Helper function. Draws a horizontal separator banner. */
void _separator_draw(const char *banner_text) {
    unsigned char i = strlen(banner_text);

    do { putchar('='); i--; } while (i); puts(_EMPTY_STRING);
}

/* vim:set nu et ts=4 sw=4: */
