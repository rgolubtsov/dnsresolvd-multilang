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

        fprintf(stderr, _ERR_CANNOT_START_SERVER _NEW_LINE _NEW_LINE,
                         daemon_name);

        syslog(LOG_ERR, _ERR_CANNOT_START_SERVER _NEW_LINE _NEW_LINE,
                         daemon_name);

        _cleanups_fixate(NULL);

        return ret;
    }

    /* Attaching Unix signal handlers to ensure daemon clean shutdown. */
    g_unix_signal_add(SIGINT,  (GSourceFunc) _cleanups_fixate, loop);
    g_unix_signal_add(SIGTERM, (GSourceFunc) _cleanups_fixate, loop);

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
