/*
 * src/c/dnsresolvd.c
 * ============================================================================
 * DNS Resolver Daemon (dnsresolvd). Version 0.1
 * ============================================================================
 * A daemon that performs DNS lookups for the given hostname
 * passed in an HTTP request, with the focus on its implementation
 * using various programming languages. (GNU libmicrohttpd-boosted impl.)
 * ============================================================================
 * Copyright (C) 2017-2018 Radislav (Radicchio) Golubtsov
 *
 * (See the LICENSE file at the top of the source tree.)
 */

#include "dnsresolvd.h"

/*
 * Helper structure. Used to iterate over HTTP GET query params
 * and over HTTP POST request body data items.
 */
struct _params {
    char *hostname;
    char *fmt;
};

/**
 * The IP version (family) used to look up in DNS:
 * <ul>
 * <li><code>4</code> &ndash; IPv4</li>
 * <li><code>6</code> &ndash; IPv6</li>
 * </ul>
 */
unsigned short ver;

/*
 * Query parse callback.
 * Iterates over query params given as key-value pairs.
 */
int _query_params_iterator(      void          *cls,
                           enum  MHD_ValueKind  kind,
                           const char          *key,
                           const char          *value) {

    int ret = MHD_YES;

    if (kind != MHD_GET_ARGUMENT_KIND) {
        ret = MHD_NO; return ret; /* Processing HTTP GET requests only. */
    }

    /*
     * http://localhost:<port_number>/?h=<hostname>&f=<fmt>
     *                                 |
     *               +-----------------+
     *               |
     *               v
     */
    if (strcmp(key, "h") == 0) {
        /*
         * /?h_____
         *      |
         *      v
         */
        if (value != NULL) {
            /*
             *   /?h=_____
             *         |
             *         v
             */
            if (strlen(value) > 0) {
                ((struct _params *) cls)->hostname  =
         strcpy(((struct _params *) cls)->hostname, value);
            }
        }
    /*
     * http://localhost:<port_number>/?h=<hostname>&f=<fmt>
     *                                              |
     *                      +-----------------------+
     *                      |
     *                      v
     */
    } else if (strcmp(key, "f") == 0) {
        /*
         *  &f_____
         *      |
         *      v
         */
        if (value != NULL) {
            /*
             *    &f=_____
             *         |
             *         v
             */
            if (strlen(value) > 0) {
                ((struct _params *) cls)->fmt  =
         strcpy(((struct _params *) cls)->fmt, value);
            }
        }
    } else {
        ret = MHD_NO;
    }

    return ret;
}

/*
 * POST body data parse callback.
 * Iterates over HTTP POST request body data given as key-value pairs.
 */
int _post_data_iterator(      void          *cls,
                        enum  MHD_ValueKind  kind,
                        const char          *key,
                        const char          *filename,
                        const char          *content_type,
                        const char          *transfer_encoding,
                        const char          *data,
                              uint64_t       off,
                              size_t         size) {

    int ret = MHD_YES;

    if ((kind != MHD_POSTDATA_KIND) || (off > 0)) {
        ret = MHD_NO; return ret; /* Processing HTTP POST requests only. */
    }

    /*
     *$ curl -d 'h=<hostname>&f=<fmt>' http://localhost:<port_number>
     *           |
     *           +---+
     *               |
     *               v
     */
    if (strcmp(key, "h") == 0) {
        /*
         * 'h_____
         *     |
         *     v
         */
        if (data != NULL) {
            /*
             *    'h=_____
             *         |
             *         v
             */
            if (strlen(data) > 0) {}
        }
    /*
     *  $ curl -d 'h=<hostname>&f=<fmt>' http://localhost:<port_number>
     *                          |
     *                      +---+
     *                      |
     *                      v
     */
    } else if (strcmp(key, "f") == 0) {
        /*
         * &f_____
         *     |
         *     v
         */
        if (data != NULL) {
            /*
             *    &f=_____
             *         |
             *         v
             */
            if (strlen(data) > 0) {}
        }
    } else {
        ret = MHD_NO;
    }

    return ret;
}

/* Main callback. Actually serving the request here. */
int _request_handler(       void            *cls,
                     struct MHD_Connection  *connection,
                     const  char            *url,
                     const  char            *method,
                     const  char            *version,
                     const  char            *upload_data,
                            size_t          *upload_data_size,
                            void           **con_cls) {

    int ret = MHD_YES;

    #define RESP_TEMPLATE_1 "<!DOCTYPE html>"                                                                  _NEW_LINE \
"<html lang=\"en-US\" dir=\"ltr\">"                                                                            _NEW_LINE \
"<head>"                                                                                                       _NEW_LINE \
"<meta http-equiv=\"" MHD_HTTP_HEADER_CONTENT_TYPE "\"    content=\"" _HDR_CONTENT_TYPE_HTML "\"           />" _NEW_LINE \
"<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"                            />"                       _NEW_LINE \
"<meta       name=\"viewport\"        content=\"width=device-width,initial-scale=1\" />"                       _NEW_LINE \
"<title>" _DMN_NAME "</title>"                                                                                 _NEW_LINE \
"</head>"                                                                                                      _NEW_LINE \
"<body>"                                                                                                       _NEW_LINE \
"<div>"

    #define RESP_TEMPLATE_2 _ONE_SPACE_STRING     \
                            _DAT_VERSION_V

    #define RESP_TEMPLATE_3 _ERR_PREFIX           \
                            _COLON_SPACE_SEP      \
                            _ERR_COULD_NOT_LOOKUP

    #define RESP_TEMPLATE_4 "</div>"  _NEW_LINE   \
                            "</body>" _NEW_LINE   \
                            "</html>" _NEW_LINE

    #define CB1   "{\""
    #define CB2   "\"}"
    #define DQ1 "\":\""
    #define DQ2 "\",\""

    #define MAX_PP_PARSE_BUFF_SIZE 1024

    enum MHD_ValueKind params_kind = MHD_HEADER_KIND;

    struct _params *params;

    char *hostname; /* The effective hostname to look up for. */
    char *fmt;      /* The response format selector.          */

    int num_hdrs;

    struct MHD_PostProcessor *pp;

    char *fmt_[] = {
        _PRM_FMT_HTML,
        _PRM_FMT_JSON
    };

    char *addr;
    char  ver_str[2];
    char *resp_buffer;
    char *HDR_CONTENT_TYPE_V;

    bool _fmt;
    bool lookup_error;

    struct MHD_Response *resp;

/* --- (1) Parse upload_data by ourselves -------------------------------------
  */bool is_____h, is_____f; char param[HOST_NAME_MAX + 1]; int i;/*
---------------------------------------------------------------------------- */

    /* --------------------------------------------------------------------- */
    /* --- Parsing and validating request params - Begin ------------------- */
    /* --------------------------------------------------------------------- */
           if (strcmp(method, MHD_HTTP_METHOD_GET ) == 0) {
        params_kind = MHD_GET_ARGUMENT_KIND;
    } else if (strcmp(method, MHD_HTTP_METHOD_POST) == 0) {
        params_kind =     MHD_POSTDATA_KIND;
    }

    params           = malloc(sizeof(struct _params            ));
    params->hostname = malloc(sizeof(char) * (HOST_NAME_MAX + 1));
    params->fmt      = malloc(sizeof(char) * (4             + 1));
            hostname = malloc(sizeof(char) * (HOST_NAME_MAX + 1));
            fmt      = malloc(sizeof(char) * (4             + 1));

    params->hostname = strcpy(params->hostname, _EMPTY_STRING);
    params->fmt      = strcpy(params->fmt,      _EMPTY_STRING);

           if (params_kind == MHD_GET_ARGUMENT_KIND) {
        num_hdrs =
            MHD_get_connection_values(connection, params_kind, NULL, NULL);

        if (num_hdrs > 0) {
            MHD_get_connection_values(connection,
                                      params_kind,
                                     _query_params_iterator,
                                      params);
        }

        hostname = strcpy(hostname, params->hostname);
        fmt      = strcpy(fmt,      params->fmt     );
    } else if (params_kind ==     MHD_POSTDATA_KIND) {
        if (*con_cls == NULL) {
            pp = MHD_create_post_processor(connection,
                                           MAX_PP_PARSE_BUFF_SIZE,
                                          _post_data_iterator,
                                           NULL);

            if (pp == NULL) {
                ret = MHD_NO; return ret;
            }

            *con_cls = (void *) pp;

            return ret;
        } else {
            pp = *con_cls;

            if (*upload_data_size > 0) {
/* --- (2) Parse upload_data by ourselves -------------------------------------
              */is_____h = false;
                is_____f = false;

                while (sscanf(upload_data, "%[^=&]%n", param, &i) == 1) {
                    if (strcmp(param, "h") == 0) {
                        is_____h = true;
                        upload_data += i;
                        upload_data++; continue;
                    }

                    if (strcmp(param, "f") == 0) {
                        is_____f = true;
                        upload_data += i;
                        upload_data++; continue;
                    }

                    if (is_____h) {
                        hostname = strcpy(hostname, param);
                        is_____h = false;
                    }

                    if (is_____f) {
                        fmt      = strcpy(fmt,      param);
                        is_____f = false;
                    }

                    upload_data += i;
                    upload_data++;
                }/*
---------------------------------------------------------------------------- */
                MHD_post_process(pp, upload_data, *upload_data_size);

                *upload_data_size = 0;

                return ret;
            }
        }
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

    addr = malloc(INET6_ADDRSTRLEN);

    /* Performing DNS lookup for the given hostname. */
    addr = dns_lookup(addr, hostname);

    lookup_error = (strcmp(addr, _ERR_PREFIX) == 0);

    if (!lookup_error) {
        sprintf(ver_str, "%u", ver);
    }

           if (strcmp(fmt, _PRM_FMT_HTML) == 0) {
        if (!lookup_error) {
            resp_buffer = malloc(sizeof(RESP_TEMPLATE_1  )
                               + strlen(hostname         )
                               + sizeof(_ONE_SPACE_STRING)
                               + strlen(addr             )
                               + sizeof(RESP_TEMPLATE_2  )
                               + strlen(ver_str          )
                               + sizeof(RESP_TEMPLATE_4  ));
        } else {
            resp_buffer = malloc(sizeof(RESP_TEMPLATE_1  )
                               + strlen(hostname         )
                               + sizeof(_ONE_SPACE_STRING)
                               + sizeof(RESP_TEMPLATE_3  )
                               + sizeof(RESP_TEMPLATE_4  ));
        }

        resp_buffer = strcpy(resp_buffer, RESP_TEMPLATE_1    );
        resp_buffer = strcat(resp_buffer, hostname           );
        resp_buffer = strcat(resp_buffer, _ONE_SPACE_STRING  );

        if (!lookup_error) {
            resp_buffer = strcat(resp_buffer, addr           );
            resp_buffer = strcat(resp_buffer, RESP_TEMPLATE_2);
            resp_buffer = strcat(resp_buffer, ver_str        );
        } else {
            resp_buffer = strcat(resp_buffer, RESP_TEMPLATE_3);
        }

        resp_buffer = strcat(resp_buffer, RESP_TEMPLATE_4    );
    } else if (strcmp(fmt, _PRM_FMT_JSON) == 0) {
        if (!lookup_error) {
            resp_buffer = malloc(sizeof(CB1                  )
                               + sizeof(_DAT_HOSTNAME_N      )
                               + sizeof(DQ1                  )
                               + strlen(hostname             )
                               + sizeof(DQ2                  )
                               + sizeof(_DAT_ADDRESS_N       )
                               + sizeof(DQ1                  )
                               + strlen(addr                 )
                               + sizeof(DQ2                  )
                               + sizeof(_DAT_VERSION_N       )
                               + sizeof(DQ1                  )
                               + sizeof(_DAT_VERSION_V       )
                               + strlen(ver_str              )
                               + sizeof(CB2                  ));
        } else {
            resp_buffer = malloc(sizeof(CB1                  )
                               + sizeof(_DAT_HOSTNAME_N      )
                               + sizeof(DQ1                  )
                               + strlen(hostname             )
                               + sizeof(DQ2                  )
                               + sizeof(_ERR_PREFIX          )
                               + sizeof(DQ1                  )
                               + sizeof(_ERR_COULD_NOT_LOOKUP)
                               + sizeof(CB2                  ));
        }

        resp_buffer = strcpy(resp_buffer, CB1                      );
        resp_buffer = strcat(resp_buffer, _DAT_HOSTNAME_N          );
        resp_buffer = strcat(resp_buffer, DQ1                      );
        resp_buffer = strcat(resp_buffer, hostname                 );
        resp_buffer = strcat(resp_buffer, DQ2                      );

        if (!lookup_error) {
            resp_buffer = strcat(resp_buffer, _DAT_ADDRESS_N       );
            resp_buffer = strcat(resp_buffer, DQ1                  );
            resp_buffer = strcat(resp_buffer, addr                 );
            resp_buffer = strcat(resp_buffer, DQ2                  );
            resp_buffer = strcat(resp_buffer, _DAT_VERSION_N       );
            resp_buffer = strcat(resp_buffer, DQ1                  );
            resp_buffer = strcat(resp_buffer, _DAT_VERSION_V       );
            resp_buffer = strcat(resp_buffer, ver_str              );
        } else {
            resp_buffer = strcat(resp_buffer, _ERR_PREFIX          );
            resp_buffer = strcat(resp_buffer, DQ1                  );
            resp_buffer = strcat(resp_buffer, _ERR_COULD_NOT_LOOKUP);
        }

        resp_buffer = strcat(resp_buffer, CB2                      );
    } else {
        resp_buffer = '\0';
    }

    free(addr);

    /* Creating the response. */
    resp = MHD_create_response_from_buffer(strlen(resp_buffer),
                                           (void *) resp_buffer,
                                           MHD_RESPMEM_MUST_FREE);

    /*
     * Note: Do not manually free the response buffer
     *       due to the MHD_RESPMEM_MUST_FREE mode is selected --
     *       MHD itself will take care of it.
     *
     *       free(resp_buffer);
     */

    if (resp == NULL) {
        ret = MHD_NO;

        free(        fmt     );
        free(        hostname);
        free(params->fmt     );
        free(params->hostname);
        free(params          );

        return ret;
    }

    HDR_CONTENT_TYPE_V = malloc(MAX(
        sizeof(_HDR_CONTENT_TYPE_HTML),
        sizeof(_HDR_CONTENT_TYPE_JSON)
    ));

           if (strcmp(fmt, _PRM_FMT_HTML) == 0) {
        HDR_CONTENT_TYPE_V = strcpy(HDR_CONTENT_TYPE_V,_HDR_CONTENT_TYPE_HTML);
    } else if (strcmp(fmt, _PRM_FMT_JSON) == 0) {
        HDR_CONTENT_TYPE_V = strcpy(HDR_CONTENT_TYPE_V,_HDR_CONTENT_TYPE_JSON);
    }

    /* Adding headers to the response. */
    if (MHD_add_response_header(resp, MHD_HTTP_HEADER_CONTENT_TYPE,
                                HDR_CONTENT_TYPE_V) == MHD_NO) {return MHD_NO;}

    if (MHD_add_response_header(resp, MHD_HTTP_HEADER_CACHE_CONTROL,
                               _HDR_CACHE_CONTROL ) == MHD_NO) {return MHD_NO;}

    if (MHD_add_response_header(resp, MHD_HTTP_HEADER_EXPIRES,
                               _HDR_EXPIRES       ) == MHD_NO) {return MHD_NO;}

    if (MHD_add_response_header(resp, MHD_HTTP_HEADER_PRAGMA,
                               _HDR_PRAGMA        ) == MHD_NO) {return MHD_NO;}

    free(HDR_CONTENT_TYPE_V);

    free(        fmt     );
    free(        hostname);
    free(params->fmt     );
    free(params->hostname);
    free(params          );

    /* Enqueueing the response to transmit to. */
    ret = MHD_queue_response(connection, MHD_HTTP_OK, resp);

    /* Destroying the response. */
    MHD_destroy_response(resp);

    return ret;
}

/* Last callback. Finalizes the request, releases resources. */
void _request_finalizer(       void                        *cls,
                        struct MHD_Connection              *connection,
                               void                       **con_cls,
                        enum   MHD_RequestTerminationCode   toe) {

    if (*con_cls != NULL) {
        MHD_destroy_post_processor(*con_cls);

        *con_cls = NULL;
    }
}

/**
 * Performs DNS lookup action for the given hostname,
 * i.e. (in this case) IP address retrieval by hostname.
 *
 * @param addr     The buffer to store the IP address retrieved.
 * @param hostname The effective hostname to look up for.
 *
 * @return The IP address of the analyzing host/service.
 */
char *dns_lookup(char *addr, const char *hostname) {
    struct hostent *hent;

    hent = gethostbyname2(hostname, AF_INET);

    /*
     * If the host doesn't have the A record (IPv4),
     * trying to find its AAAA record (IPv6).
     */
    if (hent == NULL) {
        hent = gethostbyname2(hostname, AF_INET6);

        if (hent == NULL) {
            addr = strcpy(addr, _ERR_PREFIX);
        } else {
            addr = (char *) inet_ntop(AF_INET6, hent->h_addr_list[0], addr,
                                         INET6_ADDRSTRLEN);

            ver  = 6;
        }
    } else {
        addr     = (char *) inet_ntop(AF_INET,  hent->h_addr_list[0], addr,
                                         INET_ADDRSTRLEN);

        ver      = 4;
    }

    return addr;
}

/* Helper function. Makes final buffer cleanups, closes streams, etc. */
void _cleanups_fixate() {
    /* Closing the system logger. */
    closelog();
}

/* Helper function. Draws a horizontal separator banner. */
void _separator_draw(const char *banner_text) {
    unsigned char i = strlen(banner_text);

    do { putchar('='); i--; } while (i); puts(_EMPTY_STRING);
}

/* The daemon entry point. */
int main(int argc, char *const *argv) {
    int ret = EXIT_SUCCESS;

    char *daemon_name = argv[0];
    unsigned short port_number;

    struct MHD_Daemon *daemon;

    int c; /* <== Needs this for Ctrl+C hitting check only. */

    char *print_banner_opt = _EMPTY_STRING;

    if (argc > 2) {
        print_banner_opt = malloc(strlen(           argv[2]));
        print_banner_opt = strcpy(print_banner_opt, argv[2]);
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

        _cleanups_fixate();

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

        _cleanups_fixate();

        return ret;
    }

    /* Creating, configuring, and starting the server. */
    daemon = MHD_start_daemon(/* MHD_USE_INTERNAL_POLLING_THREAD, */
/*
 * The following commit deprecates the use of the MHD_USE_SELECT_INTERNALLY
 * flag and introduces the new one instead -- MHD_USE_INTERNAL_POLLING_THREAD:
 *
 *     https://gnunet.org/git/libmicrohttpd.git/commit/src/include/microhttpd.h
 *                                 ?id=a48ca85fb74479a8a81e137138a30fab58f3eb1e
 *
 * In Ubuntu 16.04 LTS the GNU libmicrohttpd package is of version 0.9.44,
 * hence it needs to use the old one as the only option to go.))
 */
                              MHD_USE_SELECT_INTERNALLY,
                              port_number, /*    ^^^^^^^    */
                              NULL,        /*  Thread pool  */
                              NULL,
                              &_request_handler,
                              NULL,
                              MHD_OPTION_NOTIFY_COMPLETED,
                              &_request_finalizer,
                              NULL,
    /* Thread pool >>>>>>> */ MHD_OPTION_THREAD_POOL_SIZE, 4, MHD_OPTION_END);

    if (daemon == NULL) {
        ret = EXIT_FAILURE;

        fprintf(stderr, _ERR_CANNOT_START_SERVER _NEW_LINE _NEW_LINE,
                         daemon_name);

        syslog(LOG_ERR, _ERR_CANNOT_START_SERVER _NEW_LINE _NEW_LINE,
                         daemon_name);

        _cleanups_fixate();

        return ret;
    }

    /* Serving the request. (Hit Ctrl+C to terminate the daemon.) */
    printf(          _MSG_SERVER_STARTED_1 _NEW_LINE _MSG_SERVER_STARTED_2 \
                     _NEW_LINE, port_number);

    syslog(LOG_INFO, _MSG_SERVER_STARTED_1 _NEW_LINE _MSG_SERVER_STARTED_2 \
                     _NEW_LINE, port_number);

    while ((c = getchar()) != EOF) {}

    /* Stopping the server. */
    MHD_stop_daemon(daemon);

    /* Making final cleanups. */
    _cleanups_fixate();

    return ret;
}

/* vim:set nu et ts=4 sw=4: */
