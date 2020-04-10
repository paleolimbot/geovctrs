
#ifndef GEOVCTRS_GEOS_HANDLER_H
#define GEOVCTRS_GEOS_HANDLER_H

// prevents using non-thread-safe GEOSxx functions without _r extension.
#define GEOS_USE_ONLY_R_API
#include <geos_c.h>
#include <memory>
#include <Rcpp.h>

// version constants
#if GEOS_VERSION_MAJOR == 3
# if GEOS_VERSION_MINOR >= 5
#  define HAVE350
# endif
# if GEOS_VERSION_MINOR >= 8
#  define HAVE380
# endif
# if GEOS_VERSION_MINOR == 6
#  if GEOS_VERSION_PATCH >= 1
#   define HAVE361
#  endif
# endif
# if GEOS_VERSION_MINOR >= 7
#  define HAVE361
#  define HAVE370
# endif
#else
# if GEOS_VERSION_MAJOR > 3
#  define HAVE350
#  define HAVE370
#  define HAVE361
#  define HAVE380
# endif
#endif


class GeovctrsGEOSHandler {
public:
  GEOSContextHandle_t context;

  GeovctrsGEOSHandler() {
#ifdef HAVE350
    context = GEOS_init_r();
    GEOSContext_setNoticeHandler_r(this->context, this->handleWarning);
    GEOSContext_setErrorHandler_r(this->context, this->handleError);
#else
    return initGEOS_r((GEOSMessageHandler) this->handleWarning, (GEOSMessageHandler) this->handleError);
#endif
  }

  ~GeovctrsGEOSHandler() {
#ifdef HAVE350
    GEOS_finish_r(context);
#else
    finishGEOS_r(context);
#endif
  }

  static std::string runtimeVersion() {
    return GEOSversion();
  }

  static std::string buildVersion() {
    return GEOS_CAPI_VERSION;
  }

  static void handleError(const char *fmt, ...) {

    char buf[BUFSIZ], *p;
    va_list ap;
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';

    throw Rcpp::exception(buf);
  }

  // no idea how to get this to fire
  // nocov start
  static void handleWarning(const char *fmt, ...) {

    char buf[BUFSIZ], *p;
    va_list ap;
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';

    Rcpp::Function warning("warning");
    warning(buf);

    return;
  }
  // nocov end
};

#endif
