# dnl Process this file with autoconf to produce a configure script.
# anything in this script that works is (c) Edzer Pebezma from sf
# anything that does not is (c) Dewey Dunnington from geovctrs
define([pkgversion], esyscmd([sh -c "grep Version: DESCRIPTION | cut -d' ' -f2 | tr -d '\n'"]))dnl
AC_INIT(geovctrs, [pkgversion], dewey@fishandwhistle.net)
AC_CONFIG_SRCDIR(src/geos-version.cpp)

# find R home and set correct compiler + flags
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  AC_MSG_ERROR([cannot determine R_HOME. Make sure you use R CMD INSTALL!])
fi
RBIN="${R_HOME}/bin/R"

# pick all flags for testing from R
: ${CC=`"${RBIN}" CMD config CC`}
: ${CXX=`"${RBIN}" CMD config CXX`}
: ${CPP=`"${RBIN}" CMD config CPP`}
: ${CFLAGS=`"${RBIN}" CMD config CFLAGS`}
: ${CPPFLAGS=`"${RBIN}" CMD config CPPFLAGS`}
: ${LDFLAGS=`"${RBIN}" CMD config LDFLAGS`}
# AC_SUBST([CC],["clang"])
# AC_SUBST([CXX],["clang++"])
AC_MSG_NOTICE([CC: ${CC}])
AC_MSG_NOTICE([CXX: ${CXX}])

INLIBS="${LIBS}"
INCPPFLAGS="${CPPFLAGS}"
INPKG_CPPFLAGS="${PKG_CPPFLAGS}"
INPKG_LIBS="${PKG_LIBS}"

AC_MSG_NOTICE([${PACKAGE_NAME}: ${PACKAGE_VERSION}])

#
# GEOS:
#
GEOS_CONFIG="geos-config"

GEOS_CONFIG_SET="no"

AC_ARG_WITH([geos-config],
    AC_HELP_STRING([--with-geos-config=GEOS_CONFIG],
           [the location of geos-config]),
           [geos_config=$withval])
if test [ -n "$geos_config" ] ; then
    GEOS_CONFIG_SET="yes"
    AC_SUBST([GEOS_CONFIG],["${geos_config}"])
    AC_MSG_NOTICE(geos-config set to $GEOS_CONFIG)
fi

if test ["$GEOS_CONFIG_SET" = "no"] ; then
  AC_PATH_PROG([GEOS_CONFIG], ["$GEOS_CONFIG"],["no"])
  if test ["$GEOS_CONFIG" = "no"] ; then
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([geos-config not found or not executable.])
  fi
else
  AC_MSG_CHECKING(geos-config exists)
  if test -r "${GEOS_CONFIG}"; then
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([geos-config not found - configure argument error.])
  fi
  AC_MSG_CHECKING(geos-config executable)
  if test -x "${GEOS_CONFIG}"; then
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([geos-config not executable.])
  fi
fi

AC_MSG_CHECKING(geos-config usability)
if test `${GEOS_CONFIG} --version`
then
  GEOS_CLIBS="`${GEOS_CONFIG} --clibs`"
  #GEOS_DEP_CLIBS=`geos-config --static-clibs` -- this gives -m instead of -lm, which breaks clang
  # fixed in 3.7.0 at https://github.com/libgeos/libgeos/pull/73#issuecomment-262208677
  GEOS_DEP_CLIBS=`${GEOS_CONFIG} --static-clibs | sed 's/-m/-lm/g'`
  GEOS_CPPFLAGS=`${GEOS_CONFIG} --cflags`
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
  AC_MSG_ERROR([${GEOS_CONFIG} not usable])
fi

GEOS_VERSION=`${GEOS_CONFIG} --version`
AC_MSG_NOTICE([GEOS: ${GEOS_VERSION}])
AC_MSG_CHECKING([GEOS version >= 3.4.0])
GEOS_VER_DOT=`echo $GEOS_VERSION | tr -d ".[[:alpha:]]"`
if test ${GEOS_VER_DOT} -lt 340 ; then
  AC_MSG_RESULT(no)
  AC_MSG_ERROR([upgrade GEOS to 3.4.0 or later])
else
  AC_MSG_RESULT(yes)
fi

AC_SUBST([PKG_CPPFLAGS], ["${INPKG_CPPFLAGS} ${GEOS_CPPFLAGS}"])
AC_SUBST([PKG_LIBS], ["${INPKG_LIBS}"])

# honor PKG_xx overrides
# for CPPFLAGS we will superfluously double R's flags
# since we'll set PKG_CPPFLAGS with this, but that shouldn't hurt
CPPFLAGS="${CPPFLAGS} ${PKG_CPPFLAGS}"
LIBS="${LIBS} ${PKG_LIBS}"

geosok=yes
AC_CHECK_HEADERS(geos_c.h,,geosok=no)
if test "${geosok}" = no; then
   AC_MSG_ERROR([geos_c.h not found in given locations.])
fi

[cat > geos_test.cpp <<_EOCONF
#include <geos_c.h>
#ifdef __cplusplus
extern "C" {
#endif
static void __errorHandler(const char *fmt, ...) {
    return;
}
static void __warningHandler(const char *fmt, ...) {
    return;
}
int main() {
    GEOSContextHandle_t r = initGEOS_r((GEOSMessageHandler) __warningHandler, (GEOSMessageHandler) __errorHandler);
    finishGEOS_r(r);
}
#ifdef __cplusplus
}
#endif
_EOCONF]

#echo "${CXX} ${CPPFLAGS} -o geos_test geos_test.cpp ${LIBS}"
AC_MSG_CHECKING(geos: linking with ${GEOS_CLIBS})
${CXX} ${CPPFLAGS} -o geos_test geos_test.cpp ${GEOS_CLIBS} 2> errors.txt
if test `echo $?` -ne 0 ; then
 geosok=no
 AC_MSG_RESULT(no)
else
 AC_SUBST([PKG_LIBS], ["${PKG_LIBS} ${GEOS_CLIBS}"])
 AC_MSG_RESULT(yes)
fi

if test "${geosok}" = no; then
  AC_MSG_CHECKING(geos: linking with ${GEOS_DEP_CLIBS})
  ${CXX} ${CPPFLAGS} -o geos_test geos_test.cpp ${GEOS_DEP_CLIBS} 2> errors.txt
  if test `echo $?` -ne 0 ; then
    geosok=no
    AC_MSG_RESULT(no)
    cat errors.txt
    AC_MSG_NOTICE([Install failure: compilation and/or linkage problems.])
    AC_MSG_ERROR([initGEOS_r not found in libgeos_c.])
  else
    AC_SUBST([PKG_LIBS], ["${PKG_LIBS} ${GEOS_DEP_CLIBS}"])
    AC_MSG_RESULT(yes)
  fi
fi

rm -f geos_test errors.txt geos_test.cpp

#
# concluding substitution
#
AC_MSG_NOTICE([Package CPP flags: ${PKG_CPPFLAGS}])
AC_MSG_NOTICE([Package LIBS: ${PKG_LIBS}])

AC_CONFIG_FILES(src/Makevars)
AC_OUTPUT
