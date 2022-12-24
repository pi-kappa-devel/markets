AC_INIT([markets],[1.1.4])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

AC_MSG_CHECKING([whether R CXX17 configuration is available])
AS_IF(
 ["${R_HOME}/bin/R" CMD config CXX17 | grep ERROR >/dev/null 2>/dev/null],
 [CXX=`"${R_HOME}/bin/R" CMD config CXX`
  CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
  HAS_CXX17="no"
  AC_MSG_RESULT([no])],
 [CXX=`"${R_HOME}/bin/R" CMD config CXX17`
  CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXX17FLAGS`
  HAS_CXX17="yes"
  AC_MSG_RESULT([yes])])

AC_LANG(C++)
AC_PROG_CXX

GSL_TEST_PROGRAM="
#include <gsl/gsl_multimin.h>
int main(void) { gsl_vector v; }
"

GSL_FLAGS=`gsl-config --cflags`
GSL_LIBS=`gsl-config --libs`
AS_IF([test "${GSL_LIBS}" != ""], [
  AC_MSG_CHECKING([whether libgsl is available])
  OLD_CPPFLAGS="$CPPFLAGS"
  OLD_LIBS="$LIBS"
  CPPFLAGS="$CPPFLAGS ${GSL_FLAGS}"
  LIBS="$LIBS ${GSL_LIBS}"
  AC_LINK_IFELSE(
    [AC_LANG_SOURCE([${GSL_TEST_PROGRAM}])],
    [PKG_CPPFLAGS="${PKG_CPPFLAGS} -D_MARKETS_HAS_GSL_ ${GSL_FLAGS}"
     PKG_LIBS="${PKG_LIBS} ${GSL_LIBS}"
     AC_MSG_RESULT([yes])],
    [CPPFLAGS="${OLD_CPPFLAGS}"
     LIBS="${OLD_LIBS}"
     AC_MSG_RESULT([no])])
])

EXECUTION_TEST_PROGRAM="
#include <execution>
int main(void) { const auto& ex = std::execution::par_unseq; }
"

AS_IF([test "${HAS_CXX17}" = "yes"], [
  AC_MSG_CHECKING([whether execution policies are available])
  OLD_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -std=c++17"
  dnl Is std=c++17 available?
  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([${EXECUTION_TEST_PROGRAM}])],
    [dnl Can we link when using execution policies?
      AC_LINK_IFELSE(
      [AC_LANG_SOURCE([${EXECUTION_TEST_PROGRAM}])],
      [PKG_CPPFLAGS="$PKG_CPPFLAGS -D_MARKETS_HAS_EXECUTION_POLICIES_"
       PKG_CXXFLAGS="$PKG_CXXFLAGS -std=c++17"
       AC_MSG_RESULT([yes])],
      [dnl If not, do we need tbb for linking?
       OLD_LIBS="$LIBS"
       LIBS="$LIBS -ltbb"
       AC_LINK_IFELSE(
       [AC_LANG_SOURCE([${EXECUTION_TEST_PROGRAM}])],
       [PKG_CPPFLAGS="$PKG_CPPFLAGS -D_MARKETS_HAS_EXECUTION_POLICIES_"
        PKG_CXXFLAGS="$PKG_CXXFLAGS -std=c++17"
        PKG_LIBS="$PKG_LIBS -ltbb"
        AC_MSG_RESULT([yes])],
       [CXXFLAGS="${OLD_CXXFLAGS}"
        LIBS="${OLD_LIBS}"
        AC_MSG_RESULT([no])])])],
    [CXXFLAGS="${OLD_CXXFLAGS}"
     AC_MSG_RESULT([no])])
])

dnl Create the  src/Makevars file.
AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS}"])
AC_SUBST([PKG_CXXFLAGS], ["${PKG_CXXFLAGS}"])
AC_SUBST([PKG_LIBS], ["${PKG_LIBS}"])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    cxx: ${CXX}
    cppflags: ${PKG_CPPFLAGS}
    cxxflags: ${PKG_CXXFLAGS}
    libs:     ${PKG_LIBS}

  --------------------------------------------------
"
