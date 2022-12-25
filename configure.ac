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
  AC_MSG_RESULT([no])],
 [CXX=`"${R_HOME}/bin/R" CMD config CXX17`
  CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXX17FLAGS`
  CXX_STD=CXX17
  AC_MSG_RESULT([yes])])
AC_LANG(C++)
AC_PROG_CXX

PKG_CPPFLAGS="${PKG_CPPFLAGS} -D_MARKETS_HAS_GSL_"
GSL_LIBS=`"${R_HOME}/bin/Rscript" -e "RcppGSL::LdFlags()"`
PKG_LIBS="${PKG_LIBS} ${GSL_LIBS}"

EXECUTION_TEST_PROGRAM="
#include <execution>
int main(void) { const auto& ex = std::execution::par_unseq; return 0; }
"

AS_IF([test "${CXX_STD}" = "CXX17"], [
  AC_MSG_CHECKING([whether execution policies are available])
  CXXFLAGS="$CXXFLAGS -std=gnu++17"
  dnl Is std=c++17 available?
  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([${EXECUTION_TEST_PROGRAM}])],
    [dnl Can we link when using execution policies?
      AC_LINK_IFELSE(
      [AC_LANG_SOURCE([${EXECUTION_TEST_PROGRAM}])],
      [PKG_CPPFLAGS="$PKG_CPPFLAGS -D_MARKETS_HAS_EXECUTION_POLICIES_"
       AC_MSG_RESULT([yes])],
      [dnl If not, do we need tbb for linking?
       LIBS="$LIBS -ltbb"
       AC_LINK_IFELSE(
       [AC_LANG_SOURCE([${EXECUTION_TEST_PROGRAM}])],
       [PKG_CPPFLAGS="$PKG_CPPFLAGS -D_MARKETS_HAS_EXECUTION_POLICIES_"
        PKG_LIBS="$PKG_LIBS -ltbb"
        AC_MSG_RESULT([yes])],
       [AC_MSG_RESULT([no])])])],
    [AC_MSG_RESULT([no])])
])

dnl Create the  src/Makevars file.
AC_SUBST([CXX_STD], ["${CXX_STD}"])
AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS}"])
AC_SUBST([PKG_LIBS], ["${PKG_LIBS}"])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    cxx: ${CXX}
    cxx_std: ${CXX_STD}
    cppflags: ${PKG_CPPFLAGS}
    libs:     ${PKG_LIBS}

  --------------------------------------------------
"
