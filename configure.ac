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

if test "${CXX_STD}" = "CXX17"; then
  AC_CHECK_HEADER([execution], [], [])

  if test "${ac_cv_header_execution}" = "yes"; then
    AC_CHECK_LIB([tbb], [main], [HAS_TBB=yes], [])

    if test "${ac_cv_lib_tbb_main}" = "yes"; then
      AC_MSG_CHECKING([whether tbb can be linked against])
      CXXFLAGS="${CXXFLAGS} -std=c++17"
      LIBS="${LIBS} -ltbb"
      AC_LINK_IFELSE(
         [AC_LANG_PROGRAM([#include <execution>],
          [std::execution::par_unseq])],
         [PKG_CXXFLAGS="${PKG_CXXFLAGS} -std=c++17"
          PKG_CPPFLAGS="${PKG_CPPFLAGS} -D_MARKETS_HAS_EXECUTION_POLICIES_"
          PKG_LIBS="${PKG_LIBS} -ltbb"
          AC_MSG_RESULT([yes])],
         [AC_MSG_RESULT([no])])
    fi
  fi
fi


dnl Create the  src/Makevars file.
AC_SUBST([CXX_STD], ["${CXX_STD}"])
AC_SUBST([PKG_CXXFLAGS], ["${PKG_CXXFLAGS}"])
AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS}"])
AC_SUBST([PKG_LIBS], ["${PKG_LIBS}"])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    cxx: ${CXX}
    cxx_std: ${CXX_STD}
    cxxflags: ${PKG_CXXFLAGS}
    cppflags: ${PKG_CPPFLAGS}
    libs:     ${PKG_LIBS}

  --------------------------------------------------
"
