AC_PREREQ(2.61)
AC_INIT([brianscheme], [0.9], [http://github.com/netguy204/brianscheme/issues])

AM_INIT_AUTOMAKE

AC_PROG_CC

AC_CONFIG_FILES([Makefile])

PKG_CHECK_MODULES([libffi], [libffi >= 3.0])

AC_CHECK_LIB([z], [gzread], ,[AC_MSG_ERROR([zlib not found, see http://www.zlib.net])])
AC_CHECK_LIB([ltdl], [lt_dlopen], ,[AC_MSG_ERROR([ltdl not found, install libtool])])
AC_CHECK_LIB([m], [ceil], ,[AC_MSG_ERROR([libm is missing, you should install it])])

TARGET="unknown"
case "$host" in
     x86_64-*-darwin*)
          TARGET=X86_DARWIN
          ;;
     x86_64-*)
          TARGET=X86
          ;;
esac

if test $TARGETDIR = unknown; then
   AC_MSG_ERROR(["brianscheme has not been ported to $host"])
fi

AC_SUBST(AM_LTLDFLAGS)

AC_OUTPUT