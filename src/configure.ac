#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)

# AC_CANONICAL_SYSTEM

##
### Set :version in dswm.asd and replace to next, when release
# AC_INIT(Deep Space Window Manager, esyscmd(grep :version dswm.asd | cut -d\" -f2 | tr -d \\n), esyscmd(grep :author dswm.asd | cut -d\< -f2 | cut -d\> -f1 | tr -d \\n))
AC_INIT(Deep Space Window Manager, esyscmd(date +%Y%m%d | tr -d \\n), esyscmd(grep :author dswm.asd | cut -d\< -f2 | cut -d\> -f1 | tr -d \\n))

AC_SUBST(MAKE_ALL_DEPS)
AC_SUBST(MAKE_INSTALL_DEPS)
AC_SUBST(MAKE_UNINSTALL_DEPS)
AC_SUBST(MAKE_STATIC_OR_SHARED)
AC_SUBST(MODULES_DIR)
AC_SUBST(LISP)
AC_SUBST(LISP_PROGRAM)
AC_SUBST(LISP_BUILD_OPTIONS)
AC_SUBST(LISP_INFO_OPTIONS)
AC_SUBST(STATICP)
AC_SUBST(SHAREDP)
AC_SUBST(DSWM_SOURCE_DIR)

## detect Cygwin`s clisp
if test ! -z esyscmd(uname -o | grep "Cygwin" | tr -d \\n); then
   CLISP_LINK_AUTO="full+new-clx"
else
   CLISP_LINK_AUTO="base"
fi

# Checks for programs.
AC_ARG_WITH(lisp, [  --with-lisp=IMPL        use the specified lisp (sbcl, cmucl, clisp, ccl, ecl or lw)], LISP=$withval, LISP="any")

AC_ARG_WITH([sbcl], [AC_HELP_STRING([--with-sbcl=PATH], [specify location of sbcl])], SBCL_PATH=$withval, SBCL_PATH="")
AC_ARG_WITH([cmucl], [AC_HELP_STRING([--with-cmucl=PATH], [specify location of cmucl])], CMUCL_PATH=$withval, CMUCL_PATH="")
AC_ARG_WITH([clisp], [AC_HELP_STRING([--with-clisp=PATH], [specify location of clisp])], CLISP_PATH=$withval, CLISP_PATH="")
AC_ARG_WITH([ccl], [AC_HELP_STRING([--with-ccl=PATH], [specify location of ccl])], CCL_PATH=$withval, CCL_PATH="")
AC_ARG_WITH([ecl], [AC_HELP_STRING([--with-ecl=PATH], [specify location of ecl])], ECL_PATH=$withval, ECL_PATH="")

AC_ARG_ENABLE([static], [AC_HELP_STRING([--disable-static], [do not build DSWM as static binary (enabled by default)])], [STATICP="$enableval"], [STATICP=yes])
AC_ARG_ENABLE([shared], [AC_HELP_STRING([--enable-shared], [build DSWM as shared source files (disabled by default)])], [SHAREDP="$enableval"], [SHAREDP=no])

AC_ARG_WITH([clisp-link], [AC_HELP_STRING([--with-clisp-link=LINKNAME], [specify clisp link ('base' by default)])], CLISP_LINK=$withval, CLISP_LINK="$CLISP_LINK_AUTO")
AC_ARG_WITH([lisp-extra-options], [AC_HELP_STRING([--with-lisp-extra-options=OPTS], [specify addition lisp translator options])], LISP_OPTIONS=$withval, LISP_OPTIONS="")

AC_ARG_WITH([dswm-source-dir], [AC_HELP_STRING([--with-dswm-source-dir=DIR], [set directory for DSWM source files (optional. May need, if you build DSWM with --enable-shared option)])], DSWM_SOURCE_DIR=$withval, DSWM_SOURCE_DIR='${data_dir}/dswm/source/')

dnl AC_ARG_WITH(manual, [  --with-manual compile with manual for DSWM (need cl-ppcre)], MANUALP="true", MANUALP="")

# checking for working SBCL
if test -x "$SBCL_PATH"; then
   SBCL=$SBCL_PATH
   AC_MSG_CHECKING([for sbcl])
   AC_MSG_RESULT($SBCL)
else
   AC_PATH_PROG([SBCL], sbcl,"")
fi

# checking for working CMUCL
if test -x "$CMUCL_PATH"; then
   CMUCL=$CMUCL_PATH
   AC_MSG_CHECKING([for cmucl])
   AC_MSG_RESULT($CMUCL)
else
   AC_PATH_PROG([CMUCL], cmucl,"")
fi

# checking for working CLISP
if test -x "$CLISP_PATH"; then
   CLISP=$CLISP_PATH
   AC_MSG_CHECKING([for clisp])
   AC_MSG_RESULT($CLISP)
else
   AC_PATH_PROG([CLISP],clisp,"")
fi

# checking for working CCL
if test -x "$CCL_PATH"; then
   CCL=$CCL_PATH
   AC_MSG_CHECKING([for ccl])
   AC_MSG_RESULT($CCL)
else
   AC_PATH_PROG([CCL],ccl,"")
fi

# checking for working ECL
if test -x "$ECL_PATH"; then
   ECL=$ECL_PATH
   AC_MSG_CHECKING([for ecl])
   AC_MSG_RESULT($ECL)
else
   AC_PATH_PROG([ECL], ecl,"")
fi

## If lisp implementation not set
if test "x$LISP" = "xany"; then
   if test "$SBCL"; then
      LISP=sbcl
   elif test "$CMUCL"; then
      LISP=cmucl
   elif test "$CLISP"; then
      LISP=clisp
   elif test "$CCL"; then
      LISP=ccl
   elif test "$ECL"; then
      LISP=ecl
   fi
fi

## set common compile parameters for different lisp implementations
if test "x$LISP" = "xsbcl"; then
   LISP_PROGRAM=$SBCL
   LISP_BUILD_OPTIONS="$LISP_OPTIONS --eval '(require :asdf)' --load ./dswm.asd --load ./make-image.lisp"
   LISP_INFO_OPTIONS="--eval '(progn (require :asdf) (require :dswm) (load \"manual.lisp\"))' --eval '(progn (dswm::generate-manual) (sb-ext:quit))'"

elif test "x$LISP" = "xcmucl"; then
   LISP_PROGRAM=$CMUCL
   LISP_BUILD_OPTIONS="$LISP_OPTIONS -batch -load ./make-image.lisp -eval \"(quit)\""
   LISP_INFO_OPTIONS="-eval \"(progn (require :asdf) (require :dswm) (load \"manual.lisp\"))\" -eval '(progn (dswm::generate-manual) (quit))'"

elif test "x$LISP" = "xclisp"; then
   LISP_PROGRAM=$CLISP
   LISP_BUILD_OPTIONS="$LISP_OPTIONS -modern -K $CLISP_LINK -i ../contrib/asdf.lisp -on-error exit -i ./make-image.lisp -x '(quit)'"
   LISP_INFO_OPTIONS="-K $CLISP_LINK -on-error exit -x \"(require :asdf) (load \"dswm.asd\") (asdf:operate 'asdf:load-op :dswm) (load (compile-file \"manual.lisp\")) (dswm::generate-manual) (ext:exit)\""

elif test "x$LISP" = "xccl"; then
   LISP_PROGRAM=$CCL
   LISP_BUILD_OPTIONS="$LISP_OPTIONS --eval '(require :asdf)' --load dswm.asd --load ./make-image.lisp"
   LISP_INFO_OPTIONS="--eval \"(progn (require :asdf) (require :dswm))\" --load manual.lisp --eval \"(progn (dswm::generate-manual) (quit))\""

elif test "x$LISP" = "xecl"; then
   LISP_PROGRAM=$ECL
   LISP_BUILD_OPTIONS="$LISP_OPTIONS -shell ./make-image.lisp"
   LISP_INFO_OPTIONS="-eval '(progn (require :asdf) (require :dswm) (load \"manual.lisp\"))' -eval '(progn (dswm::generate-manual) (ext:quit))'"
   
fi

if test "$XDPINFO" = "no"; then
   AC_MSG_WARN([xdpyinfo is needed for xinerama support.])
fi

## if no lisp available
if test "x$LISP_PROGRAM" = "x"; then
   AC_MSG_ERROR([*** No supported lisp is available.])
fi

## check, if all targets are disabled
if test "x$STATICP" = "xno" && test "x$SHAREDP" = "xno"; then
   echo "(neither static nor shared) given. Nothing to do, exiting. Use --enable-shared or/and --enable-static for correct working"
   exit 0
fi

AC_MSG_NOTICE([Using $LISP at $LISP_PROGRAM])

# check for makeinfo
if test "x$MANUALP" = "yes"; then
   MAKE_ALL_DEPS="dswm dswm.info"
   MAKE_INSTALL_DEPS="dswm.install dswm.info.install dswm.dm.install"

   # check for makeinfo
   AC_CHECK_PROG(MAKEINFO,makeinfo,yes,no)
   if test "$MAKEINFO" = "no"; then
      AC_MSG_ERROR([*** Please install makeinfo for the manual.])
   fi
else
   MAKE_ALL_DEPS="dswm"
   MAKE_INSTALL_DEPS="dswm.install dswm.dm.install"
   MAKE_UNINSTALL_DEPS="dswm.uninstall"
fi

# check for shared part
if test "x$SHAREDP" = "xyes"; then
   MAKE_ALL_DEPS="$MAKE_ALL_DEPS dswm.shared"
   MAKE_INSTALL_DEPS="$MAKE_INSTALL_DEPS dswm.shared.install"
   # MAKE_UNINSTALL_DEPS="dswm.uninstall"
fi

# check for static part
if test "x$STATICP" = "xyes"; then
      MAKE_ALL_DEPS="$MAKE_ALL_DEPS dswm.static"
      MAKE_INSTALL_DEPS="$MAKE_INSTALL_DEPS dswm.static.install"
      # MAKE_UNINSTALL_DEPS="dswm.uninstall"
fi

## It`s temporary. Just caution message, that we moved to XDG recommendations
MAKE_ALL_DEPS="$MAKE_ALL_DEPS dswm.message"

AC_CHECK_PROG(XDPYINFO,xdpyinfo,yes,no)
if test "$XDPINFO" = "no"; then
   AC_MSG_WARN([xdpyinfo is needed for xinerama support.])
fi

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_OUTPUT(Makefile)
AC_OUTPUT(module.lisp)
AC_OUTPUT(help.lisp)
AC_OUTPUT(../config/dswm.desktop)
AC_OUTPUT(../doc/dswm.texi)
AC_OUTPUT(loader.sh)
AC_OUTPUT(loader.lisp)