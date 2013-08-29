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
AC_SUBST(DSWM_BIN_DIR)
AC_SUBST(LISP_BUILD_OPTIONS)
AC_SUBST(LISP_INFO_OPTIONS)
AC_SUBST(STATICP)
AC_SUBST(SHAREDP)
AC_SUBST(DSWM_SHARE_DIR)

## detect Cygwin`s clisp
if test ! -z esyscmd(uname -o | grep "Cygwin" | tr -d \\n); then
   CLISP_LINK="full+new-clx"
fi

# Checks for programs.
AC_ARG_WITH(lisp, [  --with-lisp=IMPL        use the specified lisp (sbcl, cmucl, clisp, ccl, ecl or lw)], LISP=$withval, LISP="any")

AC_ARG_WITH(sbcl, [  --with-sbcl=PATH        specify location of sbcl], SBCL_PATH=$withval, SBCL_PATH="")
AC_ARG_WITH(cmucl, [  --with-cmucl=PATH       specify location of cmucl], CMUCL_PATH=$withval, CMUCL_PATH="")
AC_ARG_WITH(clisp, [  --with-clisp=PATH       specify location of clisp], CLISP_PATH=$withval, CLISP_PATH="")
AC_ARG_WITH(ccl, [  --with-ccl=PATH         specify location of ccl], CCL_PATH=$withval, CCL_PATH="")
AC_ARG_WITH(ecl, [  --with-ecl=PATH         specify location of ecl], ECL_PATH=$withval, ECL_PATH="")

AC_ARG_ENABLE(static, [  --disable-static        do not compile DSWM as static binary (enabled by default)], STATICP="", STATICP="true")
AC_ARG_ENABLE(shared, [  --enable-shared         compile DSWM as shared source files (disabled by default)], SHAREDP="true", SHAREDP="")

AC_ARG_WITH(clisp-link, [  --with-clisp-link=LINKNAME        specify clisp link ('base' by default)], CLISP_LINK=$withval, CLISP_LINK="base")
AC_ARG_WITH(lisp-options, [  --with-lisp-options=OPTS          specify addition lisp translator options], LISP_OPTIONS=$withval, LISP_OPTIONS="")

AC_ARG_WITH(dswm-share-dir, [  --with-dswm-share-dir=DIR         set directory for DSWM share files (optional. May need, if you build DSWM with --enable-shared option)], DSWM_SHARE_DIR=$withval, DSWM_SHARE_DIR='${data_dir}/dswm/source/')

dnl AC_ARG_WITH(manual, [  --with-manual compile with manual for DSWM (need cl-ppcre)], MANUALP="true", MANUALP="")

if test -x "$SBCL_PATH"; then
   SBCL=$SBCL_PATH
   AC_MSG_CHECKING([for sbcl])
   AC_MSG_RESULT($SBCL)
else
   AC_PATH_PROG([SBCL], sbcl,"")
fi

if test -x "$CMUCL_PATH"; then
   CMUCL=$CMUCL_PATH
   AC_MSG_CHECKING([for cmucl])
   AC_MSG_RESULT($CMUCL)
else
   AC_PATH_PROG([CMUCL], cmucl,"")
fi

if test -x "$CLISP_PATH"; then
   CLISP=$CLISP_PATH
   AC_MSG_CHECKING([for clisp])
   AC_MSG_RESULT($CLISP)
else
   AC_PATH_PROG([CLISP],clisp,"")
fi

if test -x "$CCL_PATH"; then
   CCL=$CCL_PATH
   AC_MSG_CHECKING([for ccl])
   AC_MSG_RESULT($CCL)
else
   AC_PATH_PROG([CCL],ccl,"")
fi

if test -x "$ECL_PATH"; then
   ECL=$ECL_PATH
   AC_MSG_CHECKING([for ecl])
   AC_MSG_RESULT($ECL)
else
   AC_PATH_PROG([ECL], ecl,"")
fi

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
   LISP_BUILD_OPTIONS="$LISP_OPTIONS -K $CLISP_LINK -i ../contrib/asdf.lisp -on-error exit -i ./make-image.lisp -x '(quit)'"
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

## some unused
if test "x$LISP_PROGRAM" = "x"; then
   AC_MSG_ERROR([*** No supported lisp is available.])
fi

## check, if all targets are disabled
if test "x$STATICP" = "x" && test "x$SHAREDP" = "x"; then
   echo "(neither static nor shared) given. Nothing to do, exiting. Use --with-shared or/and --with-static for correct working"
   exit 0
fi

AC_MSG_NOTICE([Using $LISP at $LISP_PROGRAM])

# check for makeinfo
if test "x$MANUALP" = "true"; then
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
if test "x$SHAREDP" = "xtrue"; then
   MAKE_ALL_DEPS="$MAKE_ALL_DEPS dswm.shared"
   MAKE_INSTALL_DEPS="$MAKE_INSTALL_DEPS dswm.shared.install"
   # MAKE_UNINSTALL_DEPS="dswm.uninstall"
fi

# check for static part
if test "x$STATICP" = "xtrue"; then
      MAKE_ALL_DEPS="$MAKE_ALL_DEPS dswm.static"
      MAKE_INSTALL_DEPS="$MAKE_INSTALL_DEPS dswm.static.install"
      # MAKE_UNINSTALL_DEPS="dswm.uninstall"
fi

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

if test "x$SHAREDP" = "xtrue"; then
   AC_OUTPUT(loader.lisp)
fi

AC_OUTPUT(../config/dswm.desktop)
AC_OUTPUT(../doc/dswm.texi)

