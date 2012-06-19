#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(Deep Space Window Manager, esyscmd(grep :version dswm.asd | cut -d\" -f2 | tr -d \\n), esyscmd(grep :author dswm.asd | cut -d\< -f2 | cut -d\> -f1 | tr -d \\n))

AC_SUBST(MAKE_ALL_DEPS)
AC_SUBST(MODULES_DIR)
AC_SUBST(LISP_PROGRAM)
AC_SUBST(LISP)
# AC_SUBST(PPCRE_PATH)
AC_SUBST(DSWM_BIN_DIR)

# Checks for programs.
AC_ARG_WITH(lisp,    [  --with-lisp=IMPL        use the specified lisp (sbcl, clisp, ccl or ecl)], LISP=$withval, LISP="sbcl")
AC_ARG_WITH(sbcl,    [  --with-sbcl=PATH        specify location of sbcl], SBCL_PATH=$withval, SBCL_PATH="")
AC_ARG_WITH(clisp,   [  --with-clisp=PATH       specify location of clisp], CLISP_PATH=$withval, CLISP_PATH="")
AC_ARG_WITH(manual,  [  --with-manual		compile with manual for DSWM (need cl-ppcre)], MANUAL="true", MANUAL="")
AC_ARG_WITH(ppcre,   [  --with-ppcre=PATH       specify location of cl-ppcre], PPCRE_PATH=$withval, PPCRE_PATH="`pwd`/cl-ppcre")

if test -x "$SBCL_PATH"; then
   SBCL=$SBCL_PATH
   AC_MSG_CHECKING([for sbcl])
   AC_MSG_RESULT($SBCL)
else
   AC_PATH_PROG([SBCL], sbcl,"")
fi

if test -x "$CLISP_PATH"; then
   CLISP=$CLISP_PATH
   AC_MSG_CHECKING([for clisp])
   AC_MSG_RESULT($CLISP)
else
   AC_PATH_PROG([CLISP],clisp,"")
fi

if test "x$LISP" = "xclisp"; then
   if test "x$CLISP" = "x"; then
      LISP=sbcl
      LISP_PROGRAM=$SBCL
   else
      LISP_PROGRAM=$CLISP
   fi
elif test "x$LISP" = "xsbcl"; then
   if test "x$SBCL" = "x"; then
      LISP=clisp
      LISP_PROGRAM=$CLISP
   else
      LISP_PROGRAM=$SBCL
   fi
fi

if test "x$LISP_PROGRAM" = "x"; then
   AC_MSG_ERROR([*** No lisp is available.])
fi

AC_MSG_NOTICE([Using $LISP at $LISP_PROGRAM])

if test "$MANUAL" = "true"; then
   MAKE_ALL_DEPS="dswm dswm.info"

   if test "$LISP" = "clisp"; then
      AC_CHECK_FILE([$PPCRE_PATH/cl-ppcre.asd],,AC_MSG_ERROR([Cannot find ppcre. When using clisp you must specify its location using --with-ppcre]))
   fi

   # check for makeinfo
   AC_CHECK_PROG(MAKEINFO,makeinfo,yes,no)
   if test "$MAKEINFO" = "no"; then
      AC_MSG_ERROR([Please install makeinfo for the manual.])
   fi
else
   MAKE_ALL_DEPS="dswm"
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
AC_OUTPUT(make-image.lisp)
AC_OUTPUT(module.lisp)
AC_OUTPUT(help.lisp)
AC_OUTPUT(dswm.desktop)
AC_OUTPUT(dswm.texi)

