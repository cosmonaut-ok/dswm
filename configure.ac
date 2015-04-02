#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.59])

AC_INIT(dswm, esyscmd(date +%Y%m%d | tr -d \\n), esyscmd(grep :author src/dswm.asd.in | cut -d\< -f2 | cut -d\> -f1 | tr -d \\n))

m4_pattern_allow([^AC_])
m4_pattern_allow([^AM_])
 
AC_CONFIG_AUX_DIR([acdir])
AC_CONFIG_MACRO_DIR([acdir])

AM_INIT_AUTOMAKE([-Wno-portability])

# AM_PATH_LISPDIR

AC_CANONICAL_HOST

AM_CONDITIONAL(WIN32, test x$win32 = xtrue)

AC_DEFINE_DIR([DATADIR], [datadir], [Where data are placed to.])
AC_DEFINE_DIR([LIBEXECDIR], [libexecdir], [Where libexec are placed to.])
AC_DEFINE_DIR([BINDIR], [bindir], [Where libexec are placed to.])

AC_SUBST(LISP_NAME)
AC_SUBST(LISP_PROGRAM)
AC_SUBST(LISP_COMMON_DEPS)
AC_SUBST(LISP_BUILD_OPTIONS)
AC_SUBST(LISP_INFO_OPTIONS)
AC_SUBST(MAKE_ALL_DEPS)
AC_SUBST(MAKE_INSTALL_DEPS)
AC_SUBST(MAKE_UNINSTALL_DEPS)
dnl AC_SUBST(STATICP)
dnl AC_SUBST(SHAREDP)
dnl AC_SUBST(MAKE_STATIC_OR_SHARED)
AC_SUBST(DSWM_SOURCE_DIR)
AC_SUBST(DSWM_DATA_DIR)
# AC_SUBST(MODULES_DIR)
AC_SUBST(QLP)
AC_SUBST(QL_DIR)
AC_SUBST(DOCP)
AC_SUBST(DOC_DIR)
#
AC_SUBST(DBUSP)
AC_SUBST(DBUS_FILE)
AC_SUBST(DBUS_IMPORT_FILE)
# *DE integrations
AC_SUBST(GNOMEP)
AC_SUBST(KDEP)
AC_SUBST(CINNAMONP)
AC_SUBST(XFCEP)
AC_SUBST(MATEP)
AC_SUBST(LXDEP)

AC_SUBST(XSESSION_DATA)
AC_SUBST(SESSION_DATA)

AC_SUBST(BINDIR)
AC_SUBST(LIBEXECDIR)
AC_SUBST(ASDFDIR)

AC_SUBST(etcdir)
AC_SUBST([sysconfdir], ['/etc'])

AC_ARG_ENABLE([debug], [AC_HELP_STRING([--enable-debug],
                     [build system with debugger support (disabled by default)])],

[DEBUG=yes], [DEBUG=no])

AC_ARG_ENABLE([static], [AC_HELP_STRING([--disable-static],
			[do not build DSWM as static binary (enabled by default)])],
			[STATICP="$enableval"], [STATICP=yes])
AC_ARG_ENABLE([shared], [AC_HELP_STRING([--enable-shared],
			[build DSWM as shared source files (disabled by default)])],
			[SHAREDP="$enableval"], [SHAREDP=no])

AC_ARG_ENABLE([dbus], [AC_HELP_STRING([--enable-dbus],
                        [compile with dbus support (ndded for some dbus-related plugins, disabled by default)])],
                        DBUSP="$enableval", DBUSP=no)

AC_ARG_ENABLE([ql], [AC_HELP_STRING([--enable-ql],
                        [resolve dependencies with quicklisp. Disabled by default])],
                        QLP="$enableval", QLP=no)

AC_ARG_ENABLE([doc], [AC_HELP_STRING([--disable-doc],
                        [install documentation. Eg. info, man pages etc. enabled by default)])],
                        DOCP="$enableval", DOCP=yes)

AC_ARG_ENABLE([gnome], [AC_HELP_STRING([--enable-gnome],
                     [build system with gnome support (can use DSWM as gnome window manager)])],
                     [GNOMEP=$enableval], [GNOMEP=no])
                

AC_ARG_ENABLE([kde], [AC_HELP_STRING([--enable-kde],
                     [build system with kde support (can use DSWM as kde window manager)])],
                     [KDEP=$enableval], [KDEP=no])
                

AC_ARG_ENABLE([mate], [AC_HELP_STRING([--enable-mate],
                     [build system with mate support (can use DSWM as mate window manager)])],
                     [MATEP=$enableval], [MATEP=no])
                

AC_ARG_ENABLE([xfce], [AC_HELP_STRING([--enable-xfce],
                     [build system with xfce support (can use DSWM as xfce window manager)])],
                     [XFCEP=$enableval], [XFCEP=no])
                
AC_ARG_ENABLE([cinnamon], [AC_HELP_STRING([--enable-cinnamon],
                     [build system with cinnamon support (can use DSWM as cinnamon window manager)])],
                     [CINNAMONP=$enableval], [CINNAMONP=no])
                     
AC_ARG_ENABLE([lxde], [AC_HELP_STRING([--enable-lxde],
                     [build system with lxde support (can use DSWM as lxde window manager)])],
                     [LXDEP=$enableval], [LXDEP=no])

##################################################
AC_ARG_WITH([lisp],  [AC_HELP_STRING([--with-lisp=IMPL],
		     [use the specified lisp (sbcl, clisp)])],
                     [case "$withval" in
                          sbcl|cmucl|clisp|ccl) LISP_NAME=$withval ;;
                          *) AC_MSG_ERROR([*** invalid lisp implementation specified]) ;;
                     esac
                     ],LISP_NAME="any")

AC_ARG_WITH([sbcl],  [AC_HELP_STRING([--with-sbcl=PATH],
		     [specify location of sbcl])],
		     SBCL_PATH=$withval, SBCL_PATH="")
AC_ARG_WITH([clisp], [AC_HELP_STRING([--with-clisp=PATH],
		     [specify location of clisp])],
		     CLISP_PATH=$withval, CLISP_PATH="")
AC_ARG_WITH([ccl],   [AC_HELP_STRING([--with-ccl=PATH],
		     [specify location of ccl])],
		     CCL_PATH=$withval, CCL_PATH="")
dnl AC_ARG_WITH([cmucl], [AC_HELP_STRING([--with-cmucl=PATH],
dnl 		     [specify location of cmucl])],
dnl 		     CMUCL_PATH=$withval, CMUCL_PATH="")
dnl AC_ARG_WITH([ecl], [AC_HELP_STRING([--with-ecl=PATH],
dnl 		   [specify location of ecl])],
dnl 		   ECL_PATH=$withval, ECL_PATH="")

AC_ARG_WITH([clisp-link], [AC_HELP_STRING([--with-clisp-link=LINKNAME],
			  [specify clisp link ('base' by default for linux and 'full+new-clx' for Cygwin)])],
			  CLISP_LINK=$withval, CLISP_LINK="")

AC_ARG_WITH([lisp-extra-options], [AC_HELP_STRING([--with-lisp-extra-options=OPTS],
				  [specify addition lisp translator options])],
				  LISP_EXTRA_OPTIONS=$withval, LISP_EXTRA_OPTIONS="")

AC_ARG_WITH([dswm-source-dir], [AC_HELP_STRING([--with-dswm-source-dir=DIR],
			       [set directory for DSWM source files (optional. May need, if you build DSWM with --enable-shared option)])],
			       DSWM_SOURCE_DIR=$withval, DSWM_SOURCE_DIR="$DATADIR/dswm/source/")

AC_ARG_WITH([dswm-data-dir], [AC_HELP_STRING([--with-dswm-data-dir=DIR],
			       [set directory for DSWM data files (optional)])],
			       DSWM_DATA_DIR=$withval, DSWM_DATA_DIR="$DATADIR/dswm/")

AC_ARG_WITH([ql-dir], [AC_HELP_STRING([--with-ql-dir=DIR],
		    [resolve dependencies with quicklisp. Disabled by default])],
		    QL_DIR=$withval, QL_DIR="${ac_pwd}/${ac_top_build_prefix}quicklisp")

## Set common global variables
MAKE_ALL_DEPS="dswm.common"
MAKE_INSTALL_DEPS="dswm.install dswm.data.install"
MAKE_UNINSTALL_DEPS="dswm.uninstall dswm.data.install"
ASDFDIR="\$(abs_top_srcdir)/contrib/asdf/"

## set lisp common deps
DSWM_COMMON_DEPS='"alexandria" "xembed" "cl-ppcre"'
DSWM_DEPS="$DSWM_DEPS $DSWM_COMMON_DEPS"
# define initial DSWM xsessions .desktop file
XSESSION_DATA="$XSESSION_DATA xsessions/dswm.desktop"
   
## set dbus deps
if test "DBUSP" = "yes"; then
   DSWM_DEPS=$DSWM_DEPS '"dbus" "bordeaux-threads" "mailbox" "cl-async-future"'
   DBUS_FILE="(:file \"experimental/dbus\")"
   DBUS_IMPORT_FILE="(:file \"experimental/dbus-import\")"
fi
## set info deps
if test x"$DOCP" = xyes; then
   DSWM_INFO_DEPS="$DSWM_COMMON_DEPS \"cl-ppcre\" \"xembed\""
   DOC_DIR="doc man examples"
fi

## etcdir
if test x${XDG_CONFIG_DIRS} != x; then
   etcdir_=esyscmd(echo ${XDG_CONFIG_DIRS} | cut -d':' -f1)
   etcdir=$etcdir_/dswm/
else
   etcdir=${sysconfdir}/xdg/dswm/
fi

## check for Cygwin`s clisp
case "$host" in
        *mingw*|*cygwin*)
                CLISP_LINK_AUTO="full+new-clx" ;;
        *)
                CLISP_LINK_AUTO="base" ;;
esac
dnl if test ! -z esyscmd(uname -o | grep "Cygwin" | tr -d \\n); then
dnl    CLISP_LINK_AUTO="full+new-clx"
dnl else
dnl    CLISP_LINK_AUTO="base"
dnl fi
test $CLISP_LINK || CLISP_LINK=$CLISP_LINK_AUTO

## check, if all targets are disabled
if test "x$STATICP" = "xno" && test "x$SHAREDP" = "xno"; then
   AC_MSG_ERROR([*** Do not Use --disable-shared and --disable-static at the same time. Nothing to do, exiting."])
   exit 0
fi

# Checks for programs.
## checking for working SBCL
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
if test "x$LISP_NAME" = "xany"; then
   if test "$SBCL"; then
      LISP_NAME=sbcl
   elif test "$CMUCL"; then
      LISP_NAME=cmucl
   elif test "$CLISP"; then
      LISP_NAME=clisp
   elif test "$CCL"; then
      LISP_NAME=ccl
   elif test "$ECL"; then
      LISP_NAME=ecl
   fi
fi

## if no lisp available
if test "x$LISP_NAME" = "x"; then
   AC_MSG_ERROR([*** No supported lisp is available.])
fi

## Warn for xdpyinfo
AC_CHECK_PROG(XDPYINFO,xdpyinfo,yes,no)
if test "$XDPYINFO" = "no"; then
   AC_MSG_WARN([xdpyinfo is needed for xinerama support.])
fi

## set common compile parameters for different lisp implementations
if test "x$LISP_NAME" = "xsbcl"; then
   DSWM_DEPS="$DSWM_DEPS \"sb-posix\" \"clx\" \"cl-ppcre\""
   LISP_PROGRAM=$SBCL
   LISP_EVAL="--eval"
   LISP_LOAD="--load"
   LISP_EXIT="(sb-ext:quit)"
   if test $DEBUG = no; then
      LISP_DEBUG="--disable-debugger"
   fi
   LISP_COMMON_OPTIONS="$LISP_DEBUG $LISP_EXTRA_OPTIONS $LISP_EVAL '(require :asdf)' $LISP_LOAD \$(abs_top_srcdir)/src/dswm.asd"
dnl elif test "x$LISP_NAME" = "xcmucl"; then
dnl    LISP_PROGRAM=$CMUCL
dnl    LISP_EVAL="-eval"
dnl    LISP_LOAD="-load"
dnl    LISP_COMMON_OPTIONS=""
dnl    LISP_BUILD_OPTIONS="$LISP_EXTRA_OPTIONS -batch -load ./make-image.lisp -eval \"(quit)\""
dnl    LISP_INFO_OPTIONS="-eval \"(progn (require :asdf) (require :dswm) (load \"manual.lisp\"))\" -eval '(progn (dswm::generate-manual) (quit))'"
elif test "x$LISP_NAME" = "xclisp"; then
   DSWM_DEPS="$DSWM_DEPS \"clx\" \"cl-ppcre\"" ## TODO: test for adding CLX module
   LISP_PROGRAM=$CLISP
   LISP_EVAL="-x"
   LISP_LOAD="-i"
   LISP_EXIT="(quit)"
   LISP_COMMON_OPTIONS="-K $CLISP_LINK $LISP_EXTRA_OPTIONS -modern -on-error exit $LISP_LOAD $ASDFDIR/build/asdf.lisp $LISP_EVAL \"(require :asdf)\" $LISP_LOAD dswm.asd"
elif test "x$LISP_NAME" = "xccl"; then
   LISP_PROGRAM=$CCL
   LISP_EVAL="--eval"
   LISP_LOAD="--load"
   LISP_EXIT="(quit)"
   LISP_COMMON_OPTIONS="$LISP_EXTRA_OPTIONS $LISP_LOAD $ASDFDIR/build/asdf.lisp $LISP_EVAL '(require :asdf)' $LISP_LOAD \$(abs_top_srcdir)/src/dswm.asd"
dnl elif test "x$LISP_NAME" = "xecl"; then
dnl    LISP_PROGRAM=$ECL
dnl       LISP_EVAL=
dnl    LISP_LOAD=
dnl    LISP_COMMON_OPTIONS=
dnl    LISP_BUILD_OPTIONS="$LISP_EXTRA_OPTIONS -shell ./make-image.lisp"
dnl    LISP_INFO_OPTIONS="-eval '(progn (require :asdf) (require :dswm) (load \"manual.lisp\"))' -eval '(progn (dswm::generate-manual) (ext:quit))'"   
fi

## We always use this values, because info builds only after
## main application. So, all needed quicklisp deps should be
## already fetched
QL_INFO_LOAD_FILE=$QL_DIR/setup.lisp
QUICKINSTALL_INFO="(princ \"skip\")"

if test "$QLP" = "yes"; then
   if test -f $QL_DIR/setup.lisp; then
      QL_LOAD_FILE=$QL_INFO_LOAD_FILE
      QUICKINSTALL=$QUICKINSTALL_INFO
   else
      QL_LOAD_FILE="\$(abs_top_srcdir)/contrib/quicklisp-bootstrap/quicklisp.lisp"
      QUICKINSTALL="(quicklisp-quickstart:install :path \"$QL_DIR\")"
   fi
   LISP_QL_OPTIONS="$LISP_LOAD $QL_LOAD_FILE $LISP_EVAL '$QUICKINSTALL' $LISP_EVAL '(dolist (v (list $DSWM_DEPS)) (ql:quickload v))'"
   LISP_QL_INFO_OPTIONS="$LISP_LOAD $QL_INFO_LOAD_FILE $LISP_EVAL '$QUICKINSTALL_INFO' $LISP_EVAL '(dolist (v (list $DSWM_INFO_DEPS)) (ql:quickload v))'"
fi

LISP_BUILD_OPTIONS="$LISP_COMMON_OPTIONS $LISP_QL_OPTIONS $LISP_LOAD ./make-image.lisp"
LISP_INFO_OPTIONS="$LISP_COMMON_OPTIONS $LISP_QL_INFO_OPTIONS $LISP_EVAL '(progn (require :dswm) (load \"manual.lisp\"))' $LISP_EVAL \"(progn (dswm::generate-manual) $LISP_EXIT)\""

AC_MSG_NOTICE([Using $LISP_NAME at $LISP_PROGRAM])

# Checks for shared part.
if test "x$SHAREDP" = "xyes"; then
   MAKE_ALL_DEPS="$MAKE_ALL_DEPS dswm.shared"
   MAKE_INSTALL_DEPS="$MAKE_INSTALL_DEPS dswm.shared.install"
   MAKE_UNINSTALL_DEPS="$MAKE_UNINSTALL_DEPS dswm.shared.uninstall"
fi

# Checks for static part.
if test "x$STATICP" = "xyes"; then
   MAKE_ALL_DEPS="$MAKE_ALL_DEPS dswm.static"
   MAKE_INSTALL_DEPS="$MAKE_INSTALL_DEPS dswm.static.install"
   MAKE_UNINSTALL_DEPS="$MAKE_UNINSTALL_DEPS dswm.static.uninstall"
fi

# Checks for makeinfo.
if test x"$DOCP" = xyes; then
   AC_CHECK_PROG(MAKEINFO,makeinfo,yes,no)
   if test "$MAKEINFO" = "no"; then
      AC_MSG_ERROR([*** Please install makeinfo for the info.])
   fi
fi

# Checks for libraries.
AC_HAVE_LIBRARY(fixposix, ,
	AC_MSG_WARN([No such library libfixposix. Build could be broken with --with-dbus option]))

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

# Generate xsession and session data
if test ${GNOMEP} = yes; then
   AC_OUTPUT(data/xsessions/dswm-gnome.desktop scripts/dswm-gnome-session)
   SESSION_DATA="$SESSION_DATA dswm-gnome-session"
   XSESSION_DATA="$XSESSION_DATA xsessions/dswm-gnome.desktop"
fi

if test ${KDEP} = yes; then
   AC_OUTPUT(data/xsessions/dswm-kde.desktop scripts/dswm-kde-session)
   SESSION_DATA="$SESSION_DATA dswm-kde-session"
   XSESSION_DATA="$XSESSION_DATA xsessions/dswm-kde.desktop"
fi

if test ${MATEP} = yes; then
   AC_OUTPUT(data/xsessions/dswm-mate.desktop scripts/dswm-mate-session)
   SESSION_DATA="$SESSION_DATA dswm-mate-session"
   XSESSION_DATA="$XSESSION_DATA xsessions/dswm-mate.desktop"
fi

if test ${XFCEP} = yes; then
   AC_OUTPUT(data/xsessions/dswm-xfce.desktop scripts/dswm-xfce-session)
   SESSION_DATA="$SESSION_DATA dswm-xfce-session"
   XSESSION_DATA="$XSESSION_DATA xsessions/dswm-xfce.desktop"
fi

if test ${CINNAMONP} = yes; then
   AC_OUTPUT(data/xsessions/dswm-cinnamon.desktop scripts/dswm-cinnamon-session)
   SESSION_DATA="$SESSION_DATA dswm-cinnamon-session"
   XSESSION_DATA="$XSESSION_DATA xsessions/dswm-cinnamon.desktop"
fi

if test ${LXDEP} = yes; then
   AC_OUTPUT(data/xsessions/dswm-lxde.desktop scripts/dswm-lxde-session)
   SESSION_DATA="$SESSION_DATA dswm-lxde-session"
   XSESSION_DATA="$XSESSION_DATA xsessions/dswm-lxde.desktop"
fi

### AC_CONFIG_SUBDIRS([src])
# AC_OUTPUT
AC_OUTPUT(Makefile src/Makefile examples/Makefile scripts/Makefile etc/Makefile data/Makefile)
AC_OUTPUT(doc/Makefile man/Makefile)
AC_OUTPUT(src/module.lisp src/help.lisp src/loader.lisp src/dswm.asd)
AC_OUTPUT(scripts/loader.sh)
AC_OUTPUT(data/applications/dswm.desktop)
AC_OUTPUT(data/xsessions/dswm.desktop)

# # TODO: add ‘lispdir’ for emacs lisp files to build
