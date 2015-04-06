AUTOMAKE_OPTIONS = foreign
SUBDIRS = src @DOC_DIR@ scripts etc data
CLEANFILES = config.log config.status aclocal.m4 *~
DISTCLEANFILES = Makefile.in acdir/compile acdir/config.guess acdir/config.sub \
	acdir/install-sh acdir/libtool.m4 acdir/ltmain.sh acdir/lt~obsolete.m4 \
	acdir/ltoptions.m4 acdir/ltsugar.m4 acdir/ltversion.m4 acdir/missing \
	acdir/texinfo.tex configure

distclean-local:
	if test ! -z @QL_DIR@ && test -d @QL_DIR@; then \
		rm -rf @QL_DIR@; \
	fi
