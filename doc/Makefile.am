docdir = $(datadir)/doc/@PACKAGE@
doc_DATA = README INSTALL
info_TEXINFOS = dswm.texi

DISTCLEANFILES = dswm.texi

.PHONY: dswm.texi

dswm.texi:
	if test ! -z @DOCP@; then \
		@LISP_PROGRAM@ @LISP_INFO_OPTIONS@; \
	else :;	fi

distclean-local:
	rm -f dswm.info
