docdir = $(datadir)/doc/@PACKAGE@
doc_DATA = README INSTALL
info_TEXINFOS = dswm.texi

.PHONY: dswm.texi

dswm.texi:
	if test ! x@INFOP@ == xno; then \
		@LISP_PROGRAM@ @LISP_INFO_OPTIONS@; \
	else :;	fi

