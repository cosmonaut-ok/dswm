AUTOMAKE_OPTIONS = foreign
SUBDIRS = src doc examples man scripts

# # prefix=@prefix@
# datarootdir = @datarootdir@
# # exec_prefix= @exec_prefix@
# # bindir=@bindir@
# # infodir=@infodir@
sysconfdir = $(shell if [ ! -z ${XDG_CONFIG_DIRS} ]; then echo ${XDG_CONFIG_DIRS} | cut -d':' -f1; else echo /etc/xdg; fi)

# data_dir=$(DESTDIR)$(datarootdir)
# DSWMSOURCEDIR=@DSWM_SOURCE_DIR@
# root_dir=${PWD}
# dest_dir=dswm-@PACKAGE_VERSION@
# ##
# linux_style_data_dir=$(DESTDIR)/usr/share/

FILES=$(shell cat dswm.asd | grep '\:file' | grep -vE '\;.*file' | \
	grep -oE '\(\:.*\)' | grep -oE '\"[a-z,A-Z,0-9,.,-,_]*\"' | \
	sed 's/^\"*//g;s/\"/\.lisp/g')








clean-local:
	rm -rf compile depcomp install-sh missing config.log config.status configure aclocal.m4  autom4te.cache Makefile.in autom4te.cache
