bin_PROGRAMS=dswm

##
# linux_style_data_dir=$(DESTDIR)/usr/share/

# dswm_SOURCES=$(shell cat dswm.asd | grep '\:file' | grep -vE '\;.*file' | grep -oE '\(\:.*\)' | grep -oE '\"[a-z,A-Z,0-9,.,-,_]*\"' | sed 's/^\"*/src\//g;s/\"/\.lisp/g')

# dswm_SOURCES=easycmd(cat dswm.asd | grep '\:file' | grep -vE '\;.*file' | grep -oE '\(\:.*\)' | grep -oE '\"[a-z,A-Z,0-9,.,-,_]*\"' | sed 's/^\"*/src\//g;s/\"/\.lisp/g')

dswm_SOURCES="bindings.lisp \
	color.lisp \
	command.lisp \
	deepshell \
	definitions.lisp \
	dswm.asd \
	dswm.desktop \
	dswm.lisp \
	events.lisp \
	fdump.lisp \
	floating-group.lisp \
	floating-window.lisp \
	group.lisp \
	head.lisp \
	helper.lisp \
	help.lisp \
	input.lisp \
	install.sh \
	iresize.lisp \
	keysyms.lisp \
	keytrans.lisp \
	kmap.lisp \
	library.lisp \
	loader.lisp \
	make-image.lisp \
	manual.lisp \
	menu.lisp \
	message-window.lisp \
	mode-line.lisp \
	module.lisp \
	package.lisp \
	rfr.lisp \
	scratchpad.lisp \
	screen.lisp \
	selection.lisp \
	test.lisp \
	test-wm.lisp \
	tile-group.lisp \
	tile-window.lisp \
	time.lisp \
	user.lisp \
	window.lisp \
	window-placement.lisp \
	workarounds.lisp \
	wrappers.lisp"

dswm$(EXEEXT): @MAKE_ALL_DEPS@

dswm.common:

dswm.shared:

dswm.static: $(FILES)
	if test @QLP@ == yes && test ! -z "@QL_DIR@"; then if test wget -O - quicklisp.org 1>/dev/null 2>/dev/null; then \
		echo "No internet connection for quicklisp working"; \
		exit 1; \
	fi; fi
	@LISP_PROGRAM@ @LISP_BUILD_OPTIONS@

deps:
#	@LISP_PROGRAM@ @LISP_BUILD_OPTIONS@

install-exec-local: @MAKE_INSTALL_DEPS@
	echo $(dswm_SOURCES)

install-data-local:

install-exec-hook:

install-data-hook:

clean-local:
	rm -f *.fasl *.fas *.lib *.*fsl *.o *.log *.fns *.fn *.aux *.cp *.ky *.log *.toc *.pg *.tp *.vr *.vrs *.tar.*
# rm -rf dswm ../doc/dswm.texi dswm.info autom4te.cache config.status  version.lisp module.lisp help.lisp loader.sh loader.lisp ../config/dswm.desktop Makefile install

# # release:
# # 	# TODO: We need to rewrite it!
# # 	# git tag -a -m "version @PACKAGE_VERSION@" @PACKAGE_VERSION@
# # 	git archive --format=tar --prefix=dswm-@PACKAGE_VERSION@/ HEAD > dswm-@PACKAGE_VERSION@.tar
# # 	tar -xf dswm-@PACKAGE_VERSION@.tar
# # 	rm -rf dswm-@PACKAGE_VERSION@/.git* dswm-@PACKAGE_VERSION@/autom4te.cache dswm-@PACKAGE_VERSION@/configure dswm-@PACKAGE_VERSION@/Makefile dswm-@PACKAGE_VERSION@/module.lisp dswm-@PACKAGE_VERSION@/config/dswm.desktop
# # 	git log > dswm-@PACKAGE_VERSION@/ChangeLog
# # 	cp configure dswm-@PACKAGE_VERSION@/
# # 	tar -zcf dswm-@PACKAGE_VERSION@.tar.gz dswm-@PACKAGE_VERSION@
# # 	tar -jcf dswm-@PACKAGE_VERSION@.tar.bz2 dswm-@PACKAGE_VERSION@
# # 	rm -fr dswm-@PACKAGE_VERSION@/ dswm-@PACKAGE_VERSION@.tar
# # release-upload:
# # #	gpg -b dswm-@PACKAGE_VERSION@.tgz
# # 	ssh cosmonaut-ok,dswm@shell.sourceforge.net create
# # 	ssh cosmonaut-ok,dswm@shell.sourceforge.net test -d /home/frs/project/d/ds/dswm/@PACKAGE_VERSION@ || mkdir /home/frs/project/d/ds/dswm/@PACKAGE_VERSION@/
# # 	scp dswm-@PACKAGE_VERSION@.tar.gz cosmonaut-ok,dswm@frs.sourceforge.net:/home/frs/project/d/ds/dswm/@PACKAGE_VERSION@/dswm-@PACKAGE_VERSION@.tar.gz
# # 	scp dswm-@PACKAGE_VERSION@.tar.bz2 cosmonaut-ok,dswm@frs.sourceforge.net:/home/frs/project/d/ds/dswm/@PACKAGE_VERSION@/dswm-@PACKAGE_VERSION@.tar.bz2

# deps:
# 	@LISP_PROGRAM@ @LISP_QL_LOADDEPS_OPTIONS@

testbuild:
# 	cd $(dirname $0)
# 	rm -rf ~/.cache/common-lisp/
# 	test -z "/tmp/$(dest_dir).build" || rm -rf /tmp/$(dest_dir).build
# 	mkdir -p /tmp/$(dest_dir).build
# 	cp -r $(root_dir) /tmp/$(dest_dir).build/src
# 	cp -r $(root_dir)/../config /tmp/$(dest_dir).build/config
# 	cp -r $(root_dir)/../copyright /tmp/$(dest_dir).build/copyright
# 	cd /tmp/$(dest_dir).build/src
# 	autoconf
# 	./configure --prefix=/tmp/$(dest_dir).build/dst
# 	make
# 	make install
# 	ls -l /tmp/$(dest_dir).build/dst/bin /tmp/$(dest_dir).build/dst/share/dswm /tmp/$(dest_dir).build/dst/share/dswm/modules
# 	rm -rf /tmp/$(dest_dir).build/

dswm.install:

dswm.data.install:
	test -d "$(DESTDIR)$(DATADIR)/dswm/modules" || mkdir -p "$(DESTDIR)$(DATADIR)/dswm/modules"
	install -d -m 755 modules "$(DESTDIR)$(DATADIR)/dswm/modules"
#	find modules -type f -exec install -D {} -m 644 $(data_dir)/dswm/{} \; || true


dswm.static.install:
# Needed only, when both --enable-static and --enable-shared are enabled
# 	if test "@SHAREDP@" = "yes"; \
# 	then install -m 755 dswm "$(DESTDIR)$(bindir)/dswm.static"; \
# 	else install -m 755 dswm "$(DESTDIR)$(bindir)/dswm"; \
# 	fi

# 	test -f "$(DESTDIR)$(etcdir)/dswm/dswm.lisp" || install -m 644 ../config/dswm.lisp "$(DESTDIR)$(etcdir)/dswm/dswm.lisp"
# 	install -d -m 755 modules "$(data_dir)/dswm/modules"
# 	find modules -type f -exec install -D {} $(data_dir)/dswm/{} \;
# 	install -m 644 ../copyright/COPYING "$(data_dir)/dswm/"
# 	install -m 644 ../copyright/AUTHORS "$(data_dir)/dswm/"

dswm.shared.install:
# 	mkdir -p "$(DSWMSOURCEDIR)"
# 	find . -name '*.lisp' -type f -exec install -m 644 -D {} "$(DSWMSOURCEDIR)" \;
# 	install -m 755 loader.sh "$(DESTDIR)$(bindir)/dswm"
# 	install -m 644 ../contrib/asdf.lisp "$(DSWMSOURCEDIR)"
# 	install -m 644 dswm.asd "$(DSWMSOURCEDIR)"

dswm.dm.install:
# 	## Install dswm.desktop for desktop managers
# 	if test -d "$(data_dir)/xsessions/"; \
# 	then install -m 644 ../config/dswm.desktop "$(data_dir)/xsessions"; \
# 	if test -d "$(data_dir)/apps/kdm/sessions/"; \
# 	then ln -sf "$(data_dir)/xsessions/dswm.desktop" "$(data_dir)/apps/kdm/sessions/"; \
# 	else if test -d "$(linux_style_data_dir)/apps/kdm/sessions/"; \
# 	then ln -sf "$(data_dir)/xsessions/dswm.desktop" "$(linux_style_data_dir)/apps/kdm/sessions/"; \
# 	fi; fi; \
# 	else if test -d "$(linux_style_data_dir)/xsessions/"; \
# 	then install -m 644 ../config/dswm.desktop "$(linux_style_data_dir)/xsessions/"; \
# 	if test -d "$(DESTDIR)$(datarootdir)/apps/kdm/sessions/"; \
# 	then ln -s "$(linux_style_data_dir)/xsessions/dswm.desktop" "$(DESTDIR)$(datarootdir)/apps/kdm/sessions/"; \
# 	else if test -d "$(linux_style_data_dir)/apps/kdm/sessions/"; \
# 	then ln -sf "$(linux_style_data_dir)/xsessions/dswm.desktop" "$(linux_style_data_dir)/apps/kdm/sessions/"; \
# 	fi; fi; fi; fi

# uninstall: @MAKE_UNINSTALL_DEPS@

dswm.uninstall:

dswm.shared.uninstall:
# 	rm -r $(DSWMSOURCEDIR) || true

dswm.static.uninstall:
# 	rm "$(DESTDIR)$(bindir)/dswm.static" || true

dswm.dm.uninstall:
# 	test -f "$(data_dir)/xsessions/dswm.desktop" && "$(data_dir)/xsessions/dswm.desktop" || test -f "$(linux_style_data_dir)/xsessions/dswm.desktop" && rm "$(linux_style_data_dir)/xsessions/dswm.desktop"
# 	test -L "$(data_dir)/apps/kdm/sessions/dswm.desktop" && rm "$(data_dir)/apps/kdm/sessions/dswm.desktop" || test -f "$(linux_style_data_dir)/apps/kdm/sessions/dswm.desktop" && rm "$(linux_style_data_dir)/apps/kdm/sessions/dswm.desktop" || true

# End of file
