LISP=/usr/bin/sbcl

clisp_BUILDOPTS=-K full -on-error exit -i ./make-image.lisp
sbcl_BUILDOPTS=--load ./make-image.lisp
ccl_BUILDOPTS=--load ./make-image.lisp
ecl_BUILDOPTS=-norc -shell ./make-image.lisp

clisp_INFOOPTS=-K full -on-error exit -x "(require 'asdf '(\"asdf.lisp\")) (load \"dswm.asd\") (load \"/home/cosmonaut/dev/dss/dswm/cl-ppcre/cl-ppcre.asd\") (asdf:operate 'asdf:load-op :dswm) (load (compile-file \"manual.lisp\")) (dswm::generate-manual) (ext:exit)"
sbcl_INFOOPTS=--eval "(progn (require 'asdf) (require 'dswm) (load \"manual.lisp\"))" --eval "(progn (dswm::generate-manual) (sb-ext:quit))"
ccl_INFOOPTS=--eval "(progn (require 'asdf) (require 'dswm))" --load manual.lisp --eval "(progn (dswm::generate-manual) (quit))"
ecl_INFOOPTS=-eval "(progn (require 'asdf) (load \"/home/cosmonaut/dev/dss/dswm/cl-ppcre/cl-ppcre.asd\") (require 'dswm) (load \"manual.lisp\"))" -eval "(progn (dswm::generate-manual) (ext:quit))"
datarootdir = ${prefix}/share
prefix=/usr/local
exec_prefix= ${prefix}
bindir=${exec_prefix}/bin
infodir=${datarootdir}/info

params="--with-lisp=sbcl"
root_dir=${PWD}
dest_dir=dswm-0.0.4

# You shouldn't have to edit past this

# This is copied from the .asd file. It'd be nice to have the list in
# one place, but oh well.
FILES=dswm.asd package.lisp primitives.lisp wrappers.lisp		\
pathnames.lisp keysyms.lisp keytrans.lisp kmap.lisp input.lisp		\
core.lisp command.lisp menu.lisp screen.lisp head.lisp group.lisp	\
window.lisp floating-group.lisp tile-window.lisp window-placement.lisp	\
message-window.lisp selection.lisp user.lisp iresize.lisp		\
bindings.lisp events.lisp help.lisp fdump.lisp mode-line.lisp		\
time.lisp color.lisp module.lisp dswm.lisp

all: dswm dswm.info

dswm.info: dswm.texi
	makeinfo dswm.texi

# FIXME: This rule is too hardcoded
dswm.texi: dswm.texi.in
	$(LISP) $(sbcl_INFOOPTS)

dswm: $(FILES)
	$(LISP) $(sbcl_BUILDOPTS)

release:
	git tag -a -m "version 0.0.4" 0.0.4
	git archive --format=tar --prefix=dswm-0.0.4/ HEAD > dswm-0.0.4.tar
	tar -xf dswm-0.0.4.tar
	rm -rf dswm-0.0.4/.git* dswm-0.0.4/autom4te.cache dswm-0.0.4/version.lisp dswm-0.0.4/module.lisp dswm-0.0.4/make-image.lisp dswm-0.0.4/dswm.desktop
	git log > dswm-0.0.4/ChangeLog
	cp configure dswm-0.0.4/
	tar -zcf dswm-0.0.4.tar.gz dswm-0.0.4
	tar -jcf dswm-0.0.4.tar.bz2 dswm-0.0.4
	rm -fr dswm-0.0.4/ dswm-0.0.4.tar
release-upload:
#	gpg -b dswm-0.0.4.tgz
	ssh cosmonaut-ok,dswm@shell.sourceforge.net create
	ssh cosmonaut-ok,dswm@shell.sourceforge.net test -d /home/frs/project/d/ds/dswm/0.0.4 || mkdir /home/frs/project/d/ds/dswm/0.0.4/
	scp dswm-0.0.4.tar.gz cosmonaut-ok,dswm@frs.sourceforge.net:/home/frs/project/d/ds/dswm/0.0.4/dswm-0.0.4.tar.gz
	scp dswm-0.0.4.tar.bz2 cosmonaut-ok,dswm@frs.sourceforge.net:/home/frs/project/d/ds/dswm/0.0.4/dswm-0.0.4.tar.bz2
testbuild:
	cd $(dirname $0)
	rm -rf ~/.cache/common-lisp/
	test -z "/tmp/$(dest_dir).build" || rm -rf /tmp/$(dest_dir).build
	cp -r $(root_dir) /tmp/$(dest_dir).build
	cd /tmp/$(dest_dir).build
	autoconf
	./configure --prefix=/usr/local ${params}
	printf '\n\nNow, run \"cd /tmp/$(dest_dir).build && sudo rm -rf /etc/dswm && sudo make install && cd $(root_dir)\" to install DSWM\n\n'
clean:
	rm -f *.fasl *.fas *.lib *.*fsl
	rm -f *.log *.fns *.fn *.aux *.cp *.ky *.log *.toc *.pg *.tp *.vr *.vrs
	rm -rf dswm dswm.texi dswm.info autom4te.cache  *.tar.* version.lisp module.lisp make-image.lisp help.lisp dswm.desktop dswm.asd

install: dswm.info dswm
	test -z "$(destdir)$(bindir)" || mkdir -p "$(destdir)$(bindir)"
	install -m 755 dswm "$(destdir)$(bindir)"
	test -z "$(destdir)$(infodir)" || mkdir -p "$(destdir)$(infodir)"
	install -m 644 dswm.info "$(destdir)$(infodir)"
	install-info --info-dir="$(destdir)$(infodir)" "$(destdir)$(infodir)/dswm.info"
	test -d "/etc/dswm/" || mkdir -p "/etc/dswm"
	test -f "/etc/dswm/dswm.conf" || install -m 644 dswm.conf "/etc/dswm"
	mkdir -p "$(destdir)$(datarootdir)/dswm/modules"
	install -m 644 modules/*.lisp "$(destdir)$(datarootdir)/dswm/modules"
	install -m 644 COPYING "$(destdir)$(datarootdir)/dswm/"
	install -m 644 AUTHORS "$(destdir)$(datarootdir)/dswm/"
	test -d "$(destdir)$(datarootdir)/xsessions" && install -m 644 dswm.desktop "$(destdir)$(datarootdir)/xsessions" || install -m 644 dswm.desktop "/usr/share/xsessions/"

uninstall:
	rm "$(destdir)$(bindir)/dswm"
	rm -r "$(destdir)$(datarootdir)/dswm"
	rm "/etc/dswm/dswm.conf"
	install-info --info-dir="$(destdir)$(infodir)" --remove "$(destdir)$(infodir)/dswm.info"
	rm "$(destdir)$(infodir)/dswm.info"

# End of file
