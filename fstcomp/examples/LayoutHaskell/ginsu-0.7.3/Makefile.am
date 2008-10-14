
AUTOMAKE_OPTIONS=no-dependencies foreign subdir-objects

GHCPACKAGES= -package unix  -package network -package parsec -package mtl
GHCFEATURES= @OPT_ARGS@ @PROFILE_ARGS@ -fglasgow-exts # -fallow-overlapping-instances -fallow-undecidable-instances
GHCWARN= -W -fwarn-deprecations -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports
GHCOPTS=  $(GHCDEBUGOPTS) $(GHCINC) $(GHCWARN) $(GHCFLAGS) $(GHCPACKAGES) $(GHCFEATURES) @CURSES_INCLUDEDIR@  -\#include hs_include.h -I. -I@srcdir@ -i. -i@srcdir@
CFLAGS=  @CURSES_INCLUDEDIR@ -I. -I@srcdir@
CC=$(HC)

EXTRA_DIST = docs/Changelog.old Curses.hsc ginsu.config.sample \
             Changelog RSA.hsc  docs/ginsu.1 ginsu.spec.in LICENSE MyLocale.hsc CWString.hsc \
             Version.hs.in docs/ginsu-manual.txt docs/ginsu-manual.html \
             docs/wiki.css ginsu-mdk \
             ac-macros/ac_caolan_check_package.m4 \
             ac-macros/acincludepackage.m4 \
             ac-macros/check_ssl.m4 \
             ac-macros/check_zlib.m4 \
             ac-macros/curses.m4 \
             ac-macros/mp_with_curses.m4 \
             gen_keyhelp.prl \
             actions.def


EXTRA_PROGRAMS = ginsu_static

man_MANS = docs/ginsu.1

bin_PROGRAMS = ginsu
bin_SCRIPTS =  ginsu-mdk

# BUILT_SOURCES = RSA.hs RSA_hsc.h RSA_hsc.c Curses.hs Curses_hsc.c Curses_hsc.h Locale.hs CWString_hsc.h CWString_hsc.c

BUILT_SOURCES =


ACLOCAL_AMFLAGS = -I ac-macros

ginsu_SOURCES = ConfigFile.hs Gale/Gale.hs GenUtil.hs \
    Main.hs  Version.hs ExampleConf.hs Filter.hs SimpleParser.hs  ErrorLog.hs  UTF8.hs \
    EIO.hs Gale/Puff.hs my_curses.h my_rsa.h Charset.hs Format.hs Screen.hs \
    SHA1.hs  KeyName.hs Help.hs  hs_include.h  CacheIO.hs FastMutInt.hs \
    Binary.hs PackedString.hs  Options.hs GinsuConfig.hs Regex.hs KeyHelpTable.hs \
    Doc/DocLike.hs Gale/KeyCache.hs Gale/Proto.hs UArrayParser.hs  Atom.hs \
    Boolean/Boolean.hs Status.hs Doc/Chars.hs CircularBuffer.hs Boolean/Algebra.hs


HSNODISTSOURCES= Curses.hs MyLocale.hs CWString.hs RSA.hs
nodist_ginsu_SOURCES = $(HSNODISTSOURCES) Curses_hsc.c RSA_hsc.c RSA_hsc.h Curses_hsc.h CWString_hsc.c CWString_hsc.h

ginsu_static_SOURCES = $(ginsu_SOURCES)
nodist_ginsu_static_SOURCES =   $(nodist_ginsu_SOURCES)

ginsu_static_LINK = $(ginsu_LINK) -static -optl-static -ldl -lgpm

ginsu_LINK = $(HC) $(HCFLAGS) $(GHCOPTS) $(LIBS)  @CURSES_LIBS@ -o $@


SUFFIXES = .hs .lhs .o .hi .hsc .c .h

ginsu: $(ginsu_SOURCES) $(nodist_ginsu_SOURCES)
	$(HC) $(HCFLAGS) $(GHCOPTS) $(LIBS) @CURSES_LIBS@ -o $@ -hidir . -odir . --make @srcdir@/Main.hs

ginsu_static: $(ginsu_SOURCES) $(nodist_ginsu_SOURCES)
	$(HC) $(HCFLAGS) $(GHCOPTS) $(LIBS) @CURSES_LIBS@ -o $@ -hidir . -odir . -static -optl-static -ldl -lgpm --make @srcdir@/Main.hs

.c.o:
	$(HC) $(CFLAGS) $(GHCOPTS) -o $@ -c $<

CHEADERS= RSA_hsc.h Curses_hsc.h CWString_hsc.h

.hs.o: $(CHEADERS)
	$(HC) -i. -i@srcdir@ $(HCFLAGS) $(GHCOPTS) -hidir . -o $(@F) -c $<
	test $(@F) != $@ && cp -f $(@F) $@  || true
.lhs.o: $(CHEADERS)
	$(HC) -i. -i@srcdir@ $(HCFLAGS) $(GHCOPTS) -hidir . -o $(@F) -c $<

%.hi: %.o
	@:
%_hsc.c: %.hs
	@:
%_hsc.h: %.hs
	@:

$(ginsu_OBJECTS): $(CHEADERS)

%.hs: %.hsc
	$(HSC2HS) -C "$(CFLAGS)" -C -I. -C -I@srcdir@ -o $@ $<

.SECONDARY: _darcs/inventory
Changelog: _darcs/inventory
	darcs changes > $@

CLEANFILES = Gale/*.hi Gale/*.o Boolean/*.hi Boolean/*.o Doc/*.hi Doc/*.o *.hi *.o *_hsc.c *_hsc.h RSA.hs Curses.hs ginsu_static CWString.hs MyLocale.hs
DISTCLEANFILES = Version.hs ginsu.spec Boolean.o Algebra.o DocLike.o Chars.o
MAINTAINERCLEANFILES = ExampleConf.hs KeyHelpTable.hs



Curses_hsc.o : Curses_hsc.h my_curses.h
RSA_hsc.o : RSA_hsc.h my_rsa.h
CWString_hsc.o: CWString_hsc.h


#depend.make: $(HSNODISTSOURCES) $(ginsu_SOURCES)
#	 ghc -M -optdep-f -optdepdepend.make $(GHCOPTS)  @srcdir@/*.hs      @srcdir@/Boolean/*.hs @srcdir@/Doc/*.hs
#	 ghc -M -optdep-f -optdepdepend.make $(GHCOPTS) @builddir@/*.hs  @srcdir@/*.hs    @builddir@/Boolean/*.hs  @srcdir@/Boolean/*.hs



# -include depend.make

BINDISTDIR=$(PACKAGE)-$(VERSION)-`uname -s`-`uname -m`

bin-dist: ginsu ginsu_static ginsu.config.sample
	$(RM) -rf -- $(BINDISTDIR)
	mkdir $(BINDISTDIR)
	strip -- ginsu || true
	strip -- ginsu_static || true
	cp -- ginsu $(BINDISTDIR)
	cp -- ginsu_static $(BINDISTDIR)
	cp -- ginsu-mdk $(BINDISTDIR)
	cp -- ginsu.config.sample $(BINDISTDIR)
	tar cvf $(BINDISTDIR).tar $(BINDISTDIR)
	gzip -f -- $(BINDISTDIR).tar
	$(RM) -rf -- $(BINDISTDIR)

.PHONY: bin-dist depend clean


KeyHelpTable.hs: actions.def gen_keyhelp.prl
	perl ./gen_keyhelp.prl -h actions.def > $@ || rm -f -- $@

ExampleConf.hs: ginsu.config.sample
	echo "module ExampleConf(exampleConf) where" > ExampleConf.hs
	echo "{-# NOINLINE exampleConf #-}" >> ExampleConf.hs
	echo "exampleConf :: String" >> ExampleConf.hs
	echo "exampleConf = unlines [" >> ExampleConf.hs
	cat ginsu.config.sample | sed -e 's/"/\\"/g;s/.*/    "&",/' >> ExampleConf.hs
	echo "  \"\"]" >> ExampleConf.hs

PUBLISH_DIR = /home/john/public_html/computer/ginsu

docs/ginsu-manual.html : docs/ginsu-manual.txt actions.def
	cd docs && mup.prl -s wiki.css ginsu-manual.txt > ginsu-manual.html

publish-docs: docs/ginsu-manual.html
	cp docs/Changelog.old $(PUBLISH_DIR)
	cp docs/ginsu-manual.html $(PUBLISH_DIR)
	cp docs/wiki.css $(PUBLISH_DIR)

ARCH = x86_64

publish: depend ginsu bin-dist dist rpm docs/ginsu-manual.html
	rm -f -- $(PUBLISH_DIR)/drop/ginsu-*-Linux-$(ARCH).tar.gz
	rm -f -- $(PUBLISH_DIR)/drop/ginsu-*.rpm
	rm -f -- "$(PUBLISH_DIR)/drop/ginsu-$(VERSION).tar.gz"
	cp "$(BINDISTDIR).tar.gz" "$(PUBLISH_DIR)/drop"
	cp "ginsu-$(VERSION).tar.gz" "$(PUBLISH_DIR)/drop"
	cp "$(HOME)/var/rpm/RPMS/$(ARCH)/ginsu-$(VERSION)-1.$(ARCH).rpm" "$(PUBLISH_DIR)/drop"
	cp "$(HOME)/var/rpm/SRPMS/ginsu-$(VERSION)-1.src.rpm" "$(PUBLISH_DIR)/drop"
	cp Changelog $(PUBLISH_DIR)
	cp docs/Changelog.old $(PUBLISH_DIR)
	cp docs/ginsu-manual.html $(PUBLISH_DIR)
	cp docs/wiki.css $(PUBLISH_DIR)
	./ginsu --man > "$(PUBLISH_DIR)/man.txt"
	make -C $(PUBLISH_DIR)
	

rpm: depend ginsu.spec dist
	cp ginsu-$(VERSION).tar.gz $(HOME)/var/rpm/SOURCES/
	rpmbuild -ba ginsu.spec
	
