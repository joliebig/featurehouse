## Process this file with automake to produce Makefile.in
##
## Makefile for hircules
##
## Author : Jens Petersen <petersen@haskell.org>
## Created: 2003-05-14
##
## Version $Revision: 1.7 $ from $Date: 2003/10/05 08:16:34 $
##
## Copyright (c) 2003 Jens-Ulrik Holger Petersen
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation; either
## version 2 of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.

LIBMODULES = Charset.hs Debug.hs Directories.hs Display.hs GdkKeys.hs GtkKeymap.hs Iconv.hs MaybeDo.hs TextBufferCreateTag.hs Threads.hs UTF8.hs Version.hs.in WordString.hs
PACKAGES = gtk2 network
LAMBDABOT = @LAMBDABOT@
HIRCSUBDIRS = lib $(LAMBDABOT) $(wildcard $(LAMBDABOT)/*Module)

bin_PROGRAMS = hircules
hircules_SOURCES = Main.hs Channel.hs GUI.hs IRC.hs CTCP.hs Config.hs EntryArea.hs LambdaBot.hsc $(addprefix lib/,$(LIBMODULES))
EXTRA_DIST = index.html hircules.spec Screenshot-Hircules.png ANNOUNCE 

HCFLAGS = -Wall $(addprefix -package ,$(PACKAGES)) $(addprefix -i,$(HIRCSUBDIRS))

hircules$(EXEEXT): $(hircules_SOURCES:.hs.in=.hs) LambdaBot.hs
	$(HC) --make -o $@ $(HCFLAGS) Main.hs
#	strip $@

%.hs: %.hsc
	hsc2hs $(addprefix -DLAMBDABOT=, $(LAMBDABOT)) -o $@ $<

LambdaBot.hs: config.status

ghci:
	$(subst ghc,ghci,$(HC)) $(HCFLAGS) $<

RPMARCH = i386

rpm: dist 
	rpmbuild -ta --define "_sourcedir ." --define "_srcrpmdir ." --define "_rpmdir ." $(distdir).tar.gz

-include Makefile.local
