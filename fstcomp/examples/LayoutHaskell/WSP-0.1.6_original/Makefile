main:
	./Setup.lhs configure
	./Setup.lhs build

install: main
	./Setup.lhs install

clean:
	./Setup.lhs clean

### only for the distributor's enjoyment

FILES= \
 LICENSE Makefile Setup.lhs WSP.cabal \
 $(HS_SOURCES) $(SCRIPT_SOURCES) $(CONFIG_SOURCES)

HS_SOURCES= \
 Webserver/MimeTypes.hs \
 Webserver/Global.hs \
 Webserver/ParseToken.hs \
 Webserver/Config.hs \
 Webserver/ConfigParser.hs \
 Webserver/Util.hs \
 Webserver/Response.hs \
 Webserver/Request.hs \
 Webserver/RunCGI.hs \
 Webserver/ErrorLogger.hs \
 Webserver/AccessLogger.hs \
 Webserver/Deployment.hs \
 Webserver/Compile.hs \
 Webserver/State.hs \
 Webserver/Dispatch.hs \
 Webserver/Deploy.hs \
 Webserver/Console.hs \
 Webserver/ConsoleDeployment.hs \
 Webserver/InitialDeployments.hs \
 Webserver/Server.hs \
 Webserver/Main.hs \

SCRIPT_SOURCES= \
 scripts/CGICounter.hs \
 scripts/CalcFix.hs \
 scripts/CalcHistory.hs \
 scripts/HelloWorld.hs \
 scripts/Multi.hs \
 scripts/Multiplication.hs \

CONFIG_SOURCES= \
 conf/myconfig.conf \

DISTFILE= $(PACKAGE).tgz
PACKAGE= $(PACKAGENAME)-$(VERSION)
PACKAGENAME= `awk '/^Name:/{print $$2}' WSP.cabal`
VERSION=  `awk '/^Version:/{print $$2}' WSP.cabal`

distribution:
	rm -rf /tmp/$(PACKAGE)
	mkdir -p /tmp/$(PACKAGE)
	rm -rf dist/$(DISTFILE)
	tar cf - $(FILES) | tar xfC - /tmp/$(PACKAGE)
	tar cvfzC dist/$(DISTFILE) /tmp $(PACKAGE)

WWWHOST = nakalele.informatik.uni-freiburg.de
WWWDIR  = $(HOME)/public/www/haskell/WASH
SSH     = ssh
SCP     = scp

install-distribution: distribution
	$(SSH) $(WWWHOST) mkdir -p $(WWWDIR)
	for f in $(DISTFILE) ; \
	do \
	$(SCP) -q dist/$$f $(WWWHOST):$(WWWDIR)/$$f; \
	$(SSH) $(WWWHOST) chmod 644 $(WWWDIR)/$$f ; \
	done
