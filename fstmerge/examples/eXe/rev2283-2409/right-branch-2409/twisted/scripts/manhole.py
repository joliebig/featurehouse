"""Start a L{twisted.manhole} client.
@var toolkitPreference: A list of all toolkits we have front-ends for, with
   the ones we most prefer to use listed first.
@type toolkitPreference: list of strings
"""
import sys
from twisted.python import usage
toolkitPreference = ('gtk2', 'gtk1')
class NoToolkitError(usage.UsageError):
    wantToolkits = toolkitPreference
    def __str__(self):
        return (
            "I couldn't find any of these toolkits installed, and I need "
            "one of them to run: %s" % (', '.join(self.wantToolkits),))
def bestToolkit():
    """The most-preferred available toolkit.
    @returntype: string
    """
    avail = getAvailableToolkits()
    for v in toolkitPreference:
        if v in avail:
            return v
    else:
        raise NoToolkitError
_availableToolkits = None
def getAvailableToolkits():
    """Autodetect available toolkits.
    @returns: A list of usable toolkits.
    @returntype: list of strings
    """
    global _availableToolkits
    if _availableToolkits is not None:
        return _availableToolkits
    avail = []
    try:
        import pygtk
    except:
        pass
    else:
        gtkvers = pygtk._get_available_versions().keys()
        for v in gtkvers:
            frontend = {'1.2': 'gtk1',
                        '2.0': 'gtk2'}.get(v)
            if frontend is not None:
                avail.append(frontend)
    if not avail:
        try:
            import gtk
        except:
            pass
        else:
            avail.append('gtk1')
    _availableToolkits = avail
    return avail
def run():
    config = MyOptions()
    try:
        config.parseOptions()
    except usage.UsageError, e:
        print str(e)
        print str(config)
        sys.exit(1)
    try:
        run = getattr(sys.modules[__name__], 'run_' + config.opts['toolkit'])
    except AttributeError:
        print "Sorry, no support for toolkit %r." % (config.opts['toolkit'],)
        sys.exit(1)
    run(config)
    from twisted.internet import reactor
    reactor.run()
def run_gtk1(config):
    from twisted.internet import gtkreactor
    gtkreactor.install()
    from twisted.spread.ui import gtkutil
    sys.argv[:] = ['manhole']
    from twisted.manhole.ui import gtkmanhole
    i = gtkmanhole.Interaction()
    lw = gtkutil.Login(i.connected,
                       i.client,
                       initialUser=config.opts['user'],
                       initialPassword=config.opts['password'],
                       initialService=config.opts['service'],
                       initialHostname=config.opts['host'],
                       initialPortno=config.opts['port'],
                       initialPerspective=config.opts['perspective'])
    i.loginWindow = lw
    lw.show_all()
def run_gtk2(config):
    from twisted.internet import gtk2reactor
    gtk2reactor.install()
    from twisted.spread.ui import gtk2util
    sys.argv[:] = ['manhole']
    from twisted.manhole.ui import gtk2manhole
    o = config.opts
    defaults = {
        'host': o['host'],
        'port': o['port'],
        'identityName': o['user'],
        'password': o['password'],
        'serviceName': o['service'],
        'perspectiveName': o['perspective']
        }
    w = gtk2manhole.ManholeWindow()
    w.setDefaults(defaults)
    w.login()
pbportno = 8787
class MyOptions(usage.Options):
    optParameters=[("user", "u", "guest", "username"),
                   ("password", "w", "guest"),
                   ("service", "s", "twisted.manhole", "PB Service"),
                   ("host", "h", "localhost"),
                   ("port", "p", str(pbportno)),
                   ("perspective", "P", "",
                    "PB Perspective to ask for "
                    "(if different than username)"),
                   ("toolkit", "t", bestToolkit(),
                    "Front-end to use; one of %s"
                    % (' '.join(getAvailableToolkits()),)),
                   ]
    zsh_actions = {"host":"_hosts",
                   "toolkit":"(%s)" % (' '.join(getAvailableToolkits()),)}
if __name__ == '__main__':
    run()
