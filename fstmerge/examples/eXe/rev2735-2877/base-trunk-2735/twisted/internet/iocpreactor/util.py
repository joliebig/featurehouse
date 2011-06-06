from twisted.python import log
class StateEventMachineType(type):
    def makeHandleGetter(klass, name):
        def helpful(self):
            return getattr(self, "handle_%s_%s" % (self.state, name))
        return helpful
    makeHandleGetter = classmethod(makeHandleGetter)
    def makeMethodProxy(klass, name):
        def helpful(self, *a, **kw):
            return getattr(self, "handle_%s_%s" % (self.state, name))(*a, **kw)
        return helpful
    makeMethodProxy = classmethod(makeMethodProxy)
    def __init__(klass, name, bases, attrs):
        type.__init__(klass, name, bases, attrs)
        for e in klass.events:
            setattr(klass, e, klass.makeMethodProxy(e))
