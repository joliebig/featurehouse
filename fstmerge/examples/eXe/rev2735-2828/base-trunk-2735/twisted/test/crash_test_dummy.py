from twisted.python import components
from zope import interface
def foo():
    return 2
class X:
    def __init__(self, x):
        self.x = x
    def do(self):
        pass
class XComponent(components.Componentized):
    pass
class IX(components.Interface):
    pass
class XA(components.Adapter):
    interface.implements(IX)
    def method(self):
        pass
components.registerAdapter(XA, X, IX)
