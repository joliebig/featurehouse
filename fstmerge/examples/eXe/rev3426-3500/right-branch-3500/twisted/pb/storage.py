try:
    import cStringIO as StringIO
except ImportError:
    import StringIO
from twisted.pb import slicer, banana
class UnsafeRootUnslicer(slicer.RootUnslicer):
    topRegistry = [slicer.UnslicerRegistry, slicer.UnsafeUnslicerRegistry]
    openRegistry = [slicer.UnslicerRegistry, slicer.UnsafeUnslicerRegistry]
class StorageRootUnslicer(UnsafeRootUnslicer, slicer.ScopedUnslicer):
    def __init__(self):
        slicer.ScopedUnslicer.__init__(self)
        UnsafeRootUnslicer.__init__(self)
    def setObject(self, counter, obj):
        return slicer.ScopedUnslicer.setObject(self, counter, obj)
    def getObject(self, counter):
        return slicer.ScopedUnslicer.getObject(self, counter)
class UnsafeRootSlicer(slicer.RootSlicer):
    slicerTable = slicer.UnsafeSlicerTable
class StorageRootSlicer(UnsafeRootSlicer):
    def __init__(self, protocol):
        UnsafeRootSlicer.__init__(self, protocol)
        self.references = {}
    def registerReference(self, refid, obj):
        self.references[id(obj)] = (obj,refid)
    def slicerForObject(self, obj):
        obj_refid = self.references.get(id(obj), None)
        if obj_refid is not None:
            return slicer.ReferenceSlicer(obj_refid[1])
        return UnsafeRootSlicer.slicerForObject(self, obj)
class StorageBanana(banana.Banana):
    slicerClass = StorageRootSlicer
    unslicerClass = StorageRootUnslicer
    def receivedObject(self, obj):
        self.object = obj
def serialize(obj):
    """Serialize an object graph into a sequence of bytes. Returns a Deferred
    that fires with the sequence of bytes."""
    b = StorageBanana()
    b.transport = StringIO.StringIO()
    d = b.send(obj)
    d.addCallback(lambda res: b.transport.getvalue())
    return d
def unserialize(str):
    """Unserialize a sequence of bytes back into an object graph."""
    b = StorageBanana()
    b.dataReceived(str)
    return b.object
