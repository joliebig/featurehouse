try:
    import cStringIO as StringIO
except ImportError:
    import StringIO
from twisted.python import failure, log
from twisted.python.components import registerAdapter
from twisted.internet import defer
from twisted.pb import remoteinterface, copyable, slicer, schema, tokens
from tokens import BananaError, Violation, ISlicer
class PendingRequest(object):
    active = True
    methodName = None # for debugging
    def __init__(self, reqID, rref=None):
        self.reqID = reqID
        self.rref = rref # keep it alive
        self.broker = None # if set, the broker knows about us
        self.deferred = defer.Deferred()
        self.constraint = None # this constrains the results
    def setConstraint(self, constraint):
        self.constraint = constraint
    def complete(self, res):
        if self.broker:
            self.broker.removeRequest(self)
        if self.active:
            self.active = False
            self.deferred.callback(res)
        else:
            log.msg("PendingRequest.complete called on an inactive request")
    def fail(self, why):
        if self.active:
            if self.broker:
                self.broker.removeRequest(self)
            self.active = False
            self.failure = why
            log.msg("a callRemote(reqID=%d, rref=%s, methname=%s) failed" \
                    % (self.reqID, self.rref, self.methodName))
            log.msg(" the failure is: %s" % (why,))
            self.deferred.errback(why)
        else:
            log.msg("multiple failures")
            log.msg("first one was:", self.failure)
            log.msg("this one was:", why)
            log.err("multiple failures indicate a problem")
class CallSlicer(slicer.ScopedSlicer):
    opentype = ('call',)
    def __init__(self, reqID, clid, methodname, args):
        slicer.ScopedSlicer.__init__(self, None)
        self.reqID = reqID
        self.clid = clid
        self.methodname = methodname
        self.args = args
    def sliceBody(self, streamable, banana):
        yield self.reqID
        yield self.clid
        yield self.methodname
        keys = self.args.keys()
        keys.sort()
        for argname in keys:
            yield argname
            yield self.args[argname]
    def describe(self):
        return "<call-%s-%s-%s>" % (self.reqID, self.clid, self.methodname)
class CallUnslicer(slicer.ScopedUnslicer):
    stage = 0
    reqID = None
    obj = None
    interface = None
    methodname = None
    methodSchema = None # will be a MethodArgumentsConstraint
    argname = None
    argConstraint = None
    def start(self, count):
        self.args = {}
        self.deferred = defer.Deferred()
        self.num_unreferenceable_children = 0
        self.num_unready_children = 0
    def checkToken(self, typebyte, size):
        if self.stage == 0:
            if typebyte != tokens.INT:
                raise BananaError("request ID must be an INT")
        elif self.stage == 1:
            if typebyte not in (tokens.INT, tokens.NEG):
                raise BananaError("object ID must be an INT/NEG")
        elif self.stage == 2:
            if typebyte not in (tokens.STRING, tokens.VOCAB):
                raise BananaError("method name must be a STRING")
        elif self.stage == 3:
            if self.argname == None:
                if typebyte not in (tokens.STRING, tokens.VOCAB):
                    raise BananaError("argument name must be a STRING")
            else:
                if self.argConstraint:
                    self.argConstraint.checkToken(typebyte, size)
    def doOpen(self, opentype):
        if self.argConstraint:
            self.argConstraint.checkOpentype(opentype)
        unslicer = self.open(opentype)
        if unslicer:
            if self.argConstraint:
                unslicer.setConstraint(self.argConstraint)
        return unslicer
    def reportViolation(self, f):
        if f.value.args[0] == "ABORT received":
            return f
        if self.stage > 0:
            self.broker.callFailed(f, self.reqID)
        return f # give up our sequence
    def receiveChild(self, token, ready_deferred=None):
        if self.stage < 3:
            assert not isinstance(token, defer.Deferred)
            assert ready_deferred is None
        if self.stage == 0: # reqID
            self.reqID = token
            self.stage += 1
            assert not self.broker.activeLocalCalls.get(self.reqID)
            self.broker.activeLocalCalls[self.reqID] = self
            return
        if self.stage == 1: # objID
            self.objID = token
            self.obj = self.broker.getMyReferenceByCLID(token)
            if self.objID < 0:
                self.interface = None
            else:
                self.interface = self.obj.getInterface()
            self.stage = 2
            return
        if self.stage == 2: # methodname
            if self.objID < 0:
                self.methodSchema = getattr(self.obj, "methodSchema", None)
                self.methodname = None # TODO: give it something useful
                if self.broker.requireSchema and not self.methodSchema:
                    why = "This broker does not accept unconstrained " + \
                          "method calls"
                    raise Violation(why)
                self.stage = 3
                return
            methodname = token
            ms = None
            if self.interface:
                ms = self.interface.get(methodname)
                if not ms:
                    why = "method '%s' not defined in %s" % \
                          (methodname, self.interface.__remote_name__)
                    raise Violation(why)
            self.methodSchema = ms
            self.methodname = methodname
            if self.broker.requireSchema and not self.methodSchema:
                why = "This broker does not accept unconstrained method calls"
                raise Violation(why)
            self.stage = 3
            return
        if self.stage == 3: # argname/value pairs
            if self.argname == None:
                assert not isinstance(token, defer.Deferred)
                assert ready_deferred is None
                argname = token
                if self.args.has_key(argname):
                    raise BananaError("duplicate argument '%s'" % argname)
                ms = self.methodSchema
                if ms:
                    accept, self.argConstraint = ms.getArgConstraint(argname)
                    assert accept # TODO: discard if not
                self.argname = argname
            else:
                argvalue = token
                if isinstance(argvalue, defer.Deferred):
                    self.num_unreferenceable_children += 1
                    argvalue.addCallback(self.update, self.argname)
                    argvalue.addErrback(self.explode)
                self.args[self.argname] = argvalue
                self.argname = None
                if ready_deferred:
                    self.num_unready_children += 1
                    ready_deferred.addCallback(self.ready)
    def update(self, obj, argname):
        self.args[argname] = obj
        self.num_unreferenceable_children -= 1
        self.checkComplete()
        return obj
    def ready(self, obj):
        self.num_unready_children -= 1
        self.checkComplete()
        return obj
    def receiveClose(self):
        if self.stage != 3 or self.argname != None:
            raise BananaError("'call' sequence ended too early")
        self.stage = 4
        return self.checkComplete()
    def checkComplete(self):
        if self.stage != 4:
            return
        if self.num_unreferenceable_children or self.num_unready_children:
            return self.deferred, None
        if self.methodSchema:
            self.methodSchema.checkArgs(self.args)
        self.broker.doCall(self.reqID, self.obj, self.methodname,
                           self.args, self.methodSchema)
        self.deferred.callback(None)
        return None, None
    def describe(self):
        s = "<methodcall"
        if self.stage == 0:
            pass
        if self.stage == 1:
            s += " reqID=%d" % self.reqID
        if self.stage == 2:
            s += " obj=%s" % (self.obj,)
            ifacename = "[none]"
            if self.interface:
                ifacename = self.interface.__remote_name__
            s += " iface=%s" % ifacename
        if self.stage == 3:
            s += " .%s" % self.methodname
            if self.argname != None:
                s += " arg[%s]" % self.argname
        if self.stage == 4:
            s += " .close"
        s += ">"
        return s
class AnswerSlicer(slicer.ScopedSlicer):
    opentype = ('answer',)
    def __init__(self, reqID, results):
        slicer.ScopedSlicer.__init__(self, None)
        self.reqID = reqID
        self.results = results
    def sliceBody(self, streamable, banana):
        yield self.reqID
        yield self.results
    def describe(self):
        return "<answer-%s>" % self.reqID
class AnswerUnslicer(slicer.ScopedUnslicer):
    request = None
    resultConstraint = None
    haveResults = False
    def checkToken(self, typebyte, size):
        if self.request is None:
            if typebyte != tokens.INT:
                raise BananaError("request ID must be an INT")
        elif not self.haveResults:
            if self.resultConstraint:
                try:
                    self.resultConstraint.checkToken(typebyte, size)
                except Violation, v:
                    if v.args:
                        why = v.args[0] + " in inbound method results"
                        v.args = why,
                    else:
                        v.args = ("in inbound method results",)
                    raise v # this will errback the request
        else:
            raise BananaError("stop sending me stuff!")
    def doOpen(self, opentype):
        if self.resultConstraint:
            self.resultConstraint.checkOpentype(opentype)
        unslicer = self.open(opentype)
        if unslicer:
            if self.resultConstraint:
                unslicer.setConstraint(self.resultConstraint)
        return unslicer
    def receiveChild(self, token, ready_deferred=None):
        assert not isinstance(token, defer.Deferred)
        assert ready_deferred is None
        if self.request == None:
            reqID = token
            self.request = self.broker.getRequest(reqID)
            self.resultConstraint = self.request.constraint
        else:
            self.results = token
            self.haveResults = True
    def reportViolation(self, f):
        if self.request != None:
            self.request.fail(f)
        return f # give up our sequence
    def receiveClose(self):
        self.request.complete(self.results)
        return None, None
    def describe(self):
        if self.request:
            return "Answer(req=%s)" % self.request.reqID
        return "Answer(req=?)"
class ErrorSlicer(slicer.ScopedSlicer):
    opentype = ('error',)
    def __init__(self, reqID, f):
        slicer.ScopedSlicer.__init__(self, None)
        assert isinstance(f, failure.Failure)
        self.reqID = reqID
        self.f = f
    def sliceBody(self, streamable, banana):
        yield self.reqID
        yield self.f
    def describe(self):
        return "<error-%s>" % self.reqID
class ErrorUnslicer(slicer.ScopedUnslicer):
    request = None
    fConstraint = schema.FailureConstraint()
    gotFailure = False
    def checkToken(self, typebyte, size):
        if self.request == None:
            if typebyte != tokens.INT:
                raise BananaError("request ID must be an INT")
        elif not self.gotFailure:
            self.fConstraint.checkToken(typebyte, size)
        else:
            raise BananaError("stop sending me stuff!")
    def doOpen(self, opentype):
        self.fConstraint.checkOpentype(opentype)
        unslicer = self.open(opentype)
        if unslicer:
            unslicer.setConstraint(self.fConstraint)
        return unslicer
    def reportViolation(self, f):
        if self.request != None:
            self.request.fail(f)
        return f # give up our sequence
    def receiveChild(self, token, ready_deferred=None):
        assert not isinstance(token, defer.Deferred)
        assert ready_deferred is None
        if self.request == None:
            reqID = token
            self.request = self.broker.getRequest(reqID)
        else:
            self.failure = token
            self.gotFailure = True
    def receiveClose(self):
        self.request.fail(self.failure)
        return None, None
    def describe(self):
        if self.request is None:
            return "<error-?>"
        return "<error-%s>" % self.request.reqID
class FailureSlicer(slicer.BaseSlicer):
    slices = failure.Failure
    classname = "twisted.python.failure.Failure"
    def slice(self, streamable, banana):
        self.streamable = streamable
        yield 'copyable'
        yield self.classname
        state = self.getStateToCopy(self.obj, banana)
        for k,v in state.iteritems():
            yield k
            yield v
    def describe(self):
        return "<%s>" % self.classname
    def getStateToCopy(self, obj, broker):
        state = {}
        if isinstance(obj.value, failure.Failure):
            raise RuntimeError("not implemented yet")
        else:
            state['value'] = str(obj.value) # Exception instance
        state['type'] = str(obj.type) # Exception class
        if broker.unsafeTracebacks:
            io = StringIO.StringIO()
            obj.printTraceback(io)
            state['traceback'] = io.getvalue()
        else:
            state['traceback'] = 'Traceback unavailable\n'
        if len(state['traceback']) > 1900:
            state['traceback'] = (state['traceback'][:1900] +
                                  "\n\n-- TRACEBACK TRUNCATED --\n")
        state['parents'] = obj.parents
        return state
class CopiedFailure(failure.Failure, copyable.RemoteCopyOldStyle):
    """I am a shadow of some remote Failure instance. I contain less
    information than the original did.
    You can still extract a (brief) printable traceback from me. My .parents
    attribute is a list of strings describing the class of the exception
    that I contain, just like the real Failure had, so my trap() and check()
    methods work fine. My .type and .value attributes are string
    representations of the original exception class and exception instance,
    respectively. The most significant effect is that you cannot access
    f.value.args, and should instead just use f.value .
    My .frames and .stack attributes are empty, although this may change in
    the future (and with the cooperation of the sender).
    """
    nonCyclic = True
    stateSchema = schema.FailureConstraint()
    def __init__(self):
        copyable.RemoteCopyOldStyle.__init__(self)
    def setCopyableState(self, state):
        self.__dict__ = state
        self.tb = None
        self.frames = []
        self.stack = []
    def __str__(self):
        return "[CopiedFailure instance: %s]" % self.getBriefTraceback()
    pickled = 1
    def printTraceback(self, file=None, elideFrameworkCode=0,
                       detail='default'):
        if not file: file = log.logfile
        file.write("Traceback from remote host -- ")
        file.write(self.traceback)
    printBriefTraceback = printTraceback
    printDetailedTraceback = printTraceback
copyable.registerRemoteCopy(FailureSlicer.classname, CopiedFailure)
