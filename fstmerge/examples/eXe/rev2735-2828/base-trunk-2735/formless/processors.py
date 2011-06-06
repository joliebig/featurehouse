import warnings
from zope.interface import implements
from twisted.python import components
from nevow.util import Deferred, DeferredList, getPOSTCharset
from nevow.tags import *
from nevow import inevow
from nevow.context import WovenContext
import formless
from formless.formutils import enumerate
from formless import iformless
faketag = html()
def exceptblock(f, handler, exception, *a, **kw):
    try:
        result = f(*a, **kw)
    except exception, e:
        return handler(e)
    if isinstance(result, Deferred):
        def _(fail):
            fail.trap(exception)
            return handler(fail.value)
        return result.addErrback(_)
    else:
        return result
class ProcessGroupBinding(components.Adapter):
    implements(iformless.IInputProcessor)
    def process(self, context, boundTo, data):
        spec = self.original.typedValue.iface.__spec__
        resultList = [None] * len(spec)
        message = ''
        results = {}
        failures = {}
        waiters = []
        for i, sub in enumerate(spec):
            def _process():
                resultList[i] = iformless.IInputProcessor(sub).process(context, boundTo, data, autoConfigure = False)
                resulti = resultList[i]
                results.update(resulti)
            def _except(e):
                errors = context.locate(iformless.IFormErrors)
                errors.updateErrors(getattr(context, 'key', ''), e.errors)
                pf = e.partialForm
                err = e.errors
                msg = e.formErrorMessage
                errors.setError(getattr(context, 'key', ''), msg)
                results.update(pf)
                failures.update(e.errors)
            maybe = exceptblock(_process, _except, formless.ValidateError)
            if isinstance(maybe, Deferred):
                waiters.append(maybe)
        def _finish(ignored):
            if not failures:
                for specobj, result in zip(spec, resultList):
                    specobj.configure(boundTo, result)
            else:
                raise formless.ValidateError(failures, 'Error:', results)
        return DeferredList(waiters).addBoth(_finish)
class ProcessMethodBinding(components.Adapter):
    implements(iformless.IInputProcessor)
    def process(self, context, boundTo, data, autoConfigure = True):
        """Knows how to process a dictionary of lists
        where the dictionary may contain a key with the same
        name as some of the arguments to the MethodBinding
        instance.
        """
        typedValue = self.original.typedValue
        results = {}
        failures = {}
        if data.has_key('----'):
            data[typedValue.arguments[0].name] = data['----']
            del data['----']
        for binding in typedValue.arguments:
            name = binding.name
            try:
                context = WovenContext(context, faketag)
                context.remember(binding, iformless.IBinding)
                results[name] = iformless.IInputProcessor(binding.typedValue).process(context, boundTo, data.get(name, ['']))
            except formless.InputError, e:
                results[name] = data.get(name, [''])[0]
                failures[name] = e.reason
        if failures:
            raise formless.ValidateError(failures, "Error:", results)
        if autoConfigure:
            def _except(e):
                failures[''] = e.reason # self.original.name
                raise formless.ValidateError(failures, e.reason, results)
            return exceptblock(self.original.configure, _except, formless.InputError,
                               boundTo, results)
        return results
class ProcessPropertyBinding(components.Adapter):
    implements(iformless.IInputProcessor)
    def process(self, context, boundTo, data, autoConfigure = True):
        """Knows how to process a dictionary of lists
        where the dictionary may contain a key with the
        same name as the property binding's name.
        """
        binding = self.original
        context.remember(binding, iformless.IBinding)
        result = {}
        try:
            result[binding.name] = iformless.IInputProcessor(binding.typedValue).process(context, boundTo, data.get(binding.name, ['']))
        except formless.InputError, e:
            result[binding.name] = data.get(binding.name, [''])
            raise formless.ValidateError({binding.name: e.reason}, e.reason, result)
        if autoConfigure:
            try:
                return self.original.configure(boundTo, result)
            except formless.InputError, e:
                result[binding.name] = data.get(binding.name, [''])
                raise formless.ValidateError({binding.name: e.reason}, e.reason, result)
        return result
class ProcessTyped(components.Adapter):
    implements(iformless.IInputProcessor)
    def process(self, context, boundTo, data):
        """data is a list of strings at this point
        """
        typed = self.original
        val = data[0]
        if typed.unicode:
            try:
                val = val.decode(getPOSTCharset(context), 'replace')
            except LookupError:
                val = val.decode('utf-8', 'replace')
        if typed.strip:
            val = val.strip()
        if val == '' or val is None:
            if typed.required:
                raise formless.InputError(typed.requiredFailMessage)
            else:
                return typed.null
        try:
            return typed.coerce(val, boundTo)
        except TypeError, e:
            warnings.warn('Typed.coerce takes two values now, the value to coerce and the configurable in whose context the coerce is taking place. %s %s' % (typed.__class__, typed))
            return typed.coerce(val)
class ProcessPassword(components.Adapter):
    implements(iformless.IInputProcessor)
    def process(self, context, boundTo, data):
        """Password needs to look at two passwords in the data,
        """
        typed = self.original
        pw1 = data[0]
        args = context.locate(inevow.IRequest).args
        binding = context.locate(iformless.IBinding)
        pw2 = args.get("%s____2" % binding.name, [''])[0]
        if typed.strip:
            pw1 = pw1.strip()
            pw2 = pw2.strip()
        if pw1 != pw2:
            raise formless.InputError("Passwords do not match. Please reenter.")
        elif pw1 == '':
            if typed.required:
                raise formless.InputError(typed.requiredFailMessage)
            else:
                return typed.null
        val = data[0]
        if typed.unicode:
            try:
                val = val.decode(getPOSTCharset(context), 'replace')
            except LookupError:
                val = val.decode('utf-8', 'replace')
        try:
            return typed.coerce(val, boundTo)
        except TypeError:
            warnings.warn('Typed.coerce takes two values now, the value to coerce and the configurable in whose context the coerce is taking place. %s %s' % (typed.__class__, typed))
            return typed.coerce(data[0])
class ProcessRequest(components.Adapter):
    implements(iformless.IInputProcessor)
    def process(self, context, boundTo, data):
        return context.locate(inevow.IRequest)
class ProcessContext(components.Adapter):
    implements(iformless.IInputProcessor)
    def process(self, context, boundTo, data):
        return context
class ProcessUpload(components.Adapter):
    implements(iformless.IInputProcessor)
    def process(self, context, boundTo, data):
        typed = self.original
        bind = context.locate(iformless.IBinding)
        fields = context.locate(inevow.IRequest).fields
        try:
            field = fields[bind.name]
        except KeyError:
            return ''
        def hasContent(field):
            """Test if the uploaded file has any content by looking for a single byte.
            """
            file = field.file
            pos = file.tell()
            file.seek(0)
            ch = file.read(1)
            file.seek(pos)
            return ch != ''
        if isinstance(field, list):
            fieldList = field
        else:
            fieldList = [field]
        for maybeEmptyField in fieldList:
            if maybeEmptyField.filename.strip() or hasContent(maybeEmptyField):
                break
        else:
            if typed.required:
                raise formless.InputError(typed.requiredFailMessage)
            else:
                return typed.null
        return field
def process(typed, data, configurable=None, ctx=None):
    if ctx is None:
        from nevow.testutil import FakeRequest
        from nevow import context
        fr = FakeRequest()
        if type(data) is dict:
            fr.args = data
        ctx = context.RequestContext(tag=fr)
        ctx.remember(fr, inevow.IRequest)
        ctx.remember(None, inevow.IData)
    try:
        return iformless.IInputProcessor(typed).process(ctx, configurable, data, autoConfigure=False)
    except TypeError:
        return iformless.IInputProcessor(typed).process(ctx, configurable, data)
