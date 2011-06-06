'''a module to add a sandbox to methods, functions, objects and
classes to avoid them from raising exceptions'''
import types
import inspect
import functools 
class sandbox(object):
    '''decorator that will catch the exceptions of type
    exception_type and call the callback passing the function, the
    exception type and the exception object as parameters'''
    def __init__(self, callback, exception_type=Exception):
        self.callback = callback
        self.exception_type= exception_type
    def __call__(self, function):
        @functools.wraps(function)
        def wrapper(*args, **kwds):
            try:
                return function(*args, **kwds)
            except self.exception_type, exception:
                self.callback(function, self.exception_type, exception)
        return wrapper
def meta_decorator(decorator, *args, **kwds):
    '''return a metaclass that used on a class will decorate
    all the methods of the *class* with the decorator
    passing args and kwds to the decorator'''
    class MetaDecorator(type):
        def __init__(cls, name, bases, dct):
            type.__init__(cls, name, bases, dct)
            methods = [x for x in dct if isinstance(dct[x], 
                types.FunctionType)]
            dec = decorator(*args, **kwds)
            for method in methods:
                setattr(cls, method, dec(getattr(cls, method)))
    return MetaDecorator
class MethodSandbox(object):
    '''wrap a method with the sandbox decorator and return a callable
    object that is almost identical to the method''' 
    def __init__(self, method, callback, exception_type=Exception):
        functools.update_wrapper(self, method)
        self.method = method
        self.callback = callback
        self.exception_type = exception_type
    def __call__(self, *args, **kwds):
        try:
            return self.method(*args, **kwds)
        except self.exception_type, exception:
            self.callback(self.method, self.exception_type, exception)
def decorate_object(obj, decorator, *args, **kwds):
    '''wrap all the obj methods with the sandbox decorator, 
    and calling the callback parameter when an exception is raised
    it decorates all the methods on an *object*'''
    dec = decorator(*args, **kwds)
    [setattr(obj, method, dec(getattr(obj, method)))\
        for method in dir(obj) if inspect.ismethod(getattr(obj, method))]
