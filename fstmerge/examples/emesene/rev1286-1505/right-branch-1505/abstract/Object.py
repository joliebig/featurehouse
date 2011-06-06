'''a module to define a base object that can emit signals and call
methods that are connected to those signals'''
import gc
import weakref
import inspect
class Object(object):
    '''a base class to add signal support to our class'''
    def __init__(self):
        '''constructor'''
        self.signals = {}
        self.callbacks = {}
    def signal_add(self, name, param_count):
        '''add a supported signal to the object with the name 
        "name" and the number of parameters param_count'''
        if name in self.signals:
            raise Exception("Signal already defined")
        self.signals[name] = param_count
        self.callbacks[name] = []
    def signal_connect(self, name, callback, *args):
        '''connect the callback to the signal "name", add optional
        user parameter to the callback args.
        The number of parameters received by the callback are checked
        and an Exception is raised if the number of parameters does
        not match with the defined param_count of the signal.
        The signal is connected with a weakref, so if only reference
        to your object are from signal connections, it will be
        garbage collected'''
        if name not in self.signals:
            raise Exception("signal '%s' not defined" % (name,))
        arg_spec = inspect.getargspec(callback)
        if not arg_spec[1]:
            args_num = len(arg_spec[0])
            if (inspect.isfunction(callback) and 
                args_num != self.signals[name] + 1 + len(args)):
                raise TypeError(
                    "invalid number of arguments (%d required %d found)" % \
                    (self.signals[name] + 1 + len(args), args_num))
            elif (inspect.ismethod(callback) and 
                args_num != self.signals[name] + 2 + len(args)):
                raise TypeError(
                    "invalid number of arguments (%d required %d found)" % \
                    (self.signals[name] + 2 + len(args), args_num))
        self.callbacks[name].append((callback, args))
    def signal_emit(self, name, *args):
        '''emit the signal of name "name" using args "args"
        args must match the number of arguments of the signal
        and name must exist'''
        if name not in self.signals:
            raise ValueError("signal '%s' not defined" % (name,))
        for (callback, user_args) in self.callbacks[name]: 
            all_args = list(args or []) + list(user_args or [])
            args_num = len(all_args)
            if self.signals[name] + len(user_args) != args_num:
                raise TypeError("incorrect number of parameters \
(%d required %d found)" % (self.signals[name], args_num))
            if inspect.ismethod(callback) and \
                    len(gc.get_referrers(callback.im_self)) == 1:
                del self.signals[name]
            else:
                callback(self, *all_args)
