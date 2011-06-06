from matplotlib.testing.noseclasses import KnownFailureTest
import sys
def knownfailureif(fail_condition, msg=None):
    if msg is None:
        msg = 'Test known to fail'
    def known_fail_decorator(f):
        import nose
        def failer(*args, **kwargs):
            try:
                result = f(*args, **kwargs)
            except:
                if fail_condition:
                    raise KnownFailureTest(msg)
                else:
                    raise
            return result
        return nose.tools.make_decorator(f)(failer)
    return known_fail_decorator
