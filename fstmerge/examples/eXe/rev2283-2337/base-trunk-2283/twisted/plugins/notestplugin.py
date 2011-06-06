"""Plugin module for use by the plugin system's unit tests.
Nothing to see here, really.
"""
import os
from twisted.python.util import sibpath
if os.path.exists(sibpath(__file__, 'dropin.cache')):
    import twisted.test.test_plugin as TEST
    assert not TEST.running
