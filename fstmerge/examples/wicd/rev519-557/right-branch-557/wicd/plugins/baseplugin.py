class BasePlugin(object):
    PRIORITY = 1000
    def __init__(self, daemon):
        self.daemon = daemon
