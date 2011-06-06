from twisted.scripts.mktap import _tapHelper
TwistedTelnet = _tapHelper(
    "Twisted Telnet Shell Server",
    "twisted.tap.telnet",
    "A simple, telnet-based remote debugging service.",
    "telnet")
