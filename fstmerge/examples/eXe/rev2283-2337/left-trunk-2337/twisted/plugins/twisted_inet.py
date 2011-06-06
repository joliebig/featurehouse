from twisted.scripts.mktap import _tapHelper
TwistedINETD = _tapHelper(
    "Twisted INETD Server",
    "twisted.runner.inetdtap",
    "An inetd(8) replacement.",
    "inetd")
