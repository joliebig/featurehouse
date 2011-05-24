"""
Plugins for Core Server.
"""

__author__ = "Skip Montanaro <skip@pobox.com"

__credits__ = "The Spambayes folk."

class  Plugin :
	def __init__(self, name, ui):

        self.name = name

        self.ui = ui

        self.state = None


class  PluginUI :
	pass


