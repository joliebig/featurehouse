import os
import re
from htmlentitydefs import name2codepoint
class HtmlToText(object):
  def __init__(self, html):
    self.html = html
  def convertToText(self):
    lastch = ""
    text   = ""
    tag    = ""
    whitespace = (' ', '\t', '\n' '\r')
    inBrackets = False
    for ch in self.html:
      if ch == "<":
        inBrackets = True
        tag = ch
      elif inBrackets:
        tag += ch
        if ch == ">":
          inBrackets = False
          if tag.lower() == "<br/>":
              text += os.linesep
        elif ch.lower() == "p" and lastch == "<":
          text += os.linesep * 2
      elif not (lastch in whitespace and ch in whitespace):         
        text += ch
      lastch = ch
    text = self.unescape(text)
    return text
  def unescape(self, data):
      """
      convert html entitydefs into unicode characters
      """
      chunks = re.split("&(#?\w+);", data)
      for i in range(1, len(chunks), 2):
          if chunks[i] in name2codepoint:
              chunks[i] = unichr(name2codepoint[chunks[i]])
          elif re.match("#\d+$", chunks[i]):
              chunks[i] = unichr(int(chunks[i][1:]))
          elif re.match("#x[0-9a-fA-F]+$", chunks[i]):
              chunks[i] = unichr(int(chunks[i][2:], 16))
      return "".join(chunks)
