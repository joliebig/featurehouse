import os
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
    text = text.replace('&nbsp;','')
    text = text.replace('&lt;', '<')
    text = text.replace('&gt;', '>')
    text = text.replace('&quot;', '"')
    return text
