



package com.microstar.xml;

import com.microstar.xml.XmlHandler;
import com.microstar.xml.XmlException;
import java.io.Reader;



public class HandlerBase implements XmlHandler {

  
  public void startDocument () 
    throws java.lang.Exception
  {
  }

  
  public void endDocument ()
    throws java.lang.Exception
  {
  }

  
  public Object resolveEntity (String publicId, String systemId) 
    throws java.lang.Exception
  {
    return null;
  }


  
  public void startExternalEntity (String systemId)
    throws java.lang.Exception
  {
  }

  
  public void endExternalEntity (String systemId)
    throws java.lang.Exception
  {
  }

  
  public void doctypeDecl (String name, String publicId, String systemId)
    throws java.lang.Exception
  {
  }

  
  public void attribute (String aname, String value, boolean isSpecified)
    throws java.lang.Exception
  {
  }

  
  public void startElement (String elname)
    throws java.lang.Exception
  {
  }

  
  public void endElement (String elname)
    throws java.lang.Exception
  {
  }

  
  public void charData (char ch[], int start, int length)
    throws java.lang.Exception
  {
  }

  
  public void ignorableWhitespace (char ch[], int start, int length)
    throws java.lang.Exception
  {
  }

  
  public void processingInstruction (String target, String data)
    throws java.lang.Exception
  {
  }

  
  public void error (String message, String systemId, int line, int column)
    throws XmlException, java.lang.Exception
  {
    throw new XmlException(message, systemId, line, column);
  }

}
