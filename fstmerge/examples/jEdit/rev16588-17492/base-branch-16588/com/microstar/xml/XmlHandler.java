



package com.microstar.xml;


public interface XmlHandler {

  
  public void startDocument ()
    throws java.lang.Exception;


  
  public void endDocument ()
    throws java.lang.Exception;


  
  public Object resolveEntity (String publicId, String systemId)
    throws java.lang.Exception;


  
  public void startExternalEntity (String systemId)
    throws java.lang.Exception;


  
  public void endExternalEntity (String systemId)
    throws java.lang.Exception;


  
  public void doctypeDecl (String name, String publicId, String systemId)
    throws java.lang.Exception;


  
  public void attribute (String name, String value, boolean isSpecified)
    throws java.lang.Exception;


  
  public void startElement (String elname)
    throws java.lang.Exception;


  
  public void endElement (String elname)
    throws java.lang.Exception;


  
  public void charData (char ch[], int start, int length)
    throws java.lang.Exception;


  
  public void ignorableWhitespace (char ch[], int start, int length)
    throws java.lang.Exception;


  
  public void processingInstruction (String target, String data)
    throws java.lang.Exception;


  
  public void error (String message, String systemId, int line, int column)
    throws java.lang.Exception;

}
