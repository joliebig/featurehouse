



package com.microstar.xml;



public class XmlException extends Exception
{
  private String message;
  private String systemId;
  private int line;
  private int column;


  
  public XmlException (String message, String systemId, int line, int column)
  {
    this.message = message;
    this.systemId = systemId;
    this.line = line;
    this.column = column;
  }


  
  public String getMessage ()
  {
    return message;
  }


  
  public String getSystemId ()
  {
    return systemId;
  }


  
  public int getLine ()
  {
    return line;
  }

  
  public int getColumn ()
  {
    return column;
  }

}
