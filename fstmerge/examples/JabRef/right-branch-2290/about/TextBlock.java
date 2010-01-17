









package net.sf.jabref.about ;

import java.util.*;

public class TextBlock
{
  private Vector textLines ;
  private AboutTextLine headLine ;
  private boolean visible ;

  public TextBlock()
  {
    textLines = new Vector() ;
    visible = false ;
  }



  public void add(AboutTextLine line)
  {
    textLines.add(line);
  }

  public Enumeration getEnumeration() { return textLines.elements() ; }


  public void setHeading(AboutTextLine head)
  {
    headLine = head ;
  }

  public AboutTextLine getHeading() { return headLine ; }


  public boolean isVisible()
  {
    return visible;
  }

  public void setVisible(boolean pVisible)
  {
    this.visible = pVisible;
  }


}
