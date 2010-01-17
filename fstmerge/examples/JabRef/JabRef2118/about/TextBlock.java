









package net.sf.jabref.about ; 

import java.util.*; 

import java.util.Iterator; 
import java.util.Vector; 

public  class  TextBlock implements  Iterable<AboutTextLine> {
	
  

	
  private AboutTextLine headLine ;

	
  private boolean visible ;

	

  public TextBlock()
  {
    textLines = new Vector<AboutTextLine>() ;
    visible = false ;
  }


	



  public void add(AboutTextLine line)
  {
    textLines.add(line);
  }


	

  


	


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


	

  private Vector<AboutTextLine> textLines ;

	

  public Iterator<AboutTextLine> iterator() { 
	  return textLines.iterator(); 
  }


}
