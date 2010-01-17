

package net.sf.jabref.about ; 

import java.awt.Color; 
import java.awt.image.RGBImageFilter; 


 


class  HighlightFilter  extends RGBImageFilter {
	
  boolean brighter ;

	
  int percent ;

	
  private int middleX ;

	
  private int middleY ;

	
  private int dimX ;

	
  private int dimY ;

	
  private int distance = 0 ;

	
  private int startSize = 10 ;

	

  private int white = Color.white.getRGB() ;

	
  private int black = Color.black.getRGB() ;

	

  public HighlightFilter( boolean b, int p )
  {
    brighter = b ;
    percent = p ;
    canFilterIndexColorModel = true ;
  }


	

  public void setMiddle(int x, int y)
  {
    middleX = x/2 ;
    middleY = y/2 ;
    dimX = x ;
    dimY = y ;
    distance = startSize ;
  }


	

  public final void nextStep() { distance+= distance/1.5 +1; }


	

  public boolean isReady()
  {
    boolean back = false ;
    if ((dimX < distance) && (dimY < distance))
      back = true ;

    return back ;
  }


	

  public final int filterRGB( int x, int y, int rgb )
  {
    int back = rgb ;

    int x1 = x - middleX ;
    int y1 = y - middleY ;

    int dist = (int) Math.sqrt( Math.abs(2*x1*y1) ) ;

    if ( ((dist < distance) && (x != middleX) && (y != middleY)) ||
         (((x == middleX) || (y == middleY)) && (distance > 30)) )
    {
       back = white ;
    } else if ((dist == distance) && (dist > 20)) 
    {
      back = black ;
    }

    return back ;
  }



}
