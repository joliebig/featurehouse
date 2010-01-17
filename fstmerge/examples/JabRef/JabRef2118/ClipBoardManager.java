








package net.sf.jabref ; 

import java.awt.datatransfer.Clipboard ; 
import java.awt.datatransfer.ClipboardOwner ; 
import java.awt.datatransfer.Transferable ; 
import java.awt.datatransfer.StringSelection ; 
import java.awt.datatransfer.DataFlavor ; 
import java.awt.datatransfer.UnsupportedFlavorException ; 

import java.awt.Toolkit; 
import java.io.* ; 
import java.awt.datatransfer.*; 
import java.io.IOException; 

public  class  ClipBoardManager implements  ClipboardOwner {
	
  public static ClipBoardManager clipBoard = new ClipBoardManager() ;

	

  
  public void lostOwnership( Clipboard aClipboard, Transferable aContents )
  {
    
  }


	

  
  public void setClipboardContents( String aString )
  {
    StringSelection stringSelection = new StringSelection( aString ) ;
    Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard() ;
    clipboard.setContents( stringSelection, this ) ;
  }


	

  
  public String getClipboardContents()
  {
    String result = "" ;
    Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard() ;
    
    Transferable contents = clipboard.getContents( null ) ;
    boolean hasTransferableText = ( contents != null ) &&
        contents.isDataFlavorSupported( DataFlavor.stringFlavor ) ;
    if ( hasTransferableText )
    {
      try
      {
        result = ( String ) contents.getTransferData( DataFlavor.stringFlavor ) ;
      }
      catch ( UnsupportedFlavorException ex )
      {
        
        System.out.println( ex ) ;
      }
      catch ( IOException ex )
      {
        System.out.println( ex ) ;
      }
    }
    return result ;
  }



}
