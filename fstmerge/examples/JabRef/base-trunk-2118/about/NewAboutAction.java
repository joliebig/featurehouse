









package net.sf.jabref.about ;

import java.awt.event.* ;
import javax.swing.* ;

import net.sf.jabref.* ;

public class NewAboutAction
    extends MnemonicAwareAction
{

  private String type = null ; 
  private KeyStroke keyStroke = null ; 

  public NewAboutAction()
  {
    
    super(GUIGlobals.getImage("about")) ;
    putValue( NAME, "About JabRef" ) ;

    putValue( SHORT_DESCRIPTION, Globals.lang( "About JabRef" ) ) ;
  }

  public void actionPerformed( ActionEvent e )
  {
    About2 dialog = new About2((JFrame) null) ;
  }
}
