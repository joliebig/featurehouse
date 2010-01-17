






package net.sf.jabref.about ;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;

import net.sf.jabref.Globals;

public class About2 extends JDialog implements ActionListener
{
  private static final long serialVersionUID = 1L ;

  
  public About2( JFrame parent )
  {
    super( parent, Globals.lang("About JabRef"), true ) ;



    JPanel contentPanel = new JPanel( new BorderLayout() ) ;
    contentPanel.setBackground( Color.white);

    setContentPane( contentPanel ) ;

    contentPanel.add( BorderLayout.CENTER, new ExtendedInfoPanel(this) ) ;

    this.setBackground( Color.white);

    pack() ;
    setResizable( false ) ;
    setLocationRelativeTo( parent ) ;
    setVisible( true ) ;
  }

  protected void processWindowEvent( WindowEvent e )
  {
    super.processWindowEvent( e ) ;
    if ( e.getID() == WindowEvent.WINDOW_CLOSING )
    {
      System.exit( 0 ) ;
    }
  }

  public void actionPerformed( ActionEvent e )
  {
    String cmd = e.getActionCommand() ;
    if ( cmd.equals( "close" ) )
    {

      setVisible( false ) ;
      dispose() ;

    }
    else if ( cmd.equals( "license" ) )
    {

    }
  }



}
