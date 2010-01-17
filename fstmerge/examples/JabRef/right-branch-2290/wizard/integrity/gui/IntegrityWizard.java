










package net.sf.jabref.wizard.integrity.gui ;

import java.io.* ;
import java.net.* ;

import java.awt.* ;
import java.awt.event.* ;
import javax.swing.* ;
import javax.swing.border.* ;

import net.sf.jabref.* ;

public class IntegrityWizard
    extends JDialog
    implements ActionListener
{

  private JabRefFrame _frame ;
  private BibtexDatabase dbase ;
  private BasePanel basePanel;
  private JButton closeButton ;
  private JButton startButton ;
  private IntegrityMessagePanel warnPanel ;

  public IntegrityWizard( JabRefFrame frame, BasePanel basePanel)
  {
    super( frame, "dialog", false ) ;  

    _frame = frame ;
    this.basePanel = basePanel;
    dbase = basePanel.database();

    try
    {
      jbInit() ;
      pack() ;
    }
    catch ( Exception ex )
    {
      ex.printStackTrace() ;
    }
  }

  private void jbInit() {

    this.setResizable( false ) ;

    
    this.setTitle( Globals.lang("Integrity_check") ) ;
    
    
    warnPanel = new IntegrityMessagePanel(basePanel) ;


    
    JPanel buttonPanel = new JPanel() ;
    GridBagLayout gbl = new GridBagLayout() ;
    GridBagConstraints con = new GridBagConstraints() ;
    con.weightx = 0 ;
    con.insets = new Insets( 5, 10, 0, 10 ) ;
    con.fill = GridBagConstraints.HORIZONTAL ;

    
    startButton = new JButton(Globals.lang("Scan")) ;
    startButton.addActionListener( this) ;
    closeButton = new JButton(Globals.lang("Close"));
    closeButton.addActionListener( this) ;

    
    con.gridwidth = GridBagConstraints.REMAINDER ;
    gbl.setConstraints( startButton, con ) ;
    buttonPanel.add( startButton ) ;

    gbl.setConstraints( closeButton, con ) ;
    buttonPanel.add( closeButton ) ;

    
    

    JEditorPane infoText = null ;

    URL infoURL = JabRef.class.getResource(GUIGlobals.getLocaleHelpPath()
                                           +GUIGlobals.shortIntegrityCheck);
    if (infoURL != null)
      try
      {
        infoText = new JEditorPane() ;
        infoText.setEditable(false);
        infoText.setPreferredSize( new Dimension(220, 60));
        infoText.setMinimumSize( new Dimension(180, 50));
        infoText.setPage(infoURL);
        infoText.setBackground(GUIGlobals.infoField);
        infoText.setBorder(new EtchedBorder(EtchedBorder.LOWERED));

      }
      catch (IOException e)
      {
        infoText = null ;
      }

    

    
    Container content = this.getContentPane() ;
    content.setLayout( new BorderLayout());

    if (infoText != null) 
    {
      content.add( infoText, BorderLayout.PAGE_START ) ;
    }
    content.add(warnPanel, BorderLayout.CENTER) ;
    content.add(buttonPanel, BorderLayout.PAGE_END) ;
  }





  public void actionPerformed( ActionEvent e )
  {
    Object sender = e.getSource() ;

    if (sender == closeButton)
    {
      dispose() ;
    }
    else if (sender == startButton)
    {
      startButton.setEnabled(false);
      Runnable scanWork = new Runnable()
      {
        public void run()
        {
          warnPanel.updateView(dbase);
        }
      } ;
      SwingUtilities.invokeLater(scanWork);
      startButton.setEnabled(true);
    }
  }



}
