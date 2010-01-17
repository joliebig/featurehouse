









package net.sf.jabref.wizard.integrity.gui ;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.sf.jabref.*;
import net.sf.jabref.undo.UndoableFieldChange;
import net.sf.jabref.wizard.integrity.IntegrityCheck;
import net.sf.jabref.wizard.integrity.IntegrityMessage;
import net.sf.jabref.wizard.text.gui.HintListModel;

public class IntegrityMessagePanel
    extends JPanel
    implements ListSelectionListener, KeyListener, ActionListener

{
  private JList warnings ;
  private HintListModel warningData ;

  private IntegrityCheck validChecker ;

  private JTextField content  ;
  private JButton applyButton ;
  private JButton fixButton ;
  private BasePanel basePanel;

  public IntegrityMessagePanel(BasePanel basePanel)
  {
    this.basePanel = basePanel;
    validChecker = new IntegrityCheck() ; 

  
    warningData = new HintListModel() ;
    warnings = new JList( warningData ) ;
    warnings.setCellRenderer( new IntegrityListRenderer() );
    warnings.addListSelectionListener(this);

    JScrollPane paneScrollPane = new JScrollPane( warnings ) ;
    paneScrollPane.setVerticalScrollBarPolicy(
        JScrollPane.VERTICAL_SCROLLBAR_ALWAYS ) ;
    paneScrollPane.setPreferredSize( new Dimension( 540, 255 ) ) ;
    paneScrollPane.setMinimumSize( new Dimension( 10, 10 ) ) ;

  
    JPanel fixPanel = new JPanel() ;


    JLabel label1 = new JLabel(Globals.lang("Field_content")) ;

    content = new JTextField(40) ;
    content.addKeyListener(this);
    applyButton = new JButton(Globals.lang("Apply")) ;
    applyButton.addActionListener(this) ;
    applyButton.setEnabled(false);
    fixButton = new JButton(Globals.lang("Suggest")) ;
    fixButton.setEnabled(false);

    fixPanel.add(label1) ;
    fixPanel.add(content) ;
    fixPanel.add(applyButton) ;
    fixPanel.add(fixButton) ;

  
    this.setLayout( new BorderLayout() );
    this.add( paneScrollPane, BorderLayout.CENTER ) ;
    this.add( fixPanel, BorderLayout.SOUTH) ;
  }

  

  public void updateView( BibtexEntry entry )
  {
    warningData.clear();
    IntegrityMessage.setPrintMode( IntegrityMessage.SINLGE_MODE) ;
    warningData.setData( validChecker.checkBibtexEntry( entry ) ) ;
  }

  public void updateView( BibtexDatabase base )
  {
    warningData.clear();
    IntegrityMessage.setPrintMode( IntegrityMessage.FULL_MODE) ;
    warningData.setData( validChecker.checkBibtexDatabase( base ) ) ;
  }


  
  
  public void valueChanged( ListSelectionEvent e )
  {
    if ( e.getValueIsAdjusting() )
    {
      Object obj = warnings.getSelectedValue() ;
      String str = "" ;
      if (obj != null)
      {
        IntegrityMessage msg = (IntegrityMessage) obj ;
        BibtexEntry entry = msg.getEntry() ;

        if (entry != null)
        {
          str = (String) entry.getField(msg.getFieldName()) ;
          basePanel.highlightEntry(entry);
  
  
  
        }
      }
      content.setText(str);
      applyButton.setEnabled(false);
    }
  }



  public void keyPressed( KeyEvent e )
  {
  }

  public void keyReleased( KeyEvent e )
  {
    applyButton.setEnabled(true);
    if (e.getKeyCode() == KeyEvent.VK_ENTER)
    {
      applyButton.doClick();
    }
  }

  public void keyTyped( KeyEvent e )
  {
  }

  public void actionPerformed( ActionEvent e )
  {
    Object obj = e.getSource() ;
    if (obj == applyButton)
    {
      Object data = warnings.getSelectedValue() ;
      if (data != null)
      {
        IntegrityMessage msg = (IntegrityMessage) data ;
        BibtexEntry entry = msg.getEntry() ;

        if (entry != null)
        {

            String oldContent = entry.getField(msg.getFieldName());
            UndoableFieldChange edit = new UndoableFieldChange(entry, msg.getFieldName(), oldContent,
                        content.getText());
            entry.setField(msg.getFieldName(), content.getText());
            basePanel.undoManager.addEdit(edit);
            basePanel.markBaseChanged();
            msg.setFixed(true);

          warningData.valueUpdated(warnings.getSelectedIndex()) ;
        }
      }

      applyButton.setEnabled(false);
    }
  }
  
  
  class IntegrityListRenderer extends DefaultListCellRenderer
  {
    final ImageIcon warnIcon = GUIGlobals.getImage("integrityWarn");
    final ImageIcon infoIcon = GUIGlobals.getImage("integrityInfo");
    final ImageIcon failIcon = GUIGlobals.getImage("integrityFail");
    final ImageIcon fixedIcon = GUIGlobals.getImage("complete");

    public Component getListCellRendererComponent(
        JList list,
        Object value, 
        int index, 
        boolean iss, 
        boolean chf ) 
    {
      super.getListCellRendererComponent( list, value, index, iss, chf ) ;

      if (value != null)
      {
        IntegrityMessage msg = (IntegrityMessage) value ;
        if (msg.getFixed())
        {
          setIcon(fixedIcon) ;
        }
        else
        {
          int id = msg.getType() ;
          if ( id < 1000 )
            setIcon( infoIcon ) ;
          else if ( id < 2000 )
            setIcon( warnIcon ) ;
          else setIcon( failIcon ) ;
        }
      }
      return this ;
    }
  }

}
