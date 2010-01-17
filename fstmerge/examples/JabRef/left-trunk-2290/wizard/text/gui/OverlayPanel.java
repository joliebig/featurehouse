











package net.sf.jabref.wizard.text.gui ;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;

import javax.swing.*;

public class OverlayPanel extends JPanel
{

  private JLabel label ;
  private JComponent overlay ;

  private JScrollPane scroller ;

  public OverlayPanel(JComponent container, String text)
  {
    OverlayLayout layout = new OverlayLayout(this) ;
    this.setLayout( layout );
    overlay = container ;

    label = new JLabel(text) ;
    label.setFont(new Font("dialog", Font.ITALIC, 18));

    label.setForeground( new Color(224, 220, 220) );
    label.setLocation(0, 0);

     scroller = new JScrollPane( overlay ) ;
     scroller.setLocation(0, 0);

     scroller.setVerticalScrollBarPolicy( JScrollPane.VERTICAL_SCROLLBAR_ALWAYS ) ;

    add(label) ;
    add(scroller) ;
  }

  public void paint(Graphics g)
  {
    int len = label.getWidth() ;

    Dimension dim = this.getSize() ;
    if ((dim.height > 25) && (dim.width > len+10))
    {
      int x = (dim.width-len) / 2 ;
      int y = dim.height / 2 ;

      label.setLocation(x, y) ;
    }

    super.paint(g);
  }


}
