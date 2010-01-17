
package net.sf.jabref;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.net.URL;

public class SidePaneHeader extends JPanel implements ActionListener {

  private JButton close = new JButton(GUIGlobals.getImage("close2"));
    private JLabel nameLabel;
    private SidePaneComponent parent;
    private GridBagLayout gbl = new GridBagLayout();
    private GridBagConstraints con = new GridBagConstraints();

    

    public SidePaneHeader(String name, URL image, SidePaneComponent parent_) {
    addPart(name, image, parent_);
    }

    public void paintComponent(Graphics g) {
      Graphics2D g2 = (Graphics2D)g;
      Paint oldPaint = g2.getPaint();
      
      Insets ins = getInsets();
      int width = getWidth() - ins.left - ins.right,
          height = getHeight() - ins.top - ins.bottom;
      
     
     g2.setPaint(new GradientPaint(ins.left, ins.top, GUIGlobals.gradientGray,
                                   width, height, GUIGlobals.gradientBlue, false));
      g2.fillRect(ins.left, ins.top, width-1, height);
      
      g2.setPaint(oldPaint);
      
    }

    

    private void addPart(String name, URL image, SidePaneComponent parent_) {
    parent = parent_;
    setLayout(gbl);
        
        
    
    nameLabel = new JLabel(Globals.lang(name), new ImageIcon(image),
                   SwingConstants.LEFT);


        
        nameLabel.setForeground(new Color(230, 230, 230));
    
        
  
  
  close.setBorder(null);
  close.setOpaque(false);
  close.setPreferredSize(new Dimension(15, 15));
  close.setMaximumSize(new Dimension(15, 15));
  close.setMinimumSize(new Dimension(15, 15));
  close.addActionListener(this);

  
  
    
    con.insets = new Insets(1, 1, 1, 1);
    con.gridwidth = 1;
    con.anchor = GridBagConstraints.WEST;
    con.fill = GridBagConstraints.NONE;
    gbl.setConstraints(nameLabel, con);
    add(nameLabel);
    JPanel pan = new JPanel();
        pan.setOpaque(false);
    con.fill = GridBagConstraints.HORIZONTAL;
    con.weightx = 1;
    gbl.setConstraints(pan, con);
    add(pan);
    con.weightx = 0;
    con.fill = GridBagConstraints.NONE;
    con.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(close, con);
    add(close);
    }

    public void actionPerformed(ActionEvent e) {
    parent.hideAway(); 
    }
}
