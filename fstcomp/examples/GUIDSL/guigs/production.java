import javax.swing.*;
import java.awt.*;
import Jakarta.util.*;
import java.awt.event.*;
import java.math.BigInteger;
import java.util.*;

class production  {
   private static boolean toplevel = true;

    public JComponent draw (int several) {
       pattern p;
       // Step 1: create vertical panel and add patterns
       JPanel panel = new JPanel();
       panel.setLayout( new BoxLayout(panel, BoxLayout.Y_AXIS) );
       int size = pat.size();
       Iterator i = pat.iterator();

       boolean flag = false;//indicates whether we've started a new tab this time
       if (toplevel || var.tab){//either top level or annotation
            panel = new JPanel();
            panel.setLayout( new BoxLayout(panel, BoxLayout.Y_AXIS) );
            panel.setVisible(true);
            Gui.tabs.insertTab(var.disp, null, panel, "See " + var.disp, Gui.tabs.getTabCount());
            toplevel = false;
            flag = true;
       }
       if (size==1) {//consists of only one pattern
          p = (pattern) i.next();
           boolean ntd = p.hasNonOpt();
          if (several == 1 && ntd){//if optional parent has non-optional child
             JPanel row = new JPanel();
             row.setLayout( new FlowLayout(FlowLayout.LEFT) );
           JCheckBox cb = new JCheckBox(p.var.disp);
          // JCheckBox cb = new JCheckBox("ADD:");
            cb.setToolTipText("Optional");
            row.add(cb);
            p.setWidget(cb);
            row.add( p.draw(several) );
            panel.add( row);
        }
        else
          panel.add(p.draw(several));
       }
        else {
            int min = 0, max = 0;
            if (several == 1){
                min = 0; max = 1;
            } else if (several == 2){
                min = 1; max = 100;
            } else if (several == 0){
                min = 1; max = 1;
            }
            GroupButtons gp = new GroupButtons(min, max);//form a group of buttons

            while ( i.hasNext() ) {//iterate through the patterns
                p = (pattern) i.next();
                JPanel row = new JPanel();
                row.setLayout( new FlowLayout(FlowLayout.LEFT) );
                AbstractButton ab;
                if (several == 1 || several == 2){ //optional production, or plus production
                    JCheckBox cb = new JCheckBox(p.var.disp);
                    row.add(cb);
                    gp.addToGroup(cb);
                    ab = cb;
                    if(several == 2) //plus
                        ab.setToolTipText("Select at least one from this group");
                    if (several == 1)//opt
                        ab.setToolTipText("Optional");
                }
                else if (several == 3){//star production
                    JCheckBox cb = new JCheckBox(p.var.disp);
                    row.add(cb);
                    ab = cb;
                    ab.setToolTipText("Select zero or more from this group");
                }
                else{
                    JRadioButton jb = new JRadioButton(p.var.disp);
                    row.add( jb );
                    gp.addToGroup(jb);
                    jb.setToolTipText("Select exactly one from this group");
                    ab = jb;
                }
                p.setWidget(ab);
                row.add( p.draw(several) );//call the draw method of whatever the pattern points to
                panel.add( row);// add the entire thing to the panel
            }
       }


       if (flag){//if tab were inserted at this step
            //add a button to lead over to the next tab
            JButton seeNext = new JButton("See next: " + var.disp);
            final JPanel thisone = panel;
            seeNext.addMouseListener(new MouseListener(){
                    public void mouseReleased(MouseEvent e){}
                    public void mouseExited(MouseEvent e){}
                    public void mousePressed(MouseEvent e){}
                    public void mouseClicked(MouseEvent e){
                        Gui.tabs.setSelectedComponent(thisone);}
                    public void mouseEntered(MouseEvent e){}
                    });
            seeNext.setToolTipText("Proceed for more selections");
            return seeNext;
       }

       // Step 2: add title
       if (var == null)
          Util.fatalError( "var is null " + name );
       panel.setBorder( BorderFactory.createTitledBorder(var.disp) );

        return panel;
    }
}
