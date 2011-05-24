//created on: Sun Sep 26 12:37:58 CDT 2004
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;

/*
allows for functionality of select (min, max) from a group of buttons or checkboxes
commented out code can be uncommented to disallow invalid selections
*/

public class GroupButtons implements ActionListener{
    protected Vector buttons;

    private int min, max;

    public GroupButtons(int mn, int mx){
       buttons = new Vector();
        min = mn;
        max = mx;
    }

    public void addToGroup(AbstractButton jb){
        buttons.add(jb);
        jb.addActionListener(this);
    }

    public void actionPerformed(ActionEvent e){
        Object eobj = e.getSource();
        int sel = 0;
        Iterator it = buttons.iterator();
        while (it.hasNext()){
            AbstractButton cur = (AbstractButton)it.next();
            if (cur.isSelected())
                sel++;
        }
     /*   if (sel < min){
            AbstractButton bt = (AbstractButton)eobj;//pop up a message and set more options
            String opt = " option";
            if ( min > 1)
                opt = " options";

            JOptionPane.showMessageDialog(null,
                                     "Please select at least " + min +opt,
                                     "Invalid configuration",
                                    JOptionPane.INFORMATION_MESSAGE);
        }
        else if (sel > max){
            AbstractButton bt = (AbstractButton)eobj;//pop up a message and unset this option
            JOptionPane.showMessageDialog(null,
                                     "Please deselect some options first not to exceed "+max+" total",
                                     "Invalid configuration",
                                    JOptionPane.INFORMATION_MESSAGE);
        }
        */

    }

}

