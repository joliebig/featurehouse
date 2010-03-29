import java.awt.event.*;
import java.util.*;
import javax.swing.*;

//created on: Wed Sep 22 15:08:01 CDT 2004
class ActionList implements ActionListener{

    public static void setGui(){
    //set the right values (and enabled/disabled) to the buttons and checkboxes
        Iterator it = variable.Vtable.values().iterator();
        while ( it.hasNext() ) {
            variable v = ( variable ) it.next();
            if (v.widget != null && v.hidden == false){
                if (JToggleButton.class.isInstance(v.widget)){
                    JToggleButton ab = (JToggleButton)(v.widget);
                    if (v.value == variable.T)
                        ab.setSelected(true);
                    else
                        ab.setSelected(false);//means we're either false or unknown here

                    if (v.userSet == false && v.value != variable.U) // not set by user, value is known
                        ab.setEnabled(false);
                    else
                        ab.setEnabled(true);
                }
                else if (JPanel.class.isInstance(v.widget)){
                    JPanel ab = (JPanel)(v.widget);

                    if (v.userSet == false) // not user set, set by system
                        ab.setEnabled(false);
                    else
                        ab.setEnabled(true);

                }
            }
        }
    }

    public void actionPerformed(ActionEvent e){
        //something from the buttons and checkboxes has been changed
        grammar.UserSelections.clear();//empty the list
        Iterator i = Gui.visibleObjects.keySet().iterator();
        while ( i.hasNext() ){
            AbstractButton w = (AbstractButton)i.next();
            if (w.isSelected() && w.isEnabled())
                grammar.UserSelections.add(Gui.visibleObjects.get(w)); //add the selected variable to user selections list
        }

       grammar.propagate();
       setGui();
    }
}