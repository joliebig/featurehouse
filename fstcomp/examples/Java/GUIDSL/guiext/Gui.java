//created on: Wed Sep 22 15:04:49 CDT 2004
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;

class Gui{
   Gui() {
        finishInit();
   }

   Gui(String AppTitle){
        finishInit();
   }
    static public HashMap visibleObjects; //contains JComponents (buttons and checkboxes) linked to their respective variables

    //completes the gui initialization by  adding text area listeners to all radio buttons and checkboxes
    public void finishInit(){
        visibleObjects = new HashMap();
        Iterator i = variable.Vtable.values().iterator();
        while ( i.hasNext() ){
            variable v = ( variable ) i.next();
            if (v.widget != null && v.hidden == false){//has an associated checkbox or radiobutton, not hidden
                if (AbstractButton.class.isInstance(v.widget))
                    visibleObjects.put(v.widget, v);
            }
        }
        if (visibleObjects.keySet() != null){
            Iterator it = visibleObjects.keySet().iterator();
            while ( it.hasNext() ){
                final AbstractButton w = (AbstractButton)it.next();
                w.addActionListener(new ActionList());
                w.addMouseListener(new MouseListener(){
                    public void mouseReleased(MouseEvent e){}
                    public void mouseExited(MouseEvent e){}
                    public void mousePressed(MouseEvent e){}
                    public void mouseClicked(MouseEvent e){}
                    public void mouseEntered(MouseEvent e){
                        if (showhelp == 1){
                            variable var = (variable)(visibleObjects.get(w));
                            if (var.help != null){
                                tarea.setText(var.help);

                            }
                            else
                                tarea.setText("No help available for this variable");
                            }
                         else if (showhelp == 0)
                            tarea.setText(((variable)visibleObjects.get(w)).explainValue());//display reason
                    }
                 });
            }
       }
  }
}
