package net.sf.jabref; 
import java.awt.event.FocusListener; 
import java.awt.event.FocusEvent; 
import java.awt.*; 

import java.awt.Color; 
import java.awt.Component; 



public  class  FieldEditorFocusListener implements  FocusListener {
	

    public FieldEditorFocusListener() {
    }


	

    public void focusGained(FocusEvent event) {
        ((Component)event.getSource()).setBackground(GUIGlobals.activeEditor);
    }


	


    public void focusLost(FocusEvent event) {
        ((Component)event.getSource()).setBackground(Color.white);
    }



}
