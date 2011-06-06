



package com.lowagie.toolbox.arguments;

import java.awt.event.ActionEvent;

import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.swing.CustomDialog;


public class StringArgument extends AbstractArgument {

    
    public StringArgument() {
    }

    
    public StringArgument(AbstractTool tool, String name, String description) {
        super(tool, name, description, null);
    }

    
    public void actionPerformed(ActionEvent e) {
        CustomDialog cd = new CustomDialog("Enter a value for " + name +
                                           ":",
                                           CustomDialog.
                                           instantiateStringDocument());
        setValue(cd.showInputDialog(this.getValue()==null?"":this.getValue().toString()));
    }
}
