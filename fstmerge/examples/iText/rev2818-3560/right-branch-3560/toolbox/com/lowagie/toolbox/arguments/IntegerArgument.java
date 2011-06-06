



package com.lowagie.toolbox.arguments;

import java.awt.event.ActionEvent;

import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.swing.CustomDialog;


public class IntegerArgument extends AbstractArgument {

    
    public IntegerArgument() {
    }

    
    public IntegerArgument(AbstractTool tool, String name, String description) {
        super(tool, name, description, null);
    }

    
    public void actionPerformed(ActionEvent e) {
        CustomDialog cd = new CustomDialog("Enter a value for " + name +
                                           ":",
                                           CustomDialog.instantiateIntegerDocument());
        setValue(cd.showInputDialog(this.getValue()==null?"0":this.getValue().toString()));
    }
}
