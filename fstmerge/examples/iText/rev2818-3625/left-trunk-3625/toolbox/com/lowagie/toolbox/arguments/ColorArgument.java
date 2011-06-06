


package com.lowagie.toolbox.arguments;

import java.awt.Color;
import java.awt.event.ActionEvent;

import javax.swing.JColorChooser;

import com.lowagie.toolbox.AbstractTool;


public class ColorArgument extends AbstractArgument {
    public ColorArgument() {
        super();
    }

    public ColorArgument(AbstractTool tool, String name, String description) {
        super(tool, name, description, null);
    }

    public Object getArgument() throws InstantiationException {
        if (value == null) {
            return null;
        }
        try {
            return Color.decode(value.toString());
        } catch (Exception e) {
            throw new InstantiationException(e.getMessage());
        }
    }

    public void actionPerformed(ActionEvent e) {
        Color initialColor = new Color(0xFF, 0xFF, 0xFF);
        if (value != null) {
            initialColor = Color.decode(value.toString());
        }
        Color newColor = JColorChooser.showDialog(tool.getInternalFrame(),
                                                  "Choose Color", initialColor);
        setValue("0x"
                 + Integer.toHexString(
                         (newColor.getRed() << 16)
                         | (newColor.getGreen() << 8)
                         | (newColor.getBlue() << 0)).toUpperCase());
    }


}
