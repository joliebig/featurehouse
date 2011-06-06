



package com.lowagie.toolbox.arguments;

import java.awt.event.ActionEvent;

import javax.swing.JCheckBox;
import javax.swing.JOptionPane;

import com.lowagie.toolbox.AbstractTool;


public class BitsetArgument extends AbstractArgument {
    
    private JCheckBox[] options;

    
    public BitsetArgument(AbstractTool tool, String name, String description,
                          String[] options) {
        super(tool, name, description, null);
        this.options = new JCheckBox[options.length];
        for (int i = 0; i < options.length; i++) {
            this.options[i] = new JCheckBox(options[i]);
        }
    }

    
    public String getUsage() {
        StringBuffer buf = new StringBuffer(super.getUsage());
        buf.append("    possible options:\n");
        for (int i = 0; i < options.length; i++) {
            buf.append("    - ");
            buf.append(options[i].getText());
            buf.append('\n');
        }
        return buf.toString();
    }

    
    public void actionPerformed(ActionEvent evt) {
        Object[] message = new Object[1 + options.length];
        message[0] = "Check the options you need:";
        System.arraycopy(options, 0, message, 1, options.length);
        int result = JOptionPane.showOptionDialog(
                tool.getInternalFrame(),
                message,
                description,
                JOptionPane.OK_CANCEL_OPTION,
                JOptionPane.QUESTION_MESSAGE,
                null,
                null,
                null
                     );
        if (result == 0) {
            StringBuffer buf = new StringBuffer();
            for (int i = 0; i < options.length; i++) {
                if (options[i].isSelected()) {
                    buf.append('1');
                } else {
                    buf.append('0');
                }
            }
            setValue(buf.toString());
        }
    }
}
