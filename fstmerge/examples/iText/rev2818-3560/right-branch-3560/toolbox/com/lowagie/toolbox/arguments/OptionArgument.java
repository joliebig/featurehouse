



package com.lowagie.toolbox.arguments;

import java.awt.event.ActionEvent;
import java.util.TreeMap;

import javax.swing.JComboBox;
import javax.swing.JOptionPane;

import com.lowagie.toolbox.AbstractTool;


public class OptionArgument extends AbstractArgument {

    
    public class Entry {
        
        private Object description;
        
        private Object value;
        
        public Entry(Object value) {
            this.value = value;
            this.description = value;
        }

        
        public Entry(Object description, Object value) {
            this.description = description;
            this.value = value;
        }

        
        public String toString() {
            return description.toString();
        }

        
        public String getValueToString() {
            return value.toString();
        }

        
        public Object getDescription() {
            return description;
        }

        
        public void setDescription(Object description) {
            this.description = description;
        }

        
        public Object getValue() {
            return value;
        }

        
        public void setValue(Object value) {
            this.value = value;
        }
    }


    private TreeMap<String, Entry> options = new TreeMap<String, Entry>();

    
    public OptionArgument(AbstractTool tool, String name, String description) {
        super(tool, name, description, null);

    }

    
    public void addOption(Object description, Object value) {
        options.put(value.toString(), new Entry(description, value));
    }

    
    public Object getArgument() throws InstantiationException {
        if (value == null) {
            return null;
        }
        try {
            return options.get(value).getValue();
        } catch (Exception e) {
            throw new InstantiationException(e.getMessage());
        }
    }

    
    public String getUsage() {
        StringBuffer buf = new StringBuffer(super.getUsage());
        buf.append("    possible options:\n");
        for (Entry entry: options.values()) {
            buf.append("    - ");
            buf.append(entry.getValueToString());
            buf.append(": ");
            buf.append(entry.toString());
            buf.append('\n');
        }
        return buf.toString();
    }

    
    public void actionPerformed(ActionEvent evt) {
        Object[] message = new Object[2];
        message[0] = "Choose one of the following options:";
        JComboBox cb = new JComboBox();
        for (Entry entry: options.values()) {
            cb.addItem(entry);
        }
        message[1] = cb;
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
            Entry entry = (Entry) cb.getSelectedItem();
            setValue(entry.getValueToString());
        }
    }
}
