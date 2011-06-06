



package com.lowagie.toolbox.arguments;

import java.awt.event.ActionEvent;
import java.lang.reflect.Field;
import java.util.Iterator;
import java.util.TreeMap;

import javax.swing.JComboBox;
import javax.swing.JOptionPane;

import com.lowagie.text.PageSize;
import com.lowagie.toolbox.AbstractTool;


public class PageSizeArgument extends OptionArgument {

    private TreeMap<Object, Object> options = new TreeMap<Object, Object>();

    
    public PageSizeArgument(AbstractTool tool, String name, String description) {
        super(tool, name, description);
        Class<?> ps = PageSize.class;
        Field[] sizes = ps.getDeclaredFields();
        try {
            for (int i = 0; i < sizes.length; i++) {
                addOption(sizes[i].getName(), sizes[i].get(null));
            }
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
    }

    
    public void addOption(Object description, Object value) {
        options.put(description, value);
    }

    
    public TreeMap<Object, Object> getOptions() {
        return options;
    }

    
    public Object getArgument() throws InstantiationException {
        if (value == null) {
            return null;
        }
        try {
            return options.get(value);
        } catch (Exception e) {
            throw new InstantiationException(e.getMessage());
        }
    }

    
    public String getUsage() {
        StringBuffer buf = new StringBuffer("  ");
        buf.append(name);
        buf.append(" -  ");
        buf.append(description);
        buf.append('\n');
        buf.append("    possible options:\n");
        String s;
        for (Iterator<Object> i = options.keySet().iterator(); i.hasNext(); ) {
            s = (String) i.next();
            buf.append("    - ");
            buf.append(s);
            buf.append('\n');
        }
        return buf.toString();
    }

    
    public void actionPerformed(ActionEvent evt) {
        Object[] message = new Object[2];
        message[0] = "Choose one of the following pagesizes:";
        JComboBox cb = new JComboBox();
        for(Iterator<Object> i = options.keySet().iterator(); i.hasNext(); ) {
            cb.addItem(i.next());
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
            setValue(cb.getSelectedItem());
        }
    }
    
    public String toString() {
        return super.getValue().toString();
    }

}
