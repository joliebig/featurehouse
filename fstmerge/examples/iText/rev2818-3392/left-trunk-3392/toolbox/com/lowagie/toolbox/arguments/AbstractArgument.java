package com.lowagie.toolbox.arguments;

import java.beans.PropertyChangeSupport;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import com.lowagie.toolbox.AbstractTool;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


public abstract class AbstractArgument implements ActionListener, PropertyChangeListener{
    public AbstractArgument() {
    }
    public AbstractArgument(AbstractTool tool, String name, String description, Object value) {
            this.tool = tool;
            this.name = name;
            this.description = description;
            this.value=value;
    }
    protected PropertyChangeSupport pcs = new PropertyChangeSupport(this);
    
    protected Object value = null;
    
    protected String name;
    
    protected AbstractTool tool;
    
    protected String description;
    protected synchronized void firePropertyChange(PropertyChangeEvent evt) {
        pcs.firePropertyChange(evt);
    }

    public synchronized void removePropertyChangeListener(
            PropertyChangeListener l) {
        pcs.removePropertyChangeListener(l);
    }

    public synchronized void addPropertyChangeListener(PropertyChangeListener l) {
        pcs.addPropertyChangeListener(l);
    }

    
    public Object getValue() {
        return value;
    }



    public void setValue(Object value, String propertyname) {
        Object oldvalue = this.value;
        this.value = value;
        tool.valueHasChanged(this);
        this.firePropertyChange(new PropertyChangeEvent(this, propertyname,
                oldvalue, this.value));
    }



    
    public void setDescription(String description) {
        this.description = description;
    }

    
    public String getDescription() {
        return description;
    }

    
    public void setName(String name) {
        this.name = name;
    }

    
    public String getUsage() {
        StringBuffer buf = new StringBuffer("  ");
        buf.append(name);
        buf.append(" -  ");
        buf.append(description);
        buf.append('\n');
        return buf.toString();
    }

    public AbstractTool getTool() {
        return tool;
    }

    public void setTool(AbstractTool tool) {
        this.tool = tool;
    }

    
    public Object getArgument() throws InstantiationException {
        if (value == null) {
            return null;
        }
        return value;
    }

    
    public String getName() {
        return name;
    }

    
    public void setValue(Object value) {
        Object oldvalue = this.value;
        this.value = value;
        tool.valueHasChanged(this);
        this.firePropertyChange(new PropertyChangeEvent(this, name, oldvalue,
                this.value));
    }

    public void propertyChange(PropertyChangeEvent evt) {
        System.out.println("AbstractArgument PropertyChange");
    }

    public abstract void actionPerformed(ActionEvent e);
    
    public String toString() {
        return getValue().toString();
    }

}
