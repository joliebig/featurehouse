package net.sf.jabref.external;

import javax.swing.*;

import net.sf.jabref.GUIGlobals;


public class ExternalFileType implements Comparable<ExternalFileType> {

    protected String name, extension, openWith, iconName;
    protected ImageIcon icon;
    protected JLabel label = new JLabel();

    public ExternalFileType(String name, String extension, String openWith,
                            String iconName) {
        label.setText(null);
        this.name = name;
        label.setToolTipText(this.name);
        this.extension = extension;
        this.openWith = openWith;
        setIconName(iconName);
    }

    
    public ExternalFileType(String[] val) {
        if ((val == null) || (val.length < 4))
            throw new IllegalArgumentException("Cannot contruct ExternalFileType without four elements in String[] argument.");
        this.name = val[0];
        label.setToolTipText(this.name);
        this.extension = val[1];
        this.openWith = val[2];
        label.setText(null);
        setIconName(val[3]);
    }

    
    public String[] getStringArrayRepresentation() {
        return new String[] {name, extension, openWith, iconName};
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
        label.setToolTipText(this.name);
    }

    public String getExtension() {
        return extension;
    }

    public void setExtension(String extension) {
        this.extension = extension;
    }

    
    public String getFieldName() {
        return extension;
    }

    public String getOpenWith() {
        return openWith;
    }

    public void setOpenWith(String openWith) {
        this.openWith = openWith;
    }

    
    public void setIconName(String name) {
        this.iconName = name;
        try {
            this.icon = GUIGlobals.getImage(iconName);
        } catch (NullPointerException ex) {
            
            
            
            
            this.icon = null;
        }
        label.setIcon(this.icon);
    }

    
    public JLabel getIconLabel() {
        return label;
    }

    
    public String getIconName() {
        return iconName;
    }

    public ImageIcon getIcon() {
        return icon;
    }

    public void setIcon(ImageIcon icon) {
        this.icon = icon;
    }

    public String toString() {
        return getName();
    }

    public int compareTo(ExternalFileType o) {
        return getName().compareTo(o.getName());
    }

    public ExternalFileType copy() {
        return new ExternalFileType(name, extension, openWith, iconName);
    }


    public int hashCode() {
        return name.hashCode();
    }

    
    public boolean equals(Object object) {
        ExternalFileType other = (ExternalFileType)object;
        if (other == null)
            return false;
        return (name == null ? other.name == null : name.equals(other.name))
                && (extension == null ? other.extension == null : extension.equals(other.extension))
                && (openWith== null ? other.openWith == null : openWith.equals(other.openWith))
                && (iconName== null ? other.iconName == null : iconName.equals(other.iconName));
    }
}
