package net.sf.jabref.external; 

import net.sf.jabref.GUIGlobals; 

import javax.swing.*; 

import javax.swing.ImageIcon; 


public  class  ExternalFileType implements  Comparable ,  Comparable<ExternalFileType> {
	

    protected String name, extension, openWith, iconName;

	
    protected ImageIcon icon;

	

    public ExternalFileType(String name, String extension, String openWith,
                            String iconName) {
        this.name = name;
        this.extension = extension;
        this.openWith = openWith;
        this.iconName = iconName;
        this.icon = GUIGlobals.getImage(iconName);
    }


	

    public ExternalFileType(String[] val) {
        if ((val == null) || (val.length < 4))
            throw new IllegalArgumentException("Cannot contruct ExternalFileType without four elements in String[] argument.");
        this.name = val[0];
        this.extension = val[1];
        this.openWith = val[2];
        this.iconName = val[3];
        this.icon = GUIGlobals.getImage(val[3]);
    }


	

    
    public String[] getStringArrayRepresentation() {
        return new String[] {name, extension, openWith, iconName};
    }


	

    public String getName() {
        return name;
    }


	

    public void setName(String name) {
        this.name = name;
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
        this.icon = GUIGlobals.getImage(iconName);
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


	

    


	

    public ExternalFileType copy() {
        return new ExternalFileType(name, extension, openWith, iconName);
    }

	

    public int compareTo(ExternalFileType o) {
        return getName().compareTo(o.getName());
    }


}
