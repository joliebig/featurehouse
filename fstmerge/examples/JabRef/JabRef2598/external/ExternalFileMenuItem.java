package net.sf.jabref.external; 

import java.awt.event.ActionEvent; 
import java.awt.event.ActionListener; 
import java.io.File; 
import java.io.IOException; 

import javax.swing.*; 

import net.sf.jabref.*; 


public  class  ExternalFileMenuItem  extends JMenuItem implements  ActionListener {
	

    private BibtexEntry entry;

	
    final String link;

	
    final MetaData metaData;

	
    ExternalFileType fileType;

	
    final JabRefFrame frame;

	
    private String fieldName = null;

	

    public ExternalFileMenuItem(JabRefFrame frame, BibtexEntry entry, String name,
                                String link, Icon icon,
                                MetaData metaData,
                                ExternalFileType fileType) {
        super(name, icon);
        this.frame = frame;
        this.entry = entry;
        this.link = link;
        this.metaData = metaData;
        this.fileType = fileType;
        addActionListener(this);
    }


	

    public ExternalFileMenuItem(JabRefFrame frame, BibtexEntry entry, String name,
                                String link, Icon icon, MetaData metaData, String fieldName) {
        this(frame, entry, name, link, icon, metaData, (ExternalFileType)null);
        this.fieldName = fieldName;
    }


	

    public void actionPerformed(ActionEvent e) {
        boolean success = openLink();
        if (!success) {
            frame.output(Globals.lang("Unable to open link."));
        }
    }


	

    public boolean openLink() {
        frame.output(Globals.lang("External viewer called") + ".");
        try {
            ExternalFileType type = fileType;
            if (this.fileType == null) {
                if (this.fieldName != null) {
                    Util.openExternalViewer(frame.basePanel().metaData(), link, fieldName);
                    return true;
                }
                else {
                    
                    File file = new File(link);
                    
                    String name = file.getName();
                    int pos = name.indexOf('.');
                    String extension = ((pos >= 0) && (pos < name.length() - 1)) ? name.substring(pos + 1)
                        .trim().toLowerCase() : null;
                    
                    type = Globals.prefs.getExternalFileTypeByExt(extension);
                    fileType = type;
                }
            }

            if (type instanceof UnknownExternalFileType)
                return Util.openExternalFileUnknown(frame, entry, metaData, link,
                        (UnknownExternalFileType)type);
            else
                return Util.openExternalFileAnyFormat(metaData, link, type);



        } catch (IOException e1) {
            
            
            
            
            if ((fileType != null) && (fileType.getOpenWith() != null)
                && (fileType.getOpenWith().length() > 0) &&
                    (e1.getMessage().indexOf(fileType.getOpenWith()) >= 0)) {

                JOptionPane.showMessageDialog(frame, Globals.lang("Unable to open link. "
                    +"The application '%0' associated with the file type '%1' could not be called.",
                        fileType.getOpenWith(), fileType.getName()),
                        Globals.lang("Could not open link"), JOptionPane.ERROR_MESSAGE);
                return false;
            }

            e1.printStackTrace();
        }
        return false;
    }



}
