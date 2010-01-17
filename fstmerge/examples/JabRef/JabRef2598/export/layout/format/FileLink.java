package net.sf.jabref.export.layout.format; 

import java.io.File; 

import net.sf.jabref.GUIGlobals; 
import net.sf.jabref.Globals; 
import net.sf.jabref.Util; 
import net.sf.jabref.export.layout.ParamLayoutFormatter; 
import net.sf.jabref.gui.FileListEntry; 
import net.sf.jabref.gui.FileListTableModel; 
import java.io.IOException; 


public  class  FileLink implements  ParamLayoutFormatter {
	

    String fileType = null;

	

    public String format(String field) {
        FileListTableModel tableModel = new FileListTableModel();
        if (field == null)
            return "";

        tableModel.setContent(field);
        String link = null;
        if (fileType == null) {
            
            if (tableModel.getRowCount() > 0)
                link = tableModel.getEntry(0).getLink();
        }
        else {
            
            for (int i=0; i< tableModel.getRowCount(); i++) {
                FileListEntry flEntry = tableModel.getEntry(i);
                if (flEntry.getType().getName().toLowerCase().equals(fileType)) {
                    link = flEntry.getLink();
                    break;
                }
            }
        }
        
        if (link == null)
            return "";


        String dir;
        
        
        
        
        if (Globals.prefs.fileDirForDatabase != null)
            dir = Globals.prefs.fileDirForDatabase;
        else
            dir = Globals.prefs.get(GUIGlobals.FILE_FIELD + "Directory");
        
		File f = Util.expandFilename(link, new String[] { dir });

        
		if (f != null) {
            try {
                return f.getCanonicalPath();
            } catch (IOException e) {
                e.printStackTrace();
                return f.getPath();
            }
        } else {
			return link;
		}


    }


	

    
    public void setArgument(String arg) {
        fileType = arg;
    }



}
