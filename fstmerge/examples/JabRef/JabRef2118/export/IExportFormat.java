package net.sf.jabref.export; 

import net.sf.jabref.BibtexDatabase; 

import java.io.Writer; 
import java.util.Set; 

import javax.swing.filechooser.FileFilter; 

public  interface  IExportFormat {
	

	
	String getConsoleName();

	

	
	String getDisplayName();

	

	
	FileFilter getFileFilter();

	

	
	void performExport(BibtexDatabase database, String file, String encoding,
		Set<String> entryIds) throws Exception;


}
