package net.sf.jabref.imports; 

import java.net.URL; 

import javax.swing.JPanel; 

import net.sf.jabref.OutputPrinter; 
import net.sf.jabref.gui.ImportInspectionDialog; 


public  interface  EntryFetcher  extends ImportInspectionDialog.CallBack {
	

    
    public boolean processQuery(String query, ImportInspector inspector, OutputPrinter status);


	

    
    public String getTitle();


	

    
    public String getKeyName();


	

    
    public URL getIcon();


	

    
    public String getHelpPage();


	

    
    public JPanel getOptionsPanel();



}
