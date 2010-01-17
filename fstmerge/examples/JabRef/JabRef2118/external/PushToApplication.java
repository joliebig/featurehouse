package net.sf.jabref.external; 
import net.sf.jabref.BibtexEntry; 

import net.sf.jabref.BasePanel; 

import javax.swing.*; 

import javax.swing.Icon; 


public  interface  PushToApplication {
	

    public String getName();


	

    public String getApplicationName();


	

    public String getTooltip();


	

    public Icon getIcon();


	

    public String getKeyStrokeName();


	
    
    public void pushEntries(BibtexEntry[] entries, String keyString);


	

    
    public void operationCompleted(BasePanel panel);


	

    
    public boolean requiresBibtexKeys();



}
