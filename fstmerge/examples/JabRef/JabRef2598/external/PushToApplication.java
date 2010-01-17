package net.sf.jabref.external; 

import net.sf.jabref.BasePanel; 
import net.sf.jabref.BibtexDatabase; 
import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.MetaData; 

import javax.swing.*; 


public  interface  PushToApplication {
	

    public String getName();


	

    public String getApplicationName();


	

    public String getTooltip();


	

    public Icon getIcon();


	

    public String getKeyStrokeName();


	


    
    public JPanel getSettingsPanel();


	

    
    public void storeSettings();


	

    
    public void pushEntries(BibtexDatabase database, BibtexEntry[] entries,
                            String keyString, MetaData metaData);


	

    
    public void operationCompleted(BasePanel panel);


	

    
    public boolean requiresBibtexKeys();



}
