package net.sf.jabref.external;

import javax.swing.Icon;

import net.sf.jabref.BasePanel;
import net.sf.jabref.BibtexEntry;


public interface PushToApplication {

    public String getName();

    public String getApplicationName();

    public String getTooltip();

    public Icon getIcon();

    public String getKeyStrokeName();
    
    public void pushEntries(BibtexEntry[] entries, String keyString);

    
    public void operationCompleted(BasePanel panel);

    
    public boolean requiresBibtexKeys();



}
