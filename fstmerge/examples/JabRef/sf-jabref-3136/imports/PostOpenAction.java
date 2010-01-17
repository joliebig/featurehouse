package net.sf.jabref.imports;

import net.sf.jabref.BasePanel;


public interface PostOpenAction {

    
    public boolean isActionNecessary(ParserResult pr);

    
    public void performAction(BasePanel panel, ParserResult pr);
}
