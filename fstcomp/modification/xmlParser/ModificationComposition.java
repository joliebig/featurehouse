package modification.xmlParser;

import java.util.LinkedList;
import java.util.List;

public class ModificationComposition {

    private List<Modification> modList = new LinkedList<Modification>();

    public void add(Modification m) {
	modList.add(m);
    }

    /**
     * @return the modList
     */
    public List<Modification> getModList() {
        return modList;
    }
    
    
}
