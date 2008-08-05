package modification;

import java.util.LinkedList;
import java.util.List;

import modification.traversalLanguageParser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;

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

    /**
     * apply every single modification
     */
    public void apply(FSTNode root) {
	for (Modification mod : modList) {
	    try {
			mod.apply(root);
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
    }

}
