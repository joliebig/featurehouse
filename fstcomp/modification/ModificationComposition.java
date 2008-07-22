package modification;

import java.util.LinkedList;
import java.util.List;

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
	    mod.apply(root);
	}
    }

}
