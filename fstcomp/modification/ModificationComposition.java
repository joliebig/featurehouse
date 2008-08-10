package modification;

import java.io.FileNotFoundException;
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
     * 
     * @throws ParseException
     * @throws cide.gparser.ParseException
     * @throws InvalidFSTTraversalException
     * @throws FileNotFoundException
     */
    public void apply(FSTNode root) throws ParseException,
	    FileNotFoundException, cide.gparser.ParseException,
	    InvalidFSTTraversalException {
	for (Modification mod : modList) {
	    mod.apply(root);
	}
    }

}
