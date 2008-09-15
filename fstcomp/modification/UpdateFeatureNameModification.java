/**
 * 
 */
package modification;

import java.io.FileNotFoundException;

import modification.content.InvalidFSTTraversalException;
import modification.traversalLanguageParser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * @author Boxleitner Stefan
 * 
 */
public class UpdateFeatureNameModification implements Modification {

    private String newName;

    public UpdateFeatureNameModification(String newName) {
	this.newName = newName;
    }

    @Override
    public void apply(FSTNode root) throws ParseException,
	    FileNotFoundException, cide.gparser.ParseException,
	    InvalidFSTTraversalException {
	root.setName(newName);
    }

}
