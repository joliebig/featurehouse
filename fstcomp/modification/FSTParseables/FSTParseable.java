/**
 * 
 */
package modification.FSTParseables;

import java.io.FileNotFoundException;

import cide.gparser.ParseException;

import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * @author Boxleitner Stefan
 * 
 */
public abstract class FSTParseable {

    protected Input input;

    public FSTParseable(Input input) {
	this.input = input;
    }

    public abstract FSTNode parseToFST() throws FileNotFoundException,
	    ParseException;

}
