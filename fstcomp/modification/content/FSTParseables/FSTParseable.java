/**
 * 
 */
package modification.content.FSTParseables;

import java.io.FileNotFoundException;

import modification.content.Content;

import cide.gparser.ParseException;

import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * @author Boxleitner Stefan
 * 
 */
public abstract class FSTParseable implements Content {

    protected Input input;

    public FSTParseable(Input input) {
	this.input = input;
    }

    public abstract FSTNode getFST() throws FileNotFoundException,
	    ParseException;

}
