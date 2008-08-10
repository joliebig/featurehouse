/**
 * 
 */
package modification.FSTParseables;

import java.io.FileNotFoundException;

import tmp.generated_java15.Java15Parser;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * @author Boxleitner Stefan
 * 
 */
public class JavaMethod extends FSTParseable {

    public JavaMethod(Input input) {
	super(input);
    }

    @Override
    public FSTNode parseToFST() throws FileNotFoundException, ParseException {
	Java15Parser p = new Java15Parser(super.input.getCharStream());
	p.ClassOrInterfaceBodyDeclaration(false);
	return p.getRoot();
    }
}
