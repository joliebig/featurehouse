/**
 * 
 */
package modification.content.Parseables.C;

import java.io.FileNotFoundException;

import modification.content.Parseables.ParseableCodeSnippet;
import tmp.generated_capprox.CApproxParser;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * @author boxleitner
 * 
 */
public class CFunction extends ParseableCodeSnippet {

    /**
     * 
     * @param string
     * @param type
     */
    public CFunction(String content) {
	super(content);
    }

    /*
     * (non-Javadoc)
     * 
     * @see modification.content.Content#getFST()
     */
    public FSTNode getFST() throws FileNotFoundException, ParseException {
	CApproxParser p = new CApproxParser(getCharStream());
	p.CodeUnit_TopLevel(false);
	return p.getRoot();
    }

}
