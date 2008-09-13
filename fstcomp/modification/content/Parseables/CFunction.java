/**
 * 
 */
package modification.content.Parseables;

import java.io.FileNotFoundException;

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
    public CFunction(String type, String content) {
	super(type, content);
	// TODO Auto-generated constructor stub
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
