/**
 * 
 */
package modification.content.Parseables;

import java.io.FileNotFoundException;

import tmp.generated_java15.Java15Parser;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * @author Boxleitner Stefan
 * 
 */
public class JavaMethod extends ParseableCodeSnippet {

    /**
     * 
     * @param string
     * @param type
     */
    public JavaMethod(String type, String content) {
	super(type, content);
	// TODO Auto-generated constructor stub
    }

    /*
     * (non-Javadoc)
     * 
     * @see modification.content.Content#getFST()
     */
    public FSTNode getFST() throws FileNotFoundException, ParseException {
	Java15Parser p = new Java15Parser(getCharStream());
	p.ClassOrInterfaceBodyDeclaration(false);
	return p.getRoot();
    }
}
