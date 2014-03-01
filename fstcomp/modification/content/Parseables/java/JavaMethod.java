/**
 * 
 */
package modification.content.Parseables.java;

import java.io.FileNotFoundException;

import modification.content.Parseables.ParseableCodeSnippet;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.parsers.generated_java15.Java15Parser;

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
    public JavaMethod(String content) {
	super(content);	
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
