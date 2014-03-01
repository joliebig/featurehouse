/**
 * 
 */
package modification.content.Parseables.java;

import java.io.File;
import java.io.FileNotFoundException;

import modification.content.Parseables.ParseableFile;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_java15.Java15Parser;

/**
 * @author Boxleitner Stefan
 * 
 */
public class JavaFile extends ParseableFile {

    /**
     * 
     * @param file
     */
    public JavaFile(File file) {
	super(file);
    }

    /*
     * (non-Javadoc)
     * 
     * @see modification.content.Content#getFST()
     */
    public FSTNode getFST() throws FileNotFoundException, ParseException {
	Java15Parser p = new Java15Parser(getCharStream());
	p.CompilationUnit(false);
	FSTNonTerminal javaFile = new FSTNonTerminal("Java-File", file
		.getName());
	javaFile.addChild(p.getRoot());
	return javaFile;
    }

}
