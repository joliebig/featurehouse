/**
 * 
 */
package modification.FSTParseables;

import java.io.FileNotFoundException;

import tmp.generated_java15.Java15Parser;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

/**
 * @author Boxleitner Stefan
 * 
 */
public class JavaFile extends FSTParseable {

    public JavaFile(Input input) {
	super(input);
	// TODO Auto-generated constructor stub
    }

    @Override
    public FSTNode getFST() throws FileNotFoundException, ParseException {
	Java15Parser p = new Java15Parser(super.input.getCharStream());
	p.CompilationUnit(false);
	FSTNonTerminal javaFile = new FSTNonTerminal("Java-File", p.getRoot()
		.getName()
		+ ".java");
	javaFile.addChild(p.getRoot());
	return javaFile;
    }

}
