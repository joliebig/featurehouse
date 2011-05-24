package modification.content.Parseables.java;

import java.io.FileNotFoundException;

import modification.content.InvalidFSTTraversalException;
import modification.content.Parseables.ParseableCodeSnippet;
import tmp.generated_java15.Java15Parser;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;

public class JavaMethodBody extends ParseableCodeSnippet {

    public JavaMethodBody(String content) {
	super(content);	
    }

    @Override
    public FSTNode getFST() throws FileNotFoundException, ParseException,
	    modification.traversalLanguageParser.ParseException,
	    InvalidFSTTraversalException {
	Java15Parser p = new Java15Parser(getCharStream());
	p.MethodDeclarationBody(false);
	return p.getRoot();
    }

}
