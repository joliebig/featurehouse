/**
 * 
 */
package modification.content.Parseables.CSharp;

import java.io.File;
import java.io.FileNotFoundException;

import modification.content.InvalidFSTTraversalException;
import modification.content.Parseables.ParseableFile;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_csharp.CSharpParser;

/**
 * @author Boxleitner Stefan
 * 
 */
public class CSharpFile extends ParseableFile {

    public CSharpFile(File file) {
	super(file);	
    }

    /*
     * (non-Javadoc)
     * 
     * @see modification.content.Content#getFST()
     */
    @Override
    public FSTNode getFST() throws FileNotFoundException, ParseException,
	    modification.traversalLanguageParser.ParseException,
	    InvalidFSTTraversalException {	
	CSharpParser p = new CSharpParser(getCharStream());
	p.compilation_unit(false);
	FSTNonTerminal javaFile = new FSTNonTerminal("CSharp-File", file
		.getName());
	javaFile.addChild(p.getRoot());
	return javaFile;
    }

}
