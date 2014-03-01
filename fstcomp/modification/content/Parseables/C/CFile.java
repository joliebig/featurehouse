package modification.content.Parseables.C;

import java.io.File;
import java.io.FileNotFoundException;

import modification.content.Parseables.ParseableFile;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_capprox.CApproxParser;

/**
 * 
 * @author boxleitner
 * 
 */
public class CFile extends ParseableFile {

    /**
     * 
     * @param file
     */
    public CFile(File file) {
	super(file);
	// TODO Auto-generated constructor stub
    }

    /*
     * (non-Javadoc)
     * 
     * @see modification.content.Content#getFST()
     */
    public FSTNode getFST() throws FileNotFoundException, ParseException {
	CApproxParser p = new CApproxParser(getCharStream());
	p.TranslationUnit(false);
	FSTNonTerminal cFile = new FSTNonTerminal("C-File", file.getName());
	cFile.addChild(p.getRoot());
	return cFile;
    }

}
