package modification.content.Parseables.haskell;

import java.io.File;
import java.io.FileNotFoundException;

import modification.content.Parseables.ParseableFile;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_haskell.HaskellParser;

/**
 * 
 * @author boxleitner
 * 
 */
public class HaskellFile extends ParseableFile {

    /**
     * 
     * @param f
     */
    public HaskellFile(File f) {
	super(f);
	// TODO Auto-generated constructor stub
    }

    /*
     * (non-Javadoc)
     * 
     * @see modification.content.Content#getFST()
     */
    public FSTNode getFST() throws FileNotFoundException, ParseException {
	HaskellParser p = new HaskellParser(getCharStream());
	p.module(false);
	FSTNonTerminal haskellFile = new FSTNonTerminal("Haskell-File", file
		.getName());
	haskellFile.addChild(p.getRoot());
	return haskellFile;
    }

}
