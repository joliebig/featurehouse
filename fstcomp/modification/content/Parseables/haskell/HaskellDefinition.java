package modification.content.Parseables.haskell;

import java.io.FileNotFoundException;

import modification.content.Parseables.ParseableCodeSnippet;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.parsers.generated_haskell.HaskellParser;

public class HaskellDefinition extends ParseableCodeSnippet {

    /**
     * 
     * @param string
     * @param type
     */
    public HaskellDefinition(String content) {
	super(content);	
    }

    /*
     * (non-Javadoc)
     * 
     * @see modification.content.Content#getFST()
     */
    public FSTNode getFST() throws FileNotFoundException, ParseException {
	HaskellParser p = new HaskellParser(getCharStream());
	p.definition(false);
	return p.getRoot();
    }

}
