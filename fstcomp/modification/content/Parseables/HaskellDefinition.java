package modification.content.Parseables;

import java.io.FileNotFoundException;

import tmp.generated_haskell.HaskellParser;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;

public class HaskellDefinition extends ParseableCodeSnippet {

    /**
     * 
     * @param string
     * @param type
     */
    public HaskellDefinition(String type, String content) {
	super(type, content);
	// TODO Auto-generated constructor stub
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
