package modification;

import java.io.FileNotFoundException;

import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;

public class ParsedFSTContent implements Content {

    /**
     * @param content
     * @param type
     */
    public ParsedFSTContent(String content, String type) {
	super();
	this.content = content;
	this.type = type;
    }

    private String content;

    private String type;

    @Override
    public FSTNode getFST() throws FileNotFoundException, ParseException {
	return StringFSTGenerator.createFST(content, type);
    }

}