package modification;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

/**
 * content, given directly by 2 Strings: content and type
 * 
 * @author Boxleitner Stefan
 */
public class ImplicitContent implements Content {

    /**
     * content given as a string
     */
    private String content;

    /**
     * type of the FST node to create
     */
    private String type;

    /**
     * name of the FST node to create
     */
    private String name;

    /**
     * @param name
     *                name of the FST node to create
     * @param content
     *                content given as a string
     * @param type
     *                type of the FST node to create
     */
    public ImplicitContent(String content, String type, String name) {
	super();
	this.content = content;
	this.type = type;
	this.name = name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see modification.xmlParser.Content#getContent()
     */
    @Override
    public FSTNode getContent() {
	return new FSTTerminal(type, name, content, "");
    }

}
