package modification;

import de.ovgu.cide.fstgen.ast.FSTNode;

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
     * @param content
     *                content given as a string
     * @param type
     *                type of the FST node to create
     */
    public ImplicitContent(String content, String type) {
	super();
	this.content = content;
	this.type = type;
    }

    /*
     * (non-Javadoc)
     * 
     * @see modification.xmlParser.Content#getContent()
     */
    @Override
    public FSTNode getContent() {
	// TODO Auto-generated method stub
	return null;
    }

}
