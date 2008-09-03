package modification.content;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

/**
 * content, given directly by at least name, type and content
 * 
 * @author Boxleitner Stefan
 */
public class CustomFSTContent implements Content {

    /**
     * @param compositionMechanism
     * @param content
     * @param name
     * @param prefix
     * @param type
     */
    public CustomFSTContent(String compositionMechanism,
	    String content, String name, String prefix, String type) {
	
	super();	
	this.compositionMechanism = compositionMechanism;
	this.content = content;
	this.name = name;
	this.prefix = prefix;
	this.type = type;
    }

    /**
     * @param name
     * @param content
     * @param type
     * 
     */
    public CustomFSTContent(String content, String type, String name) {
	super();
	this.content = content;
	this.type = type;
	this.name = name;
    }

    /**
     * compositionMechanism of the FST node to create
     */
    private String compositionMechanism;

    /**
     * prefix of the FST node to create
     */
    private String prefix;

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

    /*
     * (non-Javadoc)
     * 
     * @see modification.xmlParser.Content#getContent()
     */
    @Override
    public FSTNode getFST() {
	return new FSTTerminal(type, name, content, prefix,
		compositionMechanism);
    }

}
