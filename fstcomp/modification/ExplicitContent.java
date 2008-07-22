package modification;

import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * content, given by an address to a content directory in the filesystem and an
 * FST traversal to select the desired artifact, i.e. an FST node
 * 
 * @author Boxleitner Stefan
 * 
 */
public class ExplicitContent implements Content {

    /**
     * path to the base directory/file
     */
    private String rootPath;

    /**
     * FST traversal to select the desired artifact, i.e. an FST node
     */
    private String fstTraversal;

    /**
     * @param rootPath
     *                path to the base directory/file
     * @param fstTraversal
     *                FST traversal to select the desired artifact, i.e. an
     *                FST node
     */
    public ExplicitContent(String rootPath, String fstTraversal) {
	super();
	this.rootPath = rootPath;
	this.fstTraversal = fstTraversal;
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
