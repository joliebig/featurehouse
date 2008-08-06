package modification;

import java.io.File;

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
     * reference to an FSTgen
     */
    private FSTGenerator fstGen;

    /**
     * path to the base directory/file
     */
    private File rootPath;

    /**
     * FST traversal to select the desired artifact, i.e. an FST node
     */
    private String fstTraversal;

    /**
     * @param fstGen
     *                reference to an FSTgen
     * @param rootPath
     *                path to the base directory/file
     * @param fstTraversal
     *                FST traversal to select the desired artifact, i.e. an FST
     *                node
     */
    public ExplicitContent(File rootPath, String fstTraversal,
	    FSTGenerator fstGen) {
	super();
	this.rootPath = rootPath;
	this.fstTraversal = fstTraversal;
	this.fstGen = fstGen;
    }

    /*
     * (non-Javadoc)
     * 
     * @see modification.xmlParser.Content#getContent()
     */
    @Override
    public FSTNode getContent() {
	fstGen.setRootPath(rootPath);	
	return null;
    }

}
