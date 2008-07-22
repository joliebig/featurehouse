package modification;

import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * @author Boxleitner Stefan
 * 
 */
public abstract class Modification {

    /**
     * content of the modification
     */
    private Content content;

    /**
     * traversal to select nodes in the FST to modify
     */
    private String fstTraversal;

    /**
     * @param fstTraversal
     *                traversal to select nodes in the FST to modify
     * @param content
     *                content of the modification
     */
    public Modification(String fstTraversal, Content content) {
	super();
	this.fstTraversal = fstTraversal;
	this.content = content;
    }

    /**
     * applies the modification to an existing FST
     * 
     * @param root
     *                root of the FST to modify
     */
    public abstract void apply(FSTNode root);

    /**
     * @return the content
     */
    public Content getContent() {
	return content;
    }

    /**
     * @return the fstTraversal
     */
    public String getFstTraversal() {
	return fstTraversal;
    }
}
