package modification.xmlParser;

public class Modification {

    private String fstTraversal, fstNodeType, modType;
    
    private Content content;

    /**
     * @return the fstTraversal
     */
    public String getFstTraversal() {
	return fstTraversal;
    }

    /**
     * @param fstTraversal
     *                the fstTraversal to set
     */
    public void setFstTraversal(String fstTraversal) {
	this.fstTraversal = fstTraversal;
    }

    /**
     * @return the content
     */
    public Content getContent() {
	return content;
    }

    /**
     * @param content
     *                the content to set
     */
    public void setContent(Content content) {
	this.content = content;
    }

    /**
     * @return the fstNodeType
     */
    public String getFstNodeType() {
	return fstNodeType;
    }

    /**
     * @param fstNodeType
     *                the fstNodeType to set
     */
    public void setFstNodeType(String fstNodeType) {
	this.fstNodeType = fstNodeType;
    }

    /**
     * @return the modType
     */
    public String getModType() {
	return modType;
    }

    /**
     * @param modType
     *                the modType to set
     */
    public void setModType(String modType) {
	this.modType = modType;
    }
}
