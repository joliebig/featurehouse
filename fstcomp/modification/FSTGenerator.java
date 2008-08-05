/**
 * 
 */
package modification;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;

import builder.ArtifactBuilderInterface;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

/**
 * generates an FST out of a given root path and a list of ArtifactBuilders
 * 
 * @author Boxleitner Stefan
 */
public class FSTGenerator {

    /**
     * root path is the top-level node in the file system, excluded in the
     * created FST
     */
    private File rootPath;

    /**
     * list of ArtifactBuilders
     */
    private List<ArtifactBuilderInterface> builderList;

    /**
     * 
     * @param rootPath
     *                root path is the top-level node in the file system where
     *                FST creation starts, excluded in the created FST
     * @param builderList
     *                list of ArtifactBuilders
     */
    public FSTGenerator(File rootPath,
	    List<ArtifactBuilderInterface> builderList) {
	this.rootPath = rootPath;
	this.builderList = builderList;
    }

    /**
     * 
     * @param builderList
     *                list of ArtifactBuilders
     */
    public FSTGenerator(List<ArtifactBuilderInterface> builderList) {
	this.builderList = builderList;
    }

    /**
     * creates and returns an FST
     * 
     * @return root of the created FST
     * @throws FileNotFoundException
     */
    public FSTNode getFST() throws FileNotFoundException {
	for (ArtifactBuilderInterface builder : builderList) {
	    builder.setBaseDirectoryName(rootPath.getAbsolutePath());
	}
	for (File f : getContainingFiles(rootPath))
	    for (ArtifactBuilderInterface builder : builderList)
		if (builder.acceptFile(f))
		    builder.processFile(f);

	List<FSTNode> treeList = new LinkedList<FSTNode>();

	for (ArtifactBuilderInterface builder : builderList) {
	    FSTNonTerminal root = new FSTNonTerminal("root", "root");
	    treeList.add(root);
	    for (FSTNode node : builder.getFeatures()) {
		System.out.println(node);
		root.addChild(node);
	    }
	}

	return mergeFSTs(treeList);
    }

    private FSTNode mergeFSTs(List<FSTNode> nodeList) {
	FSTNode merged = null;
	for (FSTNode node : nodeList) {
	    if (merged == null)
		merged = node.getDeepClone();
	    else if (node.compatibleWith(merged)) {

	    }

	}

	return null;
    }

    /*
     * returns all files in a given directory (recursive)
     */
    private List<File> getContainingFiles(File folder) {
	List<File> fileList = new LinkedList<File>();
	for (File f : folder.listFiles()) {
	    fileList.add(f);
	    if (f.isDirectory())
		fileList.addAll(getContainingFiles(f));
	}
	return fileList;
    }

    /**
     * @return the rootPath
     */
    public File getRootPath() {
	return rootPath;
    }

    /**
     * @param rootPath
     *                the rootPath to set
     */
    public void setRootPath(File rootPath) {
	this.rootPath = rootPath;
    }

}
