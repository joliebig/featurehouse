/**
 * 
 */
package modification;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNode;

import builder.ArtifactBuilderInterface;
import builder.java.JavaBuilder;

/**
 * @author Boxleitner Stefan
 * 
 */
public class Test {

    /**
     * @param args
     * @throws FileNotFoundException
     */
    public static void main(String[] args) throws FileNotFoundException {
	ArtifactBuilderInterface jb = new JavaBuilder();
	File dir = new File("examples/Modification");
	jb.setBaseDirectoryName(dir.toString());
	for (File f : getFiles(dir))
	    if (jb.acceptFile(f)) {
		jb.processFile(f);
	    }
	System.out.println(getFiles(dir).size());
	System.out.println(jb.getFeatures().size());
	for (FSTNode n:jb.getFeatures()){
	    System.out.println(n);
	}

    }

    private static List<File> getFiles(File dir) {
	List<File> files = new LinkedList<File>();
	if (!dir.isDirectory())
	    files.add(dir);
	else
	    for (File f : dir.listFiles())
		files.addAll(getFiles(f));
	return files;
    }

}
