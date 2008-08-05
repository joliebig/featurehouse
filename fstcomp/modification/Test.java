package modification;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;

import builder.ArtifactBuilderInterface;
import builder.java.JavaBuilder;

public class Test {

    /**
     * @param args
     * @throws FileNotFoundException
     */
    public static void main(String[] args) throws FileNotFoundException {
	List<ArtifactBuilderInterface> builderList = new LinkedList<ArtifactBuilderInterface>();
	builderList.add(new JavaBuilder());
	FSTGenerator gen = new FSTGenerator(new File("modification/test"),
		builderList);
	gen.getFST();

    }
}
