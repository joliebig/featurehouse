

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.LinkedList;
import java.util.StringTokenizer;
import java.util.Vector;

import tmp.generated_java15.Java15Parser;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;

import de.ovgu.cide.fstgen.ast.FSTFeatureNode;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class JavaBuilder extends ArtifactBuilder {
	private String suffix = ".java";
	private LinkedList<FSTNonTerminal> featureNodes= new LinkedList<FSTNonTerminal>();
	@Override
	public boolean acceptFile(File inputFile) {
		if (inputFile.isFile()) {
		      if (inputFile.getName().endsWith(suffix)) {
		        return true;
		      }
		    }
		    return false;
	}

	public LinkedList<FSTNonTerminal> getFeatures() {
		return featureNodes;
	}
	@Override
	public void processFile(File inputFile) throws FileNotFoundException {
		System.out.println("processing: " + inputFile.getPath());
		FSTNonTerminal parent = null;
		StringTokenizer st = new StringTokenizer(inputFile.getPath(), File.separator);
		
		// Directory containing the Features

		int beginBaseDir = this.getBaseDirectoryName().lastIndexOf(File.separator) < 0 ? 0 : this.getBaseDirectoryName().lastIndexOf(File.separator) + 1;
		int endBaseDir = this.getBaseDirectoryName().length();
		String baseDir = this.getBaseDirectoryName().substring(beginBaseDir, endBaseDir);

 		String fileDir = st.nextToken();
		
 		// forward through st until the current token is baseDir
 		while(st.hasMoreTokens()) {
 			if(baseDir.equals(fileDir))
 				break;
 			fileDir = st.nextToken();
 		}

		if(baseDir.equals(fileDir)) {
			String featureName = st.nextToken(); // the name of the feature must be the next token
			// get the correct Feature tree
			parent = getFeatureNodeByName(featureName);

			while(st.hasMoreTokens()) {
				if(st.countTokens() > 1) {
					// still folders between current position and file
					FSTNonTerminal newNode = new FSTNonTerminal("Folder", st.nextToken());
					FSTNonTerminal oldNode = (FSTNonTerminal)parent.getCompatibleChild(newNode);
					if(parent != null &&  oldNode == null) {
						parent.addChild(newNode);
						parent = newNode;
					} else {
						parent = oldNode;
					}
						
				} else {
					FSTNonTerminal rootDocument = new FSTNonTerminal("JavaFile", st.nextToken());
					parent.addChild(rootDocument);
					parent = rootDocument;
					Java15Parser p = new Java15Parser(new OffsetCharStream( new FileInputStream(inputFile)));
					try {
						p.CompilationUnit(false);
						rootDocument.addChild(p.getRoot());
						System.err.println(p.getRoot().getName());
					} catch (ParseException e) {
						e.printStackTrace();
					}
				}
			}
		}
	}
	/**
	 * Return the Feature Tree corresponding to the given Name. Constructs a new Feature if none matches.
	 * @param name the name of a feature
	 * @return matching Feature Tree
	 */
	private FSTNonTerminal getFeatureNodeByName(String name) {
		for(FSTNonTerminal featureNode : featureNodes) {
			if(featureNode.getName().equals(name))
				return featureNode;
		}
		FSTNonTerminal newFeatureNode = new FSTFeatureNode(name);
		featureNodes.add(newFeatureNode);
		return newFeatureNode;
	}

}
