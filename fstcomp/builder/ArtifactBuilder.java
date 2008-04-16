package builder;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.StringTokenizer;
import de.ovgu.cide.fstgen.ast.FSTFeatureNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public abstract class ArtifactBuilder implements ArtifactBuilderInterface {

  private String baseDirectoryName;

  private LinkedList<FSTNonTerminal> featureNodes= new LinkedList<FSTNonTerminal>();
  
  /* (non-Javadoc)
 * @see builder.ArtifactBuilderInterface#acceptFile(java.io.File)
 */
public abstract boolean acceptFile(File inputFile);

  /* (non-Javadoc)
 * @see builder.ArtifactBuilderInterface#getFeatures()
 */
public LinkedList<FSTNonTerminal> getFeatures() {
	return featureNodes;
  }

  public abstract void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException;
  
  /* (non-Javadoc)
 * @see builder.ArtifactBuilderInterface#setBaseDirectoryName(java.lang.String)
 */
public void setBaseDirectoryName(String baseDirectoryName) {
    this.baseDirectoryName = baseDirectoryName;
  }

  /* (non-Javadoc)
 * @see builder.ArtifactBuilderInterface#getBaseDirectoryName()
 */
public String getBaseDirectoryName() {
    return this.baseDirectoryName;
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

  /* (non-Javadoc)
 * @see builder.ArtifactBuilderInterface#processFile(java.io.File)
 */
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
				  processNode(parent, st, inputFile);
			  }
		  }
	  }
  }
}
