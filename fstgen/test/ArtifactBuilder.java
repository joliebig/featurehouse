import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;
import java.util.Vector;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public abstract class ArtifactBuilder {

  private String baseDirectoryName;

  public abstract boolean acceptFile(File inputFile);

  public abstract void processFile(File inputFile) throws FileNotFoundException;
  
  public abstract List<FSTNonTerminal> getFeatures();

  public void setBaseDirectoryName(String baseDirectoryName) {
    this.baseDirectoryName = baseDirectoryName;
  }

  public String getBaseDirectoryName() {
    return this.baseDirectoryName;
  }

}
