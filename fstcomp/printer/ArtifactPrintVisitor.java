package printer;

import java.io.File;

import de.ovgu.cide.fstgen.ast.FSTNode;

public abstract class ArtifactPrintVisitor implements PrintVisitorInterface {
	
	private String suffix;
	
	public ArtifactPrintVisitor(String suffix) {
		this.suffix = suffix;
	}
	
	protected String getSuffix() {
		return suffix;
	}

	public boolean acceptNode(FSTNode node) {
		if (node.getType().equals(getSuffix())) 
			return true;
		else
			return false;
	}
	
	public abstract void processNode(FSTNode node, File folderPath) throws PrintVisitorException;

}