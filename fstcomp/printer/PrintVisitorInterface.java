package printer;

import java.io.File;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public interface PrintVisitorInterface {
	public abstract boolean acceptNode(FSTNode node);
	public abstract void processNode(FSTNode node, File folderPath) throws PrintVisitorException;
}
