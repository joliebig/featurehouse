package printer.xml;

import java.io.File;

import printer.ArtifactPrintVisitor;
import printer.PrintVisitorException;
import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMLPrintVisitor extends ArtifactPrintVisitor {

	public XMLPrintVisitor() {
		super("XML-File");
	}
	
	public void processNode(FSTNode node, File folderPath) throws PrintVisitorException {
		String fileName = folderPath.getPath() + File.separator + node.getName();
		XMLPrinter printer = new XMLPrinter(node, fileName);
		printer.transformDocument();
		
	}

}
