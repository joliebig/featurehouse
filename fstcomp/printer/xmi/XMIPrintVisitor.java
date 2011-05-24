package printer.xmi;

import java.io.File;

import printer.ArtifactPrintVisitor;
import printer.PrintVisitorException;
import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMIPrintVisitor extends ArtifactPrintVisitor {

	public XMIPrintVisitor() {
		super("XMI-File");
	}
	public void processNode(FSTNode node, File folderPath) throws PrintVisitorException {
		//System.out.println(node.toString());
		String fileName = folderPath.getPath() + File.separator + node.getName();
		XMIPrinter printer = new XMIPrinter(node, fileName);
		//TODO
		printer.transformDocument();
		
		/*if(node instanceof FSTNonTerminal) {
			FSTNonTerminal nonterminal = (FSTNonTerminal)node;
			for(FSTNode child : nonterminal.getChildren()) {
				String fileName = folderPath.getPath() + File.separator + nonterminal.getName();

				SimplePrintVisitor visitor;
				try {
					visitor = new SimplePrintVisitor(new PrintStream(fileName));
					visitor.visit((FSTNonTerminal)child);
					visitor.getResult();
				} catch (FileNotFoundException e) {
					throw new PrintVisitorException(e.getMessage());
				}
			}
		} else {
			assert(!(node instanceof FSTNonTerminal));
		}*/
	}

}
