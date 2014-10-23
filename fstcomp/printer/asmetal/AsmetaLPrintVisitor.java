package printer.asmetal;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;

import printer.ArtifactPrintVisitor;
import printer.PrintVisitorException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_AsmetaL.SimplePrintVisitor;

public class AsmetaLPrintVisitor extends ArtifactPrintVisitor {

	public AsmetaLPrintVisitor() {
		super("AsmetaL-File");
	}

	public void processNode(FSTNode node, File folderPath)
			throws PrintVisitorException {
		if (node instanceof FSTNonTerminal) {
			FSTNonTerminal nonterminal = (FSTNonTerminal) node;
			for (FSTNode child : nonterminal.getChildren()) {
				String fileName = folderPath.getPath() + File.separator
						+ nonterminal.getName();

				SimplePrintVisitor visitor;
				try {
					visitor = new SimplePrintVisitor(new PrintStream(fileName));
					
					// in order to display the problems during composition, the
					// runtime exception needs to be caught
					try {
						visitor.visit((FSTNonTerminal) child);
					} catch (RuntimeException r) {
						r.printStackTrace();
					}
					visitor.getResult();
				} catch (FileNotFoundException e) {
					throw new PrintVisitorException(e.getMessage());
				}
			}
		} else {
			assert (!(node instanceof FSTNonTerminal));
		}
	}
}
