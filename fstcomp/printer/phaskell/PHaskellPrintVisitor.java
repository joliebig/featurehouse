package printer.phaskell;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;

import printer.ArtifactPrintVisitor;
import printer.PrintVisitorException;
import tmp.generated_phaskell.SimplePrintVisitor;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class PHaskellPrintVisitor extends ArtifactPrintVisitor {

	public PHaskellPrintVisitor() {
		super("PHaskell-File");
	}
	public void processNode(FSTNode node, File folderPath) throws PrintVisitorException {
		if(node instanceof FSTNonTerminal) {
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
		}
	}
}
