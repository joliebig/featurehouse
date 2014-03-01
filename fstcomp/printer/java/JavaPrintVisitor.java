package printer.java;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;

import printer.ArtifactPrintVisitor;
import printer.PrintVisitorException;

import de.ovgu.cide.fstgen.ast.AbstractFSTPrintVisitor;
import de.ovgu.cide.fstgen.ast.CommandLineParameterHelper;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class JavaPrintVisitor extends ArtifactPrintVisitor {

	public JavaPrintVisitor() {
		super("Java-File");
	}
	public void processNode(FSTNode node, File folderPath) throws PrintVisitorException {
		if(node instanceof FSTNonTerminal) {
			FSTNonTerminal nonterminal = (FSTNonTerminal)node;
			for(FSTNode child : nonterminal.getChildren()) {
				String fileName = folderPath.getPath() + File.separator + nonterminal.getName();

				AbstractFSTPrintVisitor visitor;
				try {
					if(CommandLineParameterHelper.isJML()){
						visitor = new de.ovgu.cide.fstgen.parsers.generated_jml_contract_composition.SimplePrintVisitor(new PrintStream(fileName));
					}
					else{
						visitor = new de.ovgu.cide.fstgen.parsers.generated_java15.SimplePrintVisitor(new PrintStream(fileName));
					}
					
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
