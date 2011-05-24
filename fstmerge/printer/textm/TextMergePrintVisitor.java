package printer.textm;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import printer.ArtifactPrintVisitor;
import printer.PrintVisitorException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class TextMergePrintVisitor extends ArtifactPrintVisitor {

	public TextMergePrintVisitor(String suffix) {
		super(suffix + ".merge-File");
	}

	public void processNode(FSTNode node, File folderPath) throws PrintVisitorException {
		if(node instanceof FSTNonTerminal) {
			FSTNonTerminal nonterminal = (FSTNonTerminal)node;
			assert(nonterminal.getChildren().isEmpty());
			assert(!(nonterminal.getChildren().get(0) instanceof FSTTerminal));
			
			String content = ((FSTTerminal)nonterminal.getChildren().get(0)).getBody();
			File textFile = new File(folderPath, nonterminal.getName());
			try {
				textFile.createNewFile();
				BufferedWriter textFileWriter = new BufferedWriter(new FileWriter(textFile));
				textFileWriter.write(content + "\n");
				textFileWriter.flush();
				textFileWriter.close();
			} catch (IOException e) {
				throw new PrintVisitorException(e.getMessage());
			} 
		} else {
			assert(!(node instanceof FSTNonTerminal));
		}
	}
}
