package printer.text;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;
import printer.PrintVisitorException;
import printer.PrintVisitorInterface;

public class TextPrintVisitor implements PrintVisitorInterface {

	private String suffix;
	
	private TextPrintVisitor() {}
	
	public TextPrintVisitor(String suffix) {
		this.suffix = suffix;
	}
	
	public boolean acceptNode(FSTNode node) {
		if (node.getType().equals(suffix + "-File")) 
			return true;
		else
			return false;
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
