package printer;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;

import tmp.generated_java15.SimplePrintVisitor;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;


public class FeaturePrintVisitor implements PrintVisitorInterface {
	private String workingDir;
	private String expressionName;
	private File featurePath;
	private File folderPath;
	private File oldFolderPath;
	
	public FeaturePrintVisitor(String workingDir, String expressionName) {
		this.workingDir = workingDir;
		this.expressionName = expressionName;
	}
	
	public void visit(FSTNonTerminal nonterminal) throws PrintVisitorException {
		if(nonterminal == null) {
			System.err.println("Nonterminal with null value encountered");
		} else if (nonterminal.getType().equals("JavaFile")) {
			for(FSTNode child : nonterminal.getChildren()) {
				String fileName = folderPath.getPath() + File.separator + nonterminal.getName();
				
				SimplePrintVisitor javaVisitor;
				try {
					javaVisitor = new SimplePrintVisitor(new PrintStream(fileName));
					javaVisitor.visit((FSTNonTerminal)child);
					javaVisitor.getResult();
				} catch (FileNotFoundException e) {
					throw new PrintVisitorException(e.getMessage());
				}
			}
			
		} else if(nonterminal.getType().equals("Feature")) {
			StringBuffer sb = new StringBuffer(expressionName);
			sb.setLength(sb.lastIndexOf("."));
			sb.delete(0, sb.lastIndexOf(File.separator) + 1);
			featurePath = new File(workingDir + sb.toString());
			featurePath.mkdir();
			folderPath = featurePath;
			for(FSTNode child : nonterminal.getChildren())
				visit((FSTNonTerminal)child);
			
		} else if(nonterminal.getType().equals("Folder")) {
			oldFolderPath = folderPath;
			folderPath = new File(folderPath, nonterminal.getName());
			folderPath.mkdir();
			for(FSTNode child : nonterminal.getChildren())
				visit((FSTNonTerminal)child);
			folderPath = oldFolderPath;
		} else {
			System.err.println("Nonterminal type not supported: " + nonterminal.getType());
		}
	}
}
