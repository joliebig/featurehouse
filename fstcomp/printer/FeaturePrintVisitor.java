package printer;
import java.io.File;
import java.util.LinkedList;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;


public class FeaturePrintVisitor {
	private String workingDir = ".";
	private String expressionName = "default.expression";
	//private File featurePath;
	//private File folderPath;
	//private File oldFolderPath;
	private LinkedList<PrintVisitorInterface> visitors = new LinkedList<PrintVisitorInterface>();
	
	public FeaturePrintVisitor() {	}
	
	public FeaturePrintVisitor(String workingDir, String expressionName) {
		this.setWorkingDir(workingDir);
		this.setExpressionName(expressionName);
	}
	
	public void setWorkingDir(String workingDir) {
		this.workingDir = workingDir;
	}

	public String getWorkingDir() {
		return workingDir;
	}

	public void setExpressionName(String expressionName) {
		this.expressionName = expressionName;
	}

	public String getExpressionName() {
		return expressionName;
	}

	public void registerPrintVisitor(PrintVisitorInterface visitor) {
		this.visitors.add(visitor);
	}

	public void unregisterPrintVisitor(PrintVisitorInterface visitor) {
		this.visitors.remove(visitor);
	}

	public LinkedList<PrintVisitorInterface> getPrintVisitors() {
		return visitors;
	}
	
	public void visit(FSTNonTerminal root) throws PrintVisitorException {
		visit(root, null, null, null);
	}
	
	private void visit(FSTNonTerminal nonterminal, File featurePath, File folderPath, File oldFolderPath) throws PrintVisitorException {
		if(nonterminal != null) {
			if(nonterminal.getType().equals("Feature")) {
				StringBuffer sb = new StringBuffer(getExpressionName());
				sb.setLength(sb.lastIndexOf("."));
				sb.delete(0, sb.lastIndexOf(File.separator) + 1);
				featurePath = new File(getWorkingDir() + File.separator + sb.toString());
				featurePath.mkdir();
				folderPath = featurePath;
				for(FSTNode child : nonterminal.getChildren()) {
					visit((FSTNonTerminal)child, featurePath, folderPath, oldFolderPath);
				}
			} else if(nonterminal.getType().equals("Folder")) {
				oldFolderPath = folderPath;
				folderPath = new File(folderPath, nonterminal.getName());
				folderPath.mkdir();
				for(FSTNode child : nonterminal.getChildren()) {
					visit((FSTNonTerminal)child, featurePath, folderPath, oldFolderPath);
				}
				folderPath = oldFolderPath;
			} else {
				boolean processed = false;
				for(PrintVisitorInterface visitor : getPrintVisitors()) {

					if(visitor.acceptNode(nonterminal)) {
						visitor.processNode(nonterminal, folderPath);
						processed = true;
					}
				}
				if(!processed)
					System.err.println("Nonterminal type not supported: " + nonterminal.getType());
			}
		}
	}
}
