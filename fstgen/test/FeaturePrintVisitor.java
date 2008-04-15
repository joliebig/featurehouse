import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;


public class FeaturePrintVisitor {

	public void visit(FSTNonTerminal nonterminal) {
		if (nonterminal.getType().equals("JavaFile")) {
			for(FSTNode child : nonterminal.getChildren()) {
				SimplePrintVisitor javaVisitor = new SimplePrintVisitor();
				javaVisitor.visit((FSTNonTerminal)child);
				System.out.println(javaVisitor.getResult());
			}
			
		} else if(nonterminal.getType().equals("Feature") || nonterminal.getType().equals("Folder")) {
			System.out.println(nonterminal.getName());
			for(FSTNode child : nonterminal.getChildren())
				visit((FSTNonTerminal)child);
		} else {
			System.err.println("Nonterminal type not supported: " + nonterminal.getType());
		}
	}
}
