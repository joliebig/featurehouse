package merger;

import java.util.LinkedList;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class MergeVisitor {
	
	private LinkedList<MergerInterface> mergerList = new LinkedList<MergerInterface>();
	
	public void registerMerger(MergerInterface merger) {
		mergerList.add(merger);
	}
	
	private LinkedList<MergerInterface> getMergerList() {
		return mergerList;
	}
	
	public void visit(FSTNode current) {
		if(current instanceof FSTNonTerminal) {
			for(FSTNode child : ((FSTNonTerminal)current).getChildren())
				visit(child);
		} else if(current instanceof FSTTerminal) {
			for(MergerInterface merger : getMergerList()) {
				try {
					
					if(((FSTTerminal)current).getBody().contains(FSTGenMerger.MERGE_SEPARATOR)) {
						merger.merge((FSTTerminal)current);
					}
						
				} catch (ContentMergeException e) {
					System.err.println(e.toString());
				} 
			}
				
		} else {
			System.err.println("MergerVisitor: node is neither non-terminal nor terminal!");			
		}
	}
}
