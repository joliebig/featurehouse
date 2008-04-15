import java.io.FileInputStream;
import java.io.PrintStream;
import java.util.LinkedList;
import java.util.List;

import tmp.generated_java15.Java15Parser;
import cide.gparser.OffsetCharStream;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class FSTGenComposer {
	public static void main(String[] args) {
		
		try {
			Java15Parser p = new Java15Parser(new OffsetCharStream( new FileInputStream("../FeatureAlgebra/examples/GraphJava/BasicGraph/Graph/Edge.java")));
			p.CompilationUnit(false);
			System.out.println(p.getRoot().printFST(0));
			
			Java15Parser q = new Java15Parser(new OffsetCharStream( new FileInputStream("../FeatureAlgebra/examples/GraphJava/Weight/Graph/Edge.java")));
			q.CompilationUnit(false);
			System.out.println(q.getRoot().printFST(0));
			
			LinkedList<FSTNode> features = new LinkedList<FSTNode>();
			features.add(p.getRoot());
			features.add(q.getRoot());
			
			FSTNode comp = compose(features); 
			System.out.println(comp.printFST(0));
			
			String result = new String();
			SimplePrintVisitor printer = new SimplePrintVisitor(new PrintStream("./ttt.java"));
			printer.visit((FSTNonTerminal)comp);
			System.out.println(result);
			
		} catch (Exception e1) {
			e1.printStackTrace();
		}

	}
	
	private static FSTNode compose(List<FSTNode> tl) {
		FSTNode composed = null;
		for (FSTNode current : tl) {
			//Tree.traverse(current);
			if (composed != null) {
				composed = compose(current, composed);
			} else
				composed = current;
		}
		return composed;
	}
	
	public static FSTNode compose(FSTNode treeA, FSTNode treeB) {
		return compose(treeA, treeB, null);
	}

	public static FSTNode compose(FSTNode nodeA, FSTNode nodeB, FSTNode compParent) {
		
		if(nodeA.compatibleWith(nodeB)) {
			FSTNode compNode = nodeA.clone();
			compNode.setParent(compParent);

			// composed SubTree-stub is integrated in the new Tree, needs children
			if(nodeA instanceof FSTNonTerminal && nodeB instanceof FSTNonTerminal) {
				FSTNonTerminal nonterminalA = (FSTNonTerminal)nodeA;
				FSTNonTerminal nonterminalB = (FSTNonTerminal)nodeB;
				FSTNonTerminal nonterminalComp = (FSTNonTerminal)compNode;
				
				for(FSTNode childB : nonterminalB.getChildren()) { 
					FSTNode childA = nonterminalA.getCompatibleChild(childB);
					//for each child of B get the first compatible child of A (CompatibleChild means a Child which root equals B's root)
					if(childA == null) {
						// no compatible child, FST-node only in B
						FSTNode newChild = childB.clone();
						nonterminalComp.addChild(newChild);
					} else {
						nonterminalComp.addChild(compose(childA, childB, nonterminalComp));
					}
				}
				for(FSTNode childA : nonterminalA.getChildren()) {
					FSTNode childB = nonterminalB.getCompatibleChild(childA);
					if(childB == null) {
						// no compatible child, FST-node only in A
						nonterminalComp.addChild(childA.clone()); }
				}
			}
			return compNode;
		}
		else
			return null;
	}
}