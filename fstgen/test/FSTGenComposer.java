import java.io.FileInputStream;
import java.util.List;

import tmp.generated_java15.Java15Parser;
import cide.gparser.OffsetCharStream;
import de.ovgu.cide.fstgen.ast.FSTNode;

public class FSTGenComposer {
	public static void main(String[] args) {
		
		try {
			Java15Parser p = new Java15Parser(new OffsetCharStream( new FileInputStream("examples/GraphJava/BasicGraph/Graph.java")));
			p.CompilationUnit(false);
			System.out.println(p.getRoot().printFST(0));
		} catch (Exception e1) {
			e1.printStackTrace();
		}

	}
	
	private FSTNode compose(List<FSTNode> tl) {
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
	
	public FSTNode compose(FSTNode treeA, FSTNode treeB) {
		return compose(treeA, treeB, null);
	}

	public FSTNode compose(FSTNode treeA, FSTNode treeB, FSTNode composedParent) {
		//FSTNode compVertex = treeA.root().compose(treeB.root(), composedParent);
		
		if(treeA.getName().equals(treeB.getName()) && treeA.getName().equals(treeB.getName()))
			FSTNode compVertex = treeA.getClass().newInstance();
		
		if(compVertex != null) {
			// roots have been composed
			FSTNode compTree = new FSTNode(compVertex);
			compTree.setParent(composedParent);
			// composed SubTree-stub is integrated in the new Tree, needs children
			for(FSTNode childB : treeB.children()) { 
				FSTNode childA = treeA.getCompatibleChild(childB);
				//for each child of B get the first compatible child of A (CompatibleChild means a Child which root equals B's root)
				if(childA == null) {
					// no compatible child, FST-node only in B 
					compTree.addChild(childB.getGroupIndex(), childB.getClone());
				} else {
					int ga = childA.getGroupIndex();
					int gb = childB.getGroupIndex();
					assert (ga == gb) : "Child GroupIndizes differ: " + ga + " " + gb;
					// childA and childB are compatible, they should be composable
					// this method might throw a NotComposableException, if e.g. the children have conflicting, compatible children
					try {
						compTree.addChild(ga, compose(childA, childB, compTree));
					} catch (NotComposableException e) {
						// implement a "Tree-Trace" so the user can see where the exception occured
						e.insertAtBeginningOfVertexTrace(childA.root.getType() + " : " + childA.root.getName() + "\n");
						throw e;
					}
				}
			}
			for(FSTNode childA : treeA.children()) {
				FSTNode childB = treeB.getCompatibleChild(childA);
				if(childB == null) {
					// no compatible child, FST-node only in A
					compTree.addChild(childA.getGroupIndex(), childA.getClone()); }
			}
			return compTree;
		}
		else
			return null;
	}
}