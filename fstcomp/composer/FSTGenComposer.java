package composer;

import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;

import composer.rules.Overriding;

import builder.java.JavaBuilder;
import printer.FeaturePrintVisitor;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class FSTGenComposer {
	public static void main(String[] args) {
		
		CmdLineInterpreter cmd = new CmdLineInterpreter();
		cmd.parseCmdLineArguments(args);
		
		FileLoader fileLoader = new FileLoader();
		JavaBuilder javaBuilder = new JavaBuilder();
		
		fileLoader.registerArtifactBuilder(javaBuilder);
		try {
			fileLoader.loadFiles(cmd.equationFileName, cmd.equationBaseDirectoryName, cmd.isAheadEquationFile);
			LinkedList<FSTNonTerminal> features = javaBuilder.getFeatures();
			//for(FSTNonTerminal node : features)
			//	System.err.println(node.toString());
			FSTNode composition = compose(features);
			
			String outputDir = cmd.equationBaseDirectoryName;
			if (cmd.outputDirectoryName!=null)
				outputDir=cmd.outputDirectoryName;
			FeaturePrintVisitor featurePrintVisitor = new FeaturePrintVisitor(outputDir, cmd.equationFileName);
			featurePrintVisitor.visit((FSTNonTerminal)composition);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}
	
	private static FSTNode compose(List<FSTNonTerminal> tl) {
		FSTNode composed = null;
		for (FSTNode current : tl) {
			if (composed != null) {
				composed = compose(current, composed);
			} else
				composed = current;
		}
		return composed;
	}
	
	public static FSTNode compose(FSTNode nodeA, FSTNode nodeB) {
		return compose(nodeA, nodeB, null);
	}

	public static FSTNode compose(FSTNode nodeA, FSTNode nodeB, FSTNode compParent) {
		
		if(nodeA.compatibleWith(nodeB)) {
			FSTNode compNode = nodeA.getShallowClone();
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
						nonterminalComp.addChild(childB.getDeepClone());
					} else {
						nonterminalComp.addChild(compose(childA, childB, nonterminalComp));
					}
				}
				for(FSTNode childA : nonterminalA.getChildren()) {
					FSTNode childB = nonterminalB.getCompatibleChild(childA);
					if(childB == null) {
						// no compatible child, FST-node only in A
						nonterminalComp.addChild(childA.getDeepClone()); }
				}
				return nonterminalComp;
			} else if(nodeA instanceof FSTTerminal && nodeB instanceof FSTTerminal && compParent instanceof FSTNonTerminal) {
				FSTTerminal terminalA = (FSTTerminal)nodeA;
				FSTTerminal terminalB = (FSTTerminal)nodeB;
				FSTTerminal terminalComp = (FSTTerminal)compNode;
				FSTNonTerminal nonterminalParent = (FSTNonTerminal)compParent;
				
				if(terminalA.getBody().length() > 0 && terminalB.getBody().length() > 0 && !terminalA.getBody().equals(terminalB.getBody())) {
					
					if(terminalA.getCompositionMechanism().equals("replacement")) {
						System.out.println("Terminal replacement: " + terminalA.toString() + " replaces " + terminalB.toString());
					} else if(terminalA.getCompositionMechanism().equals("concatenation")) {
						
					} else if(terminalA.getCompositionMechanism().equals("overriding")) {
						System.out.println("Terminal overriding: " + terminalA.toString() + " overrides " + terminalB.toString());
						new Overriding().compose(terminalA, terminalB, terminalComp, nonterminalParent);
					} else if(terminalA.getCompositionMechanism().equals("warning")) {
						System.out.println("Warning: terminal replacement: " + terminalB.toString() + " replaces " + terminalA.toString());
					} else {
						System.err.println("Error: don't know how to compose terminals: " + terminalB.toString() + " replaces " + terminalA.toString());
					}
				}
				return terminalComp;
			}
			return null;
		}
		else
			return null;
	}
}