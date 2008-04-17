package composer;

import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;

import composer.rules.ConstructorConcatenation;
import composer.rules.ImplementsListMerging;
import composer.rules.ModifierListSpecification;
import composer.rules.StringConcatenation;
import composer.rules.MethodOverriding;

import builder.java.JavaBuilder;
import printer.FeaturePrintVisitor;
import printer.PrintVisitorException;
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
			FSTNode composition = compose(features);
			
			String outputDir = cmd.equationBaseDirectoryName;
			if (cmd.outputDirectoryName!=null)
				outputDir=cmd.outputDirectoryName;
			FeaturePrintVisitor featurePrintVisitor = new FeaturePrintVisitor(outputDir, cmd.equationFileName);
			featurePrintVisitor.visit((FSTNonTerminal)composition);
		} catch (PrintVisitorException e) {
			e.printStackTrace();
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

/*			System.err.println("nodeA: " + nodeA.getName());
			System.err.println("nodeB: " + nodeB.getName());
			System.err.println(".............................");
			
			if(nodeA.getName().equals("Foo"))
				{System.err.println(nodeA.toString());System.err.println(nodeB.toString());}
*/
			
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
				
				if(!terminalA.getBody().trim().equals(terminalB.getBody().trim())) {
					
					if(terminalA.getCompositionMechanism().equals("Replacement")) {
						System.out.println("Terminal replacement: " + terminalA.toString() + " replaces " + terminalB.toString());
					} else if(terminalA.getCompositionMechanism().equals("StringConcatenation")) {
						System.out.println("Terminal concatenation: " + terminalA.toString() + " is concatenated to " + terminalB.toString());
						StringConcatenation.compose(terminalA, terminalB, terminalComp, nonterminalParent);
					} else if(terminalA.getCompositionMechanism().equals("ImplementsListMerging")) {
						System.out.println("Implements list merging: " + terminalA.toString() + " extends " + terminalB.toString());
						ImplementsListMerging.compose(terminalA, terminalB, terminalComp, nonterminalParent);
					} else if(terminalA.getCompositionMechanism().equals("MethodOverriding")) {
						System.out.println("Method overriding: " + terminalA.toString() + " overrides " + terminalB.toString());
						MethodOverriding.compose(terminalA, terminalB, terminalComp, nonterminalParent);
					} else if(terminalA.getCompositionMechanism().equals("ConstructorConcatenation")) {
						System.out.println("Constructor concatenation: " + terminalA.toString() + " extends " + terminalB.toString());
						ConstructorConcatenation.compose(terminalA, terminalB, terminalComp, nonterminalParent);
					} else if(terminalA.getCompositionMechanism().equals("ModifierListSpecialization")) {
						System.out.println("Modifier list specification: " + terminalA.toString() + " specializes " + terminalB.toString());
						ModifierListSpecification.compose(terminalA, terminalB, terminalComp, nonterminalParent);
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