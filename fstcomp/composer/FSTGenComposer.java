package composer;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;

import modification.content.InvalidFSTTraversalException;
import modification.traversalLanguageParser.ParseException;
import printer.PrintVisitorException;
import builder.ArtifactBuilderInterface;
import builder.capprox.CApproxBuilder;
import builder.java.JavaBuilder;

import composer.rules.CSharpMethodOverriding;
import composer.rules.CompositionError;
import composer.rules.ConstructorConcatenation;
import composer.rules.ExpansionOverriding;
import composer.rules.FieldOverriding;
import composer.rules.ImplementsListMerging;
import composer.rules.JavaMethodOverriding;
import composer.rules.ModifierListSpecialization;
import composer.rules.Replacement;
import composer.rules.StringConcatenation;
import counter.Counter;

import de.ovgu.cide.fstgen.ast.AbstractFSTParser;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;
import de.ovgu.cide.fstgen.ast.FSTVisitor;

public class FSTGenComposer extends FSTGenProcessor {

	private CmdLineInterpreter cmd = new CmdLineInterpreter();

	public FSTGenComposer() {
		super();
	}

	public void run(String[] args) {
		cmd.parseCmdLineArguments(args);
		try {
			try {
				fileLoader.loadFiles(cmd.equationFileName, cmd.equationBaseDirectoryName, cmd.isAheadEquationFile);
			} catch (cide.gparser.ParseException e1) {
				System.out.println("error");
				fireParseErrorOccured(e1);
				e1.printStackTrace();
			}
			String outputDir = cmd.equationBaseDirectoryName;
			if (cmd.outputDirectoryName != null)
				outputDir = cmd.outputDirectoryName;

			featureVisitor.setWorkingDir(outputDir);
			featureVisitor.setExpressionName(cmd.equationFileName);
			
			for (ArtifactBuilderInterface builder : getArtifactBuilders()) {
				LinkedList<FSTNonTerminal> features = builder.getFeatures();

				if(cmd.isCount && (builder instanceof JavaBuilder || builder instanceof CApproxBuilder)) {
					Counter counter = new Counter();
					for (FSTNonTerminal feature : features) {
						counter.collect(feature);
					}
					if(features.size() > 0)
						counter.writeFile(new File(cmd.equationFileName + ".rsf"));
				}
				
				/*for (FSTNonTerminal feature : features) {
					System.out.println(feature.toString());
				}*/
				
				FSTNode composition = compose(features);
//				modify(composition);

//				if(composition != null)
//					System.err.println(composition.toString());
				
				
				/* 
				 * hook for general purpose visitors
				 */
				/*if (null != composition)
        				for (FSTVisitor visitor: getFSTVisitors()) {
        				    composition.accept(visitor);
        				}
				*/
				try {
					featureVisitor.visit((FSTNonTerminal) composition);
				} catch (PrintVisitorException e) {
					e.printStackTrace();
				}
			}
			setFstnodes(AbstractFSTParser.fstnodes);
		} catch (FileNotFoundException e1) {
			//e1.printStackTrace();
		}
	}

	private void modify(FSTNode composition) {
		if (composition != null) {
			try {
				fileLoader.getModifications().apply(composition);
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ParseException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (cide.gparser.ParseException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvalidFSTTraversalException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			System.out.println(composition);
		}
	}

	public static void main(String[] args) {
		FSTGenComposer composer = new FSTGenComposer();
		composer.run(args);
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

	public static FSTNode compose(FSTNode nodeA, FSTNode nodeB,
			FSTNode compParent) {

		if (nodeA.compatibleWith(nodeB)) {
			FSTNode compNode = nodeA.getShallowClone();
			compNode.setParent(compParent);

			// composed SubTree-stub is integrated in the new Tree, needs
			// children
			if (nodeA instanceof FSTNonTerminal
					&& nodeB instanceof FSTNonTerminal) {
				FSTNonTerminal nonterminalA = (FSTNonTerminal) nodeA;
				FSTNonTerminal nonterminalB = (FSTNonTerminal) nodeB;
				FSTNonTerminal nonterminalComp = (FSTNonTerminal) compNode;

				for (FSTNode childB : nonterminalB.getChildren()) {
					FSTNode childA = nonterminalA.getCompatibleChild(childB);
					// for each child of B get the first compatible child of A
					// (CompatibleChild means a Child which root equals B's
					// root)
					if (childA == null) {
						// no compatible child, FST-node only in B
						nonterminalComp.addChild(childB.getDeepClone());
					} else {
						nonterminalComp.addChild(compose(childA, childB,
								nonterminalComp));
					}
				}
				for (FSTNode childA : nonterminalA.getChildren()) {
					FSTNode childB = nonterminalB.getCompatibleChild(childA);
					if (childB == null) {
						// no compatible child, FST-node only in A
						nonterminalComp.addChild(childA.getDeepClone());
					}
				}
				return nonterminalComp;
			} else if (nodeA instanceof FSTTerminal
					&& nodeB instanceof FSTTerminal
					&& compParent instanceof FSTNonTerminal) {
				FSTTerminal terminalA = (FSTTerminal) nodeA;
				FSTTerminal terminalB = (FSTTerminal) nodeB;
				FSTTerminal terminalComp = (FSTTerminal) compNode;
				FSTNonTerminal nonterminalParent = (FSTNonTerminal) compParent;

				if (terminalA.getCompositionMechanism().equals(
						Replacement.COMPOSITION_RULE_NAME)) {
					// System.out.println("Terminal replacement: " +
					// terminalA.toString() + " replaces " +
					// terminalB.toString());
				} else if (terminalA.getCompositionMechanism().equals(
						StringConcatenation.COMPOSITION_RULE_NAME)) {
					// System.out.println("Terminal concatenation: " +
					// terminalA.toString() + " is concatenated to " +
					// terminalB.toString());
					StringConcatenation.compose(terminalA, terminalB,
							terminalComp, nonterminalParent);
				} else if (terminalA.getCompositionMechanism().equals(
						ImplementsListMerging.COMPOSITION_RULE_NAME)) {
					// System.out.println("Implements list merging: " +
					// terminalA.toString() + " extends " +
					// terminalB.toString());
					ImplementsListMerging.compose(terminalA, terminalB,
							terminalComp, nonterminalParent);
				} else if (terminalA.getCompositionMechanism().equals(
						JavaMethodOverriding.COMPOSITION_RULE_NAME)) {
					// System.out.println("Java method overriding: " +
					// terminalA.toString() + " overrides " +
					// terminalB.toString());
					JavaMethodOverriding.compose(terminalA, terminalB,
							terminalComp, nonterminalParent);
				} else if (terminalA.getCompositionMechanism().equals(
						CSharpMethodOverriding.COMPOSITION_RULE_NAME)) {
					// System.out.println("C# method overriding: " +
					// terminalA.toString() + " overrides " +
					// terminalB.toString());
					CSharpMethodOverriding.compose(terminalA, terminalB,
							terminalComp, nonterminalParent);
				} else if (terminalA.getCompositionMechanism().equals(
						ConstructorConcatenation.COMPOSITION_RULE_NAME)) {
					// System.out.println("Constructor concatenation: " +
					// terminalA.toString() + " extends " +
					// terminalB.toString());
					ConstructorConcatenation.compose(terminalA, terminalB,
							terminalComp, nonterminalParent);
				} else if (terminalA.getCompositionMechanism().equals(
						ModifierListSpecialization.COMPOSITION_RULE_NAME)) {
					 // System.out.println("Modifier list specialization: " +
					 // terminalA.toString() + " specializes " +
					 // terminalB.toString());
					ModifierListSpecialization.compose(terminalA, terminalB,
							terminalComp, nonterminalParent);
				} else if (terminalA.getCompositionMechanism().equals(
						FieldOverriding.COMPOSITION_RULE_NAME)) {
					// System.out.println("Field overiding: " +
					// terminalA.toString() + " overrides " +
					// terminalB.toString());
					FieldOverriding.compose(terminalA, terminalB, terminalComp,
							nonterminalParent);
				} else if (terminalA.getCompositionMechanism().equals(
						ExpansionOverriding.COMPOSITION_RULE_NAME)) {
					// System.out.println("Expansion overiding: " +
					// terminalA.toString() + " overrides " +
					// terminalB.toString());
					ExpansionOverriding.compose(terminalA, terminalB,
							terminalComp, nonterminalParent);
				} else if (terminalA.getCompositionMechanism().equals(
						CompositionError.COMPOSITION_RULE_NAME)) {
					CompositionError.compose(terminalA, terminalB,
							terminalComp, nonterminalParent);
				} else {
					System.err
							.println("Error: don't know how to compose terminals: "
									+ terminalB.toString()
									+ " replaces "
									+ terminalA.toString());
				}
				return terminalComp;
			}
			return null;
		} else
			return null;
	}
}