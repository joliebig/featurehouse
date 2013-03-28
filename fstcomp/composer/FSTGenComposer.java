package composer;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import metadata.CompositionMetadataStore;
import printer.PrintVisitorException;
import builder.ArtifactBuilderInterface;
import builder.capprox.CApproxBuilder;
import builder.java.JavaBuilder;

import composer.rules.CSharpMethodOverriding;
import composer.rules.CompositionError;
import composer.rules.CompositionRule;
import composer.rules.ConstructorConcatenation;
import composer.rules.ContractComposition;
import composer.rules.ExpansionOverriding;
import composer.rules.FieldOverriding;
import composer.rules.ImplementsListMerging;
import composer.rules.JavaMethodOverriding;
import composer.rules.ModifierListSpecialization;
import composer.rules.Replacement;
import composer.rules.StringConcatenation;
import composer.rules.rtcomp.c.CRuntimeFeatureSelection;
import composer.rules.rtcomp.c.CRuntimeFunctionRefinement;
import composer.rules.rtcomp.c.CRuntimeReplacement;
import composer.rules.rtcomp.c.CRuntimeSubtreeIntegration;
import composer.rules.rtcomp.java.JavaRuntimeFeatureSelection;
import composer.rules.rtcomp.java.JavaRuntimeFunctionRefinement;
import composer.rules.rtcomp.java.JavaRuntimeReplacement;
import composer.rules.rtcomp.java.JavaRuntimeSubtreeIntegration;

import counter.Counter;
import de.ovgu.cide.fstgen.ast.AbstractFSTParser;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class FSTGenComposer extends FSTGenProcessor {

	protected CmdLineInterpreter cmd = new CmdLineInterpreter();
	
	protected CompositionMetadataStore meta = CompositionMetadataStore.getInstance();
	protected List<CompositionRule> compositionRules;
	protected CRuntimeSubtreeIntegration subtreeRewriterC = null;
	protected JavaRuntimeSubtreeIntegration subtreeRewriterJava = null;
	
	public FSTGenComposer() {
		super();
	}

	@SuppressWarnings("unchecked")
	public FSTGenComposer(boolean rememberFSTNodes) {
		super();
		if (!rememberFSTNodes) {
			setFstnodes((ArrayList<FSTNode>)AbstractFSTParser.fstnodes.clone());
			AbstractFSTParser.fstnodes.clear();
		}
	}
	
	private FSTNode rewriteSubtree(FSTNode n) {
		meta.discoverFuncIntroductions(n);	
		if (cmd.lifting) {
			if (cmd.lifting_language.equals("c")) { 
				return subtreeRewriterC.rewrite(n.getDeepClone());
			} else if (cmd.lifting_language.equals("java")) {
				return subtreeRewriterJava.rewrite(n.getDeepClone());
			} else {
				throw new InternalError("lifting language \"" + cmd.lifting_language + "\" is not implemented.");
			}
		} else {
			return n.getDeepClone();
		}
	}
	

	public void run(String[] args) {
		meta.clearFeatures();
		cmd.parseCmdLineArguments(args);
		JavaMethodOverriding.setFeatureAnnotation(cmd.featureAnnotation);
		JavaRuntimeFunctionRefinement.setFeatureAnnotation(cmd.featureAnnotation);
		
		//select the composition rules
		compositionRules = new ArrayList<CompositionRule>();
		if (cmd.lifting) {
			if (cmd.lifting_language.equals("c")) { 
				compositionRules.add(new CRuntimeReplacement());
				compositionRules.add(new CRuntimeFunctionRefinement());			
				subtreeRewriterC = new CRuntimeSubtreeIntegration();
			} else if (cmd.lifting_language.equals("java")) {
				compositionRules.add(new JavaRuntimeReplacement());
				compositionRules.add(new JavaRuntimeFunctionRefinement());
				subtreeRewriterJava = new JavaRuntimeSubtreeIntegration();
			} else {
				throw new InternalError("lifting language \"" + cmd.lifting_language + "\" is not implemented.");
			}
		} else {
			compositionRules.add(new Replacement());
			compositionRules.add(new JavaMethodOverriding());
			compositionRules.add(new ContractComposition(cmd.contract_style));
		}
		compositionRules.add(new StringConcatenation());
		compositionRules.add(new ImplementsListMerging());
		compositionRules.add(new CSharpMethodOverriding());
		compositionRules.add(new ConstructorConcatenation());
		compositionRules.add(new ModifierListSpecialization());
		compositionRules.add(new FieldOverriding());
		compositionRules.add(new ExpansionOverriding());
		compositionRules.add(new CompositionError());
		
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

			if (outputDir.endsWith(File.separator))
				outputDir = outputDir.substring(0, outputDir.length()-1);
				
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
				for (FSTNonTerminal feature : features) {
					meta.addFeature(feature.getName());
				}
				FSTNode composition = compose(features);
//				modify(composition);

//				if(composition != null)
//					System.err.println(composition.toString());
				
				
				/* 
				 * hook for general purpose visitors
				 */
				 /*	if (null != composition)
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
		
			String equationName = new File(cmd.equationFileName).getName();
			equationName = equationName.substring(0, equationName.length() - 4);
		
			if (cmd.featureAnnotation) {
				File srcDir = new File(outputDir + File.separator + equationName+ File.separator);
				saveFeatureAnnotationFile(srcDir);
			}
			try {
				//System.out.println(outputDir + "features/roles.meta");
				
				//was passiert hier?!
				// outputDir: /home/rhein/FeatureHouseWS/Test_Features/features
				// equationFileName: Selection.features
				// -> exp = Selection.feat
				
				//meta.saveToFile(outputDir + "/" + exp + "/roles.meta");
				meta.saveToFile(outputDir + File.separator + "roles.meta");
				if (cmd.lifting) {
					File cnfFile = new File(cmd.equationBaseDirectoryName, "model.cnf");
					System.err.println("cnfFile:" + cnfFile.getAbsolutePath());
					//hier auch geändert.
					// wollte in /home/rhein/FeatureHouseWS/Test_Features/featuresfeatures/featureselect.h speichern
					//new RuntimeFeatureSelection(meta, cnfFile).saveTo(outputDir + "features/featureselect");
					if (cmd.lifting_language.equals("c")) {
						new CRuntimeFeatureSelection(meta, cnfFile).saveTo(outputDir + File.separator + "features/featureselect");
					} else if (cmd.lifting_language.equals("java")) {
						new JavaRuntimeFeatureSelection(meta, cnfFile).saveTo(outputDir + File.separator);
					}
				}
			} catch (IOException e) {			
				e.printStackTrace();
			}
		} catch (FileNotFoundException e1) {
			//e1.printStackTrace();
		}
	}
	
	private void saveFeatureAnnotationFile(File srcDir) {
		File f = new File(srcDir+File.separator+"featureHouse"+File.separator, "FeatureAnnotation.java");
		f.getParentFile().mkdirs();
		System.out.println("writing FeatureAnnotation to file " +  f.getAbsolutePath());
		try (FileWriter fw = new FileWriter(f)) {
			String contents =
				"package featureHouse;\n"+
				"import java.lang.annotation.ElementType;\n" +
				"import java.lang.annotation.Retention;\n" +
				"import java.lang.annotation.RetentionPolicy;\n" +
				"import java.lang.annotation.Target;\n" +
	
				"@Retention(RetentionPolicy.RUNTIME)\n" +
				"@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})\n" +
				"public @interface FeatureAnnotation {\n" +
				"	String name();\n" +
				"}";
			fw.write(contents);
		} catch (IOException e) {
			System.err.println("Could not write FeatureAnnotation.java " + e.getMessage());
		}
	}

	public static void main(String[] args) {
		FSTGenComposer composer = new FSTGenComposer();
		composer.run(args);
	}

	private FSTNode compose(List<FSTNonTerminal> tl) {
		FSTNode composed = null;
		for (FSTNode current : tl) {
			if (composed != null) {
				composed = compose(current, composed);
			} else {
				if (cmd.featureAnnotation) {
					addAnnotationToChildrenMethods(current, JavaMethodOverriding.getFeatureName(current));
				}
				composed = current;
			}
		}
		return composed;
	}
	private void addAnnotationToChildrenMethods(FSTNode current,
			String featureName) {
		if (current instanceof FSTNonTerminal) {
			for (FSTNode child : ((FSTNonTerminal)current).getChildren())
				addAnnotationToChildrenMethods(child, featureName);
		} else if (current instanceof FSTTerminal) {
			if ("MethodDecl".equals(current.getType()) || 
					"ConstructorDecl".equals(current.getType())) {
				String body = ((FSTTerminal)current).getBody();
				((FSTTerminal)current).setBody("@featureHouse.FeatureAnnotation(name=\""+ featureName +"\")\n" + body);
			}
		} else {
			throw new RuntimeException("Somebody has introduced a subclass of FSTNode \"" + 
				current.getClass().getName() 
				+ "\" that is not considered by the annotation option.");
		}
	}

	public FSTNode compose(FSTNode nodeA, FSTNode nodeB) {
		return compose(nodeA, nodeB, null);
	}

	public FSTNode compose(FSTNode nodeA, FSTNode nodeB,
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
						//nonterminalComp.addChild(childB.getDeepClone());
						nonterminalComp.addChild(rewriteSubtree(childB));
					} else {
						nonterminalComp.addChild(compose(childA, childB,
								nonterminalComp));
					}
				}
				for (FSTNode childA : nonterminalA.getChildren()) {
					FSTNode childB = nonterminalB.getCompatibleChild(childA);
					if (childB == null) {
						// no compatible child, FST-node only in A
						//nonterminalComp.addChild(childA.getDeepClone());
						FSTNode newChildA = rewriteSubtree(childA);
						if (cmd.featureAnnotation) {
							if (newChildA instanceof FSTNonTerminal) {
								addAnnotationToChildrenMethods(newChildA, JavaMethodOverriding.getFeatureName(childA));
							} else if (newChildA instanceof FSTTerminal) {
								if ("MethodDecl".equals(newChildA.getType()) ||
										"ConstructorDecl".equals(newChildA.getType())) {
									FSTTerminal termNewChildA = (FSTTerminal) newChildA;
									String body = termNewChildA.getBody();
									String feature = JavaMethodOverriding.getFeatureName(childA);
									termNewChildA.setBody("@featureHouse.FeatureAnnotation(name=\""+ feature +"\")\n" + body);
								}
							}
						}
						nonterminalComp.addChild(newChildA);
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
/*
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
					(new JavaMethodOverriding()).compose(terminalA, terminalB,
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
*/
				CompositionRule applicableRule = null;
				//get applicable rule from compositionRules
				for (CompositionRule rule: compositionRules) {
					if (terminalA.getCompositionMechanism().equals(rule.getRuleName())) {						
						 applicableRule = rule;
						 break;
					}
				}
				if (applicableRule != null) {
					//apply composition rule
					applicableRule.compose(terminalA, terminalB, terminalComp, nonterminalParent);
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