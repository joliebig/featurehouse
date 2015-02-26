package composer;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import metadata.CompositionMetadataStore;
import printer.PrintVisitorException;
import builder.ArtifactBuilderInterface;
import builder.capprox.CApproxBuilder;
import builder.java.JavaBuilder;
import builder.xml.XMLHook;
import builder.xml.XMLNode;

import composer.rules.AsmetaLFunctionOverriding;
import composer.rules.AsmetaLInitializationConcatenation;
import composer.rules.AsmetaLInvariantConjunction;
import composer.rules.AsmetaLRuleOverriding;
import composer.rules.CSharpMethodOverriding;
import composer.rules.CompositionError;
import composer.rules.CompositionRule;
import composer.rules.ConstructorConcatenation;
import composer.rules.ContractComposition;
import composer.rules.ContractKeywordComposition;
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
import composer.rules.rtcomp.java.JavaRuntimeFeatureSelection;
import composer.rules.rtcomp.java.JavaRuntimeFunctionRefinement;
import composer.rules.rtcomp.java.JavaRuntimeReplacement;

import counter.Counter;
import de.ovgu.cide.fstgen.ast.AbstractFSTParser;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class FSTGenComposer extends FSTGenProcessor {

	protected CmdLineInterpreter cmd = new CmdLineInterpreter();
	
	protected CompositionMetadataStore meta = CompositionMetadataStore.getInstance();
	protected List<CompositionRule> compositionRules;

	/* 
	 * Stream to which all output should be directed. 
	 * Normally this is set to System.out, but sometimes we want to handle output differently (e.g. unit tests).
	 */
	public static PrintStream outStream = System.out;
	
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
			} else if (cmd.lifting_language.equals("java")) {
				compositionRules.add(new JavaRuntimeReplacement());
				compositionRules.add(new JavaRuntimeFunctionRefinement());
			} else {
				throw new InternalError("lifting language \"" + cmd.lifting_language + "\" is not implemented.");
			}
		} else {
			compositionRules.add(new Replacement());
			compositionRules.add(new JavaMethodOverriding());
			compositionRules.add(new ContractComposition(cmd.contract_style));
			compositionRules.add(new ContractKeywordComposition(cmd.contract_style));
		}
		compositionRules.add(new StringConcatenation());
		compositionRules.add(new ImplementsListMerging());
		compositionRules.add(new CSharpMethodOverriding());
		compositionRules.add(new AsmetaLRuleOverriding());
		compositionRules.add(new AsmetaLFunctionOverriding());
		compositionRules.add(new AsmetaLInitializationConcatenation());
		compositionRules.add(new AsmetaLInvariantConjunction());
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
				
				for (FSTNonTerminal feature : features) {
					setOriginalFeatureName(feature, feature.getName());
					meta.addFeature(feature.getName());
					meta.discoverFuncIntroductions(feature);
				}
				FSTNode composition = compose(features);
//				modify(composition);

				
				
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
				if (cmd.lifting && "java".equals(cmd.lifting_language.toLowerCase())) {
					saveSwitchIDAnnotationFile(srcDir);
				}
			}
			try {
				if (cmd.exportRolesInJSONformat) {
					meta.saveToFile(outputDir + File.separator + "roles.meta");
				}
				if (cmd.lifting) {
					File cnfFile = new File(cmd.equationBaseDirectoryName, "model.cnf");
					FSTGenComposer.outStream.println("cnfFile:" + cnfFile.getAbsolutePath());
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
		FSTGenComposer.outStream.println("writing FeatureAnnotation to file " +  f.getAbsolutePath());
		FileWriter fw = null;
		try  {
			fw = new FileWriter(f);
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
		} finally {
			try {
				if (fw != null) {
					fw.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	private void saveSwitchIDAnnotationFile(File srcDir) {
		File f = new File(srcDir+File.separator+"featureHouse"+File.separator, "FeatureSwitchID.java");
		f.getParentFile().mkdirs();
		FSTGenComposer.outStream.println("writing FeatureSwitchID to file " +  f.getAbsolutePath());
		FileWriter fw = null; 
		try {
			fw = new FileWriter(f);
			String contents =
				"package featureHouse;\n"+
				"import java.lang.annotation.ElementType;\n" +
				"import java.lang.annotation.Retention;\n" +
				"import java.lang.annotation.RetentionPolicy;\n" +
				"import java.lang.annotation.Target;\n" +
	
				"@Retention(RetentionPolicy.RUNTIME)\n" +
				"@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})\n" +
				"public @interface FeatureSwitchID {\n" +
				"	int id();\n" +
				"	String thenFeature();\n" +
				"	String elseFeature();\n" +
				"}";
			fw.write(contents);
		} catch (IOException e) {
			System.err.println("Could not write FeatureSwitchID.java " + e.getMessage());
		} finally {
			try {
				if (fw != null) {
					fw.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	public static void main(String[] args) {
		FSTGenComposer composer = new FSTGenComposer();
		composer.run(args);
	}
	public static void composeWithPrintStream(String[] args, PrintStream out) {
		FSTGenComposer composer = new FSTGenComposer();
		FSTGenComposer.outStream = out;
		composer.run(args);
	}
	private FSTNode compose(List<FSTNonTerminal> tl) {
		FSTNode composed = null;
		for (FSTNode current : tl) {
			// Several features will be merged in this node. Therefore its original feature is removed.
			setOriginalFeatureName((FSTNonTerminal)current, "");
			if (composed != null) {
				composed = compose(current, composed);
			} else {
				if (cmd.featureAnnotation) {
					addAnnotationToChildrenMethods(current, current.getFeatureName());
				}
				composed = current;
			}
		}
		return composed;
	}
	
	/**
	 * Set the original feature of FSTTerminals, because the tree is composed and this
	 * information would be lost.
	 */
	protected void setOriginalFeatureName(FSTNonTerminal node, String feature) {
		if (node.getType().equals("Feature")) {
			feature = node.getName();
		}
		for (FSTNode child : node.getChildren()) {
			if (child instanceof FSTNonTerminal) {
				setOriginalFeatureName((FSTNonTerminal) child, feature);
			} else if (child instanceof FSTTerminal) {
				((FSTTerminal)child).setOriginalFeatureName(feature);
			}
		}
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
				((FSTTerminal)current).setBody(JavaMethodOverriding.featureAnnotationPrefix + featureName +"\")\n" + body);
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
			FSTNonTerminal compParent) {

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
						handleChildWithoutCompatibleSiblings(childA, nonterminalComp);
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
					try {
						applicableRule.compose(terminalA, terminalB, terminalComp, nonterminalParent);
					} catch (CompositionException e) {
						fireCompositionErrorOccured(e);
					}
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

	/**
	 * Handles children FSTNodes that do not have compatible siblings.
	 * Such nodes sometimes have to be processed (replacing hooks, etc).
	 * In the end a deep clone of the child is added to the composition result parent.
	 * @param child The child FSTNode which has no siblings it can be composed with.
	 * @param compParent The parent node in the final tree (not necessarily the parent of child in the feature).
	 */
	private void handleChildWithoutCompatibleSiblings(FSTNode child, FSTNonTerminal compParent) {
		if (child instanceof XMLHook) {
			/* Handles before|after hooks in Android XML
			 * If an android:id is present in the Hook, all containing
			 * widgets will be placed before|after that widget. If no id is
			 * found, they will either be prepended, or appended to the branch.
			 */
			replaceXMLHooksInNT(compParent, child);
		} else {
			/* Adds java Annotations (e.g. @Feature("base")) to methods and constructors in the java source code of the feature. */
			FSTNode newChildA = child.getDeepClone();
			if (cmd.featureAnnotation) {
				if (newChildA instanceof FSTNonTerminal) {
					addAnnotationToChildrenMethods(newChildA, child.getFeatureName());
				} else if (newChildA instanceof FSTTerminal) {
					if ("MethodDecl".equals(newChildA.getType()) ||
							"ConstructorDecl".equals(newChildA.getType())) {
						FSTTerminal termNewChildA = (FSTTerminal) newChildA;
						String body = termNewChildA.getBody();
						String feature = termNewChildA.getOriginalFeatureName();
						termNewChildA.setBody(JavaMethodOverriding.featureAnnotationPrefix + feature +"\")\n" + body);
					}
				}
			}
			compParent.addChild(newChildA);
		}
	}

	/**
	 * Handles before|after hooks in Android XML
	 * If an android:id is present in the Hook, all containing
	 * widgets will be placed before|after that widget. If no id is
	 * found, they will either be prepended, or appended to the branch.
	 * @param nonterminalComp
	 * @param childA
	 */
	private void replaceXMLHooksInNT(FSTNonTerminal nonterminalComp, FSTNode childA) {
		String beforeOrAfterId  = ((XMLNode) childA).getName().toString();
		List<FSTNode> xmlChildren = ((FSTNonTerminal) childA).getChildren();
		List<FSTNode> children = nonterminalComp.getChildren();
		FSTNode beforeOrAfterNode = null;
		for (FSTNode c : children) {
			if (c.getName().equalsIgnoreCase(beforeOrAfterId)) {
				beforeOrAfterNode = c;
				break;
			}
		}
		if (childA.getType().equalsIgnoreCase("before")) {
			int beforeOrAfter = 0;
			if (beforeOrAfterNode != null) {
				int i = children.indexOf(beforeOrAfterNode);
				for (FSTNode xmlChild : xmlChildren) {
					if (xmlChild instanceof FSTNonTerminal) {
						nonterminalComp.addChild(xmlChild.getDeepClone(), i + beforeOrAfter);
						i++;
					}
				}
			} else {
				int i = 0;
				for (FSTNode xmlChild : xmlChildren) {
					if (xmlChild instanceof FSTNonTerminal) {
						nonterminalComp.addChild(xmlChild.getDeepClone(), i);
						i++;
					}
				}
			}
		} else if (childA.getType().equalsIgnoreCase("after")) {
			int beforeOrAfter = 1;
			if (beforeOrAfterNode != null) {
				int i = children.indexOf(beforeOrAfterNode);
				for (FSTNode xmlChild : xmlChildren) {
					if (xmlChild instanceof FSTNonTerminal) {
						nonterminalComp.addChild(xmlChild.getDeepClone(), i + beforeOrAfter);
						i++;
					}
				}
			} else {
				for (FSTNode xmlChild : xmlChildren) {
					if (xmlChild instanceof FSTNonTerminal) {
						nonterminalComp.addChild(xmlChild.getDeepClone());
					}
				}
			}
		} else {
			nonterminalComp.addChild(childA.getDeepClone());
		}
	}
}