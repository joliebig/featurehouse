package composer;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import printer.PrintVisitorException;
import builder.ArtifactBuilderInterface;
import builder.capprox.CApproxBuilder;
import builder.java.JavaBuilder;

import composer.rules.CSharpMethodOverriding;
import composer.rules.CompositionError;
import composer.rules.CompositionRule;
import composer.rules.ExpansionOverriding;
import composer.rules.ImplementsListMerging;
import composer.rules.ModifierListSpecialization;
import composer.rules.Replacement;
import composer.rules.StringConcatenation;
import composer.rules.meta.ConstructorConcatenationMeta;
import composer.rules.meta.ContractCompositionMeta;
import composer.rules.meta.FeatureModelInfo;
import composer.rules.meta.FieldOverridingMeta;
import composer.rules.meta.InvariantCompositionMeta;
import composer.rules.meta.JavaMethodOverridingMeta;
import composer.rules.meta.MinimalFeatureModelInfo;
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

public class FSTGenComposerExtension extends FSTGenComposer {
	
	public static boolean key = false;
	public static boolean metaproduct = false;
	private FeatureModelInfo modelInfo = new MinimalFeatureModelInfo();
	

	public FSTGenComposerExtension() {
		super();
	}

	public FSTGenComposerExtension(FeatureModelInfo modelInfo) {
		super();
		this.modelInfo = modelInfo;
	}
	
	public void setModelInfo(FeatureModelInfo infoObject){
		this.modelInfo = infoObject;
	}
	/**
	 * Builds the full FST of the project without composition.
	 * @param args Default build parameters
	 * @param featuresArg An array containing all features of the project
	 */
	public void buildFullFST(String[] args, String[] featuresArg) {
		metaproduct = false;
		build(args, featuresArg, false);
	}
	
	public void buildMetaProduct(String[] args, String[] featuresArg) {
		metaproduct = true;
		build(args, featuresArg, true);
	}

	private void build(String[] args, String[] featuresArg, boolean compose) {
		meta.clearFeatures();
		cmd.parseCmdLineArguments(args);
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
			compositionRules.add(new JavaMethodOverridingMeta());
		}
		compositionRules.add(new InvariantCompositionMeta());
		compositionRules.add(new ContractCompositionMeta(cmd.contract_style,modelInfo));
		compositionRules.add(new StringConcatenation());
		compositionRules.add(new ImplementsListMerging());
		compositionRules.add(new CSharpMethodOverriding());
		compositionRules.add(new ConstructorConcatenationMeta());
		compositionRules.add(new ModifierListSpecialization());
		compositionRules.add(new FieldOverridingMeta());
		compositionRules.add(new ExpansionOverriding());
		compositionRules.add(new CompositionError());
		
		try {
			try {
				fileLoader.loadFiles(cmd.equationFileName, cmd.equationBaseDirectoryName, cmd.isAheadEquationFile, featuresArg);
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
					meta.addFeature(feature.getName());
				}
				modelInfo.clearFeatureNodes();
				if (features.size() > 0)
					modelInfo.addFeatureNodes(features);
				
				if (compose) {
					FSTNode composition = composeMeta(features);
					try {
						featureVisitor.visit((FSTNonTerminal) composition);
					} catch (PrintVisitorException e) {
						e.printStackTrace();
					}
				}
			}
			setFstnodes(AbstractFSTParser.fstnodes);
			if (!compose) {
				return;
			}
			try {
				String exp = new File(cmd.equationFileName).getName();
				exp = exp.substring(0, exp.length() - 4);
				
				meta.saveToFile(outputDir + File.separator + "roles.meta");
				if (cmd.lifting) {
					File cnfFile = new File(cmd.equationBaseDirectoryName, "model.cnf");
					System.err.println("cnfFile:" + cnfFile.getAbsolutePath());
					if (cmd.lifting_language.equals("c")) {
						new CRuntimeFeatureSelection(meta, cnfFile).saveTo(outputDir + File.separator + "features/featureselect");
					} else if (cmd.lifting_language.equals("java")) {
						new JavaRuntimeFeatureSelection(meta, cnfFile).saveTo(outputDir + File.separator + cmd.equationFileName + File.separator);
					}
				}
			} catch (IOException e) {			
				e.printStackTrace();
			}
		} catch (FileNotFoundException e1) {
		}
	}
	
	private FSTNode composeMeta(List<FSTNonTerminal> tl) {
		FSTNode composed = null;
		for (FSTNode current : tl) {
			if (metaproduct) {
				preProcessSubtree(current);
			}
			if (composed != null) {
				composed = compose(current, composed);
			} else
				composed = current;
		}
		if (metaproduct) {
			postProcess(composed);
		}
		return composed;
	}
	
	private void preProcessSubtree(FSTNode child) {
		if (child instanceof FSTNonTerminal) {
			if (child.getType().equals("MethodSpecification") && ((FSTNonTerminal) child).getChildren().isEmpty()) {
				FSTNonTerminal spec = new FSTNonTerminal("Specification", "-");
				((FSTNonTerminal) child).addChild(spec);
				(spec).addChild(new FSTTerminal("SpecCaseSeq", "-", "requires FM.FeatureModel." + getFeatureName(spec) + " || FM.Features.OrOriginal;", "", "ContractComposition"));
			} else {
				for (FSTNode node : ((FSTNonTerminal) child).getChildren()) {
					preProcessSubtree(node);
				}
			}
		} else if (child instanceof FSTTerminal) {
			for (CompositionRule rule: compositionRules) {
				if (((FSTTerminal) child).getCompositionMechanism().equals(rule.getRuleName())) {	
					rule.preCompose((FSTTerminal) child);
					break;
				}
			}
		}
	}
	
	private void postProcess(FSTNode child) {
		if (child instanceof FSTNonTerminal) {
			for (FSTNode node : ((FSTNonTerminal) child).getChildren()) {
				postProcess(node);
			}
		} else if (child instanceof FSTTerminal) {
			for (CompositionRule rule: compositionRules) {
				if (((FSTTerminal) child).getCompositionMechanism().equals(rule.getRuleName())) {	
					rule.postCompose((FSTTerminal) child);
					break;
				}
			}
		}
	}

	private static String getFeatureName(FSTNode node) {
		if (node.getType().equals("Feature"))
			return node.getName().toLowerCase() + (key ? "" : "()");
		else
			return getFeatureName(node.getParent());
	}
}
