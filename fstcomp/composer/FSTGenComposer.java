package composer;

import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;

import modification.traversalLanguageParser.ParseException;
import modification.traversalLanguageParser.TraversalLanguageParser;
import printer.FeaturePrintVisitor;
import printer.PrintVisitorException;
import printer.PrintVisitorInterface;
import printer.binary.BinaryPrintVisitor;
import printer.capprox.CApproxPrintVisitor;
import printer.csharp.CSharpPrintVisitor;
import printer.haskell.HaskellPrintVisitor;
import printer.java.JavaPrintVisitor;
import printer.javacc.JavaCCPrintVisitor;
import printer.phaskell.PHaskellPrintVisitor;
import printer.text.TextPrintVisitor;
import printer.xmi.XMIPrintVisitor;
import builder.ArtifactBuilderInterface;
import builder.binary.BinaryBuilder;
import builder.capprox.CApproxBuilder;
import builder.csharp.CSharpBuilder;
import builder.haskell.HaskellBuilder;
import builder.java.JavaBuilder;
import builder.javacc.JavaCCBuilder;
import builder.phaskell.PHaskellBuilder;
import builder.text.TextBuilder;
import builder.xmi.XMIBuilder;

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

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class FSTGenComposer {

    private CmdLineInterpreter cmd = new CmdLineInterpreter();

    private FileLoader fileLoader = new FileLoader();

    private FeaturePrintVisitor featureVisitor = new FeaturePrintVisitor();

    public void registerArtifactBuilder(ArtifactBuilderInterface builder) {
	fileLoader.registerArtifactBuilder(builder);
    }

    public void unregisterArtifactBuilder(ArtifactBuilderInterface builder) {
	fileLoader.unregisterArtifactBuilder(builder);
    }

    public LinkedList<ArtifactBuilderInterface> getArtifactBuilders() {
	return fileLoader.getArtifactBuilders();
    }

    public void registerPrintVisitor(PrintVisitorInterface visitor) {
	this.featureVisitor.registerPrintVisitor(visitor);
    }

    public void unregisterPrintVisitor(PrintVisitorInterface visitor) {
	this.featureVisitor.unregisterPrintVisitor(visitor);
    }

    public LinkedList<PrintVisitorInterface> getPrintVisitors() {
	return featureVisitor.getPrintVisitors();
    }

    void run(String[] args) {
	cmd.parseCmdLineArguments(args);

	try {
	    fileLoader.loadFiles(cmd.equationFileName,
		    cmd.equationBaseDirectoryName, cmd.isAheadEquationFile);
	    String outputDir = cmd.equationBaseDirectoryName;
	    if (cmd.outputDirectoryName != null)
		outputDir = cmd.outputDirectoryName;

	    featureVisitor.setWorkingDir(outputDir);
	    featureVisitor.setExpressionName(cmd.equationFileName);

	    for (ArtifactBuilderInterface builder : getArtifactBuilders()) {
		LinkedList<FSTNonTerminal> features = builder.getFeatures();
		
		for(FSTNonTerminal feature : features)
			System.err.println(feature.toString());
		
		FSTNode composition = compose(features);
		//modify(composition);

		//if(composition != null)
		//	System.err.println(composition.toString());
		try {
		    featureVisitor.visit((FSTNonTerminal) composition);
		} catch (PrintVisitorException e) {
		    e.printStackTrace();
		}
	    }
	} catch (FileNotFoundException e1) {
	    e1.printStackTrace();
	}
    }

    private void modify(FSTNode composition) {

	if (composition != null) {
	    System.err.println(composition);

	    List<FSTNode> list = new LinkedList<FSTNode>();

	    String query = "..";
	    query = "..*:*..*:*..*:*";
	    query = "..*:MethodD*";
	    //query = "*%s %s  .,?%e:*..*:*";

	    TraversalLanguageParser parser = new TraversalLanguageParser(query,
		    composition);
	    try {
		list = parser.parse();
	    } catch (ParseException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	    }

	    for (FSTNode node : list) {
		System.err.println(node.getName() + ":" + node.getType());
	    }
	}

    }

    public static void main(String[] args) {
	FSTGenComposer composer = new FSTGenComposer();
	composer.registerArtifactBuilder(new JavaBuilder());
	composer.registerArtifactBuilder(new CSharpBuilder());
	composer.registerArtifactBuilder(new CApproxBuilder());
	//composer.registerArtifactBuilder(new PHaskellBuilder());
	composer.registerArtifactBuilder(new HaskellBuilder());
	composer.registerArtifactBuilder(new JavaCCBuilder());
	composer.registerArtifactBuilder(new XMIBuilder());
	composer.registerArtifactBuilder(new TextBuilder(".properties"));
	composer.registerArtifactBuilder(new BinaryBuilder(".jpg"));
	composer.registerPrintVisitor(new JavaPrintVisitor());
	composer.registerPrintVisitor(new CSharpPrintVisitor());
	composer.registerPrintVisitor(new CApproxPrintVisitor());
	composer.registerPrintVisitor(new JavaCCPrintVisitor());
	//composer.registerPrintVisitor(new PHaskellPrintVisitor());
	composer.registerPrintVisitor(new HaskellPrintVisitor());
	composer.registerPrintVisitor(new XMIPrintVisitor());
	composer.registerPrintVisitor(new TextPrintVisitor(".properties"));
	composer.registerPrintVisitor(new BinaryPrintVisitor(".jpg"));
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
		    // System.out.println("Modifier list specification: " +
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