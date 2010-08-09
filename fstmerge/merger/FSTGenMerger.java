package merger;

import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;

import printer.PrintVisitorException;
import printer.PrintVisitorInterface;
import printer.csharp.CSharpPrintVisitor;
import printer.java.JavaPrintVisitor;
import printer.javam.JavaMergePrintVisitor;
import printer.csharpm.CSharpMergePrintVisitor;
import printer.pythonm.PythonMergePrintVisitor;
import printer.textm.TextMergePrintVisitor;
import builder.ArtifactBuilderInterface;
import builder.csharp.CSharpBuilder;
import builder.csharpm.CSharpMergeBuilder;
import builder.java.JavaBuilder;
import builder.javam.JavaMergeBuilder;
import builder.pythonm.PythonMergeBuilder;
import builder.textm.TextMergeBuilder;


import composer.FSTGenProcessor;
import composer.rules.Replacement;

import de.ovgu.cide.fstgen.ast.AbstractFSTParser;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class FSTGenMerger extends FSTGenProcessor {
	
	static final String MERGE_SEPARATOR = "##FSTMerge##";
	static final String SEMANTIC_MERGE_MARKER = "~~FSTMerge~~";
	private CmdLineInterpreter cmd = new CmdLineInterpreter();
	private static LinkedList<FSTNode> baseNodes = new LinkedList<FSTNode>();
	
	private MergeVisitor mergeVisitor = new MergeVisitor();
		
	public FSTGenMerger() {
		super();
		mergeVisitor.registerMerger(new LineBasedMerger());
		ArtifactBuilderInterface stdJavaBuilder = null;
		ArtifactBuilderInterface stdCSharpBuilder = null;
		for(ArtifactBuilderInterface builder : this.getArtifactBuilders()) {
			if(builder instanceof JavaBuilder)
				stdJavaBuilder = builder;
			if(builder instanceof CSharpBuilder)
				stdCSharpBuilder = builder;
		}
		
		unregisterArtifactBuilder(stdJavaBuilder);
		unregisterArtifactBuilder(stdCSharpBuilder);
		
		registerArtifactBuilder(new JavaMergeBuilder());
		registerArtifactBuilder(new CSharpMergeBuilder());
		registerArtifactBuilder(new PythonMergeBuilder());
		registerArtifactBuilder(new TextMergeBuilder(".java"));
		registerArtifactBuilder(new TextMergeBuilder(".cs"));
		registerArtifactBuilder(new TextMergeBuilder(".py"));
		
		PrintVisitorInterface stdJavaPrinter = null;
		PrintVisitorInterface stdCSharpPrinter = null;
		for(PrintVisitorInterface printer : this.getPrintVisitors()) {
			if(printer instanceof JavaPrintVisitor)
				stdJavaPrinter = printer;
			if(printer instanceof CSharpPrintVisitor)
				stdCSharpPrinter = printer;
		}
		
		unregisterPrintVisitor(stdJavaPrinter);
		unregisterPrintVisitor(stdCSharpPrinter);
		
		registerPrintVisitor(new JavaMergePrintVisitor());
		registerPrintVisitor(new CSharpMergePrintVisitor());
		registerPrintVisitor(new PythonMergePrintVisitor());
		registerPrintVisitor(new TextMergePrintVisitor(".java"));
		registerPrintVisitor(new TextMergePrintVisitor(".cs"));
		registerPrintVisitor(new TextMergePrintVisitor(".py"));

	}
	
	public void run(String[] args) {
		cmd.parseCmdLineArguments(args);
		try {
			try {
				if (cmd.preprocessFiles)
					fileLoader.setPreprocessFiles(true);

				fileLoader.loadFiles(cmd.equationFileName, cmd.equationBaseDirectoryName, false);
			} catch (cide.gparser.ParseException e1) {
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

				for(FSTNonTerminal feature : features)
					System.out.println(feature.toString());
				
				FSTNode merged;
				
				if(features.size() != 0) {
					merged = merge(features);
					
					mergeVisitor.visit(merged);
					
					System.err.println(merged.toString());
					
					try {
						featureVisitor.visit((FSTNonTerminal) merged);
					} catch (PrintVisitorException e) {
						e.printStackTrace();
					}
				}
			}
			setFstnodes(AbstractFSTParser.fstnodes);
		} catch (MergeException me) {
			System.err.println(me.toString());
			me.printStackTrace();
		} catch (FileNotFoundException e1) {
			e1.printStackTrace();
		}
	}

	public static void main(String[] args) {
		FSTGenMerger merger = new FSTGenMerger();
		merger.run(args);
	}
	
	private static FSTNode merge(List<FSTNonTerminal> tl) throws MergeException {
		
		if(tl.size() != 3)
			throw new MergeException(tl);
		
		tl.get(0).index = 0;
		tl.get(1).index = 1;
		tl.get(2).index = 2;
		
		FSTNode mergeLeftBase = merge(tl.get(0), tl.get(1), true);
		FSTNode mergeLeftBaseRight = merge(mergeLeftBase, tl.get(2), false);
		removeLoneBaseNodes(mergeLeftBaseRight);
		return mergeLeftBaseRight;
	}

	public static FSTNode merge(FSTNode nodeA, FSTNode nodeB, boolean firstPass) {
		return merge(nodeA, nodeB, null, firstPass);
	}

	public static FSTNode merge(FSTNode nodeA, FSTNode nodeB, FSTNode compParent, boolean firstPass) {
		
		//System.err.println("nodeA: " + nodeA.getName() + " index: " + nodeA.index);
		//System.err.println("nodeB: " + nodeB.getName() + " index: " + nodeB.index);
		
		if (nodeA.compatibleWith(nodeB)) {
			FSTNode compNode = nodeA.getShallowClone();
			compNode.index = nodeB.index;
			compNode.setParent(compParent);

			if (nodeA instanceof FSTNonTerminal	&& nodeB instanceof FSTNonTerminal) {
				FSTNonTerminal nonterminalA = (FSTNonTerminal) nodeA;
				FSTNonTerminal nonterminalB = (FSTNonTerminal) nodeB;
				FSTNonTerminal nonterminalComp = (FSTNonTerminal) compNode;

				for (FSTNode childB : nonterminalB.getChildren()) {
					FSTNode childA = nonterminalA.getCompatibleChild(childB);
					if (childA == null) {
						FSTNode cloneB = childB.getDeepClone();
						if(childB.index == -1)
							childB.index = nodeB.index;
						cloneB.index = childB.index;
						nonterminalComp.addChild(cloneB);
						//System.err.println("cloneB: " + cloneB.getName() + " index: " + cloneB.index);
						if(firstPass) {
							baseNodes.add(cloneB);
						}
					} else {
						if(childA.index == -1)
							childA.index = nodeA.index;
						if(childB.index == -1)
							childB.index = nodeB.index;
						nonterminalComp.addChild(merge(childA, childB, nonterminalComp, firstPass));
					}
				}
				for (FSTNode childA : nonterminalA.getChildren()) {
					FSTNode childB = nonterminalB.getCompatibleChild(childA);
					if (childB == null) {
						FSTNode cloneA = childA.getDeepClone();
						if(childA.index == -1)
							childA.index = nodeA.index;
						cloneA.index = childA.index;
						nonterminalComp.addChild(cloneA);
						//System.err.println("cloneA: " + cloneA.getName() + " index: " + cloneA.index);
						if(baseNodes.contains(childA)) {
							baseNodes.remove(childA);
							baseNodes.add(cloneA);
						}
					} else {
						if(!firstPass) {
							baseNodes.remove(childA);
						}
					}
				}
				return nonterminalComp;
			} else if (nodeA instanceof FSTTerminal && nodeB instanceof FSTTerminal && compParent instanceof FSTNonTerminal) {
				FSTTerminal terminalA = (FSTTerminal) nodeA;
				FSTTerminal terminalB = (FSTTerminal) nodeB;
				FSTTerminal terminalComp = (FSTTerminal) compNode;
				
				// SPECIAL CONFLICT HANDLER
				if (!terminalA.getMergingMechanism().equals("Default")) {
					terminalComp.setBody(mergeBody(terminalA.getBody(), terminalB.getBody(), firstPass, terminalA.index, terminalB.index));
				} 
				return terminalComp;
			}
			return null;
		} else
			return null;
	}
	
	private static String mergeBody(String bodyA, String bodyB, boolean firstPass, int indexA, int indexB) {

		//System.err.println(firstPass);
		//System.err.println("#" + bodyA + "#");
		//System.err.println("#" + bodyB + "#");
		
		if (bodyA.contains(SEMANTIC_MERGE_MARKER)) {
			return bodyA + " " + bodyB;
		}
		else {
			if(firstPass) {
				return SEMANTIC_MERGE_MARKER + " " + bodyA + " " + MERGE_SEPARATOR + " " + bodyB + " " + MERGE_SEPARATOR;
			} else {
				if(indexA == 0)
					return SEMANTIC_MERGE_MARKER + " " + bodyA + " " + MERGE_SEPARATOR + " " + MERGE_SEPARATOR + " " + bodyB;
				else
					return SEMANTIC_MERGE_MARKER + " " + MERGE_SEPARATOR + " " + bodyA + " " + MERGE_SEPARATOR + " " + bodyB;
			}	
		}
	}
	private static void removeLoneBaseNodes(FSTNode mergeLeftBaseRight) {
		boolean removed = false;
		for(FSTNode loneBaseNode : baseNodes) {
			if(mergeLeftBaseRight == loneBaseNode) {
				FSTNonTerminal parent = (FSTNonTerminal)mergeLeftBaseRight.getParent();
				if(parent != null) {
					parent.removeChild(mergeLeftBaseRight);
					removed = true;
				}
			}
		}
		if(!removed && mergeLeftBaseRight instanceof FSTNonTerminal) {
			Object[] children = ((FSTNonTerminal)mergeLeftBaseRight).getChildren().toArray();
			for(Object child : children) {
				removeLoneBaseNodes((FSTNode)child);
			}
		}
	}
}
