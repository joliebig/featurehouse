package merger;

import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;
import java.util.TreeMap;
import java.util.Map.Entry;

import printer.PrintVisitorException;
import printer.PrintVisitorInterface;
import printer.java.JavaPrintVisitor;
import printer.javam.JavaMergePrintVisitor;
import printer.textm.TextMergePrintVisitor;
import builder.ArtifactBuilderInterface;
import builder.java.JavaBuilder;
import builder.javam.JavaMergeBuilder;
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
		for(ArtifactBuilderInterface builder : this.getArtifactBuilders()) {
			if(builder instanceof JavaBuilder)
				stdJavaBuilder = builder;
		}
		unregisterArtifactBuilder(stdJavaBuilder);
		registerArtifactBuilder(new JavaMergeBuilder());
		registerArtifactBuilder(new TextMergeBuilder(".java"));
		PrintVisitorInterface stdJavaPrinter = null;
		for(PrintVisitorInterface printer : this.getPrintVisitors()) {
			if(printer instanceof JavaPrintVisitor)
				stdJavaPrinter = printer;
		}
		unregisterPrintVisitor(stdJavaPrinter);
		registerPrintVisitor(new JavaMergePrintVisitor());
		registerPrintVisitor(new TextMergePrintVisitor(".java"));

	}
	
	public void run(String[] args) {
		cmd.parseCmdLineArguments(args);
		try {
			try {
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

				//for(FSTNonTerminal feature : features)
				//	System.out.println(feature.toString());
				
				FSTNode merged;
				
				if(features.size() != 0) {
					merged = merge(features);
					
					mergeVisitor.visit(merged);
					
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
		
		FSTNode mergeLeftBase = merge(tl.get(0), tl.get(1), true);
		FSTNode mergeLeftBaseRight = merge(mergeLeftBase, tl.get(2), false);
		removeLoneBaseNodes(mergeLeftBaseRight);
		return mergeLeftBaseRight;
	}

	public static FSTNode merge(FSTNode nodeA, FSTNode nodeB, boolean firstPass) {
		return merge(nodeA, nodeB, null, firstPass);
	}

	public static FSTNode merge(FSTNode nodeA, FSTNode nodeB, FSTNode compParent, boolean firstPass) {

		if (nodeA.compatibleWith(nodeB)) {
			FSTNode compNode = nodeA.getShallowClone();
			compNode.setParent(compParent);

			if (nodeA instanceof FSTNonTerminal	&& nodeB instanceof FSTNonTerminal) {
				FSTNonTerminal nonterminalA = (FSTNonTerminal) nodeA;
				FSTNonTerminal nonterminalB = (FSTNonTerminal) nodeB;
				FSTNonTerminal nonterminalComp = (FSTNonTerminal) compNode;

				for (FSTNode childB : nonterminalB.getChildren()) {
					FSTNode childA = nonterminalA.getCompatibleChild(childB);
					if (childA == null) {
						FSTNode cloneB = childB.getDeepClone();
						nonterminalComp.addChild(cloneB);
						if(firstPass) {
							baseNodes.add(cloneB);
						}
					} else {
						nonterminalComp.addChild(merge(childA, childB, nonterminalComp, firstPass));
					}
				}
				for (FSTNode childA : nonterminalA.getChildren()) {
					FSTNode childB = nonterminalB.getCompatibleChild(childA);
					if (childB == null) {
						FSTNode cloneA = childA.getDeepClone();
						nonterminalComp.addChild(cloneA);
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

				if (!terminalA.getCompositionMechanism().equals(Replacement.COMPOSITION_RULE_NAME)) {
					terminalComp.setBody(mergeBody(terminalA.getBody(), terminalB.getBody(), firstPass));
				} 
				return terminalComp;
			}
			return null;
		} else
			return null;
	}
	
	private static String mergeBody(String bodyA, String bodyB, boolean firstPass) {
		if (bodyA.equals(bodyB)) {
			return bodyA;
		} else {
			if (bodyA.contains(SEMANTIC_MERGE_MARKER)) {
				return bodyA + " " + bodyB;
			}
			else {
				if(firstPass)
					return SEMANTIC_MERGE_MARKER + " " + bodyA + " " + MERGE_SEPARATOR + " " + bodyB + " " + MERGE_SEPARATOR;
				else
					return SEMANTIC_MERGE_MARKER + " " + bodyA + " " + MERGE_SEPARATOR + " " + MERGE_SEPARATOR + " " + bodyB;
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
