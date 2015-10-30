package merger;

import jargs.gnu.CmdLineParser;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
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
import de.ovgu.cide.fstgen.ast.AbstractFSTParser;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class FSTGenMerger extends FSTGenProcessor {
	
	static final String MERGE_SEPARATOR = "##FSTMerge##";
	static final String SEMANTIC_MERGE_MARKER = "~~FSTMerge~~";
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
	
	public void printUsage() {
		System.err.println(
"Usage: FSTGenMerger [-h, --help] [-o, --output-directory] \n" +
"                    [-b, --base-directory] [-p, --preprocess-files] \n" +
"                    <-e, --expression>|<-f, --filemerge> myfile parentfile yourfile \n");
	}
	
	public void run(String[] args) {
		// configuration options
		CmdLineParser cmdparser = new CmdLineParser();
		CmdLineParser.Option outputdir = cmdparser.addStringOption('o',	"output-directory");
		CmdLineParser.Option expression = cmdparser.addStringOption('e', "expression");
		CmdLineParser.Option basedir = cmdparser.addStringOption('b', "base-directory");
		@SuppressWarnings("unused")
		CmdLineParser.Option help = cmdparser.addBooleanOption('h', "help");
		CmdLineParser.Option preprocessfiles = cmdparser.addBooleanOption('p', "preprocess-files");
		CmdLineParser.Option quiet = cmdparser.addBooleanOption('q', "quiet");
		CmdLineParser.Option filemerge = cmdparser.addBooleanOption('f', "filemerge");
		
		try {
			cmdparser.parse(args);
		} catch (CmdLineParser.OptionException e) {
			System.out.println(e.getMessage());
			printUsage();
			System.exit(2);
		}
		
		Boolean preprocessfilesval = (Boolean)cmdparser.getOptionValue(preprocessfiles, Boolean.FALSE);
		fileLoader.setPreprocessFiles(preprocessfilesval);
		Boolean filemergeval = (Boolean)cmdparser.getOptionValue(filemerge);
		String expressionval = (String)cmdparser.getOptionValue(expression);
		String basedirval = (String)cmdparser.getOptionValue(basedir);

		if (null == expressionval) {
			if (null == filemergeval || cmdparser.getRemainingArgs().length != 3) {
				printUsage();
				System.exit(2);
			} else {
				String config[] = generateExpressionFile(cmdparser.getRemainingArgs());
				expressionval = config[0];
				basedirval = config[1];
			}
		}

		assert expressionval != null : "Expression file must not be null!";

		if (null == basedirval) {
			basedirval = (new File(expressionval)).getAbsoluteFile().getParentFile().getPath();
		}
		String outputdirval = (String)cmdparser.getOptionValue(outputdir);
		Boolean quietval = (Boolean)cmdparser.getOptionValue(quiet, Boolean.FALSE);
		
		try {
			try {
				fileLoader.loadFiles(expressionval, basedirval, false);
			} catch (cide.gparser.ParseException e1) {
				fireParseErrorOccured(e1);
				e1.printStackTrace();
			}

			if (null != outputdirval) featureVisitor.setWorkingDir(outputdirval);
			else featureVisitor.setWorkingDir(basedirval);
			featureVisitor.setExpressionName(expressionval);

			for (ArtifactBuilderInterface builder : getArtifactBuilders()) {
				LinkedList<FSTNonTerminal> features = builder.getFeatures();

				if (!quietval)
					for(FSTNonTerminal feature : features){
						String ftos = feature.toString();
						if (! ftos.isEmpty())
							System.out.println(feature.toString());
					}
						
				
				FSTNode merged;
				
				if(features.size() != 0) {
					merged = merge(features);
					
					mergeVisitor.visit(merged);
					
					if (!quietval) {
						String mtos = merged.toString();
						if (! mtos.isEmpty())
							System.err.println(merged.toString());
					}
						
					
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

	public static FSTNode merge(FSTNode nodeA, FSTNode nodeB, FSTNonTerminal compParent, boolean firstPass) {
		
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

	/**
	 * Returns the common, leading part of a set of directory paths.
	 *
	 * @param paths set of directory paths
	 * @return common leading path
	 */
	private static String getCommonPath(String... paths) {
		String common = "";
		String dirs[][] = new String[paths.length][];

		for (int i = 0; i < paths.length; i++) {
			dirs[i] = paths[i].split(File.separator);
		}

		for (int j = 0; j < dirs[0].length; j++) {
			String dir = dirs[0][j];

			for (int i = 1; i < paths.length; i++) {
				if (dirs[i].length <= j || !dirs[i][j].equals(dir))
					return common;
			}

			common += dir + File.separator;
		}

		return common;
	}

    /**
     * Takes three variants and generates a temporary expression file.
     *
     * Returns the filename of the expression file and the value of the base directory.
     *
     * @param variants [mine, base, other]
     * @return [expressionFile, baseDir]
     */
    private static String[] generateExpressionFile(String[] variants) {
        assert variants.length == 3 : "Exactly three variants (mine, base, other) have to be specified.";

        String expressionval = null;
        String basedirval = null;

        try {
            File configFile = File.createTempFile("fstmerge", null);
            configFile.deleteOnExit();
            expressionval = configFile.getAbsolutePath();
            Path basePath = Paths.get(getCommonPath(variants));
            basedirval = basePath.toString();

            PrintWriter writer = new PrintWriter(configFile);
            for (String variant : variants)
                writer.println(basePath.relativize(Paths.get(variant)));

            writer.close();

        } catch (IOException e) {
            e.printStackTrace();
        }

        return new String[] {expressionval, basedirval};
    }
}
