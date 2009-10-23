package merger;

import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;

import printer.PrintVisitorException;
import builder.ArtifactBuilderInterface;
import composer.FSTGenProcessor;
import composer.rules.Replacement;
import composer.rules.StringConcatenation;
import de.ovgu.cide.fstgen.ast.AbstractFSTParser;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class FSTGenMerger extends FSTGenProcessor {
	
	private CmdLineInterpreter cmd = new CmdLineInterpreter();
	
	public FSTGenMerger() {
		super();
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

				FSTNode merged;
				
				if(features.size() != 0) {
					merged = merge(features);
				
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
		
		return merge(tl.get(0), tl.get(1), tl.get(2));
	}

	private static FSTNode merge(FSTNode var1, FSTNode base, FSTNode var2) {
		return merge(var1, base, var2, null);
	}

	private static FSTNode merge(FSTNode var1, FSTNode base, FSTNode var2, FSTNode compParent) {

		if (base.compatibleWith(var1) && base.compatibleWith(var2)) {
			FSTNode compNode = getHollowClone(base);
			compNode.setParent(compParent);

			if (var1 instanceof FSTNonTerminal && base instanceof FSTNonTerminal && var2 instanceof FSTNonTerminal) {
				FSTNonTerminal ntVar1 = (FSTNonTerminal) var1;
				FSTNonTerminal ntBase = (FSTNonTerminal) base;
				FSTNonTerminal ntVar2 = (FSTNonTerminal) var2;
				FSTNonTerminal ntComp = (FSTNonTerminal) compNode;

				for (FSTNode childBase : ntBase.getChildren()) {
					FSTNode childVar1 = ntVar1.getCompatibleChild(childBase);
					FSTNode childVar2 = ntVar2.getCompatibleChild(childBase);
					if (childVar1 == null)
						childVar1 = getHollowClone(childBase);
					if (childVar2 == null)
						childVar2 = getHollowClone(childBase);
					ntComp.addChild(merge(childVar1, childBase, childVar2, ntComp));
				}
				for (FSTNode childVar1 : ntVar1.getChildren()) {
					FSTNode childBase = ntBase.getCompatibleChild(childVar1);
					FSTNode childVar2 = ntVar2.getCompatibleChild(childVar1);
					if (childBase == null) {
						if (childVar2 == null)
							childVar2 = getHollowClone(childVar1);
						ntComp.addChild(merge(childVar1, getHollowClone(childVar1), childVar2, ntComp));
					}
				}
				for (FSTNode childVar2 : ntVar2.getChildren()) {
					FSTNode childVar1 = ntVar1.getCompatibleChild(childVar2);
					FSTNode childBase = ntBase.getCompatibleChild(childVar2);
					if (childBase == null && childVar1 == null) {
						ntComp.addChild(childVar2.getDeepClone());
					}
				}

				return ntComp;
			} else if (var1 instanceof FSTTerminal && base instanceof FSTTerminal && var2 instanceof FSTTerminal && compParent instanceof FSTNonTerminal) {
				FSTTerminal tVar1 = (FSTTerminal) var1;
				FSTTerminal tBase = (FSTTerminal) base;
				FSTTerminal tVar2 = (FSTTerminal) var2;
				FSTTerminal tComp = (FSTTerminal) compNode;

				tComp.setBody(mergeBody(tVar1.getBody(), tBase.getBody(), tVar2.getBody()));

				return tComp;
			}
			return null;
		} else
			return null;
	}
	
	private static String mergeBody(String sVar1, String sBase, String sVar2) {
		if (sVar1.equals(sBase) && sVar1.equals(sVar2)) {
			return sBase;
		} else if (sVar1.equals(sBase)) {
			if (sVar1.length() == 0) return sVar2;
			else if (sVar2.length() == 0) return sVar1;
		} else if (sVar2.equals(sBase)) {
			if (sVar2.length() == 0) return sVar1;
			else if (sVar1.length() == 0) return sVar2;
		} else if (sVar1.equals(sVar2)) {
			if (sVar1.length() == 0) return sBase;
			else if (sBase.length() == 0) return sVar1;
		} 
		return sVar1 + " # " + sBase + " # " + sVar2;
	}
	
	private static FSTNode getHollowClone(FSTNode node) {
		if (node instanceof FSTTerminal)
			return new FSTTerminal(node.getType(), node.getName(), "", ((FSTTerminal)node).getSpecialTokenPrefix(), ((FSTTerminal)node).getCompositionMechanism());
		else
			return node.getShallowClone();
	}
}
