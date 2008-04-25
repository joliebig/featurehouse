package builder.binary;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;

import builder.ArtifactBuilder;

import composer.rules.CompositionError;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class BinaryBuilder extends ArtifactBuilder {
	public BinaryBuilder(String suffix) {
		super(suffix);
	}
	
	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException {
		String docName = st.nextToken();
		FSTNonTerminal rootDocument = new FSTNonTerminal(getSuffix() + "-File", docName);
		parent.addChild(rootDocument);
		FSTTerminal contentNode = new FSTTerminal(getSuffix() + "-Content", docName, inputFile.getAbsolutePath(), "", CompositionError.COMPOSITION_RULE_NAME);
		rootDocument.addChild(contentNode);
	}
}
