package builder.binary;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.StringTokenizer;

import composer.FSTGenComposer;
import composer.rules.CompositionError;
import composer.rules.StringConcatenation;

import builder.ArtifactBuilder;
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
