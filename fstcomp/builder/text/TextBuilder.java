package builder.text;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.StringTokenizer;

import builder.ArtifactBuilder;

import composer.rules.StringConcatenation;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class TextBuilder extends ArtifactBuilder {
	public TextBuilder(String suffix) {
		super(suffix);
	}
	
	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException {
		BufferedReader bufRead = new BufferedReader(new FileReader(inputFile));
		String content = "";
		try {
			while(bufRead.ready()) {
				content += bufRead.readLine() + "\n";
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		String docName = st.nextToken();
		FSTNonTerminal rootDocument = new FSTNonTerminal(getSuffix() + "-File", docName);
		parent.addChild(rootDocument);
		FSTTerminal contentNode = new FSTTerminal(getSuffix() + "-Content", docName, content, "", StringConcatenation.COMPOSITION_RULE_NAME);
		rootDocument.addChild(contentNode);
	}
}
