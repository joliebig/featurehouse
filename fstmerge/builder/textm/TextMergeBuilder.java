package builder.textm;

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

public class TextMergeBuilder extends ArtifactBuilder {
	public TextMergeBuilder(String suffix) {
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
		FSTNonTerminal rootDocument = new FSTNonTerminal(getSuffix() + ".merge-File", docName + ".merge");
		parent.addChild(rootDocument);
		FSTTerminal contentNode = new FSTTerminal(getSuffix() + "-Content", docName, content, "", StringConcatenation.COMPOSITION_RULE_NAME, "LineBased");
		rootDocument.addChild(contentNode);
	}
}
