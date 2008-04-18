package builder.text;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.StringTokenizer;

import composer.FSTGenComposer;
import composer.rules.StringConcatenation;

import builder.ArtifactBuilder;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class TextBuilder extends ArtifactBuilder {
	private String suffix;

	public TextBuilder(String suffix) {
		this.suffix = suffix;
	}
	
	@Override
	public boolean acceptFile(File inputFile) {
		if (inputFile.isFile()) {
		      if (inputFile.getName().endsWith(suffix)) {
		        return true;
		      }
		    }
		    return false;
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
		FSTNonTerminal rootDocument = new FSTNonTerminal(suffix + "-File", docName);
		parent.addChild(rootDocument);
		FSTTerminal contentNode = new FSTTerminal(suffix + "-Content", docName, content, "", StringConcatenation.COMPOSITION_RULE_NAME);
		rootDocument.addChild(contentNode);
	}
}
