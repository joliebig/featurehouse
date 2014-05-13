package builder.xml;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;

import builder.ArtifactBuilder;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class XMLBuilder extends ArtifactBuilder {
	public XMLBuilder() {
		super(".xml");
	}
	
	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("XML-File", st.nextToken());
		parent.addChild(rootDocument);
		
		XMLFactory builder = new XMLFactory(inputFile, rootDocument);
		builder.extract();
	}
}
