package builder.xmi;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;

import builder.ArtifactBuilder;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class XMIBuilder extends ArtifactBuilder {
	public XMIBuilder() {
		super(".xmi");
	}
	
	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("XMI-File", st.nextToken());
		parent.addChild(rootDocument);
		
		XMIFactory builder = new XMIFactory(inputFile, rootDocument);
		builder.extract();
	}
}
