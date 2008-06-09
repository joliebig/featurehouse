package builder.xmi;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;
import builder.ArtifactBuilder;
import tmp.generated_xmi.XMIParser;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class XMIBuilder extends ArtifactBuilder {
	public XMIBuilder() {
		super(".xmi");
	}
	
	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("XMI-File", st.nextToken());
		parent.addChild(rootDocument);
		XMIParser p = new XMIParser(new OffsetCharStream( new FileInputStream(inputFile)));
		try {
			p.Document(false);
			rootDocument.addChild(p.getRoot());
			System.err.println(p.getRoot().toString());
		} catch (ParseException e) {
			e.printStackTrace();
		}
	}
}
