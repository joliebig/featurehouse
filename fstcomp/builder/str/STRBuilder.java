package builder.str;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;

import builder.ArtifactBuilder;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_stratego.StrategoParser;

public class STRBuilder extends ArtifactBuilder {

	public STRBuilder() {
		super(".str");
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st,
			File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("STR-File",
				st.nextToken());
		parent.addChild(rootDocument);
		StrategoParser p = new StrategoParser(new OffsetCharStream(
				new FileInputStream(inputFile)));
		p.Module(false);
		rootDocument.addChild(p.getRoot());
	}

}
