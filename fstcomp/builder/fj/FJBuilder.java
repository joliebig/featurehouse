package builder.fj;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;

import builder.ArtifactBuilder;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_fj.FJParser;

public class FJBuilder extends ArtifactBuilder {
	public FJBuilder() {
		super(".fj");
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st,
			File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("FeatherweightJava-File", st
				.nextToken());
		parent.addChild(rootDocument);
		FJParser p = new FJParser(new OffsetCharStream(
				new FileInputStream(inputFile)));
		p.TypeDeclaration(false);
		rootDocument.addChild(p.getRoot());
	}
}
