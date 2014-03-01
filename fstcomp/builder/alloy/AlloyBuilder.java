package builder.alloy;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;

import builder.ArtifactBuilder;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_alloy.AlloyParser;

public class AlloyBuilder extends ArtifactBuilder {
	public AlloyBuilder() {
		super(".als");
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st,
			File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("Alloy-File", st
				.nextToken());
		parent.addChild(rootDocument);
		AlloyParser p = new AlloyParser(new OffsetCharStream(
				new FileInputStream(inputFile)));
		p.Specification(false);
		rootDocument.addChild(p.getRoot());
	}
}
