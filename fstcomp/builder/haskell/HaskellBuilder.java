package builder.haskell;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;

import builder.ArtifactBuilder;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_haskell.HaskellParser;

public class HaskellBuilder extends ArtifactBuilder {
	public HaskellBuilder() {
		super(".hs");
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st,
			File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("Haskell-File", st
				.nextToken());
		parent.addChild(rootDocument);
		HaskellParser p = new HaskellParser(new OffsetCharStream(
				new FileInputStream(inputFile)));
		p.module(false);
		rootDocument.addChild(p.getRoot());
	}
}
