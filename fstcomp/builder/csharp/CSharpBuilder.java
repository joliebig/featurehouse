package builder.csharp;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;

import builder.ArtifactBuilder;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_csharp.CSharpParser;

public class CSharpBuilder extends ArtifactBuilder {
	public CSharpBuilder() {
		super(".cs");
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st,
			File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("CSharp-File", st
				.nextToken());
		parent.addChild(rootDocument);
		CSharpParser p = new CSharpParser(new OffsetCharStream(
				new FileInputStream(inputFile)));
		p.compilation_unit(false);
		rootDocument.addChild(p.getRoot());
	}
}
