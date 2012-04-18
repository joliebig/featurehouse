package builder.jcop;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;

import tmp.generated_jcop.JCopParser;

import builder.ArtifactBuilder;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class JCopBuilder extends ArtifactBuilder {
	public JCopBuilder() {
		super(".jcop");
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st,
			File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("JCop-File", st
				.nextToken());
		parent.addChild(rootDocument);
		JCopParser p = new JCopParser(new OffsetCharStream(
				new FileInputStream(inputFile)));
		p.CompilationUnit(false);
		rootDocument.addChild(p.getRoot());
	}

}
