package builder.asmetal;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;

import builder.ArtifactBuilder;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.CommandLineParameterHelper;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_AsmetaL.AsmetaLParser;

public class AsmetaLBuilder extends ArtifactBuilder {
	public AsmetaLBuilder() {
		super(".asm");
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st,
			File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("AsmetaL-File",
				st.nextToken());
		parent.addChild(rootDocument);

		AsmetaLParser p = new AsmetaLParser(new OffsetCharStream(
				new FileInputStream(inputFile)));
		p.CompilationUnit(false);
		rootDocument.addChild(p.getRoot());

		
	}
}
