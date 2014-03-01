package builder.csharpm;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;

import builder.ArtifactBuilder;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_csharp_merge.CSharpMergeParser;

public class CSharpMergeBuilder extends ArtifactBuilder {
	public CSharpMergeBuilder() {
		super(".cs");
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("CSharp-File", st.nextToken());
		parent.addChild(rootDocument);
		CSharpMergeParser p = new CSharpMergeParser(new OffsetCharStream(new FileInputStream(inputFile)));
		p.compilation_unit(false);
		rootDocument.addChild(p.getRoot());
	}
}
