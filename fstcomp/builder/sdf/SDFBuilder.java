package builder.sdf;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;


import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_sdf.SDFParser;
import builder.ArtifactBuilder;

public class SDFBuilder extends ArtifactBuilder {

	public SDFBuilder() {
		super(".sdf");
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st,
			File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("SDF-File",
				st.nextToken());
		parent.addChild(rootDocument);
		SDFParser p = new SDFParser(new OffsetCharStream(new FileInputStream(
				inputFile)));
		p.Module(false);
		rootDocument.addChild(p.getRoot());
	}

}
