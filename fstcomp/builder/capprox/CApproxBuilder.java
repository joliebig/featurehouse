package builder.capprox;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;
import builder.ArtifactBuilder;
import tmp.generated_capprox.CApproxParser;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class CApproxBuilder extends ArtifactBuilder {
	public CApproxBuilder() {
		super(".c");
	}
	
	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("C-File", st.nextToken());
		parent.addChild(rootDocument);
		CApproxParser p = new CApproxParser(new OffsetCharStream( new FileInputStream(inputFile)));
		try {
			p.TranslationUnit(false);
			rootDocument.addChild(p.getRoot());
		} catch (ParseException e) {
			e.printStackTrace();
		}
	}
}
