package builder.phaskell;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;
import builder.ArtifactBuilder;
import tmp.generated_phaskell.PHaskellParser;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class PHaskellBuilder extends ArtifactBuilder {
	public PHaskellBuilder() {
		super(".hs");
	}
	
	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("PHaskell-File", st.nextToken());
		parent.addChild(rootDocument);
		PHaskellParser p = new PHaskellParser(new OffsetCharStream( new FileInputStream(inputFile)));
		try {
			p.module(false);
			rootDocument.addChild(p.getRoot());
		} catch (ParseException e) {
			e.printStackTrace();
		}
	}
}
