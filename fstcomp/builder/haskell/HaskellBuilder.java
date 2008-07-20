package builder.haskell;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;
import builder.ArtifactBuilder;
import tmp.generated_haskell.HaskellParser;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class HaskellBuilder extends ArtifactBuilder {
	public HaskellBuilder() {
		super(".hs");
	}
	
	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("Haskell-File", st.nextToken());
		parent.addChild(rootDocument);
		HaskellParser p = new HaskellParser(new OffsetCharStream( new FileInputStream(inputFile)));
		try {
			p.module(false);
			rootDocument.addChild(p.getRoot());
		} catch (ParseException e) {
			e.printStackTrace();
		}
	}
}
