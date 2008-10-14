package builder.lhaskell;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;

import tmp.generated_lhaskell.LHaskellParser;
import builder.ArtifactBuilder;
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
		LHaskellParser p = new LHaskellParser(new OffsetCharStream( new FileInputStream(inputFile)));
		try {
			p.module(false);
			rootDocument.addChild(p.getRoot());
		} catch (ParseException e) {
			e.printStackTrace();
		}
	}
}
