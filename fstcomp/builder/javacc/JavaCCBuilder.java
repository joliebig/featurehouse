package builder.javacc;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;
import builder.ArtifactBuilder;
import tmp.generated_javacc.JavaCCParser;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class JavaCCBuilder extends ArtifactBuilder {
	public JavaCCBuilder() {
		super(".jj");
	}
	
	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("JavaCC-File", st.nextToken());
		parent.addChild(rootDocument);
		JavaCCParser p = new JavaCCParser(new OffsetCharStream( new FileInputStream(inputFile)));
		try {
			p.javacc_input(false);
			rootDocument.addChild(p.getRoot());
		} catch (ParseException e) {
			e.printStackTrace();
		}
	}
}
