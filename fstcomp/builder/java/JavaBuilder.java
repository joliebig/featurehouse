package builder.java;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;
import builder.ArtifactBuilder;
import tmp.generated_java15.Java15Parser;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class JavaBuilder extends ArtifactBuilder {
	private String suffix1 = ".java";
	private String suffix2 = ".jak";

	@Override
	public boolean acceptFile(File inputFile) {
		if (inputFile.isFile()) {
		      if (inputFile.getName().endsWith(suffix1) || inputFile.getName().endsWith(suffix2)) {
		        return true;
		      }
		    }
		    return false;
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("JavaFile", st.nextToken());
		parent.addChild(rootDocument);
		Java15Parser p = new Java15Parser(new OffsetCharStream( new FileInputStream(inputFile)));
		try {
			p.CompilationUnit(false);
			rootDocument.addChild(p.getRoot());
		} catch (ParseException e) {
			e.printStackTrace();
		}
	}
}
