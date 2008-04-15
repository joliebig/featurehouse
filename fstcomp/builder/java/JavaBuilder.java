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
	private String suffix = ".java";

	@Override
	public boolean acceptFile(File inputFile) {
		if (inputFile.isFile()) {
		      if (inputFile.getName().endsWith(suffix)) {
		        return true;
		      }
		    }
		    return false;
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("JavaFile", st.nextToken());
		parent.addChild(rootDocument);
		parent = rootDocument;
		Java15Parser p = new Java15Parser(new OffsetCharStream( new FileInputStream(inputFile)));
		try {
			p.CompilationUnit(false);
			rootDocument.addChild(p.getRoot());
		} catch (ParseException e) {
			e.printStackTrace();
		}
	}
}
