package builder.javam;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;

import builder.ArtifactBuilder;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_java15_merge.Java15MergeParser;

public class JavaMergeBuilder extends ArtifactBuilder {
	public JavaMergeBuilder() {
		super(".java");
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("Java-File", st.nextToken());
		parent.addChild(rootDocument);
		Java15MergeParser p = new Java15MergeParser(new OffsetCharStream(new FileInputStream(inputFile)));
		p.CompilationUnit(false);
		rootDocument.addChild(p.getRoot());
	}
}