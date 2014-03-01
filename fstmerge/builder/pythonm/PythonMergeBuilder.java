package builder.pythonm;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.StringTokenizer;

import builder.ArtifactBuilder;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_python.PythonParser;
import de.ovgu.cide.fstgen.parsers.generated_python.SimplePrintVisitor;

public class PythonMergeBuilder extends ArtifactBuilder {
	public PythonMergeBuilder() {
		super(".py");
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st, File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("Python-File", st.nextToken());
		parent.addChild(rootDocument);
		PythonParser p = new PythonParser(new OffsetCharStream(new FileInputStream(inputFile)));
		p.file_input(false);
		rootDocument.addChild(p.getRoot());

		// write element one time out to delete comments
		if (isPreprocessNode()) {
			for (FSTNode child : rootDocument.getChildren()) {
				SimplePrintVisitor visitor;
				try {
					visitor = new SimplePrintVisitor(new PrintStream(inputFile));
					visitor.visit((FSTNonTerminal) child);
					visitor.getResult();
				} catch (FileNotFoundException e) {
				}
			}
		}
	}
}