package builder.java;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.StringTokenizer;


import tmp.generated_java15.Java15Parser;
import tmp.generated_jml_contract_composition.JMLParser;
import builder.ArtifactBuilder;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.CommandLineParameterHelper;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class JavaBuilder extends ArtifactBuilder {
	public JavaBuilder() {
		super(".java");
	}

	public void processNode(FSTNonTerminal parent, StringTokenizer st,
			File inputFile) throws FileNotFoundException, ParseException {
		FSTNonTerminal rootDocument = new FSTNonTerminal("Java-File",
				st.nextToken());
		parent.addChild(rootDocument);
		
		
		if(CommandLineParameterHelper.isJML()){
			JMLParser p = new JMLParser(new OffsetCharStream(
					new FileInputStream(inputFile)));
			p.CompilationUnit(false);
			rootDocument.addChild(p.getRoot());
		}
		else{
			Java15Parser p = new Java15Parser(new OffsetCharStream(
					new FileInputStream(inputFile)));
			p.CompilationUnit(false);
			rootDocument.addChild(p.getRoot());
		}
		
	}
}
