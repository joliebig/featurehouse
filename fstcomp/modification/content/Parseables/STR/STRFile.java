package modification.content.Parseables.STR;

import java.io.File;
import java.io.FileNotFoundException;

import tmp.generated_stratego.StrategoParser;

import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

import modification.content.InvalidFSTTraversalException;
import modification.content.Parseables.ParseableFile;

public class STRFile extends ParseableFile {

	public STRFile(File file) {
		super(file);
	}

	@Override
	public FSTNode getFST() throws FileNotFoundException, ParseException,
			modification.traversalLanguageParser.ParseException,
			InvalidFSTTraversalException {
		StrategoParser p = new StrategoParser(getCharStream());
		p.Module(false);
		FSTNonTerminal strFile = new FSTNonTerminal("STR-File", file.getName());
		strFile.addChild(p.getRoot());
		return strFile;
	}

}
