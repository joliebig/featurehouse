package modification.content.Parseables.SDF;

import java.io.File;
import java.io.FileNotFoundException;


import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.parsers.generated_sdf.SDFParser;
import modification.content.InvalidFSTTraversalException;
import modification.content.Parseables.ParseableFile;

public class SDFFile extends ParseableFile {

	public SDFFile(File file) {
		super(file);
	}

	@Override
	public FSTNode getFST() throws FileNotFoundException, ParseException,
			modification.traversalLanguageParser.ParseException,
			InvalidFSTTraversalException {
		SDFParser p = new SDFParser(getCharStream());
		p.Module(false);
		FSTNonTerminal sdfFile = new FSTNonTerminal("SDF-File", file.getName());
		sdfFile.addChild(p.getRoot());
		return sdfFile;
	}
}
