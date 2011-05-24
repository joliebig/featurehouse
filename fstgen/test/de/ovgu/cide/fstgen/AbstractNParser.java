package de.ovgu.cide.fstgen;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.parser.ParseException;
import cide.astgen.nparser.parser.SlimJJParser;

public class AbstractNParser {

	protected NGrammar parseFile(String fileName) throws FileNotFoundException,
			ParseException {
		SlimJJParser parser = new SlimJJParser(new FileInputStream(fileName));
		NGrammar grammar = parser.Grammar();
		return grammar;
	}

	protected NGrammar parse(String t) throws ParseException {
		SlimJJParser parser = new SlimJJParser(new ByteArrayInputStream(
				("GRAMMARSTART " + t).getBytes()));
		return parser.Grammar();
	}
}
