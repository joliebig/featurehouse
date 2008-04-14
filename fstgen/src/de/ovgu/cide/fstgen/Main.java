package de.ovgu.cide.fstgen;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;

import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.parser.ParseException;
import cide.astgen.nparser.parser.SlimJJParser;

public class Main {

	/**
	 * @param args
	 * @throws ParseException
	 * @throws FileNotFoundException
	 */
	public static void main(String[] args) throws FileNotFoundException,
			ParseException {
		if (args.length != 2) {
			System.out.println("usage: fstgen grammar.gcide output.jj");
			return;
		}
		String grammarFileName = args[0];
		String targetJJFile = args[1];

		NGrammar g = parseFile(grammarFileName);
		JavaCCPrintVisitor printVisitor = new JavaCCPrintVisitor(
				new PrintStream(new FileOutputStream(targetJJFile)));

		g.accept(new FSTInlineVisitor());

		CreateFSTVisitor checker = new CreateFSTVisitor();
		g.accept(checker);
		if (!checker.hasWellformedFSTAnnotations())
			System.out.println("Warning: " + checker.getWellformedErrorMsg());

		g.accept(printVisitor);

		CreatePrettyPrinterVisitor printer = new CreatePrettyPrinterVisitor(
				new File(targetJJFile).getParentFile(), "", true);
		g.accept(printer);

	}

	protected static NGrammar parseFile(String fileName)
			throws FileNotFoundException, ParseException {
		SlimJJParser parser = new SlimJJParser(new FileInputStream(fileName));
		NGrammar grammar = parser.Grammar();
		return grammar;
	}

}
