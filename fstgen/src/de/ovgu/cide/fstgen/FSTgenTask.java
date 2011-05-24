package de.ovgu.cide.fstgen;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.parser.ParseException;
import cide.astgen.nparser.parser.SlimJJParser;

public class FSTgenTask extends Task {

	private String grammarFileName;
	private String targetJJFile;
	private String targetPackage="";
	private boolean autoSpacingPrettyPrinter=true;

	public void execute() throws BuildException {
		try {
			NGrammar g;
			g = parseFile(grammarFileName);
			JavaCCPrintVisitor printVisitor = new JavaCCPrintVisitor(
					new PrintStream(new FileOutputStream(targetJJFile)));

			g.accept(new FSTInlineVisitor());

			CreateFSTVisitor checker = new CreateFSTVisitor();
			g.accept(checker);
			if (!checker.hasWellformedFSTAnnotations())
				System.out.println("Warning: "
						+ checker.getWellformedErrorMsg());

			g.accept(printVisitor);

			CreatePrettyPrinterVisitor printer = new CreatePrettyPrinterVisitor(
					new File(targetJJFile).getParentFile(), targetPackage, autoSpacingPrettyPrinter);
			g.accept(printer);
		} catch (FileNotFoundException e1) {
			throw new BuildException(e1);
		} catch (ParseException e1) {
			throw new BuildException(e1);
		}
	}

	public void setGrammarFileName(String g) {
		this.grammarFileName = g;
	}

	public void setTargetJJFile(String td) {
		this.targetJJFile = td;
	}

	public void setTargetPackage(String td) {
		this.targetPackage = td;
	}
	
	public void setAutoSpacingPrettyPrinter(String v){
		autoSpacingPrettyPrinter=!v.equals("false");
	}

	protected NGrammar parseFile(String fileName) throws FileNotFoundException,
			ParseException {
		SlimJJParser parser = new SlimJJParser(new FileInputStream(fileName));
		NGrammar grammar = parser.Grammar();
		return grammar;
	}

}
