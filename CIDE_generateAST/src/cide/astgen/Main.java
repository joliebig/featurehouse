package cide.astgen;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.PrintStream;

import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.parser.ParseException;
import cide.astgen.nparser.parser.SlimJJParser;
import cide.astgen.nparser.visitor.ASTCreationVisitor;
import cide.astgen.nparser.visitor.CreateReferenceManagerVisitor;
import cide.astgen.nparser.visitor.CreateSimplePrintVisitorVisitor;
import cide.astgen.nparser.visitor.JavaCCPrintVisitor;
import cide.astgen.nparser.visitor.SlimPrintVisitor;

public class Main {

	private String grammarName;

	public Main(String name) {
		grammarName = name;
	}

	public static void main(String[] args) throws Exception {
		new Main(args[0]).run();

	}

	private void run() throws Exception {
		try {
			runGenerator(grammarName + ".gcide", new File("generated_"
					+ grammarName + "/"), "tmp.generated_" + grammarName);
		} catch (cide.astgen.nparser.parser.ParseException e) {
			e.printStackTrace();
			System.err.flush();
			System.out.println("Line " + e.currentToken.beginLine);
		}
	}

	public void runGenerator(String grammarFileName, File targetDirectory,
			String targetPackage) throws FileNotFoundException, ParseException {
		FileReader reader = new FileReader(grammarFileName);
		System.out.println("Reading " + grammarFileName);
		SlimJJParser parser = new SlimJJParser(reader);
		NGrammar grammar = parser.Grammar();
		runProductions(grammar, targetDirectory, targetPackage);
	}

	private void runProductions(NGrammar grammar, File targetDirectory,
			String targetPackage) throws FileNotFoundException {
		// for (NormalProduction production : bnfproductions) {
		// if (containsNestedChoice(production)) {
		// printProduction(production);
		// throw new NestedChoiceException();
		// }
		//
		// production.productionAnnotation.collectProduction();
		//
		// // printClasses(production);
		// }
		// NGrammar grammar = new NGrammarBuilder().build(bnfproductions);
		PrintStream jjout = new PrintStream(new File(targetDirectory,new File(grammarName + ".jj").getName()));
		grammar.accept(new SlimPrintVisitor(System.out));
		grammar.accept(new JavaCCPrintVisitor(jjout));

		targetDirectory.mkdir();
		grammar.accept(new ASTCreationVisitor(targetDirectory, targetPackage));
		grammar.accept(new CreateSimplePrintVisitorVisitor(targetDirectory,
				targetPackage));
		grammar.accept(new CreateReferenceManagerVisitor(targetDirectory,
				targetPackage));
		System.out.println("done.");
	}

	private void printIndented(String string, int indent) {
		for (int i = 0; i < indent; i++)
			System.out.print("  ");
		System.out.println(string);

	}

}
