package de.ovgu.cide.fstgen;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.Set;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.parser.ParseException;
import cide.astgen.nparser.parser.SlimJJParser;
import de.ovgu.cide.fstgen.ast.FSTNodeType;

public class FSTgenTaskWithJMLPostprocessing extends Task {

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
			
			postprocessFSTGrammarForJML(checker);
			
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
	
	/**
	 * Adds some JML FST node types to the grammar
	 * @param g - grammar to add JML node types to
	 */
	private void postprocessFSTGrammarForJML(CreateFSTVisitor checker) {
		Set<FSTNodeType> nodeTypes = checker.getFSTNodeTypes();
		
		// remove outdated node types (so errors can be detected)
//		boolean hadMethodDecl =  nodeTypes.remove(new FSTNodeType("MethodDecl", true));
//		if (!hadMethodDecl) throw new RuntimeException( 
//			"grammar postprocessor for JML compliance: expected FST node type 'MethodDecl'");
		// equals() NOT IMPLEMENTED CORRECTLY! WHY USING A SET THEN?
		
		// add JML node types
		FSTNodeType newNodeType;
		newNodeType = new FSTNodeType("MethodDecl", false);
		nodeTypes.add(newNodeType);
		newNodeType = new FSTNodeType("MethodCode", true);
		nodeTypes.add(newNodeType);
		newNodeType = new FSTNodeType("MethodRequires", true);
		nodeTypes.add(newNodeType);
		newNodeType = new FSTNodeType("MethodEnsures", true);
		nodeTypes.add(newNodeType);
		
//		for (FSTNodeType nodeType : nodeTypes) {
//			System.out.println(nodeType.getName() + " " + nodeType.isTerminal());
//		}
		
		// seems that these node types are not used? Only for testing?
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
