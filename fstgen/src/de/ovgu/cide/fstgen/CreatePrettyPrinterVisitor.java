package de.ovgu.cide.fstgen;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.HashMap;

import cide.astgen.nparser.ast.NAbstractValue;
import cide.astgen.nparser.ast.NChoice;
import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.ast.NNonTerminal;
import cide.astgen.nparser.visitor.NCreateFileVisitor;

public class CreatePrettyPrinterVisitor extends NCreateFileVisitor {

	private boolean autoSpacingPrettyPrinter;

	public CreatePrettyPrinterVisitor(File targetDir, String targetPackage,
			boolean autoSpacingPrettyPrinter) throws FileNotFoundException {
		super(targetDir, "SimplePrintVisitor.java", targetPackage);
		this.autoSpacingPrettyPrinter = autoSpacingPrettyPrinter;
	}

	public CreatePrettyPrinterVisitor(PrintStream stream, String targetPackage,
			boolean autoSpacingPrettyPrinter) {
		super(stream, targetPackage);
		this.autoSpacingPrettyPrinter = autoSpacingPrettyPrinter;
	}

	@Override
	public boolean visit(NGrammar g) {
		printHead();
		return super.visit(g);
	}

	@Override
	public void postVisit(NGrammar g) {
		printTail();
		super.postVisit(g);
	}

	/**
	 * I only care fore nonterminals. those are printed in the order types of
	 * FST child nodes appear
	 */
	@Override
	public boolean visit(NChoice c) {
		// collect subtype relation
		if (!JavaCCPrintVisitor.getFSTType(c).equals('"'+c.getParent().getName()+'"'))
			subtypeRelation.put(JavaCCPrintVisitor.getFSTType(c), c.getParent()
					.getName());

		if (!isNonTerminal(c))
			return false;
		if (c.units.isEmpty())
			return false;

		println("if (nonTerminal.getType().equals("
				+ JavaCCPrintVisitor.getFSTType(c) + ")) {", 2);
		// println(c.genClassname() + " n = (" + c.genClassname() + ")node;",
		// 3);

		println("printFeatures(nonTerminal,true);",3);
		if (c.isList()) {
			println(
					"Iterator<FSTNode> listElements = getChildren(nonTerminal, \""
							+ c.getListType() + "\").iterator();", 3);
		}

		for (NAbstractValue unit : c.units) {
			for (String t : unit.outerPreTokens)
				printToken(t, 3);
			if (unit instanceof NNonTerminal)
				visitUnit((NNonTerminal) unit);
			for (String t : unit.outerPostTokens)
				printToken(t, 3);
		}
		println("printFeatures(nonTerminal,false);",3);
		println("return false;", 3);
		println("}", 2);
		return false;
	}

	private boolean isNonTerminal(NChoice c) {
		return c.findAnnotation("FSTNonTerminal") != null;
	}

	private void printToken(String token, int indent) {
		/*
		 * Special rule for fixed tokens that need to be referenced as
		 * identifieres (because in certain lexer state
		 * 
		 * format: "!<X>Y" where x is the name of the token and Y is the value
		 * of the token.
		 */
		if (token.length() > 3 && token.subSequence(0, 3).equals("\"!<")) {
			token = '"' + token.substring(token.indexOf('>') + 1);
		}

		if (token.charAt(0) == '"')
			println("printToken(\"" + token.substring(1, token.length() - 1)
					+ "\");", indent);
		if (token.charAt(0) == '@')
			handleLayoutHint(token, indent);
	}

	private void handleLayoutHint(String hint, int indent) {
		// layout hints are simple @!n, they don't have to form of usual
		// annotations @X(Y)
		if (hint.indexOf('(') > 0)
			return;// no layout hint
		if (hint.indexOf('+') > 0)
			println("hintIncIndent();", indent);
		if (hint.indexOf('-') > 0)
			println("hintDecIndent();", indent);
		if (hint.indexOf('n') > 0)
			println("hintNewLine();", indent);
		if (hint.indexOf('s') > 0)
			println("hintSingleSpace();", indent);
	}

	void visitUnit(NNonTerminal unit) {
		if (unit.isListElement()) {// special handling of list elements
			String command;
			if (!NAbstractValue.isMultiType(unit.type))
				command = "if";
			else
				command = "while";
			println(command + " (listElements.hasNext()) {", 3);
			for (String t : unit.innerPreTokens)
				printToken(t, 4);
			println("listElements.next().accept(this);", 4);
			for (String t : unit.innerPostTokens)
				printToken(t, 4);
			println("}", 3);
		} else {
			if (!NAbstractValue.isMultiType(unit.type)) {
				println("{", 3);// necessary to keep variable v local
				println("FSTNode v=getChild(nonTerminal, \"" + unit.getName()
						+ "\");", 4);
				println("if (v!=null) {", 4);
				for (String t : unit.innerPreTokens)
					printToken(t, 5);
				println("v.accept(this);", 5);
				for (String t : unit.innerPostTokens)
					printToken(t, 5);
				println("}", 4);
				println("}", 3);
			} else {
				println("for (FSTNode v : getChildren(nonTerminal,\""
						+ unit.getName() + "\")) {", 3);
				for (String t : unit.innerPreTokens)
					printToken(t, 4);
				println("v.accept(this);", 4);
				for (String t : unit.innerPostTokens)
					printToken(t, 4);
				println("}", 3);
			}
		}
	}

	private void printHead() {
		if (targetPackage != null && targetPackage.length() > 0)
			println("package " + targetPackage + ";\n", 0);
		println("import java.util.*;", 0);
		println("import cide.gast.*;\n", 0);
		println("import java.io.PrintStream;\n", 0);
		println("import cide.languages.*;\n", 0);
		println("import de.ovgu.cide.fstgen.ast.*;\n", 0);
		println(
				"public class SimplePrintVisitor extends AbstractFSTPrintVisitor  {",
				0);
		println("public SimplePrintVisitor(PrintStream out) {", 1);
		println("super(out); generateSpaces="
				+ (autoSpacingPrettyPrinter ? "true" : "false") + ";", 2);
		println("}", 1);
		println("public SimplePrintVisitor() {", 1);
		println("super(); generateSpaces="
				+ (autoSpacingPrettyPrinter ? "true" : "false") + ";", 2);
		println("}", 1);
		println("public boolean visit(FSTNonTerminal nonTerminal) {", 1);
		// println("public boolean visit(ASTNode node) {", 1);
		// println("if (node instanceof ASTStringNode){", 2);
		// println("printToken(((ASTStringNode)node).getValue());", 3);
		// println("return false;", 3);
		// println("}", 2);
		// println("if (node instanceof ASTTextNode){", 2);
		// // println("printToken(((ASTTextNode)node).getValue());", 3);
		// println("return false;", 3);
		// println("}", 2);

	}

	// put(a,b) means that b is a subtype of a
	private final HashMap<String, String> subtypeRelation = new HashMap<String, String>();

	private void printTail() {
		println(
				"throw new RuntimeException(\"Unknown Non Terminal in FST \"+nonTerminal);",
				2);
		println("}", 1);

		// subtype function
		println(
				"protected boolean isSubtype(String type, String expectedType) {",
				1);
		println("if (type.equals(expectedType)) return true;", 2);

		for (String key : subtypeRelation.keySet()) {
			println("if (type.equals(" + key + ") && expectedType.equals(\""
					+ subtypeRelation.get(key) + "\")) return true;", 2);
		}

		println("return false;", 2);
		println("}", 1);

		println("}", 0);
	}
}
