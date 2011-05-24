package cide.astgen.nparser.visitor;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;

import cide.astgen.nparser.ast.NAbstractValue;
import cide.astgen.nparser.ast.NChoice;
import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.ast.NTextOnly;
import cide.astgen.nparser.ast.NAbstractValue.Type;

public class CreateSimplePrintVisitorVisitor extends NCreateFileVisitor {

	public CreateSimplePrintVisitorVisitor(File targetDir, String targetPackage)
			throws FileNotFoundException {
		super(targetDir, "SimplePrintVisitor.java", targetPackage);
	}

	public CreateSimplePrintVisitorVisitor(PrintStream stream,
			String targetPackage) {
		super(stream, targetPackage);
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

	@Override
	public boolean visit(NChoice c) {
		if (c.units.isEmpty())
			return false;

		println("if (node instanceof " + c.genClassname() + ") {", 2);
		println(c.genClassname() + " n = (" + c.genClassname() + ")node;", 3);

		if (c.isList()) {
			println("Iterator<" + c.getListType() + "> listElements = n."
					+ c.getListAccessMethod() + "().iterator();", 3);
		}

		for (NAbstractValue unit : c.units) {
			for (String t : unit.outerPreTokens)
				printToken(t, 3);
			visitUnit(unit);
			for (String t : unit.outerPostTokens)
				printToken(t, 3);
		}
		println("return false;", 3);
		println("}", 2);
		return false;
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

	void visitUnit(NAbstractValue unit) {
		if (unit instanceof NTextOnly && unit.type == Type.ONE) {
			assert unit.innerPostTokens.isEmpty()
					&& unit.innerPreTokens.isEmpty();
			return;
		}

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
				println(unit.genVariablePlainType() + " v=n."
						+ unit.genAccessMethod() + "();", 4);
				println("if (v!=null) {", 4);
				for (String t : unit.innerPreTokens)
					printToken(t, 5);
				println("v.accept(this);", 5);
				for (String t : unit.innerPostTokens)
					printToken(t, 5);
				println("}", 4);
				println("}", 3);
			} else {
				println("for (" + unit.genVariablePlainType() + " v : n."
						+ unit.genAccessMethod() + "()) {", 3);
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
		println("package " + targetPackage + ";\n", 0);
		println("import java.util.*;", 0);
		println("import cide.gast.*;\n", 0);
		println("import java.io.PrintStream;\n", 0);
		println("import cide.languages.*;\n", 0);
		println(
				"public class SimplePrintVisitor extends AbstractPrintVisitor implements ILanguagePrintVisitor {",
				0);
		println("public SimplePrintVisitor(PrintStream out) {", 1);
		println("super(out);", 2);
		println("}", 1);
		println("public SimplePrintVisitor() {", 1);
		println("super();", 2);
		println("}", 1);
		println("public boolean visit(ASTNode node) {", 1);
		println("if (node instanceof ASTStringNode){", 2);
		println("printToken(((ASTStringNode)node).getValue());", 3);
		println("return false;", 3);
		println("}", 2);
		println("if (node instanceof ASTTextNode){", 2);
		// println("printToken(((ASTTextNode)node).getValue());", 3);
		println("return false;", 3);
		println("}", 2);

	}

	private void printTail() {
		println("return true;", 2);
		println("}", 1);

		println("}", 0);
	}

}
