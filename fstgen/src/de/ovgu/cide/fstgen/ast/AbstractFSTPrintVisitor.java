package de.ovgu.cide.fstgen.ast;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintStream;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class AbstractFSTPrintVisitor extends FSTVisitor {

	private final StringBuffer buffer = new StringBuffer();
	private final PrintStream outputStream;

	private List<String> tokensInCurrentLine = new ArrayList<String>();
	private int indentInCurrentLine = 0;
	private static String[] JMLModifiers = { "spec_public", "spec_protected",
			"model", "ghost", "pure", "helper", "instance", "uninitialized",
			"spec_java_math", "spec_safe_math", "code_java_math",
			"code_safe_math", "code_bigint_math", "non_null", "nullable",
			"nullable_by_default", "extract" };

	public AbstractFSTPrintVisitor(PrintStream out) {
		this.outputStream = out;
	}

	public AbstractFSTPrintVisitor() {
		this.outputStream = null;
	}

	protected void printToken(String token) {
		if (tokensInCurrentLine.size() == 0)
			indentInCurrentLine = indent;
		tokensInCurrentLine.add(token);
	}

	protected void hintIncIndent() {
		indent++;
	}

	protected void hintDecIndent() {
		indent--;
	}

	protected void hintNewLine() {
		closeLine();
	}

	protected void hintSingleSpace() {
		printToken(" ");
	}

	public boolean generateSpaces = true;

	private void closeLine() {
		for (int i = 0; i < indentInCurrentLine; i++)
			print('\t');

		String lastToken = null;
		Iterator<String> i = tokensInCurrentLine.iterator();
		while (i.hasNext()) {
			String token = i.next();
			if (generateSpaces)
				if (lastToken != null
						&& !getNoSpaceAfterToken().contains(lastToken)
						&& !getNoSpaceBeforeToken().contains(token))
					print(' ');

			print(token);
			lastToken = token;
		}

		print('\n');

		tokensInCurrentLine.clear();
		indentInCurrentLine = 0;
	}

	/**
	 * to not use if you intent to print tokens
	 * 
	 * @param s
	 */
	protected void print(String s) {
		buffer.append(s);
		if (outputStream != null)
			outputStream.print(s);
	}

	private void print(char s) {
		buffer.append(s);
		if (outputStream != null)
			outputStream.print(s);
	}

	private int indent = 0;

	public String getResult() {
		if (tokensInCurrentLine.size() > 0)
			closeLine();
		if (outputStream != null) {
			outputStream.close();
		}
		return buffer.toString();
	}

	private List<String> noSpaceAfterToken;

	protected List<String> getNoSpaceAfterToken() {
		if (noSpaceAfterToken == null) {
			noSpaceAfterToken = new ArrayList<String>();
			noSpaceAfterToken.add("{");
			noSpaceAfterToken.add("(");
			noSpaceAfterToken.add("[");
			noSpaceAfterToken.add("@");
			noSpaceAfterToken.add(".");
		}
		return noSpaceAfterToken;
	}

	private List<String> noSpaceBeforeToken;

	protected List<String> getNoSpaceBeforeToken() {
		if (noSpaceBeforeToken == null) {
			noSpaceBeforeToken = new ArrayList<String>();
			noSpaceBeforeToken.add("}");
			noSpaceBeforeToken.add(")");
			noSpaceBeforeToken.add("]");
			noSpaceBeforeToken.add(";");
			noSpaceBeforeToken.add(".");
		}
		return noSpaceBeforeToken;
	}

	protected abstract boolean isSubtype(String type, String expectedType);

	protected List<FSTNode> getChildren(FSTNonTerminal nonTerminal,
			String childType) {
		List<FSTNode> result = new ArrayList<FSTNode>();
		for (FSTNode node : nonTerminal.getChildren()) {
			if (isSubtype(node.getType(), childType))
				result.add(node);
		}
		return result;
	}

	protected FSTNode getChild(FSTNonTerminal nonTerminal, String childType) {
		List<FSTNode> result = getChildren(nonTerminal, childType);
		if (result.size() > 1) {
			throw new RuntimeException(
					"Cannot handle multple FST nodes of type " + childType
							+ " here (" + result.get(0).getName() + ":"
							+ result.get(0).getType() + " and "
							+ result.get(1).getName() + ":"
							+ result.get(0).getType() + " parent: "
							+ nonTerminal.toString() + ")");
		}
		if (result.size() == 1)
			return result.get(0);
		return null;
	}

	@Override
	public boolean visit(FSTTerminal terminal) {
		printFeatures(terminal, true);
		if (CommandLineParameterHelper.isJML()) {
			handleJMLReplacements(terminal);
			try {
				printToken(removeBlanks(terminal.getSpecialTokenPrefix())
						+ terminal.getBody());
			} catch (IOException e) {
				e.printStackTrace();
			}
		} else {
			printToken(terminal.getSpecialTokenPrefix() + terminal.getBody());
		}

		printFeatures(terminal, false);
		// hintNewLine();
		return false;
	}

	static String removeBlanks(String input) throws IOException {
		BufferedReader br = new BufferedReader(new StringReader(input));
		StringBuffer result = new StringBuffer();

		String line;

		while ((line = br.readLine()) != null) {
			line = line.trim(); // remove leading and trailing whitespace
			if (!line.equals("")) // don't write out blank lines
			{
				result.append(line + "\n");
			}
		}

		return result.toString();
	}

	private static void handleJMLReplacements(FSTTerminal terminal) {

		if (terminal.getType().equals("MethodDecl")
				|| terminal.getType().equals("ConstructorDecl")) {

			terminal.setBody(handleJMLModifierMethod(terminal.getBody()));
		}
		if (terminal.getType().equals("ModFieldDeclaration")) {

			terminal.setBody(handleJMLModifierField(terminal.getBody()));
		}
		if (terminal.getType().equals("Modifiers2")) {

			terminal.setBody(replaceModifiers(terminal.getBody()));
		}
		if (terminal.getType().equals("Modifiers")) {

			terminal.setBody(replaceModifiers(terminal.getBody()));
		}

		if (terminal.getType().equals("AssertStatement")
				|| terminal.getType().equals("JMLAnnotationStatement ")
				|| terminal.getType().equals("ModelProgStatement ")) {

			terminal.setBody("/*@" + terminal.getBody() + "@*/");
		}
	}

	protected void printFeatures(FSTNode node, boolean b) {
		// used only in subclasses
	}

	private static String handleJMLModifierMethod(String in) {

		String out = in.substring(0, in.indexOf(")"));
		Pattern p = Pattern.compile("\\smodel\\s");
		Matcher m = p.matcher(in);
		if (m.find())
			return out = "/*@" + in + "@*/";
		out = replaceModifiers(out);
		out = out + in.substring(in.indexOf(")"));
		return out;
	}

	private static String handleJMLModifierField(String in) {

		String out = null;
		Pattern p = Pattern.compile("\\smodel\\s");
		Matcher m = p.matcher(in);
		if (m.find())
			out = "/*@" + in + "@*/";
		else {
			out = replaceModifiers(in);
		}

		return out;
	}

	/**
	 * @param out
	 * @return
	 */
	private static String replaceModifiers(String out) {
		for (String mod : JMLModifiers) {
			out = out.replaceAll("(^|\\W)" + mod + "(\\s|\\z)", " /*@" + mod
					+ "@*/ ");
		}
		return out;
	}

}
